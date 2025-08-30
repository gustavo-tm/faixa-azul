
clear all
program drop _all

cd "C:\Dev\faixa-azul"



* ================================================================================
* STATIC IMPUTATION ESTIMATOR PROGRAM
* ================================================================================

cap: program drop static_imput_boot
program define static_imput_boot, eclass

    * Clean up any existing variables
    cap drop ife tfe ai at hatYUit delta APT
    
    * Step 1: Estimate counterfactual model using UNTREATED observations only
    ppmlhdfe Y if T == 0, ///
        absorb(ife=id tfe=month) ///
        noomitted keepsing separation(fe)
    
    local cst = _b[_cons]
    
    * Step 2: Extract fixed effects and predict counterfactuals
    bysort id: egen ai = max(ife)      // Individual fixed effects
    bysort month: egen at = max(tfe)   // Time fixed effects
    
    gen hatYUit = exp(ai + at + `cst') // Predicted counterfactual
    
    * Step 3: Calculate treatment effects
    gen delta = Y - hatYUit
    
    * Step 4: Calculate aggregate ATT (Average Treatment on Treated)
    qui sum delta if T == 1
    local att = r(mean)
    
    * Step 5: Calculate scale factor (baseline counterfactual for treated)
    qui sum hatYUit if T == 1 & Y != .
    local scale = r(mean)
    
    * Step 6: Calculate proportional effect (main parameter of interest)
    local imput_effect = `att' / `scale'
    
    * Step 7: Alternative measure - Average Proportional Treatment (APT)
    gen APT = Y / hatYUit
    qui sum APT if T == 1
    local apt_effect = r(mean)
    
    * Step 8: Store results in a matrix for ereturn
    matrix results = (`imput_effect', `apt_effect', `att', `scale')
    matrix colnames results = imput_effect apt_effect att_level scale_factor
    
    * Return results
    ereturn post results
    
    * Also store in ereturn scalars for easy access
    ereturn scalar imput_effect = `imput_effect'
    ereturn scalar apt_effect = `apt_effect'
    ereturn scalar att_level = `att'
    ereturn scalar scale_factor = `scale'
    
end



* ================================================================================
* DYNAMIC IMPUTATION ESTIMATOR PROGRAM
* ================================================================================

program define dynamic_imput_boot, eclass

    * Get dimensions within the program (use actual K range)
    qui sum K if K != .
    local k_min = `r(min)'
    local k_max = `r(max)'
    
    * Define the range more carefully
    * For leads: from k_min to -2 (we skip -1 as it's used for estimation)
    * For lags: from 0 to k_max
    local n_leads = max(0, abs(`k_min') - 1)  // Number of lead periods (excluding -1)
    local n_lags = `k_max' + 1                // Number of lag periods (including 0)
    local nb_coef = `n_leads' + `n_lags'      // Total coefficients
    
    * Create column names dynamically based on actual data
    local names = ""
    * Add lead names
    forvalue i = `k_min'/-2 {
        local abs_i = abs(`i')
        local names = "`names' pre`abs_i'"
    }
    * Add lag names  
    forvalue i = 0/`k_max' {
        local names = "`names' post`i'"
    }
    
    * Create matrices to store results
    matrix imputation_att_b = J(1, `nb_coef', 0)  // Initialize with 0s
    matrix imputation_ptt_b = J(1, `nb_coef', 0)  // Initialize with 0s
    
    * Only set column names if we have coefficients
    if `nb_coef' > 0 {
        matrix colnames imputation_att_b = `names'
        matrix colnames imputation_ptt_b = `names'
    }

    * Clean up any existing variables
    cap drop ife tfe ai at hatYUit delta

    * Step 1: Estimate counterfactual model using clean observations
    qui ppmlhdfe Y if (K == -1) | (Ei == 0), ///
        absorb(ife=id tfe=month) ///
        noomitted keepsing separation(fe)
    
    local cst = _b[_cons]

    * Step 2: Extract fixed effects and predict counterfactuals
    bysort id: egen ai = max(ife)      // Individual fixed effects
    bysort month: egen at = max(tfe)   // Time fixed effects
    
    gen hatYUit = exp(ai + at + `cst') // Predicted counterfactual

    * Step 3: Calculate treatment effects (ATT)
    gen delta = Y - hatYUit

    * Step 4: Store results by event time
    local col_index = 1
    
    * Store pre-treatment effects (leads)
    forvalue i = `k_min'/-2 {
        local abs_i = abs(`i')
        
        * Check if this event variable exists and has observations
        cap confirm variable F`abs_i'event
        if _rc == 0 {
            qui count if F`abs_i'event == 1
            if r(N) > 0 {
                * ATT: Simple difference
                qui sum delta if F`abs_i'event == 1
                if `r(N)' > 0 & `r(mean)' != . {
                    matrix imputation_att_b[1, `col_index'] = `r(mean)'
                    
                    * PTT: Convert to proportional effect
                    qui sum hatYUit if F`abs_i'event == 1 & Y != .
                    if `r(N)' > 0 & `r(mean)' > 0 {
                        local ptt_val = log(imputation_att_b[1, `col_index'] / `r(mean)' + 1)
                        if `ptt_val' != . {
                            matrix imputation_ptt_b[1, `col_index'] = `ptt_val'
                        }
                    }
                }
            }
        }
        local col_index = `col_index' + 1
    }
    
    * Store post-treatment effects (lags)
    forvalues i = 0/`k_max' {
        * Check if this event variable exists and has observations
        cap confirm variable L`i'event
        if _rc == 0 {
            qui count if L`i'event == 1
            if r(N) > 0 {
                * ATT: Simple difference
                qui sum delta if L`i'event == 1
                if `r(N)' > 0 & `r(mean)' != . {
                    matrix imputation_att_b[1, `col_index'] = `r(mean)'
                    
                    * PTT: Convert to proportional effect
                    qui sum hatYUit if L`i'event == 1 & Y != .
                    if `r(N)' > 0 & `r(mean)' > 0 {
                        local ptt_val = log(imputation_att_b[1, `col_index'] / `r(mean)' + 1)
                        if `ptt_val' != . {
                            matrix imputation_ptt_b[1, `col_index'] = `ptt_val'
                        }
                    }
                }
            }
        }
        local col_index = `col_index' + 1
    }

    * Return results
    ereturn post imputation_ptt_b
    
end



* ================================================================================
* RUN THE ESTIMATORS
* ================================================================================



*** SINISTROS ENVOLVENDO MOTO, POR KM ***
import delimited "C:\Dev\faixa-azul\stata\input\km_moto.csv", clear

gen Y = sinistros
gen month = periodo
gen group = coorte

* Create treatment indicator and relative time
qui gen Ei = group  // Treatment month (0 for never-treated)
qui gen K = month - Ei if Ei != 0  // Relative time (missing for never-treated)
qui gen T = (K >= 0 & Ei != 0)     // Treatment indicator

* Create event study dummies based on actual data range
qui sum K if K != .
local k_min = `r(min)'
local k_max = `r(max)'
di "Relative time range: `k_min' to `k_max'"

* Leads (pre-treatment periods, K < 0)
forvalues l = 2/`=abs(`k_min')' {
    qui gen F`l'event = (K == -`l') if K != .
    qui count if F`l'event == 1
}
* Lags (post-treatment periods, K >= 0)
forvalues l = 0/`k_max' {
    qui gen L`l'event = (K == `l') if K != .
    qui count if L`l'event == 1
}


*** Static imputation
bootstrap imput=e(imput_effect) apt=e(apt_effect) att=e(att_level) scale=e(scale_factor), ///
    reps(1000) cluster(id) seed(12345): static_imput_boot

estimates store s_imput_sin_moto_km

*** Dynamic imputation
bootstrap, reps(1000) cluster(id) idcluster(newid) seed(12345): dynamic_imput_boot

estimates store d_imput_sin_moto_km

* Dynamic visualization
// matrix imputation_ptt_b = e(b)
// matrix imputation_ptt_v = e(V)
//
// event_plot imputation_ptt_b#imputation_ptt_v, ///
// stub_lead(pre#) stub_lag(post#) trimlag(12) trimlead(12) ///
// plottype(scatter) ciplottype(rcap) together noautolegend ///
// graph_opt(xtitle("Meses até a data de implemetação") ytitle("") xlabel(-12(1)12) yline(0, lpattern(dash) lcolor(gray)) ///
// ylabel(-1.5(0.5)1.5) legend(off))
//
// graph export "C:\Dev\faixa-azul\stata\plots\imput\05-sinistros-atropelamento.png", replace




*** TODOS OS SINISTROS, POR KM ***

import delimited "C:\Dev\faixa-azul\stata\input\km_todos.csv", clear

gen Y = sinistros
gen month = periodo
gen group = coorte

* Create treatment indicator and relative time
qui gen Ei = group  // Treatment month (0 for never-treated)
qui gen K = month - Ei if Ei != 0  // Relative time (missing for never-treated)
qui gen T = (K >= 0 & Ei != 0)     // Treatment indicator

* Create event study dummies based on actual data range
qui sum K if K != .
local k_min = `r(min)'
local k_max = `r(max)'
di "Relative time range: `k_min' to `k_max'"

* Leads (pre-treatment periods, K < 0)
forvalues l = 2/`=abs(`k_min')' {
    qui gen F`l'event = (K == -`l') if K != .
    qui count if F`l'event == 1
}
* Lags (post-treatment periods, K >= 0)
forvalues l = 0/`k_max' {
    qui gen L`l'event = (K == `l') if K != .
    qui count if L`l'event == 1
}


*** Static imputation
bootstrap imput=e(imput_effect) apt=e(apt_effect) att=e(att_level) scale=e(scale_factor), ///
    reps(1000) cluster(id) seed(12345): static_imput_boot

estimates store s_imput_sin_todos_km

*** Dynamic imputation
bootstrap, reps(1000) cluster(id) idcluster(newid) seed(12345): dynamic_imput_boot

estimates store d_imput_sin_todos_km




*** ATROPELAMENTOS ENVOLVENDO MOTO, POR KM ***

import delimited "C:\Dev\faixa-azul\stata\input\km_moto_atropelamento.csv", clear

gen Y = sinistros
gen month = periodo
gen group = coorte

* Create treatment indicator and relative time
qui gen Ei = group  // Treatment month (0 for never-treated)
qui gen K = month - Ei if Ei != 0  // Relative time (missing for never-treated)
qui gen T = (K >= 0 & Ei != 0)     // Treatment indicator

* Create event study dummies based on actual data range
qui sum K if K != .
local k_min = `r(min)'
local k_max = `r(max)'
di "Relative time range: `k_min' to `k_max'"

* Leads (pre-treatment periods, K < 0)
forvalues l = 2/`=abs(`k_min')' {
    qui gen F`l'event = (K == -`l') if K != .
    qui count if F`l'event == 1
}
* Lags (post-treatment periods, K >= 0)
forvalues l = 0/`k_max' {
    qui gen L`l'event = (K == `l') if K != .
    qui count if L`l'event == 1
}


*** Static imputation
bootstrap imput=e(imput_effect) apt=e(apt_effect) att=e(att_level) scale=e(scale_factor), ///
    reps(1000) cluster(id) seed(12345): static_imput_boot

estimates store s_imput_sin_atrop_km

*** Dynamic imputation
bootstrap, reps(1000) cluster(id) idcluster(newid) seed(12345): dynamic_imput_boot

estimates store d_imput_sin_atrop_km




*** TODOS OS ATROPELAMENTOS, POR KM ***

import delimited "C:\Dev\faixa-azul\stata\input\km_todos_atropelamento.csv", clear

gen Y = sinistros
gen month = periodo
gen group = coorte

* Create treatment indicator and relative time
qui gen Ei = group  // Treatment month (0 for never-treated)
qui gen K = month - Ei if Ei != 0  // Relative time (missing for never-treated)
qui gen T = (K >= 0 & Ei != 0)     // Treatment indicator

* Create event study dummies based on actual data range
qui sum K if K != .
local k_min = `r(min)'
local k_max = `r(max)'
di "Relative time range: `k_min' to `k_max'"

* Leads (pre-treatment periods, K < 0)
forvalues l = 2/`=abs(`k_min')' {
    qui gen F`l'event = (K == -`l') if K != .
    qui count if F`l'event == 1
}
* Lags (post-treatment periods, K >= 0)
forvalues l = 0/`k_max' {
    qui gen L`l'event = (K == `l') if K != .
    qui count if L`l'event == 1
}


*** Static imputation
bootstrap imput=e(imput_effect) apt=e(apt_effect) att=e(att_level) scale=e(scale_factor), ///
    reps(1000) cluster(id) seed(12345): static_imput_boot

estimates store s_imput_todos_atrop_km

*** Dynamic imputation
bootstrap, reps(1000) cluster(id) idcluster(newid) seed(12345): dynamic_imput_boot

estimates store d_imput_todos_atrop_km




*** SINISTROS ENVOLVENDO MOTO EM HORARIO DE PICO, POR KM ***

import delimited "C:\Dev\faixa-azul\stata\input\km_moto_horario_pico.csv", clear

gen Y = sinistros
gen month = periodo
gen group = coorte

* Create treatment indicator and relative time
qui gen Ei = group  // Treatment month (0 for never-treated)
qui gen K = month - Ei if Ei != 0  // Relative time (missing for never-treated)
qui gen T = (K >= 0 & Ei != 0)     // Treatment indicator

* Create event study dummies based on actual data range
qui sum K if K != .
local k_min = `r(min)'
local k_max = `r(max)'
di "Relative time range: `k_min' to `k_max'"

* Leads (pre-treatment periods, K < 0)
forvalues l = 2/`=abs(`k_min')' {
    qui gen F`l'event = (K == -`l') if K != .
    qui count if F`l'event == 1
}
* Lags (post-treatment periods, K >= 0)
forvalues l = 0/`k_max' {
    qui gen L`l'event = (K == `l') if K != .
    qui count if L`l'event == 1
}


*** Static imputation
bootstrap imput=e(imput_effect) apt=e(apt_effect) att=e(att_level) scale=e(scale_factor), ///
    reps(1000) cluster(id) seed(12345): static_imput_boot

estimates store s_imput_sin_hora_km

*** Dynamic imputation
bootstrap, reps(1000) cluster(id) idcluster(newid) seed(12345): dynamic_imput_boot

estimates store d_imput_sin_hora_km




*** TODOS OS SINISTROS EM HORARIO DE PICO, POR KM ***

import delimited "C:\Dev\faixa-azul\stata\input\km_todos_horario_pico.csv", clear

gen Y = sinistros
gen month = periodo
gen group = coorte

* Create treatment indicator and relative time
qui gen Ei = group  // Treatment month (0 for never-treated)
qui gen K = month - Ei if Ei != 0  // Relative time (missing for never-treated)
qui gen T = (K >= 0 & Ei != 0)     // Treatment indicator

* Create event study dummies based on actual data range
qui sum K if K != .
local k_min = `r(min)'
local k_max = `r(max)'
di "Relative time range: `k_min' to `k_max'"

* Leads (pre-treatment periods, K < 0)
forvalues l = 2/`=abs(`k_min')' {
    qui gen F`l'event = (K == -`l') if K != .
    qui count if F`l'event == 1
}
* Lags (post-treatment periods, K >= 0)
forvalues l = 0/`k_max' {
    qui gen L`l'event = (K == `l') if K != .
    qui count if L`l'event == 1
}


*** Static imputation
bootstrap imput=e(imput_effect) apt=e(apt_effect) att=e(att_level) scale=e(scale_factor), ///
    reps(1000) cluster(id) seed(12345): static_imput_boot

estimates store s_imput_todos_hora_km

*** Dynamic imputation
bootstrap, reps(1000) cluster(id) idcluster(newid) seed(12345): dynamic_imput_boot

estimates store d_imput_todos_hora_km




*** SINISTROS ENVOLVENDO MOTO NAS VIAS COM MAIS INTERSECCOES, POR KM ***

import delimited "C:\Dev\faixa-azul\stata\input\km_moto_interseccoes.csv", clear

gen Y = sinistros
gen month = periodo
gen group = coorte

* Create treatment indicator and relative time
qui gen Ei = group  // Treatment month (0 for never-treated)
qui gen K = month - Ei if Ei != 0  // Relative time (missing for never-treated)
qui gen T = (K >= 0 & Ei != 0)     // Treatment indicator

* Create event study dummies based on actual data range
qui sum K if K != .
local k_min = `r(min)'
local k_max = `r(max)'
di "Relative time range: `k_min' to `k_max'"

* Leads (pre-treatment periods, K < 0)
forvalues l = 2/`=abs(`k_min')' {
    qui gen F`l'event = (K == -`l') if K != .
    qui count if F`l'event == 1
}
* Lags (post-treatment periods, K >= 0)
forvalues l = 0/`k_max' {
    qui gen L`l'event = (K == `l') if K != .
    qui count if L`l'event == 1
}


*** Static imputation
bootstrap imput=e(imput_effect) apt=e(apt_effect) att=e(att_level) scale=e(scale_factor), ///
    reps(1000) cluster(id) seed(12345): static_imput_boot

estimates store s_imput_sin_intersec_km

*** Dynamic imputation
bootstrap, reps(1000) cluster(id) idcluster(newid) seed(12345): dynamic_imput_boot

estimates store d_imput_sin_intersec_km




*** TODOS OS SINISTROS NAS VIAS COM MAIS INTERSECCOES, POR KM ***

import delimited "C:\Dev\faixa-azul\stata\input\km_todos_interseccoes.csv", clear

gen Y = sinistros
gen month = periodo
gen group = coorte

* Create treatment indicator and relative time
qui gen Ei = group  // Treatment month (0 for never-treated)
qui gen K = month - Ei if Ei != 0  // Relative time (missing for never-treated)
qui gen T = (K >= 0 & Ei != 0)     // Treatment indicator

* Create event study dummies based on actual data range
qui sum K if K != .
local k_min = `r(min)'
local k_max = `r(max)'
di "Relative time range: `k_min' to `k_max'"

* Leads (pre-treatment periods, K < 0)
forvalues l = 2/`=abs(`k_min')' {
    qui gen F`l'event = (K == -`l') if K != .
    qui count if F`l'event == 1
}
* Lags (post-treatment periods, K >= 0)
forvalues l = 0/`k_max' {
    qui gen L`l'event = (K == `l') if K != .
    qui count if L`l'event == 1
}


*** Static imputation
bootstrap imput=e(imput_effect) apt=e(apt_effect) att=e(att_level) scale=e(scale_factor), ///
    reps(1000) cluster(id) seed(12345): static_imput_boot

estimates store s_imput_todos_intersec_km

*** Dynamic imputation
bootstrap, reps(1000) cluster(id) idcluster(newid) seed(12345): dynamic_imput_boot

estimates store d_imput_todos_intersec_km




*** SINISTROS FATAIS ENVOLVENDO MOTO, POR KM ***

import delimited "C:\Dev\faixa-azul\stata\input\km_moto_fatais.csv", clear

gen Y = sinistros
gen month = periodo
gen group = coorte

* Create treatment indicator and relative time
qui gen Ei = group  // Treatment month (0 for never-treated)
qui gen K = month - Ei if Ei != 0  // Relative time (missing for never-treated)
qui gen T = (K >= 0 & Ei != 0)     // Treatment indicator

* Create event study dummies based on actual data range
qui sum K if K != .
local k_min = `r(min)'
local k_max = `r(max)'
di "Relative time range: `k_min' to `k_max'"

* Leads (pre-treatment periods, K < 0)
forvalues l = 2/`=abs(`k_min')' {
    qui gen F`l'event = (K == -`l') if K != .
    qui count if F`l'event == 1
}
* Lags (post-treatment periods, K >= 0)
forvalues l = 0/`k_max' {
    qui gen L`l'event = (K == `l') if K != .
    qui count if L`l'event == 1
}


*** Static imputation
bootstrap imput=e(imput_effect) apt=e(apt_effect) att=e(att_level) scale=e(scale_factor), ///
    reps(1000) cluster(id) seed(12345): static_imput_boot

estimates store s_imput_sin_fatal_km

*** Dynamic imputation
bootstrap, reps(1000) cluster(id) idcluster(newid) seed(12345): dynamic_imput_boot

estimates store d_imput_sin_fatal_km




*** SINISTROS ENVOLVENDO MOTO, POR KM, BIMESTRAL ***

import delimited "C:\Dev\faixa-azul\stata\input\km_moto_bimestral.csv", clear

gen Y = sinistros
gen month = periodo
gen group = coorte

* Create treatment indicator and relative time
qui gen Ei = group  // Treatment month (0 for never-treated)
qui gen K = month - Ei if Ei != 0  // Relative time (missing for never-treated)
qui gen T = (K >= 0 & Ei != 0)     // Treatment indicator

* Create event study dummies based on actual data range
qui sum K if K != .
local k_min = `r(min)'
local k_max = `r(max)'
di "Relative time range: `k_min' to `k_max'"

* Leads (pre-treatment periods, K < 0)
forvalues l = 2/`=abs(`k_min')' {
    qui gen F`l'event = (K == -`l') if K != .
    qui count if F`l'event == 1
}
* Lags (post-treatment periods, K >= 0)
forvalues l = 0/`k_max' {
    qui gen L`l'event = (K == `l') if K != .
    qui count if L`l'event == 1
}


*** Static imputation
bootstrap imput=e(imput_effect) apt=e(apt_effect) att=e(att_level) scale=e(scale_factor), ///
    reps(1000) cluster(id) seed(12345): static_imput_boot

estimates store s_imput_sin_moto_km_bi

*** Dynamic imputation
bootstrap, reps(1000) cluster(id) idcluster(newid) seed(12345): dynamic_imput_boot

estimates store d_imput_sin_moto_km_bi






estimates save stata/output/imput/imput.dta, replace
estimates use "stata/output/imput/imput.dta", clear

estimates dir
* estimates restore name





// estimates restore d_imput_todos_atrop_km
// estimates restore d_imput_todos_hora_km
estimates restore d_imput_todos_intersec_km

_coef_table, level(95)
matrix R = r(table)

* Create a temporary dataset
clear
local k = colsof(R)
local names : colnames R
set obs `k'

qui gen str20 variable = ""
qui gen double coefficient = .
qui gen double std_error = .
qui gen double t_statistic = .
qui gen double p_value = .
qui gen double lower_ci = .
qui gen double upper_ci = .

forval i = 1/`k' {
    local vname : word `i' of `names'
    replace variable = "`vname'" in `i'
    replace coefficient = R[1,`i'] in `i'
    replace std_error = R[2,`i'] in `i'
    replace t_statistic = R[3,`i'] in `i'
    replace p_value = R[4,`i'] in `i'
    replace lower_ci = R[5,`i'] in `i'
    replace upper_ci = R[6,`i'] in `i'
}

* Export to CSV
export delimited using "stata/output/imput/km_todos_interseccoes_dynamic.csv", replace




