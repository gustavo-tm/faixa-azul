

clear all
program drop _all


global por_km = 0 			// 1 or 0

if $por_km == 0 {
	import delimited "C:\Dev\faixa-azul\dados_tratados\df_did_psm.csv"
}
if $por_km == 1 {
	import delimited "C:\Dev\faixa-azul\dados_tratados\df_did-km_psm.csv"
}


gen Y = sinistros
gen month = mes
gen group = data_implementacao

* Create treatment indicator and relative time
gen K = month - group if group != 0  // Relative time (missing for never-treated)
gen T = (K >= 0 & group != 0)     // Treatment indicator


program define aggregate_imput_boot, eclass

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


* Test the program first (without bootstrap)
// di "Testing the estimator..."
// aggregate_imput_boot
//
// di "Main results:"
// di "Imputation effect (ATT/scale): " e(imput_effect)
// di "APT effect (mean Y/hatY): " e(apt_effect)
// di "ATT level: " e(att_level)
// di "Scale factor: " e(scale_factor)


bootstrap imput=e(imput_effect) apt=e(apt_effect) att=e(att_level) scale=e(scale_factor), ///
    reps(50) cluster(id) seed(12345): aggregate_imput_boot

* Store bootstrap results
estimates store imput_static_bootstrap


