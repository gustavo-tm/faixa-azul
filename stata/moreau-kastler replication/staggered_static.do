/*
	Monte-Carlo simulations of PPML staggered DiD
	
	Date: June 2025
	Author: Ninon Moreau-Kastler (ninon.moreau-kastler@psemail.eu)

*/


clear all

*Here set your path
cd ""


* Set-up
ssc install reghdfe, replace
ssc install ppmlhdfe, replace
ssc install did_imputation, replace
ssc install parallel, replace
ssc install outreg2, replace
ssc install unique, replace
ssc install egenmore, replace


* Adjust for numer of cores
parallel setclusters 7


* Choose simulation case 
global type=4


cap: program drop simone
program simone, rclass


* Simulate data ----------------------------------------------------------------

	clear
	set obs 10000
	gen id = _n
	
	gen alpha_i = rnormal() // Individual effects
	
// 	gen Di = round(runiform()*exp(alpha_i)) // Selection into treatment
// 	replace Di = Di>0
	gen gamma_t = 0 // Time effects
	
	expand 15 // Generate 3 periods
	sort id
	bys id: gen year = _n
	
	gen Ei = ceil(runiform()*7)+15 -6 if year==1	// year when unit is first treated
	bys id (year): replace Ei = Ei[1]
	gen K = year-Ei 								// "relative time", i.e. the number periods since treated (could be missing if never-treated)
	gen T = K>=0 & Ei!=. 
	
	replace gamma_t = log(1.5)*year // Time change in post period
	
	
	****** Here choose the case *********************

	
	if $type == 1 {																// Case 1: no heteroskedasticity + homogeneous treatment effect
	
	gen mulnoise = exp( rnormal( (-1/2)*ln(1 + alpha_i) , sqrt(ln(1 + alpha_i)) ) ) // Log-normal error term, with variance function of individual heterogeneity
	su mulnoise	
	
	gen YU_it = exp(alpha_i + gamma_t) * mulnoise //DGP of counterfactual model
	replace YU_it = 0 if YU_it<0
	
	gen tau = cond(T==1,log(abs(year-12.5)), 0) 			// heterogeneous treatment effects (in this case vary over calendar periods)
	
		
	}	
		
	if $type == 2 {																// Case 2: heteroskedasticity + homogeneous treatment effect
	
	gen mulnoise = exp( rnormal( (-1/2)*ln(1 + exp(0.2*T)) , sqrt(ln(1 + exp(0.2*T))) ) ) // Log-normal error term, with variance function of observables 
	su mulnoise
	
	gen YU_it = exp(alpha_i + gamma_t) * mulnoise //DGP of counterfactual model
	replace YU_it = 0 if YU_it<0
	
	gen tau = cond(T==1,log(abs(year-12.5)), 0) 			// heterogeneous treatment effects (in this case vary over calendar periods)
	
		
	}
	
	if $type == 3 {																// Case 3: no heteroskedasticity + heterogeneous treatment effect
	
	gen mulnoise = exp( rnormal( (-1/2)*ln(1 + alpha_i) , sqrt(ln(1 + alpha_i)) ) ) // Log-normal error term, with variance function of individual heterogeneity
	su mulnoise	
	
	gen YU_it = exp(alpha_i + gamma_t) * mulnoise //DGP of counterfactual model
	replace YU_it = 0 if YU_it<0
	
	bysort id: g het_i=rnormal(0,0.5)					// heterogeneous treatment effects (in this case vary over calendar periods and individuals)
	gen tau = cond(T==1,log(abs(year-12.5))+het_i, 0) 
	
	}
	
	if $type == 4 {																// Case 4: heteroskedasticity + heterogeneous treatment effect
	
	gen mulnoise = exp( rnormal( (-1/2)*ln(1 + exp(0.2*T)) , sqrt(ln(1 + exp(0.2*T))) ) ) // Log-normal error term, with variance function of observables 
	su mulnoise
	
	gen YU_it = exp(alpha_i + gamma_t) * mulnoise //DGP of counterfactual model
	replace YU_it = 0 if YU_it<0
	
	bysort id: g het_i=rnormal(0,0.5)					// heterogeneous treatment effects (in this case vary over calendar periods and individuals)
	gen tau = cond(T==1,log(abs(year-12.5))+het_i, 0) 

		
	}


	***************************************************


	gen Y_it = exp(alpha_i + gamma_t + tau*T) * mulnoise // Switching equation
	*replace Y_it = 0 if Y_it<0

	
	
	*Store true values
	su tau if T & Y_it!=.
	global true_par = exp(r(mean))-1

 	g e_tau=exp(tau)-1
	su e_tau if T & Y_it!=.
	global true_gr = r(mean)
	
	sum Y_it if T
	local num=r(mean)
	sum YU_it if T & Y_it!=.
	global true_avgr = `num' / r(mean) -1 // Proportional effect on the average
	
	
* Variable creation ------------------------------------------------------------
	
	gen lY_it = log(Y_it)

	*Leads and lags
	forvalues l = 0/5 {
	gen L`l'event = K==`l'
	}

	forvalues l = 1/14 {
	gen F`l'event = K==-`l'
	}
	
	*Dimensions
	qui sum K
	global ymax = `r(max)'															// Last relative treatment time

	unique(Ei)
	global N=`r(unique)'															// Number of treated cohorts 
	
	*Cohort dummies
	qui tab Ei, gen(d) 						     					// Cohort dummies	
	
	mat N=J(1,($N * ($ymax +1))*2,.)
	
	mat lis N
	
	*Interaction terms (for Wooldridge, 2023) 
			local l=0	
	forvalues n=1/$N {				
										// Interactions for lags 
		local temp=""	
		forvalues i=0/$ymax {
			local l=`l'+1
			local k=`l'*2
			local temp = "`temp'"+"i.T#c.d`n'#c.L`i'event "
			
			qui count if d`n' & L`i'event & Y_it!=.
			mat N[1,`k'-1]=0
			mat N[1,`k']=r(N)
		}
		global int`n'="`temp'"

	}
	

	
	di "$int1"

	*Interaction terms as FE
	egen inter=group(Ei K)
	replace inter=0 if T==0  
	
	

* Estimation -------------------------------------------------------------
	
	
	* Imputation estimator: proposed estimator
	g omega1=(T==1)
	ppmlhdfe Y_it if omega1==0, a(ife=id tfe=year)
	local cst = _b[_cons]

	bysort id: egen ai=max(ife)
	bysort year: egen at=max(tfe)
	
	gen hatYUit = exp(ai + at + `cst')
	
	g delta=Y_it-hatYUit
	sum delta if T
	local att=r(mean)

	sum hatYUit if T & Y_it!=.
	local scale=r(mean)

	global imput = `att' / `scale' // Proportional effect on the average
	
	g APT = (Y_it) / (hatYUit)
	sum APT if omega1==1
	
	*Interaction estimator: Wooldridge 2023, summed up with Aggregation approach: Nagengast & Yotov 2025
	cap drop ife tfe ai at delta hatYUit
	qui ppmlhdfe Y_it ${int1} ${int2} ${int3} ${int4} ${int5} ${int6} ${int7}, absorb(ife=id tfe=year) d
	
	mat coef=e(b)[1,1..84]
	
	local l=0	
	forvalues n=1/$N {				
										// Interactions for lags 
		local temp=""	
		forvalues i=0/$ymax {
			local l=`l'+1
			local k=`l'*2
			
			count if d`n' & L`i'event & Y_it!=. & !d7 & e(sample)
			
			mat N[1,`k'-1]=0
			mat N[1,`k']=r(N)
		}
	}
	
	mat sum=coef*N'
	
	count if T==1 & e(sample)
	global aggregation=exp(sum[1,1]/r(N))-1
	
	bysort id: egen ai=max(ife)
	bysort year: egen at=max(tfe)
	
	gen hatYUit = exp(ai + at + _b[_cons])
	
	qui margins, dydx(T) ///
		subpop(if T==1) noestimcheck  // Average partial effect
	local num=r(b)[1,2]
	qui sum hatYUit if T & Y_it!=.
	local denom=`r(mean)'
	global interaction=`num'/`denom'
	
	
	*Log OLS staggered robust: Borusyak et al. (2024)
	did_imputation lY_it id year Ei
	global ols_rob_stag=exp(e(b)[1,1])-1
	
	
	*Twoway fixed effects
	*Non linear
	ppmlhdfe Y_it T, a(id year) // PPML estimator
	global bppml = exp(_b[T])-1
	
	*Linear
	reghdfe lY_it T, a(id year)  // Log-OLS estimator
	global bols = exp(_b[T])-1
	
	
	dis " ppml_twfe: $bppml ols_twfe: $bols  exp of average parameter: $true_par true average growth rate: $true_gr true gr of average: $true_avgr imputation: $imput interaction: $interaction ols rob stag: $ols_rob_stag aggregation: $aggregation"

	
	
	return scalar true_par = $true_par
	return scalar true_gr = $true_gr
	return scalar true_avgr = $true_avgr
	return scalar bppml = $bppml
	return scalar bols = $bols
	return scalar imput = $imput
	return scalar interaction = $interaction
	return scalar ols_rob_stag= $ols_rob_stag
	return scalar aggregation= $aggregation

	
end	
	
set seed 9876           // So that it always gives the same results
parallel sim,  expr(ols = r(bols) ppml = r(bppml) true_gr=r(true_gr) true_par=r(true_par) true_avgr=r(true_avgr) imput=r(imput) aggregation=r(aggregation) interaction=r(interaction) ols_rob_stag=r(ols_rob_stag)) reps(1000) saving("exogenous.dta", replace) : simone


use "exogenous.dta", clear 

if $type == 1 {
	
	save "exogenous_21.dta", replace
	
}

if $type == 2 {
	
	save "exogenous_22.dta", replace
	
}

if $type == 3 {
	
	save "exogenous_23.dta", replace
	
}

if $type == 4 {
	
	save "exogenous_24.dta", replace
	
}


*Use the correct file if you don't want to re-do the whole simulation
if $type == 1 {
	
	use "exogenous_21.dta", clear
	
}

if $type == 2 {
	
	use "exogenous_22.dta", clear
	
}

if $type == 3 {
	
	use "exogenous_23.dta", clear
	
}

if $type == 4 {
	
	use "exogenous_24.dta", clear
	
}


label variable true_par "exp(\delta)"
label variable true_gr "\overline{exp(\delta_i)}"
label variable ols "Log-OLS"
label variable ppml "PPML"
label variable imput "Imputation: ratio of expectation"

order true_par true_gr ols ppml imput 

su true_par
local true_par = r(mean)

su true_gr
local true_gr = r(mean)

su true_avgr
local true_avgr = r(mean)



if $type == 1 {
	
	twoway (kdensity ols, xaxis(1 2))  (kdensity ppml) (kdensity imput) (kdensity interaction) (kdensity aggregation) (kdensity ols_rob_stag), xline(`true_par' `true_avgr')   graphregion(color(white)) xlabel(`true_par' "Parameter" `true_avgr' "  Growth rate of the av.", axis(2) angle(15)) legend(order(1 "TWFE Log-OLS" 2 "TWFE PPML" 4 "Imputation" 5 "Aggregation" 6 "Robust log-OLS") rows(3) position(6) region(style(none)) size(small)) xtitle("") xtitle("",axis(2)) title("Case 1")

graph save simu_21.gph, replace
graph export simu_21.pdf, replace 

su true_par true_gr true_avgr ols ppml imput aggregation ols_rob_stag
outreg2 using simu_21.tex, replace sum(detail) keep(true_par true_gr true_avgr ols ppml imput aggregation ols_rob_stag) eqkeep(mean sd min max) label
	
	
}

if $type == 2 {
	
	twoway (kdensity ols, xaxis(1 2))  (kdensity ppml) (kdensity imput) (kdensity interaction) (kdensity aggregation) (kdensity ols_rob_stag), xline(`true_par' `true_avgr')   graphregion(color(white)) xlabel(`true_par' "Parameter" `true_avgr' "  Growth rate of the av.", axis(2) angle(15)) legend(order(1 "TWFE Log-OLS" 2 "TWFE PPML" 4 "Imputation" 5 "Aggregation" 6 "Robust log-OLS") rows(3) position(6) region(style(none)) size(small)) xtitle("") xtitle("",axis(2)) title("Case 2")

graph save simu_22.gph, replace
graph export simu_22.pdf, replace 

su true_par true_gr true_avgr ols ppml imput aggregation ols_rob_stag
outreg2 using simu_22.tex, replace sum(detail) keep(true_par true_gr true_avgr ols ppml imput aggregation ols_rob_stag) eqkeep(mean sd min max) label
	
}

if $type == 3 {
	
	su true_par
local true_par = r(mean)

su true_gr
local true_gr = r(mean)

su true_avgr
local true_avgr = r(mean)
	
	twoway (kdensity ols, xaxis(1 2))  (kdensity ppml) (kdensity imput) (kdensity interaction) (kdensity aggregation) (kdensity ols_rob_stag), xline(`true_par' `true_avgr')   graphregion(color(white)) xlabel(`true_par' "Parameter" `true_avgr' "  Growth rate of the av.", angle(15) axis(2)) legend(order(1 "TWFE Log-OLS" 2 "TWFE PPML" 4 "Imputation/Interaction" 5 "Aggregation" 6 "Robust log-OLS") rows(3) position(6) region(style(none)) size(small)) xtitle("") xtitle("",axis(2)) title("Case 3")

graph save simu_23.gph, replace
graph export simu_23.pdf, replace 

su true_par true_gr true_avgr ols ppml imput aggregation ols_rob_stag
outreg2 using simu_23.tex, replace sum(detail) keep(true_par true_gr true_avgr ols ppml imput aggregation ols_rob_stag) eqkeep(mean sd min max) label

}

if $type == 4 {

su true_par
local true_par = r(mean)

su true_gr
local true_gr = r(mean)

su true_avgr
local true_avgr = r(mean)

twoway (kdensity ols, xaxis(1 2))  (kdensity ppml) (kdensity imput) (kdensity interaction) (kdensity aggregation) (kdensity ols_rob_stag), xline(`true_par' `true_avgr')   graphregion(color(white)) xlabel(`true_par' "Parameter" `true_avgr' "  Growth rate of the av.", angle(15) axis(2)) legend(order(1 "TWFE Log-OLS" 2 "TWFE PPML" 4 "Imputation/Interaction" 5 "Aggregation" 6 "Robust log-OLS") rows(3) position(6) region(style(none)) size(small)) xtitle("") xtitle("",axis(2)) title("Case 4")

graph save simu_24.gph, replace
graph export simu_24.pdf, replace 

su true_par true_gr true_avgr ols ppml imput aggregation ols_rob_stag
outreg2 using simu_24.tex, replace sum(detail) keep(true_par true_gr true_avgr ols ppml imput aggregation ols_rob_stag) eqkeep(mean sd min max) label		
	
}
	
	
	
grc1leg simu_21.gph simu_22.gph simu_23.gph simu_24.gph, legendfrom(simu_24.gph) xcommon ycommon altshrink 

graph save simu_2.gph, replace
graph export simu_2.pdf, replace 

	
