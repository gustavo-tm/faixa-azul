/*
	Simulations of PPML staggered DiD with dynamic effects
	
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

	

* Simulate data -------------------------------------------------------------

	
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


	
	if $type == 99 {																// Case 1: no heteroskedasticity + homogeneous treatment effect
	
	gen mulnoise = exp( rnormal( (-1/2)*ln(2) , sqrt(ln(2)) ) ) // Log-normal error term, with variance function of individual heterogeneity
	su mulnoise	
	
	gen YU_it = exp(alpha_i + gamma_t) * mulnoise //DGP of counterfactual model
	replace YU_it = 0 if YU_it<0
	
	gen tau = cond(T==1,log(abs(K-0.5)), 0) 			// heterogeneous treatment effects (in this case vary over calendar periods)
	
		
	}		
	
	***************************************************


	gen Y_it = exp(alpha_i + gamma_t + tau*T) * mulnoise // Switching equation
	
	
	*Store true values
	su tau if T
	global true_par = exp(r(mean))-1
	
 	g e_tau=exp(tau)-1
	su e_tau if T
	global true_gr = r(mean)
	

	
* Variable creation ------------------------------------------------------------
	
	gen lY_it = log(Y_it)

	*Leads and lags
	forvalues l = 0/5 {
	gen L`l'event = K==`l'
	}

	forvalues l = 2/14 {
	gen F`l'event = K==-`l'
	}
	
	g omega1=(T==1)

	
	*Dimensions
	qui sum K
	global ymin = `r(min)'+1
	global ymax = `r(max)'														// Last relative treatment time

	unique(Ei) if T==1
	global N=`r(unique)'														// Number of treated cohorts
	
	qui unique K
	global nb_coef=`r(unique)'-2
	
	*Cohort dummies
	qui tab Ei, gen(d) 						     								// Cohort dummies
	
	mat N=J(1,($N * ($ymax +1))*2,.)
	
	mat lis N
	
	*Interaction terms (Wooldridge, 2023) 
			local l=0	
	forvalues n=1/$N {				
										// Interactions for lags 
		local temp=""	
		forvalues i=0/$ymax {
			local l=`l'+1
			local k=`l'*2
			local temp = "`temp'"+"i.T#c.d`n'#c.L`i'event "
			
			qui count if d`n' & L`i'event
			mat N[1,`k'-1]=0
			mat N[1,`k']=r(N)
		}
		global int`n'="`temp'"

	}
	
	di "$int1"
	
	
	forvalues n=1/$N {															// Interactions for leads
		local temp=""
		forvalues i=$ymin /-2{
			local j=abs(`i')
			local temp = "`temp'"+"i.T#c.d`n'#c.F`j'event "
		}
		global preint`n'="`temp'"
	}	
	
	
	di "$preint1"

	
	
* Matrices to store results ----------------------------------------------------

																				// Heterogeneous time effects
local names = ""																// Colnames of matrices


forvalue n=$ymin /-2 {															// Leads
		local i = abs(`n')
		local names = "`names'"+" pre`i'"
	}
	
forvalue n=0/$ymax {															// Lags
		local names = "`names'"+" post`n'"
	}
global names = "`names'"	


foreach type in imputation interaction {
	
	matrix `type'_att_b = J(1,$nb_coef ,.)										// Create matrices: ATT
	matrix `type'_att_v = J(1,$nb_coef ,.)
																				// Add names
	matrix colnames `type'_att_b = $names										
	matrix colnames `type'_att_v = $names
	
}


foreach type in imputation interaction aggregation {
	
	matrix `type'_ptt_b = J(1,$nb_coef ,.)										// Create matrices: PTT
	matrix `type'_ptt_v = J(1,$nb_coef ,.)
																				// Add names
	matrix colnames `type'_ptt_b = $names										
	matrix colnames `type'_ptt_v = $names
	
}


matrix true_par = J(1,$nb_coef ,.)												// Create matrices: PTT
matrix true_avgr = J(1,$nb_coef ,.)												// Create matrices: PTT

	matrix colnames true_par = $names										
	matrix colnames true_avgr = $names


* Estimation -------------------------------------------------------------
	
	
		* Imputation estimator: proposed estimator
program imput_boot, eclass	

	matrix imputation_att_b = J(1,$nb_coef ,.)									// Create matrices: ATT	
	matrix colnames imputation_att_b = $names									
	
	matrix imputation_ptt_b = J(1,$nb_coef ,.)									// Create matrices: PTT
	matrix colnames imputation_ptt_b = $names										


cap drop ife tfe ai at hatYUit delta 
	* Imputation estimator
	ppmlhdfe Y_it if K==-1 | Ei==16, a(ife=id tfe=year) noomitted keepsing separation(fe)
	local cst = _b[_cons]

	bysort id: egen ai=max(ife)
	bysort year: egen at=max(tfe)
	
	gen hatYUit = exp(ai + at + `cst')
	
	g delta=Y_it-hatYUit


	*Store lags		

	forvalues i=0/$ymax {	
		local j=`i'+abs($ymin )		
		qui sum delta if L`i'event==1
		matrix imputation_att_b[1,`j']=`r(mean)'
	}
	
	forvalues i=0/$ymax {	
		local j=`i'+abs($ymin )
		qui sum hatYUit if L`i'event==1 & Y_it!=.
		matrix imputation_ptt_b[1,`j']=log(imputation_att_b[1,`j']/`r(mean)'+1)
	}
	
	*Store leads 
forvalue i=$ymin /-2 {				
		local j = abs($ymin )+`i' +1
		local k=abs(`i')
		qui sum delta if F`k'event==1
		matrix imputation_att_b[1,`j']=`r(mean)'
	}
	
forvalue i=$ymin /-2 {			
		local j = abs($ymin )+`i' +1
		local k=abs(`i')
		qui sum hatYUit if F`k'event==1 & Y_it!=.
		matrix imputation_ptt_b[1,`j']=log(imputation_att_b[1,`j']/`r(mean)'+1)
	}
	
	
	 ereturn post imputation_ptt_b
	
 end

 parallel bs, reps(500)  cluster(id) idcluster(newid): imput_boot

 estimate store imputation
	
	mat imputation_ptt_b=e(b)
	
	forvalue i=1/19{
		mat imputation_ptt_v[1,`i']=e(V)[`i',`i']
	}
	

	
	
program aggreg_boot, eclass	
								
	
	matrix aggregation_ptt_b = J(1,$nb_coef ,.)										// Create matrices: PTT
	matrix colnames aggregation_ptt_b = $names	
	
	qui ppmlhdfe Y_it ${preint1} ${preint2} ${preint3} ${preint4} ${preint5} ${preint6} ${int1} ${int2} ${int3} ${int4} ${int5} ${int6}, absorb(id year) d noomitted keepsing separation(fe)
	
	mat coef=e(b)
	
*	mat sum=coef*N'
	
	forvalues i=$ymin /-2 {														// Store leads
		local k=abs(`i')
		local j = `i'+abs($ymin ) +1
		local temp=0	

		forvalues n=1/$N {				
			
			qui count if d`n' & F`k'event  & e(sample)
			local temp = `temp'+r(N)*(-coef[1,"0b.T#c.d`n'#c.F`k'event"])

		}
		qui count if F`k'event & !d7 & e(sample)
		di `temp'

		matrix aggregation_ptt_b[1,`j']=`temp'/r(N)

	}
																				// Store lags 
	forvalues i=0/$ymax {
		local temp=0	
		local j=`i'+abs($ymin )
			forvalues n=1/$N {				

			qui count if d`n' & L`i'event  & e(sample)
			local temp = `temp'+r(N)*coef[1,"1.T#c.d`n'#c.L`i'event"]

		}
		qui count if L`i'event  & e(sample)

		matrix aggregation_ptt_b[1,`j']=`temp'/r(N)

	}
	
	
		 ereturn post aggregation_ptt_b
	
 end


  parallel bs, reps(500) cluster(id) idcluster(newid): aggreg_boot
	estimate store aggregation
	
	mat aggregation_ptt_b=e(b)
	
	forvalue i=1/19{
		mat aggregation_ptt_v[1,`i']=e(V)[`i',`i']
	}


	
	*Log OLS staggered robust: Borusyak et al. (2024)
	did_imputation lY_it id year Ei, allhorizons pretrend(5)
	estimates store bjs
	
	*Twoway fixed effects
	*Non-linear
	ppmlhdfe Y_it F*event L*event, a(id year) // PPML estimator
	estimates store ppml
	
	*Linear
	reghdfe lY_it F*event L*event, a(id year)  // Log-OLS estimator
	estimates store ols
	
	
	
	*Store true values
	
	*Leads
	forvalue i=$ymin /-2 {
	local k=abs(`i')
	local j = `i'+abs($ymin ) +1
	
	matrix true_par[1,`j']=0
	matrix true_avgr[1,`j']=0

	}
	
	*Lags
	forvalues i=0/$ymax {	
	local j=`i'+abs($ymin )

	su tau if L`i'event==1 & Y_it!=.
	matrix true_par[1,`j']=r(mean)

	sum Y_it if L`i'event==1
	local num=r(mean)
	sum YU_it if L`i'event==1 & Y_it!=.
	matrix true_avgr[1,`j']=log(`num' / r(mean))

	}
	
			if $type == 1 {																
	
save results/simu_eventstudy_1.dta, replace
		
	}	

	
		if $type == 2 {																
	
save results/simu_eventstudy_2.dta, replace
		
	}	
	
	
		if $type == 3 {															
	
save results/simu_eventstudy_3.dta, replace
		
	}	
	

			if $type == 4 {																
	
save results/simu_eventstudy_4.dta, replace
		
	}	
	

event_plot true_avgr# true_par# imputation_ptt_b#imputation_ptt_v aggregation_ptt_b#aggregation_ptt_v ols ppml, ///
	stub_lead(pre# pre# pre# pre# F#event F#event ) stub_lag(post# post# post# post# L#event L#event) plottype(scatter) ciplottype(rcap) ///
	together trimlead(5) noautolegend ///
	graph_opt(xtitle(" ") ytitle("Coefficient") xlabel(-5(1)5) yline(0, lpattern(dash) lcolor(gray)) ///
		legend(order(1 "True PTT" 2 "Av. true par" 3 "Imputation/Interaction" 5 "Aggregation" 7 "TWFE OLS" 9 "TWFE PPML" ) rows(3) position(6) region(style(none))) name(graph_ptt, replace))	///
	lag_opt1(msymbol(O) color(cranberry)) lag_ci_opt1(color(cranberry)) ///
	lag_opt2(msymbol(+) color(cranberry)) lag_ci_opt2(color(cranberry)) ///
	lag_opt3(msymbol(Dh) color(navy)) lag_ci_opt3(color(navy)) ///
	lag_opt4(msymbol(Th) color(forest_green)) lag_ci_opt4(color(forest_green)) ///
	lag_opt5(msymbol(Sh) color(dkorange)) lag_ci_opt5(color(dkorange)) ///
	lag_opt6(msymbol(Oh) color(purple)) lag_ci_opt6(color(purple)) 

event_plot true_avgr# true_par# imputation_ptt_b#imputation_ptt_v aggregation_ptt_b#aggregation_ptt_v ols ppml, ///
	stub_lead(pre# pre# pre# pre# F#event F#event ) stub_lag(post# post# post# post# L#event L#event) plottype(scatter) ciplottype(rcap) ///
	together trimlead(5) noautolegend ///
	graph_opt(xtitle(" ") ytitle("Coefficient") xlabel(-5(1)5) yline(0, lpattern(dash) lcolor(gray)) ///
		legend(order(1 "True PTT" 2 "Av. true par" 3 "Imputation/Interaction" 5 "Aggregation" 7 "TWFE OLS" 9 "TWFE PPML" ) rows(3) position(6) region(style(none))) name(graph_ptt, replace))	///
	lag_opt1(msymbol(O) color(cranberry)) lag_ci_opt1(color(cranberry)) ///
	lag_opt2(msymbol(S) color(cranberry)) lag_ci_opt2(color(cranberry)) ///
	lag_opt3(msymbol(Oh) msize(medium) color(forest_green)) lag_ci_opt3(color(forest_green)) ///
	lag_opt4(msymbol(Dh) msize(medium) color(navy)) lag_ci_opt4(color(navy)) ///
	lag_opt5(msymbol(Sh) color(purple)) lag_ci_opt5(color(purple)) ///
	lag_opt6(msymbol(Oh) color(purple)) lag_ci_opt6(color(purple)) 



	
		if $type == 4 {		
			
	
graph save results/simu_eventstudy_4.gph, replace
graph export results/simu_eventstudy_4.pdf, replace 	

use results/simu_eventstudy_4.dta, clear

event_plot true_avgr#, ///
	stub_lead(pre#  ) stub_lag(post# ) plottype(scatter) ciplottype(rcap) ///
	together trimlead(5) noautolegend ///
	graph_opt(xtitle(" ") ytitle("Coefficient") xlabel(-5(1)5) yline(0, lpattern(dash) lcolor(gray)) ///
		legend(order(1 "True PTT") rows(3) position(6) region(style(none))) name(graph_ptt, replace))	///
	lag_opt1(msymbol(O) msize(medium)  color(cranberry)) lag_ci_opt1(color(cranberry))
	
graph save results/simu_eventstudy_4_1.gph, replace
graph export results/simu_eventstudy_4_1.pdf, replace 	

event_plot true_avgr# true_par# , ///
	stub_lead(pre# pre#  ) stub_lag(post# post# ) plottype(scatter) ciplottype(rcap) ///
	together trimlead(5) noautolegend ///
	graph_opt(xtitle(" ") ytitle("Coefficient") xlabel(-5(1)5) yline(0, lpattern(dash) lcolor(gray)) ///
		legend(order(1 "True RoR" 2 "Av. true par" ) rows(2) position(6) region(style(none))) name(graph_ptt, replace))	///
	lag_opt1(msymbol(O) msize(medium) color(cranberry)) lag_ci_opt1(color(cranberry)) ///
	lag_opt2(msymbol(S) msize(medium) color(cranberry)) lag_ci_opt2(color(cranberry)) 

graph save results/simu_eventstudy_4_2.gph, replace
graph export results/simu_eventstudy_4_2.pdf, replace 	

event_plot true_avgr# true_par# ols, ///
	stub_lead(pre# pre# F#event ) stub_lag(post# post#  L#event ) plottype(scatter) ciplottype(rcap) ///
	together trimlead(5) noautolegend ///
	graph_opt(xtitle(" ") ytitle("Coefficient") xlabel(-5(1)5) yline(0, lpattern(dash) lcolor(gray)) ///
		legend(order(1 "True PTT" 2 "Av. true par" 3 "TWFE OLS" ) rows(2) position(6) region(style(none))) name(graph_ptt, replace))	///
	lag_opt1(msymbol(O)  color(cranberry)) lag_ci_opt1(color(cranberry)) ///
	lag_opt2(msymbol(S) color(cranberry)) lag_ci_opt2(color(cranberry)) ///
	lag_opt3(msymbol(Sh) msize(medium) color(purple)) lag_ci_opt3(color(purple)) 

graph save results/simu_eventstudy_4_3.gph, replace
graph export results/simu_eventstudy_4_3.pdf, replace 	
	
		
event_plot true_avgr# true_par# ols ppml, ///
	stub_lead(pre# pre# F#event F#event ) stub_lag(post# post# L#event L#event) plottype(scatter) ciplottype(rcap) ///
	together trimlead(5) noautolegend ///
	graph_opt(xtitle(" ") ytitle("Coefficient") xlabel(-5(1)5) yline(0, lpattern(dash) lcolor(gray)) ///
		legend(order(1 "True PTT" 2 "Av. true par" 3 "TWFE OLS" 5 "TWFE PPML" ) rows(3) position(6) region(style(none))) name(graph_ptt, replace))	///
	lag_opt1(msymbol(O)  color(cranberry)) lag_ci_opt1(color(cranberry)) ///
	lag_opt2(msymbol(S)  color(cranberry)) lag_ci_opt2(color(cranberry)) ///
	lag_opt3(msymbol(Sh) msize(medium) color(purple)) lag_ci_opt3(color(purple)) ///
	lag_opt4(msymbol(Oh) msize(medium) color(purple)) lag_ci_opt4(color(purple)) 

graph save results/simu_eventstudy_4_4.gph, replace
graph export results/simu_eventstudy_4_4.pdf, replace 	


event_plot true_avgr# true_par# aggregation_ptt_b#aggregation_ptt_v ols ppml, ///
	stub_lead(pre# pre# pre# F#event F#event ) stub_lag(post# post# post# L#event L#event) plottype(scatter) ciplottype(rcap) ///
	together trimlead(5) noautolegend ///
	graph_opt(xtitle(" ") ytitle("Coefficient") xlabel(-5(1)5) yline(0, lpattern(dash) lcolor(gray)) ///
		legend(order(1 "True PTT" 2 "Av. true par" 3 "Aggregation" 5 "TWFE OLS" 7 "TWFE PPML" ) rows(3) position(6) region(style(none))) name(graph_ptt, replace))	///
	lag_opt1(msymbol(O) color(cranberry)) lag_ci_opt1(color(cranberry)) ///
	lag_opt2(msymbol(S) color(cranberry)) lag_ci_opt2(color(cranberry)) ///
	lag_opt3(msymbol(Dh) color(navy)) lag_ci_opt3(color(navy)) ///
	lag_opt4(msymbol(Sh) msize(medium) color(purple)) lag_ci_opt4(color(purple)) ///
	lag_opt5(msymbol(Oh) color(purple)) lag_ci_opt5(color(purple)) 
		
graph save results/simu_eventstudy_4_5.gph, replace
graph export results/simu_eventstudy_4_5.pdf, replace 	
	
	
	}	
