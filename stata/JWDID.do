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



jwdid Y, ivar(id) tbar(month) gvar(group) never method(poisson)
estimates store jw

estat simple

estat event, window(-12, 12)
estimates store jw_event

estat plot


event_plot jw_event, ///
stub_lead(pre#) stub_lag(post#) ///
plottype(scatter) ciplottype(rcap) together noautolegend ///
graph_opt(xtitle("Meses") ytitle("Efeito") xlabel(-12(1)12) yline(0, lpattern(dash) lcolor(gray)) ///
ylabel(-1(0.5)1) legend(off))
