
clear all
program drop _all

cd "C:\Dev\faixa-azul"


*** SINISTROS ENVOLVENDO MOTO ***
import delimited "C:\Dev\faixa-azul\stata\input\moto.csv", clear

qui gen Y = sinistros
qui gen month = periodo
qui gen group = coorte

jwdid Y if group != 121 & group != 85, ///
	ivar(id) tvar(month) gvar(group) never method(poisson)

estat simple
estat event, window(-12, 12)

// estat plot, pstyle1(p1) xtitle("Meses") ytitle("Efeito") legend(off) // ylabel(-0.3(0.1)0.3) // ylabel(-0.8(0.2)0.8)

// graph save "C:\Dev\faixa-azul\stata\plots\jwdid\gravidade\fatal-todos.gph", replace
// graph export "C:\Dev\faixa-azul\stata\plots\jwdid\gravidade\fatal-todos.png", replace




*** TODOS OS SINISTROS ***
import delimited "C:\Dev\faixa-azul\stata\input\todos.csv", clear

qui gen Y = sinistros
qui gen month = periodo
qui gen group = coorte

jwdid Y if group != 121 & group != 85, ///
	ivar(id) tvar(month) gvar(group) never method(poisson)

estat simple
estat event, window(-12, 12)




*** ATROPELAMENTOS ENVOLVENDO MOTO, POR KM ***
// import delimited "C:\Dev\faixa-azul\stata\input\km_moto_atropelamento.csv", clear
//
// qui gen Y = sinistros
// qui gen month = periodo
// qui gen group = coorte
//
// jwdid Y if group != 121 & group != 85, ///
// 	ivar(id) tvar(month) gvar(group) never method(poisson)
//
// estat simple
// estimates store s_jwdid_sin_atrop_km
//
// estat event, window(-12, 12)
// estimates store d_jwdid_sin_atrop_km




*** SINISTROS ENVOLVENDO MOTO EM HORARIO DE PICO, POR KM ***
// import delimited "C:\Dev\faixa-azul\stata\input\km_moto_atropelamento.csv", clear
//
// qui gen Y = sinistros
// qui gen month = periodo
// qui gen group = coorte
//
// jwdid Y if group != 121 & group != 85, ///
// 	ivar(id) tvar(month) gvar(group) never method(poisson)
//
// estat simple
// estimates store s_jwdid_sin_atrop_km
//
// estat event, window(-12, 12)
// estimates store d_jwdid_sin_atrop_km





*** SINISTROS ENVOLVENDO MOTO, BIMESTRAL ***
import delimited "C:\Dev\faixa-azul\stata\input\moto_bimestral.csv", clear

qui gen Y = sinistros
qui gen month = periodo
qui gen group = coorte

jwdid Y if group != 121 & group != 85, ///
	ivar(id) tvar(month) gvar(group) never method(poisson)

estat simple
estat event, window(-12, 12)




