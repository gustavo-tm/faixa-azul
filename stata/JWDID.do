clear all
program drop _all


global por_km = 0 			// 1 or 0

if $por_km == 0 {
	import delimited "C:\Dev\faixa-azul\dados_tratados\df_did_psm.csv"
}
if $por_km == 1 {
	import delimited "C:\Dev\faixa-azul\dados_tratados\df_did-km_psm.csv"
}

gen Y = sinistros_hora_05_10
gen month = mes
gen group = data_implementacao


jwdid Y , ivar(id) tvar(month) gvar(group) never method(poisson)




estat simple


estat event, window(-12, 12)
estat plot, pstyle1(p1) xtitle("Meses") ytitle("Efeito") legend(off) // ylabel(-0.3(0.1)0.3) // ylabel(-0.8(0.2)0.8)

graph save "C:\Dev\faixa-azul\stata\plots\jwdid\gravidade\fatal-todos.gph", replace
graph export "C:\Dev\faixa-azul\stata\plots\jwdid\gravidade\fatal-todos.png", replace
