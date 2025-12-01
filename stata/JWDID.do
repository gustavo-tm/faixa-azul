ssc install hdfe, replace
ssc install jwdid, replace
ssc install estout, replace

clear all
program drop _all

cd "~/Developer/faixa-azul/stata"


* ================================================================================
* RUN JWDID PROGRAM
* ================================================================================

capture program drop run_jwdid
program define run_jwdid
    syntax , FILENAME(string) SAVENAME(string) [NEVERGROUP(numlist) METHOD(string)]
    
    if "`method'" == "" {
        local method "poisson"
    }
    
    import delimited "input/`filename'.csv", clear
    
    qui gen Y = sinistros
    qui gen month = periodo
    qui gen group = coorte
    
    local if_condition ""
    if "`nevergroup'" != "" {
		foreach num of numlist `nevergroup' {
			if "`if_condition'" == "" {
				local if_condition "if group != `num'"
			}
			else {
				local if_condition "`if_condition' & group != `num'"
			}
		}
	}
    
    jwdid Y `if_condition', ///
        ivar(id) tvar(month) gvar(group) method(`method') never
    estimates store jwdid_`savename'
    
    export_jwdid_results, filename("`filename'")
    
    display as result "`filename'"
    display as text "  Excluded groups: `nevergroup'"
end


* ================================================================================
* EXPORT RESULTS PROGRAM
* ================================================================================

capture program drop export_jwdid_results
program define export_jwdid_results
    syntax, FILENAME(string)
	
	display as result "Running simple aggregation..."
    
    estat simple
    matrix simple_table = r(table)
    
    matrix simple_out = (simple_table[1,1], simple_table[2,1], simple_table[3,1], simple_table[4,1], simple_table[5,1], simple_table[6,1])
    matrix colnames simple_out = coefficient std_error t_statistic p_value lower_ci upper_ci
    
    frame create simple_frame
    frame simple_frame {
        clear
        svmat simple_out, names(col)
        export delimited using "output/jwdid/`filename'-s.csv", replace
    }
    frame drop simple_frame
	
	
	display as result "Running dynamic aggregation..."
    
    estat event, window(-12, 12)
    matrix event_table = r(table)'
    
    matrix event_out = (event_table[1..., 1], event_table[1..., 2], event_table[1..., 5], event_table[1..., 4], event_table[1..., 5], event_table[1..., 6])
    matrix colnames event_out = coefficient std_error t_statistic p_value lower_ci upper_ci
    
    frame create event_frame
    frame event_frame {
        clear
        svmat event_out, names(col)
        export delimited using "output/jwdid/`filename'-d.csv", replace
    }
    frame drop event_frame
end


*** SINISTROS ENVOLVENDO MOTO ***

* 1 PADRAO =====
run_jwdid, filename("1-moto-padrao") savename("moto") nevergroup(85 121)

run_jwdid, filename("1-moto-padrao-km") savename("moto_km") nevergroup(85 121)

run_jwdid, filename("1-moto-padrao-bi") savename("moto_bi")

run_jwdid, filename("1-moto-padrao-bi-km") savename("moto_bi_km")


* 2 PICO =====
run_jwdid, filename("2-moto-pico") savename("mpico") nevergroup(85 106 121 123)

run_jwdid, filename("2-moto-pico-bi") savename("mpico_bi") nevergroup(43 53 61)


* 3 ATROPELAMENTO =====
* run_jwdid, filename("3-moto-atrop") savename("matrop") nevergroup(85 106 121 123)

run_jwdid, filename("3-moto-atrop-bi") savename("matrop_bi") nevergroup(43 56 61 64)


* 4 INTERSECCAO =====
run_jwdid, filename("4-moto-inter") savename("minter") nevergroup(114 121)

run_jwdid, filename("4-moto-inter-bi") savename("minter_bi") nevergroup(61)



estimates save output/jwdid/jwdid-moto.dta, replace













