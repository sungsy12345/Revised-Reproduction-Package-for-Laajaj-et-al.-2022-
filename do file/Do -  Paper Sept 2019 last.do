*****************************************************************************************************
*												Ser Pilo Paga 									    *
*									Do File - Motivational Effects of SPP	             			*
*	    Reproduces tables 1, 3, 4, A1, A2, A3 and figures 2, 5, 6 and 7 with Saber 11 results:	    *
*****************************************************************************************************


clear all
cls

/* Danilo Aristizabal
*/
* set a global for the database 

** FOR REPLICATION ADAPT THESE LINKS USING THE LOCATION OF THE FOLDER **
global base "C:\Users\Seung Yong Sung\Dropbox\1. Academics\I. Spring 2022\EC 270B Dev\PS\DataReplication\Authors Replication Files\data\"
global tabla "C:\Users\Seung Yong Sung\Dropbox\1. Academics\I. Spring 2022\EC 270B Dev\PS\DataReplication\Authors Replication Files\tables\"
global figura "C:\Users\Seung Yong Sung\Dropbox\1. Academics\I. Spring 2022\EC 270B Dev\PS\DataReplication\Authors Replication Files\figures\"
global regresiones "C:\Users\Seung Yong Sung\Dropbox\1. Academics\I. Spring 2022\EC 270B Dev\PS\DataReplication\Authors Replication Files\regressions\"

set more off, perm
set scheme sj
set seed 1984
set matsize 11000
set maxvar 32000


*********************************************************
*		    		Saber 3, 5 y 9						*
*********************************************************

 // Crear Indicador de porcentaje de elegibles por institucion educativa //

use codigo_dane_sede elegible_sisben_completo using "${base}base_final_2014_parcial", clear

collapse elegible_sisben_completo, by(codigo_dane_sede)
rename elegible_sisben_completo p_elegible
tempfile elegibles
save `elegibles'

use "${base}Saber_panel_EE", clear

* RANKING *

foreach grade in 3 5 9 {

	egen puntaje_`grade'=rowmean(leng_`grade'_promedio mat_`grade'_promedio)
	
}

foreach puntaje of varlist leng*promedio mat*promedio puntaje* {
egen r_`puntaje'=rank(`puntaje'), by(year)

foreach t in 2009 2012 2013 2014 2015 2016 2017 {
	qui sum r_`puntaje' if year==`t', d
	
	* Ranking over 100
	* replace r_`puntaje'=((r_`puntaje'-r(min))/(r(max)-r(min)))*100 if year==`t'
	* Standardized results
	replace r_`puntaje' = (`puntaje'-r(mean))/r(sd) if year==`t'
}
replace `puntaje'=r_`puntaje'
}

// Departamento

gen depto="" 
tostring MUNI_ID, gen(s_muni)

replace depto=substr(s_muni,1,2) if length(s_muni)==5	
replace depto=substr(s_muni,1,1) if length(s_muni)==4

destring depto, replace

replace NIVEL=. if NIVEL==9 // Missing observations
encode CALENDARIO, gen(calendario) // String

rename CODIGO_DANE_EE codigo_dane_sede
merge m:1 codigo_dane_sede using `elegibles'


drop if year==2009

aorder

save "${base}saber_panel.dta"

* DISTR

preserve

keep if year==2015
histogram p_elegible, xtitle("Share of {it:Sisbén} elegible students") ytitle("")
graph export "${figura}hist_p_eligible.png", replace 

graph drop _all

foreach puntaje of varlist leng_9_desviacion mat_9_desviacion   {
	
	twoway (scatter `puntaje' p_elegible,  msize(.5)) (lfit `puntaje' p_elegible), name("`puntaje'")
	
	}

restore




*********************************************************
*		      F1 Access by SE Strata					*
*********************************************************

* Graph from AEJ: Applied publication

*********************************************************
*		    T Descriptive Statistics (Ranking)			*
*********************************************************

**** RANKING ****

use puntaje_20152 elegible_sisben_completo puntaje_sisben_dnp_20152 ///
elegible_sb11 elegible ranking using "$base2015_2", clear

drop if puntaje_20152==.
*drop if puntaje_sisben_dnp_20152==.

collapse (count) n=ranking (mean) mean=ranking (p25) p25=ranking (p50) p50=ranking (p75) p75=ranking (p90) p90=ranking (p95) p95=ranking , by(elegible_sisben_completo)

* Descriptive statistics for the sisben eligible group, non-elibile group 
* with sisben and non-eligible group without sisben 
export excel "${tabla}descriptivas_elegible", firstrow(variables) sheet("2015", replace) 

use puntaje_20142 elegible_sisben_completo puntaje_sisben_dnp_20142 ///
elegible_sb11 elegible ranking using "$base2014_2", clear

drop if puntaje_20142==.
*drop if puntaje_sisben_dnp_20142==.

collapse (count) n=ranking (mean) mean=ranking (p25) p25=ranking (p50) p50=ranking (p75) p75=ranking (p90) p90=ranking (p95) p95=ranking , by(elegible_sisben_completo)

* Descriptive statistics for the sisben eleigible group, non-elibile group 
* with sisben and non-eligible group without sisben 
export excel "${tabla}descriptivas_elegible", firstrow(variables) sheet("2014", replace) 

* Diff-Diff

use "${completa2}", clear

drop if year==2013
keep if year==2014 | year==2015
*drop if puntaje_sisben_dnp==.

gen elegible_cuartil=.
*Treatment: Elegible
replace elegible_cuartil=1 if categoria==1

* Control: Q1 Non-Elegible with Sisben 
replace elegible_cuartil=0 if categoria>=2

rename year year_
gen year=.
replace year=1 if year_==2015
replace year=0 if year_==2014
gen y_sisben=year*elegible_cuartil

reg ranking elegible_cuartil year y_sisben, r
outreg2 using "${tabla}dd_ds.xls", label ///
ctitle("Agregado") addtext(Cuantil,MCO) replace less(1)

foreach j in .25 .50 .75 .90 {

qreg ranking elegible_cuartil year y_sisben , q(`j') vce(r) 
outreg2 using "${tabla}dd_ds.xls", ///
addtext(Cuantil,`j') ctitle("Agregado") append less(1)

}


*********************************************************
*		     F Eligibility Criteria					*
*********************************************************

global controles area_ciudades area_urbano icfes_padre_nivel1 icfes_padre_nivel2 ///
icfes_padre_nivel3 icfes_madre_nivel1 icfes_madre_nivel2 icfes_madre_nivel3 ///
edad sexo_sisben ///
ranking_col

use puntaje_sisben_dnp max_puntaje_elegible area_sisben_dnp $controles ///
mejor_recibio depa_reside mejor_asistio_sig mejor_asistio ///
elegible_sisben_completo ranking puntaje_20152 col_cod_icfes  ///
depa_reside puntaje_std using "$base2015_2", clear

*drop elegible_sisben2 elegibles 
set seed 1984
gen sisben_=.
replace sisben_=puntaje_sisben_dnp-57.21 if area_sisben_dnp==1
replace sisben_=puntaje_sisben_dnp-56.32 if area_sisben_dnp==2
replace sisben_=puntaje_sisben_dnp-40.75 if area_sisben_dnp==3

twoway (histogram puntaje_sisben_dnp) ///
(scatteri 0 57.21 0.025 57.21 (3), c(l) m(i) color(red) ) ///
(scatteri 0 56.32 0.025 56.32 (9), c(l) m(i) color(red))  ///
(scatteri 0 40.75 0.025 40.75 (9), c(l) m(i) color(red)), ///
	text(0.025 57.21 "14 Cities", place(e) size(large)) ///
	text(0.025 56.32 "Urban", place(w) size(large))  ///
	text(0.025 40.75 "Rural", place(w) size(large)) ///
legend(off) xtitle("{it:Sisbén} -  Socioeconomic Index") ///
ytitle("Density") graphregion(style(none) color(gs16)) ///
ylabel(, format(%04.3f) angle(horizontal)) 
graph export "${tabla}histogram_sisben.pdf", replace

twoway (histogram puntaje_20152) ///
(scatteri 0 318 0.01 318, c(l) m(i) color(red)), ///
text(0.01 318 "Academic Criteria", place(e) size(large)) legend(off) ///
xtitle("Saber 11 - High School Exit Exam") ytitle("Density")	///
graphregion(style(none) color(gs16)) ///
ylabel(, format(%04.3f) angle(horizontal)) 
graph export "${tabla}histogram_saber_fullsample.pdf", replace

*********************************************************
*		      T RDD Results							*
*********************************************************

global controles area_ciudades area_urbano icfes_padre_nivel1 icfes_padre_nivel2 ///
icfes_padre_nivel3 icfes_madre_nivel1 icfes_madre_nivel2 icfes_madre_nivel3 ///
edad sexo_sisben ///
ranking_col

use puntaje_sisben_dnp max_puntaje_elegible area_sisben_dnp $controles ///
mejor_recibio depa_reside mejor_asistio_sig mejor_asistio ///
elegible_sisben_completo ranking puntaje_20152 col_cod_icfes cole_cod_dane_institucion  ///
depa_reside puntaje_std ranking using "$base2015_2", clear

keep if sexo_sisben == 0

* Standardized Results *

* Sample with SISBEN
* keep if puntaje_sisben_dnp!=.


set seed 1984
gen sisben_=.
replace sisben_=puntaje_sisben_dnp-57.21 if area_sisben_dnp==1
replace sisben_=puntaje_sisben_dnp-56.32 if area_sisben_dnp==2
replace sisben_=puntaje_sisben_dnp-40.75 if area_sisben_dnp==3

// Con efectos fijos de departamento
encode depa_reside, gen(departamento_reside)
tab depa_reside, gen(departamento_reside_)

gen sisben_2=sisben_*sisben_
gen sisben_3=sisben_*sisben_*sisben_
gen e_sisben1=elegible_sisben_completo*sisben_
gen e_sisben2=elegible_sisben_completo*sisben_2 
gen e_sisben3=elegible_sisben_completo*sisben_3


global controles area_ciudades area_urbano icfes_padre_nivel1 icfes_padre_nivel2 ///
icfes_padre_nivel3 icfes_madre_nivel1 icfes_madre_nivel2 icfes_madre_nivel3 ///
edad sexo_sisben departamento_reside_1-departamento_reside_32 ranking_col ///

* ranking_col cambiado por efectos fijos de colegio (cole_cod_dane_institucion)


* Bandwidth

rdbwselect ranking sisben_ covs($controles), all c(0) p(2)

scalar cersum=e(h_cersum)
display scalar(cersum)
scalar mserd=e(h_mserd)
display scalar(mserd)

* Generate Weights

foreach x in cersum mserd {
	gen w_`x'=scalar(`x')-abs(sisben_)
}



reg puntaje_20152 sisben_
outreg2 using "${tabla}rdd.xls", addtext(Cuantil,BORRAR) replace
				/* Este solo sirve para que no tenga que borrar manualmente cada vez el archivo, toca borrarlo cada vez*/
		
foreach x in cersum mserd {


qui reg ranking elegible_sisben_completo sisben_ e_sisben1 sisben_2 e_sisben2 ${controles} [iw = w_`x'] ///
if sisben_>=-scalar(`x') & sisben_<=scalar(`x'), r 

outreg2 using "${tabla}rdd.xls", addtext(Cuantil,MCO,Tipo de Banda,"`x'",) ///
 label ctitle("Agregado") append less(1)
 

foreach j in .25 .50 .75 .90 {

qui qreg ranking elegible_sisben_completo sisben_ e_sisben1 sisben_2 e_sisben2 ${controles} [iw = w_`x'] ///
if sisben_>=-scalar(`x') & sisben_<=scalar(`x'), q(`j') vce(r) iter(1000)

outreg2 using "${tabla}rdd.xls", addtext(Cuantil,`j',Tipo de Banda,"`x'") ///
 ctitle("Agregado") append less(1)
}

}


*************************** Brecha por percentil ***************************

use "$base2014_2", clear

gen e_total=.
replace e_total=1 if elegible_sisben_completo==1
replace e_total=0 if elegible_sisben_completo==0 | puntaje_sisben_dnp_20142==.

collapse (mean)mean=ranking (p25) p25=ranking (p50) p50=ranking (p75) p75=ranking (p90) p90=ranking (p95) p95=ranking ///
(p99) p99=ranking, by(e_total)


*********************************************************
*		    		  F BANDWIDTH					*
*********************************************************

rdbwselect ranking sisben_ covs($controles) , all c(0) p(2)
scalar cersum=e(h_cersum)
scalar msesum=e(h_msesum)
scalar mserd=e(h_mserd)

foreach x in  4  5  6  7  8  9  10  11  12  13  {
	gen w_`x'=scalar(`x')-abs(sisben_)
}


local c=1

local t_u= invnorm(0.95)

local t_l= invnorm(0.05)

mat b=J(4,10,.)

* cersum mserd msesum cerrd
*3 3.5 4 4.5 5 cersum 6 6.5 7 7.5 8 8.5 9 9.5 10 10.5 msesum mserd 12 12.5 13 13.5 14 14.5 15

foreach x in  4  5  6  7  8  9  10  11  12  13   {

qreg ranking elegible_sisben_completo sisben_ e_sisben1 sisben_2 e_sisben2 ${controles} [iw = w_`x'] ///
if sisben_>=-scalar(`x') & sisben_<=scalar(`x'), vce(r) iter(1000) q(90)

mat C=e(b)
mat D=e(V)

mat b[1,`c']=C[1,1]

mat b[2,`c']=sqrt(D[1,1])

mat b[3,`c']=C[1,1]+ `t_l' * sqrt(D[1,1])

mat b[4,`c']=C[1,1]+`t_u' * sqrt(D[1,1])

local c =`c'+1


}

*mat coln b=2 2.5 3 cersum cerrd 4.5 5 5.5 6 msesum 7 mserd 8

mat coln b=   4  5  6  7  8  9  10  11  12  13

mat rown b= coef sd t_l t_u

mat l b

mat C=b[1..4,1..10]

svmat double C

mat l C


*mkmat c*, matrix(C)

coefplot matrix(C[1]), ci((3 4)) vertical  xtitle("Bandwith") ytitle("Effect of eligibility at the 90th pctile") yline(0,lpattern(dash)) ///
graphregion(style(none) color(gs16))  bgcolor(white) 

graph save "${tabla}rdd_bw_r.gph", replace 
graph export "${tabla}rdd_bw_r.png", replace 

*********************************************************
*		      T RDD on Controls						*
*********************************************************

global control area_ciudades area_urbano icfes_padre_nivel1 icfes_padre_nivel2 ///
icfes_padre_nivel3 icfes_madre_nivel1 icfes_madre_nivel2 icfes_madre_nivel3 ///
edad sexo_sisben ranking_col

reg puntaje_20152 elegible_sisben_completo sisben_ sisben_2 e_sisben1 e_sisben2, r
outreg2 using "${tabla}rdd_cntrl_p90_15.xls", addtext(Control,BORRAR,Tipo de Banda,Borrar) replace

* For 90th pctile of test scores*

/*
sum puntaje_20152, d
keep if puntaje_20152>=r(p90)
*/

foreach x in cersum mserd {
	foreach y in $control {
	
reg `y' elegible_sisben_completo sisben_  e_sisben1 sisben_2 e_sisben2 [iw = w_`x'] ///
if sisben_>=-scalar(`x') & sisben_<=scalar(`x'), r 

outreg2 using "${tabla}rdd_cntrl_p90.xls", addtext(Controls,NO,Tipo de Banda,"`x'") ///
addstat(Banda,scalar(`x')) append less(1)

	}

}


*********************************************************
*		    F RDD Gap Reduction					 	*
*********************************************************

global controles area_ciudades area_urbano icfes_padre_nivel1 icfes_padre_nivel2 ///
icfes_padre_nivel3 icfes_madre_nivel1 icfes_madre_nivel2 icfes_madre_nivel3 ///
edad sexo_sisben ///
ranking_col

use puntaje_sisben_dnp max_puntaje_elegible area_sisben_dnp $controles ///
mejor_recibio depa_reside mejor_asistio_sig mejor_asistio ///
elegible_sisben_completo ranking puntaje_20152 col_cod_icfes  ///
depa_reside puntaje_std ranking using "$base2015_2", clear

* Standardized Results *

* Sample with SISBEN
* keep if puntaje_sisben_dnp!=.

set seed 1984
gen sisben_=.
replace sisben_=puntaje_sisben_dnp-57.21 if area_sisben_dnp==1
replace sisben_=puntaje_sisben_dnp-56.32 if area_sisben_dnp==2
replace sisben_=puntaje_sisben_dnp-40.75 if area_sisben_dnp==3

// Con efectos fijos de departamento
encode depa_reside, gen(departamento_reside)
tab depa_reside, gen(departamento_reside_)

global controles area_ciudades area_urbano icfes_padre_nivel1 icfes_padre_nivel2 ///
icfes_padre_nivel3 icfes_madre_nivel1 icfes_madre_nivel2 icfes_madre_nivel3 ///
edad sexo_sisben departamento_reside_1-departamento_reside_32 ///
ranking_col

global controles2 icfes_padre_nivel1 icfes_padre_nivel2 ///
icfes_padre_nivel3 icfes_madre_nivel1 icfes_madre_nivel2 icfes_madre_nivel3 ///
edad sexo_sisben departamento_reside_1-departamento_reside_32 ///
ranking_col

gen sisben_2=sisben_*sisben_
gen e_sisben1=elegible_sisben_completo*sisben_
gen e_sisben2=elegible_sisben_completo*sisben_2 

* Bandwidth

rdbwselect ranking sisben_ covs($controles), all c(0) p(2)

scalar cersum=e(h_cersum)
display scalar(cersum)

* Generate Weights

foreach x in cersum {
	gen w_`x'=scalar(`x')-abs(sisben_)
}

foreach x in cersum  {

reg ranking sisben_, r

outreg2 using "${tabla}rdd_gap.xls", addtext(BORRAR,MCO,Tipo de Banda,"`x'",) ///
 label ctitle("Agregado") replace

forvalues j=.01(.01).99 {

qreg ranking elegible_sisben_completo sisben_ e_sisben1 sisben_2 e_sisben2 ${controles} [iw = w_`x'] ///
if sisben_>=-scalar(`x') & sisben_<=scalar(`x'), q(`j') vce(r) iter(1000)

outreg2 using "${tabla}rdd_gap.xls", addtext(Cuantil,`j',Tipo de Banda,"`x'") ///
 ctitle("Agregado") append
}
}

*********************************************************
*		  		  F RDD Graphs 					 	*
*********************************************************

****************** Generate data with 2014 for Kernel graph of 2015 - 2014 ***************

use "$base2014_2", clear

drop area_sisbeniii

gen sisben_=.
replace sisben_=puntaje_sisben_dnp-57.21 if area_sisben_dnp==1
replace sisben_=puntaje_sisben_dnp-56.32 if area_sisben_dnp==2
replace sisben_=puntaje_sisben_dnp-40.75 if area_sisben_dnp==3


// Con efectos fijos por departamento
tab s11_departamento, gen(departamento_reside_)
global controles area_ciudades area_urbano icfes_padre_nivel1 icfes_padre_nivel2 ///
icfes_padre_nivel3 icfes_madre_nivel1 icfes_madre_nivel2 icfes_madre_nivel3 ///
edad sexo_sisben  ///
departamento_reside_1-departamento_reside_32 ranking_col

gen sisben_2=sisben_*sisben_
gen e_sisben1=elegible_sisben*sisben_
gen e_sisben=elegible_sisben*sisben_2 


**************** Prepare data 2014 for Graph Kernel 90th percentile  *********************

*scalar bw = 3.224
scalar bw = 5.041

keep if sisben_ > -bw & sisben_ < bw

qui qreg ranking ${controles}, 
predict resid, residuals

* Generates large bins that will appear in the graph:
scalar binsize = .4
gen bins4 = round(sisben_ + binsize/2, binsize)- binsize/2 if sisben_ > -4.8 & sisben_ < 4.8 // excludes noisy bins on the extreme with half observations
egen ranking_p90r_b4 = pctile(resid), by(bins) p(90)


* Small bins at which 90th percentiles will be calculated to make it non parametric:
scalar binsize = .02
gen bins = round(sisben_,binsize)

* Calculation of local percentiles:
egen ranking_p90=pctile(ranking), by(bins) p(90)
egen ranking_p90r=pctile(resid), by(bins) p(90)

** Adjustment for the constant (so that the average is the same as the data)
foreach x in ranking_p90r ranking_p90r_b4 {
qui sum `x'
scalar m_`x' = r(mean)
qui sum ranking , d
scalar p90_ranking = r(p90)
scalar dify = p90_ranking - m_`x'
replace `x' = `x' + dify
}

* One will be used for counting and then weight based on number of observations used to calculate the percentile
gen one = 1

* Collapse by bins, so that we keep only 1 observation per bin, which makes us avoid artificial significance through repetition of observations:
collapse ranking_p90r ranking_p90r_b4 sisben_ bins4 (count) one, by(bins)

rename one one_2014

save "bybeans_2014.dta", replace


	
*************** Starts working with data 2015 **********************

global controles area_ciudades area_urbano icfes_padre_nivel1 icfes_padre_nivel2 ///
icfes_padre_nivel3 icfes_madre_nivel1 icfes_madre_nivel2 icfes_madre_nivel3 ///
edad sexo_sisben ///
ranking_col

use puntaje_sisben_dnp max_puntaje_elegible area_sisben_dnp $controles ///
mejor_recibio depa_reside mejor_asistio_sig mejor_asistio ///
elegible_sisben_completo ranking ranking col_cod_icfes  ///
depa_reside puntaje_std using "$base2015_2", clear

* STANDARDIZED SCORES *
/* 
foreach var of varlist ranking {
                               sum `var' , d

                               replace `var' = (`var' - r(mean))/r(sd)

}
*/

*drop elegible_sisben2 elegibles 
set seed 1984
gen sisben_=.
replace sisben_=puntaje_sisben_dnp-57.21 if area_sisben_dnp==1
replace sisben_=puntaje_sisben_dnp-56.32 if area_sisben_dnp==2
replace sisben_=puntaje_sisben_dnp-40.75 if area_sisben_dnp==3

// Con efectos fijos de departamento
encode depa_reside, gen(departamento_reside)
qui tab depa_reside, gen(departamento_reside_)

global controles area_ciudades area_urbano icfes_padre_nivel1 icfes_padre_nivel2 ///
icfes_padre_nivel3 icfes_madre_nivel1 icfes_madre_nivel2 icfes_madre_nivel3 ///
edad sexo_sisben departamento_reside_1-departamento_reside_32 ///
ranking_col

gen sisben_2=sisben_*sisben_
gen e_sisben1=elegible_sisben_completo*sisben_
gen e_sisben=elegible_sisben_completo*sisben_2 


*********************  Graph Kernel 90th percentile  **************************


* Keep sample of the main regression
scalar bw = 3.224
scalar bw = 5.041

keep if sisben_ > -bw & sisben_ < bw

qreg ranking ${controles}, 
predict resid, residuals

* Generates large bins that will appear in the graph:
scalar binsize = .4
gen bins4 = round(sisben_ + binsize/2, binsize)- binsize/2 if sisben_ > -4.8 & sisben_ < 4.8 // excludes noisy bins on the extreme with half observations
egen ranking_p90r_b4 = pctile(resid), by(bins) p(90)
egen ranking_p50r_b4 = pctile(resid), by(bins) p(50)

* Small bins at which 90th percentiles will be calculated to make it non parametric:
scalar binsize = .02
gen bins = round(sisben_,binsize)

* Calculation of local percentiles:
egen ranking_p90=pctile(ranking), by(bins) p(90)
egen ranking_p90r=pctile(resid), by(bins) p(90)

** Adjustment for the constant (so that the average is the same as the data)
foreach x in ranking_p90r ranking_p90r_b4 {
qui sum `x'
scalar m_`x' = r(mean)
qui sum ranking , d
scalar p90_ranking = r(p90)
scalar dify = p90_ranking - m_`x'
replace `x' = `x' + dify
}
** Same adjustment for the median:
** Adjustment for the constant (so that the average is the same as the data)
foreach x in ranking_p50r ranking_p50r_b4 {
qui sum `x'
scalar m_`x' = r(mean)
qui sum ranking , d
scalar p50_ranking = r(p50)
scalar dify = p50_ranking - m_`x'
replace `x' = `x' + dify
}

* One will be used for counting and then weight based on number of observations used to calculate the percentile
gen one = 1

* Collapse by bins, so that we keep only 1 observation per bin, which makes us avoid artificial significance through repetition of observations:
collapse ranking_p90r ranking_p90r_b4 sisben_ bins4 (count) one, by(bins)
 
*save "bybeans_2015.dta", replace
 
* Makes Kernel graph for 2015
 
twoway (lpolyci ranking_p90r sisben_ if sisben_ <= 0 & sisben_ >= -bw [aw=one], degree(1) bwidth(1) level(95)  ciplot(rline) clpattern(solid) fcolor(none) alcolor(edkblue) alpattern(dash)) ///
       (lpolyci ranking_p90r sisben_ if sisben_ > 0  & sisben_ <  bw  [aw=one], degree(1) bwidth(1) level(95) ciplot(rline) clpattern(solid)  fcolor(none) alcolor(edkblue) alpattern(dash))  ///
(scatter ranking_p90r_b4 bins4 if sisben_ < bw & sisben_ >= -bw & bins == bins4,  msymbol(O) xline(0)), ///
 legend(label(2 "Loc. Polyn. of 90{superscript:th}         ") label(4 "pctiles of Ranking          ") label(1 "95% Confidence Interval") label(3 "") label(5 "90{superscript:th} pctile of Ranking (by large bin)")) ///	
 ytitle("90{superscript:th} pctile of ranking"" ") xtitle("Eligible                                                  Not Eligible" " " "{it:Sisbén} score (centered at cutoff)") graphregion(style(none) color(gs16))  bgcolor(white)

 

graph export "${tabla}RD_Fig_Kern_90.png", replace
graph export "${tabla}RD_Fig_Kern_90.eps", replace
graph save "${tabla}RD_Fig_Kern_90.gph", replace

 
************************* Kernel Local Polynomial of difference in 90th percentiles from 2014 to 2015  *********************** 

rename ranking_p90r ranking15_p90r
rename ranking_p90r_b4 ranking15_p90r_b4

merge 1:1 bins using "bybeans_2014.dta", keepusing(ranking_p90r ranking_p90r_b4 one_2014)

gen p90dif2015_2014 = ranking15_p90r - ranking_p90r

gen p90_b4_dif2015_2014 = ranking15_p90r_b4 - ranking_p90r_b4

* Take the average weight between the nb of observations in the bin of 2014 and the one of 2015:
replace one = (one + one_2014)/2

*scalar bw = 5.041

* Makes Kernel graph of change from 2014 to 2015

twoway  (lpolyci p90dif2015_2014 bins if sisben_ <= 0 & sisben_ >= -bw [aw=one], degree(1) bwidth(1) level(95)  ciplot(rline) clpattern(solid) fcolor(none) alcolor(edkblue) alpattern(dash)) ///
        (lpolyci p90dif2015_2014 bins if sisben_ > 0  & sisben_ <  bw  [aw=one], degree(1) bwidth(1) level(95) ciplot(rline) clpattern(solid)  fcolor(none) alcolor(edkblue) alpattern(dash))  ///
(scatter p90_b4_dif2015_2014 bins4 if sisben_ < bw & sisben_ >= -bw & bins == bins4,  msymbol(O) xline(0)), ///
legend(label(2 "Loc. Polyn. of 90{superscript:th}         ") label(4 "pctiles of Ranking          ") label(1 "95% Confidence Interval") label(3 "") label(5 "90{superscript:th} pctile of Ranking (by large bin)")) ///	
ytitle("Ranking 90{superscript:th} pctile 2015 minus 2014" " ") xtitle("Eligible                                                  Not Eligible" " " "{it:Sisbén} score (centered at cutoff)")  graphregion(style(none) color(gs16))  bgcolor(white)

graph export "${tabla}f6_rd_dif_ci95.png", replace
*graph export "${tabla}f6_rd_dif_ci95.eps", replace


graph export "${tabla}RD_Fig_2015_2014_Kern_90.png", replace
graph export "${tabla}RD_Fig_2015_2014_Kern_90.eps", replace
graph save "${tabla}RD_Fig_2015_2014_Kern_90.gph", replace

/*
*/
************ Using Our Actual Main Regression (together with the controls)  **************** P90

* Now we want to get back to data of 2015 before collapse

*Restricting the range to exactly what is used in the table
*scalar bw = 3.224
scalar bw = 5.041

keep if sisben_ > -bw & sisben_ < bw

gen weight=scalar(bw)-abs(sisben_)

*qreg ranking elegible_sisben_completo sisben_ sisben_2 e_sisben1 e_sisben ${controles} ///
*if sisben_>=-bw & sisben_<=bw, q(90) vce(r)

* reg with weights
gen e_sisben2=elegible_sisben_completo*sisben_2 

qui qreg ranking elegible_sisben_completo sisben_  e_sisben1 sisben_2 e_sisben ${controles} [iw = weight] ///
if sisben_>=-bw & sisben_<=bw, q(90) vce(r)

foreach x in area_ciudades	area_urbano	icfes_padre_nivel1	icfes_padre_nivel2	icfes_padre_nivel3	icfes_madre_nivel1	///
icfes_madre_nivel2	icfes_madre_nivel3	edad	sexo_sisben	departamento_reside_1	departamento_reside_2	departamento_reside_3	///
departamento_reside_4	departamento_reside_5	departamento_reside_6	departamento_reside_7	departamento_reside_8	departamento_reside_9	///
departamento_reside_10	departamento_reside_11	departamento_reside_12	departamento_reside_13	departamento_reside_14	departamento_reside_15	///
departamento_reside_16	departamento_reside_17	departamento_reside_18	departamento_reside_19	departamento_reside_20	departamento_reside_21	///
departamento_reside_22	departamento_reside_23	departamento_reside_24	departamento_reside_25	departamento_reside_26	departamento_reside_27	///
departamento_reside_28	departamento_reside_29	departamento_reside_30	departamento_reside_31	departamento_reside_32	ranking_col {
qui sum `x'
scalar aux = r(mean)
replace `x' = aux
}

predict yh, xb
predict y_stdp, stdp
gen ci_h=.
gen ci_l=.

***** CHANGE to CI 95% ****
/*
replace ci_h=yh+1.65*y_stdp 
replace ci_l=yh-1.65*y_stdp
*/
replace ci_h=yh+1.96*y_stdp 
replace ci_l=yh-1.96*y_stdp
*replace ci_h=yh+invnormal(0.95)*y_stdp 
*replace ci_l=yh-invnormal(0.95)*y_stdp
sort sisben_ ranking

** Adjustment for the constant (so that the average is the same as the 90th percentile)

foreach x in yh{
qui sum `x'
scalar m_`x' = r(mean)
qui sum ranking , d
scalar p90_ranking = r(p90)
scalar dify = p90_ranking - m_`x'
replace `x' = `x' + dify
}

replace ci_h=yh+invnormal(0.95)*y_stdp 
replace ci_l=yh-invnormal(0.95)*y_stdp

replace ci_h=yh+1.96*y_stdp 
replace ci_l=yh-1.96*y_stdp


* Makes graph of the main estimation (quadratic fit on each side):

twoway (rline ci_l ci_h sisben_ if sisben_ < bw & sisben_ >= -bw,  xline(0) lcolor(edkblue) lpattern(dash)) ///
(qfit yh sisben_ if sisben_ < bw & sisben_ > 0 [aw = weight] , xline(0) lpattern(solid)) ///
 (scatter ranking_p90r_b4 bins4 if sisben_ < bw & sisben_ >= -bw,  msymbol(O) xline(0) mcolor(black)) ///
  (qfit yh sisben_ if sisben_ <= 0 & sisben_ >= -bw [aw = weight] , xline(0)  lpattern(solid)), ///
 ytitle("90{superscript:th} pctile of ranking"" ") xtitle("Eligible                                                  Not Eligible" " " "{it:Sisbén} score (centered at cutoff)") ///	
 legend(label(1 "95% Confidence Interval") label(2 "Quadratic Quantile       ") label(4 "Regression of Ranking        ") label(3 "90{superscript:th} pctile of Ranking (by bin)")) graphregion(style(none) color(gs16))  bgcolor(white)

graph export "${tabla}RD_Fig_Quad_90.png", replace
graph export "${tabla}RD_Fig_Quad_90.eps", replace
graph save "${tabla}RD_Fig_Quad_90.gph", replace


** Intent to make the linear fit straight from there

************ Using Our Actual Main Regression but FIRST ORDER POLYNOMIAL (together with the controls)  FOR ROBUSTNESS  ********


global controles area_ciudades area_urbano icfes_padre_nivel1 icfes_padre_nivel2 ///
icfes_padre_nivel3 icfes_madre_nivel1 icfes_madre_nivel2 icfes_madre_nivel3 ///
edad sexo_sisben ///
ranking_col

use puntaje_sisben_dnp max_puntaje_elegible area_sisben_dnp $controles ///
mejor_recibio depa_reside mejor_asistio_sig mejor_asistio ///
elegible_sisben_completo ranking ranking col_cod_icfes  ///
depa_reside puntaje_std using "$base2015_2", clear


*drop elegible_sisben2 elegibles 
set seed 1984
gen sisben_=.
replace sisben_=puntaje_sisben_dnp-57.21 if area_sisben_dnp==1
replace sisben_=puntaje_sisben_dnp-56.32 if area_sisben_dnp==2
replace sisben_=puntaje_sisben_dnp-40.75 if area_sisben_dnp==3

// Con efectos fijos de departamento
encode depa_reside, gen(departamento_reside)
qui tab depa_reside, gen(departamento_reside_)

global controles area_ciudades area_urbano icfes_padre_nivel1 icfes_padre_nivel2 ///
icfes_padre_nivel3 icfes_madre_nivel1 icfes_madre_nivel2 icfes_madre_nivel3 ///
edad sexo_sisben departamento_reside_1-departamento_reside_32 ///
ranking_col

gen sisben_2=sisben_*sisben_
gen e_sisben1=elegible_sisben_completo*sisben_
gen e_sisben=elegible_sisben_completo*sisben_2 


*********************  Graph Kernel 90th percentile  **************************


* Keep sample of the main regression
scalar bw = 3.224

keep if sisben_ > -bw & sisben_ < bw

qreg ranking ${controles}, 
predict resid, residuals

* Generates large bins that will appear in the graph:
scalar binsize = .2
gen bins4 = round(sisben_ + binsize/2, binsize)- binsize/2 if sisben_ > -4.8 & sisben_ < 4.8 // excludes noisy bins on the extreme with half observations
egen ranking_p90r_b4 = pctile(resid), by(bins) p(90)
egen ranking_p50r_b4 = pctile(resid), by(bins) p(50)

* Small bins at which 90th percentiles will be calculated to make it non parametric:
scalar binsize = .02
gen bins = round(sisben_,binsize)

* Calculation of local percentiles:
egen ranking_p90=pctile(ranking), by(bins) p(90)
egen ranking_p90r=pctile(resid), by(bins) p(90)

** Adjustment for the constant (so that the average is the same as the data)
foreach x in ranking_p90r ranking_p90r_b4 {
qui sum `x'
scalar m_`x' = r(mean)
qui sum ranking , d
scalar p90_ranking = r(p90)
scalar dify = p90_ranking - m_`x'
replace `x' = `x' + dify
}


* One will be used for counting and then weight based on number of observations used to calculate the percentile
gen one = 1

gen weight=scalar(bw)-abs(sisben_)

* reg with weights
qui qreg ranking elegible_sisben_completo sisben_  e_sisben1 ${controles} [iw = weight] ///
if sisben_>=-bw & sisben_<=bw, q(90) vce(r)

foreach x in area_ciudades	area_urbano	icfes_padre_nivel1	icfes_padre_nivel2	icfes_padre_nivel3	icfes_madre_nivel1	///
icfes_madre_nivel2	icfes_madre_nivel3	edad	sexo_sisben	departamento_reside_1	departamento_reside_2	departamento_reside_3	///
departamento_reside_4	departamento_reside_5	departamento_reside_6	departamento_reside_7	departamento_reside_8	departamento_reside_9	///
departamento_reside_10	departamento_reside_11	departamento_reside_12	departamento_reside_13	departamento_reside_14	departamento_reside_15	///
departamento_reside_16	departamento_reside_17	departamento_reside_18	departamento_reside_19	departamento_reside_20	departamento_reside_21	///
departamento_reside_22	departamento_reside_23	departamento_reside_24	departamento_reside_25	departamento_reside_26	departamento_reside_27	///
departamento_reside_28	departamento_reside_29	departamento_reside_30	departamento_reside_31	departamento_reside_32	ranking_col {
qui sum `x'
scalar aux = r(mean)
replace `x' = aux
}

predict yh, xb
predict y_stdp, stdp
gen ci_h=.
gen ci_l=.

***** CHANGE to CI 95% ****

replace ci_h=yh+1.96*y_stdp 
replace ci_l=yh-1.96*y_stdp
*replace ci_h=yh+invnormal(0.95)*y_stdp 
*replace ci_l=yh-invnormal(0.95)*y_stdp
sort sisben_ ranking

** Adjustment for the constant (so that the average is the same as the 90th percentile)

foreach x in yh{
qui sum `x'
scalar m_`x' = r(mean)
qui sum ranking , d
scalar p90_ranking = r(p90)
scalar dify = p90_ranking - m_`x'
replace `x' = `x' + dify
}

replace ci_h=yh+invnormal(0.95)*y_stdp 
replace ci_l=yh-invnormal(0.95)*y_stdp

replace ci_h=yh+1.96*y_stdp 
replace ci_l=yh-1.96*y_stdp


* Makes graph of the main estimation (LINEAR fit on each side):

twoway (rline ci_l ci_h sisben_ if sisben_ < bw & sisben_ >= -bw,  xline(0) lcolor(edkblue) lpattern(dash)) ///
(lfit yh sisben_ if sisben_ < bw & sisben_ > 0 [aw = weight] , xline(0) lpattern(solid)) ///
 (scatter ranking_p90r_b4 bins4 if sisben_ < bw & sisben_ >= -bw,  msymbol(O) xline(0) mcolor(black)) ///
  (lfit yh sisben_ if sisben_ <= 0 & sisben_ >= -bw [aw = weight] , xline(0)  lpattern(solid)), ///
 ytitle("90{superscript:th} pctile of ranking"" ") xtitle("Eligible                                                  Not Eligible" " " "{it:Sisbén} score (centered at cutoff)") ///	
 legend(label(1 "95% Confidence Interval") label(2 "Linear Quantile       ") label(4 "Regression of Ranking        ") label(3 "90{superscript:th} pctile of Ranking (by bin)")) graphregion(style(none) color(gs16))  bgcolor(white)

graph export "${tabla}RD_Fig_Lin_90.png", replace
graph export "${tabla}RD_Fig_Lin_90.eps", replace
graph save "${tabla}RD_Fig_Lin_90.gph", replace

*********************************************************
*		     FA Eligibility Criteria					*
*********************************************************

global controles area_ciudades area_urbano icfes_padre_nivel1 icfes_padre_nivel2 ///
icfes_padre_nivel3 icfes_madre_nivel1 icfes_madre_nivel2 icfes_madre_nivel3 ///
edad sexo_sisben ///
ranking_col

use puntaje_sisben_dnp max_puntaje_elegible area_sisben_dnp $controles ///
mejor_recibio depa_reside mejor_asistio_sig mejor_asistio ///
elegible_sisben_completo ranking puntaje_20152 col_cod_icfes  ///
depa_reside puntaje_std using "$base2015_2", clear

*drop elegible_sisben2 elegibles 
set seed 1984
gen sisben_=.
replace sisben_=puntaje_sisben_dnp-57.21 if area_sisben_dnp==1
replace sisben_=puntaje_sisben_dnp-56.32 if area_sisben_dnp==2
replace sisben_=puntaje_sisben_dnp-40.75 if area_sisben_dnp==3

graph drop _all

*rddensity sisben_ if area_sisben_dnp==1, c(0) p(1)

twoway (histogram puntaje_sisben_dnp if area_sisben_dnp==1 ) ///
(scatteri 0 56.32 0.025 56.32 (9), c(l) m(i) color(red)), ///
text(0.025 57.21 "14 Cities", place(e) size(medium)) ///
text(0.025 0 "P-Val=.3030", place(e) size(medium)) ///
legend(off) xtitle("{it:Sisbén} -  Socioeconomic Index") ylabel(0(.01).025) ///
ytitle("Density") graphregion(style(none) color(gs16)) name("fx_histogram_sisben_14c")
*graph export "${tabla}fx_histogram_sisben_14c.eps", replace

*rddensity sisben_ if area_sisben_dnp==2, c(0) p(1)

twoway (histogram puntaje_sisben_dnp if area_sisben_dnp==2 ) ///
(scatteri 0 57.21 0.025 57.21 (3), c(l) m(i) color(red) ), ///
text(0.025 58.32 "Urban", place(e) size(medium)) ///
text(0.025 0 "P-Val=.7825", place(e) size(medium)) ///
legend(off) xtitle("{it:Sisbén} -  Socioeconomic Index")  ylabel(0(.01).025) ///
ytitle("Density") graphregion(style(none) color(gs16)) name("fx_histogram_sisben_urban")
*graph export "${tabla}fx_histogram_sisben_urban.eps", replace

*rddensity sisben_ if area_sisben_dnp==3, c(0) p(1)

twoway (histogram puntaje_sisben_dnp if area_sisben_dnp==3 ) ///
(scatteri 0 40.75 0.04 40.75 (9), c(l) m(i) color(red)), ///
text(0.04 41.75 "Rural", place(e) size(medium)) ///
text(0.04 0 "P-Val=.1376", place(e) size(medium)) ///
legend(off) xtitle("{it:Sisbén} -  Socioeconomic Index") ylabel(0(.01).035) ///
ytitle("Density") graphregion(style(none) color(gs16)) name("fx_histogram_sisben_rural")
*graph export "${tabla}fx_histogram_sisben_rural.eps", replace

 graph combine fx_histogram_sisben_14c fx_histogram_sisben_urban ///
	fx_histogram_sisben_rural, ///
	rows(3) graphregion(style(none) color(gs16)) ///
	imargin(medsmall) xcommon ycommon
graph export "${tabla}fx_histogram_sisben.eps", replace
	
*********************************************************
*		  FA DD Graph						 		*
*********************************************************

clear all
use "$completa2", clear

* Keep Sample with SISBEN
keep if puntaje_sisben_dnp!=.

levelsof year, local(date)
foreach var of varlist puntaje_saber {
	foreach y in `date' {
		sum `var' if year==`y' , d
		replace `var' = (`var' - r(mean))/r(sd) if year==`y' 
}
}

* & puntaje_sisben_dnp!=.

* Keep same program: Replace ranking with standarized test

*replace ranking=puntaje_saber

collapse (mean)ranking_m=ranking ///
		 (median)ranking_me=ranking ///
		 (p10)ranking_p10=ranking ///
		 (p25)ranking_p25=ranking ///
		 (p75)ranking_p75=ranking ///
		 (p90)ranking_p90=ranking ///
		 (p95)ranking_p95=ranking ///
		 (p99)ranking_p99=ranking, ///
		 by(year categoria)

label var ranking_m "Mean"	 
label var ranking_me "Median"
label var ranking_p10 "10{superscript:th} Percentile"
label var ranking_p25 "25{superscript:th} Percentile"
label var ranking_p75 "75{superscript:th} Percentile"	 
label var ranking_p90 "90{superscript:th} Percentile"
label var ranking_p95 "95{superscript:th} Percentile"
label var ranking_p99 "99{superscript:th} Percentile"
		 
xtset categoria year, yearly 
graph drop _all

foreach i in ranking_m ranking_me ranking_p10 ///
  ranking_p25 ranking_p75 ranking_p90 ranking_p95 ranking_p99 {
  twoway (tsline `i' if categoria==5, msymbol(O) recast(connected)) ///
	(tsline `i' if categoria==4, msymbol(D) recast(connected)) ///
	(tsline `i' if categoria==3, msymbol(T) recast(connected)) ///
	(tsline `i' if categoria==2, msymbol(+) recast(connected)) ///
	(tsline `i' if categoria==1, msymbol(X) recast(connected)), ///
	legend(label(5 "Eligible") label(4 "Non-eligible Q1") ///
	label(2 "Non-eligible Q3") label(3 "Non-eligible Q2") ///
	label (1 "Non-eligible Q4") col(3) size(medsmall)) xtitle("") ylabel(#4) ///
	title(`: variable label `i'') ytitle("") name(f3_`i', replace) graphregion(style(none) color(gs16)) ///
	scheme(s2color) bgcolor(white) 
   graph export "${tabla}f3_dd_`i'.eps", as(eps) replace 
   }
   
  *ysc(r(40 100))

 grc1leg  f3_ranking_me f3_ranking_p75 ///
	f3_ranking_p90 f3_ranking_p95, ///
	rows(2) cols(2) graphregion(style(none) color(gs16)) ///
	imargin(medsmall) ysize(15) xsize(10)
	
 graph export "${tabla}f3_dd_all.eps", replace 
 
*********************************************************
*		     TA RDD Placebo							*
*********************************************************

global controles area_ciudades area_urbano icfes_padre_nivel1 icfes_padre_nivel2 ///
icfes_padre_nivel3 icfes_madre_nivel1 icfes_madre_nivel2 icfes_madre_nivel3 ///
edad sexo_sisben ranking_col

use puntaje_sisben_dnp max_puntaje_elegible area_sisben_dnp $controles ///
mejor_recibio   ///
elegible_sisben_completo ranking puntaje_20142 col_cod_icfes  ///
s11_departamento puntaje_std using "$base2014_2", clear

* Standardized

* Sample with SISBEN
* keep if puntaje_sisben_dnp!=.


set seed 1984
gen sisben_=.
replace sisben_=puntaje_sisben_dnp-57.21 if area_sisben_dnp==1
replace sisben_=puntaje_sisben_dnp-56.32 if area_sisben_dnp==2
replace sisben_=puntaje_sisben_dnp-40.75 if area_sisben_dnp==3

// Con efectos fijos por departamento
 tab s11_departamento, gen(departamento_reside_)

global controles area_ciudades area_urbano icfes_padre_nivel1 icfes_padre_nivel2 ///
icfes_padre_nivel3 icfes_madre_nivel1 icfes_madre_nivel2 icfes_madre_nivel3 ///
edad sexo_sisben departamento_reside_1-departamento_reside_32 ranking_col

gen sisben_2=sisben_*sisben_
gen e_sisben1=elegible_sisben*sisben_
gen e_sisben2=elegible_sisben*sisben_2 

* Bandwidth


rdbwselect ranking sisben_ covs($controles), all c(0) p(2)

scalar cersum=e(h_cersum)
display scalar(cersum)
scalar mserd=e(h_mserd)
display scalar(mserd)

/*
** Maintaining cersum and mserd bandwidths of main regressions:
scalar cersum = 5.0405499
display scalar(cersum)

scalar mserd = 11.025592
display scalar(mserd)

*/
* Generate Weights
foreach x in cersum mserd{
	gen w_`x'=scalar(`x')-abs(sisben_)
	
}


reg puntaje_20142 sisben_
outreg2 using "${tabla}rdd_placebo.xls", addtext(Cuantil,BORRAR) replace
				/* Este solo sirve para que no tenga que borrar manualmente cada vez el archivo, toca borrarlo cada vez*/
		
foreach x in cersum mserd {


qui reg ranking elegible_sisben_completo sisben_  e_sisben1  sisben_2 e_sisben2 ${controles}  ///
if sisben_>=-scalar(`x') & sisben_<=scalar(`x'), r 

outreg2 using "${tabla}rdd_placebo.xls", addtext(Cuantil,MCO,Tipo de Banda,"`x'",) ///
 label ctitle("Agregado") append less(1)
 

 foreach j in .95 {

 qreg ranking elegible_sisben_completo sisben_  e_sisben1 sisben_2 e_sisben2 ${controles} ///
if sisben_>=-scalar(`x') & sisben_<=scalar(`x'), q(`j') vce(r) 
*iter(1000)

outreg2 using "${tabla}rdd_placebo.xls", addtext(Cuantil,`j',Tipo de Banda,"`x'") ///
 ctitle("Agregado") append less(1)
}

}
 
foreach j in .25 .50 .75 .90 {

qui qreg ranking elegible_sisben_completo sisben_  e_sisben1 sisben_2 e_sisben2 ${controles} ///
if sisben_>=-scalar(`x') & sisben_<=scalar(`x'), q(`j') vce(r) 
*iter(1000)

outreg2 using "${tabla}rdd_placebo.xls", addtext(Cuantil,`j',Tipo de Banda,"`x'") ///
 ctitle("Agregado") append less(1)
}

}


*********************************************************
*		FA.   F7 Enrollment +- cutoff					*
*********************************************************

*************
*	2015	*
*************

global controles area_ciudades area_urbano icfes_padre_nivel1 icfes_padre_nivel2 ///
icfes_padre_nivel3 icfes_madre_nivel1 icfes_madre_nivel2 icfes_madre_nivel3 ///
edad sexo_sisben ranking_col

use en_ES en_ES_sig puntaje_20152 ies_ies_acreditada elegible_sisben_completo puntaje_sisben_dnp area_sisben_dnp ///
elegible_sb11 max_puntaje_elegible cole_cod_dane_institucion ${controles} using "$base2015_2" , clear

bysort elegible_sb11: sum puntaje_20152 if elegible_sisben_completo == 1 // eligible if puntaje_20152>=318
sum elegible_sb11 if elegible_sisben_completo == 1 // 95.04 is the percent of non eligible based on Saber 11 

scalar el_2015_p_FS = 100 - (100 * `r(mean)') // keeps the % of non eligible, here 95.035655
scalar list

/*
* This drops observations from colleges that do not have anyone in the bandwidth, but don't know why it's used 
merge m:1 cole_cod_dane_institucion using  "${tabla}col_rdd.dta"
keep if _merge==3
*/


gen sisben_=.
replace sisben_=puntaje_sisben_dnp-57.21 if area_sisben_dnp==1
replace sisben_=puntaje_sisben_dnp-56.32 if area_sisben_dnp==2
replace sisben_=puntaje_sisben_dnp-40.75 if area_sisben_dnp==3

* Bandwidth

*rdbwselect ranking sisben_ covs($controles), all c(0) p(2)
*scalar cersum=e(h_cersum)

* Para ganar tiempo reemplazamos directamente el valor del cersum:
scalar cersum = 5.041

*keep if sisben_>=-scalar(cersum) & sisben_<=scalar(cersum)

** But among the restricted RD sample:
bysort elegible_sb11: sum puntaje_20152 if elegible_sisben_completo == 1 // eligible if puntaje_20152>=318
sum elegible_sb11 if elegible_sisben_completo == 1 // 92.75 is the percent of non eligible based on Saber 11 top 7.25% are eligible 

scalar el_2015_p_RD = 100 - (100 * `r(mean)') // keeps the % of non eligible, here 92.749828
scalar list

** Values with RD sample: 		87.75   &   92.75
** Values with full sample: 	90.04   &   95.04

* lb and upper bound for noneligibles:
_pctile puntaje_20152 if elegible_sisben_completo == 0 , p(90.04)
scalar lb2015_noneligible = r(r1)  

_pctile puntaje_20152 if elegible_sisben_completo == 0 , p(95.04)
scalar ub2015_noneligible = r(r1)  

* lb and upper bound for eligibles:
_pctile puntaje_20152 if elegible_sisben_completo == 1 , p(90.04)
scalar lb2015_eligible = r(r1)  

_pctile puntaje_20152 if elegible_sisben_completo == 1 , p(95.04)
scalar ub2015_eligible = r(r1)  

scalar list
/*
lb2015_noneligible =        308
ub2015_noneligible =        324

lb2015_eligible =        	303
ub2015_eligible =        	318
*/

gen topstud = 0
replace topstud = 1 if elegible_sisben_completo == 0 & puntaje_20152 >= scalar(ub2015_noneligible) 
replace topstud = 1 if elegible_sisben_completo == 1 & puntaje_20152 >= scalar(ub2015_eligible) 


gen topstud2 = 0
replace topstud2 = 1 if elegible_sisben_completo == 0 & puntaje_20152 >= scalar(lb2015_noneligible)  & puntaje_20152  < scalar(ub2015_noneligible) 
replace topstud2 = 1 if elegible_sisben_completo == 1 &  puntaje_20152 >= scalar(lb2015_eligible)  & puntaje_20152  < scalar(ub2015_eligible) 

gen year = 2015

rename puntaje_sisben_dnp_2015 puntaje_sisben_dnp


save "${base}\enrol2015.dta", replace


*************
*	2014	*
*************

use en_ES en_ES_sig puntaje_20142 elegible_sb11 ies_ies_acreditada elegible_sisben_completo puntaje_sisben_dnp ///
area_sisben_dnp elegible_sb11 codigo_dane_sede  ${controles} using "$base2014_2", clear


rename codigo_dane_sede cole_cod_dane_institucion


tab elegible_sb11 // Before cutting it to the RD sample, in 2014, 90.64% of the sample is above the Saber 11 eligibility cutoff

bysort elegible_sb11: sum puntaje_20142 if elegible_sisben_completo == 1 // eligible if puntaje_20152>=310
tab elegible_sb11 if elegible_sisben_completo == 1 // 95.06 is the percent of non eligible based on Saber 11 4.94 are eligible


gen sisben_=.
replace sisben_=puntaje_sisben_dnp-57.21 if area_sisben_dnp==1
replace sisben_=puntaje_sisben_dnp-56.32 if area_sisben_dnp==2
replace sisben_=puntaje_sisben_dnp-40.75 if area_sisben_dnp==3

* Bandwidth
scalar cersum = 5.041

*keep if sisben_>=-scalar(cersum) & sisben_<=scalar(cersum)

** But among the restricted RD sample:
tab elegible_sb11 if elegible_sisben_completo == 1 // 92.01 is the percent of non eligible based on Saber 11 7.99 are eligible in RD sample eligible
tab elegible_sb11 // Useful for the paper, 91.08% are above the Saber 11 Cutoff of 2014 (in full RD sample)


** values with RD sample:   	90.06   &   95.06 
** values with Full sample:   	87.01   &   92.01

* lb and upper bound for noneligibles:
_pctile puntaje_20142 if elegible_sisben_completo == 0 , p(90.06)
scalar lb2014_noneligible = r(r1)  
  
_pctile puntaje_20142 if elegible_sisben_completo == 0 , p(95.06)
scalar ub2014_noneligible = r(r1)

* lb and upper bound for eligibles:
_pctile puntaje_20142 if elegible_sisben_completo == 1 , p(90.06)
scalar lb2014_eligible = r(r1)

_pctile puntaje_20142 if elegible_sisben_completo == 1 , p(95.06)
scalar ub2014_eligible = r(r1) 


scalar list
/*
lb2014_noneligible =        302
ub2014_noneligible =        315

lb2014_eligible =        	297
ub2014_eligible =        	309
*/

gen topstud = 0
replace topstud = 1 if elegible_sisben_completo == 0 & puntaje_20142 >= scalar(ub2014_noneligible) 
replace topstud = 1 if elegible_sisben_completo == 1 & puntaje_20142 >= scalar(ub2014_eligible) 


gen topstud2 = 0
replace topstud2 = 1 if elegible_sisben_completo == 0 & puntaje_20142 >= scalar(lb2014_noneligible)  & puntaje_20142  < scalar(ub2014_noneligible) 
replace topstud2 = 1 if elegible_sisben_completo == 1 &  puntaje_20142 >= scalar(lb2014_eligible)  & puntaje_20142  < scalar(ub2014_eligible) 
tab elegible_sisben_completo topstud2

gen year = 2014

rename puntaje_sisben_dnp_2014 puntaje_sisben_dnp

save "${base}\enrol2014.dta", replace


*** guardar la base 2014
/*
* Elegibles
sum en_ES_sig if puntaje_20142>=310 & elegible_sisben_completo==1
sum en_ES_sig if puntaje_20142<310 & puntaje_20142>=b  & elegible_sisben_completo==1

* No Elegibles

sum en_ES_sig if puntaje_20142>=310 & elegible_sisben_completo==0
sum en_ES_sig if puntaje_20142<310 & puntaje_20142>=b & elegible_sisben_completo==0
*/
*************
*	2013	*
*************

use puntaje_20132 elegible_sb11 elegible_sisben_completo puntaje_sisben_dnp identificacionunica ///
area_sisben_dnp elegible_sb11 codigo_dane_sede  ${controles} using "$base2013_2", clear

*use  puntaje_20132 puntaje_sisben_dnp_2013  identificacionunica ///
*area_sisben_dnp_2013 elegible_sisben_completo  codigo_dane_sede using "$base2013_2", clear

drop if identificacionunica==""

merge 1:1 identificacionunica using "$base2013_2_ies", nogen

rename codigo_dane_sede cole_cod_dane_institucion


*merge m:1 cole_cod_dane_institucion using  "${tabla}col_rdd.dta"
*keep if _merge==3


gen sisben_=.
replace sisben_=puntaje_sisben_dnp-57.21 if area_sisben_dnp==1
replace sisben_=puntaje_sisben_dnp-56.32 if area_sisben_dnp==2
replace sisben_=puntaje_sisben_dnp-40.75 if area_sisben_dnp==3

* Bandwidth
scalar cersum = 5.041

*keep if sisben_>=-scalar(cersum) & sisben_<=scalar(cersum)

** values with RD sample:   	90.06   &   95.06 
** values with Full sample:   	87.01   &   92.01

* lb and upper bound for noneligibles:
_pctile puntaje_20132 if elegible_sisben_completo == 0 , p(90.06) // applying percentiles of 2014
scalar lb2013_noneligible = r(r1)  
  
_pctile puntaje_20132 if elegible_sisben_completo == 0 , p(95.06)
scalar ub2013_noneligible = r(r1)

* lb and upper bound for eligibles:
_pctile puntaje_20132 if elegible_sisben_completo == 1 , p(90.06)
scalar lb2013_eligible = r(r1)

_pctile puntaje_20132 if elegible_sisben_completo == 1 , p(95.06)
scalar ub2013_eligible = r(r1) 


gen topstud = 0
replace topstud = 1 if elegible_sisben_completo == 0 & puntaje_20132 >= scalar(ub2013_noneligible) 
replace topstud = 1 if elegible_sisben_completo == 1 & puntaje_20132 >= scalar(ub2013_eligible) 


gen topstud2 = 0
replace topstud2 = 1 if elegible_sisben_completo == 0 & puntaje_20132 >= scalar(lb2013_noneligible)  & puntaje_20132  < scalar(ub2013_noneligible) 
replace topstud2 = 1 if elegible_sisben_completo == 1 &  puntaje_20132 >= scalar(lb2013_eligible)  & puntaje_20132  < scalar(ub2013_eligible) 
tab elegible_sisben_completo topstud2

gen year = 2013

rename puntaje_sisben_dnp_2013 puntaje_sisben_dnp

save "${base}\enrol2013.dta", replace

append using "${base}\enrol2014.dta"

append using "${base}\enrol2015.dta"

keep if topstud == 1 | topstud2 == 1

save "${base}\enrol2013_4_5.dta", replace




*** ESTIMATIONS 

use "${base}\enrol2013_4_5.dta", clear



bysort year topstud: sum en_ES_sig  // these are the means to use in figure

gen e_y_topstud = 100000*(1-elegible_sisben_completo) +  0.5* topstud + year

*tabform en_ES_sig using enrolstats.xls, by(e_y_topstud) bdec(5)


*sd sdbracket vertical mtest 


* university enrollment of eligible students increased by 7 percentage points more than of non-eligible students from 2014 to 2015
foreach y in 2013 2014 2015 {
gen y_`y' =(year == `y')
foreach e in 1 0 {
foreach t in 1 0 {
gen el`e'_top`t'_`y' = (elegible_sisben_completo == `e' & topstud == `t' & year == `y')
}

}
gen el_`y' =(elegible_sisben_completo == 1 & year == `y')
}

*** REGRESSIONS DD ENROLLMENT:
tab topstud

global controles area_ciudades area_urbano icfes_padre_nivel1 icfes_padre_nivel2 ///
icfes_padre_nivel3 icfes_madre_nivel1 icfes_madre_nivel2 icfes_madre_nivel3 ///
edad sexo_sisben ranking_col

* Only 2014 & 2015
*5 to 10%				
qui reg en_ES_sig el_2015  elegible_sisben_completo y_2015 if (year == 2014 | year == 2015) & topstud == 0, ro 
outreg2 using "${tabla}\enrol_DD.xls", replace dec(3) nor2  keep(el_2015 elegible_sisben_completo y_2015) nocons

* with controls
qui reg en_ES_sig el_2015 elegible_sisben_completo sisben_ ${controles} y_2015 if (year == 2014 | year == 2015) & topstud == 0, ro 
outreg2 using "${tabla}\enrol_DD.xls", append dec(3) nor2  keep(el_2015 elegible_sisben_completo y_2015) nocons


* Top 5
qui reg en_ES_sig el_2015 elegible_sisben_completo y_2015 if (year == 2014 | year == 2015) & topstud == 1, ro 
outreg2 using "${tabla}\enrol_DD.xls", append dec(3) nor2  keep(el_2015 elegible_sisben_completo y_2015) nocons

* with controls
qui reg en_ES_sig el_2015 elegible_sisben_completo sisben_ ${controles} y_2015 if (year == 2014 | year == 2015) & topstud == 1, ro 
outreg2 using "${tabla}\enrol_DD.xls", append dec(3) nor2  keep(el_2015 elegible_sisben_completo y_2015) nocons



** Placebo: from 2013 to 2014

*5 to 10%				
qui reg en_ES_sig el_2014 elegible_sisben_completo y_2014 if (year == 2013 | year == 2014) & topstud == 0, ro 
outreg2 using "${tabla}\enrol_DD.xls", append dec(3) nor2  keep(el_2014 elegible_sisben_completo y_2014) nocons

* with controls
qui reg en_ES_sig el_2014 elegible_sisben_completo sisben_ ${controles} y_2014 if (year == 2013 | year == 2014) & topstud == 0, ro 
outreg2 using "${tabla}\enrol_DD.xls", append dec(3) nor2  keep(el_2014 elegible_sisben_completo y_2014) nocons


* Top 5
qui reg en_ES_sig el_2014 elegible_sisben_completo y_2014 if (year == 2013 | year == 2014) & topstud == 1, ro 
outreg2 using "${tabla}\enrol_DD.xls", append dec(3) nor2  keep(el_2014 elegible_sisben_completo y_2014) nocons

* with controls
qui reg en_ES_sig el_2014 elegible_sisben_completo sisben_ ${controles} y_2014 if (year == 2013 | year == 2014) & topstud == 1, ro 
outreg2 using "${tabla}\enrol_DD.xls", append dec(3) nor2  keep(el_2014 elegible_sisben_completo y_2014) nocons



			
				
	

outreg2 using "${tabla}\enrol_reg.xls", replace dec(3) nor2 adec(3) keep(el1_top0_2015 el1_top1_2015 el0_top0_2015 el0_top1_2015) nocons ///
adds(dif_below,`dif_el1_el0_top0',se_below,`se_el1_el0_top0', pval_below_cutoff,`pv_el1_el0_top0', dif_above,`dif_el1_el0_top1',se_above,`se_el1_el0_top1',pval_above_cutoff,`pv_el1_el0_top1')







* Only 2014 & 2015
qui reg en_ES_sig 	el1_top0_2015 el1_top1_2015 el0_top0_2015 el0_top1_2015 ///
				elegible_sisben_completo topstud c.elegible_sisben_completo#c.topstud ///
				if year == 2014 | year == 2015, ro 

				
qui lincom _b[el1_top0_2015] - _b[el0_top0_2015]
local dif_el1_el0_top0 = r(estimate) // Gives the Dif in Dif Coef
local se_el1_el0_top0 = r(se)  // Gives the standard error of that Dif in Dif

qui test el1_top0_2015 = el0_top0_2015	
local pv_el1_el0_top0 = r(p) // gives the p-val of that Dif in Dif (though I think we will only use it for stars)


qui lincom _b[el1_top1_2015] - _b[el0_top1_2015] // then same thing for students above cutoff
local dif_el1_el0_top1 = r(estimate)
local se_el1_el0_top1 = r(se)

qui test el1_top1_2015 = el0_top1_2015	
local pv_el1_el0_top1 = r(p)

outreg2 using "${tabla}\enrol_reg.xls", replace dec(3) nor2 adec(3) keep(el1_top0_2015 el1_top1_2015 el0_top0_2015 el0_top1_2015) nocons ///
adds(dif_below,`dif_el1_el0_top0',se_below,`se_el1_el0_top0', pval_below_cutoff,`pv_el1_el0_top0', dif_above,`dif_el1_el0_top1',se_above,`se_el1_el0_top1',pval_above_cutoff,`pv_el1_el0_top1')



		
		* Reg 2015 Con Controles		
qui reg en_ES_sig 	el1_top0_2015 el1_top1_2015 el0_top0_2015 el0_top1_2015 ///
				elegible_sisben_completo topstud c.elegible_sisben_completo#c.topstud puntaje_sisben_dnp ${controles} ///
				if year == 2014 | year == 2015, ro 
				
qui lincom _b[el1_top0_2015] - _b[el0_top0_2015]
local dif_el1_el0_top0 = r(estimate) // Gives the Dif in Dif Coef
local se_el1_el0_top0 = r(se)  // Gives the standard error of that Dif in Dif

qui test el1_top0_2015 = el0_top0_2015	
local pv_el1_el0_top0 = r(p) // gives the p-val of that Dif in Dif (though I think we will only use it for stars)


qui lincom _b[el1_top1_2015] - _b[el0_top1_2015] // then same thing for students above cutoff
local dif_el1_el0_top1 = r(estimate)
local se_el1_el0_top1 = r(se)

qui test el1_top1_2015 = el0_top1_2015	
local pv_el1_el0_top1 = r(p)

outreg2 using "${tabla}\enrol_reg.xls", append dec(3) nor2 adec(3) keep(el1_top0_2015 el1_top1_2015 el0_top0_2015 el0_top1_2015) nocons ///
adds(dif_below,`dif_el1_el0_top0',se_below,`se_el1_el0_top0', pval_below_cutoff,`pv_el1_el0_top0', dif_above,`dif_el1_el0_top1',se_above,`se_el1_el0_top1',pval_above_cutoff,`pv_el1_el0_top1')

		
		
		
* Only 2013 & 2014
qui reg en_ES_sig 	el1_top0_2014 el1_top1_2014 el0_top0_2014 el0_top1_2014 ///
				elegible_sisben_completo topstud c.elegible_sisben_completo#c.topstud ///
				if year == 2013 | year == 2014, ro
				
qui lincom _b[el1_top0_2014] - _b[el0_top0_2014]
local dif_el1_el0_top0 = r(estimate) // Gives the Dif in Dif Coef
local se_el1_el0_top0 = r(se)  // Gives the standard error of that Dif in Dif

qui test el1_top0_2014 = el0_top0_2014	
local pv_el1_el0_top0 = r(p) // gives the p-val of that Dif in Dif (though I think we will only use it for stars)


qui lincom _b[el1_top1_2014] - _b[el0_top1_2014] // then same thing for students above cutoff
local dif_el1_el0_top1 = r(estimate)
local se_el1_el0_top1 = r(se)

qui test el1_top1_2014 = el0_top1_2014	
local pv_el1_el0_top1 = r(p)

outreg2 using "${tabla}\enrol_reg.xls", append dec(3) nor2 adec(3) keep(el1_top0_2014 el1_top1_2014 el0_top0_2014 el0_top1_2014) nocons ///
adds(dif_below,`dif_el1_el0_top0',se_below,`se_el1_el0_top0', pval_below_cutoff,`pv_el1_el0_top0', dif_above,`dif_el1_el0_top1',se_above,`se_el1_el0_top1',pval_above_cutoff,`pv_el1_el0_top1')




				
* Only 2013 & 2014 con controles
qui reg en_ES_sig 	el1_top0_2014 el1_top1_2014 el0_top0_2014 el0_top1_2014 ///
				elegible_sisben_completo topstud c.elegible_sisben_completo#c.topstud puntaje_sisben_dnp ${controles} ///
				if year == 2013 | year == 2014, ro
				
qui lincom _b[el1_top0_2014] - _b[el0_top0_2014]
local dif_el1_el0_top0 = r(estimate) // Gives the Dif in Dif Coef
local se_el1_el0_top0 = r(se)  // Gives the standard error of that Dif in Dif

test el1_top0_2014 = el0_top0_2014	
local pv_el1_el0_top0 = r(p) // gives the p-val of that Dif in Dif (though I think we will only use it for stars)


qui lincom _b[el1_top1_2014] - _b[el0_top1_2014] // then same thing for students above cutoff
local dif_el1_el0_top1 = r(estimate)
local se_el1_el0_top1 = r(se)

qui test el1_top1_2014 = el0_top1_2014	
local pv_el1_el0_top1 = r(p)

outreg2 using "${tabla}\enrol_reg.xls", append dec(3) nor2 adec(3) keep(el1_top0_2014 el1_top1_2014 el0_top0_2014 el0_top1_2014) nocons ///
adds(dif_below,`dif_el1_el0_top0',se_below,`se_el1_el0_top0', pval_below_cutoff,`pv_el1_el0_top0', dif_above,`dif_el1_el0_top1',se_above,`se_el1_el0_top1',pval_above_cutoff,`pv_el1_el0_top1')



*********************************************************
*		      TA RDD Results	Std. Score						*
*********************************************************

global controles area_ciudades area_urbano icfes_padre_nivel1 icfes_padre_nivel2 ///
icfes_padre_nivel3 icfes_madre_nivel1 icfes_madre_nivel2 icfes_madre_nivel3 ///
edad sexo_sisben ///
ranking_col

use puntaje_sisben_dnp max_puntaje_elegible area_sisben_dnp $controles ///
mejor_recibio depa_reside mejor_asistio_sig mejor_asistio ///
elegible_sisben_completo ranking puntaje_20152 col_cod_icfes cole_cod_dane_institucion  ///
depa_reside puntaje_std ranking using "$base2015_2", clear

* Standardized Results *

* Sample with SISBEN
* keep if puntaje_sisben_dnp!=.

foreach var of varlist puntaje_20152 {
		sum `var', d
		replace `var' = (`var' - r(mean))/r(sd) 
}

set seed 1984
gen sisben_=.
replace sisben_=puntaje_sisben_dnp-57.21 if area_sisben_dnp==1
replace sisben_=puntaje_sisben_dnp-56.32 if area_sisben_dnp==2
replace sisben_=puntaje_sisben_dnp-40.75 if area_sisben_dnp==3

// Con efectos fijos de departamento
encode depa_reside, gen(departamento_reside)
tab depa_reside, gen(departamento_reside_)

gen sisben_2=sisben_*sisben_
gen sisben_3=sisben_*sisben_*sisben_
gen e_sisben1=elegible_sisben_completo*sisben_
gen e_sisben2=elegible_sisben_completo*sisben_2 
gen e_sisben3=elegible_sisben_completo*sisben_3


global controles area_ciudades area_urbano icfes_padre_nivel1 icfes_padre_nivel2 ///
icfes_padre_nivel3 icfes_madre_nivel1 icfes_madre_nivel2 icfes_madre_nivel3 ///
edad sexo_sisben departamento_reside_1-departamento_reside_32 ranking_col ///

* ranking_col cambiado por efectos fijos de colegio (cole_cod_dane_institucion)


* Bandwidth

rdbwselect puntaje_20152 sisben_ covs($controles), all c(0) p(2)

scalar cersum=e(h_cersum)
display scalar(cersum)
scalar mserd=e(h_mserd)
display scalar(mserd)

* Generate Weights

foreach x in cersum mserd {
	gen w_`x'=scalar(`x')-abs(sisben_)
}



reg puntaje_20152 sisben_
outreg2 using "${tabla}rdd_std.xls", addtext(Cuantil,BORRAR) replace
				/* Este solo sirve para que no tenga que borrar manualmente cada vez el archivo, toca borrarlo cada vez*/
		
foreach x in cersum mserd {


reg puntaje_20152 elegible_sisben_completo sisben_ e_sisben1 sisben_2 e_sisben2 ${controles} [iw = w_`x'] ///
if sisben_>=-scalar(`x') & sisben_<=scalar(`x'), r 

outreg2 using "${tabla}rdd_std.xls", addtext(Cuantil,MCO,Tipo de Banda,"`x'",) ///
 label ctitle("Agregado") append less(1)
 

foreach j in .25 .50 .75 .90 {

qreg puntaje_20152 elegible_sisben_completo sisben_ e_sisben1 sisben_2 e_sisben2 ${controles} [iw = w_`x'] ///
if sisben_>=-scalar(`x') & sisben_<=scalar(`x'), q(`j') vce(r) iter(1000)

outreg2 using "${tabla}rdd_std.xls", addtext(Cuantil,`j',Tipo de Banda,"`x'") ///
 ctitle("Agregado") append less(1)
}

}

*************************** Brecha por percentil ***************************

use puntaje_20142 puntaje_sisben_dnp_20142 elegible_sisben_completo using "$base2014_2", clear

foreach var of varlist puntaje_20142 {
		sum `var', d
		replace `var' = (`var' - r(mean))/r(sd) 
}

gen e_total=.
replace e_total=1 if elegible_sisben_completo==1
replace e_total=0 if elegible_sisben_completo==0 | puntaje_sisben_dnp_20142==.

collapse (mean)mean=puntaje_20142 (p25) p25=puntaje_20142 (p50) p50=puntaje_20142 (p75) p75=puntaje_20142 (p90) p90=puntaje_20142 (p95) p95=puntaje_20142 ///
(p99) p99=puntaje_20142, by(e_total)











*********************************************************
*	Ser Pilo Paga - Saber 9						    	*
*   This version: Daniel Pinzon    					    *
*********************************************************	


cd "C:\Users/`c(username)'\Dropbox\Asistencia Investigación\Rachid Laajaj\Ser Pilo Paga\tablas\From DP"
global tabla "C:\Users/`c(username)'\Dropbox\Asistencia Investigación\Rachid Laajaj\Ser Pilo Paga\tablas\From DP\Working\"
global completa "C:\Users/`c(username)'\Dropbox\Asistencia Investigación\Rachid Laajaj\Ser Pilo Paga\data\Finales\base_final_completa.dta"
global completa2 "C:\Users/`c(username)'\Dropbox\Asistencia Investigación\Rachid Laajaj\Ser Pilo Paga\data\Finales\base_final_completa2.dta"
global base2015_2 "C:\Users/`c(username)'\Dropbox\Asistencia Investigación\Rachid Laajaj\Ser Pilo Paga\data\Finales\base_final_2015_completa.dta"
global base2014_2 "C:\Users/`c(username)'\Dropbox\Asistencia Investigación\Rachid Laajaj\Ser Pilo Paga\data\Finales\base_final_2014_parcial.dta"
global base2013_2 "C:\Users/`c(username)'\Dropbox\Asistencia Investigación\Rachid Laajaj\Ser Pilo Paga\data\Finales\base_final_2013_parcial.dta"
global base2013_2_ies "C:\Users\r.laajaj\Dropbox (Uniandes)\PAPERS\SPP Motivation Effect\Ser Pilo Paga\data\Finales\2013_IES.dta"
global base359 "C:\Users\r.laajaj\Dropbox (Uniandes)\PAPERS\SPP Motivation Effect\Ser Pilo Paga\data\Saber_panel_EE.dta"

* RL office:
global base359 "C:\Users\r.laajaj\Dropbox (Uniandes)\Shared Folders\Papers Shared\Ser Pilo Paga\data\Saber_panel_EE.dta"



clear all

set more off, perm
set scheme sj
set seed 1984
set matsize 11000
set maxvar 32000

*********************************************************
*		    		Saber 3, 5 y 9						*
*********************************************************

 // Crear Indicador de porcentaje de elegibles por institucion educativa //

use codigo_dane_sede elegible_sisben_completo using "$base2014_2", clear

collapse elegible_sisben_completo, by(codigo_dane_sede)
rename elegible_sisben_completo p_elegible
tempfile elegibles
save `elegibles'

use "${base359}", clear

* RANKING *

foreach grade in 3 5 9 {

	egen puntaje_`grade'=rowmean(leng_`grade'_promedio mat_`grade'_promedio)
	
}

foreach puntaje of varlist leng*promedio mat*promedio puntaje* {
egen r_`puntaje'=rank(`puntaje'), by(year)

foreach t in 2009 2012 2013 2014 2015 2016 2017 {
	qui sum r_`puntaje' if year==`t', d
	
	* Ranking over 100
	* replace r_`puntaje'=((r_`puntaje'-r(min))/(r(max)-r(min)))*100 if year==`t'
	* Standardized results
	replace r_`puntaje' = (`puntaje'-r(mean))/r(sd) if year==`t'
}
replace `puntaje'=r_`puntaje'
}

// Departamento

gen depto="" 
tostring MUNI_ID, gen(s_muni)

replace depto=substr(s_muni,1,2) if length(s_muni)==5	
replace depto=substr(s_muni,1,1) if length(s_muni)==4

destring depto, replace

replace NIVEL=. if NIVEL==9 // Missing observations
encode CALENDARIO, gen(calendario) // String

rename CODIGO_DANE_EE codigo_dane_sede
merge m:1 codigo_dane_sede using `elegibles'

/*
    Result                           # of obs.
    -----------------------------------------
    not matched                        39,837
        from master                    38,917  (_merge==1) // 10,460 colegios
        from using                        920  (_merge==2) 

    matched                            56,156  (_merge==3) // 8,554
    -----------------------------------------
*/



drop if year==2009

aorder

* DISTR

preserve

keep if year==2015
histogram p_elegible, xtitle("Share of {it:Sisbén} elegible students") ytitle("")
graph export "${tabla}hist_p_elegible.png", replace 

graph drop _all

foreach puntaje of varlist leng_9_desviacion mat_9_desviacion   {
	
	twoway (scatter `puntaje' p_elegible,  msize(.5)) (lfit `puntaje' p_elegible), name("`puntaje'")
	
	}

restore

*********************************************************
*		T.   DIFF - DIFF			*
*********************************************************	

gen trend=0
replace trend=1 if year>=2015

foreach puntaje of varlist leng_9_promedio mat_9_promedio puntaje_9 {

	reghdfe `puntaje' i.trend##c.p_elegible , abs( codigo_dane_sede year MUNI_ID##i.year ZONA##i.year SECTOR##i.year calendario##i.year) vce(r)
	
	* Ranking over 100
	* outreg2 using "${tabla}diff_diff_s9.xls", label append
	* Standardized
	outreg2 using "${tabla}diff_diff_s9_std.xls", label append

	}


*********************************************************
*		TA.   PLACEBO			*
*********************************************************	
	
drop if year>=2015

*2014*	

gen trend14=0
replace trend14=1 if year>=2014


foreach puntaje of varlist leng_9_promedio mat_9_promedio puntaje_9 {

	reghdfe `puntaje' i.trend14##c.p_elegible , abs(codigo_dane_sede MUNI_ID##i.year ZONA##i.year SECTOR##i.year calendario##i.year) vce(r)
	outreg2 using "${tabla}placebo14_s9.xls", label append

	}

*2013*	

gen trend13=0
replace trend13=1 if year>=2013

foreach puntaje of varlist leng_9_promedio mat_9_promedio puntaje_9 {

	reghdfe `puntaje' i.trend13##c.p_elegible , abs(codigo_dane_sede year MUNI_ID##i.year ZONA##i.year SECTOR##i.year calendario##i.year) vce(r)
	outreg2 using "${tabla}placebo13_s9.xls", label append

	}

