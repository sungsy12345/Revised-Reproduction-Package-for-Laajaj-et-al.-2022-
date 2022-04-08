*****************************************************************************************************
*							 Do File - Motivational Effects of SPP	             		         	*
* Reproduces tables 1, 3, 4, A1, A2, A3 and figures 2, 4, 5, 6, 7, A2 and A3 with Saber 11 results	*
*****************************************************************************************************
clear all
cls

/* Danilo Aristizabal
*/
* set a global for the database
global base "C:\Users\Seung Yong Sung\Dropbox\1. Academics\I. Spring 2022\EC 270B Dev\PS\DataReplication\Authors Replication Files\data\"
global tabla "C:\Users\Seung Yong Sung\Dropbox\1. Academics\I. Spring 2022\EC 270B Dev\PS\DataReplication\Authors Replication Files\tables\"
global figura "C:\Users\Seung Yong Sung\Dropbox\1. Academics\I. Spring 2022\EC 270B Dev\PS\DataReplication\Authors Replication Files\figures\"
global regresiones "C:\Users\Seung Yong Sung\Dropbox\1. Academics\I. Spring 2022\EC 270B Dev\PS\DataReplication\Authors Replication Files\regressions\"

use "${base}\originals\SPP_ready.dta", clear

*********************************************************
*** 			BANDWIDTH CALCULATIONS  				*
*********************************************************

** For pre period (year 2013 and 2014) and post (2015) and 2013 and 2014, for MSERD and CERSUM

** Usually desactivated because I ran it only once and then copied the BW below to save time:

/*
* pre mserd:
rdrobust ranking sisben if post == 0, kernel(uniform)  
scalar bw_pre_mserd = e(h_r)
* post mserd:
rdrobust ranking sisben if post == 1, kernel(uniform) 
scalar bw_post_mserd = e(h_r)
* 2013 mserd
rdrobust ranking sisben if year == 2013, kernel(uniform)
scalar bw_2013_mserd = e(h_r)
* 2014 mserd:
rdrobust ranking sisben if year == 2014, kernel(uniform)
scalar bw_2014_mserd = e(h_r) 

scalar bw_dif_mserd  = (bw_pre_mserd +bw_post_mserd )/2

* pre cersum:
rdrobust ranking sisben if post == 0, kernel(uniform) bwselect(cersum)
scalar bw_pre_cersum = e(h_l) 
* post cersum:
rdrobust ranking sisben if post == 1, kernel(uniform) bwselect(cersum)
scalar bw_post_cersum = e(h_l) 
* 2013 cersum
rdrobust ranking sisben if year == 2013, kernel(uniform) bwselect(cersum)
scalar bw_2013_cersum = e(h_l)
* 2014 cersum:
rdrobust ranking sisben if year == 2014, kernel(uniform) bwselect(cersum)
scalar bw_2014_cersum = e(h_l) 

scalar bw_dif_cersum = (bw_pre_cersum +bw_post_cersum)/2 

scalar bw_2013_14_cersum = (bw_2013_cersum +bw_2014_cersum)/2 


scalar list _all // copied below from this result


* pre cerrd:
rdrobust ranking sisben if post == 0, kernel(uniform) bwselect(cerrd)
scalar bw_pre_cerrd = e(h_l) 
* post cerrd:
rdrobust ranking sisben if post == 1, kernel(uniform) bwselect(cerrd)
scalar bw_post_cerrd = e(h_l) 
*dif: avg between pre and post:
scalar bw_dif_cerrd = (bw_pre_cerrd +bw_post_cerrd)/2 

scalar list _all

* pre msesum:
rdrobust ranking sisben if post == 0, kernel(uniform) bwselect(msesum)
scalar bw_pre_msesum = e(h_l) 
* post msesum:
rdrobust ranking sisben if post == 1, kernel(uniform) bwselect(msesum)
scalar bw_post_msesum = e(h_l) 
*dif: avg between pre and post:
scalar bw_dif_msesum = (bw_pre_msesum +bw_post_msesum)/2 

***** I suggest adding notes here as to what these other choices of bandwidth are and why CERSUM was deemed optimal.

*/

scalar bw_2013_14_cersum =   3.874886
scalar bw_dif_cersum =  3.3502511
scalar bw_2014_cersum =   4.715694
scalar bw_2013_cersum =  3.0340779
scalar bw_post_cersum =  2.9604268
scalar bw_2015_cersum =  2.9604268
scalar bw_pre_cersum =  3.7400753

scalar bw_dif_mserd =  6.3925306
scalar bw_2014_mserd =  6.1100027
scalar bw_2013_mserd =  7.8075754
scalar bw_post_mserd =  5.8188645
scalar bw_pre_mserd =  6.9661967

scalar bw_dif_msesum =  6.4639249
scalar bw_post_msesum =  5.6099094
scalar bw_pre_msesum =  7.3179403

scalar bw_dif_cerrd =  3.3155002
scalar bw_post_cerrd =  3.0706953
scalar bw_pre_cerrd =  3.5603052


global controls area_ciudades area_urbano icfes_padre_nivel1 icfes_padre_nivel2 ///
icfes_padre_nivel3 icfes_madre_nivel1 icfes_madre_nivel2 icfes_madre_nivel3 ///
edad sexo_sisben departamento_cod_1-departamento_cod_32 saber_rk_col

global controls_std area_ciudades area_urbano icfes_padre_nivel1 icfes_padre_nivel2 ///
icfes_padre_nivel3 icfes_madre_nivel1 icfes_madre_nivel2 icfes_madre_nivel3 ///
edad sexo_sisben departamento_cod_1-departamento_cod_32 saber_std_col

global controls_orig area_ciudades area_urbano icfes_padre_nivel1 icfes_padre_nivel2 ///
icfes_padre_nivel3 icfes_madre_nivel1 icfes_madre_nivel2 icfes_madre_nivel3 ///
edad sexo_sisben departamento_cod_1-departamento_cod_32 saber_orig_col

st 

*Run until here. Then, each figure or table can be run independetly

***************************************************************
***  FIGURE 2 NEED-BASED AND ACADEMIC ELIGIBILITY CRITERIA	***
***************************************************************
use "${base}\originals\SPP_ready.dta", clear

keep if year == 2015

twoway (histogram puntaje_sisben_dnp, fcolor(black) bcolor(gray)) ///
(scatteri 0 57.21 0.025 57.21 (3), c(l) m(i) color(red) ) ///
(scatteri 0 56.32 0.025 56.32 (9), c(l) m(i) color(red))  ///
(scatteri 0 40.75 0.025 40.75 (9), c(l) m(i) color(red)), ///
	text(0.025 57.21 "14 Cities", place(e) size(large)) ///
	text(0.025 56.32 "Urban", place(w) size(large))  ///
	text(0.025 40.75 "Rural", place(w) size(large)) ///
legend(off) xtitle("{it:Sisbén} -  Socioeconomic Index") ///
ytitle("Density") graphregion(style(none) color(gs16)) ///
ylabel(, format(%04.3f) angle(horizontal)) name(puntaje_sisben, replace)
graph export "${figura}figure_2a.png", replace
graph export "${figura}figure_2a.eps", replace

twoway (histogram puntaje_saber, fcolor(black) bcolor(gray)) ///
(scatteri 0 318 0.01 318, c(l) m(i) color(red)), ///
text(0.01 318 "Academic Criteria", place(e) size(large)) legend(off) ///
xtitle("Saber 11 - High School Exit Exam") ytitle("Density")	///
graphregion(style(none) color(gs16)) ///
ylabel(, format(%04.3f) angle(horizontal))  name(puntaje_saber, replace)
graph export "${figura}figure_2b.png", replace
graph export "${figura}figure_2b.eps", replace

graph combine puntaje_sisben puntaje_saber, graphregion(color(white))
graph export "${figura}figure_2.png", replace
graph export "${figura}figure_2.eps", replace

********************************************************************************************************
* TABLE 1 SABER 11 RANK OF ELIGIBLE AND NON-ELIGIBLE STUDENTS BEFORE AND AFTER THE MOTIVATIONAL EFFECT *
********************************************************************************************************
use "${base}originals\SPP_ready.dta", clear

format %9.1f ranking

preserve
collapse (count) n=ranking (mean) mean=ranking (p25) p25=ranking (p50) p50=ranking (p75) p75=ranking (p90) p90=ranking , by(post non_eligible)

export excel "${tabla}descriptive", firstrow(variables) sheet("descript", replace) // This goes into table 1A
* This provides the descriptive statistics for the sisben eleigible group, non-elibile group in pre-period and post-period
* with sisben and non-eligible group without sisben 
* This is copied (on the side) in Table 1 (A & B)

restore

reg ranking eligible_post eligible post, r
outreg2 using "${tabla}descr_dif.xls", label ///
ctitle("Dif_Diff") addtext(Cuantil,MCO) replace less(1)

foreach j in .25 .50 .75 .90 {

qreg ranking eligible_post eligible post, q(`j') vce(r) 
outreg2 using "${tabla}descr_dif.xls", ///
addtext(Cuantil,`j') ctitle("Agregado") append less(1) // // This goes into table 1C (Diff in diff results with significance level)

}

** Some of the results are redundant with the table above but we keep the coef, significance level (stars) and SE of the coef eligible_post
* This is copied (on the side) in Table 1 (C)

**************************************************************************
* TABLE 2: OTHER PROGRAMS THAT SHARE THE SAME SISBEN ELIGIBILITY CUTOFF  *
**************************************************************************
* ALL TAKEN FROM INFO AVAILABLE ONLINE (NO ESTIMATION INCLUDED)


*****************************************************************************************************************
* TABLE 3 DIFFERENCE-IN-DISCONTINUITIES ESTIMATIONS OF THE MOTIVATIONAL EFFECT ON RANKS OF SABER 11 TEST SCORES *
*****************************************************************************************************************
use "${base}originals\SPP_ready.dta", clear

*DIF IN RD
*MEAN EFFECT:
reg ranking eligible_post eligible post sisben sisben_eligible sisben_post sisben_eligible_post  ${controls} if abs(sisben)<bw_dif_cersum , ro
outreg2 using "${tabla}/DIF_RD_main.xls", addtext(Cuantil,"OLS Dif RD", Bandwidth, "`bw_dif_cersum'") ctitle("CERSUM") less(1) keep(eligible_post eligible) nocons replace

*QUANTILE EFFECTS
foreach q in 25 50 75 90 {
qreg ranking eligible_post eligible post sisben sisben_eligible sisben_post sisben_eligible_post  ${controls} if abs(sisben)<bw_dif_cersum , q(`q') vce(r) iter(1000) 
outreg2 using "${tabla}/DIF_RD_main.xls", addtext(Cuantil,"`q' Dif RD", Bandwidth, `bw_dif_cersum') ctitle("CERSUM") less(1) keep(eligible_post eligible) nocons append
}

* As done in excel sheet, to obtain gap reduction, the value of the coefficients are divided by the gap estimated in Table 1A

***********************************************************************************************************************
* TABLE 4 DIFFERENCE-IN-DISCONTINUITIES AND RD ESTIMATIONS OF DISCONTINUITY IN CONTROLS AROUND THE ELIGIBILITY CUTOFF *
***********************************************************************************************************************
use "${base}originals\SPP_ready.dta", clear

*DIF IN RD on CONTROLS
local rep_app = "replace"

foreach y in area_ciudades area_urbano icfes_padre_nivel1 icfes_padre_nivel2 ///
icfes_padre_nivel3 icfes_madre_nivel1 icfes_madre_nivel2 icfes_madre_nivel3 ///
edad sexo_sisben saber_rk_col {
qui reg `y' eligible_post eligible post sisben sisben_eligible sisben_post sisben_eligible_post if abs(sisben)<bw_dif_cersum , ro
outreg2 using "${tabla}DIF_RD_Controls.xls", dec(3) addtext(Cuantil,"OLS Dif RD", Bandwidth, bw_dif_cersum) less(1) keep(eligible_post eligible) nocons `rep_app'
local rep_app = "append" 
}


*RD on CONTROLS 
foreach y in area_ciudades area_urbano icfes_padre_nivel1 icfes_padre_nivel2 ///
icfes_padre_nivel3 icfes_madre_nivel1 icfes_madre_nivel2 icfes_madre_nivel3 ///
edad sexo_sisben saber_rk_col {
qui reg `y'  eligible  sisben sisben_eligible if abs(sisben)<bw_post_cersum & year == 2015, ro 
outreg2 using "${tabla}DIF_RD_Controls.xls", dec(3) addtext(Cuantil,"OLS 2015", Bandwidth, "`bw_post_cersum'") less(1) keep(eligible) nocons append 

}

*******************************************************************************************
***  FIGURE 4. GRAPHICAL REPRESENTATION OF THE DIFFERENCE-IN-DISCONTINUITIES ESTIMATORS ***
*******************************************************************************************
scalar drop _all

use "${base}originals\SPP_ready.dta", clear

scalar bw_dif_cersum =  3.3502511

global controls area_ciudades area_urbano icfes_padre_nivel1 icfes_padre_nivel2 ///
icfes_padre_nivel3 icfes_madre_nivel1 icfes_madre_nivel2 icfes_madre_nivel3 ///
edad sexo_sisben departamento_cod_1-departamento_cod_32 saber_rk_col

set seed 1984

scalar bw =  bw_dif_cersum
scalar small_binsize = .01
scalar large_binsize = .3

***** Generate data with 2014 for Kernel graph of 2015 - 2014 *********
******* Prepare data 2014 for Graph Kernel 90th percentile  ***************
use "${base}originals\SPP_ready.dta", clear

keep if abs(sisben)<bw 

gen pre = (post == 0) if post < .

* Generates large bins that will appear in the graph:

gen large_bins = round(sisben + large_binsize/2, large_binsize)- large_binsize/2 if abs(sisben)<bw  

* Small bins gives a smaller unit

gen small_bins = round(sisben,small_binsize)

* for OLS:

reg ranking ${controls} post , ro
predict resid_ols, residuals

* Calculation of local percentiles:

foreach t in pre post {
bysort small_bins: egen ranking_ols_`t'=mean(ranking)  if `t' == 1 //, by(small_bins) 
bysort small_bins: egen ranking_ols_r_`t'=mean(resid_ols)  if `t' == 1  //, by(small_bins)
bysort large_bins: egen ranking_ols_r_largeb_`t'  = mean(resid_ols)  if `t' == 1
}

** Adjustment for the constant (so that the average is the same as the data)
foreach t in pre post {
foreach x in ranking_ols_r_`t' ranking_ols_r_largeb_`t' {
qui sum `x' if `t' == 1
scalar m_`x' = r(mean)
qui sum ranking if `t' == 1 
scalar ols_ranking = r(mean)
scalar dify = ols_ranking - m_`x' 
replace `x' = `x' + dify if `t' == 1
}
}

bysort post: sum ranking ranking_ols_r_* 

** For Quantile estimations

foreach q in 25 50 75 90 {

qui qreg ranking ${controls} post , q(`q') vce(r)
predict resid_`q', residuals

* Calculation of local percentiles:

foreach t in pre post {
egen ranking_p`q'_`t'=pctile(ranking) if `t' == 1, by(small_bins) p(`q') 
egen ranking_p`q'r_`t'=pctile(resid_`q') if `t' == 1, by(small_bins) p(`q') 
egen ranking_p`q'r_largeb_`t' = pctile(resid_`q') if `t' == 1, by(large_bins) p(`q') 
}


** Adjustment for the constant (so that the average is the same as the data)
foreach t in pre post {
foreach x in ranking_p`q'r_`t' ranking_p`q'r_largeb_`t' {
qui sum `x' if `t' == 1
scalar m_`x' = r(mean)
qui sum ranking if `t' == 1 , d 
scalar p`q'_ranking = r(p`q')
scalar dify = p`q'_ranking - m_`x' 
replace `x' = `x' + dify if `t' == 1
}
}
}

* One will be used for counting and then weight based on number of observations used to calculate the percentile
* Now for predictions, we need to replace each control by its average value:
foreach x in area_ciudades	area_urbano	icfes_padre_nivel1	icfes_padre_nivel2	icfes_padre_nivel3	icfes_madre_nivel1	icfes_madre_nivel2	icfes_madre_nivel3	edad	sexo_sisben	departamento_cod_1	departamento_cod_2	departamento_cod_3	departamento_cod_4	departamento_cod_5	departamento_cod_6	departamento_cod_7	departamento_cod_8	departamento_cod_9	departamento_cod_10	departamento_cod_11	departamento_cod_12	departamento_cod_13	departamento_cod_14	departamento_cod_15	departamento_cod_16	departamento_cod_17	departamento_cod_18	departamento_cod_19	departamento_cod_20	departamento_cod_21	departamento_cod_22	departamento_cod_23	departamento_cod_24	departamento_cod_25	departamento_cod_26	departamento_cod_27	departamento_cod_28	departamento_cod_29	departamento_cod_30	departamento_cod_31	departamento_cod_32	saber_rk_col {
qui sum `x'																																										
scalar aux = r(mean)
qui replace `x' = aux
}


foreach t in pre post {
gen one_`t' = 1 if `t' == 1
}

* Collapse by bins, so that we keep only 1 observation per bin, which makes us avoid artificial significance through repetition of observations:
collapse ranking_* sisben large_bins eligible_post eligible post sisben_eligible sisben_post sisben_eligible_post  ${controls} (count) one_pre one_post, by(small_bins)
* ranking_p*

foreach x in ranking_ols_r_ ranking_ols_r_largeb_ {
gen `x'dif = `x'post - `x'pre 
}


foreach q in 25 50 75 90 {
foreach x in ranking_p`q'r_ ranking_p`q'r_largeb_ {
gen `x'dif = `x'post - `x'pre 
}
}

save "${base}processed\bybeans_pre_post_dif.dta", replace

*OLS version:

use "${base}originals\SPP_ready.dta", clear

reg ranking eligible_post eligible post sisben sisben_eligible sisben_post sisben_eligible_post  ${controls} if abs(sisben)<bw , ro  

use "${base}processed\bybeans_pre_post_dif.dta", clear

replace eligible_post = eligible
replace eligible_post = 1 if eligible_post > 0.5  // to adjust one case that is slightly in between but with average like .85
replace sisben_post = sisben
replace sisben_eligible_post = sisben_eligible

foreach x in post eligible sisben sisben_eligible{
replace `x' = 0
}

predict yh_ols, xb
predict y_stdp_ols, stdp
gen ci_h_ols=.
gen ci_l_ols=.

replace ci_h_ols=yh_ols+1.96*y_stdp_ols 
replace ci_l_ols=yh_ols-1.96*y_stdp_ols

** Adjustment for the constant (so that the average is the same as the `q'th percentile)

qui sum yh_ols
scalar m_yh_ols = r(mean)
qui sum ranking_ols_r_dif
scalar ols_ranking_dif = r(mean)
scalar dify = ols_ranking_dif - m_yh_ols
replace yh_ols = yh_ols + dify

replace ci_h_ols=yh_ols+1.96*y_stdp_ols 
replace ci_l_ols=yh_ols-1.96*y_stdp_ols
 
* Makes graph of the main estimation (linear fit on each side):

replace sisben = small_bins 

* Make the graph for average effect using the sisben score at the cutoff

twoway (scatter ranking_ols_r_largeb_dif large_bins if large_bins == small_bins, msymbol(O) mcolor(gray)) /*
*/ (line yh_ols  small_bins if sisben <0, pstyle(p) sort lcolor(blue))/*
*/ (line  ci_l_ols  small_bins if sisben <0, pstyle(p3 p3) lpattern(dash)  sort  lcolor(green)) /*
*/ (line   ci_h_ols small_bins if sisben <0, pstyle(p3 p3) lpattern(dash)  sort  lcolor(green)) /*
*/ (line yh_ols  small_bins if sisben >0, pstyle (p) sort lcolor(blue)) /*
*/ (line  ci_l_ols  small_bins if sisben >0, pstyle (p3 p3) lpattern(dash)  sort  lcolor(green)) /*
*/ (line  ci_h_ols small_bins if sisben >0, pstyle (p3 p3) lpattern(dash)  sort lcolor(green)), /*
*/  ytitle("Ranking change in average" " ") xtitle("		Eligible         Not Eligible" " " "{it:Sisbén} score (centered at cutoff)") legend( label(1 "Change by bin" ) label(2 "Dif in RD linear prediction")  label(3 "95% CI of linear prediction") order(2 1 3)) title(Average Effect) graphregion(style(none) color(gs16))  bgcolor(white) xline(0,lcolor(red)) name(DifRD_ols)

graph export "${figura}DIf_RD_Fig_Lin_ols.png", replace

graph save "${figura}DIf_RD_Fig_Lin_ols.gph", replace
 
 
foreach q in 25 50 75 90 {
use "${base}originals\SPP_ready.dta", clear

qui qreg ranking eligible_post eligible post sisben sisben_eligible sisben_post sisben_eligible_post  ${controls} if abs(sisben)<bw , q(`q') vce(r)  

use "${base}processed\bybeans_pre_post_dif.dta", clear

replace eligible_post = eligible
replace eligible_post = 1 if eligible_post > 0.5  // to adjust one case that is slightly in between but with average like .85
replace sisben_post = sisben
replace sisben_eligible_post = sisben_eligible

foreach x in post eligible sisben sisben_eligible{
replace `x' = 0
}

predict yh_`q', xb
predict y_stdp_`q', stdp
gen ci_h_`q'=.
gen ci_l_`q'=.

replace ci_h_`q'=yh_`q'+1.96*y_stdp_`q' 
replace ci_l_`q'=yh_`q'-1.96*y_stdp_`q'


** Adjustment for the constant (so that the average is the same as the `q'th percentile)

qui sum yh_`q'
scalar m_yh_`q' = r(mean)
qui sum ranking_p`q'r_dif
scalar p`q'_ranking_dif = r(mean)
scalar dify = p`q'_ranking_dif - m_yh_`q'
replace yh_`q' = yh_`q' + dify

replace ci_h_`q'=yh_`q'+1.96*y_stdp_`q' 
replace ci_l_`q'=yh_`q'-1.96*y_stdp_`q' 

replace sisben = small_bins 
 
* Makes graph of the main estimation for percetiles 25, 50, 75 and 90 (linear fit on each side):

twoway (scatter ranking_p`q'r_largeb_dif large_bins if large_bins == small_bins, msymbol(O) mcolor(gray)) /*
*/ (line yh_`q'  small_bins if sisben <0, pstyle(p) sort lcolor(blue)) /*
*/ (line  ci_l_`q'  small_bins if sisben <0, pstyle(p3 p3) lpattern(dash)  sort  lcolor(green)) /*
*/ (line   ci_h_`q' small_bins if sisben <0, pstyle(p3 p3) lpattern(dash)  sort  lcolor(green)) /*
*/ (line yh_`q'  small_bins if sisben >0, pstyle (p) sort lcolor(blue)) /*
*/ (line  ci_l_`q'  small_bins if sisben >0, pstyle (p3 p3) lpattern(dash)  sort  lcolor(green)) /*
*/ (line  ci_h_`q' small_bins if sisben >0, pstyle (p3 p3) lpattern(dash)  sort lcolor(green)), /*
*/  ytitle("Ranking change in `q'{superscript:th} percentile" " ") xtitle("		Eligible         Not Eligible" " " "{it:Sisbén} score (centered at cutoff)") legend( label(1 "Change by bin" ) label(2 "Dif in RD linear prediction")  label(3 "95% CI of linear prediction") order(2 1 3)) title(`q'th percentile) graphregion(style(none) color(gs16))  bgcolor(white) xline(0,lcolor(red)) name(DifRD_`q', replace)

graph export "${figura}DIf_RD_Fig_Lin_`q'.png", replace
*graph export "${tabla}RD_Fig_Quad_`q'.eps", replace
graph save "${figura}DIf_RD_Fig_Lin_`q'.gph", replace
}
 
grc1leg DifRD_ols DifRD_25 DifRD_50, rows(1) name(row_1,replace) graphregion(color(white))
grc1leg DifRD_75 DifRD_90, rows(1) name(row_2, replace) graphregion(color(white))
grc1leg row_1 row_2, cols(1) graphregion(color(white))

graph export "${figura}figure_4.png", replace
graph export "${figura}figure_4.eps", replace
graph save "${figura}figure_4.gph", replace


****************************************************************************************
*** FIGURE 5 RD ESTIMATIONS OF THE GAP REDUCTION IN THE SABER 11 RANKS BY PERCENTILE ***
****************************************************************************************
use "${base}originals\SPP_ready.dta", clear

** Calculations available in SPP Tables do file

scalar bw_2013_14_cersum =   3.874886
scalar bw_dif_cersum =  3.3502511
scalar bw_2014_cersum =   4.715694
scalar bw_2013_cersum =  3.0340779
scalar bw_post_cersum =  2.9604268
scalar bw_2015_cersum =  2.9604268
scalar bw_pre_cersum =  3.7400753

scalar bw_dif_mserd =  6.3925306
scalar bw_2014_mserd =  6.1100027
scalar bw_2013_mserd =  7.8075754
scalar bw_post_mserd =  5.8188645
scalar bw_pre_mserd =  6.9661967

scalar bw_dif_msesum =  6.4639249
scalar bw_post_msesum =  5.6099094
scalar bw_pre_msesum =  7.3179403

scalar bw_dif_cerrd =  3.3155002
scalar bw_post_cerrd =  3.0706953
scalar bw_pre_cerrd =  3.5603052

global controls area_ciudades area_urbano icfes_padre_nivel1 icfes_padre_nivel2 ///
icfes_padre_nivel3 icfes_madre_nivel1 icfes_madre_nivel2 icfes_madre_nivel3 ///
edad sexo_sisben departamento_cod_1-departamento_cod_32 saber_rk_col

global controls_std area_ciudades area_urbano icfes_padre_nivel1 icfes_padre_nivel2 ///
icfes_padre_nivel3 icfes_madre_nivel1 icfes_madre_nivel2 icfes_madre_nivel3 ///
edad sexo_sisben departamento_cod_1-departamento_cod_32 saber_std_col

global controls_orig area_ciudades area_urbano icfes_padre_nivel1 icfes_padre_nivel2 ///
icfes_padre_nivel3 icfes_madre_nivel1 icfes_madre_nivel2 icfes_madre_nivel3 ///
edad sexo_sisben departamento_cod_1-departamento_cod_32 saber_orig_col


* GRAPH PERCENTILE BY PERCENTILE 

*** Dif in RD Results for each percentile then divided by its initial gap (obtained by simple qreg without controls)*

*DIF IN RD

local c=1

mat b=J(2,99,.)

*QUANTILE EFFECTS

forval q = 1/99 {
qui qreg ranking eligible_post eligible post sisben sisben_eligible sisben_post sisben_eligible_post  ${controls} if abs(sisben)<bw_dif_cersum , q(`q') vce(r)  
display  "`q'" 

mat C=e(b) // we save the matrix of coefficients (the first one is the one of interest for the eligible_post coefficient)
mat D=e(V) // we save the matrix of Variance (the first one is the one of interest)

mat b[1,`c']=C[1,1] // coefficient of the variable eligible_post (the dif in RD estimator)

mat b[2,`c']=sqrt(D[1,1]) // Variance of the variable eligible_post (the dif in RD estimator)

local c =`c'+1 // next column

}

* To rename variables for the coefficient and sd
mat rown b= coef sd

mat l b

mat C=b[1..2,1..99]

* Now create the matrix with the coefficients of the regression ranking over elegible if year == 2013 or year 2014 
local co=1

mat bo=J(2,99,.)

*GAP FOR EACH QUANTILE
forval q = 1/99 {
qui qreg ranking eligible if year == 2014 | year==2013, q(`q')
display  "`q'" 

mat Co=e(b) // we save the matrix of coefficients (the first one is the one of interest for the eligible_post coefficient)
mat Do=e(V) // we save the matrix of Variance (the first one is the one of interest)

mat bo[1,`co']=Co[1,1] // coefficient of the variable eligible_post (the dif in RD estimator)

mat bo[2,`co']=sqrt(Do[1,1]) // Variance of the variable eligible_post (the dif in RD estimator)

local co =`co'+1 // next column

}

* To rename variables for the coefficient and sd
mat rown bo= coef2 sd2

mat l bo

mat Co=bo[1..2,1..99]

* Now we append the two matrices with the coefficients and the standard deviations
preserve
mat define Coeficiente2 = Co'
clear
svmat Coeficiente2 
gen percentil = _n
rename (Coeficiente21 Coeficiente22) (coef2 sd2) 
save "${regresiones}reg1.dta", replace

mat define Coeficiente1 = C'
clear
svmat Coeficiente1 
gen percentil = _n
rename (Coeficiente11 Coeficiente12) (coef sd) 
save "${regresiones}reg2.dta", replace

merge 1:1 percentil using "${regresiones}reg1.dta"

* To compute gap reduction
gen estimador = -coef/coef2
* To compute SE
gen SE = sd/coef2
* To compute t lower and t upper
gen lower_limit = estimador - (SE * 1.65)
gen upper_limit = estimador + (SE * 1.65)
* To leave with percentile 6 to 95
drop if percentil<7
drop if percentil>95
* Graph region is -0.2 to 0.4
replace lower_limit=. if lower_limit > 0.4 
replace upper_limit=. if upper_limit < -0.2 

* To make the graph for the different percentiles
twoway (connected estimador percentil, msize(vtiny) sort lcolor(navy)) (line lower_limit percentil, pstyle(p p3 p3) sort lpattern(dash) lcolor(green)) /*
*/ (line upper_limit percentil, pstyle(p p3 p3) sort lpattern(dash) lcolor(green)), /*
*/  xtitle(Percentile) yline(0, lcolor(cranberry)) graphregion(color(white)) bgcolor(white) legend(off) yscale(range(-0.2 0.4)) ylabel(minmax) ylabel(-0.2(0.1)0.4) xlabel(0(10)100)

graph export "${figura}figure_5.png", as(png) replace

restore

*******************************************************************************
*** FIGURE 6 DIFFERENCE-IN-DISCONTINUITIES EFFECTS FOR DIFFERENT BANDWIDTHS ***
*******************************************************************************
use "${base}originals\SPP_ready.dta", clear

foreach q in 25 50 75 90 {

cap mat drop b
cap mat drop C
cap mat drop D
cap mat drop coef

local c=1
mat b=J(4,15,.)

forval x = 1/15 {
display  "`x'" 
qui qreg ranking eligible_post eligible post sisben sisben_eligible sisben_post sisben_eligible_post  ${controls} if abs(sisben) < scalar(`x') , vce(r) q(`q') 

mat C=e(b) // we save the matrix of coefficients (the first one is the one of interest for the eligible_post coefficient)
mat D=e(V) // we save the matrix of Variance (the first one is the one of interest)

mat b[1,`c']=C[1,1] // coefficient of the variable eligible_post (the dif in RD estimator)
mat b[2,`c']=sqrt(D[1,1]) // Variance of the variable eligible_post (the dif in RD estimator)
local c =`c'+1 // next column

}

** To obtain upper and lower bounds:
local c=1
foreach x in 1 2 3 4  5  6  7  8  9  10  11  12  13 14 15  {
mat b[3,`c']=b[1,`c']+ 1.96 * b[2,`c'] // Upper Bound
mat b[4,`c']=b[1,`c']- 1.96 * b[2,`c']  // Lower Bound
local c =`c'+1 // next column
}

mat coln b=   1 2 3 4 5 6  7  8  9  10  11  12  13 14 15
mat rown b= coef sd t_l t_u
mat l b
mat C=b[1..4,1..15]

* To take the RD coefficients, t lower and t upper to make the graph
matrix coef = b'

* Make the graph using AEJ Applied format
preserve
clear
svmat coef, names(col)
gen bandwidth = _n

twoway (connected coef bandwidth, pstyle(p p3 p3) sort lcolor(navy)) (line t_l bandwidth, pstyle(p p3 p3) sort lpattern(dash) lcolor(green)) /*
*/ (line t_u bandwidth, pstyle(p p3 p3) sort lpattern(dash) lcolor(green)), /*
*/ title(`q'th percentile) ytitle("RD Effect of eligibility at the `q'th pctile") xtitle(Bandwidth) xlabel(minmax) yline(0, lcolor(cranberry)) graphregion(color(white)) bgcolor(white) legend(off) name(pectile_`q', replace) ylabel(0(1)4) xlabel(1(1)15)

restore
graph export "${figura}coef_`q'.png", as(png) replace

}

* Continue building figure 6 for average effect
cap mat drop b
cap mat drop C
cap mat drop D
cap mat drop coef

local c=1

mat b=J(4,15,.)

forval x = 1/15 {
display  "`x'" 
qui reg ranking eligible_post eligible post sisben sisben_eligible sisben_post sisben_eligible_post  ${controls} if abs(sisben) < scalar(`x') , vce(r) 

mat C=e(b) // we save the matrix of coefficients (the first one is the one of interest for the eligible_post coefficient)
mat D=e(V) // we save the matrix of Variance (the first one is the one of interest)

mat b[1,`c']=C[1,1] // coefficient of the variable eligible_post (the dif in RD estimator)

mat b[2,`c']=sqrt(D[1,1]) // Variance of the variable eligible_post (the dif in RD estimator)

local c =`c'+1 // next column

}


** To obtain upper and lower bounds:
local c=1

foreach x in 1 2 3 4  5  6  7  8  9  10  11  12  13 14 15  {

mat b[3,`c']=b[1,`c']+ 1.96 * b[2,`c'] // Upper Bound

mat b[4,`c']=b[1,`c']- 1.96 * b[2,`c']  // Lower Bound

local c =`c'+1 // next column

}

* To rename variables for the coefficient, sd, t lower and t upper
mat coln b=   1 2 3 4 5 6  7  8  9  10  11  12  13 14 15

mat rown b= coef sd t_l t_u

mat l b

mat C=b[1..4,1..15]

* To take the RD coefficient for OLS, t lower and t upper to make the graph
matrix coef = b'

* Make the graph using AEJ Applied format
preserve
clear
svmat coef, names(col)
gen bandwidth = _n

twoway (connected coef bandwidth, pstyle(p p3 p3) sort lcolor(navy)) (line t_l bandwidth, pstyle(p p3 p3) sort lpattern(dash) lcolor(green)) /*
*/ (line t_u bandwidth, pstyle(p p3 p3) sort lpattern(dash) lcolor(green)), /*
*/ title(Average Effect) ytitle("RD Effect of eligibility for OLS") xtitle(Bandwidth) xlabel(minmax) yline(0, lcolor(cranberry)) graphregion(color(white)) bgcolor(white) legend(off) name(ols, replace) ylabel(0(1)4) xlabel(1(1)15)

restore

graph export "${figura}coef_ols.png", as(png) replace

* To combine all percentiles graphs in one including ols 
graph combine ols pectile_25 pectile_50, rows(1) name(row_1, replace) graphregion(color(white))
graph combine pectile_75 pectile_90, rows(1) name(row_2,replace) graphregion(color(white))
graph combine row_1 row_2, cols(1) graphregion(color(white))

graph export "${figura}figure_6.png", as(png) replace
graph export "${figura}figure_6.eps", replace
graph save "${figura}figure_6.gph", replace


***********************************************************************
***	  FIGURE 7. REGRESSION DISCONTINUITY EFFECTS AT EACH YEAR		***
***********************************************************************
use "${base}originals\SPP_ready.dta", clear

* Estimate the RD coefficient for each percentile using qreg command
foreach q in 25 50 75 90 {

cap mat drop b
cap mat drop C
cap mat drop D
cap drop coeficiente
cap drop t_l
cap drop t_u

local c=1

mat b=J(4,3,.)

foreach x in 2013 2014 2015  {
display  "`x'" 
qreg ranking eligible sisben sisben_eligible  ${controls} if abs(sisben) < bw_`x'_cersum & year == `x' , vce(r) q(`q') 

mat C=e(b) // we save the matrix of coefficients (the first one is the one of interest for the eligible_post coefficient)
mat D=e(V) // we save the matrix of Variance (the first one is the one of interest)

mat b[1,`c']=C[1,1] // coefficient of the variable eligible_post (the dif in RD estimator)

mat b[2,`c']=sqrt(D[1,1]) // Variance of the variable eligible_post (the dif in RD estimator)

local c =`c'+1 // next column

}

** To obtain upper and lower bounds:
local c=1

foreach x in  2013 2014 2015  {
mat b[3,`c']=b[1,`c']+ 1.96 * b[2,`c'] // Upper Bound
mat b[4,`c']=b[1,`c']- 1.96 * b[2,`c']  // Lower Bound
local c =`c'+1 // next column
}

* To rename variables for the coefficient, sd, t lower and t upper
mat coln b=   2013 2014 2015
mat rown b= coef sd t_l t_u
mat l b
mat C=b[1..4,1..3]

* To create variables for the RD coefficient for each year
gen coeficiente = b[1,1] if year==2013
replace coeficiente = b[1,2] if year==2014
replace coeficiente = b[1,3] if year==2015

* To create variables for the t lower for each year
gen t_l= b[3,1] if year==2013
replace t_l = b[3,2] if year==2014
replace t_l = b[3,3] if year==2015

* To create variables for the t upper for each year
gen t_u= b[4,1] if year==2013
replace t_u = b[4,2] if year==2014
replace t_u = b[4,3] if year==2015

* Collapse by year the RD coefficients and make the graph 
preserve
collapse (mean) coeficiente t_l t_u, by(year)
twoway (connected coeficiente year, pstyle(p p3 p3) sort lcolor(navy)) (line t_l year, pstyle(p p3 p3) sort lpattern(dash) lcolor(green)) /*
*/ (line t_u year, pstyle(p p3 p3) sort lpattern(dash) lcolor(green)), /*
*/ title(`q'th percentile) ytitle("RD Effect of eligibility at the `q'th pctile") xtitle(Year) xlabel(minmax) yline(0, lcolor(cranberry)) graphregion(color(white)) bgcolor(white) legend(off) name(pct_`q', replace) ylabel(-3(0.5)2) xlabel(2013(1)2015)
restore

graph export "${figura}coef_`q'_(sameaxis).png", as(png) replace

}

* Continue building figure 7 for the average effect
cap mat drop b
cap mat drop C
cap mat drop D
cap drop coeficiente
cap drop t_l
cap drop t_u

local c=1

mat b=J(4,3,.)

foreach x in 2013 2014 2015  {
display  "`x'" 
reg ranking eligible sisben sisben_eligible  ${controls} if abs(sisben) < bw_`x'_cersum & year == `x' , vce(r)

mat C=e(b) // we save the matrix of coefficients (the first one is the one of interest for the eligible_post coefficient)
mat D=e(V) // we save the matrix of Variance (the first one is the one of interest)

mat b[1,`c']=C[1,1] // coefficient of the variable eligible_post (the dif in RD estimator)

mat b[2,`c']=sqrt(D[1,1]) // Variance of the variable eligible_post (the dif in RD estimator)

local c =`c'+1 // next column

}

** To obtain upper and lower bounds:
local c=1

foreach x in  2013 2014 2015  {
mat b[3,`c']=b[1,`c']+ 1.96 * b[2,`c'] // Upper Bound
mat b[4,`c']=b[1,`c']- 1.96 * b[2,`c']  // Lower Bound
local c =`c'+1 // next column
}

* To rename variables for the coefficient, sd, t lower and t upper
mat coln b=   2013 2014 2015
mat rown b= coef sd t_l t_u
mat l b
mat C=b[1..4,1..3]

coefplot matrix(C[1]), ci((3 4)) vertical title(OLS) xtitle("Year") ytitle("RD Effect of eligibility OLS") yline(0,lpattern(dash)) ///
graphregion(style(none) color(gs16))  bgcolor(white) 

graph export "${figura}coef_ols.pdf", as(pdf) replace

* To create variables for the RD coefficient for each year
gen coeficiente = b[1,1] if year==2013
replace coeficiente = b[1,2] if year==2014
replace coeficiente = b[1,3] if year==2015

* To create variables for the t lower for each year
gen t_l= b[3,1] if year==2013
replace t_l = b[3,2] if year==2014
replace t_l = b[3,3] if year==2015

* To create variables for the t upper for each year
gen t_u= b[4,1] if year==2013
replace t_u = b[4,2] if year==2014
replace t_u = b[4,3] if year==2015

* Collapse by year the RD coefficients of OLS and make the graph using AEJ Applied format
preserve
collapse (mean) coeficiente t_l t_u, by(year)
twoway (connected coeficiente year, pstyle(p p3 p3) sort lcolor(navy)) (line t_l year, pstyle(p p3 p3) sort lpattern(dash) lcolor(green)) /*
*/ (line t_u year, pstyle(p p3 p3) sort lpattern(dash) lcolor(green)), /*
*/ title(Average Effect) ytitle("RD Effect of eligibility for OLS") xtitle(Year) xlabel(minmax) yline(0, lcolor(cranberry)) graphregion(color(white)) bgcolor(white) legend(off) name(ols, replace) ylabel(-2(0.5)1.5) xlabel(2013(1)2015)
restore

graph export "${figura}coef_ols.png", as(png) replace

* To combine all percentiles graphs in one including ols 
graph combine ols pct_25 pct_50, rows(1) name(row1, replace) graphregion(color(white))
graph combine pct_75 pct_90, rows(1) name(row2,replace) graphregion(color(white))
graph combine row1 row2, cols(1) graphregion(color(white))

graph export "${figura}figure_7.png", as(png) replace
graph export "${figura}figure_7.eps", replace
graph save "${figura}figure_7.gph", replace


*****************************************************************************************************************************
* TABLE A1 ESTIMATIONS OF MOTIVATIONAL EFFECT ON SABER 11 STANDARDIZED TEST SCORES AND (NON-MODIFIED) SABER 11 TEST SCORES  *
*****************************************************************************************************************************
use "${base}originals\SPP_ready.dta", clear

*DIF IN RD with STANDARDIZED SCORES
*MEAN EFFECT:
reg saber_std eligible_post eligible post sisben sisben_eligible sisben_post sisben_eligible_post  ${controls_std} if abs(sisben)<bw_dif_cersum , ro
outreg2 using "${tabla}/std_score.xls", addtext(Cuantil,"OLS Dif RD")  less(1) keep(eligible_post eligible) nocons replace


*QUANTILE EFFECTS
foreach q in 25 50 75 90 {
qreg saber_std eligible_post eligible post sisben sisben_eligible sisben_post sisben_eligible_post  ${controls_std} if abs(sisben)<bw_dif_cersum , q(`q') vce(r)  
outreg2 using "${tabla}/std_score.xls", addtext(Cuantil,"`q' Dif RD")  less(1) keep(eligible_post eligible) nocons append
}


*** GAP ESTIMATION FOR SHARE OF THE GAP CLOSED ***
*Gap STD OLS:
reg saber_std eligible if post == 0 
outreg2 using "${tabla}/std_score.xls", addtext(Cuantil,"GAP STD")  noaster nocons append

*Gap STD Quantile:
foreach q in 25 50 75 90 {
qreg saber_std eligible if post == 0 , q(`q')
outreg2 using "${tabla}/std_score.xls", addtext(Cuantil,"`q' GAP STD")  noaster nocons append
}


*DIF IN RD with ORIGINAL SCORES (keeping only year 2014 & 2015)
*MEAN EFFECT:
reg puntaje_saber eligible_post eligible post sisben sisben_eligible sisben_post sisben_eligible_post  ${controls_orig} if abs(sisben)<bw_dif_cersum &(year==2014 | year == 2015), ro
outreg2 using "${tabla}/std_score.xls", addtext(Cuantil,"OLS Dif RD")  less(1) keep(eligible_post eligible) nocons append

*QUANTILE EFFECTS
foreach q in 25 50 75 90 {
qreg puntaje_saber eligible_post eligible post sisben sisben_eligible sisben_post sisben_eligible_post  ${controls_orig} if abs(sisben)<bw_dif_cersum &(year==2014 | year == 2015), q(`q') vce(r)  
outreg2 using "${tabla}/std_score.xls", addtext(Cuantil,"`q' Dif RD")  less(1) keep(eligible_post eligible) nocons append
}

*Gap Original scores:
reg puntaje_saber eligible if year == 2014 
outreg2 using "${tabla}/std_score.xls", addtext(Cuantil,"GAP orig")  noaster nocons append

*Gap Original Scores Quantile:
foreach q in 25 50 75 90 {
qreg puntaje_saber eligible if year == 2014 , q(`q')
outreg2 using "${tabla}/std_score.xls", addtext(Cuantil,"`q' GAP orig") noaster nocons append
}
 
***************************************************************************************
* TABLE A2 DIFFERENCE-IN-DISCONTINUITIES ESTIMATIONS WITH POLYNOMIAL OF ORDER 0 AND 2 *
***************************************************************************************
use "${base}originals\SPP_ready.dta", clear

* 0 DEGREE POLYNOMIAL
*MEAN EFFECT:
reg ranking eligible_post eligible post  ${controls} if abs(sisben)<bw_dif_cersum , ro
outreg2 using "${tabla}DIF_RD_POLYN.xls", addtext(Cuantil,"OLS POLYN0", Bandwidth, "`bw_dif_cersum'") ctitle("POLYN0") less(1) keep(eligible_post eligible) nocons replace

*QUANTILE EFFECTS
foreach q in 25 50 75 90 {
qreg ranking eligible_post eligible post  ${controls} if abs(sisben)<bw_dif_cersum , q(`q') vce(r)  
outreg2 using "${tabla}DIF_RD_POLYN.xls", addtext(Cuantil,"`q' POLYN0", Bandwidth, `bw_dif_cersum') ctitle("POLYN0") less(1) keep(eligible_post eligible) nocons append
}

* 2ND DEGREE POLYNOMIAL
*MEAN EFFECT:
reg ranking eligible_post eligible post sisben sisben_eligible sisben_post sisben_eligible_post ///
sisben_2 sisben_eligible_2 sisben_post_2 sisben_eligible_post_2 ///
 ${controls} if abs(sisben)<bw_dif_cersum , ro
outreg2 using "${tabla}DIF_RD_POLYN.xls", addtext(Cuantil,"OLS POLYN2", Bandwidth, "`bw_dif_cersum'") ctitle("POLYN2") less(1) keep(eligible_post eligible) nocons append

*QUANTILE EFFECTS
foreach q in 25 50 75 90 {
qreg ranking eligible_post eligible post sisben sisben_eligible sisben_post sisben_eligible_post ///
sisben_2 sisben_eligible_2 sisben_post_2 sisben_eligible_post_2 ///
  ${controls} if abs(sisben)<bw_dif_cersum , q(`q') vce(r)  
outreg2 using "${tabla}DIF_RD_POLYN.xls", addtext(Cuantil,"`q' POLYN2", Bandwidth, `bw_dif_cersum') ctitle("POLYN2") less(1) keep(eligible_post eligible) nocons append
}
 
 
********************************************************************************
* TABLE A3 PLACEBO DIFFERENCE-IN-DISCONTINUITIES ESTIMATIONS FROM 2013 TO 2014 *
********************************************************************************
use "${base}originals\SPP_ready.dta", clear

qreg ranking eligible_y2014 eligible y2014 sisben sisben_eligible sisben_y2014 sisben_eligible_y2014  ${controls} if abs(sisben)<bw_2013_14_cersum & (year == 2013 | year == 2014), q(90) vce(r)  

*MEAN EFFECT DIF IN RD 2014 - 2013:
reg ranking eligible_y2014 eligible y2014 sisben sisben_eligible sisben_y2014 sisben_eligible_y2014  ${controls} if abs(sisben)<bw_2013_14_cersum & (year == 2013 | year == 2014), ro 
outreg2 using "${tabla}/placebo.xls", addtext(Cuantil,"`q' Dif RD 2014-13", upperbound, bw_2013_14_cersum) ctitle("DIF RD 2013-14") less(1) keep(eligible_y2014 eligible) nocons append

* QUANTILE EFFECT DIF IN RD 2014 - 2013
foreach q in 25 50 75 90 {
qreg ranking eligible_y2014 eligible y2014 sisben sisben_eligible sisben_y2014 sisben_eligible_y2014  ${controls} if abs(sisben)<bw_2013_14_cersum & (year == 2013 | year == 2014), q(`q') vce(r)  
outreg2 using "${tabla}/placebo.xls", addtext(Cuantil,"`q' Dif RD 2014-13", upperbound, bw_2013_14_cersum) ctitle("DIF RD 2013-14") less(1) keep(eligible_y2014 eligible) nocons append
}

*In the excel sheet, we divide by the gaps estimated in table 1A to obtain the gap reduction


**************************************************************************************
* FIGURE A2. DENSITY OF THE SISBÉN INDEX AROUND THE SOCIOECONOMIC ELIGIBILITY CUTOFF *
**************************************************************************************
use "${base}\originals\SPP_ready.dta", clear

keep if year == 2015

rddensity sisben, p(1) 

rddensity sisben if area_sisben_dnp==1, p(1)

rddensity sisben if area_sisben_dnp==2, p(1)

rddensity sisben if area_sisben_dnp==3, p(1)

graph drop _all


twoway (histogram puntaje_sisben_dnp if area_sisben_dnp==1, fcolor(black) bcolor(gray)) ///
(scatteri 0 56.32 0.025 56.32 (9), c(l) m(i) color(red)), ///
text(0.025 57.21 "14 Cities", place(e) size(medium)) ///
text(0.025 0 "P-Val=.3183", place(e) size(medium)) ///
legend(off) xtitle("{it:Sisbén} -  Socioeconomic Index") ylabel(0(.01).025) ///
ytitle("Density") graphregion(style(none) color(gs16)) name("histogram_14cities", replace)  
graph export "${figura}histogram_14cities.png", replace

*rddensity sisben_ if area_sisben_dnp==2, c(0) p(1)

twoway (histogram puntaje_sisben_dnp if area_sisben_dnp==2, fcolor(black) bcolor(gray)) ///
(scatteri 0 57.21 0.025 57.21 (3), c(l) m(i) color(red)), ///
text(0.025 58.32 "Urban", place(e) size(medium)) ///
text(0.025 0 "P-Val=.7747", place(e) size(medium)) ///
legend(off) xtitle("{it:Sisbén} -  Socioeconomic Index")  ylabel(0(.01).025) ///
ytitle("Density") graphregion(style(none) color(gs16)) name("histogram_urban", replace) 
graph export "${figura}histogram_urban.png", replace

*rddensity sisben_ if area_sisben_dnp==3, c(0) p(1)

twoway (histogram puntaje_sisben_dnp if area_sisben_dnp==3, fcolor(black) bcolor(gray)) ///
(scatteri 0 40.75 0.04 40.75 (9), c(l) m(i) color(red)), ///
text(0.04 41.75 "Rural", place(e) size(medium)) ///
text(0.04 0 "P-Val=.1285", place(e) size(medium)) ///
legend(off) xtitle("{it:Sisbén} -  Socioeconomic Index") ylabel(0(.01).035) ///
ytitle("Density") graphregion(style(none) color(gs16)) name("histogram_rural", replace) 
graph export "${figura}histogram_rural.png", replace
*graph export "${tabla}fx_histogram_sisben_rural.eps", replace

graph combine histogram_14cities histogram_urban histogram_rural, ///
	rows(3) graphregion(style(none) color(gs16)) ///
	imargin(medsmall) xcommon ycommon
graph export "${figura}figure_A2.eps", replace
graph export "${figura}figure_A2.png", replace

*********************************************************************************
*   FIGURE A3 DISTRIBUTION OF THE SHARE OF SISBEN ELIGIBLE STUDENTS IN SCHOOLS  *
*********************************************************************************
use "${base}originals\saber_panel.dta"

preserve

keep if year==2015
histogram p_elegible, xtitle("Share of {it:Sisbén} eligible students") ytitle("") fcolor(black) bcolor(gray) graphregion(style(none) color(gs16))
graph export "${figura}figure_A3.png", as(png) replace 
graph export "${figura}figure_A3.eps", replace
graph save "${figura}figure_A3.gph", replace

restore

