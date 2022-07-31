/* This file tests the parallel trend assumption required for DD prediction.
Men between the ages of 18-24 were chosen as the treatment group, and men 
between the ages of 25-28 were selected as the control group.

This file does not contain the codes that make up the final graphics and tables. 
=====================================================================
install packages: 
ssc install estout, replace 
ssc install coefplot, replace
input: `final.dta`' from data.R
output: 
*******************************************************************************/
global root "...\master_thesis" /*Project folder root */
global sysdate=c(current_date)
cd $root
/* Keep log file  */
cap log close
local date =  string(date(c(current_date), "DMY"), "%tdCCYYNNDD") + subinstr("`c(current_time)'", ":", "", .)
log using "output/log_estim_`date'.txt",replace text name(estimations)
********************************************************************************
*
/* Read the data file */
use "data/clean/final.dta", clear
*
/* Variable lists */
*
keep if inrange(age,18,28)
drop if female==1 
keep if inrange(year,2015,2019) 
*
/**************************************************************************
* Check parallel trends  *************************************************
**************************************************************************/
/* Linear Probability Model estimations */
/*Formal Wage Worker*/
qui eststo for_pr: reg forww ib(2017).year##target i.year, robust cluster(region)
coefplot,  drop(_cons 1.target 0.target *.year) yline(0) vertical baselevels ///
	xline (3, lpattern(dash) lcolor(red)) levels(95) msymbol(o) ///
	ciopts(lwidth(*1 *2 )) ///
	legend(title ("Güven Derecesi", size (vsmall)) order(1 "95%") row (1) size (vsmall)) ///
	legend(symys(*.5) symxs(*.5) size(3) region(c(none)) bm(tiny)) ///
	coeflabels(2015.*="2015" 2016.*="2016" 2017.*="2017" 2018.*="2018" 2019.*="2019") ///
	title("") ytitle("2017 Yılına Göre Etki")
*****
/*Informal Wage Worker*/
qui eststo inf_pr: reg infww ib(2017).year##target i.year, robust cluster(region)
coefplot,  drop(_cons 1.target 0.target *.year) yline(0) vertical baselevels ///
	xline (3, lpattern(dash) lcolor(red)) levels(95) msymbol(o) ///
	ciopts(lwidth(*1 *2 )) ///
	legend(title ("Güven Derecesi", size (vsmall)) order(1 "95%") row (1) size (vsmall)) ///
	legend(symys(*.5) symxs(*.5) size(3) region(c(none)) bm(tiny)) ///
	coeflabels(2015.*="2015" 2016.*="2016" 2017.*="2017" 2018.*="2018" 2019.*="2019") ///
	title("") ytitle("2017 Yılına Göre Etki")
*****
/*Real Wage in Formal Sector*/
qui eststo for_wage_pr: reg lrhwage ib(2017).year##target i.year if(forww==1 & x==1), robust cluster(region)
coefplot,  drop(_cons 1.target 0.target *.year) yline(0) vertical baselevels ///
	xline (3, lpattern(dash) lcolor(red)) levels(95) msymbol(o) ///
	ciopts(lwidth(*1 *2 )) ///
	legend(title ("Güven Derecesi", size (vsmall)) order(1 "95%") row (1) size (vsmall)) ///
	legend(symys(*.5) symxs(*.5) size(3) region(c(none)) bm(tiny)) ///
	coeflabels(2015.*="2015" 2016.*="2016" 2017.*="2017" 2018.*="2018" 2019.*="2019") ///
	title("") ytitle("2017 Yılına Göre Etki")
*****
/*Real Wage in Informal Sector*/
qui eststo inf_wage_pr: reg lrhwage ib(2017).year##target i.year if(infww==1 & x==1), robust cluster(region)
coefplot,  drop(_cons 1.target 0.target *.year) yline(0) vertical baselevels ///
	xline (3, lpattern(dash) lcolor(red)) levels(95) msymbol(o) ///
	ciopts(lwidth(*1 *2 )) ///
	legend(title ("Güven Derecesi", size (vsmall)) order(1 "95%") row (1) size (vsmall)) ///
	legend(symys(*.5) symxs(*.5) size(3) region(c(none)) bm(tiny)) ///
	coeflabels(2015.*="2015" 2016.*="2016" 2017.*="2017" 2018.*="2018" 2019.*="2019") ///
	title("") ytitle("2017 Yılına Göre Etki")
********************************************************************************
/*Parallel Trend Assumption Test Results (Table) */
esttab for_pr for_wage_pr inf_pr inf_wage_pr, keep(*.year#1.target) ///
 title("Paralel Trend Varsayımı Testi") unstack ///
 mtitle("Kayıtlı İstihdam", "Kayıtlı Sektör Reel Ücret", "Kayıt Dışı İstihdam", /// 
 "Kayıt Dışı Sektör Reel Ücret")
 b(a2) se(3) star(* 0.1 ** 0.05 *** 0.01) 
********************************************************************************
log close
