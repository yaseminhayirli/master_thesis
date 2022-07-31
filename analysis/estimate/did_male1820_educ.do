/* This file estimates the impact of the program for men aged 18-20 based on 
their education level.
This file does not contain the codes that make up the final tables.
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
log using "output/estall_`date'.txt",replace text name(estimations)
********************************************************************************
*
/* Read the data file */
use "data/clean/final.dta", clear 
*
drop if female==1 
keep if inrange(year,2016,2019) 
keep if (male1820==1 | male2528==1) 
********************************************************************************
/* Variable lists */ 
*****
gen year17 = (year==2017)
gen year18 = (year==2018)
gen year19 = (year==2019) 
*
global int target##post18 /* Treatment group and period interaction variable */
global xx0 age i.educ1 i.married
global xx1 c.age##i.educ1 c.age##i.married i.educ1##i.married 
global xx2 c.age##i.married 
********************************************************************************
* The Program Effect by Education Levels (2016-2019) * 
********************************************************************************
* Formal Wage Worker 
qui eststo ed0a1: regress forww i.year i.region year#i.region $int $xx2 if(educ1==0), robust cluster(region)
qui eststo ed1a1: regress forww i.year i.region year#i.region $int $xx2 if(educ1==1), robust cluster(region)
qui eststo ed2a1: regress forww i.year i.region year#i.region $int $xx2 if(educ1==2), robust cluster(region)
qui eststo ed3a1: regress forww i.year i.region year#i.region $int $xx2 if(educ1==3), robust cluster(region)
*
esttab ed0a1 ed1a1 ed2a1 ed3a1, b(a2) keep(1.target#1.post18) title("Programın Eğitim Seviyelerine Göre Etkisi- Kayıtlı Ücretli Çalışan") se(3) star(* 0.1 ** 0.05 *** 0.01)

esttab ed0a1 ed1a1 ed2a1 ed3a1, b(a2) keep(1.target 1.target#1.post18 age 1.married 1.married#c.age) title("Programın Eğitim Seviyelerine Göre Etkisi (Bütün Tahminler)- Kayıtlı Ücretli Çalışan") se(3) star(* 0.1 ** 0.05 *** 0.01)
*****
* Real Wage in Formal Sector 
qui eststo ed0a2: regress lrhwage i.year i.region year#i.region $int $xx2 if((educ1==0) & (forww==1 & x==1)), robust cluster(region)
qui eststo ed1a2: regress lrhwage i.year i.region year#i.region $int $xx2 if((educ1==1) & (forww==1 & x==1)), robust cluster(region)
qui eststo ed2a2: regress lrhwage i.year i.region year#i.region $int $xx2 if((educ1==2) &  (forww==1 & x==1)), robust cluster(region)
qui eststo ed3a2: regress lrhwage i.year i.region year#i.region $int $xx2 if((educ1==3) &  (forww==1 & x==1)), robust cluster(region)
*
esttab ed0a2 ed1a2 ed2a2 ed3a2, b(a2) keep(1.target#1.post18) title("Programın Eğitim Seviyelerine Göre Etkisi- Kayıtlı Sektör Reel Ücret") se(3) star(* 0.1 ** 0.05 *** 0.01)

esttab ed0a2 ed1a2 ed2a2 ed3a2, b(a2) keep(1.target 1.target#1.post18 age 1.married 1.married#c.age) title("Programın Eğitim Seviyelerine Göre Etkisi (Bütün Tahminler)- Kayıtlı Sektör Reel Ücret") se(3) star(* 0.1 ** 0.05 *** 0.01)
*****
* Informal Wage Worker 
qui eststo ed0b1: regress infww i.year i.region year#i.region $int $xx2 if(educ1==0), robust cluster(region)
qui eststo ed1b1: regress infww i.year i.region year#i.region $int $xx2 if(educ1==1), robust cluster(region)
qui eststo ed2b1: regress infww i.year i.region year#i.region $int $xx2 if(educ1==2), robust cluster(region)
qui eststo ed3b1: regress infww i.year i.region year#i.region $int $xx2 if(educ1==3), robust cluster(region)
*
esttab ed0b1 ed1b1 ed2b1 ed3b1, b(a2) keep(1.target#1.post18) title("Programın Eğitim Seviyelerine Göre Etkisi- Kayıt Dışı Ücretli Çalışan") se(3) star(* 0.1 ** 0.05 *** 0.01)

esttab ed0b1 ed1b1 ed2b1 ed3b1, b(a2) keep(1.target 1.target#1.post18 age 1.married 1.married#c.age) title("Programın Eğitim Seviyelerine Göre Etkisi (Bütün Tahminler)- Kayıt Dışı Ücretli Çalışan") se(3) star(* 0.1 ** 0.05 *** 0.01)
*****
* Real Wage in Informal Sector 
qui eststo ed0b2: regress lrhwage i.year i.region year#i.region $int $xx2 if((educ1==0) & (infww==1 & x==1)), robust cluster(region)
qui eststo ed1b2: regress lrhwage i.year i.region year#i.region $int $xx2 if((educ1==1) & (infww==1 & x==1)), robust cluster(region)
qui eststo ed2b2: regress lrhwage i.year i.region year#i.region $int $xx2 if((educ1==2) & (infww==1 & x==1)), robust cluster(region)
qui eststo ed3b2: regress lrhwage i.year i.region year#i.region $int $xx2 if((educ1==3) & (infww==1 & x==1)), robust cluster(region)
*
esttab ed0b2 ed1b2 ed2b2 ed3b2, b(a2) keep(1.target#1.post18) title("Programın Eğitim Seviyelerine Göre Etkisi- Kayıt Dışı Sektör Reel Ücret") se(3) star(* 0.1 ** 0.05 *** 0.01) 

esttab ed0b2 ed1b2 ed2b2 ed3b2, b(a2) keep(1.target 1.target#1.post18 age 1.married 1.married#c.age) title("Programın Eğitim Seviyelerine Göre Etkisi (Bütün Tahminler)- Kayıt Dışı Sektör Reel Ücret") se(3) star(* 0.1 ** 0.05 *** 0.01)    

log close 









