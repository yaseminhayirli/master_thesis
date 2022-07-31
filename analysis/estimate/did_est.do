/* This file do the estimations for difference-in-differences models.
This file calculate the effects of 2018 Additional Employment Program 
for male aged 18-24.
/* This file does not output the final version of the tables. */
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
keep if inrange(age,18,28) 
drop if female==1 
keep if inrange(year,2016,2019) 
********************************************************************************
/* Variable lists */ 
*****
gen year17 = (year==2017)
gen year18 = (year==2018)
gen year19 = (year==2019) 
global int18 target##year18 
global int19 target##year19 
*
global int target##post18 /* Treatment group and period interaction variable */
global placebo target##year17 
global xx0 age i.educ1 i.married
global xx1 c.age##i.educ1 c.age##i.married i.educ1##i.married 
global xx2 c.age##i.married 

gen treatpost = target*post18
********************************************************************************
* The Effect of Subsidy Program on Labor Outcomes of Young Male ****************
* 2018-2019 * 
* Formal Wage Worker * 
qui eststo a_1: regress forww $int $xx1, robust cluster(region)
qui eststo a_2: regress forww i.year i.region $int $xx1, robust cluster(region)
qui eststo a_3: regress forww i.year i.region year#i.region $int $xx1, robust cluster(region)
qui eststo a_4: regress lrhwage $int $xx1 if(forww==1 & x==1), robust cluster(region)
qui eststo a_5: regress lrhwage i.year i.region $int $xx1 if(forww==1 & x==1), robust cluster(region)
qui eststo a_6: regress lrhwage i.year i.region year#i.region $int $xx1 if(forww==1 & x==1), robust cluster(region)

*****
esttab a_1 a_2 a_3 a_4 a_5 a_6, b(a2) keep(1.target#1.post18) title("Programın Genç Erkeklerin Kayıtlı İstihdamına Etkisi") unstack se(3) star(* 0.1 ** 0.05 *** 0.01)

esttab a_1 a_2 a_3 a_4 a_5 a_6, b(a2) keep(1.target 1.post18 1.target#1.post18 1.educ1 2.educ1 3.educ1 age 1.married 1.educ1#c.age 2.educ1#c.age 3.educ1#c.age 1.married 1.married#c.age 1.educ1#1.married 2.educ1#1.married 3.educ1#1.married)title("Programın Genç Erkeklerin Kayıtlı İstihdamına Etkisi (Bütün Tahminler)") se(3) star(* 0.1 ** 0.05 *** 0.01)
********************************************************************************
* Informal Wage Worker *
qui eststo b_1: regress infww $int $xx1, robust cluster(region)
qui eststo b_2: regress infww i.year i.region $int $xx1, robust cluster(region)
qui eststo b_3: regress infww i.year i.region year#i.region $int $xx1, robust cluster(region)
qui eststo b_4: regress lrhwage $int $xx1 if(infww==1 & x==1), robust cluster(region)
qui eststo b_5: regress lrhwage i.year i.region $int $xx1 if(infww==1 & x==1), robust cluster(region)
qui eststo b_6: regress lrhwage i.year i.region year#i.region $int $xx1 if(infww==1 & x==1), robust cluster(region)
*
esttab b_1 b_2 b_3 b_4 b_5 b_6, b(a2) keep(1.target#1.post18) title("Programın Genç Erkeklerin Kayıtsız İstihdamına Etkisi") unstack se(3) star(* 0.1 ** 0.05 *** 0.01)

esttab b_1 b_2 b_3 b_4 b_5 b_6, b(a2) keep(1.target 1.post18 1.target#1.post18 1.educ1 2.educ1 3.educ1 age 1.married 1.educ1#c.age 2.educ1#c.age 3.educ1#c.age 1.married 1.married#c.age 1.educ1#1.married 2.educ1#1.married 3.educ1#1.married)title("Programın Genç Erkeklerin Kayıt Dışı İstihdamına Etkisi (Bütün Tahminler)") se(3) star(* 0.1 ** 0.05 *** 0.01)
********************************************************************************
* Year-Spesific Policy Effect **************************************************
qui eststo c_1: regress forww i.year i.region year#i.region $int18 $int19 $xx1, robust cluster(region)
qui eststo c_2: regress lrhwage i.year i.region year#i.region $int18 $int19 $xx1 if(forww==1 & x==1), robust cluster(region)
qui eststo c_3: regress infww i.year i.region year#i.region $int18 $int19 $xx1, robust cluster(region)
qui eststo c_4: regress lrhwage i.year i.region year#i.region $int18 $int19 $xx1 if(infww==1 & x==1), robust cluster(region)
*
esttab c_1 c_2 c_3 c_4, b(a2) keep(1.target#1.year18 1.target#1.year19) title("Programın Yıllara Göre Etkisi") unstack mtitle("Kayıtlı İst" "Kayıtlı Reel Ücret" "Kayıt Dışı İst" "Kayıt Dışı Reel Ücret") se(3) star(* 0.1 ** 0.05 *** 0.01)

esttab c_1 c_2 c_3 c_4, b(a2) keep(1.target 1.target#1.year18 1.target#1.year19 1.educ1 2.educ1 3.educ1 age 1.married 1.educ1#c.age 2.educ1#c.age 3.educ1#c.age 1.married 1.married#c.age 1.educ1#1.married 2.educ1#1.married 3.educ1#1.married)title("Programın Yıllara Göre Etkisi (Bütün Tahminler)") unstack mtitle("Kayıtlı İst" "Kayıtlı Reel Ücret" "Kayıt Dışı İst" "Kayıt Dışı Reel Ücret") se(3) star(* 0.1 ** 0.05 *** 0.01)
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
********************************************************************************
* The Policy Effect by Age Grups
* Treatment Group = 18-20, Control Group = 25-28 
* 2016-2019 * 
qui eststo d_1: regress forww i.year i.region year#i.region $int $xx1 if(male1820==1 | male2528==1), robust cluster(region)
qui eststo d_2: regress lrhwage i.year i.region year#i.region $int $xx1 if((male1820==1 | male2528==1) & (forww==1 & x==1)), robust cluster(region)
qui eststo d_3: regress infww i.year i.region year#i.region $int $xx1 if(male1820==1 | male2528==1), robust cluster(region)
qui eststo d_4: regress lrhwage i.year i.region year#i.region $int $xx1 if((male1820==1 | male2528==1) & (infww==1 & x==1)), robust cluster(region)
*
esttab d_1 d_2 d_3 d_4, b(a2) keep(1.target#1.post18) title("Programın Yaş Gruplarına Göre Etkisi (18-20)") unstack mtitle("Kayıtlı İstihdam" "Kayıtlı Sektör Reel Ücret" "Kayıt Dışı İstihdam" "Kayıt Dışı Sektör Reel Ücret") se(3) star(* 0.1 ** 0.05 *** 0.01) 

esttab d_1 d_2 d_3 d_4, b(a2) keep(1.target 1.post18 1.target#1.post18 1.educ1 2.educ1 3.educ1 age 1.married 1.educ1#c.age 2.educ1#c.age 3.educ1#c.age 1.married 1.married#c.age 1.educ1#1.married 2.educ1#1.married 3.educ1#1.married) title("Programın Yaş Gruplarına Göre Etkisi (Bütün Tahminler)- (18-20)") unstack mtitle("Kayıtlı İstihdam" "Kayıtlı Sektör Reel Ücret" "Kayıt Dışı İstihdam" "Kayıt Dışı Sektör Reel Ücret") se(3) star(* 0.1 ** 0.05 *** 0.01)

* Treatment Group = 21-24, Control Group = 25-28 
* 2016-2019 * 
qui eststo e_1: regress forww i.year i.region year#i.region $int $xx1 if(male2124==1 | male2528==1), robust cluster(region)
qui eststo e_2: regress lrhwage i.year i.region year#i.region $int $xx1 if((male2124==1 | male2528==1) & (forww==1 & x==1)), robust cluster(region)
qui eststo e_3: regress infww i.year i.region year#i.region $int $xx1 if(male2124==1 | male2528==1), robust cluster(region)
qui eststo e_4: regress lrhwage i.year i.region year#i.region $int $xx1 if((male2124==1 | male2528==1) & (infww==1 & x==1)), robust cluster(region)
*
esttab e_1 e_2 e_3 e_4, b(a2) keep(1.target#1.post18) title("Programın Yaş Gruplarına Göre Etkisi (21-24)") unstack mtitle("Kayıtlı İstihdam" "Kayıtlı Sektör Reel Ücret" "Kayıt Dışı İstihdam" "Kayıt Dışı Sektör Reel Ücret") se(3) star(* 0.1 ** 0.05 *** 0.01)

esttab e_1 e_2 e_3 e_4, b(a2) keep(1.target 1.post18 1.target#1.post18 1.educ1 2.educ1 3.educ1 age 1.married 1.educ1#c.age 2.educ1#c.age 3.educ1#c.age 1.married 1.married#c.age 1.educ1#1.married 2.educ1#1.married 3.educ1#1.married) title("Programın Yaş Gruplarına Göre Etkisi (Bütün Tahminler)- (18-20)") unstack mtitle("Kayıtlı İstihdam" "Kayıtlı Sektör Reel Ücret" "Kayıt Dışı İstihdam" "Kayıt Dışı Sektör Reel Ücret") se(3) star(* 0.1 ** 0.05 *** 0.01) 
********************************************************************************
* ROBUSTNESS CHECKS * 
********************************************************************************
********************************************************************************
* Placebo Test
* 2016-2017 
* Formal Wage Worker
qui eststo p1: regress forww i.year i.region year#i.region $placebo $xx1 if(year==2016 | year==2017), robust cluster(region)
* Real Wage in Formal Sector
qui eststo p2: regress lrhwage i.year i.region year#i.region $placebo $xx1 if((year==2016 | year==2017) & (forww==1 & x==1)), robust cluster(region)
* Informal Wage Worker 
qui eststo p3: regress infww i.year i.region year#i.region $placebo $xx1 if(year==2016 | year==2017), robust cluster(region)
* Real Wage in Informal Sector  
qui eststo p4: regress lrhwage i.year i.region year#i.region $placebo $xx1 if((year==2016 | year==2017) & (infww==1 & x==1)), robust cluster(region)
*
esttab p1 p2 p3 p4, b(a2) keep(1.target#1.year17) title("Plasebo Testi") unstack mtitle("Kayıtlı İstihdam" "Kayıtlı Sektör Reel Ücret" "Kayıt Dışı İstihdam" "Kayıt Dışı Sektör Reel Ücret") se(3) star(* 0.1 ** 0.05 *** 0.01)

esttab p1 p2 p3 p4, b(a2) keep(1.target 1.year17 1.target#1.year17 1.educ1 2.educ1 3.educ1 age 1.married 1.educ1#c.age 2.educ1#c.age 3.educ1#c.age 1.married 1.married#c.age 1.educ1#1.married 2.educ1#1.married 3.educ1#1.married) title("Plasebo Testi (Bütün Tahminler)") unstack mtitle("Kayıtlı İstihdam" "Kayıtlı Sektör Reel Ücret" "Kayıt Dışı İstihdam" "Kayıt Dışı Sektör Reel Ücret") se(3) star(* 0.1 ** 0.05 *** 0.01)

********************************************************************************

* 2016-2018 *  
* Formal Wage Worker
qui eststo x1: regress forww i.year i.region year#i.region $int $xx1 if(year==2016 | year==2018), robust cluster(region)
* Real Wage in Formal Sector
qui eststo x2: regress lrhwage i.year i.region year#i.region $int $xx1 if((year==2016 | year==2018) & (forww==1 & x==1)), robust cluster(region)
* Informal Wage Worker 
qui eststo x3: regress infww i.year i.region year#i.region $int $xx1 if(year==2016 | year==2018), robust cluster(region)
* Real Wage in Informal Sector  
qui eststo x4: regress lrhwage i.year i.region year#i.region $int $xx1 if((year==2016 | year==2018) & (infww==1 & x==1)), robust cluster(region)
*
esttab x1 x2 x3 x4, b(a2) keep(1.target#1.post18) title("Programın İşgücü Çıktılarına Etkisi (2016-2018)") unstack mtitle("Kayıtlı İstihdam" "Kayıtlı Reel Ücret" "Kayıtsız İstihdam" "Kayıtsız Reel Ücret") se(3) star(* 0.1 ** 0.05 *** 0.01)

esttab x1 x2 x3 x4, b(a2) keep(1.target 1.post18 1.target#1.post18 1.educ1 2.educ1 3.educ1 age 1.married 1.educ1#c.age 2.educ1#c.age 3.educ1#c.age 1.married 1.married#c.age 1.educ1#1.married 2.educ1#1.married 3.educ1#1.married) title("Programın İşgücü Çıktılarına Etkisi (Bütün Tahminler)- (2016-2018)") unstack mtitle("Kayıtlı İstihdam" "Kayıtlı Reel Ücret" "Kayıtsız İstihdam" "Kayıtsız Reel Ücret") se(3) star(* 0.1 ** 0.05 *** 0.01)


* 2017-2018 *  
* Formal Wage Worker
qui eststo y1: regress forww i.year i.region year#i.region $int $xx1 if(year==2017 | year==2018), robust cluster(region)
* Real Wage in Formal Sector
qui eststo y2: regress lrhwage i.year i.region year#i.region $int $xx1 if((year==2017 | year==2018) & (forww==1 & x==1)), robust cluster(region)
* Informal Wage Worker 
qui eststo y3: regress infww i.year i.region year#i.region $int $xx1 if(year==2017 | year==2018), robust cluster(region)
* Real Wage in Informal Sector  
qui eststo y4: regress lrhwage i.year i.region year#i.region $int $xx1 if((year==2017 | year==2018) & (infww==1 & x==1)), robust cluster(region)
*
esttab y1 y2 y3 y4, b(a2) keep(1.target#1.post18) title("Programın İşgücü Çıktılarına Etkisi (2017-2018)") unstack mtitle("Kayıtlı İstihdam" "Kayıtlı Reel Ücret" "Kayıtsız İstihdam" "Kayıtsız Reel Ücret") se(3) star(* 0.1 ** 0.05 *** 0.01)

esttab y1 y2 y3 y4, b(a2) keep(1.target 1.post18 1.target#1.post18 1.educ1 2.educ1 3.educ1 age 1.married 1.educ1#c.age 2.educ1#c.age 3.educ1#c.age 1.married 1.married#c.age 1.educ1#1.married 2.educ1#1.married 3.educ1#1.married) title("Programın İşgücü Çıktılarına Etkisi (Bütün Tahminler)- (2017-2018)") unstack mtitle("Kayıtlı İstihdam" "Kayıtlı Reel Ücret" "Kayıtsız İstihdam" "Kayıtsız Reel Ücret") se(3) star(* 0.1 ** 0.05 *** 0.01)


log close 









