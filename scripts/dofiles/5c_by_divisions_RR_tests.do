/*
Replication files  
*/

clear all
set matsize 11000

*Paths
*Working directory
cd "~/Dropbox/Research/cbsa_discrimination/github/Racial_Discrimination_US_Rental_Market/"

global path_tables  "views"
*global path_tables  "~/Dropbox/Apps/ShareLaTeX/Welfare Impact of Housing Discrimination/tables/cbsa_discrimination/prelim"


*Load Data
use "stores/matchedinquiries.dta"



************************************************************************************************
* Minority
************************************************************************************************

loc geog = 9

matrix define M=J(`geog',3,.)
matrix colnames M = minority_lci minority_coef minority_uci

forvalues i = 1/`geog'{
  sum choice if White==1 & div`i'==1
  loc mean`i' `r(mean)'
}



reghdfe choice  Minority_div*  , absorb(gender education_level inquiry_order  Address)  cl(CBSA_downtown) level(90) nocons

nlcom (RR_Minority_div1: _b[Minority_div1]/`mean1') ///
      (RR_Minority_div2: _b[Minority_div2]/`mean2') ///
      (RR_Minority_div3: _b[Minority_div3]/`mean3') ///
      (RR_Minority_div4: _b[Minority_div4]/`mean4') ///
      (RR_Minority_div5: _b[Minority_div5]/`mean5') ///
      (RR_Minority_div6: _b[Minority_div6]/`mean6') ///
      (RR_Minority_div7: _b[Minority_div7]/`mean7') ///
      (RR_Minority_div8: _b[Minority_div8]/`mean8') ///
      (RR_Minority_div9: _b[Minority_div9]/`mean9') ///
     , post


*1
test  (RR_Minority_div1=RR_Minority_div2) ///
      (RR_Minority_div1=RR_Minority_div3) ///
      (RR_Minority_div1=RR_Minority_div4) ///
      (RR_Minority_div1=RR_Minority_div5) ///
      (RR_Minority_div1=RR_Minority_div6) ///
      (RR_Minority_div1=RR_Minority_div7) ///
      (RR_Minority_div1=RR_Minority_div8) ///
      (RR_Minority_div1=RR_Minority_div9) ///
      , mtest(sidak)
mat T1_1=r(mtest)
mat T2_1=J(1,4,.)
mat T2_1[1,1]= `r(chi2)'
mat T2_1[1,2]= `r(df)'
mat T2_1[1,3]= `r(p)'
mat T2_1[1,4]= `r(p)'           
*2
test  (RR_Minority_div2=RR_Minority_div1) ///
      (RR_Minority_div2=RR_Minority_div3) ///
      (RR_Minority_div2=RR_Minority_div4) ///
      (RR_Minority_div2=RR_Minority_div5) ///
      (RR_Minority_div2=RR_Minority_div6) ///
      (RR_Minority_div2=RR_Minority_div7) ///
      (RR_Minority_div2=RR_Minority_div8) ///
      (RR_Minority_div2=RR_Minority_div9) ///
      , mtest(sidak)
mat T1_2=r(mtest)
mat T2_2=J(1,4,.)
mat T2_2[1,1]= `r(chi2)'
mat T2_2[1,2]= `r(df)'
mat T2_2[1,3]= `r(p)'
mat T2_2[1,4]= `r(p)'     

*3
test  (RR_Minority_div3=RR_Minority_div1) ///
      (RR_Minority_div3=RR_Minority_div2) ///
      (RR_Minority_div3=RR_Minority_div4) ///
      (RR_Minority_div3=RR_Minority_div5) ///
      (RR_Minority_div3=RR_Minority_div6) ///
      (RR_Minority_div3=RR_Minority_div7) ///
      (RR_Minority_div3=RR_Minority_div8) ///
      (RR_Minority_div3=RR_Minority_div9) ///
      , mtest(sidak)
mat T1_3=r(mtest)
mat T2_3=J(1,4,.)
mat T2_3[1,1]= `r(chi2)'
mat T2_3[1,2]= `r(df)'
mat T2_3[1,3]= `r(p)'
mat T2_3[1,4]= `r(p)'     

*4
test  (RR_Minority_div4=RR_Minority_div1) ///
      (RR_Minority_div4=RR_Minority_div2) ///
      (RR_Minority_div4=RR_Minority_div3) ///
      (RR_Minority_div4=RR_Minority_div5) ///
      (RR_Minority_div4=RR_Minority_div6) ///
      (RR_Minority_div4=RR_Minority_div7) ///
      (RR_Minority_div4=RR_Minority_div8) ///
      (RR_Minority_div4=RR_Minority_div9) ///
      , mtest(sidak)
mat T1_4=r(mtest) 
mat T2_4=J(1,4,.)
mat T2_4[1,1]= `r(chi2)'
mat T2_4[1,2]= `r(df)'
mat T2_4[1,3]= `r(p)'
mat T2_4[1,4]= `r(p)'     

test  (RR_Minority_div5=RR_Minority_div1) ///
      (RR_Minority_div5=RR_Minority_div2) ///
      (RR_Minority_div5=RR_Minority_div3) ///
      (RR_Minority_div5=RR_Minority_div4) ///
      (RR_Minority_div5=RR_Minority_div6) ///
      (RR_Minority_div5=RR_Minority_div7) ///
      (RR_Minority_div5=RR_Minority_div8) ///
      (RR_Minority_div5=RR_Minority_div9) ///
      , mtest(sidak)
mat T1_5=r(mtest)
mat T2_5=J(1,4,.)
mat T2_5[1,1]= `r(chi2)'
mat T2_5[1,2]= `r(df)'
mat T2_5[1,3]= `r(p)'
mat T2_5[1,4]= `r(p)'

test  (RR_Minority_div6=RR_Minority_div1) ///
      (RR_Minority_div6=RR_Minority_div2) ///
      (RR_Minority_div6=RR_Minority_div3) ///
      (RR_Minority_div6=RR_Minority_div4) ///
      (RR_Minority_div6=RR_Minority_div5) ///
      (RR_Minority_div6=RR_Minority_div7) ///
      (RR_Minority_div6=RR_Minority_div8) ///
      (RR_Minority_div6=RR_Minority_div9) ///
      , mtest(sidak)
mat T1_6=r(mtest)
mat T2_6=J(1,4,.)
mat T2_6[1,1]= `r(chi2)'
mat T2_6[1,2]= `r(df)'
mat T2_6[1,3]= `r(p)'
mat T2_6[1,4]= `r(p)'

test  (RR_Minority_div7=RR_Minority_div1) ///
      (RR_Minority_div7=RR_Minority_div2) ///
      (RR_Minority_div7=RR_Minority_div3) ///
      (RR_Minority_div7=RR_Minority_div4) ///
      (RR_Minority_div7=RR_Minority_div5) ///
      (RR_Minority_div7=RR_Minority_div6) ///
      (RR_Minority_div7=RR_Minority_div8) ///
      (RR_Minority_div7=RR_Minority_div9) ///
      , mtest(sidak)
mat T1_7=r(mtest)
mat T2_7=J(1,4,.)
mat T2_7[1,1]= `r(chi2)'
mat T2_7[1,2]= `r(df)'
mat T2_7[1,3]= `r(p)'
mat T2_7[1,4]= `r(p)'

test  (RR_Minority_div8=RR_Minority_div1) ///
      (RR_Minority_div8=RR_Minority_div2) ///
      (RR_Minority_div8=RR_Minority_div3) ///
      (RR_Minority_div8=RR_Minority_div4) ///
      (RR_Minority_div8=RR_Minority_div5) ///
      (RR_Minority_div8=RR_Minority_div6) ///
      (RR_Minority_div8=RR_Minority_div7) ///
      (RR_Minority_div8=RR_Minority_div9) ///
      , mtest(sidak)
mat T1_8=r(mtest)
mat T2_8=J(1,4,.)
mat T2_8[1,1]= `r(chi2)'
mat T2_8[1,2]= `r(df)'
mat T2_8[1,3]= `r(p)'
mat T2_8[1,4]= `r(p)'

test  (RR_Minority_div9=RR_Minority_div1) ///
      (RR_Minority_div9=RR_Minority_div2) ///
      (RR_Minority_div9=RR_Minority_div3) ///
      (RR_Minority_div9=RR_Minority_div4) ///
      (RR_Minority_div9=RR_Minority_div5) ///
      (RR_Minority_div9=RR_Minority_div6) ///
      (RR_Minority_div9=RR_Minority_div7) ///
      (RR_Minority_div9=RR_Minority_div8) ///
      , mtest(sidak)
mat T1_9=r(mtest)
mat T2_9=J(1,4,.)
mat T2_9[1,1]= `r(chi2)'
mat T2_9[1,2]= `r(df)'
mat T2_9[1,3]= `r(p)'
mat T2_9[1,4]= `r(p)'

mat T1=T1_1\T1_2\T1_3\T1_4\T1_5\T1_6\T1_7\T1_8\T1_9
mat T2=T2_1\T2_2\T2_3\T2_4\T2_5\T2_6\T2_7\T2_8\T2_9

mat def T=T1\T2

forvalues i = 1/`geog'{ 
  matrix M[`i',1] =  _b[RR_Minority_div`i'] - invttail(e(N),0.05)*_se[RR_Minority_div`i']
  matrix M[`i',2] =  _b[RR_Minority_div`i']
  matrix M[`i',3] =  _b[RR_Minority_div`i'] + invttail(e(N),0.05)*_se[RR_Minority_div`i']
}


************************************************************************************************
* Af American and Hispanic/Latinx
************************************************************************************************



reghdfe choice   AfAm_div*   Hispanic_div* , absorb(gender education_level inquiry_order  Address)  cl(CBSA_downtown) level(90) nocons

    nlcom (RR_AfAm_div1: _b[AfAm_div1]/`mean1') ///
          (RR_AfAm_div2: _b[AfAm_div2]/`mean2') ///
          (RR_AfAm_div3: _b[AfAm_div3]/`mean3') ///
          (RR_AfAm_div4: _b[AfAm_div4]/`mean4') ///
          (RR_AfAm_div5: _b[AfAm_div5]/`mean5') ///
          (RR_AfAm_div6: _b[AfAm_div6]/`mean6') ///
          (RR_AfAm_div7: _b[AfAm_div7]/`mean7') ///
          (RR_AfAm_div8: _b[AfAm_div8]/`mean8') ///
          (RR_AfAm_div9: _b[AfAm_div9]/`mean9') ///
          (RR_Hispanic_div1: _b[Hispanic_div1]/`mean1') ///
          (RR_Hispanic_div2: _b[Hispanic_div2]/`mean2') ///
          (RR_Hispanic_div3: _b[Hispanic_div3]/`mean3') ///
          (RR_Hispanic_div4: _b[Hispanic_div4]/`mean4') ///
          (RR_Hispanic_div5: _b[Hispanic_div5]/`mean5') ///
          (RR_Hispanic_div6: _b[Hispanic_div6]/`mean6') ///
          (RR_Hispanic_div7: _b[Hispanic_div7]/`mean7') ///
          (RR_Hispanic_div8: _b[Hispanic_div8]/`mean8') ///
          (RR_Hispanic_div9: _b[Hispanic_div9]/`mean9') ///
     , post



*1
test  (RR_AfAm_div1=RR_AfAm_div2) ///
      (RR_AfAm_div1=RR_AfAm_div3) ///
      (RR_AfAm_div1=RR_AfAm_div4) ///
      (RR_AfAm_div1=RR_AfAm_div5) ///
      (RR_AfAm_div1=RR_AfAm_div6) ///
      (RR_AfAm_div1=RR_AfAm_div7) ///
      (RR_AfAm_div1=RR_AfAm_div8) ///
      (RR_AfAm_div1=RR_AfAm_div9) ///
      , mtest(sidak)
mat T1_1=r(mtest)
mat T2_1=J(1,4,.)
mat T2_1[1,1]= `r(chi2)'
mat T2_1[1,2]= `r(df)'
mat T2_1[1,3]= `r(p)'
mat T2_1[1,4]= `r(p)'           
*2
test  (RR_AfAm_div2=RR_AfAm_div1) ///
      (RR_AfAm_div2=RR_AfAm_div3) ///
      (RR_AfAm_div2=RR_AfAm_div4) ///
      (RR_AfAm_div2=RR_AfAm_div5) ///
      (RR_AfAm_div2=RR_AfAm_div6) ///
      (RR_AfAm_div2=RR_AfAm_div7) ///
      (RR_AfAm_div2=RR_AfAm_div8) ///
      (RR_AfAm_div2=RR_AfAm_div9) ///
      , mtest(sidak)
mat T1_2=r(mtest)
mat T2_2=J(1,4,.)
mat T2_2[1,1]= `r(chi2)'
mat T2_2[1,2]= `r(df)'
mat T2_2[1,3]= `r(p)'
mat T2_2[1,4]= `r(p)'     

*3
test  (RR_AfAm_div3=RR_AfAm_div1) ///
      (RR_AfAm_div3=RR_AfAm_div2) ///
      (RR_AfAm_div3=RR_AfAm_div4) ///
      (RR_AfAm_div3=RR_AfAm_div5) ///
      (RR_AfAm_div3=RR_AfAm_div6) ///
      (RR_AfAm_div3=RR_AfAm_div7) ///
      (RR_AfAm_div3=RR_AfAm_div8) ///
      (RR_AfAm_div3=RR_AfAm_div9) ///
      , mtest(sidak)
mat T1_3=r(mtest)
mat T2_3=J(1,4,.)
mat T2_3[1,1]= `r(chi2)'
mat T2_3[1,2]= `r(df)'
mat T2_3[1,3]= `r(p)'
mat T2_3[1,4]= `r(p)'     

*4
test  (RR_AfAm_div4=RR_AfAm_div1) ///
      (RR_AfAm_div4=RR_AfAm_div2) ///
      (RR_AfAm_div4=RR_AfAm_div3) ///
      (RR_AfAm_div4=RR_AfAm_div5) ///
      (RR_AfAm_div4=RR_AfAm_div6) ///
      (RR_AfAm_div4=RR_AfAm_div7) ///
      (RR_AfAm_div4=RR_AfAm_div8) ///
      (RR_AfAm_div4=RR_AfAm_div9) ///
      , mtest(sidak)
mat T1_4=r(mtest) 
mat T2_4=J(1,4,.)
mat T2_4[1,1]= `r(chi2)'
mat T2_4[1,2]= `r(df)'
mat T2_4[1,3]= `r(p)'
mat T2_4[1,4]= `r(p)'     

test  (RR_AfAm_div5=RR_AfAm_div1) ///
      (RR_AfAm_div5=RR_AfAm_div2) ///
      (RR_AfAm_div5=RR_AfAm_div3) ///
      (RR_AfAm_div5=RR_AfAm_div4) ///
      (RR_AfAm_div5=RR_AfAm_div6) ///
      (RR_AfAm_div5=RR_AfAm_div7) ///
      (RR_AfAm_div5=RR_AfAm_div8) ///
      (RR_AfAm_div5=RR_AfAm_div9) ///
      , mtest(sidak)
mat T1_5=r(mtest)
mat T2_5=J(1,4,.)
mat T2_5[1,1]= `r(chi2)'
mat T2_5[1,2]= `r(df)'
mat T2_5[1,3]= `r(p)'
mat T2_5[1,4]= `r(p)'

test  (RR_AfAm_div6=RR_AfAm_div1) ///
      (RR_AfAm_div6=RR_AfAm_div2) ///
      (RR_AfAm_div6=RR_AfAm_div3) ///
      (RR_AfAm_div6=RR_AfAm_div4) ///
      (RR_AfAm_div6=RR_AfAm_div5) ///
      (RR_AfAm_div6=RR_AfAm_div7) ///
      (RR_AfAm_div6=RR_AfAm_div8) ///
      (RR_AfAm_div6=RR_AfAm_div9) ///
      , mtest(sidak)
mat T1_6=r(mtest)
mat T2_6=J(1,4,.)
mat T2_6[1,1]= `r(chi2)'
mat T2_6[1,2]= `r(df)'
mat T2_6[1,3]= `r(p)'
mat T2_6[1,4]= `r(p)'

test  (RR_AfAm_div7=RR_AfAm_div1) ///
      (RR_AfAm_div7=RR_AfAm_div2) ///
      (RR_AfAm_div7=RR_AfAm_div3) ///
      (RR_AfAm_div7=RR_AfAm_div4) ///
      (RR_AfAm_div7=RR_AfAm_div5) ///
      (RR_AfAm_div7=RR_AfAm_div6) ///
      (RR_AfAm_div7=RR_AfAm_div8) ///
      (RR_AfAm_div7=RR_AfAm_div9) ///
      , mtest(sidak)
mat T1_7=r(mtest)
mat T2_7=J(1,4,.)
mat T2_7[1,1]= `r(chi2)'
mat T2_7[1,2]= `r(df)'
mat T2_7[1,3]= `r(p)'
mat T2_7[1,4]= `r(p)'

test  (RR_AfAm_div8=RR_AfAm_div1) ///
      (RR_AfAm_div8=RR_AfAm_div2) ///
      (RR_AfAm_div8=RR_AfAm_div3) ///
      (RR_AfAm_div8=RR_AfAm_div4) ///
      (RR_AfAm_div8=RR_AfAm_div5) ///
      (RR_AfAm_div8=RR_AfAm_div6) ///
      (RR_AfAm_div8=RR_AfAm_div7) ///
      (RR_AfAm_div8=RR_AfAm_div9) ///
      , mtest(sidak)
mat T1_8=r(mtest)
mat T2_8=J(1,4,.)
mat T2_8[1,1]= `r(chi2)'
mat T2_8[1,2]= `r(df)'
mat T2_8[1,3]= `r(p)'
mat T2_8[1,4]= `r(p)'

test  (RR_AfAm_div9=RR_AfAm_div1) ///
      (RR_AfAm_div9=RR_AfAm_div2) ///
      (RR_AfAm_div9=RR_AfAm_div3) ///
      (RR_AfAm_div9=RR_AfAm_div4) ///
      (RR_AfAm_div9=RR_AfAm_div5) ///
      (RR_AfAm_div9=RR_AfAm_div6) ///
      (RR_AfAm_div9=RR_AfAm_div7) ///
      (RR_AfAm_div9=RR_AfAm_div8) ///
      , mtest(sidak)
mat T1_9=r(mtest)
mat T2_9=J(1,4,.)
mat T2_9[1,1]= `r(chi2)'
mat T2_9[1,2]= `r(df)'
mat T2_9[1,3]= `r(p)'
mat T2_9[1,4]= `r(p)'

mat T1=T1_1\T1_2\T1_3\T1_4\T1_5\T1_6\T1_7\T1_8\T1_9
mat T2=T2_1\T2_2\T2_3\T2_4\T2_5\T2_6\T2_7\T2_8\T2_9

mat def TAfAm=T1\T2

*Hispanic Tests
test  (RR_Hispanic_div1=RR_Hispanic_div2) ///
      (RR_Hispanic_div1=RR_Hispanic_div3) ///
      (RR_Hispanic_div1=RR_Hispanic_div4) ///
      (RR_Hispanic_div1=RR_Hispanic_div5) ///
      (RR_Hispanic_div1=RR_Hispanic_div6) ///
      (RR_Hispanic_div1=RR_Hispanic_div7) ///
      (RR_Hispanic_div1=RR_Hispanic_div8) ///
      (RR_Hispanic_div1=RR_Hispanic_div9) ///
      , mtest(sidak)
mat T1_1=r(mtest)
mat T2_1=J(1,4,.)
mat T2_1[1,1]= `r(chi2)'
mat T2_1[1,2]= `r(df)'
mat T2_1[1,3]= `r(p)'
mat T2_1[1,4]= `r(p)'           
*2
test  (RR_Hispanic_div2=RR_Hispanic_div1) ///
      (RR_Hispanic_div2=RR_Hispanic_div3) ///
      (RR_Hispanic_div2=RR_Hispanic_div4) ///
      (RR_Hispanic_div2=RR_Hispanic_div5) ///
      (RR_Hispanic_div2=RR_Hispanic_div6) ///
      (RR_Hispanic_div2=RR_Hispanic_div7) ///
      (RR_Hispanic_div2=RR_Hispanic_div8) ///
      (RR_Hispanic_div2=RR_Hispanic_div9) ///
      , mtest(sidak)
mat T1_2=r(mtest)
mat T2_2=J(1,4,.)
mat T2_2[1,1]= `r(chi2)'
mat T2_2[1,2]= `r(df)'
mat T2_2[1,3]= `r(p)'
mat T2_2[1,4]= `r(p)'     

*3
test  (RR_Hispanic_div3=RR_Hispanic_div1) ///
      (RR_Hispanic_div3=RR_Hispanic_div2) ///
      (RR_Hispanic_div3=RR_Hispanic_div4) ///
      (RR_Hispanic_div3=RR_Hispanic_div5) ///
      (RR_Hispanic_div3=RR_Hispanic_div6) ///
      (RR_Hispanic_div3=RR_Hispanic_div7) ///
      (RR_Hispanic_div3=RR_Hispanic_div8) ///
      (RR_Hispanic_div3=RR_Hispanic_div9) ///
      , mtest(sidak)
mat T1_3=r(mtest)
mat T2_3=J(1,4,.)
mat T2_3[1,1]= `r(chi2)'
mat T2_3[1,2]= `r(df)'
mat T2_3[1,3]= `r(p)'
mat T2_3[1,4]= `r(p)'     

*4
test  (RR_Hispanic_div4=RR_Hispanic_div1) ///
      (RR_Hispanic_div4=RR_Hispanic_div2) ///
      (RR_Hispanic_div4=RR_Hispanic_div3) ///
      (RR_Hispanic_div4=RR_Hispanic_div5) ///
      (RR_Hispanic_div4=RR_Hispanic_div6) ///
      (RR_Hispanic_div4=RR_Hispanic_div7) ///
      (RR_Hispanic_div4=RR_Hispanic_div8) ///
      (RR_Hispanic_div4=RR_Hispanic_div9) ///
      , mtest(sidak)
mat T1_4=r(mtest) 
mat T2_4=J(1,4,.)
mat T2_4[1,1]= `r(chi2)'
mat T2_4[1,2]= `r(df)'
mat T2_4[1,3]= `r(p)'
mat T2_4[1,4]= `r(p)'     

test  (RR_Hispanic_div5=RR_Hispanic_div1) ///
      (RR_Hispanic_div5=RR_Hispanic_div2) ///
      (RR_Hispanic_div5=RR_Hispanic_div3) ///
      (RR_Hispanic_div5=RR_Hispanic_div4) ///
      (RR_Hispanic_div5=RR_Hispanic_div6) ///
      (RR_Hispanic_div5=RR_Hispanic_div7) ///
      (RR_Hispanic_div5=RR_Hispanic_div8) ///
      (RR_Hispanic_div5=RR_Hispanic_div9) ///
      , mtest(sidak)
mat T1_5=r(mtest)
mat T2_5=J(1,4,.)
mat T2_5[1,1]= `r(chi2)'
mat T2_5[1,2]= `r(df)'
mat T2_5[1,3]= `r(p)'
mat T2_5[1,4]= `r(p)'

test  (RR_Hispanic_div6=RR_Hispanic_div1) ///
      (RR_Hispanic_div6=RR_Hispanic_div2) ///
      (RR_Hispanic_div6=RR_Hispanic_div3) ///
      (RR_Hispanic_div6=RR_Hispanic_div4) ///
      (RR_Hispanic_div6=RR_Hispanic_div5) ///
      (RR_Hispanic_div6=RR_Hispanic_div7) ///
      (RR_Hispanic_div6=RR_Hispanic_div8) ///
      (RR_Hispanic_div6=RR_Hispanic_div9) ///
      , mtest(sidak)
mat T1_6=r(mtest)
mat T2_6=J(1,4,.)
mat T2_6[1,1]= `r(chi2)'
mat T2_6[1,2]= `r(df)'
mat T2_6[1,3]= `r(p)'
mat T2_6[1,4]= `r(p)'

test  (RR_Hispanic_div7=RR_Hispanic_div1) ///
      (RR_Hispanic_div7=RR_Hispanic_div2) ///
      (RR_Hispanic_div7=RR_Hispanic_div3) ///
      (RR_Hispanic_div7=RR_Hispanic_div4) ///
      (RR_Hispanic_div7=RR_Hispanic_div5) ///
      (RR_Hispanic_div7=RR_Hispanic_div6) ///
      (RR_Hispanic_div7=RR_Hispanic_div8) ///
      (RR_Hispanic_div7=RR_Hispanic_div9) ///
      , mtest(sidak)
mat T1_7=r(mtest)
mat T2_7=J(1,4,.)
mat T2_7[1,1]= `r(chi2)'
mat T2_7[1,2]= `r(df)'
mat T2_7[1,3]= `r(p)'
mat T2_7[1,4]= `r(p)'

test  (RR_Hispanic_div8=RR_Hispanic_div1) ///
      (RR_Hispanic_div8=RR_Hispanic_div2) ///
      (RR_Hispanic_div8=RR_Hispanic_div3) ///
      (RR_Hispanic_div8=RR_Hispanic_div4) ///
      (RR_Hispanic_div8=RR_Hispanic_div5) ///
      (RR_Hispanic_div8=RR_Hispanic_div6) ///
      (RR_Hispanic_div8=RR_Hispanic_div7) ///
      (RR_Hispanic_div8=RR_Hispanic_div9) ///
      , mtest(sidak)
mat T1_8=r(mtest)
mat T2_8=J(1,4,.)
mat T2_8[1,1]= `r(chi2)'
mat T2_8[1,2]= `r(df)'
mat T2_8[1,3]= `r(p)'
mat T2_8[1,4]= `r(p)'

test  (RR_Hispanic_div9=RR_Hispanic_div1) ///
      (RR_Hispanic_div9=RR_Hispanic_div2) ///
      (RR_Hispanic_div9=RR_Hispanic_div3) ///
      (RR_Hispanic_div9=RR_Hispanic_div4) ///
      (RR_Hispanic_div9=RR_Hispanic_div5) ///
      (RR_Hispanic_div9=RR_Hispanic_div6) ///
      (RR_Hispanic_div9=RR_Hispanic_div7) ///
      (RR_Hispanic_div9=RR_Hispanic_div8) ///
      , mtest(sidak)
mat T1_9=r(mtest)
mat T2_9=J(1,4,.)
mat T2_9[1,1]= `r(chi2)'
mat T2_9[1,2]= `r(df)'
mat T2_9[1,3]= `r(p)'
mat T2_9[1,4]= `r(p)'

mat T1=T1_1\T1_2\T1_3\T1_4\T1_5\T1_6\T1_7\T1_8\T1_9
mat T2=T2_1\T2_2\T2_3\T2_4\T2_5\T2_6\T2_7\T2_8\T2_9

mat def THispanic=T1\T2



* Results into a Matrix  
matrix define A=J(`geog',3,.)
matrix colnames A = afam_lci afam_coef afam_uci
matrix define H=J(`geog',4,.)
matrix colnames H = hispanic_lci hispanic_coef hispanic_uci resp_white


forvalues i = 1/`geog'{
  matrix A[`i',1] =  _b[RR_AfAm_div`i'] - invttail(e(N),0.05)*_se[RR_AfAm_div`i']
  matrix A[`i',2] =  _b[RR_AfAm_div`i']
  matrix A[`i',3] =  _b[RR_AfAm_div`i'] + invttail(e(N),0.05)*_se[RR_AfAm_div`i']
  matrix H[`i',1] =  _b[RR_Hispanic_div`i'] - invttail(e(N),0.05)*_se[RR_Hispanic_div`i']
  matrix H[`i',2] =  _b[RR_Hispanic_div`i']
  matrix H[`i',3] =  _b[RR_Hispanic_div`i'] + invttail(e(N),0.05)*_se[RR_Hispanic_div`i']
  matrix H[`i',4] =  `mean`i'' 
}






 


************************************************************************************************
*Consolidate matrices and export
************************************************************************************************
mat def Res=M,A,H

local rows 
foreach var of varlist div* {
local this = strtoname("`: variable label `var''") 
local rows `rows' `this'
}

mat rownames Res = `rows'


preserve
clear
svmat2 Res,  names(col)  rnames(CBSA)
gen n=_n

save "stores/matrices/division_single_regression_RR.dta"  , replace
restore



mat def Tests=T,TAfAm,THispanic
preserve
clear
svmat2 Tests, rnames(CBSA)

rename Tests1  Minority_chi2         
rename Tests2  Minority_df
rename Tests3  Minority_p
rename Tests4  Minority_adjustp
rename Tests5  AfAm_chi2         
rename Tests6  AfAm_df
rename Tests7  AfAm_p
rename Tests8  AfAm_adjustp
rename Tests9  Hispanci_chi2         
rename Tests10 Hispanci_df
rename Tests11 Hispanci_p
rename Tests12 Hispanci_adjustp

save "stores/matrices/division_tests_hispanic_RR.dta"  , replace
export delimited using "stores/matrices/division_tests_hispanic_RR.csv", replace
restore