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




est clear


************************************************************************************************
* Af American and Hispanic/Latinx
************************************************************************************************



forvalues i = 1/50{
  qui sum choice if White==1 & cbsa`i'==1
  loc r_white_`i'=r(mean)
  loc r_sd_white_`i'=r(sd)
}

* Results into a Matrix  

matrix define A=J(50,4,.)
matrix colnames A = afam_lci afam_coef afam_uci afam_se

matrix define H=J(50,8,.)
matrix colnames H = hispanic_lci hispanic_coef hispanic_uci hispanic_se resp_white resp_se_white resp_lci_white resp_uci_white


reghdfe choice   AfAm_cbsa*   Hispanic_cbsa*    , absorb(gender education_level inquiry_order Address )  cl(CBSA_downtown) level(90)  nocons



  nlcom (RR_AfAm_cbsa1  : _b[AfAm_cbsa1]  /`r_white_1') ///
        (RR_AfAm_cbsa2  : _b[AfAm_cbsa2]  /`r_white_2') ///
        (RR_AfAm_cbsa3  : _b[AfAm_cbsa3]  /`r_white_3') ///
        (RR_AfAm_cbsa4  : _b[AfAm_cbsa4]  /`r_white_4') ///
        (RR_AfAm_cbsa5  : _b[AfAm_cbsa5]  /`r_white_5') ///
        (RR_AfAm_cbsa6  : _b[AfAm_cbsa6]  /`r_white_6') ///
        (RR_AfAm_cbsa7  : _b[AfAm_cbsa7]  /`r_white_7') ///
        (RR_AfAm_cbsa8  : _b[AfAm_cbsa8]  /`r_white_8') ///
        (RR_AfAm_cbsa9  : _b[AfAm_cbsa9]  /`r_white_9') ///
        (RR_AfAm_cbsa10 : _b[AfAm_cbsa10] /`r_white_10') ///
        (RR_AfAm_cbsa11 : _b[AfAm_cbsa11] /`r_white_11') ///
        (RR_AfAm_cbsa12 : _b[AfAm_cbsa12] /`r_white_12') ///
        (RR_AfAm_cbsa13 : _b[AfAm_cbsa13] /`r_white_13') ///
        (RR_AfAm_cbsa14 : _b[AfAm_cbsa14] /`r_white_14') ///
        (RR_AfAm_cbsa15 : _b[AfAm_cbsa15] /`r_white_15') ///
        (RR_AfAm_cbsa16 : _b[AfAm_cbsa16] /`r_white_16') ///
        (RR_AfAm_cbsa17 : _b[AfAm_cbsa17] /`r_white_17') ///
        (RR_AfAm_cbsa18 : _b[AfAm_cbsa18] /`r_white_18') ///
        (RR_AfAm_cbsa19 : _b[AfAm_cbsa19] /`r_white_19') ///
        (RR_AfAm_cbsa20 : _b[AfAm_cbsa20] /`r_white_20') ///
        (RR_AfAm_cbsa21 : _b[AfAm_cbsa21] /`r_white_21') ///
        (RR_AfAm_cbsa22 : _b[AfAm_cbsa22] /`r_white_22') ///
        (RR_AfAm_cbsa23 : _b[AfAm_cbsa23] /`r_white_23') ///
        (RR_AfAm_cbsa24 : _b[AfAm_cbsa24] /`r_white_24') ///
        (RR_AfAm_cbsa25 : _b[AfAm_cbsa25] /`r_white_25') ///
        (RR_AfAm_cbsa26 : _b[AfAm_cbsa26] /`r_white_26') ///
        (RR_AfAm_cbsa27 : _b[AfAm_cbsa27] /`r_white_27') ///
        (RR_AfAm_cbsa28 : _b[AfAm_cbsa28] /`r_white_28') ///
        (RR_AfAm_cbsa29 : _b[AfAm_cbsa29] /`r_white_29') ///
        (RR_AfAm_cbsa30 : _b[AfAm_cbsa30] /`r_white_30') ///
        (RR_AfAm_cbsa31 : _b[AfAm_cbsa31] /`r_white_31') ///
        (RR_AfAm_cbsa32 : _b[AfAm_cbsa32] /`r_white_32') ///
        (RR_AfAm_cbsa33 : _b[AfAm_cbsa33] /`r_white_33') ///
        (RR_AfAm_cbsa34 : _b[AfAm_cbsa34] /`r_white_34') ///
        (RR_AfAm_cbsa35 : _b[AfAm_cbsa35] /`r_white_35') ///
        (RR_AfAm_cbsa36 : _b[AfAm_cbsa36] /`r_white_36') ///
        (RR_AfAm_cbsa37 : _b[AfAm_cbsa37] /`r_white_37') ///
        (RR_AfAm_cbsa38 : _b[AfAm_cbsa38] /`r_white_38') ///
        (RR_AfAm_cbsa39 : _b[AfAm_cbsa39] /`r_white_39') ///
        (RR_AfAm_cbsa40 : _b[AfAm_cbsa40] /`r_white_40') ///
        (RR_AfAm_cbsa41 : _b[AfAm_cbsa41] /`r_white_41') ///
        (RR_AfAm_cbsa42 : _b[AfAm_cbsa42] /`r_white_42') ///
        (RR_AfAm_cbsa43 : _b[AfAm_cbsa43] /`r_white_43') ///
        (RR_AfAm_cbsa44 : _b[AfAm_cbsa44] /`r_white_44') ///
        (RR_AfAm_cbsa45 : _b[AfAm_cbsa45] /`r_white_45') ///
        (RR_AfAm_cbsa46 : _b[AfAm_cbsa46] /`r_white_46') ///
        (RR_AfAm_cbsa47 : _b[AfAm_cbsa47] /`r_white_47') ///
        (RR_AfAm_cbsa48 : _b[AfAm_cbsa48] /`r_white_48') ///
        (RR_AfAm_cbsa49 : _b[AfAm_cbsa49] /`r_white_49') ///
        (RR_AfAm_cbsa50 : _b[AfAm_cbsa50] /`r_white_50') ///
        (RR_Hispanic_cbsa1  : _b[Hispanic_cbsa1]  /`r_white_1') ///
        (RR_Hispanic_cbsa2  : _b[Hispanic_cbsa2]  /`r_white_2') ///
        (RR_Hispanic_cbsa3  : _b[Hispanic_cbsa3]  /`r_white_3') ///
        (RR_Hispanic_cbsa4  : _b[Hispanic_cbsa4]  /`r_white_4') ///
        (RR_Hispanic_cbsa5  : _b[Hispanic_cbsa5]  /`r_white_5') ///
        (RR_Hispanic_cbsa6  : _b[Hispanic_cbsa6]  /`r_white_6') ///
        (RR_Hispanic_cbsa7  : _b[Hispanic_cbsa7]  /`r_white_7') ///
        (RR_Hispanic_cbsa8  : _b[Hispanic_cbsa8]  /`r_white_8') ///
        (RR_Hispanic_cbsa9  : _b[Hispanic_cbsa9]  /`r_white_9') ///
        (RR_Hispanic_cbsa10 : _b[Hispanic_cbsa10] /`r_white_10') ///
        (RR_Hispanic_cbsa11 : _b[Hispanic_cbsa11] /`r_white_11') ///
        (RR_Hispanic_cbsa12 : _b[Hispanic_cbsa12] /`r_white_12') ///
        (RR_Hispanic_cbsa13 : _b[Hispanic_cbsa13] /`r_white_13') ///
        (RR_Hispanic_cbsa14 : _b[Hispanic_cbsa14] /`r_white_14') ///
        (RR_Hispanic_cbsa15 : _b[Hispanic_cbsa15] /`r_white_15') ///
        (RR_Hispanic_cbsa16 : _b[Hispanic_cbsa16] /`r_white_16') ///
        (RR_Hispanic_cbsa17 : _b[Hispanic_cbsa17] /`r_white_17') ///
        (RR_Hispanic_cbsa18 : _b[Hispanic_cbsa18] /`r_white_18') ///
        (RR_Hispanic_cbsa19 : _b[Hispanic_cbsa19] /`r_white_19') ///
        (RR_Hispanic_cbsa20 : _b[Hispanic_cbsa20] /`r_white_20') ///
        (RR_Hispanic_cbsa21 : _b[Hispanic_cbsa21] /`r_white_21') ///
        (RR_Hispanic_cbsa22 : _b[Hispanic_cbsa22] /`r_white_22') ///
        (RR_Hispanic_cbsa23 : _b[Hispanic_cbsa23] /`r_white_23') ///
        (RR_Hispanic_cbsa24 : _b[Hispanic_cbsa24] /`r_white_24') ///
        (RR_Hispanic_cbsa25 : _b[Hispanic_cbsa25] /`r_white_25') ///
        (RR_Hispanic_cbsa26 : _b[Hispanic_cbsa26] /`r_white_26') ///
        (RR_Hispanic_cbsa27 : _b[Hispanic_cbsa27] /`r_white_27') ///
        (RR_Hispanic_cbsa28 : _b[Hispanic_cbsa28] /`r_white_28') ///
        (RR_Hispanic_cbsa29 : _b[Hispanic_cbsa29] /`r_white_29') ///
        (RR_Hispanic_cbsa30 : _b[Hispanic_cbsa30] /`r_white_30') ///
        (RR_Hispanic_cbsa31 : _b[Hispanic_cbsa31] /`r_white_31') ///
        (RR_Hispanic_cbsa32 : _b[Hispanic_cbsa32] /`r_white_32') ///
        (RR_Hispanic_cbsa33 : _b[Hispanic_cbsa33] /`r_white_33') ///
        (RR_Hispanic_cbsa34 : _b[Hispanic_cbsa34] /`r_white_34') ///
        (RR_Hispanic_cbsa35 : _b[Hispanic_cbsa35] /`r_white_35') ///
        (RR_Hispanic_cbsa36 : _b[Hispanic_cbsa36] /`r_white_36') ///
        (RR_Hispanic_cbsa37 : _b[Hispanic_cbsa37] /`r_white_37') ///
        (RR_Hispanic_cbsa38 : _b[Hispanic_cbsa38] /`r_white_38') ///
        (RR_Hispanic_cbsa39 : _b[Hispanic_cbsa39] /`r_white_39') ///
        (RR_Hispanic_cbsa40 : _b[Hispanic_cbsa40] /`r_white_40') ///
        (RR_Hispanic_cbsa41 : _b[Hispanic_cbsa41] /`r_white_41') ///
        (RR_Hispanic_cbsa42 : _b[Hispanic_cbsa42] /`r_white_42') ///
        (RR_Hispanic_cbsa43 : _b[Hispanic_cbsa43] /`r_white_43') ///
        (RR_Hispanic_cbsa44 : _b[Hispanic_cbsa44] /`r_white_44') ///
        (RR_Hispanic_cbsa45 : _b[Hispanic_cbsa45] /`r_white_45') ///
        (RR_Hispanic_cbsa46 : _b[Hispanic_cbsa46] /`r_white_46') ///
        (RR_Hispanic_cbsa47 : _b[Hispanic_cbsa47] /`r_white_47') ///
        (RR_Hispanic_cbsa48 : _b[Hispanic_cbsa48] /`r_white_48') ///
        (RR_Hispanic_cbsa49 : _b[Hispanic_cbsa49] /`r_white_49') ///
        (RR_Hispanic_cbsa50 : _b[Hispanic_cbsa50] /`r_white_50') ///
        , post


forvalues i = 1/50{

  matrix A[`i',1] =  _b[RR_AfAm_cbsa`i'] - invttail(e(N),0.05)*_se[RR_AfAm_cbsa`i']
  matrix A[`i',2] =  _b[RR_AfAm_cbsa`i']
  matrix A[`i',3] =  _b[RR_AfAm_cbsa`i'] + invttail(e(N),0.05)*_se[RR_AfAm_cbsa`i']
  matrix A[`i',4] =  _se[RR_AfAm_cbsa`i'] 

  matrix H[`i',1] =  _b[RR_Hispanic_cbsa`i'] - invttail(e(N),0.05)*_se[RR_Hispanic_cbsa`i']
  matrix H[`i',2] =  _b[RR_Hispanic_cbsa`i']
  matrix H[`i',3] =  _b[RR_Hispanic_cbsa`i'] + invttail(e(N),0.05)*_se[RR_Hispanic_cbsa`i']
  matrix H[`i',4] =  _se[RR_Hispanic_cbsa`i']
  matrix H[`i',5] =  `r_white_`i'' 
  matrix H[`i',6] =  `r_sd_white_`i'' 
  matrix H[`i',7] =  `r_white_`i''  - invnormal(0.05)*`r_sd_white_`i'' / sqrt(e(N))
  matrix H[`i',8] =  `r_white_`i''  + invnormal(0.05)*`r_sd_white_`i''  /sqrt(e(N))
}

 

************************************************************************************************
*Consolidate matrices and export
************************************************************************************************
mat def Res=A,H

mat list Res

local rows 
foreach var of varlist cbsa* {
local this = strtoname("`: variable label `var''") 
local rows `rows' `this'
}

mat rownames Res = `rows'


preserve
clear
svmat2 Res,  names(col)  rnames(CBSA)
gen n=_n

save "stores/matrices/cbsa_single_regression_RR.dta"  , replace
restore


  *end 