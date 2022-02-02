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



gen qwhitepopchange=(whitepopchange<=-0.1)
replace qwhitepopchange=2 if whitepopchange>-0.1 & whitepopchange<=-0.02
replace qwhitepopchange=3 if whitepopchange>-0.02 & whitepopchange<=0.02
replace qwhitepopchange=4 if whitepopchange>0.02 & whitepopchange<=0.1
replace qwhitepopchange=5 if whitepopchange>0.1

forvalues i=0/5{
  sum whitepopchange if qwhitepopchange==`i'  
}

tabulate qwhitepopchange, generate(d_qwhitepopchange)

loc cuts=5
foreach race in Minority Hispanic AfAm {
    forvalues i = 1/`cuts'{
      gen `race'_qwhitepopchange`i'=`race'*d_qwhitepopchange`i'
    }
}

************************************************************************************************
* Af American and Hispanic/Latinx
************************************************************************************************

reghdfe choice   AfAm_qwhitepopchange*   Hispanic_qwhitepopchange* , absorb(gender education_level inquiry_order  Address)  cl(CBSA_downtown) level(90) nocons
estadd scalar obs = `e(N)'
forvalues i = 1/`cuts'{
    sum choice if White==1 & d_qwhitepopchange`i'==1
    loc loc_white`i'=r(mean)
    estadd scalar loc_white`i' = `loc_white`i'', replace 
}
estadd local gender = "Yes", replace 
estadd local edu = "Yes", replace 
estadd local order = "Yes", replace 
estadd local address = "Yes", replace 
estadd local dm = "Yes", replace 
estimates store model


    nlcom (RR_AfAm_qwhitepopchange1: _b[AfAm_qwhitepopchange1]/`loc_white1') ///
          (RR_AfAm_qwhitepopchange2: _b[AfAm_qwhitepopchange2]/`loc_white2') ///
          (RR_AfAm_qwhitepopchange3: _b[AfAm_qwhitepopchange3]/`loc_white3') ///
          (RR_AfAm_qwhitepopchange4: _b[AfAm_qwhitepopchange4]/`loc_white4') ///
          (RR_AfAm_qwhitepopchange5: _b[AfAm_qwhitepopchange5]/`loc_white5') ///
          (RR_Hispanic_qwhitepopchange1: _b[Hispanic_qwhitepopchange1]/`loc_white1') ///
          (RR_Hispanic_qwhitepopchange2: _b[Hispanic_qwhitepopchange2]/`loc_white2') ///
          (RR_Hispanic_qwhitepopchange3: _b[Hispanic_qwhitepopchange3]/`loc_white3') ///
          (RR_Hispanic_qwhitepopchange4: _b[Hispanic_qwhitepopchange4]/`loc_white4') ///
          (RR_Hispanic_qwhitepopchange5: _b[Hispanic_qwhitepopchange5]/`loc_white5') ///
     , post



