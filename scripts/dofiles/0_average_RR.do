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


loc geog = 4


sum choice if White==1 
loc mean1 `r(mean)'



************************************************************************************************
* Af American and Hispanic/Latinx
************************************************************************************************



reghdfe choice   AfAm   Hispanic , absorb(gender education_level inquiry_order Address)  cl(CBSA_downtown) level(90) nocons

    nlcom (RR_AfAm: _b[AfAm]/`mean1') ///
          (RR_Hispanic: _b[Hispanic]/`mean1') ///
     , post  level(90)  

