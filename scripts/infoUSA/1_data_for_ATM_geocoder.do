/*
Replication files  
*/

clear all
set matsize 11000

*Paths
*Working directory
cd "~/Dropbox/Research/cbsa_discrimination/"

global path_tables  "views"
*global path_tables  "~/Dropbox/Apps/ShareLaTeX/Welfare Impact of Housing Discrimination/tables/cbsa_discrimination/prelim"


*Load Data
use "stores/matchedinquiries.dta"


 keep  Address City State Zip_Code 
 duplicates drop Address City State Zip_Code , force

rename Address StreetAddress  
rename Zip_Code Zip

export delimited using "~/Dropbox/Research/cbsa_discrimination/github/Racial_Discrimination_US_Rental_Market/stores/infoUSA/ATM_geocoder.csv", replace
 
