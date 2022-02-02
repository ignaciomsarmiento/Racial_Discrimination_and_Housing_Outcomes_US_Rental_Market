/*
Replication files  
*/

clear all
set matsize 11000

*Paths
*Working directory
cd "~/Dropbox/Research/cbsa_discrimination/github/Racial_Discrimination_US_Rental_Market"

global path_tables  "views"
*global path_tables  "~/Dropbox/Apps/ShareLaTeX/Welfare Impact of Housing Discrimination/tables/cbsa_discrimination/prelim"


*Load Data
use "stores/matchedinquiries.dta"


 
gen StreetAddress=upper(Address)

merge n:1 StreetAddress Zip_Code using "stores/infoUSA_wru_building.dta"

save "stores/matchedinquiries_infoUSA_wru_building.dta",replace
 
