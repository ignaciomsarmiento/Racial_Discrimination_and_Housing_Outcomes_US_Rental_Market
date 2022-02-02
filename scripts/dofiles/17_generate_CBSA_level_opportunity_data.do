/*
Replication files  
*/

clear all
set maxvar 11000

*Paths
*Working directory
cd "~/Dropbox/Research/cbsa_discrimination/github/Racial_Discrimination_US_Rental_Market/"

global path_tables  "views"
*global path_tables  "~/Dropbox/Apps/ShareLaTeX/Welfare Impact of Housing Discrimination/tables/cbsa_discrimination/prelim"



*load oportunity atlas and create tract id
use "../../Oportunity_Data/tract_outcomes_early_dta.dta"


gen str2 state_str=string(state,"%02.0f")
gen str3 county_str=string(county,"%03.0f")
gen str6 tract_str=string(tract,"%06.0f")
gen id_tract=state_str+county_str+tract_str

drop state_str county_str tract_str

rename county  county_opp
rename state  state_opp
rename tract  tract_opp

*br  county_opp state_opp  tract_opp  id_tract  if county_opp==73

keep id_tract cz czname
tempfile oportunity
save `oportunity'


*Load Matched inquiries
use "stores/matchedinquiries.dta",clear


gen id_tract=substr(GEOID,1,11)

merge n:1 id_tract using `oportunity'

drop if _merge==2
drop _merge

keep CBSA cz czname 
duplicates drop CBSA cz czname, force



merge n:n cz czname using "../../Oportunity_Data/tract_outcomes_early_dta.dta"
drop if _merge==2
drop _merge

compress

save "stores/opportunities_cbsa.dta",replace


