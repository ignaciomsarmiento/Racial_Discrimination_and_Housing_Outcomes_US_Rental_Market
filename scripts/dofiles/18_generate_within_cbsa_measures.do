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
use "stores/opportunities_cbsa.dta"


merge 1:1 state county tract using "../../Oportunity_Data/tract_covariates.dta"
drop if _merge!=3

gen str2 state_str=string(state,"%02.0f")
gen str3 county_str=string(county,"%03.0f")
gen str6 tract_str=string(tract,"%06.0f")
gen id_tract=state_str+county_str+tract_str

*1 lowest
foreach race in black white hisp {
	egen cbsa_quart_kfr_`race'_pooled_mean = xtile(kfr_`race'_pooled_mean), by(CBSA) n(4)
	egen cbsa_quart_share_`race' = xtile(share_`race'2010), by(CBSA) n(4)
}

egen cbsa_quart_med_hhinc= xtile(med_hhinc2016), by(CBSA) n(4)
egen cbsa_quart_singleparent_share= xtile(singleparent_share2010), by(CBSA) n(4)

keep CBSA cz czname id_tract cbsa_quart_*

tempfile oportunity
save `oportunity'


*Load Matched inquiries
use "stores/matchedinquiries.dta",clear


gen id_tract=substr(GEOID,1,11)

merge n:1 id_tract using `oportunity'

drop if _merge==2
drop _merge


compress

save "stores/matched_within_cbsa_opportunities.dta",replace

