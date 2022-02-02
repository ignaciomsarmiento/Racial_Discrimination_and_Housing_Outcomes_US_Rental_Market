/*
Replication files  
*/

clear all


*Paths
*Working directory
cd "~/Dropbox/Research/cbsa_discrimination/github/Racial_Discrimination_US_Rental_Market/"

global path_tables  "views"
*global path_tables  "~/Dropbox/Apps/ShareLaTeX/Welfare Impact of Housing Discrimination/tables/cbsa_discrimination/prelim"


use "stores/matchedinquiries_opportunities_tract.dta", clear

drop if  Address =="4245 W Pine Blvd #24O"
drop if race==2
gen t=1 if race==1
replace t=2 if race==3

drop dir
encode Address, gen(dir)
xtset  dir t


gen diff_white_hisp=kfr_white_pooled_mean-kfr_hisp_pooled_mean
gen diff_white_black=kfr_white_pooled_mean-kfr_black_pooled_mean,

xi: xtsemipar choice gender i.education_level i.inquiry_order, nonpar(diff_white_black) cluster(CBSA_downtown) ci  generate(a b) nograph

scatter a diff_white_black, lwidth(thick)						///	
	xtitle("Rank White-Rank Aff. Am.") 							///
	ytitle("Relative Response Fit") 	, scheme(s1mono)	
graph export "views/poly_income_w_b.pdf", replace


