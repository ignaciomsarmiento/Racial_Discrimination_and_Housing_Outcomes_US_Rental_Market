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

use "stores/matched_within_cbsa_opportunities.dta"

mat define A=J(4,3,.)
forvalues i = 1/4{
di "***********************************"
di "Whites"
di "***********************************"
sum choice if race==1 & cbsa_quart_kfr_black_pooled_mean==`i'
matrix A[`i',1] =  `r(mean)'
di "***********************************"
di "Hispanics"
di "***********************************"
sum choice if race==2 & cbsa_quart_kfr_black_pooled_mean==`i'
matrix A[`i',3] =  `r(mean)'
di "***********************************"
di "Blacks"
di "***********************************"
sum choice if race==3 & cbsa_quart_kfr_black_pooled_mean==`i'
matrix A[`i',2] =  `r(mean)'
}
mat list A






mat define A=J(4,3,.)
forvalues i = 1/4{
di "***********************************"
di "Whites"
di "***********************************"
sum choice if race==1 & cbsa_quart_kfr_hisp_pooled_mean==`i'
matrix A[`i',1] =  `r(mean)'
di "***********************************"
di "Hispanics"
di "***********************************"
sum choice if race==2 & cbsa_quart_kfr_hisp_pooled_mean==`i'
matrix A[`i',3] =  `r(mean)'
di "***********************************"
di "Blacks"
di "***********************************"
sum choice if race==3 & cbsa_quart_kfr_hisp_pooled_mean==`i'
matrix A[`i',2] =  `r(mean)'
}
mat list A





mat define A=J(4,3,.)
forvalues i = 1/4{
di "***********************************"
di "Whites"
di "***********************************"
sum choice if race==1 & cbsa_quart_kfr_white_pooled_mean==`i'
matrix A[`i',1] =  `r(mean)'
di "***********************************"
di "Hispanics"
di "***********************************"
sum choice if race==2 & cbsa_quart_kfr_white_pooled_mean==`i'
matrix A[`i',3] =  `r(mean)'
di "***********************************"
di "Blacks"
di "***********************************"
sum choice if race==3 & cbsa_quart_kfr_white_pooled_mean==`i'
matrix A[`i',2] =  `r(mean)'
}
mat list A




mat define A=J(4,3,.)
forvalues i = 1/4{
di "***********************************"
di "Whites"
di "***********************************"
sum choice if race==1 & cbsa_quart_share_black==`i'
matrix A[`i',1] =  `r(mean)'
di "***********************************"
di "Hispanics"
di "***********************************"
sum choice if race==2 & cbsa_quart_share_black==`i'
matrix A[`i',3] =  `r(mean)'
di "***********************************"
di "Blacks"
di "***********************************"
sum choice if race==3 & cbsa_quart_share_black==`i'
matrix A[`i',2] =  `r(mean)'
}
mat list A




mat define A=J(4,3,.)
forvalues i = 1/4{
di "***********************************"
di "Whites"
di "***********************************"
sum choice if race==1 & cbsa_quart_share_hisp==`i'
matrix A[`i',1] =  `r(mean)'
di "***********************************"
di "Hispanics"
di "***********************************"
sum choice if race==2 & cbsa_quart_share_hisp==`i'
matrix A[`i',3] =  `r(mean)'
di "***********************************"
di "Blacks"
di "***********************************"
sum choice if race==3 & cbsa_quart_share_hisp==`i'
matrix A[`i',2] =  `r(mean)'
}
mat list A





mat define A=J(4,3,.)
forvalues i = 1/4{
di "***********************************"
di "Whites"
di "***********************************"
sum choice if race==1 & cbsa_quart_share_white==`i'
matrix A[`i',1] =  `r(mean)'
di "***********************************"
di "Hispanics"
di "***********************************"
sum choice if race==2 & cbsa_quart_share_white==`i'
matrix A[`i',3] =  `r(mean)'
di "***********************************"
di "Blacks"
di "***********************************"
sum choice if race==3 & cbsa_quart_share_white==`i'
matrix A[`i',2] =  `r(mean)'
}
mat list A