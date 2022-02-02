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

gen choice_white=choice if race==1
gen choice_hispanic=choice if race==2
gen choice_black=choice if race==3

collapse (mean)  choice_white choice_hispanic choice_black (median) kfr_black_pooled_mean, by(tract_opp)

gen black_white=choice_black-choice_white

sum kfr_black_pooled_mean, d
egen cut_kfr=cut(kfr_black_pooled_mean), at(0.2(0.1)0.8) label
*
preserve
collapse (mean)  black_white,  by(cut_kfr)

twoway (line black_white cut_kfr)

gen diff_white_hisp=kfr_white_pooled_mean-kfr_hisp_pooled_mean
gen diff_white_black=kfr_white_pooled_mean-kfr_black_pooled_mean,


bys CBSA: egen mean_kfr=mean(kfr_white_pooled_mean)
gen diff= kfr_black_pooled_mean-mean_kfr

loc quantiles 20
loc h=0.5
loc quantiles_1=`quantiles'-1


matrix define A=J(`quantiles_1',4,.)

forvalues i= 1/`quantiles_1'{
	qui _pctile  kfr_black_pooled_mean,  nquantiles(`quantiles')
	matrix A[`i',4] =  `r(r`i')'
	gen distance=  kfr_black_pooled_mean-`r(r`i')'
	qui sum distance
	loc h=`r(sd)'/`h'
	replace distance=. if abs(distance)>=`h'
	replace distance=distance/`r(max)'
	gen weight`i'= (1-(abs(distance))^3)^3
	drop distance
	replace weight`i'=0 if weight`i'>=1
	replace weight`i'=0 if weight`i'<=0
}

*reghdfe choice   AfAm  Hispanic   , absorb(gender education_level inquiry_order tract )  cl(CBSA_downtown) level(90)  

forvalues i= 1/`quantiles_1'{
  qui reghdfe choice   AfAm  Hispanic   [pweight=weight`i'], absorb(gender education_level inquiry_order tract_opp )  cl(CBSA_downtown) level(90)  
  matrix A[`i',1] =  _b[AfAm] - invttail(e(N),0.05)*_se[AfAm]
  matrix A[`i',2] =  _b[AfAm]
  matrix A[`i',3] =  _b[AfAm] + invttail(e(N),0.05)*_se[AfAm]
  
}

mat list A
*drop weight*


preserve
clear
svmat2 A,  names(col)  
twoway  rarea c3 c1 c4 || line c2 c4 
restore
