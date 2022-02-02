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


foreach race in black white{

	tabulate cbsa_quart_kfr_`race'_pooled_mean , gen(quart_`race')

	tab2xl cbsa_quart_kfr_`race'_pooled_mean  using views/table_kfr_black_`race', col(1) row(1) 

	forvalues i=1/4{
		gen AfAm`i'= AfAm*quart_`race'`i'
	}

	reghdfe choice   AfAm1  ///
					 AfAm2  ///
					 AfAm3  ///
					 AfAm4  ///
					 if race!=2  , absorb(gender education_level inquiry_order  Address)  cl(CBSA) level(90) nocons


	forvalues i = 1/4{
	  sum choice if White==1 & quart_`race'`i'==1
	  loc mean`i' `r(mean)'
	}

	   nlcom (RR_AfAm_reg1: _b[AfAm1]/`mean1') ///
	          (RR_AfAm_reg2: _b[AfAm2]/`mean2') ///
	          (RR_AfAm_reg3: _b[AfAm3]/`mean3') ///
	          (RR_AfAm_reg4: _b[AfAm4]/`mean4')     , post



	* Results into a Matrix  
	matrix define A=J(4,4,.)
	matrix colnames A = afam_lci afam_coef afam_uci mean


	forvalues i = 1/4{
	  matrix A[`i',1] =  _b[RR_AfAm_reg`i'] - invttail(e(N),0.05)*_se[RR_AfAm_reg`i']
	  matrix A[`i',2] =  _b[RR_AfAm_reg`i']
	  matrix A[`i',3] =  _b[RR_AfAm_reg`i'] + invttail(e(N),0.05)*_se[RR_AfAm_reg`i']
	  matrix A[`i',4] =  `mean`i'' 
	}




	drop AfAm1 AfAm2 AfAm3 AfAm4

	************************************************************************************************
	*Results
	************************************************************************************************

	preserve
	clear
	svmat2 A, names(col)
	gen quartiles=_n

	label define quart 1 "0-25" 2 "25-50" 3 "50-75" 4 "75-100"
	label values quartiles quart

	twoway  (rarea afam_lci afam_uci quartiles, color(gray%20))  ///
			(line afam_coef quartiles),  ///
			legend(order(2 "Coef." 1 "90% CI")) ///
			xlabel(1 "0-25" 2 "25-50" 3 "50-75" 4 "75-100", valuelabel) ///
			xtitle(Within CBSA quartile `race' distribution) ///
			ytitle(Relative Responses) ///
			scheme(plotplain) ///
			yscale(range(-0.20(.025)0.025)) ylabel(-0.20(0.025)0.025) 
	graph export views/quartiles_black_`race'.png, width(800) replace



	*save "stores/matrices/region_single_regression_RR.dta"  , replace
	restore
stop
}