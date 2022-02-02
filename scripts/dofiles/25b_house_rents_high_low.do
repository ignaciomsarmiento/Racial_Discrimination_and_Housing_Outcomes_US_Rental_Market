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

use "stores/matchedinquiries.dta"

loc quartiles=2



egen quartile_rent_Cbsa=xtile(rent), n(`quartiles') by(CBSA)
tabulate quartile_rent_Cbsa, generate(q_rent) 


foreach race in  Hispanic AfAm {
  forvalues i = 1/`quartiles'{
    gen `race'_rent`i'=`race'*q_rent`i'
    
  }
}


forvalues i = 1/`quartiles'{
  sum choice if White==1 & q_rent`i'==1
  loc mean`i' `r(mean)'
}



reghdfe choice   AfAm_rent*   Hispanic_rent* , absorb(gender education_level inquiry_order Address)  cl(CBSA_downtown) level(90) nocons


nlcom 	  (RR_AfAm_rent1: _b[AfAm_rent1]/`mean1') ///
          (RR_AfAm_rent2: _b[AfAm_rent2]/`mean2') ///
          (RR_Hispanic_rent1: _b[Hispanic_rent1]/`mean1') ///
          (RR_Hispanic_rent2: _b[Hispanic_rent2]/`mean2') ///
     , post




	* Results into a Matrix  
	matrix define A=J(4,3,.)
	matrix colnames A = afam_lci afam_coef afam_uci 


	forvalues i = 1/`quartiles'{
	  matrix A[`i',1] =  _b[RR_AfAm_rent`i'] - invttail(e(N),0.05)*_se[RR_AfAm_rent`i']
	  matrix A[`i',2] =  _b[RR_AfAm_rent`i']
	  matrix A[`i',3] =  _b[RR_AfAm_rent`i'] + invttail(e(N),0.05)*_se[RR_AfAm_rent`i']
	}



	preserve
	clear
	svmat2 A, names(col)
	gen quartiles=_n

	label define quart 1 "Low" 2 "High"
	label values quartiles quart

	
	twoway scatter afam_coef quartiles, msymbol(+) mcolor(black)  || rcap afam_lci afam_uci quartiles, ///
			legend(order(2 "Coef." 1 "90% CI")) ///
			xlabel(1 "Low" 2 "High", valuelabel) ///
			xtitle(Within CBSA quartile rent distribution) ///
			ytitle(Relative Responses) ///
			scheme(plotplain) ///
			yscale(range(-0.20(.025)0.025)) ylabel(-0.20(0.025)0.025) 
			xscale(range(1(1)2))
	graph export views/quartiles_rent_black_high_low.png, width(800) replace



	restore



* Results into a Matrix  
	matrix define H=J(4,3,.)
	matrix colnames H = hisp_lci hisp_coef hisp_uci 


	forvalues i = 1/`quartiles'{
	  matrix H[`i',1] =  _b[RR_Hispanic_rent`i'] - invttail(e(N),0.05)*_se[RR_Hispanic_rent`i']
	  matrix H[`i',2] =  _b[RR_Hispanic_rent`i']
	  matrix H[`i',3] =  _b[RR_Hispanic_rent`i'] + invttail(e(N),0.05)*_se[RR_Hispanic_rent`i']
	}



	preserve
	clear
	svmat2 H, names(col)
	gen quartiles=_n

	label define quart 1 "Low" 2 "High"
	label values quartiles quart

	twoway scatter hisp_coef quartiles, msymbol(+) mcolor(black)  || rcap hisp_lci hisp_uci quartiles, ///
			legend(order(2 "Coef." 1 "90% CI")) ///
			xlabel(1 "Low" 2 "High", valuelabel) ///
			xtitle(Within CBSA quartile rent distribution) ///
			ytitle(Relative Responses) ///
			scheme(plotplain) ///
			yscale(range(-0.20(.025)0.025)) ylabel(-0.20(0.025)0.025) 
	graph export views/quartiles_rent_hisp_high_low.png, width(800) replace



	restore
