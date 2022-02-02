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

loc quartiles=4



egen quartile_rent_Cbsa=xtile(rent), n(`quartiles') by(CBSA)
tabulate quartile_rent_Cbsa, generate(q_rent) 


foreach race in  Hispanic AfAm {
  forvalues i = 1/4{
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
          (RR_AfAm_rent3: _b[AfAm_rent3]/`mean3') ///
          (RR_AfAm_rent4: _b[AfAm_rent4]/`mean4') ///
          (RR_Hispanic_rent1: _b[Hispanic_rent1]/`mean1') ///
          (RR_Hispanic_rent2: _b[Hispanic_rent2]/`mean2') ///
          (RR_Hispanic_rent3: _b[Hispanic_rent3]/`mean3') ///
          (RR_Hispanic_rent4: _b[Hispanic_rent4]/`mean4') ///
     , post




	* Results into a Matrix  
	matrix define A=J(4,3,.)
	matrix colnames A = afam_lci afam_coef afam_uci 


	forvalues i = 1/4{
	  matrix A[`i',1] =  _b[RR_AfAm_rent`i'] - invttail(e(N),0.05)*_se[RR_AfAm_rent`i']
	  matrix A[`i',2] =  _b[RR_AfAm_rent`i']
	  matrix A[`i',3] =  _b[RR_AfAm_rent`i'] + invttail(e(N),0.05)*_se[RR_AfAm_rent`i']
	}



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
			xtitle(Within CBSA quartile rent distribution) ///
			ytitle(Relative Responses) ///
			scheme(plotplain) ///
			yscale(range(-0.20(.025)0.025)) ylabel(-0.20(0.025)0.025) 
	graph export views/quartiles_rent_black.png, width(800) replace



	restore



* Results into a Matrix  
	matrix define H=J(4,3,.)
	matrix colnames H = hisp_lci hisp_coef hisp_uci 


	forvalues i = 1/4{
	  matrix H[`i',1] =  _b[RR_Hispanic_rent`i'] - invttail(e(N),0.05)*_se[RR_Hispanic_rent`i']
	  matrix H[`i',2] =  _b[RR_Hispanic_rent`i']
	  matrix H[`i',3] =  _b[RR_Hispanic_rent`i'] + invttail(e(N),0.05)*_se[RR_Hispanic_rent`i']
	}



	preserve
	clear
	svmat2 H, names(col)
	gen quartiles=_n

	label define quart 1 "0-25" 2 "25-50" 3 "50-75" 4 "75-100"
	label values quartiles quart

	twoway  (rarea hisp_lci hisp_uci quartiles, color(gray%20))  ///
			(line hisp_coef quartiles),  ///
			legend(order(2 "Coef." 1 "90% CI")) ///
			xlabel(1 "0-25" 2 "25-50" 3 "50-75" 4 "75-100", valuelabel) ///
			xtitle(Within CBSA quartile rent distribution) ///
			ytitle(Relative Responses) ///
			scheme(plotplain) ///
			yscale(range(-0.20(.025)0.025)) ylabel(-0.20(0.025)0.025) 
	graph export views/quartiles_rent_hisp.png, width(800) replace



	restore
