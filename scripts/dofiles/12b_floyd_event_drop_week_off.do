/*
Replication files  
*/

clear all
set matsize 11000

*Paths
*Working directory
cd "~/Dropbox/Research/cbsa_discrimination/github/Racial_Discrimination_US_Rental_Market/"

global path_tables  "views"
*global path_tables  "~/Dropbox/Apps/ShareLaTeX/Welfare Impact of Housing Discrimination/tables/cbsa_discrimination/prelim"


*Load Data
use "stores/matchedinquiries.dta"



tab Round
tabulate Round, generate(week) 
drop dir
encode Address, gen(dir)

foreach race in Minority Hispanic AfAm {
	forvalues i = 1/35{
		gen `race'_week_`i'=`race'*week`i'
	}
}	

tab Round after_floyd


drop if week21==1


 reghdfe choice  AfAm_week_1-AfAm_week_19 AfAm_week_22-AfAm_week_35 ///
        Hispanic_week_1-Hispanic_week_19 Hispanic_week_22-Hispanic_week_35,  absorb(White gender education_level inquiry_order dir#Round)      
	
forvalues i = 1/35{
  qui sum choice if White==1 & week`i'==1
  loc r_white_`i'=r(mean)
}


        matrix define AfAm=J(35,3,.)
        matrix define Hispanic=J(35,3,.)
        matrix colnames AfAm = "AfAm_lci" "AfAm_RR" "AfAm_uci"
        matrix colnames Hispanic = "Hispanic_lci" "Hispanic_RR" "Hispanic_uci"

        foreach race in  AfAm Hispanic  { 
		forvalues i = 1/19{
			 matrix `race'[`i',1] =  _b[`race'_week_`i'] - invttail(e(N),0.05)*_se[`race'_week_`i']
		     matrix `race'[`i',2] =  _b[`race'_week_`i'] 
		     matrix `race'[`i',3] =  _b[`race'_week_`i'] + invttail(e(N),0.05)*_se[`race'_week_`i']
		}
             matrix `race'[20,1] =  0
             matrix `race'[20,2] =  0
             matrix `race'[20,3] =  0
		 	 matrix `race'[21,1] =  0
		     matrix `race'[21,2] =  0
		     matrix `race'[21,3] =  0
		forvalues i = 22/35{
			 matrix `race'[`i',1] =  _b[`race'_week_`i'] - invttail(e(N),0.05)*_se[`race'_week_`i']
		     matrix `race'[`i',2] =  _b[`race'_week_`i'] 
		     matrix `race'[`i',3] =  _b[`race'_week_`i'] + invttail(e(N),0.05)*_se[`race'_week_`i']
		}
		
        /*
		preserve
		clear
		svmat `race'
		rename `race'1 M1
		rename `race'2 M2
		rename `race'3 M3
		gen n=_n
		replace n=n-22 if n<22
		replace n=n-22 if n>21
		
		twoway scatter M2 n, msymbol(+) mcolor(black)  || ///
								rcap M1 M3 n, lcolor(black) ///
								 yscale(range(-0.20(.1)0.1)) ylabel(-0.20(0.1)0.1)  ///
								 legend(label(1 "Coefficient") label(2 "90% CI") position(4) ring(0) ///
								 region(lcolor(none) fcolor(none))) xtitle("Week Relative to G. Floyd Homicide") ytitle("Coefficient") ylabel(,nogrid)   ///
								 graphregion(color(white)) plotregion(lstyle(solid))  ///
								 yline(0, lcolor(dknavy) lpatter(dash_dot))  ///
								 xscale(range(-20(1)15)) xlabel(-20(5)15)  /// ///
								 xline(0, lcolor(dknavy) lpatter(dashed))
		graph export "views/floyd_event_`race'_RR.pdf", as(pdf) replace
		
	restore
    */
		}





************************************************************************************************
*Consolidate matrices and export
************************************************************************************************
mat def Res=AfAm,Hispanic



preserve
clear
svmat2 Res,  names(col)  
gen n=_n

save "stores/matrices/event_floyd.dta"  , replace
restore

