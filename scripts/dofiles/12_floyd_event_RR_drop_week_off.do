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


replace Round = Round-3  if Round>19
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

*(RR_AfAm_week_22 : _b[AfAm_week_22] /`r_white_22') ///
*(RR_Hispanic_week_22 : _b[Hispanic_week_22] /`r_white_22') ///
*(RR_AfAm_week_20 : _b[AfAm_week_20] /`r_white_20') ///
*(RR_Hispanic_week_20 : _b[Hispanic_week_20] /`r_white_20') ///

nlcom   (RR_AfAm_week_1  : _b[AfAm_week_1]  /`r_white_1') ///
        (RR_AfAm_week_2  : _b[AfAm_week_2]  /`r_white_2') ///
        (RR_AfAm_week_3  : _b[AfAm_week_3]  /`r_white_3') ///
        (RR_AfAm_week_4  : _b[AfAm_week_4]  /`r_white_4') ///
        (RR_AfAm_week_5  : _b[AfAm_week_5]  /`r_white_5') ///
        (RR_AfAm_week_6  : _b[AfAm_week_6]  /`r_white_6') ///
        (RR_AfAm_week_7  : _b[AfAm_week_7]  /`r_white_7') ///
        (RR_AfAm_week_8  : _b[AfAm_week_8]  /`r_white_8') ///
        (RR_AfAm_week_9  : _b[AfAm_week_9]  /`r_white_9') ///
        (RR_AfAm_week_10 : _b[AfAm_week_10] /`r_white_10') ///
        (RR_AfAm_week_11 : _b[AfAm_week_11] /`r_white_11') ///
        (RR_AfAm_week_12 : _b[AfAm_week_12] /`r_white_12') ///
        (RR_AfAm_week_13 : _b[AfAm_week_13] /`r_white_13') ///
        (RR_AfAm_week_14 : _b[AfAm_week_14] /`r_white_14') ///
        (RR_AfAm_week_15 : _b[AfAm_week_15] /`r_white_15') ///
        (RR_AfAm_week_16 : _b[AfAm_week_16] /`r_white_16') ///
        (RR_AfAm_week_17 : _b[AfAm_week_17] /`r_white_17') ///
        (RR_AfAm_week_18 : _b[AfAm_week_18] /`r_white_18') ///
        (RR_AfAm_week_19 : _b[AfAm_week_19] /`r_white_19') ///
        (RR_AfAm_week_22 : _b[AfAm_week_22] /`r_white_22') ///
        (RR_AfAm_week_23 : _b[AfAm_week_23] /`r_white_23') ///
        (RR_AfAm_week_24 : _b[AfAm_week_24] /`r_white_24') ///
        (RR_AfAm_week_25 : _b[AfAm_week_25] /`r_white_25') ///
        (RR_AfAm_week_26 : _b[AfAm_week_26] /`r_white_26') ///
        (RR_AfAm_week_27 : _b[AfAm_week_27] /`r_white_27') ///
        (RR_AfAm_week_28 : _b[AfAm_week_28] /`r_white_28') ///
        (RR_AfAm_week_29 : _b[AfAm_week_29] /`r_white_29') ///
        (RR_AfAm_week_30 : _b[AfAm_week_30] /`r_white_30') ///
        (RR_AfAm_week_31 : _b[AfAm_week_31] /`r_white_31') ///
        (RR_AfAm_week_32 : _b[AfAm_week_32] /`r_white_32') ///
        (RR_AfAm_week_33 : _b[AfAm_week_33] /`r_white_33') ///
        (RR_AfAm_week_34 : _b[AfAm_week_34] /`r_white_34') ///
        (RR_AfAm_week_35 : _b[AfAm_week_35] /`r_white_35') ///
        (RR_Hispanic_week_1  : _b[Hispanic_week_1]  /`r_white_1') ///
        (RR_Hispanic_week_2  : _b[Hispanic_week_2]  /`r_white_2') ///
        (RR_Hispanic_week_3  : _b[Hispanic_week_3]  /`r_white_3') ///
        (RR_Hispanic_week_4  : _b[Hispanic_week_4]  /`r_white_4') ///
        (RR_Hispanic_week_5  : _b[Hispanic_week_5]  /`r_white_5') ///
        (RR_Hispanic_week_6  : _b[Hispanic_week_6]  /`r_white_6') ///
        (RR_Hispanic_week_7  : _b[Hispanic_week_7]  /`r_white_7') ///
        (RR_Hispanic_week_8  : _b[Hispanic_week_8]  /`r_white_8') ///
        (RR_Hispanic_week_9  : _b[Hispanic_week_9]  /`r_white_9') ///
        (RR_Hispanic_week_10 : _b[Hispanic_week_10] /`r_white_10') ///
        (RR_Hispanic_week_11 : _b[Hispanic_week_11] /`r_white_11') ///
        (RR_Hispanic_week_12 : _b[Hispanic_week_12] /`r_white_12') ///
        (RR_Hispanic_week_13 : _b[Hispanic_week_13] /`r_white_13') ///
        (RR_Hispanic_week_14 : _b[Hispanic_week_14] /`r_white_14') ///
        (RR_Hispanic_week_15 : _b[Hispanic_week_15] /`r_white_15') ///
        (RR_Hispanic_week_16 : _b[Hispanic_week_16] /`r_white_16') ///
        (RR_Hispanic_week_17 : _b[Hispanic_week_17] /`r_white_17') ///
        (RR_Hispanic_week_18 : _b[Hispanic_week_18] /`r_white_18') ///
        (RR_Hispanic_week_19 : _b[Hispanic_week_19] /`r_white_19') ///
        (RR_Hispanic_week_22 : _b[Hispanic_week_22] /`r_white_22') ///
        (RR_Hispanic_week_23 : _b[Hispanic_week_23] /`r_white_23') ///
        (RR_Hispanic_week_24 : _b[Hispanic_week_24] /`r_white_24') ///
        (RR_Hispanic_week_25 : _b[Hispanic_week_25] /`r_white_25') ///
        (RR_Hispanic_week_26 : _b[Hispanic_week_26] /`r_white_26') ///
        (RR_Hispanic_week_27 : _b[Hispanic_week_27] /`r_white_27') ///
        (RR_Hispanic_week_28 : _b[Hispanic_week_28] /`r_white_28') ///
        (RR_Hispanic_week_29 : _b[Hispanic_week_29] /`r_white_29') ///
        (RR_Hispanic_week_30 : _b[Hispanic_week_30] /`r_white_30') ///
        (RR_Hispanic_week_31 : _b[Hispanic_week_31] /`r_white_31') ///
        (RR_Hispanic_week_32 : _b[Hispanic_week_32] /`r_white_32') ///
        (RR_Hispanic_week_33 : _b[Hispanic_week_33] /`r_white_33') ///
        (RR_Hispanic_week_34 : _b[Hispanic_week_34] /`r_white_34') ///
        (RR_Hispanic_week_35 : _b[Hispanic_week_35] /`r_white_35') ///
        , post


        matrix define AfAm=J(35,3,.)
        matrix define Hispanic=J(35,3,.)
        matrix colnames AfAm = "AfAm_lci" "AfAm_RR" "AfAm_uci"
        matrix colnames Hispanic = "Hispanic_lci" "Hispanic_RR" "Hispanic_uci"

        foreach race in  AfAm Hispanic  { 
		forvalues i = 1/19{
			 matrix `race'[`i',1] =  _b[RR_`race'_week_`i'] - invttail(e(N),0.05)*_se[RR_`race'_week_`i']
		     matrix `race'[`i',2] =  _b[RR_`race'_week_`i'] 
		     matrix `race'[`i',3] =  _b[RR_`race'_week_`i'] + invttail(e(N),0.05)*_se[RR_`race'_week_`i']
		}
             matrix `race'[20,1] =  0
             matrix `race'[20,2] =  0
             matrix `race'[20,3] =  0
		 	 matrix `race'[21,1] =  0
		     matrix `race'[21,2] =  0
		     matrix `race'[21,3] =  0
		forvalues i = 22/35{
			 matrix `race'[`i',1] =  _b[RR_`race'_week_`i'] - invttail(e(N),0.05)*_se[RR_`race'_week_`i']
		     matrix `race'[`i',2] =  _b[RR_`race'_week_`i'] 
		     matrix `race'[`i',3] =  _b[RR_`race'_week_`i'] + invttail(e(N),0.05)*_se[RR_`race'_week_`i']
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

save "stores/matrices/event_floyd_RR.dta"  , replace
restore

