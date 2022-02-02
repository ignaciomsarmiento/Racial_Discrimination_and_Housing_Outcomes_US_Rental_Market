/*
Replication files  
*/

clear all
set maxvar  32767

*Paths
*Working directory
cd "~/Dropbox/Research/cbsa_discrimination/github/Racial_Discrimination_US_Rental_Market/"

global path_tables  "views"
*global path_tables  "~/Dropbox/Apps/ShareLaTeX/Welfare Impact of Housing Discrimination/tables/cbsa_discrimination/prelim"


*Load Data
use "stores/matchedinquiries_opportunities_tract.dta"

gen StreetAddress=upper(Address)

merge n:1 StreetAddress Zip_Code using "stores/infoUSA_wru.dta"

save "stores/matchedinquiries_opportunities_tract_infoUSA_wru.dta",replace


drop if _merge==1

gen white_infoUSA=(race_renter=="white")
gen black_infoUSA=(race_renter=="black")
gen hispanic_infoUSA=(race_renter=="hispanic")

foreach i in White AfAm Hispanic{
    gen `i'_White=`i'*white_infoUSA 

    gen `i'_Black=`i'*black_infoUSA 

    gen `i'_Hispanic=`i'*hispanic_infoUSA 
    }


gen same_race=(race_renter=="white"  & race==1)
replace same_race=1 if race_renter=="black"  & race==3
replace same_race=1 if race_renter=="hispanic"  & race==2



foreach i in White AfAm Hispanic{
    gen `i'_same_race=`i'*same_race 
    gen `i'_choice=`i'*choice 

    }






*Full Sample
clogit same_race  AfAm_choice Hispanic_choice AfAm Hispanic i.gender i.education_level i.inquiry_order , group(Address)   cl(CBSA_downtown) level(90)  or


    
matrix define A1=J(4,4,.)
matrix colnames A1= coef lci uci pval 
*mean_lhs obs


loc i 1
foreach j in AfAm_choice Hispanic_choice AfAm Hispanic{
    matrix A1[`i',1] =  _b[`j'] 
    matrix A1[`i',2] =  _b[`j'] - invttail(e(N),0.05)*_se[`j']
    matrix A1[`i',3] =  _b[`j'] + invttail(e(N),0.05)*_se[`j']
    matrix A1[`i',4] =  (2 * ttail(e(N), abs(_b[`j']/_se[`j']))) 
    loc ++i
}
mat list A1

 *Save to a matrix
preserve
clear
svmat2 A1

save "stores/matrices/temp/same_race.dta"  , replace

restore
    

*********************************************************************************
*********************************************************************************
*Above Median White Tracts/Below Median White Tracts
*Above Median Af American Tracts/Below Median Af American Tracts
*Above Median LatinX Tracts/Below Median LatinX Tracts
*Above Median Price/Below Median Price
*Central City/Suburbs
*********************************************************************************
*********************************************************************************
gen rent_sqf=rent/sqft


rename  whiterentersshare2014_2018          w_rent_sh
rename  whiteshare2014_2018                 w_sh
rename  blackrentersshare2014_2018          bk_rent_sh
rename  blackshare2014_2018                 bk_sh
rename  hispanicrentersshare2014_2018       hisp_rent_sh
rename  hispanicshare2014_2018              hisp_sh
rename  medinc2014_2018                     medinc

gen diff_w_bk=kfr_white_pooled_mean-kfr_black_pooled_mean
gen diff_w_hisp=kfr_white_pooled_mean-kfr_hisp_pooled_mean



foreach vars in  w_rent_sh        ///
                 w_sh             ///
                 bk_rent_sh       ///
                 bk_sh            ///
                 hisp_rent_sh     ///
                 hisp_sh          ///
                 medinc           ///
                rent_sqf         ///
                diss_b_w         ///
                diss_h_w         ///
                diff_w_bk        ///
                diff_w_hisp {


egen median_`vars'= xtile(`vars'),  n(2)  
* 1 below median
* 2 above median

    forvalues k =1/2{

        clogit same_race  AfAm_choice Hispanic_choice AfAm Hispanic ///
                i.gender i.education_level i.inquiry_order  if  median_`vars'==`k' , group(Address)   cl(CBSA_downtown) level(90)            


        matrix define `vars'_`k'=J(4,4,.)
        matrix colnames `vars'_`k'= coef lci uci pval
        loc i 1
        foreach j in  AfAm_choice Hispanic_choice AfAm Hispanic  {
            matrix `vars'_`k'[`i',1] =  _b[`j'] 
            matrix `vars'_`k'[`i',2] =  _b[`j'] - invttail(e(N),0.05)*_se[`j']
            matrix `vars'_`k'[`i',3] =  _b[`j'] + invttail(e(N),0.05)*_se[`j']
            matrix `vars'_`k'[`i',4] =  (2 * ttail(e(N), abs(_b[`j']/_se[`j']))) 
            loc ++i
        }
        mat list `vars'_`k'
    
     *Save to a matrix
        preserve
        clear
        svmat2 `vars'_`k'

        save "stores/matrices/temp/`vars'_`k'.dta"  , replace

        restore
    }
}


*Central City/Suburbs

rename reg1 Midwest
rename reg2 Northeast
rename reg3 South
rename reg4 West


foreach vars in Downtown Suburb Midwest Northeast South West {
        
        clogit same_race  AfAm_choice Hispanic_choice AfAm Hispanic ///
                i.gender i.education_level i.inquiry_order  if  `vars'==1 , group(Address)   cl(CBSA_downtown) level(90)            


        matrix define `vars'=J(4,4,.)
        matrix colnames `vars'= coef lci uci pval
        loc i 1
        foreach j in  AfAm_choice Hispanic_choice AfAm Hispanic  {
            matrix `vars'[`i',1] =  _b[`j'] 
            matrix `vars'[`i',2] =  _b[`j'] - invttail(e(N),0.05)*_se[`j']
            matrix `vars'[`i',3] =  _b[`j'] + invttail(e(N),0.05)*_se[`j']
            matrix `vars'[`i',4] =  (2 * ttail(e(N), abs(_b[`j']/_se[`j']))) 
            loc ++i
        }
        mat list `vars'

        *Save to a matrix
        preserve
        clear
        svmat2 `vars' 

        save "stores/matrices/temp/`vars'.dta"  , replace

        restore
}




    

