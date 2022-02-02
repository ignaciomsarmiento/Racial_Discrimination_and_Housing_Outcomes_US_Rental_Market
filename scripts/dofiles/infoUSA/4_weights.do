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

gen Minority_choice=(AfAm_choice==1 | Hispanic_choice==1)

save "stores/matchedinquiries_opportunities_tract_infoUSA_wru_test.dta",replace
*----------------------------------------------------------------------------------------------------------------------------------
*----------------------------------------------------------------------------------------------------------------------------------
* Regressions
*----------------------------------------------------------------------------------------------------------------------------------
*----------------------------------------------------------------------------------------------------------------------------------
matrix define RR_rel_full_sample=J(5,3,.)
matrix colnames  RR_rel_full_sample= coef lci uci  
matrix rownames  RR_rel_full_sample= choice min afam hisp white  


reghdfe  same_race   choice, absorb(i.gender i.education_level i.inquiry_order Address)     cl(CBSA_downtown) level(90) 

di (_b[_cons] ) 
di (_b[choice] )
di (_b[_cons] + _b[choice] ) 

nlcom (RR_choice   : (_b[_cons]  )     / (_b[_cons] + _b[choice] )  )     ///
     , post


reghdfe  same_race  Minority_choice  White_choice Minority , absorb(i.gender i.education_level i.inquiry_order Address)     cl(CBSA_downtown) level(90) 

di (_b[_cons]  +  _b[Minority] )     
di (_b[_cons] + _b[Minority_choice] +  _b[Minority])  
di (_b[_cons]  )                  
di (_b[_cons] + _b[White_choice])  


nlcom (RR_Minority   : (_b[_cons]  +  _b[Minority] )     / (_b[_cons] + _b[Minority_choice] +  _b[Minority])  )     ///
      (RR_White :  (_b[_cons]  )                  / (_b[_cons] + _b[White_choice])   ) ///
     , post



loc i=2
loc j Minority
matrix RR_rel_full_sample[`i',1] =  _b[RR_`j'] 
matrix RR_rel_full_sample[`i',2] =  _b[RR_`j'] - invttail(e(N),0.05)*_se[RR_`j']
matrix RR_rel_full_sample[`i',3] =  _b[RR_`j'] + invttail(e(N),0.05)*_se[RR_`j']

