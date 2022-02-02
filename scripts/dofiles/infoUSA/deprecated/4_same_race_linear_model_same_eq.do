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


gen Minority_choice=(AfAm_choice==1 | Hispanic_choice==1)

tab Minority_choice AfAm_choice
tab Minority_choice Hispanic_choice
tab Minority

matrix define RR_rel_full_sample=J(5,3,.)
matrix colnames  RR_rel_full_sample= coef lci uci  
matrix rownames  RR_rel_full_sample= Minority White AfAm Hisp White

reghdfe  same_race  Minority_choice Minority White_choice , absorb(i.gender i.education_level i.inquiry_order Address)     cl(CBSA_downtown) level(90) 

nlcom (RR_Minority   : (_b[_cons]  +  _b[Minority] )     / (_b[_cons] + _b[Minority_choice] +  _b[Minority])  )     ///
      (RR_White :  (_b[_cons]  )                  / (_b[_cons] + _b[White_choice])   ) ///
     , post



loc i=1
foreach j in Minority White{
    matrix RR_rel_full_sample[`i',1] =  _b[RR_`j'] 
    matrix RR_rel_full_sample[`i',2] =  _b[RR_`j'] - invttail(e(N),0.05)*_se[RR_`j']
    matrix RR_rel_full_sample[`i',3] =  _b[RR_`j'] + invttail(e(N),0.05)*_se[RR_`j']
    loc ++i
}


test RR_Minority==1     
test RR_White==1

* Full Sample Relative to same group
reghdfe  same_race  AfAm_choice Hispanic_choice AfAm Hispanic White_choice , absorb(i.gender i.education_level i.inquiry_order Address)     cl(CBSA_downtown) level(90) 

nlcom (RR_AfAm   : (_b[_cons]  +  _b[AfAm] )     / (_b[_cons] + _b[AfAm_choice] +  _b[AfAm])  )     ///
      (RR_Hisp    : (_b[_cons]  +  _b[Hispanic] ) / (_b[_cons] + _b[Hispanic_choice] +  _b[Hispanic])  ) ///
      (RR_White :  (_b[_cons]  )                  / (_b[_cons] + _b[White_choice])   ) ///
     , post




loc i=3
foreach j in AfAm  Hisp White{
    matrix RR_rel_full_sample[`i',1] =  _b[RR_`j'] 
    matrix RR_rel_full_sample[`i',2] =  _b[RR_`j'] - invttail(e(N),0.05)*_se[RR_`j']
    matrix RR_rel_full_sample[`i',3] =  _b[RR_`j'] + invttail(e(N),0.05)*_se[RR_`j']
    loc ++i
}
mat list RR_rel_full_sample      
    
preserve
clear
svmat2 RR_rel_full_sample,  names(col) rnames(race)

save "stores/matrices/temp/RR_rel_full_sample.dta"  , replace

restore    


*********************************************************************************
*********************************************************************************
*Above Median White Tracts/Below Median White Tracts
*Above Median Af American Tracts/Below Median Af American Tracts
*Above Median LatinX Tracts/Below Median LatinX Tracts
*Above Median Price/Below Median Price
*Central City/Suburbs
*********************************************************************************
/*********************************************************************************
gen rent_sqf=rent/sqft


rename  whiterentersshare2014_2018          w_rent_sh
rename  whiteshare2014_2018                 w_sh
rename  blackrentersshare2014_2018          bk_rent_sh
rename  blackshare2014_2018                 bk_sh
rename  hispanicrentersshare2014_2018       hisp_rent_sh
rename  hispanicshare2014_2018              hisp_sh
rename  medinc2014_2018                     medinc

gen min_sh=bk_sh+hisp_sh

gen diff_w_bk=kfr_white_pooled_mean-kfr_black_pooled_mean
gen diff_w_hisp=kfr_white_pooled_mean-kfr_hisp_pooled_mean



foreach vars in  w_rent_sh        ///
                 w_sh             ///
                 bk_rent_sh       ///
                 bk_sh            ///
                 hisp_rent_sh     ///
                 hisp_sh          ///
                 min_sh           ///
                 medinc           ///
                 rent_sqf         {


    egen med_`vars'= xtile(`vars'),  n(2)  by(CBSA)
    qui tabulate med_`vars', generate(median_`vars'_)

    * 1 below median
    * 2 above median


    foreach j in Minority_choice AfAm_choice Hispanic_choice White_choice Minority AfAm Hispanic White choice {
            gen `j'_above= `j'* median_`vars'_2
            gen `j'_below= `j'* median_`vars'_1
        
    }



        matrix define `vars'=J(10,3,.)
        matrix colnames `vars'= coef lci uci 
        matrix rownames  `vars'=  Minority_above White_above Minority_below White_below AfAm_above  Hisp_above White_above AfAm_below  Hisp_below White_below



        reghdfe  same_race Minority_choice_above ///
                   Minority_choice ///
                   Minority_above ///
                   Minority ///
                   White_choice_above  ///
                   White_choice ///
                   White_above /// 
                    , absorb(gender education_level inquiry_order CBSA)   cl(CBSA_downtown) level(90)


    nlcom (RR_Minority_above   :(_b[_cons] + _b[Minority_above] + _b[Minority]) /(_b[_cons] + _b[Minority_choice_above] + _b[Minority_choice] + _b[Minority_above] + _b[Minority]  )) ///
          (RR_Minority_below   :(_b[_cons]                  + _b[Minority]) /(_b[_cons]                         + _b[Minority_choice]                  + _b[Minority]  )) ///
          (RR_White_above  :(_b[_cons] + _b[White_above]                     ) / (_b[_cons]  + _b[White_above] + _b[White_choice])   ) ///
          (RR_White_below  :(_b[_cons]  )                                     / (_b[_cons] + _b[White_choice])   ) ///
             , post

        loc i=1
         foreach j in Minority_above White_above Minority_below White_below{
            matrix `vars'[`i',1] =  _b[RR_`j'] 
            matrix `vars'[`i',2] =  _b[RR_`j'] - invttail(e(N),0.05)*_se[RR_`j']
            matrix `vars'[`i',3] =  _b[RR_`j'] + invttail(e(N),0.05)*_se[RR_`j']
            loc ++i
        }




* regression
reghdfe  same_race AfAm_choice_above ///
                   AfAm_choice ///
                   AfAm_above ///
                   AfAm ///
                   Hispanic_choice_above ///
                   Hispanic_choice  ///
                   Hispanic_above ///
                   Hispanic ///
                   White_choice_above  ///
                   White_choice ///
                   White_above /// 
                    , absorb(gender education_level inquiry_order CBSA)   cl(CBSA_downtown) level(90)




nlcom (RR_AfAm_above   :(_b[_cons] + _b[AfAm_above] + _b[AfAm]) /(_b[_cons] + _b[AfAm_choice_above] + _b[AfAm_choice] + _b[AfAm_above] + _b[AfAm]  )) ///
      (RR_AfAm_below   :(_b[_cons]                  + _b[AfAm]) /(_b[_cons]                         + _b[AfAm_choice]                  + _b[AfAm]  )) ///
      (RR_Hisp_above   :(_b[_cons] + _b[Hispanic_above] + _b[Hispanic]) /(_b[_cons] + _b[Hispanic_choice_above] + _b[Hispanic_choice] + _b[Hispanic_above] + _b[Hispanic]  )) ///
      (RR_Hisp_below   :(_b[_cons]                      + _b[Hispanic]) /(_b[_cons]                             + _b[Hispanic_choice]                      + _b[Hispanic]  )) ///
      (RR_White_above  :(_b[_cons] + _b[White_above]                     ) / (_b[_cons]  + _b[White_above] + _b[White_choice])   ) ///
      (RR_White_below  :(_b[_cons]  )                                     / (_b[_cons] + _b[White_choice])   ) ///
         , post



        
        loc i 5
        foreach j in AfAm_above  Hisp_above White_above AfAm_below  Hisp_below White_below{
            matrix `vars'[`i',1] =  _b[RR_`j'] 
            matrix `vars'[`i',2] =  _b[RR_`j'] - invttail(e(N),0.05)*_se[RR_`j']
            matrix `vars'[`i',3] =  _b[RR_`j'] + invttail(e(N),0.05)*_se[RR_`j']
            loc ++i
        }
        mat list `vars'      
 
        
    
     *Save to a matrix
        preserve
        clear
        svmat2 `vars',  names(col) rnames(race)

        save "stores/matrices/temp/`vars'_above_below.dta"  , replace

        restore

 drop Minority_choice_above Minority_above  AfAm_choice_above AfAm_above  Hispanic_choice_above  Hispanic_above White_choice_above White_above choice_above ///
      Minority_choice_below Minority_below AfAm_choice_below AfAm_below  Hispanic_choice_below  Hispanic_below White_choice_below White_below  choice_below

 
}
*/


*Region
rename reg1 Midwest
rename reg2 Northeast
rename reg3 South
rename reg4 West


foreach vars in Midwest Northeast South West {
    foreach j in Minority_choice AfAm_choice Hispanic_choice White_choice Minority AfAm Hispanic White {
        gen `j'_`vars'=`j'*`vars'
    }
}


matrix define region=J(20,3,.)
matrix colnames region= coef lci uci 
matrix rownames  region= Minority_Midwest Minority_Northeast Minority_South Minority_West  White_Midwest White_Northeast White_South White_West AfAm_Midwest AfAm_Northeast AfAm_South AfAm_West Hisp_Midwest Hisp_Northeast Hisp_South Hisp_West White_Midwest White_Northeast White_South White_West


   
reghdfe  same_race  Minority_choice_Midwest ///
                    Minority_choice_Northeast ///
                    Minority_choice_South ///
                    Minority_choice ///
                    Minority_Midwest ///
                    Minority_Northeast ///
                    Minority_South ///
                    Minority ///
                    White_choice_Midwest  ///
                    White_choice_Northeast  ///
                    White_choice_South  ///
                    White_choice ///
                    White_Midwest  ///
                    White_Northeast  ///
                    White_South  ///
                     , absorb(i.gender i.education_level i.inquiry_order  CBSA)     cl(CBSA_downtown) level(90) 




nlcom (RR_Minority_Midwest       :(_b[_cons] + _b[Minority_Midwest] + _b[Minority]) /(_b[_cons] + _b[Minority_choice_Midwest] + _b[Minority_choice] + _b[Minority_Midwest] + _b[Minority]  )) ///
      (RR_Minority_Northeast     :(_b[_cons] + _b[Minority_Northeast] + _b[Minority]) /(_b[_cons] + _b[Minority_choice_Northeast] + _b[Minority_choice] + _b[Minority_Northeast] + _b[Minority]  )) ///
      (RR_Minority_South         :(_b[_cons] + _b[Minority_South] + _b[Minority]) /(_b[_cons] + _b[Minority_choice_South] + _b[Minority_choice] + _b[Minority_South] + _b[Minority]  )) ///
      (RR_Minority_West          :(_b[_cons]  + _b[Minority]) /(_b[_cons] + _b[Minority_choice] + _b[Minority]  )) ///
      (RR_White_Midwest      :(_b[_cons] + _b[White_Midwest] ) /(_b[_cons] + _b[White_choice_Midwest] + _b[White_choice] + _b[White_Midwest]   )) ///
      (RR_White_Northeast    :(_b[_cons] + _b[White_Northeast] ) /(_b[_cons] + _b[White_choice_Northeast] + _b[White_choice] + _b[White_Northeast]   )) ///
      (RR_White_South        :(_b[_cons] + _b[White_South]) /(_b[_cons] + _b[White_choice_South] + _b[White_choice] + _b[White_South]  )) ///
      (RR_White_West         :(_b[_cons]  ) /(_b[_cons] + _b[White_choice]   )) ///
         , post


        loc i 1
        foreach j in Minority_Midwest Minority_Northeast Minority_South Minority_West  White_Midwest White_Northeast White_South White_West{
            matrix region[`i',1] =  _b[RR_`j'] 
            matrix region[`i',2] =  _b[RR_`j'] - invttail(e(N),0.05)*_se[RR_`j']
            matrix region[`i',3] =  _b[RR_`j'] + invttail(e(N),0.05)*_se[RR_`j']
            loc ++i
        }
        mat list region      
        
        
reghdfe  same_race  AfAm_choice_Midwest ///
                    AfAm_choice_Northeast ///
                    AfAm_choice_South ///
                    AfAm_choice ///
                    AfAm_Midwest ///
                    AfAm_Northeast ///
                    AfAm_South ///
                    AfAm ///
                    Hispanic_choice_Midwest  ///
                    Hispanic_choice_Northeast  ///
                    Hispanic_choice_South  ///
                    Hispanic_choice ///
                    Hispanic_Midwest  ///
                    Hispanic_Northeast  ///
                    Hispanic_South  ///
                    Hispanic ///
                    White_choice_Midwest  ///
                    White_choice_Northeast  ///
                    White_choice_South  ///
                    White_choice ///
                    White_Midwest  ///
                    White_Northeast  ///
                    White_South  ///
                     , absorb(i.gender i.education_level i.inquiry_order  CBSA)     cl(CBSA_downtown) level(90) 





nlcom (RR_AfAm_Midwest       :(_b[_cons] + _b[AfAm_Midwest] + _b[AfAm]) /(_b[_cons] + _b[AfAm_choice_Midwest] + _b[AfAm_choice] + _b[AfAm_Midwest] + _b[AfAm]  )) ///
      (RR_AfAm_Northeast     :(_b[_cons] + _b[AfAm_Northeast] + _b[AfAm]) /(_b[_cons] + _b[AfAm_choice_Northeast] + _b[AfAm_choice] + _b[AfAm_Northeast] + _b[AfAm]  )) ///
      (RR_AfAm_South         :(_b[_cons] + _b[AfAm_South] + _b[AfAm]) /(_b[_cons] + _b[AfAm_choice_South] + _b[AfAm_choice] + _b[AfAm_South] + _b[AfAm]  )) ///
      (RR_AfAm_West          :(_b[_cons]  + _b[AfAm]) /(_b[_cons] + _b[AfAm_choice] + _b[AfAm]  )) ///
      (RR_Hispanic_Midwest   :(_b[_cons] + _b[Hispanic_Midwest] + _b[Hispanic]) /(_b[_cons] + _b[Hispanic_choice_Midwest] + _b[Hispanic_choice] + _b[Hispanic_Midwest] + _b[Hispanic]  )) ///
      (RR_Hispanic_Northeast :(_b[_cons] + _b[Hispanic_Northeast] + _b[Hispanic]) /(_b[_cons] + _b[Hispanic_choice_Northeast] + _b[Hispanic_choice] + _b[Hispanic_Northeast] + _b[Hispanic]  )) ///
      (RR_Hispanic_South     :(_b[_cons] + _b[Hispanic_South] + _b[Hispanic]) /(_b[_cons] + _b[Hispanic_choice_South] + _b[Hispanic_choice] + _b[Hispanic_South] + _b[Hispanic]  )) ///
      (RR_Hispanic_West      :(_b[_cons]  + _b[Hispanic]) /(_b[_cons] + _b[Hispanic_choice] + _b[Hispanic]  )) ///
      (RR_White_Midwest      :(_b[_cons] + _b[White_Midwest] ) /(_b[_cons] + _b[White_choice_Midwest] + _b[White_choice] + _b[White_Midwest]   )) ///
      (RR_White_Northeast    :(_b[_cons] + _b[White_Northeast] ) /(_b[_cons] + _b[White_choice_Northeast] + _b[White_choice] + _b[White_Northeast]   )) ///
      (RR_White_South        :(_b[_cons] + _b[White_South]) /(_b[_cons] + _b[White_choice_South] + _b[White_choice] + _b[White_South]  )) ///
      (RR_White_West         :(_b[_cons]  ) /(_b[_cons] + _b[White_choice]   )) ///
         , post


 

        

        loc i 9
        foreach j in AfAm_Midwest AfAm_Northeast AfAm_South AfAm_West Hispanic_Midwest Hispanic_Northeast Hispanic_South Hispanic_West White_Midwest White_Northeast White_South White_West{
            matrix region[`i',1] =  _b[RR_`j'] 
            matrix region[`i',2] =  _b[RR_`j'] - invttail(e(N),0.05)*_se[RR_`j']
            matrix region[`i',3] =  _b[RR_`j'] + invttail(e(N),0.05)*_se[RR_`j']
            loc ++i
        }
        mat list region      
        
        

        *Save to a matrix
        preserve
        clear
        svmat2 region ,  names(col) rnames(race)

        save "stores/matrices/temp/region.dta"  , replace

        restore




*Downtown/Suburb 
foreach vars in Downtown Suburb {
    foreach j in Minority_choice AfAm_choice Hispanic_choice White_choice  White {
        gen `j'_`vars'=`j'*`vars'
    }
}


matrix define downtown_suburb=J(10,3,.)
        matrix colnames downtown_suburb= coef lci uci 
        matrix rownames  downtown_suburb= Minority_Downtown Minority_Suburb  White_Downtown White_Suburb AfAm_Downtown AfAm_Suburb Hisp_Downtown Hisp_Suburb White_Downtown White_Suburb


        
reghdfe  same_race  Minority_choice_Downtown ///
                    Minority_choice ///
                    Minority_Downtown ///
                    Minority ///
                    White_choice_Downtown ///
                    White_choice ///
                    White_Downtown ///
                     , absorb(i.gender i.education_level i.inquiry_order CBSA)     cl(CBSA_downtown) level(90) 


nlcom (RR_Minority_Downtown   :(_b[_cons] + _b[Minority_Downtown] + _b[Minority]) /(_b[_cons] + _b[Minority_choice_Downtown] + _b[Minority_choice] + _b[Minority_Downtown] + _b[Minority]  )) ///
      (RR_Minority_Suburb     :(_b[_cons] + _b[Minority]) /(_b[_cons] + _b[Minority_choice]  + _b[Minority]  )) ///
      (RR_White_Downtown   :(_b[_cons] + _b[White_Downtown] ) /(_b[_cons] + _b[White_choice_Downtown] + _b[White_choice] + _b[White_Downtown]   )) ///
      (RR_White_Suburb     :(_b[_cons] ) /(_b[_cons] + _b[White_choice]    )) ///
         , post



        loc i 1
        foreach j in Minority_Downtown Minority_Suburb White_Downtown White_Suburb {
            matrix downtown_suburb[`i',1] =  _b[RR_`j'] 
            matrix downtown_suburb[`i',2] =  _b[RR_`j'] - invttail(e(N),0.05)*_se[RR_`j']
            matrix downtown_suburb[`i',3] =  _b[RR_`j'] + invttail(e(N),0.05)*_se[RR_`j']
            loc ++i
        }
        mat list downtown_suburb      
        


reghdfe  same_race  AfAm_choice_Downtown ///
                    AfAm_choice ///
                    AfAm_Downtown ///
                    AfAm ///
                    Hispanic_choice_Downtown ///
                    Hispanic_choice ///
                    Hispanic_Downtown ///
                    Hispanic ///
                    White_choice_Downtown ///
                    White_choice ///
                    White_Downtown ///
                     , absorb(i.gender i.education_level i.inquiry_order CBSA)     cl(CBSA_downtown) level(90) 




nlcom (RR_AfAm_Downtown   :(_b[_cons] + _b[AfAm_Downtown] + _b[AfAm]) /(_b[_cons] + _b[AfAm_choice_Downtown] + _b[AfAm_choice] + _b[AfAm_Downtown] + _b[AfAm]  )) ///
      (RR_AfAm_Suburb     :(_b[_cons] + _b[AfAm]) /(_b[_cons] + _b[AfAm_choice]  + _b[AfAm]  )) ///
      (RR_Hispanic_Downtown   :(_b[_cons] + _b[Hispanic_Downtown] + _b[Hispanic]) /(_b[_cons] + _b[Hispanic_choice_Downtown] + _b[Hispanic_choice] + _b[Hispanic_Downtown] + _b[Hispanic]  )) ///
      (RR_Hispanic_Suburb     :(_b[_cons] + _b[Hispanic]) /(_b[_cons] + _b[Hispanic_choice]  + _b[Hispanic]  )) ///
      (RR_White_Downtown   :(_b[_cons] + _b[White_Downtown] ) /(_b[_cons] + _b[White_choice_Downtown] + _b[White_choice] + _b[White_Downtown]   )) ///
      (RR_White_Suburb     :(_b[_cons] ) /(_b[_cons] + _b[White_choice]    )) ///
         , post



        

        loc i 5
        foreach j in AfAm_Downtown AfAm_Suburb Hispanic_Downtown Hispanic_Suburb White_Downtown White_Suburb {
            matrix downtown_suburb[`i',1] =  _b[RR_`j'] 
            matrix downtown_suburb[`i',2] =  _b[RR_`j'] - invttail(e(N),0.05)*_se[RR_`j']
            matrix downtown_suburb[`i',3] =  _b[RR_`j'] + invttail(e(N),0.05)*_se[RR_`j']
            loc ++i
        }
        mat list downtown_suburb      
        
        

        *Save to a matrix
        preserve
        clear
        svmat2 downtown_suburb ,  names(col) rnames(race)

        save "stores/matrices/temp/downtown_suburb.dta"  , replace

        restore



    

