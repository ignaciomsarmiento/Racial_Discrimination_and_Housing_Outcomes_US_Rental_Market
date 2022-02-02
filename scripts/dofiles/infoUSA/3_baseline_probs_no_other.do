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

drop if race_renter=="other"

foreach i in White AfAm Hispanic{
    gen `i'_same_race=`i'*same_race 
    gen `i'_choice=`i'*choice 

    }

gen Minority_choice=(AfAm_choice==1 | Hispanic_choice==1)
 


reghdfe  same_race   choice AfAm Hispanic, absorb(i.gender i.education_level i.inquiry_order Address)     cl(CBSA_downtown) level(90) 
estadd local gender = "Yes", replace 
estadd local edu = "Yes", replace 
estadd local order = "Yes", replace 
estadd scalar obs = `e(N)'
estimates store model0

reghdfe  same_race   choice, absorb(i.gender i.education_level i.inquiry_order Address)     cl(CBSA_downtown) level(90) 
estadd local gender = "Yes", replace 
estadd local edu = "Yes", replace 
estadd local order = "Yes", replace 
estadd scalar obs = `e(N)'
estimates store model1

di log(_b[choice])
di log(_b[_cons]  +  _b[choice] ) 
di log(_b[_cons]  )     

nlcom (RR_choice   : (_b[_cons]  )     / (_b[_cons] + _b[choice] )  )    

nlcom (RR_choice   : log(_b[_cons]  )     / (_b[_cons] + _b[choice] )  )    

reghdfe  same_race  Minority_choice Minority White_choice , absorb(i.gender i.education_level i.inquiry_order Address)     cl(CBSA_downtown) level(90) 
estadd local gender = "Yes", replace 
estadd local edu = "Yes", replace 
estadd local order = "Yes", replace 
estadd scalar obs = `e(N)'
estimates store model2


di log(_b[_cons]  )     
di log( _b[Minority] )     
di log(_b[Minority_choice] )            
di log(_b[White_choice])  

di log(_b[_cons]  +  _b[Minority] )     
di log(_b[_cons]  +  _b[Minority] )     
di log(_b[_cons] + _b[Minority_choice] +  _b[Minority])  
di log(_b[_cons]  )                  
di log(_b[_cons] + _b[White_choice])  



nlcom (RR_Minority   : (_b[_cons]  +  _b[Minority] )     / (_b[_cons] + _b[Minority_choice] +  _b[Minority])  )     ///
      (RR_White :  (_b[_cons]  )                  / (_b[_cons] + _b[White_choice])   ) 

nlcom (RR_Minority   : log(_b[_cons]  +  _b[Minority] )     / (_b[_cons] + _b[Minority_choice] +  _b[Minority])  )     ///
      (RR_White :  log(_b[_cons]  )                  / (_b[_cons] + _b[White_choice])   ) 


reghdfe  same_race  AfAm_choice Hispanic_choice AfAm Hispanic White_choice , absorb(i.gender i.education_level i.inquiry_order Address)     cl(CBSA_downtown) level(90) 
estadd local gender = "Yes", replace 
estadd local edu = "Yes", replace 
estadd local order = "Yes", replace 
estadd scalar obs = `e(N)'
estimates store model3


di(_b[_cons]  +  _b[AfAm] )    
di (_b[_cons] + _b[AfAm_choice] +  _b[AfAm])  
di(_b[_cons]  +  _b[AfAm] )     / (_b[_cons] + _b[AfAm_choice] +  _b[AfAm]) 

di  (_b[_cons]  +  _b[Hispanic] ) 
di  (_b[_cons] + _b[Hispanic_choice] +  _b[Hispanic]) 
di  (_b[_cons]  +  _b[Hispanic] ) / (_b[_cons] + _b[Hispanic_choice] +  _b[Hispanic]) 

di (_b[_cons]  )                  
di (_b[_cons] + _b[White_choice]) 
di (_b[_cons]  )                  / (_b[_cons] + _b[White_choice])   
  

nlcom (RR_AfAm   : (_b[_cons]  +  _b[AfAm] )     / (_b[_cons] + _b[AfAm_choice] +  _b[AfAm])  )     ///
      (RR_Hisp    : (_b[_cons]  +  _b[Hispanic] ) / (_b[_cons] + _b[Hispanic_choice] +  _b[Hispanic])  ) ///
      (RR_White :  (_b[_cons]  )                  / (_b[_cons] + _b[White_choice])   )   


#delimit ; 
esttab model0
        model1 
       model2 
       model3
       using "${path_tables}/baseline_outcomes_no_other.tex", 
       style(tex) 
       cells(b(star fmt(4)) se(par fmt(4)))    
       label 
       substitute(\_ _)
       noobs
       mlabels(,none)  
       nonumbers 
       collabels(,none) 
       eqlabels(,none)
        varlabels( Minority "Minority" 
                 AfAm "African American"
                 Hispanic "Hispanic/LatinX"
                 choice "Received a Response"
                 Minority_choice "Minority x Response" 
                 AfAm_choice "African American x Response" 
                 Hispanic_choice "Hispanic/LatinX x Response" 
                 White_choice "White x Response" 
                 _cons "Constant") 
       starl(* 0.1 ** 0.05 *** 0.01) 
       stats(gender 
             edu 
             order
             obs, fmt( 0 0 0 %9.0gc)
             labels( "\hline Gender" 
                   "Education Level" 
                   "Inquiry Order"
                   "\hline Observations"
                   )) 
         prehead(      
         \begin{table}[H]
\footnotesize \centering
\begin{threeparttable}
\captionsetup{justification=centering}
  \caption{Estimates of Baseline Renter Outcomes}
      \label{tab:baseline_outcomes}
\begin{tabular}{@{\extracolsep{5pt}}lcccc}
\\[-1.8ex]\hline
\hline \\[-1.8ex]
 & \multicolumn{3}{c}{\textit{Dependent variable: {\it Same Race}}} \\
 \cline{2-5} \\
\\[-1.8ex] 
& (1) & (2) & (3) & (4) \\ 
\\[-1.8ex] 
\hline \\[-1.8ex]

       )
       posthead() 
     postfoot(
            \\[-1.8ex]\hline 
      \hline \\[-1.8ex] 
      \end{tabular} 

      \begin{tablenotes}[scriptsize,flushleft] \scriptsize
      \item Notes: 
      \item * Significant at 10\% level, ** significant at 5\% level, *** significant at 1\% level.
      \end{tablenotes}
      \end{threeparttable}
      \end{table}
      )
       
        replace;
#delimit cr



