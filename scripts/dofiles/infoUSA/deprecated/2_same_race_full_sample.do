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
clogit same_race  AfAm_choice Hispanic_choice AfAm Hispanic  i.gender i.education_level i.inquiry_order , group(Address)   cl(CBSA_downtown) level(90)  or
sum choice if White==1 
estadd scalar responsewhite = r(mean), replace 
estadd local gender = "Yes", replace 
estadd local edu = "Yes", replace 
estadd local order = "Yes", replace 
estadd local obs = e(N)+e(N_drop)
estadd local diff_response=e(N)/(e(N)+e(N_drop))
estadd local listings= (e(N)+e(N_drop))/3
estimates store model1


*

*Full Sample
clogit same_race  AfAm_choice Hispanic_choice AfAm Hispanic White_choice i.gender i.education_level i.inquiry_order , group(Address)   cl(CBSA_downtown) level(90)  or
sum choice if White==1 
estadd scalar responsewhite = r(mean), replace 
estadd local gender = "Yes", replace 
estadd local edu = "Yes", replace 
estadd local order = "Yes", replace 
estadd local obs = e(N)+e(N_drop)
estadd local diff_response=e(N)/(e(N)+e(N_drop))
estadd local listings= (e(N)+e(N_drop))/3
estimates store model2

reghdfe  same_race  AfAm_choice Hispanic_choice AfAm Hispanic White_choice , absorb(i.gender i.education_level i.inquiry_order Address)     cl(CBSA_downtown) level(90) 
sum choice if White==1 
estadd scalar responsewhite = r(mean), replace 
estadd local gender = "Yes", replace 
estadd local edu = "Yes", replace 
estadd local order = "Yes", replace 
estadd local obs = e(N)
estimates store model3

#delimit ; 
esttab model1
        model2
        model3
       using "${path_tables}/same_race_full_Sample.tex",
       style(tex) 
       eform(1 1 0)
       cells(b(star fmt(4)) ci(par fmt(4) par(( , )))  ) 
       label 
       stats(responsewhite
             gender 
             edu 
             order
             obs
             listings
             diff_response, fmt(2 0 0 0 %9.0gc %9.0gc 2)
             labels(" Mean Response (White)"
                   "\hline Gender" 
                   "Education Level" 
                   "Inquiry Order"
                   "\hline Observations"
                   "Listings"
                   "\% w. diff. response"
                   )) 
       mlabels( ,none)  
       nonumbers
       collabels(,none) 
       eqlabels(,none)
       varlabels(AfAm_choice     "African American Identity x Response"
                  Hispanic_choice "Hispanic/LatinX Identity x Response"
                   AfAm     "African American Identity"
                    Hispanic  "Hispanic/LatinX Identity"
                    choice  "Response"
                    White_choice  "White x Response"
                    _cons "Constant"
                   
                )
       keep( AfAm_choice AfAm   Hispanic_choice Hispanic White_choice choice _cons)              
       order( AfAm_choice AfAm   Hispanic_choice Hispanic  White_choice choice _cons)                            
       starl(* 0.1 ** 0.05 *** 0.01)   
       level(90) 
       prehead( 

\begin{table}[H]
\footnotesize \centering
\begin{threeparttable}
\captionsetup{justification=centering}
  \caption{}
  \label{tab:infousasamerace}

\begin{tabular}{@{\extracolsep{5pt}} lccc} 
\\[-1.8ex]\hline 
\hline \\[-1.8ex] 
& \multicolumn{3}{c}{\it Dependent variable:} \\
& \multicolumn{3}{c}{\it  Same Race} \\
& \multicolumn{2}{c}{\it  Odds Ratio} & Linear Model\\
\cline{2-4}\\ [-1.8ex]
&(1)  &(2)  &(3)        \\
       )
       posthead(\hline) 
       prefoot() 
       postfoot(


\\[-1.8ex]\hline 
\hline \\[-1.8ex] 
\end{tabular} 

\begin{tablenotes}[scriptsize,flushleft] \scriptsize
\item Notes: 
\end{tablenotes}
\end{threeparttable}
\end{table}
)
       replace;
#delimit cr




*stop