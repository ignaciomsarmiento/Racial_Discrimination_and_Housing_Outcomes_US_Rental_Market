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
use "stores/matchedinquiries_infoUSA_wru.dta"

drop if _merge==1

gen white_infoUSA=(race_renter=="white")
gen black_infoUSA=(race_renter=="black")
gen hispanic_infoUSA=(race_renter=="hispanic")

foreach i in White AfAm Hispanic{
    gen `i'_White=`i'*white_infoUSA 

    gen `i'_Black=`i'*black_infoUSA 

    gen `i'_Hispanic=`i'*hispanic_infoUSA 
    }





clogit choice White_White White_Black  White ///
            i.gender i.education_level i.inquiry_order   , group(Address)   cl(CBSA_downtown) level(90)
sum choice if White==1 
estadd scalar responsewhite = r(mean), replace 
estadd local gender = "Yes", replace 
estadd local edu = "Yes", replace 
estadd local order = "Yes", replace 
estadd local obs = e(N)+e(N_drop)
estadd local diff_response=e(N)/(e(N)+e(N_drop))
estadd local listings= (e(N)+e(N_drop))/3
estimates store model1



clogit choice AfAm_White AfAm_Black  AfAm ///
            i.gender i.education_level i.inquiry_order   , group(Address)   cl(CBSA_downtown) level(90)
sum choice if White==1 
estadd scalar responsewhite = r(mean), replace 
estadd local gender = "Yes", replace 
estadd local edu = "Yes", replace 
estadd local order = "Yes", replace 
estadd local obs = e(N)+e(N_drop)
estadd local diff_response=e(N)/(e(N)+e(N_drop))
estadd local listings= (e(N)+e(N_drop))/3
estimates store model2


clogit choice Hispanic_White Hispanic_Black  Hispanic ///
            i.gender i.education_level i.inquiry_order   , group(Address)   cl(CBSA_downtown) level(90)
sum choice if White==1 
estadd scalar responsewhite = r(mean), replace 
estadd local gender = "Yes", replace 
estadd local edu = "Yes", replace 
estadd local order = "Yes", replace 
estadd local obs = e(N)+e(N_drop)
estadd local diff_response=e(N)/(e(N)+e(N_drop))
estadd local listings= (e(N)+e(N_drop))/3
estimates store model3



#delimit ; 
esttab model1
        model2
        model3
       using "${path_tables}/infoUSA_clogit_wru_white_heterogenity.tex",
       style(tex) 
       eform(1 1 1)
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
       varlabels(White_White     "White x White Renter"
                  White_Black "White x Af. Am. Renter"
                  White_Hispanic   "White x Hispanic Renter"
                  White "White"
                   AfAm_White     "African American x White Renter"
                  AfAm_Black "African American x Af. Am. Renter"
                   AfAm     "African American"
                  AfAm_Hispanic   "African American x Hispanic Renter"
                  Hispanic_White "Hispanic/LatinX x White Renter"
                  Hispanic_Black  "Hispanic/LatinX x Af. Am. Renter"
                  Hispanic_Hispanic "Hispanic/LatinX x Hispanic Renter"
                  Hispanic "Hispanic/LatinX"
                )
       keep( White_White White_Black  White_Hispanic White AfAm_White AfAm_Black AfAm_Hispanic AfAm Hispanic_White Hispanic_Black Hispanic_Hispanic Hispanic)              
       order( White_White White_Black  White_Hispanic White AfAm_White AfAm_Black AfAm_Hispanic AfAm Hispanic_White Hispanic_Black Hispanic_Hispanic Hispanic)              
       starl(* 0.1 ** 0.05 *** 0.01)   
       level(90) 
       prehead( 

\begin{table}[H]
\footnotesize \centering
\begin{threeparttable}
\captionsetup{justification=centering}
  \caption{}
  \label{tab:probhighexposure}

\begin{tabular}{@{\extracolsep{5pt}} lccc} 
\\[-1.8ex]\hline 
\hline \\[-1.8ex] 
& \multicolumn{3}{c}{\it Dependent variable:} \\
& \multicolumn{3}{c}{\it  Odds Ratio} \\
\cline{2-4}\\ [-1.8ex]
&(1)              & (2)                & (3)   \\
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