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



preserve
keep if race_renter=="white"
clogit choice AfAm  Hispanic /// 
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
restore

preserve
keep if race_renter=="black"
clogit choice AfAm  Hispanic /// 
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
restore



preserve
keep if race_renter=="hispanic"
clogit choice AfAm  Hispanic /// 
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
restore

#delimit ; 
esttab model1
        model2
        model3
       using "${path_tables}/infoUSA_clogit_wru_subsamples.tex",
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
       varlabels(AfAm "African American"
                  Hispanic "Hispanic/LatinX"
                )
       keep( AfAm Hispanic)              
       order( AfAm Hispanic)              
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
& \multicolumn{3}{c}{\it  Sub Samples} \\
\cline{2-4}\\ [-1.8ex]
& White Renters  & Black Renters       & Hispanic Renters         \\
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