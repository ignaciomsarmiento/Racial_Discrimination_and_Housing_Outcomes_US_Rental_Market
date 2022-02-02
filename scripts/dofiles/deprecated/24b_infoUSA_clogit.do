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



use "stores/matchedinquiries_infoUSA.dta"

gen AfAm_White=AfAm*white_infoUSA 
gen AfAm_Hispanic=AfAm*hispanic_infoUSA 
gen AfAm_Black=AfAm*black_infoUSA
gen Hispanic_White=Hispanic*white_infoUSA 
gen Hispanic_Hispanic=Hispanic*hispanic_infoUSA 
gen Hispanic_Black=Hispanic*black_infoUSA

clogit choice AfAm_White   AfAm_Black  AfAm_Hispanic Hispanic_White  Hispanic_Black Hispanic_Hispanic /// 
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


reghdfe choice AfAm_White   AfAm_Black  AfAm_Hispanic Hispanic_White  Hispanic_Black Hispanic_Hispanic, /// 
            absorb(gender education_level inquiry_order  Address)  cl(CBSA_downtown) level(90) nocons
sum choice if White==1 
estadd scalar responsewhite = r(mean), replace 
estadd local gender = "Yes", replace 
estadd local edu = "Yes", replace 
estadd local order = "Yes", replace 
estimates store model2



#delimit ; 
esttab model1
        model2
       using "${path_tables}/infoUSA_clogit.tex",
       style(tex) 
       eform(1 0)
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
       varlabels(AfAm_White     "African American x White Renter"
                  AfAm_Black "African American x Af. Am. Renter"
                  AfAm_Hispanic   "African American x Hispanic Renter"
                  Hispanic_White "Hispanic/LatinX x White Renter"
                  Hispanic_Black  "Hispanic/LatinX x Af. Am. Renter"
                  Hispanic_Hispanic "Hispanic/LatinX x Hispanic Renter"
                )
       keep( AfAm_White AfAm_Black AfAm_Hispanic Hispanic_White Hispanic_Black Hispanic_Hispanic)              
       order( AfAm_White AfAm_Black AfAm_Hispanic Hispanic_White Hispanic_Black Hispanic_Hispanic)              
       starl(* 0.1 ** 0.05 *** 0.01)   
       level(90) 
       prehead( 

\begin{table}[H]
\footnotesize \centering
\begin{threeparttable}
\captionsetup{justification=centering}
  \caption{Overall Discrimination Rates }
  \label{tab:probhighexposure}

\begin{tabular}{@{\extracolsep{5pt}} lcc} 
\\[-1.8ex]\hline 
\hline \\[-1.8ex] 
& \multicolumn{2}{c}{\it Dependent variable:} \\
& \multicolumn{2}{c}{\it  Response} \\
\cline{2-3}\\ [-1.8ex]
&Clogit              & LPM                   \\
&(1)              & (2)                   \\
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