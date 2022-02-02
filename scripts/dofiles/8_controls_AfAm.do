 /************************************************************
Average Response Rates
Author: Ignacio Sarmiento-Barbieri
************************************************************/


clear all
set matsize 11000

*Paths
*Working directory
cd "~/Dropbox/Research/cbsa_discrimination/github/Racial_Discrimination_US_Rental_Market/"

global path_tables  "views"
*global path_tables  "~/Dropbox/Apps/ShareLaTeX/Welfare Impact of Housing Discrimination/tables/cbsa_discrimination/prelim"


*Load Data
use "stores/matchedinquiries.dta"

est clear



*/
************************************************************************************************
* African American vs Hispanic/LatinX
************************************************************************************************


reghdfe choice  AfAm Hispanic   , absorb(  Address) cl(CBSA_downtown) level(90)  nocons
estadd scalar obs = `e(N)'
sum choice if White==1 
estadd scalar responsewhite = r(mean), replace 
estadd local gender = "", replace 
estadd local edu = "", replace 
estadd local order = "", replace 
estimates store modelB1

reghdfe choice  AfAm Hispanic   , absorb(gender   Address) cl(CBSA_downtown) level(90)  nocons
estadd scalar obs = `e(N)'
sum choice if White==1 
estadd scalar responsewhite = r(mean), replace 
estadd local gender = "Yes", replace 
estadd local edu = "", replace 
estadd local order = "", replace 
estimates store modelB2

reghdfe choice  AfAm Hispanic   , absorb(gender  education_level  Address) cl(CBSA_downtown) level(90)  nocons
estadd scalar obs = `e(N)'
sum choice if White==1 
estadd scalar responsewhite = r(mean), replace 
estadd local gender = "Yes", replace 
estadd local edu = "Yes", replace 
estadd local order = "", replace 
estimates store modelB3


reghdfe choice  AfAm Hispanic   , absorb(gender  education_level inquiry_order  Address) cl(CBSA_downtown) level(90)  nocons
estadd scalar obs = `e(N)'
sum choice if White==1 
estadd scalar responsewhite = r(mean), replace 
estadd local gender = "Yes", replace 
estadd local edu = "Yes", replace 
estadd local order = "Yes", replace 
estimates store modelB4


************************************************************************************************
* Export to latex
* based on http://www.eyalfrank.com/code-riffs-stata-and-regression-tables/
************************************************************************************************

************************************************************
* estout Panel A
************************************************************

#delimit ; 
esttab modelB1 
       modelB2 
       modelB3
       modelB4
       using "${path_tables}/table_controls.tex", 
       style(tex) 
       cells(b(star fmt(4)) se(par fmt(4)))    
       label 
       noobs
       mlabels(,none)  
       nonumbers 
       collabels(,none) 
       eqlabels(,none)
        varlabels( Minority "Minority" 
                 AfAm "African American"
                 Hispanic "Hispanic/LatinX") 
       starl(* 0.1 ** 0.05 *** 0.01) 
       stats(responsewhite
             gender 
             edu 
             order
             obs, fmt(2 0 0 0 %9.0gc)
             labels(" Mean Response (White)"
                   "\hline Gender" 
                   "Education Level" 
                   "Inquiry Order"
                   "\hline Observations"
                   )) 
         prehead(      
         \begin{table}[H]
\footnotesize \centering
\begin{threeparttable}
\captionsetup{justification=centering}
  \caption{Estimates of Discriminatory Constraint on Housing Choice: \\ Robustness to Controls}
      \label{tab:rob.controls}
\begin{tabular}{@{\extracolsep{5pt}}lccccc}
\\[-1.8ex]\hline
\hline \\[-1.8ex]
 & \multicolumn{4}{c}{\textit{Dependent variable: {\it Response}}} \\
 \cline{2-5} \\
\\[-1.8ex] 
& (1) & (2) & (3) & (4)   \\ 
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