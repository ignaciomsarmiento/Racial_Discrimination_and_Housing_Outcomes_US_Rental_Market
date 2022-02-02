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

foreach i in first second third {
  reghdfe `i'  Hispanic AfAm   , absorb(gender education_level   Address) cl(CBSA_downtown) level(90)  nocons
  estimates store model`i'

}

foreach i in Mon Tue Wed Thurs Fri {
  reghdfe `i'  Hispanic AfAm     , absorb(gender education_level inquiry_order  Address) cl(CBSA_downtown) level(90)  nocons
  estimates store model`i'

}


foreach i in Male Female {
  reghdfe `i'  Hispanic AfAm      , absorb(education_level inquiry_order  Address) cl(CBSA_downtown) level(90)  nocons
  estadd scalar obs = `e(N)'
sum choice if White==1 
estadd scalar responsewhite = r(mean), replace 
estadd local gender = "Yes", replace 
estadd local edu = "Yes", replace 
estadd local order = "Yes", replace 


  estimates store model`i'

}




foreach i in Low Medium High {
  reghdfe `i'  Hispanic AfAm      , absorb(gender inquiry_order  Address) cl(CBSA_downtown) level(90)  nocons
  estadd scalar obs = `e(N)'
sum choice if White==1 
estadd scalar responsewhite = r(mean), replace 
estadd local gender = "Yes", replace 
estadd local edu = "Yes", replace 
estadd local order = "Yes", replace 


  estimates store model`i'

}




*From http://www.eyalfrank.com/code-riffs-stata-and-regression-tables/


#delimit ; 
esttab modelfirst 
       modelsecond 
       modelthird
       using "views/tablebalance.tex", 
       style(tex) 
       cells(b(star fmt(4)) se(par fmt(4)))    
       label 
       mlabels(,none)  
       nonumbers 
       noobs
       collabels(,none) 
       varlabels(AfAm "African American"
                 Hispanic "Hispanic/LatinX") 
       starl(* 0.1 ** 0.05 *** 0.01)   
       keep( AfAm Hispanic )              
       order( AfAm Hispanic )
       level(90)     
       prehead( 
         		\begin{table}[H]
				\footnotesize \centering
				\begin{threeparttable}
				\captionsetup{justification=centering}
				  \caption{Overall Response Rates }
				  \label{tab:balance}

				\begin{tabular}{@{\extracolsep{5pt}}lccccc}
        \\[-1.8ex]\hline
        \hline \\[-1.8ex]
         &  (1) & (2) & (3) & (4) & (5) \\
          \cline{2-6}
            \multicolumn{6}{c}{\textit{Panel A: {\it  Inquiry Order}}} \\
             \hline \\[-1.8ex]
          
         & First & Second & Third & & \\
       )
       posthead() 
      prefoot() 
       postfoot(
            \\[-1.8ex]\hline 
      \hline \\[-1.8ex] )
       
       replace;
#delimit cr


*Panel B
#delimit ; 
esttab modelMon 
       modelTue 
       modelWed 
       modelThurs 
       modelFri
       using "views/tablebalance.tex", 
       style(tex) 
       level(90)
       cells(b(star fmt(4)) se(par fmt(4)))    
       label 
       mlabels(,none)
       nonumbers 
       noobs
       collabels(,none)      
       varlabels(AfAm "African American"
                 Hispanic "Hispanic/LatinX") 
       starl(* 0.1 ** 0.05 *** 0.01)   
       keep( AfAm Hispanic )              
       order( AfAm Hispanic )     
       prehead( 
           \multicolumn{6}{c}{\textit{Panel B: Evidence of Differential Choices by Weekday}} \\
           \hline \\[-1.8ex]

          \\[-1.8ex] & Mon & Tue & Wed & Thurs & Fri \\ 
            
       )
       posthead() 
      prefoot() 
       postfoot(
            \\[-1.8ex]\hline 
      \hline \\[-1.8ex] )
       
       append;
#delimit cr


*Panel C

#delimit ; 
esttab modelMale
       modelFemale
       modelLow 
       modelMedium
       modelHigh
       using "views/tablebalance.tex", 
       style(tex) 
       cells(b(star fmt(4)) se(par fmt(4)))    
       label 
       mlabels(,none)  
       nonumbers
       collabels(,none) 
       varlabels(AfAm "African American"
                 Hispanic "Hispanic/LatinX") 
       starl(* 0.1 ** 0.05 *** 0.01) 
       stats(responsewhite
             obs, fmt(2  %9.0gc)
             labels(" Mean Response (White)"                   
                   "\hline Observations"
                   )) 
       keep( AfAm Hispanic )              
       order( AfAm Hispanic )
       level(90)     
       prehead( 
 \multicolumn{6}{c}{\textit{Panel C: Gender and Mother's Education Level}} \\
 \hline \\[-1.8ex]

  &   \multicolumn{2}{c}{Gender} &   \multicolumn{3}{c}{Mother's Education } \\
  & Male & Female & Low  & Medium   & High  \\ 
       )
       posthead() 
      prefoot() 
       postfoot(
            \\[-1.8ex]\hline 
      \hline \\[-1.8ex] 
      \end{tabular} 

      \begin{tablenotes}[scriptsize,flushleft] \scriptsize
      \item Notes: Table reports coefficients from a within-property linear model including controls on different outcomes. In Panel A, the dependent variable takes 1 or 0 depending the order in which the inquiry was sent out, i.e. in Column (1) takes 1 if the inquiry was sent first and 0 otherwise. Regresions in Panel A include controls for gender and education level. In Panel B, takes 1 or 0 depending the weekday the inquiry was sent and includes control for gender, education and order the inquiry was sent. Panel C, does the same for male and females, and levels of maternal education. In the first case, it includes control for  education and order the inquiry was sent. In the second case, it controls for gender and order the inquiry was sent. Standard errors clustered at the CBSA Downtown/Suburb level. 
      \item * Significant at 10\% level, ** significant at 5\% level, *** significant at 1\% level.
      \end{tablenotes}
      \end{threeparttable}
      \end{table}
      )
       
       append;
#delimit cr



*end 