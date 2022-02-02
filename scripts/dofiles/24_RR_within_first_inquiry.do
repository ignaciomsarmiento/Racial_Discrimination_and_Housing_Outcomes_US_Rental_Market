/*
Replication files  
*/

clear all
set matsize 11000

*Paths
*Working directory
cd "~/Dropbox/Research/cbsa_discrimination/github/Racial_Discrimination_US_Rental_Market/"

*global path_tables  "views"
global path_tables  "~/Dropbox/Apps/ShareLaTeX/Welfare Impact of Housing Discrimination/tables/cbsa_discrimination/"


*Load Data
use "stores/matchedinquiries.dta"


sum choice if White==1 
  loc mean1 `r(mean)'

************************************************************************************************
* Af American and Hispanic/Latinx
************************************************************************************************



reghdfe choice   AfAm   Hispanic , absorb(gender education_level inquiry_order Address)  cl(CBSA_downtown) level(90) nocons
estadd scalar obs = `e(N)'
sum choice if White==1 
loc loc_res_w=r(mean)
estadd local gender = "Yes", replace 
estadd local edu = "Yes", replace 
estadd local order = "Yes", replace 
estadd local address = "Yes", replace 
estadd local dm = "Yes", replace 
estimates store model



************************************************************************************************
*First Inquiry
************************************************************************************************
reghdfe choice   AfAm   Hispanic if inquiry_order==1 , absorb(gender education_level )  cl(CBSA_downtown) level(90) nocons
estadd scalar obs = `e(N)'


estadd local gender = "Yes", replace 
estadd local edu = "Yes", replace 
estadd local order = "Yes", replace 
estadd local address = "Yes", replace 
estadd local dm = "Yes", replace 
estimates store model2



************************************************************************************************
*Tables of coeficients
************************************************************************************************

************************************************************
* estout Panel A: Relative Response Rates
************************************************************


  #delimit ; 
  esttab model
          model2
          
         using "${path_tables}/table_first_inquiry.tex", 
         style(tex) 
         cells(b(star fmt(4)) se(par fmt(4) )  )  
         label 
          stats(res_w
               gender 
               edu 
               order
               address
                obs, fmt(2 2 2 2 0 0 0 0  %9.0gc )
               labels("Mean Response (White)"
                     "\hline Gender" 
                     "Education Level" 
                     "Inquiry Order"
                     "Address"
                     "\hline Observations"
                     )) 
          noobs
         mlabels(,none)  
         nonumbers
         collabels(,none) 
         eqlabels(,none)
         varlabels(
                   AfAm   "African American"
                   Hispanic  "Hispanic/LatinX"
                   ) 
         starl(* 0.1 ** 0.05 *** 0.01)   
         keep(AfAm Hispanic)
         order(AfAm Hispanic)
         prehead( 
              \begin{table}[H]
          \scriptsize \centering
          \begin{threeparttable}
          \captionsetup{justification=centering}
            \caption{Probability of Response}
            \label{tab:ATT_controls}

          \begin{tabular}{@{\extracolsep{5pt}} lcc} 
          \\[-1.8ex]\hline 
          \hline \\[-1.8ex] 
          & \multicolumn{2}{c}{\it Dependent variable:} \\
          & \multicolumn{2}{c}{\it  Response} \\
           \cline{2-3}

          \\[-1.8ex] & Full & First      &\\
          \\[-1.8ex] & Sample & Inquiry  &\\
          \\[-1.8ex] & (1) & (2) \\
          \hline \\[-1.8ex] 
         )
         posthead({\it Panel A: Relative Responses } \\
              &  &&    \\)  
         prefoot() 
            postfoot(
              \\[-1.8ex]\hline 
        \hline \\[-1.8ex] 
        \end{tabular} 

        \begin{tablenotes}[scriptsize,flushleft] \scriptsize
        \item Notes: Table reports coeficients from a within-property linear regression model including controls for gender, education and order the inquiry was sent. Standard errors clustered at the CBSA Downtown/Suburb level reported in parentheses.
        \end{tablenotes}
        \end{threeparttable}
        \end{table}
         )
         substitute(\_ _)
         replace;
  #delimit cr
