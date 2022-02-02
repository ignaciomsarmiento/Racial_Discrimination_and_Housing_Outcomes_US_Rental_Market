/*
Replication files  
*/

clear all
set matsize 11000

*Paths
*Working directory
cd "~/Dropbox/Research/cbsa_discrimination/github/Racial_Discrimination_US_Rental_Market/"

global path_tables  "views"
*global path_tables  "~/Dropbox/Apps/ShareLaTeX/Welfare Impact of Housing Discrimination/tables/cbsa_discrimination/prelim"


*Load Data
use "stores/matchedinquiries.dta"

gen changewhiterentersshare=medincchange

gen qwhitepopchange=(changewhiterentersshare<=-0.02)
replace qwhitepopchange=2 if changewhiterentersshare>-0.02 & changewhiterentersshare<0.02
replace qwhitepopchange=3 if changewhiterentersshare>=0.02

forvalues i=0/3{
  sum changewhiterentersshare if qwhitepopchange==`i'  
}

tabulate qwhitepopchange, generate(d_qwhitepopchange)

loc cuts=3
foreach race in Minority Hispanic AfAm {
    forvalues i = 1/`cuts'{
      gen `race'_qwhitepopchange`i'=`race'*d_qwhitepopchange`i'
    }
}

************************************************************************************************
* Af American and Hispanic/Latinx
************************************************************************************************

reghdfe choice   AfAm_qwhitepopchange*   Hispanic_qwhitepopchange* , absorb(gender education_level inquiry_order  Address)  cl(CBSA_downtown) level(90) nocons
estadd scalar obs = `e(N)'
forvalues i = 1/`cuts'{
    sum choice if White==1 & d_qwhitepopchange`i'==1
    loc loc_white`i'=r(mean)
    estadd scalar mean_white`i' = `loc_white`i'', replace 
}

loc obs=`e(N)'


    nlcom (RR_AfAm_qwhitepopchange1: _b[AfAm_qwhitepopchange1]/`loc_white1') ///
          (RR_AfAm_qwhitepopchange2: _b[AfAm_qwhitepopchange2]/`loc_white2') ///
          (RR_AfAm_qwhitepopchange3: _b[AfAm_qwhitepopchange3]/`loc_white3') ///
          (RR_Hispanic_qwhitepopchange1: _b[Hispanic_qwhitepopchange1]/`loc_white1') ///
          (RR_Hispanic_qwhitepopchange2: _b[Hispanic_qwhitepopchange2]/`loc_white2') ///
          (RR_Hispanic_qwhitepopchange3: _b[Hispanic_qwhitepopchange3]/`loc_white3') ///
     , post
     forvalues i = 1/`cuts'{
    sum choice if White==1 & d_qwhitepopchange`i'==1
    loc loc_white`i'=r(mean)
    estadd scalar mean_white`i' = `loc_white`i'', replace 
}
estadd local gender = "Yes", replace 
estadd local edu = "Yes", replace 
estadd local order = "Yes", replace 
estadd local address = "Yes", replace 
estadd local obs= `obs'

estimates store modelRR
*Tables of coeficients


  #delimit ; 
esttab modelRR
         using "${path_tables}/table_medinc_02.tex", 
         style(tex) 
         cells(b(star fmt(4)) se(par fmt(4) )  )  
         label 
         stats(mean_white1
                mean_white2
                mean_white3
               gender 
               edu 
               order
               address
                obs, fmt(2 2 2  0 0 0 0  %6.0fc )
               labels("Mean Response (White) $\Delta Median Income< -0.02$"
                      "Mean Response (White) $\Delta Median Income\in [-0.02,0.02]$"
                      "Mean Response (White) $\Delta Median Income>0.02$"
                     "\hline Gender" 
                     "Education Level" 
                     "Inquiry Order"
                     "Address"
                     "\hline Observations"
                     )) 
         mlabels(,none)  
         numbers
         collabels(,none) 
         eqlabels(,none)
         varlabels(
                   RR_AfAm_qwhitepopchange1  "African American $\times \Delta Median Income< -0.02$"
                   RR_AfAm_qwhitepopchange2  "African American $\times \Delta Median Income\in [-0.02,0.02]$"
                   RR_AfAm_qwhitepopchange3  "African American $\times \Delta Median Income>0.02$"
                   RR_Hispanic_qwhitepopchange1  "Hispanic/LatinX $\times \Delta Median Income< -0.02$"
                   RR_Hispanic_qwhitepopchange2  "Hispanic/LatinX $\times \Delta Median Income\in [-0.02,0.02]$"
                   RR_Hispanic_qwhitepopchange3  "Hispanic/LatinX $\times \Delta Median Income> 0.02$"
                   ) 
         starl(* 0.1 ** 0.05 *** 0.01)   
         keep(RR_AfAm_qwhitepopchang*  RR_Hispanic_qwhitepopchange*)
         order(RR_AfAm_qwhitepopchang*  RR_Hispanic_qwhitepopchange*)
         prehead( 
              \begin{table}[H]
          \footnotesize \centering
          \begin{threeparttable}
          \captionsetup{justification=centering}
            \caption{Response Rates by Changes Median Income}
            \label{tab:change_medinc_share_02}

          \begin{tabular}{@{\extracolsep{5pt}} lcc} 
          \\[-1.8ex]\hline 
          \hline \\[-1.8ex] 
          & \multicolumn{2}{c}{\it Dependent variable:} \\
          & \multicolumn{2}{c}{\it  Response} \\
          \cline{2-3}\\ [-1.8ex]
         )
         posthead(\hline) 
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



