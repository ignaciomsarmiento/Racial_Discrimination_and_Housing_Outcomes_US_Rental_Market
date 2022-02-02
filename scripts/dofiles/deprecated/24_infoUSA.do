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


gen choice_black=choice*AfAm
gen choice_hispanic=choice*Hispanic
gen choice_white=choice*White


collapse (sum) choice_black choice_hispanic choice_white (mean) white_infoUSA black_infoUSA hispanic_infoUSA, by(Address Zip_Code)

logit white_infoUSA  choice_white choice_black  choice_hispanic 



logit  white_infoUSA  choice_white choice_black  choice_hispanic 
estimates store model1
test choice_white==choice_black
test choice_white==choice_hispanic
logit  black_infoUSA  choice_white choice_black  choice_hispanic 
estimates store model2
test choice_white==choice_black
test choice_white==choice_hispanic
logit  hispanic_infoUSA  choice_white choice_black  choice_hispanic 
estimates store model3
test choice_white==choice_black
test choice_white==choice_hispanic


#delimit ; 
esttab model1
       model2
       model3
         using "${path_tables}/infoUSA.tex",
         stats(N, fmt(%9.0fc)) 
         se
         style(tex )
         substitute(\_ _)
         varlabels(choice_white "White Identity" 
                   choice_black "African American Identity" 
                   choice_hispanic "Hispanic/LatinX"
                   _cons "Constant" 
                   )  
         mlabels(,none)  
         nonumbers
         collabels(,none) 
         eqlabels(,none)               
         prehead( 
              \begin{table}[H]
          \footnotesize \centering
          \begin{threeparttable}
          \captionsetup{justification=centering}
            \caption{Realized Rents}
            \label{tab:real_rents}

          \begin{tabular}{@{\extracolsep{5pt}} lccc}       
          \\[-1.8ex]\hline 
          \hline \\[-1.8ex] 
          & \multicolumn{3}{c}{Race/Ethnicity of Observed Renters} \\    
          &  White & African American  &  Hispanic/LatinX \\                  
            &  (1) & (2)  &  (3) \\  
         )
         posthead(\hline \\
         Inquiry from:  &   &   &   \\            
 &   &   &   \\            ) 
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