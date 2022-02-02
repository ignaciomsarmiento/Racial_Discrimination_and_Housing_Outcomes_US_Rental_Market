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


loc geog = 4

forvalues i = 1/`geog'{
  sum choice if White==1 & reg`i'==1
  loc mean`i' `r(mean)'
}


************************************************************************************************
* Af American and Hispanic/Latinx
************************************************************************************************

reghdfe choice   AfAm_reg*   Hispanic_reg* , absorb(gender education_level inquiry_order  Address)  cl(CBSA_downtown) level(90) nocons
estadd scalar obs = `e(N)'
sum choice if White==1 & reg1==1
loc loc_res_w_midwest=r(mean)
estadd scalar res_w_midwest = `loc_res_w_midwest', replace 
sum choice if White==1 & reg2==1
loc loc_res_w_northeast=r(mean)
estadd scalar res_w_northeast = `loc_res_w_northeast', replace 
sum choice if White==1 & reg3==1
loc loc_res_w_south=r(mean)
estadd scalar res_w_south = `loc_res_w_south', replace 
sum choice if White==1 & reg4==1
loc loc_res_w_west=r(mean)
estadd scalar res_w_west = `loc_res_w_west', replace 
estadd local gender = "Yes", replace 
estadd local edu = "Yes", replace 
estadd local order = "Yes", replace 
estadd local address = "Yes", replace 
estadd local dm = "Yes", replace 
estimates store model


    nlcom (RR_AfAm_reg1: _b[AfAm_reg1]/`mean1') ///
          (RR_AfAm_reg2: _b[AfAm_reg2]/`mean2') ///
          (RR_AfAm_reg3: _b[AfAm_reg3]/`mean3') ///
          (RR_AfAm_reg4: _b[AfAm_reg4]/`mean4') ///
          (RR_Hispanic_reg1: _b[Hispanic_reg1]/`mean1') ///
          (RR_Hispanic_reg2: _b[Hispanic_reg2]/`mean2') ///
          (RR_Hispanic_reg3: _b[Hispanic_reg3]/`mean3') ///
          (RR_Hispanic_reg4: _b[Hispanic_reg4]/`mean4') ///
     , post



test (RR_AfAm_reg1=RR_AfAm_reg2) ///
     (RR_AfAm_reg1=RR_AfAm_reg3) ///
     (RR_AfAm_reg1=RR_AfAm_reg4) ///
     (RR_AfAm_reg2=RR_AfAm_reg3) ///
     (RR_AfAm_reg2=RR_AfAm_reg4) ///
     (RR_AfAm_reg3=RR_AfAm_reg4) ///
     , mtest(sidak)


mat TAfAm1=r(mtest)
mat TAfAm2=J(1,4,.)
mat TAfAm2[1,1]= `r(chi2)'
mat TAfAm2[1,2]= `r(df)'
mat TAfAm2[1,3]= `r(p)'
mat TAfAm2[1,4]= `r(p)'

mat def TAfAm=TAfAm1\TAfAm2


test (RR_Hispanic_reg1=RR_Hispanic_reg2) ///
     (RR_Hispanic_reg1=RR_Hispanic_reg3) ///
     (RR_Hispanic_reg1=RR_Hispanic_reg4) ///
     (RR_Hispanic_reg2=RR_Hispanic_reg3) ///
     (RR_Hispanic_reg2=RR_Hispanic_reg4) ///
     (RR_Hispanic_reg3=RR_Hispanic_reg4) ///
     , mtest(sidak)


mat THispanic1=r(mtest)
mat THispanic2=J(1,4,.)
mat THispanic2[1,1]= `r(chi2)'
mat THispanic2[1,2]= `r(df)'
mat THispanic2[1,3]= `r(p)'
mat THispanic2[1,4]= `r(p)'

mat def THispanic=THispanic1\THispanic2



forvalues i = 1/`geog'{
test (RR_AfAm_reg`i'=RR_Hispanic_reg1) ///
     (RR_AfAm_reg`i'=RR_Hispanic_reg2) ///
     (RR_AfAm_reg`i'=RR_Hispanic_reg3) ///
     (RR_AfAm_reg`i'=RR_Hispanic_reg4) ///
     , mtest(sidak)


mat Test_Afam_reg`i'Hispanic1=r(mtest)

mat Test_Afam_reg`i'Hispanic2=J(1,4,.)
mat Test_Afam_reg`i'Hispanic2[1,1]= `r(chi2)'
mat Test_Afam_reg`i'Hispanic2[1,2]= `r(df)'
mat Test_Afam_reg`i'Hispanic2[1,3]= `r(p)'
mat Test_Afam_reg`i'Hispanic2[1,4]= `r(p)'

mat def Test_Afam_reg`i'Hispanic=Test_Afam_reg`i'Hispanic1\Test_Afam_reg`i'Hispanic2  
}





* Results into a Matrix  
matrix define A=J(`geog',3,.)
matrix colnames A = afam_lci afam_coef afam_uci
matrix define H=J(`geog',4,.)
matrix colnames H = hispanic_lci hispanic_coef hispanic_uci resp_white


forvalues i = 1/`geog'{
  matrix A[`i',1] =  _b[RR_AfAm_reg`i'] - invttail(e(N),0.05)*_se[RR_AfAm_reg`i']
  matrix A[`i',2] =  _b[RR_AfAm_reg`i']
  matrix A[`i',3] =  _b[RR_AfAm_reg`i'] + invttail(e(N),0.05)*_se[RR_AfAm_reg`i']
  matrix H[`i',1] =  _b[RR_Hispanic_reg`i'] - invttail(e(N),0.05)*_se[RR_Hispanic_reg`i']
  matrix H[`i',2] =  _b[RR_Hispanic_reg`i']
  matrix H[`i',3] =  _b[RR_Hispanic_reg`i'] + invttail(e(N),0.05)*_se[RR_Hispanic_reg`i']
  matrix H[`i',4] =  `mean`i'' 
}






 


************************************************************************************************
*Consolidate matrices and export
************************************************************************************************


************************************************************************************************
*Tests
************************************************************************************************
mat def Tests=TAfAm,THispanic
preserve
clear
svmat2 Tests, rnames(CBSA)

rename Tests1  AfAm_chi2         
rename Tests2  AfAm_df
rename Tests3  AfAm_p
rename Tests4  AfAm_adjustp
rename Tests5  Hispanci_chi2         
rename Tests6 Hispanci_df
rename Tests7 Hispanci_p
rename Tests8 Hispanci_adjustp

save "stores/matrices/region_tests_hispanic_RR.dta"  , replace
export delimited using "stores/matrices/region_tests_hispanic_RR.csv", replace
restore

mat list Tests
mat Tests2 = Tests[1...,3],Tests[1...,4],Tests[1...,7],Tests[1...,8]
mat list Tests2


matrix rownames Tests2 = reg_1_2 reg_1_3 reg_1_4 reg_2_3 reg_2_4 reg_3_4 all

#delimit ; 
esttab matrix(Tests2)
         using "${path_tables}/test_eq_coef_region.tex", 
         style(tex )
         substitute(\_ _)
         varlabels(reg_1_2 "\$RR_{Midwest}=RR_{Northeast}$" 
                   reg_1_3 "\$RR_{Midwest}=RR_{South}$" 
                   reg_1_4 "\$RR_{Midwest}=RR_{West}$" 
                   reg_2_3 "\$RR_{Northeast}=RR_{South}$" 
                   reg_2_4 "\$RR_{Northeast}=RR_{West}$" 
                   reg_3_4 "\$RR_{South}=RR_{West}$" 
                   all     "All"
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
            \caption{Test Equality Relative Response Rates}
            \label{tab:eq.coef.regions}

          \begin{tabular}{@{\extracolsep{5pt}} lcccc}       
          \\[-1.8ex]\hline 
          \hline \\[-1.8ex] 
          \multicolumn{2}{c}{African American} & \multicolumn{2}{c}{Hispanic/LatinX} \\          
           &  pval & Adj. pval &  pval & Adj. pval      \\    
            &  (1) & (2)  &  (3) & (4) \\  
         )
         posthead(\hline) 
         prefoot() 
         postfoot(
              \\[-1.8ex]\hline 
        \hline \\[-1.8ex] 
        \end{tabular} 

        \begin{tablenotes}[scriptsize,flushleft] \scriptsize
        \item Notes: Adjusted p-values are corrected for multiple testing using Sidak's correction      
        \end{tablenotes}
        \end{threeparttable}
        \end{table}
         )
         
         replace;
         
#delimit cr

************************************************************************************************
*Tests Afam vs Hispanics
************************************************************************************************
mat def Tests_Afam_Hispanic=Test_Afam_reg1Hispanic\Test_Afam_reg2Hispanic\Test_Afam_reg3Hispanic\Test_Afam_reg4Hispanic
preserve
clear
svmat2 Tests_Afam_Hispanic, rnames(CBSA)

rename Tests_Afam_Hispanic1  chi2        
rename Tests_Afam_Hispanic2  df
rename Tests_Afam_Hispanic3  p
rename Tests_Afam_Hispanic4  adjustp


save "stores/matrices/region_tests_afam_hispanic_RR.dta"  , replace
export delimited using "stores/matrices/region_afam_hispanic_RR.csv", replace
restore

mat list Tests_Afam_Hispanic
mat Tests_Afam_Hispanic = Tests_Afam_Hispanic[.,3..4]
mat list Tests_Afam_Hispanic
matrix rownames Tests_Afam_Hispanic = reg_1_reg1 ///
                                      reg_1_reg2 ///
                                      reg_1_reg3 ///
                                      reg_1_reg4 ///
                                      reg_1_all ///
                                      reg_2_reg1 ///
                                      reg_2_reg2 ///
                                      reg_2_reg3 ///
                                      reg_2_reg4 ///
                                      reg_2_all ///
                                      reg_3_reg1 ///
                                      reg_3_reg2 ///
                                      reg_3_reg3 ///
                                      reg_3_reg4 ///
                                      reg_3_all ///
                                      reg_4_reg1 ///
                                      reg_4_reg2 ///
                                      reg_4_reg3 ///
                                      reg_4_reg4 ///
                                      reg_4_all ///






#delimit ; 
esttab matrix(Tests_Afam_Hispanic)
         using "${path_tables}/test_eq_coef_region_afam_hisp.tex", 
         style(tex )
         substitute(\_ _)
          varlabels(reg_1_reg1 "\$RR^{Af. Am.}_{Midwest}=RR^{Hisp/Lat}_{Midwest}$" 
                    reg_1_reg2 "\$RR^{Af. Am.}_{Midwest}=RR^{Hisp/Lat}_{Northeast}$" 
                    reg_1_reg3 "\$RR^{Af. Am.}_{Midwest}=RR^{Hisp/Lat}_{South}$" 
                    reg_1_reg4 "\$RR^{Af. Am.}_{Midwest}=RR^{Hisp/Lat}_{West}$" 
                    reg_1_all  "All"
                    reg_2_reg1 "\$RR^{Af. Am.}_{Northeast}=RR^{Hisp/Lat}_{Midwest}$" 
                    reg_2_reg2 "\$RR^{Af. Am.}_{Northeast}=RR^{Hisp/Lat}_{Northeast}$" 
                    reg_2_reg3 "\$RR^{Af. Am.}_{Northeast}=RR^{Hisp/Lat}_{South}$" 
                    reg_2_reg4 "\$RR^{Af. Am.}_{Northeast}=RR^{Hisp/Lat}_{West}$" 
                    reg_2_all  "All"
                    reg_3_reg1 "\$RR^{Af. Am.}_{South}=RR^{Hisp/Lat}_{Midwest}$" 
                    reg_3_reg2 "\$RR^{Af. Am.}_{South}=RR^{Hisp/Lat}_{Northeast}$" 
                    reg_3_reg3 "\$RR^{Af. Am.}_{South}=RR^{Hisp/Lat}_{South}$" 
                    reg_3_reg4 "\$RR^{Af. Am.}_{South}=RR^{Hisp/Lat}_{West}$" 
                    reg_3_all  "All"
                    reg_4_reg1 "\$RR^{Af. Am.}_{West}=RR^{Hisp/Lat}_{Midwest}$" 
                    reg_4_reg2 "\$RR^{Af. Am.}_{West}=RR^{Hisp/Lat}_{Northeast}$" 
                    reg_4_reg3 "\$RR^{Af. Am.}_{West}=RR^{Hisp/Lat}_{South}$" 
                    reg_4_reg4 "\$RR^{Af. Am.}_{West}=RR^{Hisp/Lat}_{West}$" 
                    reg_4_all  "All"
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
            \caption{Test Equality Relative Response Rates}
            \label{tab:eq.coef.regions.between}

          \begin{tabular}{@{\extracolsep{5pt}} lcc}       
          \\[-1.8ex]\hline 
          \hline \\[-1.8ex] 
          &  pval & Adj. pval \\
          &  (1) & (2) \\ 
         )
         posthead(\hline) 
         prefoot() 
         postfoot(
              \\[-1.8ex]\hline 
        \hline \\[-1.8ex] 
        \end{tabular} 

        \begin{tablenotes}[scriptsize,flushleft] \scriptsize
        \item Notes:  Adjusted p-values are corrected for multiple testing using Sidak's correction      
        \end{tablenotes}
        \end{threeparttable}
        \end{table}
         )
         
         replace;
         
#delimit cr

************************************************************************************************
*Results
************************************************************************************************
mat def Res=A,H

local rows 
foreach var of varlist reg* {
local this = strtoname("`: variable label `var''") 
local rows `rows' `this'
}

mat rownames Res = `rows'


preserve
clear
svmat2 Res,  names(col)  rnames(CBSA)
gen n=_n

save "stores/matrices/region_single_regression_RR.dta"  , replace
restore


*Tables of coeficients


  #delimit ; 
  esttab model
         using "${path_tables}/table_region.tex", 
         style(tex) 
         cells(b(star fmt(4)) se(par fmt(4) )  )  
         label 
         stats(res_w_midwest
                res_w_northeast
                res_w_south
                res_w_west
               gender 
               edu 
               order
               address
                obs, fmt(2 2 2 2 0 0 0 0  %9.0gc )
               labels("Mean Response (White) Midwest"
                      "Mean Response (White) Northeast"
                      "Mean Response (White) South"
                      "Mean Response (White) West"
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
                   AfAm_reg1  "African American $\times$ Midwest"
                   AfAm_reg2  "African American $\times$ Northeast"
                   AfAm_reg3  "African American $\times$ South"
                   AfAm_reg4  "African American $\times$ West"
                   Hispanic_reg1  "Hispanic/LatinX $\times$ Midwest"
                   Hispanic_reg2  "Hispanic/LatinX $\times$ Northeast"
                   Hispanic_reg3  "Hispanic/LatinX $\times$ South"
                   Hispanic_reg4  "Hispanic/LatinX $\times$ West") 
         starl(* 0.1 ** 0.05 *** 0.01)   
         keep(AfAm_reg*  Hispanic_reg*)
         order(AfAm_reg*  Hispanic_reg*)
         prehead( 
              \begin{table}[H]
          \footnotesize \centering
          \begin{threeparttable}
          \captionsetup{justification=centering}
            \caption{Response Rates by US Regions}
            \label{tab:response_rates_regions}

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

