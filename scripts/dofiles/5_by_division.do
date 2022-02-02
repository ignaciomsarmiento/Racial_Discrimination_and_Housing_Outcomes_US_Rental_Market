/*
Replication files  
*/

clear all
set matsize 11000

*Paths
*Working directory
cd "~/Dropbox/Research/cbsa_discrimination/"

global path_tables  "views"
*global path_tables  "~/Dropbox/Apps/ShareLaTeX/Welfare Impact of Housing Discrimination/tables/cbsa_discrimination/prelim"


*Load Data
cd "~/Dropbox/Research/cbsa_discrimination/github/Racial_Discrimination_US_Rental_Market/"



************************************************************************************************
* Minority
************************************************************************************************
est clear

reghdfe choice  Minority_div*  , absorb(gender education_level inquiry_order date_sent_out Address)  cl(CBSA_downtown) level(90) nocons
estadd scalar obs = `e(N)'

forvalues i = 1/9{
  sum choice if White==1 & div`i'==1
  loc loc_res_w_`i'=r(mean)
  estadd scalar res_w_`i' = `loc_res_w_`i'', replace 
}
estadd local gender = "Yes", replace 
estadd local edu = "Yes", replace 
estadd local order = "Yes", replace 

estimates store model1


matrix define M=J(9,3,.)
matrix colnames M = minority_lci minority_coef minority_uci
forvalues i = 1/9{
  matrix M[`i',1] =  _b[Minority_div`i'] - invttail(e(N),0.05)*_se[Minority_div`i']
  matrix M[`i',2] =  _b[Minority_div`i']
  matrix M[`i',3] =  _b[Minority_div`i'] + invttail(e(N),0.05)*_se[Minority_div`i']
}


************************************************************************************************
* Af American and Hispanic/Latinx
************************************************************************************************


reghdfe choice   AfAm_div*   Hispanic_div* , absorb(gender education_level inquiry_order date_sent_out Address)  cl(CBSA_downtown) level(90) nocons
estadd scalar obs = `e(N)'
forvalues i = 1/9{
  sum choice if White==1 & div`i'==1
  loc loc_res_w_`i'=r(mean)
  estadd scalar res_w_`i' = `loc_res_w_`i'', replace 
}
estadd local gender = "Yes", replace 
estadd local edu = "Yes", replace 
estadd local order = "Yes", replace 
estimates store model2


* Results into a Matrix  
matrix define A=J(9,3,.)
matrix colnames A = afam_lci afam_coef afam_uci
forvalues i = 1/9{
  matrix A[`i',1] =  _b[AfAm_div`i'] - invttail(e(N),0.05)*_se[AfAm_div`i']
  matrix A[`i',2] =  _b[AfAm_div`i']
  matrix A[`i',3] =  _b[AfAm_div`i'] + invttail(e(N),0.05)*_se[AfAm_div`i']
}
 
  

matrix define H=J(9,3,.)
matrix colnames H = hispanic_lci hispanic_coef hispanic_uci
forvalues i = 1/9{
  matrix H[`i',1] =  _b[Hispanic_div`i'] - invttail(e(N),0.05)*_se[Hispanic_div`i']
  matrix H[`i',2] =  _b[Hispanic_div`i']
  matrix H[`i',3] =  _b[Hispanic_div`i'] + invttail(e(N),0.05)*_se[Hispanic_div`i']
}



*From http://www.eyalfrank.com/code-riffs-stata-and-regression-tables/


  #delimit ; 
  esttab model1
         model2
         using "${path_tables}/table_division.tex", 
         style(tex) 
         cells(b(star fmt(4)) se(par fmt(4) )  )  
         label 
         stats(res_w_1
               res_w_2
               res_w_3
               res_w_4
               res_w_5
               res_w_6
               res_w_7
               res_w_8
               res_w_9
               gender 
               edu 
               order
               obs
               listings
               diff_response, fmt(2 2 2 2 2 2 2 2 2 0 0 0 %9.0gc)
               labels(" Mean Response (White) East North Central"
                      " Mean Response (White) East South Central"
                      " Mean Response (White) Middle Atlantic"
                      " Mean Response (White) Mountain"
                      " Mean Response (White) New England"
                      " Mean Response (White) Pacific"
                      " Mean Response (White) South Atlantic"
                      " Mean Response (White) West North Central"
                      " Mean Response (White) West South Central"
                     "\hline Gender" 
                     "Education Level" 
                     "Inquiry Order"
                     "\hline Observations"
                     )) 
         mlabels(,none)  
         numbers
         collabels(,none) 
         eqlabels(,none)
         varlabels(Minority_div1  "Minority $\times$ East North Central"
                   Minority_div2  "Minority $\times$ East South Central"
                   Minority_div3  "Minority $\times$ Middle Atlantic"
                   Minority_div4  "Minority $\times$ Mountain"
                   Minority_div5  "Minority $\times$ New England"
                   Minority_div6  "Minority $\times$ Pacific"
                   Minority_div7  "Minority $\times$ South Atlantic"
                   Minority_div8  "Minority $\times$ West North Central"
                   Minority_div9  "Minority $\times$ West South Central"
                   AfAm_div1  "African American $\times$ East North Central"
                   AfAm_div2  "African American $\times$ East South Central"
                   AfAm_div3  "African American $\times$ Middle Atlantic"
                   AfAm_div4  "African American $\times$ Mountain"
                   AfAm_div5  "African American $\times$ New England"
                   AfAm_div6  "African American $\times$ Pacific"
                   AfAm_div7  "African American $\times$ South Atlantic"
                   AfAm_div8  "African American $\times$ West North Central"
                   AfAm_div9  "African American $\times$ West South Central"
                   Hispanic_div1  "Hispanic/LatinX $\times$ East North Central"
                   Hispanic_div2  "Hispanic/LatinX $\times$ East South Central"
                   Hispanic_div3  "Hispanic/LatinX $\times$ Middle Atlantic"
                   Hispanic_div4  "Hispanic/LatinX $\times$ Mountain"
                   Hispanic_div5  "Hispanic/LatinX $\times$ New England"
                   Hispanic_div6  "Hispanic/LatinX $\times$ Pacific"
                   Hispanic_div7  "Hispanic/LatinX $\times$ South Atlantic"
                   Hispanic_div8  "Hispanic/LatinX $\times$ West North Central"
                   Hispanic_div9  "Hispanic/LatinX $\times$ West South Central") 
         starl(* 0.1 ** 0.05 *** 0.01)   
         keep(Minority_div*  AfAm_div*  Hispanic_div*)
         order(Minority_div*  AfAm_div*  Hispanic_div*)     
         level(90)
         prehead( 
           		\begin{table}[H]
  				\scriptsize
           \centering
  				\begin{threeparttable}
  				\captionsetup{justification=centering}
  				  \caption{Response Rates by US Divisions}
  				  \label{tab:responseratesdivision}

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
  			\item Notes: Table reports odd ratios from a within-property conditional logit model including controls for gender, education and order the inquiry was sent. Standard errors clustered at the CBSA Downtown/Suburb level. 90\% confidence intervals reported in parentheses.
  			\end{tablenotes}
  			\end{threeparttable}
  			\end{table}
         )
         replace;
  #delimit cr

  *eststo clear





************************************************************************************************
*Consolidate matrices and export
************************************************************************************************
mat def Res=M,A,H

local rows 
foreach var of varlist div* {
local this = strtoname("`: variable label `var''") 
local rows `rows' `this'
}

mat rownames Res = `rows'


preserve
clear
svmat2 Res,  names(col)  rnames(CBSA)
gen n=_n

save "stores/matrices/division_single_regression.dta"  , replace
restore

  *end 