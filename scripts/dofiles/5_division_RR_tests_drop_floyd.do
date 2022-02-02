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


loc geog = 9

forvalues i = 1/`geog'{
  sum choice if White==1 & div`i'==1
  loc mean`i' `r(mean)'
}


************************************************************************************************
* Af American and Hispanic/Latinx
************************************************************************************************

reghdfe choice   AfAm_div*   Hispanic_div* , absorb(gender education_level inquiry_order Address)  cl(CBSA_downtown) level(90) nocons
estadd scalar obs = `e(N)'

forvalues i = 1/9{
  sum choice if White==1 & div`i'==1
  loc loc_res_w_`i'=r(mean)
  estadd scalar res_w_`i' = `loc_res_w_`i'', replace 
}

estadd local gender = "Yes", replace 
estadd local edu = "Yes", replace 
estadd local order = "Yes", replace 
estadd local address = "Yes", replace 
estadd local dm = "Yes", replace 
estimates store model


     nlcom (RR_AfAm_div1: _b[AfAm_div1]/`mean1') ///
          (RR_AfAm_div2: _b[AfAm_div2]/`mean2') ///
          (RR_AfAm_div3: _b[AfAm_div3]/`mean3') ///
          (RR_AfAm_div4: _b[AfAm_div4]/`mean4') ///
          (RR_AfAm_div5: _b[AfAm_div5]/`mean5') ///
          (RR_AfAm_div6: _b[AfAm_div6]/`mean6') ///
          (RR_AfAm_div7: _b[AfAm_div7]/`mean7') ///
          (RR_AfAm_div8: _b[AfAm_div8]/`mean8') ///
          (RR_AfAm_div9: _b[AfAm_div9]/`mean9') ///
          (RR_Hispanic_div1: _b[Hispanic_div1]/`mean1') ///
          (RR_Hispanic_div2: _b[Hispanic_div2]/`mean2') ///
          (RR_Hispanic_div3: _b[Hispanic_div3]/`mean3') ///
          (RR_Hispanic_div4: _b[Hispanic_div4]/`mean4') ///
          (RR_Hispanic_div5: _b[Hispanic_div5]/`mean5') ///
          (RR_Hispanic_div6: _b[Hispanic_div6]/`mean6') ///
          (RR_Hispanic_div7: _b[Hispanic_div7]/`mean7') ///
          (RR_Hispanic_div8: _b[Hispanic_div8]/`mean8') ///
          (RR_Hispanic_div9: _b[Hispanic_div9]/`mean9') ///
     , post
estimates store modelRR





* Results into a Matrix  
matrix define A=J(`geog',3,.)
matrix colnames A = afam_lci afam_coef afam_uci
matrix define H=J(`geog',4,.)
matrix colnames H = hispanic_lci hispanic_coef hispanic_uci resp_white


forvalues i = 1/`geog'{
  matrix A[`i',1] =  _b[RR_AfAm_div`i'] - invttail(e(N),0.05)*_se[RR_AfAm_div`i']
  matrix A[`i',2] =  _b[RR_AfAm_div`i']
  matrix A[`i',3] =  _b[RR_AfAm_div`i'] + invttail(e(N),0.05)*_se[RR_AfAm_div`i']
  matrix H[`i',1] =  _b[RR_Hispanic_div`i'] - invttail(e(N),0.05)*_se[RR_Hispanic_div`i']
  matrix H[`i',2] =  _b[RR_Hispanic_div`i']
  matrix H[`i',3] =  _b[RR_Hispanic_div`i'] + invttail(e(N),0.05)*_se[RR_Hispanic_div`i']
  sum choice if White==1 & div`i'==1
  matrix H[`i',4] =  `mean`i'' 
}




************************************************************************************************
*Drop two weeks following G. Floyd Hoicide
************************************************************************************************

preserve

drop if Round==20 
drop if Round==21
drop if Round==22

gen floyd_sample=1

tab floyd_sample

tempfile dbfloyd
save `dbfloyd'

forvalues i = 1/`geog'{
  sum choice if White==1 & div`i'==1
  loc mean`i' `r(mean)'
}

tab floyd_sample


reghdfe choice   AfAm_div*   Hispanic_div* , absorb(gender education_level inquiry_order Address)  cl(CBSA_downtown) level(90) nocons
estadd scalar obs = `e(N)'

forvalues i = 1/9{
  sum choice if White==1 & div`i'==1
  loc loc_res_w_`i'=r(mean)
  estadd scalar res_w_`i' = `loc_res_w_`i'', replace 
}

estadd local gender = "Yes", replace 
estadd local edu = "Yes", replace 
estadd local order = "Yes", replace 
estadd local address = "Yes", replace 
estadd local dm = "Yes", replace 
estimates store modelb


      nlcom (RR_AfAm_div1: _b[AfAm_div1]/`mean1') ///
          (RR_AfAm_div2: _b[AfAm_div2]/`mean2') ///
          (RR_AfAm_div3: _b[AfAm_div3]/`mean3') ///
          (RR_AfAm_div4: _b[AfAm_div4]/`mean4') ///
          (RR_AfAm_div5: _b[AfAm_div5]/`mean5') ///
          (RR_AfAm_div6: _b[AfAm_div6]/`mean6') ///
          (RR_AfAm_div7: _b[AfAm_div7]/`mean7') ///
          (RR_AfAm_div8: _b[AfAm_div8]/`mean8') ///
          (RR_AfAm_div9: _b[AfAm_div9]/`mean9') ///
          (RR_Hispanic_div1: _b[Hispanic_div1]/`mean1') ///
          (RR_Hispanic_div2: _b[Hispanic_div2]/`mean2') ///
          (RR_Hispanic_div3: _b[Hispanic_div3]/`mean3') ///
          (RR_Hispanic_div4: _b[Hispanic_div4]/`mean4') ///
          (RR_Hispanic_div5: _b[Hispanic_div5]/`mean5') ///
          (RR_Hispanic_div6: _b[Hispanic_div6]/`mean6') ///
          (RR_Hispanic_div7: _b[Hispanic_div7]/`mean7') ///
          (RR_Hispanic_div8: _b[Hispanic_div8]/`mean8') ///
          (RR_Hispanic_div9: _b[Hispanic_div9]/`mean9') ///
     , post
estimates store modelbRR



restore


************************************************************************************************
*Test differences
************************************************************************************************
 
use "stores/matchedinquiries.dta"
gen floyd_sample=0

append using `dbfloyd', nolabel

tab floyd_sample

forvalues i = 1/`geog'{
  gen floyd_AfAm_div`i'=AfAm_div`i'*floyd_sample
  gen floyd_Hispanic_div`i'=Hispanic_div`i'*floyd_sample
}

reghdfe choice AfAm_div*  floyd_AfAm_div*   Hispanic_div* floyd_Hispanic_div* , absorb(gender education_level inquiry_order Address)  cl(CBSA_downtown) level(90) nocons
*need to be added manually

forvalues i = 1/`geog'{
  sum choice if White==1 & div`i'==1 & floyd_sample==0
  loc mean`i' `r(mean)'
}


nlcom     (RR_floyd_AfAm_div1: _b[floyd_AfAm_div1]/`mean1') ///
          (RR_floyd_AfAm_div2: _b[floyd_AfAm_div2]/`mean2') ///
          (RR_floyd_AfAm_div3: _b[floyd_AfAm_div3]/`mean3') ///
          (RR_floyd_AfAm_div4: _b[floyd_AfAm_div4]/`mean4') ///
          (RR_floyd_AfAm_div5: _b[floyd_AfAm_div5]/`mean5') ///
          (RR_floyd_AfAm_div6: _b[floyd_AfAm_div6]/`mean6') ///
          (RR_floyd_AfAm_div7: _b[floyd_AfAm_div7]/`mean7') ///
          (RR_floyd_AfAm_div8: _b[floyd_AfAm_div8]/`mean8') ///
          (RR_floyd_AfAm_div9: _b[floyd_AfAm_div9]/`mean9') ///
          (RR_floyd_Hispanic_div1: _b[floyd_Hispanic_div1]/`mean1') ///
          (RR_floyd_Hispanic_div2: _b[floyd_Hispanic_div2]/`mean2') ///
          (RR_floyd_Hispanic_div3: _b[floyd_Hispanic_div3]/`mean3') ///
          (RR_floyd_Hispanic_div4: _b[floyd_Hispanic_div4]/`mean4') ///
          (RR_floyd_Hispanic_div5: _b[floyd_Hispanic_div5]/`mean5') ///
          (RR_floyd_Hispanic_div6: _b[floyd_Hispanic_div6]/`mean6') ///
          (RR_floyd_Hispanic_div7: _b[floyd_Hispanic_div7]/`mean7') ///
          (RR_floyd_Hispanic_div8: _b[floyd_Hispanic_div8]/`mean8') ///
          (RR_floyd_Hispanic_div9: _b[floyd_Hispanic_div9]/`mean9') ///
     , post

 







************************************************************************************************
*Tables of coeficients
************************************************************************************************

************************************************************
* estout Panel A: Relative Response Rates
************************************************************


  #delimit ; 
  esttab modelRR
          modelbRR
         using "${path_tables}/table_division.tex", 
         style(tex) 
         cells(b(star fmt(4)) se(par fmt(4) )  )  
         label 
          noobs
         mlabels(,none)  
         nonumbers
         collabels(,none) 
         eqlabels(,none)
         varlabels(
                   RR_AfAm_div1  "African American $\times$ East North Central"
                   RR_AfAm_div2  "African American $\times$ East South Central"
                   RR_AfAm_div3  "African American $\times$ Middle Atlantic"
                   RR_AfAm_div4  "African American $\times$ Mountain"
                   RR_AfAm_div5  "African American $\times$ New England"
                   RR_AfAm_div6  "African American $\times$ Pacific"
                   RR_AfAm_div7  "African American $\times$ South Atlantic"
                   RR_AfAm_div8  "African American $\times$ West North Central"
                   RR_AfAm_div9  "African American $\times$ West South Central"
                   RR_Hispanic_div1  "Hispanic/LatinX $\times$ East North Central"
                   RR_Hispanic_div2  "Hispanic/LatinX $\times$ East South Central"
                   RR_Hispanic_div3  "Hispanic/LatinX $\times$ Middle Atlantic"
                   RR_Hispanic_div4  "Hispanic/LatinX $\times$ Mountain"
                   RR_Hispanic_div5  "Hispanic/LatinX $\times$ New England"
                   RR_Hispanic_div6  "Hispanic/LatinX $\times$ Pacific"
                   RR_Hispanic_div7  "Hispanic/LatinX $\times$ South Atlantic"
                   RR_Hispanic_div8  "Hispanic/LatinX $\times$ West North Central"
                   RR_Hispanic_div9  "Hispanic/LatinX $\times$ West South Central")
         starl(* 0.1 ** 0.05 *** 0.01)   
         keep(RR_AfAm_div*  RR_Hispanic_div*)
         order(RR_AfAm_div*  RR_Hispanic_div*)
         prehead( 
              \begin{table}[H]
          \scriptsize \centering
          \begin{threeparttable}
          \captionsetup{justification=centering}
            \caption{Response Rates by US Divisions}
            \label{tab:response_rates_divisions}

          \begin{tabular}{@{\extracolsep{5pt}} lccc} 
          \\[-1.8ex]\hline 
          \hline \\[-1.8ex] 
          & \multicolumn{2}{c}{\it Dependent variable:} \\
          & \multicolumn{2}{c}{\it  Response} \\
           \cline{2-3}

          \\[-1.8ex] & Full & Drop Weeks   & pvalue\\
          \\[-1.8ex] & Sample & G. Floyd Homicide  & (1)-(2) \\
          \\[-1.8ex] & (1) & (2) & (3) \\
          \hline \\[-1.8ex] 
         )
         posthead({\it Panel A: Relative Responses } \\
              &  &&    \\)  
         prefoot() 
           postfoot(
      \hline \\[-1.8ex] )
         substitute(\_ _)
         replace;
  #delimit cr

************************************************************
* estout Panel B: Coefficients
************************************************************

  #delimit ; 
  esttab model
          modelb
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
               obs, fmt(2 2 2 2 2 2 2 2 2  %9.0gc)
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
         nonumbers
         collabels(,none) 
         eqlabels(,none)
         varlabels(
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
         keep(AfAm_div*  Hispanic_div*)
         order(AfAm_div*  Hispanic_div*)
         prehead(
         ) 
              posthead({\it Panel B: Coefficients }\\
           &  &   & \\)  
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
         append;
  #delimit cr

