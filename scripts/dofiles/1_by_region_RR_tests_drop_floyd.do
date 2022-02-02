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



reghdfe choice   AfAm_reg*   Hispanic_reg* , absorb(gender education_level inquiry_order Address)  cl(CBSA_downtown) level(90) nocons
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
estimates store modelRR





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
*Drop month following G. Floyd Homicide (Round 22)
************************************************************************************************


preserve

drop if date_sent_out>=date("25may2020", "DMY") & date_sent_out<=date("25jun2020", "DMY")
tab date_sent_out

gen floyd_sample=1


tempfile dbfloyd
save `dbfloyd'

forvalues i = 1/`geog'{
  sum choice if White==1 & reg`i'==1
  loc mean`i' `r(mean)'
}

tab floyd_sample
reghdfe choice   AfAm_reg*   Hispanic_reg* , absorb(gender education_level inquiry_order Address)  cl(CBSA_downtown) level(90) nocons
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
estimates store modelb


    nlcom (RR_AfAm_reg1: _b[AfAm_reg1]/`mean1') ///
          (RR_AfAm_reg2: _b[AfAm_reg2]/`mean2') ///
          (RR_AfAm_reg3: _b[AfAm_reg3]/`mean3') ///
          (RR_AfAm_reg4: _b[AfAm_reg4]/`mean4') ///
          (RR_Hispanic_reg1: _b[Hispanic_reg1]/`mean1') ///
          (RR_Hispanic_reg2: _b[Hispanic_reg2]/`mean2') ///
          (RR_Hispanic_reg3: _b[Hispanic_reg3]/`mean3') ///
          (RR_Hispanic_reg4: _b[Hispanic_reg4]/`mean4') ///
     , post
estimates store modelbRR



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

restore


************************************************************************************************
*Test differences
************************************************************************************************
 
use "stores/matchedinquiries.dta",clear
gen floyd_sample=0

append using `dbfloyd', nolabel

tab floyd_sample

forvalues i = 1/`geog'{
  gen floyd_AfAm_reg`i'=AfAm_reg`i'*floyd_sample
  gen floyd_Hispanic_reg`i'=Hispanic_reg`i'*floyd_sample
}

reghdfe choice AfAm_reg*  floyd_AfAm_reg*   Hispanic_reg* floyd_Hispanic_reg* , absorb(gender education_level inquiry_order Address)  cl(CBSA_downtown) level(90) nocons
*need to be added manually

forvalues i = 1/`geog'{
  sum choice if White==1 & reg`i'==1 & floyd_sample==0
  loc mean`i' `r(mean)'
}

 nlcom (RR_floyd_AfAm_reg1: _b[floyd_AfAm_reg1]/`mean1') ///
          (RR_floyd_AfAm_reg2: _b[floyd_AfAm_reg2]/`mean2') ///
          (RR_floyd_AfAm_reg3: _b[floyd_AfAm_reg3]/`mean3') ///
          (RR_floyd_AfAm_reg4: _b[floyd_AfAm_reg4]/`mean4') ///
          (RR_floyd_Hispanic_reg1: _b[floyd_Hispanic_reg1]/`mean1') ///
          (RR_floyd_Hispanic_reg2: _b[floyd_Hispanic_reg2]/`mean2') ///
          (RR_floyd_Hispanic_reg3: _b[floyd_Hispanic_reg3]/`mean3') ///
          (RR_floyd_Hispanic_reg4: _b[floyd_Hispanic_reg4]/`mean4') ///
     , post





************************************************************************************************
* Lockdowns
************************************************************************************************
use "stores/matchedinquiries.dta", clear
*Fix a couple
replace after_lockdown=0 if Round<=10 & after_lockdown==.
replace after_lockdown=1 if Round==24 & after_lockdown==.

gen before=(after_lockdown==0)
gen after=(after_lockdown==1)

foreach race in Minority Hispanic AfAm {
  foreach period in before after {
    gen `race'_`period'=`race'*`period'    
  }
}

encode Type, gen(house_type)
encode CBSA, gen(city)



reghdfe choice  AfAm_reg*   Hispanic_reg* if before ==1 , absorb(gender education_level inquiry_order   Address) cl(CBSA_downtown) level(90)  nocons
estadd scalar obs = `e(N)'

sum choice if White==1 & reg1==1 & before==1
loc loc_res_w_midwest=r(mean)
estadd scalar res_w_midwest = `loc_res_w_midwest', replace 
sum choice if White==1 & reg2==1 & before==1
loc loc_res_w_northeast=r(mean)
estadd scalar res_w_northeast = `loc_res_w_northeast', replace 
sum choice if White==1 & reg3==1 & before==1
loc loc_res_w_south=r(mean) 
estadd scalar res_w_south = `loc_res_w_south', replace 
sum choice if White==1 & reg4==1 & before==1
loc loc_res_w_west=r(mean)
estadd scalar res_w_west = `loc_res_w_west', replace 


estadd local gender = "Yes", replace 
estadd local edu = "Yes", replace 
estadd local order = "Yes", replace 
estadd local address = "Yes", replace 
estimates store modelbefore



forvalues i = 1/`geog'{
  sum choice if White==1 & reg`i'==1 & before==1
  loc mean`i' `r(mean)'
}


    nlcom (RR_AfAm_reg1: _b[AfAm_reg1]/`mean1') ///
          (RR_AfAm_reg2: _b[AfAm_reg2]/`mean2') ///
          (RR_AfAm_reg3: _b[AfAm_reg3]/`mean3') ///
          (RR_AfAm_reg4: _b[AfAm_reg4]/`mean4') ///
          (RR_Hispanic_reg1: _b[Hispanic_reg1]/`mean1') ///
          (RR_Hispanic_reg2: _b[Hispanic_reg2]/`mean2') ///
          (RR_Hispanic_reg3: _b[Hispanic_reg3]/`mean3') ///
          (RR_Hispanic_reg4: _b[Hispanic_reg4]/`mean4') ///
     , post
estimates store modelbeforeRR


reghdfe choice   AfAm_reg*   Hispanic_reg* if after==1, absorb(gender education_level inquiry_order   Address) cl(CBSA_downtown) level(90)  nocons
estadd scalar obs = `e(N)'

sum choice if White==1 & reg1==1 & after==1
loc loc_res_w_midwest=r(mean)
estadd scalar res_w_midwest = `loc_res_w_midwest', replace 
sum choice if White==1 & reg2==1 & after==1
loc loc_res_w_northeast=r(mean)
estadd scalar res_w_northeast = `loc_res_w_northeast', replace 
sum choice if White==1 & reg3==1 & after==1
loc loc_res_w_south=r(mean)
estadd scalar res_w_south = `loc_res_w_south', replace 
sum choice if White==1 & reg4==1 & after==1
loc loc_res_w_west=r(mean)
estadd scalar res_w_west = `loc_res_w_west', replace 


estadd local gender = "Yes", replace 
estadd local edu = "Yes", replace 
estadd local order = "Yes", replace 
estadd local address = "Yes", replace 

estimates store modelafter



forvalues i = 1/`geog'{
  sum choice if White==1 & reg`i'==1 & after==1
  loc mean`i' `r(mean)'
}

    nlcom (RR_AfAm_reg1: _b[AfAm_reg1]/`mean1') ///
          (RR_AfAm_reg2: _b[AfAm_reg2]/`mean2') ///
          (RR_AfAm_reg3: _b[AfAm_reg3]/`mean3') ///
          (RR_AfAm_reg4: _b[AfAm_reg4]/`mean4') ///
          (RR_Hispanic_reg1: _b[Hispanic_reg1]/`mean1') ///
          (RR_Hispanic_reg2: _b[Hispanic_reg2]/`mean2') ///
          (RR_Hispanic_reg3: _b[Hispanic_reg3]/`mean3') ///
          (RR_Hispanic_reg4: _b[Hispanic_reg4]/`mean4') ///
     , post
estimates store modelafterRR


loc geog = 4

*Diference
forvalues i = 1/`geog'{
  foreach race in Minority Hispanic AfAm {
    foreach period in after {
      gen `period'_`race'_reg`i'=`race'_reg`i'*`period'    
    }
  }
}

reghdfe choice AfAm_reg* after_AfAm_reg*    Hispanic_reg*  after_Hispanic_reg*, absorb(gender education_level inquiry_order Address)  cl(CBSA_downtown) level(90) nocons
*need to be added manually


forvalues i = 1/`geog'{
  sum choice if White==1 & reg`i'==1 & before==0
  loc mean`i' `r(mean)'
}

 nlcom (RR_after_AfAm_reg1: _b[after_AfAm_reg1]/`mean1') ///
          (RR_after_AfAm_reg2: _b[after_AfAm_reg2]/`mean2') ///
          (RR_after_AfAm_reg3: _b[after_AfAm_reg3]/`mean3') ///
          (RR_after_AfAm_reg4: _b[after_AfAm_reg4]/`mean4') ///
          (RR_after_Hispanic_reg1: _b[after_Hispanic_reg1]/`mean1') ///
          (RR_after_Hispanic_reg2: _b[after_Hispanic_reg2]/`mean2') ///
          (RR_after_Hispanic_reg3: _b[after_Hispanic_reg3]/`mean3') ///
          (RR_after_Hispanic_reg4: _b[after_Hispanic_reg4]/`mean4') ///
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
          modelbeforeRR
          modelafterRR
         using "${path_tables}/table_region.tex", 
         style(tex) 
         cells(b(star fmt(4)) se(par fmt(4) )  )  
         label 
          noobs
         mlabels(,none)  
         nonumbers
         collabels(,none) 
         eqlabels(,none)
         varlabels(
                   RR_AfAm_reg1  "African American $\times$ Midwest"
                   RR_AfAm_reg2  "African American $\times$ Northeast"
                   RR_AfAm_reg3  "African American $\times$ South"
                   RR_AfAm_reg4  "African American $\times$ West"
                   RR_Hispanic_reg1  "Hispanic/LatinX $\times$ Midwest"
                   RR_Hispanic_reg2  "Hispanic/LatinX $\times$ Northeast"
                   RR_Hispanic_reg3  "Hispanic/LatinX $\times$ South"
                   RR_Hispanic_reg4  "Hispanic/LatinX $\times$ West") 
         starl(* 0.1 ** 0.05 *** 0.01)   
         keep(RR_AfAm_reg*  RR_Hispanic_reg*)
         order(RR_AfAm_reg*  RR_Hispanic_reg*)
         prehead( 
              \begin{table}[H]
          \scriptsize \centering
          \begin{threeparttable}
          \captionsetup{justification=centering}
            \caption{Response Rates by US Regions}
            \label{tab:response_rates_regions}

          \begin{tabular}{@{\extracolsep{5pt}} lcccccc} 
          \\[-1.8ex]\hline 
          \hline \\[-1.8ex] 
          & \multicolumn{2}{c}{\it Dependent variable:} \\
          & \multicolumn{2}{c}{\it  Response} \\
           \cline{2-3}

          \\[-1.8ex] & Full & Drop Month After   & pvalue & \multicolumn{2}{c}{Lockdowns} & pvalue\\
          \\[-1.8ex] & Sample & G. Floyd Homicide  & (1)-(2) & Before & After &  (5)-(6) \\
          \\[-1.8ex] & (1) & (2) & (3) & (4) & (5) & (6) \\
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
          modelbefore
          modelafter
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
         nonumbers
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

