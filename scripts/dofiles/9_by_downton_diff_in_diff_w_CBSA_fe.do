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

************************************************************************************************
* Minority
************************************************************************************************
est clear

encode Type, gen(house_type)
encode CBSA, gen(city)



reghdfe choice   AfAm  AfAm_Suburb Hispanic  Hispanic_Suburb, absorb(gender education_level inquiry_order  Address) cl(CBSA_downtown) level(90)  nocons
estadd scalar obs = `e(N)'
sum choice if White==1 & Downtown==1
loc loc_res_w_downtown=r(mean)
estadd scalar res_w_downtown = `loc_res_w_downtown', replace 
sum choice if White==1 & Suburb==1
loc loc_res_w_suburb=r(mean)
estadd scalar res_w_suburb = `loc_res_w_suburb', replace 
estadd local gender = "Yes", replace 
estadd local edu = "Yes", replace 
estadd local order = "Yes", replace 
estadd local Address = "Yes", replace 
estadd local CBSA_Round = " ", replace 
estadd local List = " ", replace 

estimates store model1



foreach var in rent  Bedroom_max Bathroom_max  blackrentersshare2014_2018 whiterentersshare2014_2018 hispanicrentersshare2014_2018  medinc2014_2018 house_type{
  drop if `var'==.
  *replace `var' = -99 if `var'==.
  *gen d_`var'=(`var'==-99)
}

reghdfe choice   AfAm  AfAm_Suburb Hispanic  Hispanic_Suburb, absorb(gender education_level inquiry_order   city#Round ) cl(CBSA_downtown) level(90)  nocons
estadd scalar obs = `e(N)'
sum choice if White==1 & Downtown==1
loc loc_res_w_downtown=r(mean)
estadd scalar res_w_downtown = `loc_res_w_downtown', replace 
sum choice if White==1 & Suburb==1
loc loc_res_w_suburb=r(mean)
estadd scalar res_w_suburb = `loc_res_w_suburb', replace 
estadd local gender = "Yes", replace 
estadd local edu = "Yes", replace 
estadd local order = "Yes", replace 
estadd local Address = " ", replace 
estadd local CBSA_Round = "Yes", replace 
estadd local List = " ", replace 

estimates store model2




reghdfe choice   AfAm  AfAm_Suburb Hispanic  Hispanic_Suburb rent  Bedroom_max Bathroom_max  blackrentersshare2014_2018 whiterentersshare2014_2018 hispanicrentersshare2014_2018  medinc2014_2018 ,  absorb(gender education_level inquiry_order   house_type city#Round ) cl(CBSA_downtown) level(90)  nocons
estadd scalar obs = `e(N)'
sum choice if White==1 & Downtown==1
loc loc_res_w_downtown=r(mean)
estadd scalar res_w_downtown = `loc_res_w_downtown', replace 
sum choice if White==1 & Suburb==1
loc loc_res_w_suburb=r(mean)
estadd scalar res_w_suburb = `loc_res_w_suburb', replace 
estadd local gender = "Yes", replace 
estadd local edu = "Yes", replace 
estadd local order = "Yes", replace 
estadd local Address = " ", replace 
estadd local CBSA_Round = "Yes", replace 
estadd local List = "Yes", replace 

estimates store model3

#delimit ; 
esttab model1
       using "${path_tables}/table_downtown_diff_in_diff.tex", 
        noisily
       style(tex) 
       cells(b(star fmt(4)) se(par fmt(4)))   
       label
       stats(res_w_downtown
                res_w_suburb
               gender 
               edu 
               order
               Address
              CBSA_Round
               List
                obs, fmt(2 2  0 0 0 0 0  %9.0gc )
               labels("Mean Response (White) Downtown"
                      "Mean Response (White) Suburb"
                     "\hline Gender" 
                     "Education Level" 
                     "Inquiry Order"
                     "Address FE"
                     "CBSA by Date FE"
                     "Listing and Neighborhood Char"
                     "\hline Observations"
                     )) 
       mlabels(,none)  
       numbers
       collabels(,none) 
       eqlabels(,none)
       varlabels(Minority  "Minority " 
                 Minority_Suburb    "Minority Suburb" 
                 AfAm      "African American " 
                 AfAm_Suburb        "African American Suburb" 
                 Hispanic  "Hispanic/LatinX "   
                 Hispanic_Suburb    "Hispanic/LatinX Suburb" ) 
       starl(* 0.1 ** 0.05 *** 0.01)   
       keep( AfAm  AfAm_Suburb Hispanic  Hispanic_Suburb )
       order( AfAm  AfAm_Suburb Hispanic  Hispanic_Suburb ) 
       level(90)    
       prehead( 
         		\begin{table}[H]
				\footnotesize \centering
				\begin{threeparttable}
				\captionsetup{justification=centering}
				  \caption{Response Rates by Downtown and Suburbs }
				  \label{tab:responserates_Downtown_Suburb}

				\begin{tabular}{@{\extracolsep{5pt}} lc} 
				\\[-1.8ex]\hline 
				\hline \\[-1.8ex] 
				& \multicolumn{1}{c}{\it Dependent variable:} \\
				& \multicolumn{1}{c}{\it  Response} \\
				\cline{2-2}\\ [-1.8ex]
       )
       posthead(\hline) 
       prefoot() 
       postfoot(
          	\\[-1.8ex]\hline 
			\hline \\[-1.8ex] 
			\end{tabular} 

			\begin{tablenotes}[scriptsize,flushleft] \scriptsize
			\item Notes: Table reports coefficients from a within-property linear model including controls for gender, education and order the inquiry was sent. Property and Neighborhood characteristics include ent, Sqft, house type, Bedroom, Bathroom, Assault, perc black renters, perc hispanic renters, perc white renters,  Median Income from the ACS 2014-2018 at the block group level. Standard errors clustered at the CBSA Downtown/Suburb level. 
			\end{tablenotes}
			\end{threeparttable}
			\end{table}
       )
       substitute(\_ _)
       replace;
#delimit cr

*eststo clear

*end  