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
use "stores/matchedinquiries_infoUSA_wru.dta"

drop if _merge==1

gen white_infoUSA=(race_renter=="white")
gen black_infoUSA=(race_renter=="black")
gen hispanic_infoUSA=(race_renter=="hispanic")

foreach i in White AfAm Hispanic{
    gen `i'_White=`i'*white_infoUSA 

    gen `i'_Black=`i'*black_infoUSA 

    gen `i'_Hispanic=`i'*hispanic_infoUSA 
    }


gen same_race=(race_renter=="white"  & race==1)
replace same_race=1 if race_renter=="black"  & race==3
replace same_race=1 if race_renter=="hispanic"  & race==2



foreach i in White AfAm Hispanic{
    gen `i'_same_race=`i'*same_race 

    }


*Esta si o si
clogit choice    AfAm_same_race  Hispanic_same_race AfAm Hispanic  /// 
            i.gender i.education_level i.inquiry_order   , group(Address)   cl(CBSA_downtown) level(90) or
estimates store model0

#delimit ; 
esttab model0
       using "${path_tables}/infoUSA_clogit_wru.tex",
       style(tex) 
       eform(1)
       cells(b(star fmt(4)) ci(par fmt(4) par(( , )))  ) 
       label 
       mlabels( ,none)  
       nonumbers
       collabels(,none) 
       eqlabels(,none) 
        varlabels(AfAm_same_race     "African American x Same Race Renter"
                  Hispanic_same_race "Hispanic/LatinX  x Same Race Renter"
                  AfAm               "African American"
                  Hispanic           "Hispanic/LatinX"
                )
       keep( AfAm_same_race  Hispanic_same_race AfAm Hispanic)              
       order( AfAm_same_race  Hispanic_same_race AfAm Hispanic)   
       starl(* 0.1 ** 0.05 *** 0.01)   
       level(90) 
       prehead( 

\begin{table}[H]
\footnotesize \centering
\begin{threeparttable}
\captionsetup{justification=centering}
  \caption{}
  \label{tab:dads}

\begin{tabular}{@{\extracolsep{5pt}} lc} 
\\[-1.8ex]\hline 
\hline \\[-1.8ex] 

& \multicolumn{1}{c}{\it  Odds Ratio} \\

\cline{2}\\ [-1.8ex]


&(1)                            \\
       )
       posthead(\hline) 
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







egen median_whiterenters= xtile(whiterentersshare2014_2018),  n(2)  

*below
clogit choice    AfAm_same_race  Hispanic_same_race AfAm Hispanic  /// 
            i.gender i.education_level i.inquiry_order  if median_whiterenters==1 , group(Address)   cl(CBSA_downtown) level(90) or
estimates store model1

*above
clogit choice    AfAm_same_race  Hispanic_same_race AfAm Hispanic  /// 
            i.gender i.education_level i.inquiry_order  if median_whiterenters==2 , group(Address)   cl(CBSA_downtown) level(90) or            
estimates store model2


#delimit ; 
esttab model1
        model2
       using "${path_tables}/infoUSA_clogit_wru_above_below.tex",
       style(tex) 
       eform(1 1)
       cells(b(star fmt(4)) ci(par fmt(4) par(( , )))  ) 
       label 
       mlabels( ,none)  
       nonumbers
       collabels(,none) 
       eqlabels(,none) 
       starl(* 0.1 ** 0.05 *** 0.01)   
       level(90) 
       prehead( 

\begin{table}[H]
\footnotesize \centering
\begin{threeparttable}
\captionsetup{justification=centering}
  \caption{}
  \label{tab:probhighexposure}

\begin{tabular}{@{\extracolsep{5pt}} lcc} 
\\[-1.8ex]\hline 
\hline \\[-1.8ex] 

& \multicolumn{2}{c}{\it  Odds Ratio} \\
& \multicolumn{2}{c}{\it White Renter Share:} \\
\cline{2-3}\\ [-1.8ex]
&Below Median              & Above Median                   \\

&(1)              & (2)                   \\
       )
       posthead(\hline) 
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





*******

egen median_whiteshare= xtile(whiteshare2014_2018),  n(2)  

*below
clogit choice    AfAm_same_race  Hispanic_same_race AfAm Hispanic  /// 
            i.gender i.education_level i.inquiry_order  if median_whiteshare==1 , group(Address)   cl(CBSA_downtown) level(90) or
estimates store model3

*above
clogit choice    AfAm_same_race  Hispanic_same_race AfAm Hispanic  /// 
            i.gender i.education_level i.inquiry_order  if median_whiteshare==2 , group(Address)   cl(CBSA_downtown) level(90) or            
estimates store model4


#delimit ; 
esttab model3
        model4
       using "${path_tables}/infoUSA_clogit_wru_above_below_population.tex",
       style(tex) 
       eform(1 1)
       cells(b(star fmt(4)) ci(par fmt(4) par(( , )))  ) 
       label 
       mlabels( ,none)  
       nonumbers
       collabels(,none) 
       eqlabels(,none) 
       starl(* 0.1 ** 0.05 *** 0.01)   
       level(90) 
       prehead( 

\begin{table}[H]
\footnotesize \centering
\begin{threeparttable}
\captionsetup{justification=centering}
  \caption{}
  \label{tab:probhighexposure}

\begin{tabular}{@{\extracolsep{5pt}} lcc} 
\\[-1.8ex]\hline 
\hline \\[-1.8ex] 

& \multicolumn{2}{c}{\it  Odds Ratio} \\
& \multicolumn{2}{c}{\it White Population Share:} \\
\cline{2-3}\\ [-1.8ex]
&Below Median              & Above Median                   \\

&(1)              & (2)                   \\
       )
       posthead(\hline) 
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




