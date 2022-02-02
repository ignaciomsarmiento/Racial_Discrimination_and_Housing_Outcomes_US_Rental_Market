# Racial Discrimination in the United States Rental Market

Data and replication files for "Racial Discrimination in the United States Rental Market" by  [Christensen](https://www.uiuc-bdeep.org/christensen),  [Sarmiento-Barbieri](https://ignaciomsarmiento.github.io/) and  [Timmins](https://sites.duke.edu/christophertimmins/)

# Abstract

TBD



## Data files

- matchedinquiries

## Code files:

- The analysis is conducted using Stata-16 version 16.1 and R version 4.0.2 (2020-06-22) software

- All the code was run on a MacBookPro 2020 running macOS Big Sur Version 11.0.1

 
- main.sh: Contains the sequence of execution of dofiles and Rscripts to produce the figures and tables in the paper and appendix. Dofiles and Rscripts are named according to the function they perform
	- The folder dofiles contains the dofiles called by main.sh:
		- 1_estimates_figure2.do
		
	- The folder Rscripts contains the Rscripts called by main.sh:
		
		
	- main.sh creates:
		- temporary auxiliary data frames that are saved in stores/aux and deleted at the end of the script
		- a folder called logs is created and contains log files: .log for Stata  and .Rout for R


Figures and tables are saved in the "views" folder. 



 
## Data dictionary

- matchedinquiries:

	- choice                                                =1 if response and property available
	- Minority                                                                     =1 if Minority
	- White                                                                           =1 if White
	- Black                                                                =1 if African American
	- Hispanic                                                              =1 if Hispanic/LatinX
	- quartileZIP_property                                Quartile Within Zip Toxic Concentration
	- dec2                                              =1 if 0-25 Within Zip Toxic Concentration
	- dec3                                             =1 if 25-75 Within Zip Toxic Concentration
	- dec4                                            =1 if 75-100 Within Zip Toxic Concentration
	- dist                                                        Distance (miles) to closest TRI
	- within1                                                     =1 within 1 mile to closest TRI
	- more1                                                    =1 more than 1 mile to closest TRI
	- race                                                                          Race Identity
	- Minority_dec2                          =1 if Minority x 0-25 Within Zip Toxic Concentration
	- Minority_dec3                         =1 if Minority x 25-75 Within Zip Toxic Concentration
	- Minority_dec4                        =1 if Minority x 75-100 Within Zip Toxic Concentration
	- Minority_within1                              =1 if Minority x within 1 mile to closest TRI
	- Minority_more1                                  =1 if Minority x more 1 mile to closest TRI
	- Black_dec2                     =1 if African American x 0-25 Within Zip Toxic Concentration
	- Black_dec3                    =1 if African American x 25-75 Within Zip Toxic Concentration
	- Black_dec4                   =1 if African American x 75-100 Within Zip Toxic Concentration
	- Black_within1                         =1 if African American x within 1 mile to closest TRI
	- Black_more1                             =1 if African American x more 1 mile to closest TRI
	- Hispanic_dec2                   =1 if Hispanic/LatinX x 0-25 Within Zip Toxic Concentration
	- Hispanic_dec3                  =1 if Hispanic/LatinX x 25-75 Within Zip Toxic Concentration
	- Hispanic_dec4                 =1 if Hispanic/LatinX x 75-100 Within Zip Toxic Concentration
	- Hispanic_within1                       =1 if Hispanic/LatinX x within 1 mile to closest TRI
	- Hispanic_more1                           =1 if Hispanic/LatinX x more 1 mile to closest TRI
	- gender                                                                      Gender Identity
	- education_level                                             Mother Education Level Identity
	- order                                                                    Order Inquiry Sent
	- inquiry_weekday 										 Day of the week inquiry was sent out
	- Address                                                           Property Address (masked)
	- Zip_Code                                                                  ZIP Code (masked)
	- sample                                                                       =1 main sample
	- times_zip                                                 =1 times we run experiment in zip
	- sample_two_inquiries                      =1 subsample of homes with two round of inquiries
	- matched_sample                                                            =1 matched sample
	- timestamp_inquiry_sent_out                                      Time stamp Inquiry Sent Out
	- timestamp_response_received                                     Time stamp Inquiry Received
	- person_or_computer                                  if response was by a person or computer
	- toxconc                                                            RSEI Toxic Concentration
	- scorecancer                                                               RSEI Cancer Score
	- scorenoncancer                                                        RSEI Non Cancer Score
	- type                                                                       Type of Property
	- sqft                                                                                  Sqft.
	- bedroom_max                                                                        Bedrooms
	- bathroom_max                                                                      Bathrooms
	- assault                                                                             Assault
	- groceries                                                                         Groceries
	- rent                                                                                   Rent
	- povrate                                                                        Poverty Rate
	- blackshare                                                        Share of African American
	- whiteshare                                                                  Share of Whites
	- hispanicshare                                                            Share of Hispanics
	- pop                                                               Population in Block Group
	- college                                                           Share of College Educated
	- unemployed                                                                Unemployment Rate

- ACS_population:

	- race                                                                           Race Renters
	- population_renters                                     Population of Renters in Block Group
	- quartileZIP_property                                Quartile Within Zip Toxic Concentration
	- Zip_Code                                                                           ZIP Code

