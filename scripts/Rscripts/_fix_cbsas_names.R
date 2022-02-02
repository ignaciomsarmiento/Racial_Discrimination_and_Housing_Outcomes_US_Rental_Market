##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("tidyverse","here","fuzzyjoin")
lapply(pkg, require, character.only=T)
rm(pkg)


#load data
dta<-readRDS(here("stores/temp/data_maps_RR.RDS"))


cbsas<- dta %>% separate(cbsatitle,into=c("cbsa_full","state_st"),sep=",",remove=FALSE)

cbsas<- cbsas %>% mutate(cbsa_full=case_when(cbsa_full=="Sacramento--Roseville--Arden-Arcade"~"Sacramento-Roseville-Arden-Arcade",
                                             cbsa_full=="Nashville-Davidson--Murfreesboro--Franklin"~"Nashville-Davidson-Murfreesboro-Franklin",
                                             TRUE   ~ cbsa_full
                                             ))

cbsas$cbsa_full[cbsas$cbsa_full=="Sacramento--Roseville--Arden-Arcade"]<-"Sacramento-Roseville-Arden-Arcade"
cbsas$cbsa_full[cbsas$cbsa_full=="Nashville-Davidson--Murfreesboro--Franklin"]<-"Nashville-Davidson-Murfreesboro-Franklin"
cbsas<- cbsas %>% separate(cbsa_full,into=c("cbsa1","cbsa2","cbsa3","cbsa4"),sep="-",remove=FALSE)

cbsas<- cbsas %>% mutate(cbsa_st=paste0(cbsa1,",",state_st))

cbsas<- cbsas %>% mutate(cbsa_st= case_when(cbsa_st=="Boston, MA-NH"~"Boston, MA",
                                            cbsa_st=="Cambridge, MA-NH"~"Cambridge, MA",
                                            cbsa_st=="Newton, MA-NH"~"Newton, NH",
                                            cbsa_st=="Charlotte, NC-SC"~"Charlotte, NC",
                                            cbsa_st=="Concord,  NC-SC"~"Concord, NC",
                                            cbsa_st=="Gastonia,  NC-SC"~"Gastonia, NC",
                                            cbsa_st=="Chicago, IL-IN-WI"~"Chicago, IL" ,
                                            cbsa_st=="Naperville, IL-IN-WI"~"Naperville, IL",
                                            cbsa_st=="Elgin, IL-IN-WI"~"Elgin, IL",
                                            cbsa_st=="Cincinnati, OH-KY-IN"~"Cincinnati, OH",
                                            cbsa_st=="Kansas City, MO-KS"~"Kansas City, MO",
                                            cbsa_st=="Louisville, KY-IN"~"Louisville, KY",
                                            cbsa_st=="Jefferson County, KY-IN"~"Jefferson County, KY",
                                            cbsa_st=="Memphis, TN-MS-AR"~"Memphis, TN",
                                            cbsa_st=="Minneapolis, MN-WI"~"Minneapolis, MN",
                                            cbsa_st=="Bloomington, MN-WI"~"Bloomington, MN",
                                            cbsa_st=="New York, NY-NJ-PA"~"New York, NY",
                                            cbsa_st=="Newark, NY-NJ-PA"~"Newark, NJ",
                                            cbsa_st=="Jersey City, NY-NJ-PA"~"Jersey City, NJ",
                                            cbsa_st=="Philadelphia, PA-NJ-DE-MD"~"Philadelphia, PA",
                                            cbsa_st=="Camden, PA-NJ-DE-MD"~"Camden, NJ",
                                            cbsa_st=="Wilmington, PA-NJ-DE-MD"~"Wilmington, DE",
                                            cbsa_st=="Portland, OR-WA"~"Portland, OR",
                                            cbsa_st=="Vancouver, OR-WA"~"Vancouver, WA",
                                            cbsa_st=="Hillsboro, OR-WA"~"Hillsboro, OR",
                                            cbsa_st=="Providence, RI-MA"~"Providence, RI",
                                            cbsa_st=="Warwick, RI-MA"~"Warwick, RI",
                                            cbsa_st=="St. Louis, MO-IL"~"St. Louis, MO",
                                            cbsa_st=="Virginia Beach, VA-NC"~"Virginia Beach, VA",
                                            cbsa_st=="Norfolk, VA-NC"~"Norfolk, VA",
                                            cbsa_st=="Newport News, VA-NC"~"Newport News, VA",
                                            cbsa_st=="Washington, DC-VA-MD-WV"~"Washington, DC",
                                            cbsa_st=="Arlington, DC-VA-MD-WV"~"Arlington, VA",
                                            cbsa_st=="Alexandria, DC-VA-MD-WV"~"Alexandria, VA",
                                            TRUE   ~ cbsa_st))



cbsas<- cbsas %>% dplyr::select(-cbsa2,-cbsa3,-cbsa4,-cbsa_full)


saveRDS(cbsas,here("stores/temp/data_maps_RR.RDS"))

