##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("tidyverse","here","stringr","sf","haven")
lapply(pkg, require, character.only=T)
rm(pkg)



# read data ---------------------------------------------------------------
addresses<-read_csv(here("stores/cbsa_addresses.csv"))
zipcodes<-as.list(unique(addresses$Zip_Code))
files<-list.files("stores/infoUSA/",pattern = "Zip")
files<-files[grepl(".rds",files)]
files<-as.list(files)

matched<-lapply(files,function(x) readRDS(here("stores/infoUSA",x)))
matched<-lapply(matched,st_drop_geometry)
matched<-do.call(rbind,matched)

matched<-matched %>% filter(match_score==0)



# Add Race variable to infoUSA --------------------------------------------
source("scripts/infoUSA/gen_demog_infousa.R")

infoUSA<-gen_demog_infousa(matched)


# There are multiple with exact match  --------------------------------------------------
#Maybe change latter
# matched<- matched %>% group_by(address) %>% mutate(n=n())
# matched$n<-NULL

#keep only renters (bad Idea)
#matched<- matched %>% filter(renter==1)

#Keep first observation
infoUSA<- infoUSA %>% mutate(n=1)
infoUSA<- infoUSA %>% 
  group_by(address) %>%
  mutate(n=cumsum(n))

infoUSA<- infoUSA %>% filter(n==1) %>% dplyr::select(-n,-.groups)

#View(infoUSA %>% select(address,address_infoUSA,HOUSE_NUM,HOUSE_NUM_FRACTION, STREET_PRE_DIR, STREET_NAME, STREET_POST_DIR, STREET_SUFFIX,UNIT_TYPE, UNIT_NUM,Zip_Code,match_score))

infoUSA<- infoUSA %>% dplyr::select(address,Zip_Code,black,hispanic,white,ethnicity)
colnames(infoUSA)<-c("Address","Zip_Code","black_infoUSA","hispanic_infoUSA",'white_infoUSA',"ethnicity_infoUSA")
write_dta(infoUSA,here("stores/infoUSA.dta"))

# addresses<-addresses %>% dplyr::select(-time_inquiry_sent_out,-Latitude,-Longitude)
# addresses<-addresses %>% mutate(merge=1)


# Merge to matched inquiries ----------------------------------------------


dta_inquiries<-read_dta(here("stores/matchedinquiries.dta"))
dta<- dta_inquiries %>% mutate(Address=toupper(Address))
dta<-left_join(dta,infoUSA)
table(dta$white_infoUSA)

write_dta(dta,here("stores/matchedinquiries_infoUSA.dta"))
