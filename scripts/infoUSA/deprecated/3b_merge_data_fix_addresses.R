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


# Gen Demographics ---------------------------------------------------------
source("scripts/infoUSA/_gen_demog_infousa.R")
infoUSA<-gen_demog_infousa(matched)

# # -----------------------------------------------------------------------
# # Exact Match -----------------------------------------------------------
# # -----------------------------------------------------------------------
infoUSA0<-infoUSA %>% filter(match_score==0)

# # -----------------------------------------------------------------------
# # Filter those that match_score== 1  -----------------------------
# # -----------------------------------------------------------------------

infoUSA1<-infoUSA %>% filter(match_score==1) %>% dplyr::select(address,address_infoUSA,HOUSE_NUM,HOUSE_NUM_FRACTION, STREET_PRE_DIR, STREET_NAME, STREET_POST_DIR, STREET_SUFFIX,UNIT_TYPE, UNIT_NUM,CITY,STATE,Zip_Code,black,hispanic,white,ethnicity,match_score)

#View(matched1 %>% dplyr::select(address,address_infoUSA,HOUSE_NUM,HOUSE_NUM_FRACTION, STREET_PRE_DIR, STREET_NAME, STREET_POST_DIR, STREET_SUFFIX,UNIT_TYPE, UNIT_NUM,Zip_Code,match_score))

#First Filter, keep those that have the exact address number
infoUSA1<- tidyr::extract(infoUSA1, address,into="num_trulia",regex="([0-9]{1,4})",remove=FALSE)
infoUSA1<- tidyr::extract(infoUSA1, HOUSE_NUM,into="HOUSE_NUM_only_number",regex="([0-9]{1,4})",remove=FALSE)
infoUSA1<- infoUSA1 %>% filter(num_trulia==HOUSE_NUM_only_number)


#Second filter, unit number
infoUSA1<- tidyr::extract(infoUSA1, address,into="unit_num_trulia",regex="#(.*)",remove=FALSE)
infoUSA1<- infoUSA1 %>% mutate(unit_num_trulia=str_remove(unit_num_trulia,"#"))
#remove dashes from unit numer
infoUSA1<- infoUSA1 %>% mutate(unit_num_trulia=str_remove(unit_num_trulia,"-"))

#View(infoUSA1 %>% dplyr::select(address,address_infoUSA,unit_num_trulia, UNIT_NUM,HOUSE_NUM_only_number,HOUSE_NUM, STREET_NAME, STREET_SUFFIX,Zip_Code,match_score))

infoUSA1a<- infoUSA1 %>% filter(unit_num_trulia==UNIT_NUM)
infoUSA1b<- infoUSA1 %>% filter(unit_num_trulia!=UNIT_NUM)
#View(infoUSA1b %>% dplyr::select(address,address_infoUSA,unit_num_trulia, UNIT_NUM,HOUSE_NUM_only_number,HOUSE_NUM, STREET_NAME, STREET_SUFFIX,Zip_Code,match_score))

infoUSA1b<- tidyr::extract(infoUSA1b, unit_num_trulia,into="unit_num_trulia_only",regex="([0-9]{1,4})",remove=FALSE)
infoUSA1b<- infoUSA1b %>% filter(unit_num_trulia_only==UNIT_NUM) 
#View(infoUSA1b %>% dplyr::select(address,address_infoUSA,unit_num_trulia,unit_num_trulia_only, UNIT_NUM,HOUSE_NUM_only_number,HOUSE_NUM, STREET_NAME, STREET_SUFFIX,Zip_Code,match_score))

#infoUSA with score of 1 done
infoUSA1a<- infoUSA1a %>% dplyr::select(address,address_infoUSA,HOUSE_NUM,HOUSE_NUM_FRACTION, STREET_PRE_DIR, STREET_NAME, STREET_POST_DIR, STREET_SUFFIX,UNIT_TYPE, UNIT_NUM,CITY,STATE,Zip_Code,black,hispanic,white,ethnicity,match_score)
infoUSA1b<- infoUSA1b %>% dplyr::select(address,address_infoUSA,HOUSE_NUM,HOUSE_NUM_FRACTION, STREET_PRE_DIR, STREET_NAME, STREET_POST_DIR, STREET_SUFFIX,UNIT_TYPE, UNIT_NUM,CITY,STATE,Zip_Code,black,hispanic,white,ethnicity,match_score)

infoUSA1<-bind_rows(infoUSA1a,infoUSA1b)
rm(infoUSA1a,infoUSA1b)

# # -----------------------------------------------------------------------
# # Filter those that match_score== 2  -----------------------------
# # -----------------------------------------------------------------------

infoUSA2<-infoUSA %>% filter(match_score==2) %>% dplyr::select(address,address_infoUSA,HOUSE_NUM,HOUSE_NUM_FRACTION, STREET_PRE_DIR, STREET_NAME, STREET_POST_DIR, STREET_SUFFIX,UNIT_TYPE, UNIT_NUM,CITY,STATE,Zip_Code,black,hispanic,white,ethnicity,match_score)

#First Filter, keep those that have the exact address number
infoUSA2<- tidyr::extract(infoUSA2, address,into="num_trulia",regex="([0-9]{1,4})",remove=FALSE)
infoUSA2<- tidyr::extract(infoUSA2, HOUSE_NUM,into="HOUSE_NUM_only_number",regex="([0-9]{1,4})",remove=FALSE)
infoUSA2<- infoUSA2 %>% filter(num_trulia==HOUSE_NUM_only_number)

#Second filter, unit number
infoUSA2<- tidyr::extract(infoUSA2, address,into="unit_num_trulia",regex="#(.*)",remove=FALSE)
infoUSA2<- infoUSA2 %>% mutate(unit_num_trulia=str_remove(unit_num_trulia,"#"))
#remove dashes from unit numer
infoUSA2<- infoUSA2 %>% mutate(unit_num_trulia=str_remove(unit_num_trulia,"-"))

infoUSA2a<- infoUSA2 %>% filter(unit_num_trulia==UNIT_NUM)
infoUSA2b<- infoUSA2 %>% filter(unit_num_trulia!=UNIT_NUM)
#View(infoUSA2b %>% dplyr::select(address,address_infoUSA,unit_num_trulia, UNIT_NUM,HOUSE_NUM_only_number,HOUSE_NUM, STREET_NAME, STREET_SUFFIX,Zip_Code,match_score))

infoUSA2b<- tidyr::extract(infoUSA2b, unit_num_trulia,into="unit_num_trulia_only",regex="([0-9]{1,4})",remove=FALSE)
infoUSA2b<- infoUSA2b %>% filter(unit_num_trulia_only==UNIT_NUM) 

infoUSA2<-bind_rows(infoUSA2a,infoUSA2b)
rm(infoUSA2a,infoUSA2b)

# # -----------------------------------------------------------------------
# # Filter those that match_score above 2  -----------------------------
# # -----------------------------------------------------------------------
infoUSA3<-infoUSA %>% filter(match_score>2) %>% dplyr::select(address,address_infoUSA,HOUSE_NUM,HOUSE_NUM_FRACTION, STREET_PRE_DIR, STREET_NAME, STREET_POST_DIR, STREET_SUFFIX,UNIT_TYPE, UNIT_NUM,CITY,STATE,Zip_Code,black,hispanic,white,ethnicity,match_score)


#First Filter, keep those that have the exact address number
infoUSA3<- tidyr::extract(infoUSA3, address,into="num_trulia",regex="([0-9]{1,4})",remove=FALSE)
infoUSA3<- tidyr::extract(infoUSA3, HOUSE_NUM,into="HOUSE_NUM_only_number",regex="([0-9]{1,4})",remove=FALSE)
infoUSA3<- infoUSA3 %>% filter(num_trulia==HOUSE_NUM_only_number)
#Second filter, unit number
infoUSA3<- tidyr::extract(infoUSA3, address,into="unit_num_trulia",regex="#(.*)",remove=FALSE)
infoUSA3<- infoUSA3 %>% mutate(unit_num_trulia=str_remove(unit_num_trulia,"#"))
#remove dashes from unit numer
infoUSA3<- infoUSA3 %>% mutate(unit_num_trulia=str_remove(unit_num_trulia,"-"))

infoUSA3<- infoUSA3 %>% filter(unit_num_trulia==UNIT_NUM)

#View(infoUSA3 %>% dplyr::select(address,address_infoUSA,unit_num_trulia, UNIT_NUM,HOUSE_NUM_only_number,HOUSE_NUM, STREET_NAME, STREET_SUFFIX,Zip_Code,match_score,black,hispanic,white,match_score))



# # -----------------------------------------------------------------------
# bind all 3 --------------------------------------------------------------
# # -----------------------------------------------------------------------
infoUSA0<- infoUSA0 %>% dplyr::select(address,address_infoUSA,HOUSE_NUM,HOUSE_NUM_FRACTION, STREET_PRE_DIR, STREET_NAME, STREET_POST_DIR, STREET_SUFFIX,UNIT_TYPE, UNIT_NUM,CITY,STATE,Zip_Code,black,hispanic,white,ethnicity,match_score)
infoUSA1<- infoUSA1 %>% dplyr::select(address,address_infoUSA,HOUSE_NUM,HOUSE_NUM_FRACTION, STREET_PRE_DIR, STREET_NAME, STREET_POST_DIR, STREET_SUFFIX,UNIT_TYPE, UNIT_NUM,CITY,STATE,Zip_Code,black,hispanic,white,ethnicity,match_score)
infoUSA2<- infoUSA2 %>% dplyr::select(address,address_infoUSA,HOUSE_NUM,HOUSE_NUM_FRACTION,ethnicity, STREET_PRE_DIR, STREET_NAME, STREET_POST_DIR, STREET_SUFFIX,UNIT_TYPE, UNIT_NUM,CITY,STATE,Zip_Code,black,hispanic,white,match_score)
infoUSA3<- infoUSA3 %>% dplyr::select(address,address_infoUSA,HOUSE_NUM,HOUSE_NUM_FRACTION,ethnicity, STREET_PRE_DIR, STREET_NAME, STREET_POST_DIR, STREET_SUFFIX,UNIT_TYPE, UNIT_NUM,CITY,STATE,Zip_Code,black,hispanic,white,match_score)


infoUSAbind<-bind_rows(infoUSA0,infoUSA1,infoUSA2,infoUSA3)




# dta from string merge ---------------------------------------------------

load(here("stores/infoUSA/infoUSA_strings.rds"))

strmerged<-do.call(rbind,zips)

#First Filter, keep those that have the exact address number
strmerged<- tidyr::extract(strmerged, address,into="num_trulia",regex="([0-9]{1,4})",remove=FALSE)
strmerged<- tidyr::extract(strmerged, HOUSE_NUM,into="HOUSE_NUM_only_number",regex="([0-9]{1,4})",remove=FALSE)
strmerged<- strmerged %>% filter(num_trulia==HOUSE_NUM_only_number)
#Second filter, unit number
strmerged<- tidyr::extract(strmerged, address,into="unit_num_trulia",regex="#(.*)",remove=FALSE)
strmerged<- strmerged %>% mutate(unit_num_trulia=str_remove(unit_num_trulia,"#"))
#remove dashes from unit numer
strmerged<- strmerged %>% mutate(unit_num_trulia=str_remove(unit_num_trulia,"-"))
strmerged<- strmerged %>% filter(unit_num_trulia==UNIT_NUM)


strmerged<- strmerged %>% filter(!(address%in%infoUSAbind$address))



#View(strmerged %>% dplyr::select(address,unit_num_trulia, UNIT_NUM,HOUSE_NUM_only_number,HOUSE_NUM, STREET_NAME, STREET_SUFFIX,Zip_Code,match_score,match_score))

strmerged<-gen_demog_infousa(strmerged)


# # -----------------------------------------------------------------------
# Bind with infoUSA data
# # -----------------------------------------------------------------------
strmerged<-strmerged %>% dplyr::select(address,HOUSE_NUM,HOUSE_NUM_FRACTION, STREET_PRE_DIR, STREET_NAME, STREET_POST_DIR, STREET_SUFFIX,UNIT_TYPE, UNIT_NUM,CITY,STATE,Zip_Code,black,hispanic,white,match_score,ethnicity)
infoUSAbind<- infoUSAbind %>% dplyr::select(address,HOUSE_NUM,HOUSE_NUM_FRACTION, STREET_PRE_DIR, STREET_NAME, STREET_POST_DIR, STREET_SUFFIX,UNIT_TYPE, UNIT_NUM,CITY,STATE,Zip_Code,black,hispanic,white,match_score,ethnicity)


dta_match<-bind_rows(strmerged,infoUSAbind)
# # -----------------------------------------------------------------------
# Fix multiple observations
# # -----------------------------------------------------------------------

dta_match<- dta_match %>% mutate(n=1)
dta_match<- dta_match %>% 
  group_by(address) %>%
  mutate(n=cumsum(n))

dta_match<- dta_match %>% filter(n==1) %>% dplyr::select(-n)

#View(dta %>% select(address,HOUSE_NUM,HOUSE_NUM_FRACTION, STREET_PRE_DIR, STREET_NAME, STREET_POST_DIR, STREET_SUFFIX,UNIT_TYPE, UNIT_NUM,Zip_Code,match_score))

dta_match<- dta_match %>% dplyr::select(address,Zip_Code,black,hispanic,white,ethnicity)
colnames(dta_match)<-c("Address","Zip_Code","black_infoUSA","hispanic_infoUSA",'white_infoUSA',"ethnicity_infoUSA")




# Merge to matched inquiries ----------------------------------------------


dta_inquiries<-read_dta(here("stores/matchedinquiries.dta"))
dta<- dta_inquiries %>% mutate(Address_orig=Address,
                               Address=toupper(Address))
dta<-left_join(dta,dta_match)



write_dta(dta,here("stores/matchedinquiries_infoUSA.dta"))
