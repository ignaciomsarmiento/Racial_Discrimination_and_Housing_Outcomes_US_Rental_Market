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



# read data from geografic binding (duke server) ---------------------------------------------------------------
addresses<-read_csv(here("stores/cbsa_addresses.csv"))
zipcodes<-as.list(unique(addresses$Zip_Code))
files<-list.files("stores/infoUSA/",pattern = "Zip")
files<-files[grepl(".rds",files)]
files<-as.list(files)

matched<-lapply(files,function(x) readRDS(here("stores/infoUSA",x)))
matched<-lapply(matched,st_drop_geometry)
infoUSA<-do.call(rbind,matched)



# # -----------------------------------------------------------------------
# # Exact Match -----------------------------------------------------------
# # -----------------------------------------------------------------------
infoUSA0<-infoUSA %>% filter(match_score==0)

# # -----------------------------------------------------------------------
# # Filter those that match_score== 1  -----------------------------
# # -----------------------------------------------------------------------

infoUSA1<-infoUSA %>% filter(match_score==1)

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
infoUSA1<-bind_rows(infoUSA1a,infoUSA1b)
rm(infoUSA1a,infoUSA1b)

# # -----------------------------------------------------------------------
# # Filter those that match_score== 2  -----------------------------
# # -----------------------------------------------------------------------
infoUSA2<-infoUSA %>% filter(match_score==2) 

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
infoUSA3<-infoUSA %>% filter(match_score>2) 


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
infoUSAbind<-bind_rows(infoUSA0,infoUSA1,infoUSA2,infoUSA3)




# dta from string merge (duke server)---------------------------------------------------

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




# # -----------------------------------------------------------------------
# Bind with infoUSA data
# # -----------------------------------------------------------------------
dta_match<-bind_rows(strmerged,infoUSAbind)


saveRDS(dta_match,here("stores/infoUSA_matched_duke_server.Rds"))
