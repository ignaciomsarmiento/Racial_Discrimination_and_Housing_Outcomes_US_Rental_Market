##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("tidyverse","here","stringr","sf","haven","stringdist","fuzzyjoin")
lapply(pkg, require, character.only=T)
rm(pkg)


# Keep not matched --------------------------------------------------------
# dta<-read_csv(here("stores/infoUSA/ATM_geocoder.csv"))
# 
# duke<-readRDS(here("stores/infoUSA_matched_duke_server.Rds"))
# tam<-readRDS(here("stores/infoUSA_matched_TAM_db.Rds"))
# 
# tam_add<- tam %>% distinct(StreetAddress,Zip)  %>% rename(address=StreetAddress,Zip_Code=Zip) %>% mutate(address=toupper(address)) 
# duke_add<- duke %>% distinct(address,Zip_Code) %>% mutate(address=toupper(address)) 
# coded<-rbind(duke_add,tam_add) %>% distinct()
# 
# dta<- dta %>% mutate(address=toupper(StreetAddress))
# not_matched<- dta %>% filter(!c(address%in%coded$address))
# 
# saveRDS(not_matched,here("stores/infoUSA/not_matched_vTAM.rds"))
# 

# read data to match ---------------------------------------------------------------
addresses<-readRDS(here("stores/infoUSA/not_matched_vTAM.rds"))
addresses<- addresses %>% rename(Zip_Code=Zip)
zipcodes<-as.list(unique(addresses$Zip_Code))


# #The problem here is that info USA doesn't have unit number
# #Examples:
# # 1 Identify the house but Info USA doesn't have the unit number which is 3
# zip1<-readRDS(here("stores/infoUSA/raw/Zip_01420.rds"))
# zip1<-zip1 %>% filter(STREET_NAME=="ELM",
#                     HOUSE_NUM==313)
# 
# # 2 Identify the house but Info USA doesn't have the unit number which is A
# zip2<-readRDS(here("stores/infoUSA/raw/Zip_01760.rds"))
# zip2<-zip2 %>% filter(grepl("CENTRAL",STREET_NAME, ignore.case = TRUE),
#                       HOUSE_NUM==102)
#                   



matcher<-function(ZP){
  
  infoUSA<-readRDS(here(paste0("stores/infoUSA/raw/Zip_",ZP,".rds")))
  

# Create Vars -------------------------------------------------------------
  address_in_zip<-addresses %>% filter(Zip_Code==ZP) 
  
  address_in_zip<- tidyr::extract(address_in_zip, address,into="num_trulia",regex="([0-9]{1,4})",remove=FALSE)
  address_in_zip<- tidyr::extract(address_in_zip, address,into="unit_num_trulia",regex="#(.*)",remove=FALSE)
  address_in_zip<- tidyr::extract(address_in_zip,address, into="st_suffix_trulia",regex="(AVE|BLVD|CT|DR|ST|PKWY|RD|PL|CIR|TRL)",remove=FALSE)
  address_in_zip<- address_in_zip %>% mutate(street_name_trulia=str_remove(address,"([0-9]{1,4})"),
                                             street_name_trulia=str_remove(street_name_trulia,"#(.*)"),
                                             street_name_trulia=str_remove(street_name_trulia,"(AVE|BLVD|CT|DR|ST|PKWY|RD|PL|CIR|TRL)"),
                                             unit_num_trulia=str_remove(unit_num_trulia,"-"),
                                             unit_num_trulia=ifelse(is.na(unit_num_trulia),"",unit_num_trulia)
  )
  
  infoUSA$STREET_NAME<-trimws(infoUSA$STREET_NAME)
  address_in_zip$street_name_trulia<-trimws(address_in_zip$street_name_trulia)
  
  matched<- stringdist_left_join(address_in_zip,infoUSA,by=c("street_name_trulia"="STREET_NAME"), max_dist = 2,
                                 distance_col = "match_score")
  
  matched<- matched %>% filter(num_trulia==HOUSE_NUM)
  
  return(matched)
}  


#x<-matcher(zipcodes[[1]])
zips<-lapply(zipcodes,matcher)

save.image(here("stores/infoUSA_strings_not_unit_vTAM.rds"))



