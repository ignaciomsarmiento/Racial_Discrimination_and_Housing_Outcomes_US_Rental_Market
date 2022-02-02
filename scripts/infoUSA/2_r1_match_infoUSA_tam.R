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



# read data to match ---------------------------------------------------------------
addresses<-read.csv(here("stores/infoUSA/ATM_geocoder_procesed.csv"))
addresses<- addresses %>% mutate(Zip_Code=str_pad(Zip, 5, pad = "0"))
zipcodes<-as.list(unique(addresses$Zip_Code))
#ZP<-zipcodes[[11]]


matcher<-function(ZP){
  
  infoUSA<-readRDS(here(paste0("stores/infoUSA/raw/Zip_",ZP,".rds")))
  

# Create Vars -------------------------------------------------------------
  address_in_zip<-addresses %>% filter(Zip_Code==ZP)
    
  address_in_zip<- address_in_zip %>%  mutate(SuiteNumber=str_remove(SuiteNumber,"#"))
  #View(address_in_zip %>% dplyr::select(StreetAddress,StreetName,StreetNumber,StreetPreDirectional,StreetType,SuiteNumber))
  address_in_zip<- address_in_zip %>% filter(!(is.na(StreetNumber)))
  matched<- stringdist_left_join(address_in_zip,infoUSA,by=c("StreetName"="STREET_NAME",
                                                             "StreetNumber"="HOUSE_NUM",
                                                             "StreetPreDirectional"="STREET_PRE_DIR",
                                                             "StreetType"="STREET_SUFFIX",
                                                             "SuiteNumber"="UNIT_NUM"
                                                             ), max_dist = 2, distance_col = "match_score")
  
  matched <-tryCatch(
  { matched %>% filter(StreetName.match_score==0,
                                StreetNumber.match_score==0,
                                StreetPreDirectional.match_score==0,
                                StreetType.match_score==0,
                                SuiteNumber.match_score==0) 
  }, error= function(e) {
      return(NA)
    })
  
  #View(matched1 %>% select(StreetAddress,StreetName,STREET_NAME,StreetNumber,HOUSE_NUM,SuiteNumber,UNIT_NUM, StreetPreDirectional, STREET_PRE_DIR, StreetType, STREET_SUFFIX, SuiteNumber, UNIT_NUM, ends_with("match_score")))
  
  return(matched)
}  


zips<-lapply(zipcodes,matcher)
save.image(here("stores/infoUSA_matched_TAM.Rda"))

zips1<- zips[!is.na(zips)]
zips1<- do.call(rbind,zips1)

saveRDS(zips1,here("stores/infoUSA_matched_TAM_db.Rds"))
length(unique(zips1$StreetAddress))

4495/8476


