##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("tidyverse","here","stringr","sf","stringdist","fuzzyjoin")
lapply(pkg, require, character.only=T)
rm(pkg)




# Load Data ---------------------------------------------------------------


addresses<-read_csv(here("stores/cbsa_addresses.csv"))
zipcodes<-as.list(unique(addresses$Zip_Code))

#ZP<-zipcodes[[1]]



matcher<-function(ZP){
  #infoUSA<-read_delim(here(paste0("stores/Household_Ethnicity_zip_",ZP,"_year_2020.txt")),
  infoUSA<-read_delim(here(paste0("//oit-nas-fe11.oit.duke.edu/ssri-infousa/derived set/2020/Household_Ethnicity_zip_",ZP,"_year_2020.txt")),
                  delim="\t",
                   col_types=c(FamilyID="c",
                               location_type="c",
                               primary_family_ind="c",
                               HouseholdStatus="c",
                               tradeline_count="c",
                               head_hh_age_code="c",
                               length_of_residence="c",
                               ChildrenHHCount="c",
                               children_ind="c",
                               AddressType="c",
                               mailability_score="c",
                               wealth_finder_score="c",
                               find_div_1000="c",
                               owner_renter_status="c",
                               estmtd_home_val_div_1000="c",
                               marital_status="c",
                               ppi_div_1000="c",
                               CBSACode="c",
                               CBSAType="c",
                               CSACode="c",
                               LOCATIONID="c",
                               HOUSE_NUM="c",
                               HOUSE_NUM_FRACTION="c",
                               STREET_PRE_DIR="c",
                               STREET_NAME="c",
                               STREET_POST_DIR="c",
                               STREET_SUFFIX="c",
                               UNIT_TYPE="c",
                               UNIT_NUM="c",
                               BOX_TYPE="c",
                               BOX_NUM="c",
                               ROUTE_TYPE="c",
                               ROUTE_NUM="c",
                               CITY="c",
                               STATE="c",
                               ZIP="c",
                               ZIP4="c",
                               dpbc="c",
                               vacant="c",
                               USPSNOSTATS="c",
                               census_state_2010="c",
                               census_county_2010="c",
                               GE_LATITUDE_2010="c",
                               GE_LONGITUDE_2010="c",
                               GE_CENSUS_LEVEL_2010="c",
                               middle_name="c",
                               last_name_1="c",
                               Ethnicity_Code_1="c",
                               first_name_2="c",
                               last_name_2="c",
                               Ethnicity_Code_2="c",
                               first_name_3="c",
                               last_name_3="c",
                               Ethnicity_Code_3="c"
                               ),
                                na=" ",
                  skip_empty_rows=TRUE) 
  
  
  
  
  
  # Create Vars -------------------------------------------------------------
  address_in_zip<-addresses %>% filter(Zip_Code==ZP) %>% 
                  mutate(address_orig=Address) %>% 
                  rename(address=Address) %>% 
                  mutate(address=toupper(address))
  
  address_in_zip<- tidyr::extract(address_in_zip, address,into="num_trulia",regex=,remove=FALSE)
  address_in_zip<- tidyr::extract(address_in_zip, address,into="unit_num_trulia",regex="#(.*)",remove=FALSE)
  address_in_zip<- tidyr::extract(address_in_zip,address, into="st_suffix_trulia",regex="(AVE|BLVD|CT|DR|ST)",remove=FALSE)
  address_in_zip<- address_in_zip %>% mutate(street_name_trulia=str_remove(address,"([0-9]{1,4})"),
                                             street_name_trulia=str_remove(street_name_trulia,"#(.*)"),
                                             street_name_trulia=str_remove(street_name_trulia,"(AVE|BLVD|CT|DR|ST)"),
                                             unit_num_trulia=str_remove(unit_num_trulia,"-"),
                                             unit_num_trulia=ifelse(is.na(unit_num_trulia),"",unit_num_trulia)
                                             )
  
  infoUSA$STREET_NAME<-trimws(infoUSA$STREET_NAME)
  address_in_zip$street_name_trulia<-trimws(address_in_zip$street_name_trulia)
  
  matched<- stringdist_left_join(address_in_zip,infoUSA,by=c("street_name_trulia"="STREET_NAME"), max_dist = 2,
  distance_col = "match_score")
  
  matched<- matched %>% filter(num_trulia==HOUSE_NUM)
  matched<- matched %>% filter(unit_num_trulia==UNIT_NUM)
  #View(matched %>% dplyr::select(address,num_trulia,HOUSE_NUM,street_name_trulia, STREET_NAME, unit_num_trulia,UNIT_NUM,match_score))
  #x<- readRDS(here("stores/infoUSA/Zip_94118.rds"))

  
  return(matched)
}  


zips<-lapply(zipcodes,matcher)

save.image(here("stores/infoUSA_strings.rds"))
