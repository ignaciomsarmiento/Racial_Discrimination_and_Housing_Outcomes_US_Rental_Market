##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("tidyverse","here","stringr","sf","stringdist")
lapply(pkg, require, character.only=T)
rm(pkg)

# Functions ---------------------------------------------------------------

lonlat2UTM = function(lonlat) {
  utm = (floor((lonlat[1] + 180) / 6) %% 60) + 1
  if(lonlat[2] > 0) {
    utm + 32600
  } else{
    utm + 32700
  }
}


# Load Data ---------------------------------------------------------------


addresses<-read_csv(here("stores/cbsa_addresses.csv"))
zipcodes<-as.list(unique(addresses$Zip_Code))

#ZP<-zipcodes[[2]]
matcher<-function(ZP){
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
                                na=" ") 
  
  
  
  
  
  # Create Vars -------------------------------------------------------------
  address_in_zip<-addresses %>% filter(Zip_Code==ZP) %>% 
                  rename(address=Address) %>% 
                  mutate(address=toupper(address))
  
  infoUSA<-infoUSA%>%
    mutate(UNIT_TYPE=ifelse(UNIT_TYPE=="APT","#",UNIT_TYPE),
           unit=paste0(UNIT_TYPE, UNIT_NUM),
            address_infoUSA=paste(HOUSE_NUM,HOUSE_NUM_FRACTION, STREET_PRE_DIR, STREET_NAME, STREET_POST_DIR, STREET_SUFFIX, unit,sep=" "),
           address_infoUSA=gsub("\\s+"," ",address_infoUSA),
           address_infoUSA=trimws(address_infoUSA))
  
  #View(infoUSA %>% dplyr::select(address_infoUSA,HOUSE_NUM,HOUSE_NUM_FRACTION, STREET_PRE_DIR, STREET_NAME, STREET_POST_DIR, STREET_SUFFIX, UNIT_TYPE, UNIT_NUM))
  # Put in spatial terms ----------------------------------------------------
  
  #Put in spatial addresses matched inquiries
  address_in_zip<-st_as_sf(address_in_zip, coords = c("Longitude", "Latitude"), crs = 4326,remove=FALSE)
  
  #Reproject flat
  EPSG_2_UTM <- lonlat2UTM(st_coordinates(address_in_zip[1,]))
  address_in_zip <- st_transform(address_in_zip, EPSG_2_UTM)
  
  # Run the buffer. 50meters
  buf_proj <- st_buffer(address_in_zip, dist = 100)
  
  #Put in spatial infoUSA
  infoUSA_sf<-st_as_sf(infoUSA, coords = c("GE_LONGITUDE_2010", "GE_LATITUDE_2010"), crs = 4326,remove=FALSE)
  infoUSA_sf <- st_transform(infoUSA_sf, EPSG_2_UTM)
  
  #Properties within 20 meters
  buf_infoUSA<-st_join(buf_proj,infoUSA_sf, join = st_intersects)
  
  #Check
  #View(buf_infoUSA %>% dplyr::select(address,address_infoUSA))
  # # x<-infoUSA %>% filter(address=="1 BLUE ORCHARD DR")
  # # y<-address_in_zip %>% filter(Address=="1 BLUE ORCHARD DR")
  # require("leaflet")
  # buf_proj2 <-st_transform(buf_proj,crs = 4326)
  # address_in_zip2<-st_transform(address_in_zip,crs = 4326)
  # infoUSA_sf2<-st_transform(infoUSA_sf,crs = 4326)
  # leaflet() %>%
  #     addProviderTiles("Esri.WorldImagery") %>%
  #     addMarkers(data=address_in_zip2) %>%
  #     addPolygons(data=buf_proj2, color = "yellow",weight = 4, smoothFactor = 1,
  #                 opacity = 1, fillOpacity = 0) %>% 
  #     addMarkers(data=infoUSA_sf2)
  # 
  
  
  # First Clean up of matches -----------------------------------------------
  high_matches<- buf_infoUSA %>% mutate(match_score=stringdist(address,address_infoUSA,method="lv"))
  #View(high_matches%>% dplyr::select(address,address_infoUSA,match_score))
  
  high_matches<- high_matches %>%
                group_by(address) %>% 
                mutate(min_matchscore=min(match_score),.groups="drop")# %>% 
                #filter(match_score==min_matchscore)
  high_matches2<- high_matches %>%filter(match_score==min_matchscore)
  
  
  if(sum(address_in_zip$address%in%high_matches2$address)==length(address_in_zip$address)){
    saveRDS(high_matches2,here("stores/",paste0("Zip_",ZP,".rds")))
  }else{
    writeLines(c("no go"), here("stores/",paste0("Zip_",ZP,".text")))
  }
  
  return(high_matches2)
}  


zips<-lapply(zipcodes,matcher)

save.image(here("stores/infoUSA.rds"))


#View(zips[[1]]%>% dplyr::select(address,address_infoUSA,match_score))
