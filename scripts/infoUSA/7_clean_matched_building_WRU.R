##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("tidyverse","here","wru")
lapply(pkg, require, character.only=T)
rm(pkg)


# Keep not matched --------------------------------------------------------
adresses<-read_csv(here("stores/infoUSA/ATM_geocoder.csv"))

dta<-readRDS(here("stores/infoUSA_matched_TAM_db_building.Rds"))


dta<-dta %>%  dplyr::select(StreetAddress,Zip,last_name_1,Ethnicity_Code_1,STATE,census_county_2010)%>% 
              rename(address=StreetAddress,
                     Zip_Code=Zip)

info_usa<-dta %>% 
            distinct(.keep_all = TRUE) %>% 
                     rename(surname=last_name_1,
                                     state=STATE,
                                     county=census_county_2010)


info_usa_wru<-predict_race(info_usa,census.geo="county",census.key="3a00e320695ae68fac70e8f87616425d4a794f17")

info_usa_sum<- info_usa_wru %>% 
                mutate(address=toupper(address),
                       Zip_Code=str_pad(Zip_Code,width=5,side = "left",pad = "0")) %>% 
                group_by(address,Zip_Code) %>% 
                summarize(pred.whi=sum(pred.whi),
                          pred.bla=sum(pred.bla),
                          pred.his=sum(pred.his),
                          pred.asi=sum(pred.asi),
                          pred.oth=sum(pred.oth),
                          .groups="drop"
                          )
dim(info_usa_sum)[1]/dim(adresses)[1]


info_usa_sum<- info_usa_sum %>% 
              mutate(max_p=pmax(pred.whi,pred.bla, pred.his,pred.asi,pred.oth))

info_usa_sum<- info_usa_sum %>% 
                mutate(race_renter=case_when(pred.whi==max_p~"white",
                                             pred.bla==max_p~"black",
                                             pred.his==max_p~"hispanic",
                                             TRUE ~ "other"
                                             ))
prop.table(table(info_usa_sum$race_renter))


info_usa_sum<- info_usa_sum %>% dplyr::select(address,Zip_Code,race_renter) %>% rename(StreetAddress=address)
write_dta(info_usa_sum,here("stores/infoUSA_wru_building.dta"))

