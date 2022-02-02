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

duke<-readRDS(here("stores/infoUSA_matched_duke_server.Rds"))
tam<-readRDS(here("stores/infoUSA_matched_TAM_db.Rds"))
nounit<-readRDS(here("stores/infoUSA_no_units_aprox.rds"))

# tam_add<- tam %>% distinct(StreetAddress,Zip)  %>% rename(address=StreetAddress,Zip_Code=Zip) %>% mutate(address=toupper(address)) 
# dim(tam_add)[1]/dim(adresses)[1]
# duke_add<- duke %>% distinct(address,Zip_Code) %>% mutate(address=toupper(address)) 
# dim(duke_add)[1]/dim(adresses)[1]
# nounit_add<- nounit %>% distinct(StreetAddress,Zip_Code)  %>% rename(address=StreetAddress) %>% mutate(address=toupper(address)) 
# 
# coded<-rbind(duke_add,tam_add,nounit_add) %>% distinct()
# dim(coded)[1]/dim(adresses)[1]
# rm(tam_add,duke_add,nounit_add)

duke<-duke %>%  dplyr::select(address,Zip_Code,last_name_1,Ethnicity_Code_1,STATE,census_county_2010) 
tam<-tam %>%  dplyr::select(StreetAddress,Zip,last_name_1,Ethnicity_Code_1,STATE,census_county_2010)%>% 
              rename(address=StreetAddress,
                     Zip_Code=Zip)
nounit<-nounit %>%  dplyr::select(StreetAddress,Zip_Code,last_name_1,Ethnicity_Code_1,STATE,census_county_2010)%>% 
  rename(address=StreetAddress) %>% 
  mutate(address=toupper(address))

info_usa<-rbind(duke,tam,nounit) %>% 
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
write_dta(info_usa_sum,here("stores/infoUSA_wru.dta"))

