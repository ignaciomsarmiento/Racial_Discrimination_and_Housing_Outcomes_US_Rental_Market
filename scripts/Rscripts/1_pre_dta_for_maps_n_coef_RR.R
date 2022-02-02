##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("sf","ggplot2","maps","ggthemes","stringr","haven","dplyr","forcats")
lapply(pkg, require, character.only=T)
rm(pkg)


#WD
setwd("~/Dropbox/Research/cbsa_discrimination/github/Racial_Discrimination_US_Rental_Market/")

#load daa
dta<-readRDS("stores/aggregated_cbsas.rda")
dta<-st_drop_geometry(dta)
dta<- dta %>% mutate(population=str_remove_all(`2012 estimate`,","))
dta<- dta %>% mutate(population=as.numeric(population))
sum(dta$population)/(328.2*1e6) #%pop of the sample

dta<- dta %>% dplyr::select(cbsatitle,
                            state,
                            main_city_lon,
                            main_city_lat,
                            diss_m_w,
                            diss_b_w,
                            diss_h_w,
                            iss_m_w,
                            iss_b_w,
                            iss_h_w)
                     


cbsas<-read_dta("stores/matrices/cbsa_single_regression_RR.dta")
cbsas<-zap_label(cbsas)
cbsas<-zap_formats(cbsas)
cbsas<-data.frame(cbsas)
colnames(cbsas)
cbsas<- cbsas %>% mutate(CBSA=str_remove_all(CBSA,"CBSA__"),
                        n=as.numeric(n))
colnames(cbsas)[1:12]<-paste0("cbsas_",colnames(cbsas)[1:12])

#Merge Results by CBSA
dta$n<-seq(1:50)
dta<-left_join(dta,cbsas)
#View(dta[,c("cbsatitle","CBSA")]) #check good match
dta$CBSA<-NULL
rm(cbsas)



# #Add crosswalk regions --------------------------------------------------

source('scripts/Rscripts/aux/1_crosswalks_for_regions.R')
colnames(st_crosswalk)[colnames(st_crosswalk)=="state"]<-"state_abb"

dta<-left_join(dta,st_crosswalk,by=c("state"="state_name"))
dta$state_abb[dta$state=="District of Columbia"]<-"DC"
dta$region[dta$state=="District of Columbia"]<-"South"
dta$division[dta$state=="District of Columbia"]<-"South Atlantic"




#Add Results by Regions
reg<-read_dta("stores/matrices/region_single_regression_RR.dta")
reg$region<-str_replace_all(reg$CBSA,"region__","")
reg$n<-NULL
reg$CBSA<-NULL
colnames(reg)[1:7]<-paste0("region_",colnames(reg)[1:7])

dta<-left_join(dta,reg)




#Add Results by Divisions
div<-read_dta("stores/matrices/division_single_regression_RR.dta")
div$division<-str_remove_all(div$CBSA,"division__")
div$CBSA<-NULL
div$n<-NULL
div$division<-str_replace_all(div$division,"_"," ")
colnames(div)[1:7]<-paste0("div_",colnames(div)[1:7])
dta<-left_join(dta,div)


saveRDS(dta,"stores/temp/data_maps_RR.RDS")




