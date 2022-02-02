##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("sf","ggplot2","maps","ggthemes","stringr","haven","dplyr","forcats","RColorBrewer")
lapply(pkg, require, character.only=T)
rm(pkg)


#WD
setwd("~/Dropbox/Research/cbsa_discrimination/github/Racial_Discrimination_US_Rental_Market/")

#load daa
dta<-readRDS("stores/temp/data_maps_RR.RDS")


#Results by Regions
reg<-dta %>% dplyr::select(starts_with("region"))
reg<- reg %>% distinct(.keep_all = TRUE)


#Results by Division
div<-dta %>% dplyr::select(starts_with("div"))
div<- div %>% distinct(.keep_all = TRUE)



# # -----------------------------------------------------------------------
#Prepare Us Map
# # -----------------------------------------------------------------------
#us_states <- map_data("state")
us_states <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
us_states$state<-tolower(us_states$ID)
source('scripts/Rscripts/aux/1_crosswalks_for_regions.R')
colnames(st_crosswalk)[colnames(st_crosswalk)=="state"]<-"state_abb"
colnames(us_states)[colnames(us_states)=="region"]<-"state"
st_crosswalk$state<-tolower(st_crosswalk$state_name)
us_states<-left_join(us_states,st_crosswalk)
rm(st_crosswalk)



reg<- reg %>% mutate(region_response_afam=region_afam_coef,
                     region_response_hispanic=region_hispanic_coef)


reg$region_response_afam_fct<-paste0(reg$region," (",format(round(reg$region_response_afam,2), nsmall = 2),")")
reg$region_response_afam_fct<-forcats::fct_reorder(reg$region_response_afam_fct,reg$region_response_afam)

reg$region_response_hispanic_fct<-paste0(reg$region," (",format(round(reg$region_response_hispanic,2), nsmall = 2),")")
reg$region_response_hispanic_fct<-forcats::fct_reorder(reg$region_response_hispanic_fct,reg$region_response_hispanic)


us_states<-left_join(us_states,reg)


#generate factors ordered for the plots
div<-div %>% mutate(response_afam=div_afam_coef,
                    response_hispanic=div_hispanic_coef)



div$response_afam_fct<-paste0(div$division," (",format(round(div$response_afam,2), nsmall = 2),")")
div$response_afam_fct<-forcats::fct_reorder(div$response_afam_fct,div$response_afam)


div$response_hispanic_fct<-paste0(div$division," (",format(round(div$response_hispanic,2), nsmall = 2),")")
div$response_hispanic_fct<-forcats::fct_reorder(div$response_hispanic_fct,div$response_hispanic)



us_states<-left_join(us_states,div)

# Plo Maps ----------------------------------------------------------------
dta_min<-data.frame(dta)
dta_min<-zap_formats(dta_min)

dta_min<- dta_min %>% mutate(response_cbsas_afam=cbsas_afam_coef,
                             response_cbsas_hispanic=cbsas_hispanic_coef)


source("scripts/Rscripts/aux/2_plot_maps.R")

#  data_cbsa<-data.frame(dta_min)
# fill_map="region_response_afam_fct"
# cbsa_size="response_cbsas_afam"
# map<-us_states
# breaks_cbsa <- round(as.numeric(quantile(data_cbsa[,cbsa_size],na.rm=TRUE)),2)
# name_scale_map="Response Rates Relative to Whites Regions"
# name_scale_cbsa="Response Rates Relative to Whites CBSAs"
# geography="region"

plot_maps(dta_min,fill_map="region_response_afam_fct",cbsa_size="response_cbsas_afam",
          geography="region",
          save=TRUE,path="views/",name_output ="region_cbsas_afam_RR" )
plot_maps(dta_min,fill_map="region_response_hispanic_fct",cbsa_size="response_cbsas_hispanic",
          geography="region",
          save=TRUE,path="views/",name_output ="region_cbsas_hispanic_RR" )



plot_maps(dta_min,fill_map="response_afam_fct",cbsa_size="response_cbsas_afam",
          geography="division",
          save=TRUE,path="views/",name_output ="division_cbsas_afam_RR" )
plot_maps(dta_min,fill_map="response_hispanic_fct",cbsa_size="response_cbsas_hispanic",
          geography="division",
          save=TRUE,path="views/",name_output ="division_cbsas_hispanic_RR" )



