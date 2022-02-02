##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("tidyverse","here","stringr","haven")
lapply(pkg, require, character.only=T)
rm(pkg)



# read data ---------------------------------------------------------------
load(here("stores/infoUSA_strings_not_unit_vTAM.rds"))
matched<-do.call(rbind,zips)
rm(zips,zipcodes,matcher)

#View(matched %>% dplyr::select(dir,address,num_trulia,HOUSE_NUM,street_name_trulia, STREET_NAME, unit_num_trulia,UNIT_NUM,match_score))


# # -----------------------------------------------------------------------
#Problem is that several don't have unit numbers
# # -----------------------------------------------------------------------
#Start with those with unit numbers
unit<- matched %>% filter(UNIT_NUM!="")

#Those that match unit number and street but don't have street prefix
unit_m<- matched %>% filter(UNIT_NUM==unit_num_trulia)
#no street prefix
unit_m_np<- unit_m %>% filter(STREET_PRE_DIR=="")
unit_m_np<- unit_m_np %>% filter(STREET_NAME==street_name_trulia)

#with street prefix
unit_m_p<- unit_m %>% filter(STREET_PRE_DIR!="")

unit_m_p<- unit_m_p %>% mutate(street_name_trulia_no_pre=str_remove(street_name_trulia,"^(E|N|NW|S|W)"))
unit_m_p<- unit_m_p %>%tidyr::extract(street_name_trulia, into="street_pre_trulia",regex="^(E|N|NW|S|W)",remove=FALSE)

unit_m_p<- unit_m_p %>% mutate(street_name_trulia_no_pre=trimws(street_name_trulia_no_pre),
                               street_pre_trulia=trimws(street_pre_trulia))

unit_m_p<- unit_m_p %>% filter(street_name_trulia_no_pre==STREET_NAME) %>% select(-street_pre_trulia,-street_name_trulia_no_pre)

unit<- rbind(unit_m_np,unit_m_p) #matched and ready
#length(unique(unit_m_np$StreetAddress  )) #280
#length(unique(unit_m_p$StreetAddress)) #16
rm(unit_m,unit_m_p,unit_m_np)

# # -----------------------------------------------------------------------  


#No unit number
#Fix this
nounit<- matched %>% filter(UNIT_NUM=="")

nounit<- nounit %>% mutate(street_name_trulia_no_pre=str_remove(street_name_trulia,"^(E|N|NW|S|W)"))
nounit<- nounit %>%tidyr::extract(street_name_trulia, into="street_pre_trulia",regex="^(E|N|NW|S|W)",remove=FALSE)
 
nounit<- nounit %>% mutate(street_name_trulia_no_pre=trimws(street_name_trulia_no_pre),
                                street_pre_trulia=trimws(street_pre_trulia))

nounit<- nounit %>% filter(street_name_trulia_no_pre==STREET_NAME) #%>% select(-street_pre_trulia,-street_name_trulia_no_pre)

nounit<- nounit %>% mutate(STREET_PRE_DIR=ifelse(STREET_PRE_DIR=="",NA,STREET_PRE_DIR))
nounit<- nounit %>% mutate(indicator=ifelse(street_pre_trulia==STREET_PRE_DIR,1,0))

nounit1<- nounit %>% filter(indicator!=0) %>% dplyr::select(-indicator,-street_pre_trulia,-street_name_trulia_no_pre)
nounit2<- nounit %>% filter(is.na(indicator)) %>% dplyr::select(-indicator,-street_pre_trulia,-street_name_trulia_no_pre)

# View(nounit1 %>% dplyr::select(dir,address,num_trulia,HOUSE_NUM,street_name_trulia,street_name_trulia_no_pre, STREET_NAME, unit_num_trulia,UNIT_NUM,street_pre_trulia, STREET_PRE_DIR,match_score))
# View(nounit2 %>% dplyr::select(dir,address,num_trulia,HOUSE_NUM,street_name_trulia,street_name_trulia_no_pre, STREET_NAME, unit_num_trulia,UNIT_NUM,street_pre_trulia, STREET_PRE_DIR,match_score))

nounit<- rbind(nounit1,nounit2) #matched and ready
rm(nounit1,nounit2)

 
matched1<-rbind(unit,nounit)
#length(unique(matched1$StreetAddress)) #817
rm(unit,nounit)

matched1$ZIP 
View(matched1 %>% dplyr::select(StreetAddress,address,STREET_NAME,num_trulia,HOUSE_NUM,State,STATE,Zip_Code,ZIP, unit_num_trulia,UNIT_NUM,match_score))#,street_name_trulia,street_name_trulia_no_pre, STREET_NAME, unit_num_trulia,UNIT_NUM,street_pre_trulia, STREET_PRE_DIR,match_score))


saveRDS(matched1,here("stores/infoUSA_no_units_aprox.rds"))
