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



# read data ---------------------------------------------------------------
load(here("stores/infoUSA_strings_not_unit.rds"))
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
# length(unique(unit_m_np$dir)) #280
# length(unique(unit_m_p$dir)) #16
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
#length(unique(nounit$dir)) #1080
rm(nounit1,nounit2)

 
matched1<-rbind(unit,nounit)
#length(unique(matched1$dir)) #1143
rm(unit,nounit)

# # Addressed not matched yet, use geocoder  -----------------------------------------------------------------
#addresses_m<-addresses %>% filter((dir%in%unique(matched1$dir))) #address matched
addresses_no_m<-addresses %>% filter(!(dir%in%unique(matched1$dir))) #addresses yet not matched
#no_m_unit<- matched %>% filter(dir%in%addresses_no_m$dir) $no estoy seguro

#first round
add_TAM_r1<-addresses_no_m %>% dplyr::select(Address, City, State, Zip_Code) 
add_TAM_r1<- add_TAM_r1 %>% distinct()
add_TAM_r1_1<- add_TAM_r1 %>% filter(grepl("AND",Address))
add_TAM_r1_2<- add_TAM_r1 %>% filter(grepl("AND",Address)==FALSE)
add_TAM_r1_2<-add_TAM_r1_2[1:(2499-dim(add_TAM_r1_1)[1]),] #keep 2500 minus the ones that have And

add_TAM_r1<-rbind(add_TAM_r1_1,add_TAM_r1_2)
rm(add_TAM_r1_1,add_TAM_r1_2)

add_TAM_r1<- add_TAM_r1 %>% mutate(UNIQUEID=rownames(.))
colnames(add_TAM_r1)<-c("StreetAddress",	"City",	"State",	"Zip","UNIQUEID")
add_TAM_r1<- add_TAM_r1 %>% dplyr::select("UNIQUEID","StreetAddress",	"City",	"State",	"Zip")
write.csv(add_TAM_r1,here("stores/infoUSA/addresses_TAM_r1.csv"),row.names = FALSE)
