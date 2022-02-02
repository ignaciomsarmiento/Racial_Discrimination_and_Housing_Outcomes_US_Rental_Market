##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################


#create a crosswalk
st_crosswalk <- tibble(state = state.name) %>%
  bind_cols(tibble(abb = state.abb)) 

#generate regions
#https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf
st_crosswalk$region<-NA
st_crosswalk$region[st_crosswalk$state%in%c("Maine","New Hampshire", "Vermont", "Massachusetts", "Rhode Island", "Connecticut", "New York", "New Jersey",  "Pennsylvania")]<-"Northeast"
st_crosswalk$region[st_crosswalk$state%in%c("Ohio", "Michigan", "Indiana", "Wisconsin", "Illinois", "Minnesota", "Iowa", "Missouri", "North Dakota", "South Dakota", "Nebraska",  "Kansas")]<-"Midwest"
st_crosswalk$region[st_crosswalk$state%in%c("Delaware", "Maryland", "Virginia", "West Virginia", "Kentucky", "North Carolina", "South Carolina", "Tennessee", "Georgia", "Florida", "Alabama", "Mississippi", "Arkansas", "Louisiana", "Texas",  "Oklahoma")]<-"South"
st_crosswalk$region[st_crosswalk$state%in%c("Montana", "Idaho", "Wyoming", "Colorado", "New Mexico", "Arizona", "Utah", "Nevada", 'California', "Oregon", "Washington", "Alaska", "Hawaii")]<-"West"


st_crosswalk$division<-NA
st_crosswalk$division[st_crosswalk$state%in%c("Maine","New Hampshire", "Vermont", "Massachusetts", "Rhode Island", "Connecticut")]<-"New England"
st_crosswalk$division[st_crosswalk$state%in%c("New York", "New Jersey",  "Pennsylvania")]<-"Middle Atlantic"


st_crosswalk$division[st_crosswalk$state%in%c("Indiana", "Illinois", "Michigan", "Ohio" , "Wisconsin")]<-"East North Central"

st_crosswalk$division[st_crosswalk$state%in%c(  "Iowa",  "Kansas", "Minnesota", "Missouri","Nebraska", "North Dakota", "South Dakota")]<-"West North Central"


st_crosswalk$division[st_crosswalk$state%in%c("Delaware", "Florida", "Georgia", "Maryland", "North Carolina", "South Carolina", "Virginia", "West Virginia")]<-"South Atlantic"
st_crosswalk$division[st_crosswalk$state%in%c("Alabama","Kentucky", "Mississippi",  "Tennessee")]<-"East South Central"
st_crosswalk$division[st_crosswalk$state%in%c("Arkansas", "Louisiana", "Oklahoma", "Texas")]<-"West South Central"


st_crosswalk$division[st_crosswalk$state%in%c("Montana", "Idaho", "Wyoming", "Colorado", "New Mexico", "Arizona", "Utah", "Nevada")]<-"Mountain"
st_crosswalk$division[st_crosswalk$state%in%c("Alaska",'California', "Hawaii", "Oregon", "Washington")]<-"Pacific"


table(st_crosswalk$region,st_crosswalk$division,useNA = "always")
table(st_crosswalk$division,useNA = "always")

colnames(st_crosswalk)[colnames(st_crosswalk)=="state"]<-"state_name"
colnames(st_crosswalk)[colnames(st_crosswalk)=="abb"]<-"state"

