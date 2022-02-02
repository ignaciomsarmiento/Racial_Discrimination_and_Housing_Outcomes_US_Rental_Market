##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("sf","ggplot2","maps","ggthemes","stringr","haven","dplyr","forcats","RColorBrewer","here","tidyr")
lapply(pkg, require, character.only=T)
rm(pkg)


#load daa
dta<-readRDS(here("stores/temp/data_maps_RR.RDS"))


#Results by Regions
reg<-dta %>% dplyr::select(starts_with("region"))
reg<- reg %>% distinct(.keep_all = TRUE)


#Results by Division
div<-dta %>% dplyr::select(starts_with("div"))
div<- div %>% distinct(.keep_all = TRUE)



# # -----------------------------------------------------------------------
#Prepare Us Map
# # -----------------------------------------------------------------------
# #us_states <- map_data("state")
# us_states <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
#maps from https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
us_states<-read_sf(here("stores/shapefiles/cb_2018_us_state_20m/cb_2018_us_state_20m.shp"))

us_states<- us_states %>% filter(!(STUSPS%in%c("DC","HI","AK","PR")))
# plot(us_states[,"NAME"])
# us_regions<-read_sf(here("stores/shapefiles/cb_2018_us_state_20m/cb_2018_us_state_20m.shp"))
# plot(us_regions[,"NAME"])

us_states$state_abb<-us_states$STUSPS
source(here('../../scripts/github_codes/2_analysis/0_prepara_merge/0b_crosswalks_for_regions.R'))
colnames(st_crosswalk)[colnames(st_crosswalk)=="state"]<-"state_abb"
us_states<-left_join(us_states,st_crosswalk)
rm(st_crosswalk)




dta$region_response_afam_fct<-paste0(dta$region," (",format(round(dta$region_afam_coef,2), nsmall = 2),")")
dta$region_response_afam_fct<-forcats::fct_reorder(dta$region_response_afam_fct,dta$region_afam_coef)

dta$region_response_hispanic_fct<-paste0(dta$region," (",format(round(dta$region_hispanic_coef,2), nsmall = 2),")")
dta$region_response_hispanic_fct<-forcats::fct_reorder(dta$region_response_hispanic_fct,dta$region_hispanic_coef)

#plot(us_states[,"region"])
# Map  ----------------------------------------------------------------
dta_min<-data.frame(dta)
data_cbsa<-zap_formats(dta_min)
map<-us_states

blacks<- data_cbsa %>% dplyr::select(region,region_afam_coef) %>% distinct(.keep_all = TRUE)
hispanics<- data_cbsa %>% dplyr::select(region,region_hispanic_coef) %>% distinct(.keep_all = TRUE)
border<-map  %>% 
  group_by(region) %>%
  summarize() %>% ungroup()



blacks<-blacks %>% mutate(race="African American")
colnames(blacks)[2]<-"RR"
hispanics<-hispanics %>% mutate(race="Hispanic/LatinX")
colnames(hispanics)[2]<-"RR"

b_h<-rbind(blacks,hispanics)
ncols <- 100

b_h <- b_h %>% mutate(position=ceiling(RR*100/min(RR)))
b_h <- b_h %>% mutate(position=ceiling(position*ncols/100))

#Idea from https://sites.google.com/site/seascapemodelling/home/r-code/customising-plots/continous-colour-brewer-palettes
or_reds <- brewer.pal(9, 'OrRd')
newcol <- colorRampPalette(or_reds)

or_reds2 <- newcol(ncols)#apply the function to get 100 colours
or_reds2<-data.frame(position=abs(row_number(or_reds2)-ncols)+1,colors=or_reds2)
b_h<-left_join(b_h,or_reds2) %>% dplyr::select(region,race,colors)


b_h<- b_h %>% pivot_wider(names_from = race, values_from = colors)
colnames(b_h)<-c("region","AA_reg_color","Hisp_reg_color")

data_cbsa<-left_join(data_cbsa,b_h)

data_cbsa<-st_as_sf(data_cbsa,coords=c("main_city_lon","main_city_lat"))
st_crs(data_cbsa)<-st_crs(map)
data_cbsa<-st_transform(data_cbsa,crs=st_crs(map))

#put in albers projection
map<-st_transform(map,crs=5070)
border<-st_transform(border,crs=5070)
data_cbsa<-st_transform(data_cbsa,crs=5070)


breaks_cbsa_black <- as.numeric(quantile(st_drop_geometry(data_cbsa[,"cbsas_afam_coef"]),probs=seq(0,1,.1),na.rm=TRUE),2)
breaks_cbsa_black <- round(breaks_cbsa_black,2)
breaks_cbsa_black


# African American --------------------------------------------------------
ggplot(data_cbsa) +
  geom_sf(data=map,aes(geometry=geometry),color="gray48",fill="white", size = 0.2) +
  geom_sf(data=border,aes(geometry=geometry), fill=NA, color="black",size = .8) +
  geom_sf(aes(geometry=geometry,size = cbsas_afam_coef,col=AA_reg_color),alpha=1) +
  scale_colour_identity(name="Response Rates Relative to Whites Regions",labels =names(table(st_drop_geometry(data_cbsa[,"region_response_afam_fct"]))) ,guide = "legend",na.translate = F) +
  scale_size(name="Response Rates Relative to Whites CBSAs",trans="reverse",breaks = breaks_cbsa_black) +
  guides(color = guide_legend(order = 1),
         size  = guide_legend(order = 0),
         fill  = guide_legend(order = 2)) +
  theme_map() +
  theme(plot.margin=grid::unit(c(0,0,0,0), "cm"),
        legend.position="right",
        legend.key = element_rect(fill = "white"),
        text = element_text(size=10),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
ggsave(filename="views/region_cbsas_afam_RR_dots.png", height = 5.28, width = 13.1)



# Hispanic/LatinX ---------------------------------------------------------

ggplot(data_cbsa) +
  geom_sf(data=map,aes(geometry=geometry),color="gray48",fill="white", size = 0.2) +
  geom_sf(data=border,aes(geometry=geometry), fill=NA, color="black",size = .8) +
  geom_sf(aes(geometry=geometry,size = cbsas_hispanic_coef,col=Hisp_reg_color),alpha=1) +
  scale_colour_identity(name="Response Rates Relative to Whites Regions",labels =names(table(st_drop_geometry(data_cbsa[,"region_response_hispanic_fct"]))) ,guide = "legend",na.translate = F) +
  scale_size(name="Response Rates Relative to Whites CBSAs",trans="reverse",breaks = breaks_cbsa_black) +
  guides(color = guide_legend(order = 1),
         size  = guide_legend(order = 0),
         fill  = guide_legend(order = 2)) +
  theme_map() +
  theme(plot.margin=grid::unit(c(0,0,0,0), "cm"),
        legend.position="right",
        legend.key = element_rect(fill = "white"),
        text = element_text(size=10),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
ggsave(filename="views/region_cbsas_hispanic_RR_dots.png", height = 5.28, width = 13.1)
