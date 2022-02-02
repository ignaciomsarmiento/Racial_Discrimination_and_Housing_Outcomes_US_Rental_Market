##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("sf","ggplot2","maps","ggthemes","stringr","haven","dplyr","forcats","RColorBrewer","here","tidyr","viridis")
lapply(pkg, require, character.only=T)
rm(pkg)


#load daa
dta<-readRDS(here("stores/temp/data_maps_RR.RDS"))


#Results by Regions
reg<-dta %>% dplyr::select(starts_with("region"))
reg<- reg %>% distinct(.keep_all = TRUE)



reg<- reg %>% mutate(region_afam_coef=region_afam_coef*100,
                     region_hispanic_coef=region_hispanic_coef*100,
                     region_hispanic_uci=region_hispanic_uci*100,
                     region_hispanic_lci=region_hispanic_lci*100,
                     region_afam_uci=region_afam_uci*100,
                     region_afam_lci=region_afam_lci*100
                     )


reg<- reg %>% mutate(region_response_afam_fct=paste0(reg$region,": ",
                                                     format(round(reg$region_afam_coef,1), nsmall = 1),
                                                     " (",format(round(reg$region_afam_uci,1), nsmall = 1),
                                                     ", ",
                                                     format(round(reg$region_afam_lci,1), nsmall = 1),
                                                     ")"),
                     region_response_hispanic_fct=paste0(reg$region,": ",
                                                     format(round(reg$region_hispanic_coef,1), nsmall = 1),
                                                     " (",format(round(reg$region_hispanic_uci,1), nsmall = 1),
                                                     ", ",
                                                     format(round(reg$region_hispanic_lci,1), nsmall = 1),
                                                     ")"),
                     
)


# reg$region_response_afam_fct<-paste0(reg$region," (",format(round(reg$region_response_afam,1), nsmall = 1),")")
# reg$region_response_afam_fct<-forcats::fct_reorder(reg$region_response_afam_fct,reg$region_response_afam)

# reg$region_response_hispanic_fct<-paste0(reg$region,format(round(reg$region_response_hispanic,1), nsmall = 1)," (",format(round(reg$region_res  ,1), nsmall = 1),")")
# reg$region_response_hispanic_fct<-forcats::fct_reorder(reg$region_response_hispanic_fct,reg$region_response_hispanic)




# # -----------------------------------------------------------------------
#Prepare Us Map
# # -----------------------------------------------------------------------
#maps from https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
us_states<-read_sf(here("stores/shapefiles/cb_2018_us_state_20m/cb_2018_us_state_20m.shp"))

us_states<- us_states %>% filter(!(STUSPS%in%c("DC","HI","AK","PR")))
# plot(us_states[,"NAME"])
# us_regions<-read_sf(here("stores/shapefiles/cb_2018_us_state_20m/cb_2018_us_state_20m.shp"))
# plot(us_regions[,"NAME"])


# Regions -----------------------------------------------------------------
us_states$state_abb<-us_states$STUSPS
source(here('../../scripts/github_codes/2_analysis/0_prepara_merge/0b_crosswalks_for_regions.R'))
colnames(st_crosswalk)[colnames(st_crosswalk)=="state"]<-"state_abb"
us_states<-left_join(us_states,st_crosswalk)
rm(st_crosswalk)


regions_borders<-us_states  %>% 
  group_by(region) %>%
  summarize() %>% ungroup()


# Groups ------------------------------------------------------------------
dta_min<- dta %>% 
                dplyr::select(cbsatitle,region,main_city_lon,main_city_lat,cbsas_afam_coef,cbsas_hispanic_coef) %>% 
                pivot_longer(cols = c(cbsas_afam_coef,cbsas_hispanic_coef),names_to="Race",values_to="RR") %>% 
                mutate(Race=str_remove(Race,"cbsas_"),
                       Race=str_remove(Race,"_coef"))
dta_min_lci<- dta %>% 
                dplyr::select(cbsatitle,cbsas_afam_lci,cbsas_hispanic_lci) %>%
                pivot_longer(cols = c(cbsas_afam_lci,cbsas_hispanic_lci),names_to="Race",values_to="lci") %>% 
                mutate(Race=str_remove(Race,"cbsas_"),
                       Race=str_remove(Race,"_lci"))
dta_min_uci<- dta %>% 
                dplyr::select(cbsatitle,cbsas_afam_uci,cbsas_hispanic_uci) %>%
                pivot_longer(cols = c(cbsas_afam_uci,cbsas_hispanic_uci),names_to="Race",values_to="uci") %>% 
                mutate(Race=str_remove(Race,"cbsas_"),
                       Race=str_remove(Race,"_uci"))

dta_min<- dta_min %>% 
                left_join(.,dta_min_lci) %>% 
                left_join(.,dta_min_uci) %>% 
            mutate(Race=factor(Race,levels=c("afam","hispanic"),labels=c("African American","Hispanic/LatinX")))
rm(dta_min_lci,dta_min_uci)


dta_min<- dta_min %>% mutate(significant=ifelse(RR<0 & uci<=0,"sig",
                                               ifelse(RR>0 & lci>0,"sig","not_sig")))


dta_min<- dta_min %>% mutate(significant=factor(significant,levels=c("sig","not_sig"),labels=c("Significant","Not Significant")))


quantile(dta_min$RR,seq(0,1,.05))

dta_min<- dta_min %>% mutate(RR=RR*100)
dta_min<- dta_min %>% mutate(tam=cut(RR,c(-32,-29,-20,-15,-10,0,15))) %>% 
                              st_as_sf(., coords = c("main_city_lon", "main_city_lat"),crs = st_crs(4326)) %>% 
                              st_transform(.,crs=5070)
table(dta_min$tam)

dta_min<- dta_min %>% mutate(tam=factor(tam,levels=c("(-32,-29]","(-29,-20]","(-20,-15]","(-15,-10]","(-10,0]","(0,15]"),
                                            labels=c("(-32%,-29%]","(-29%,-20%]","(-20%,-15%]","(-15%,-10%]","(-10%,0%]","> 0%")))

table(dta_min$tam,useNA = "always")
table(dta_min$signifiant,useNA = "always")
# Layers ------------------------------------------------------------------


# ggplot() +
#   geom_sf(data=us_states,aes(geometry=geometry),color="gray48",fill="white", size = 0.2) +
#   geom_sf(data=regions_borders,aes(geometry=geometry), fill=NA, color="black",size = .8) +
#   geom_sf(data=dta_min         ,aes(geometry=geometry,col=factor(region),size=factor(tam)),alpha=.8) +
#   scale_color_viridis(name="Region",discrete=TRUE) +
#   scale_size_manual("Response Rates \n  (Relative to White Identities)",values=c(20,15,12,9,5,3,1)) +
#   guides(color = guide_legend(override.aes = list(size = 10),order = 1),
#          size  = guide_legend(order = 2)) +
#   facet_grid(rows= vars(Race), switch="y")+
#   theme(plot.margin=grid::unit(c(0,0,0,0), "cm"),
#         legend.position="right",
#         #legend.position = c(1, 0.3),
#         legend.key = element_rect(fill = "white"),
#         strip.text = element_text(face="bold", size=14),
#         strip.background = element_rect(fill="white", colour="black",size=2),
#         text = element_text(size=14),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border = element_blank(),
#         panel.background = element_blank(),
#         axis.title.x=element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(),
#         axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank())
# ggsave(filename="views/map_discrimination.png", height = 11, width = 13.1)


# require(scales)
# viridis_pal()(4)
# show_col(viridis_pal()(4))
# viridis_pal(option = "turbo")(4)
# show_col(viridis_pal(option = "turbo")(4))

reg<- reg %>% select(region,region_response_afam_fct,region_response_hispanic_fct)
reg<- reg %>% pivot_longer(!region,names_to="Race")
reg<- reg %>% mutate(Race=str_remove(Race,"_fct"),
                     Race=str_remove(Race,"region_response_")) %>% 
              mutate(Race=factor(Race,levels=c("afam","hispanic"),labels=c("African American","Hispanic/LatinX")))



regions_borders2<-regions_borders %>% left_join(reg) 
regions_borders2<-regions_borders2%>% rename(RR_region=value)
regions_borders2<-st_transform(regions_borders2,st_crs(dta_min))






require("ggrepel")
ggplot() +
  geom_sf(data=us_states,aes(geometry=geometry),color="gray48",fill="white", size = 0.2) +
  geom_sf(data=regions_borders,aes(geometry=geometry), fill=NA, color="black",size = .6) +
  geom_sf(data=dta_min         ,aes(geometry=geometry,fill=factor(region),size=factor(tam),alpha=factor(significant),color=factor(significant),shape = factor(significant))) +
  #scale_fill_viridis(name="Region",option="turbo",discrete=TRUE) +
  scale_fill_manual(name="Region",values=c("#30123BFF", "#1AE4B6FF", "#FABA39FF","#FDE725FF")) +
  scale_color_manual(name="",values=c("black","white")) +
  scale_size_manual("Response Rates \n  (Relative to White Identities)",values=c(20,15,12,9,5,3,0.5)) +
  scale_alpha_discrete("Statistically",range=c(1, 0.4)) +
  scale_shape_manual("Statistically",values=c(21,23)) +
  guides(fill = guide_legend(override.aes = list(size = 10,shape=21),order = 1),
         size  = guide_legend(override.aes = list(shape=21),order = 2),
         alpha = guide_legend(override.aes = list(size = 10), order = 3),
         color = "none",
         shape=  guide_legend(override.aes = list(size = 10), order = 3)) +
  geom_label_repel(data=regions_borders2         ,aes(geometry=geometry,label = RR_region),
                   box.padding   = 0.35,
                   point.padding = 0.5,
                   size          = 3,
                   segment.color = 'grey50',
                   stat = "sf_coordinates",
                   nudge_x=-3,
                   nudge_y=1,
                   min.segment.length = .25) +
  facet_grid(rows= vars(Race), switch="y")+
  theme(plot.margin=grid::unit(c(0,0,0,0), "cm"),
        legend.position="right",
        #legend.position = c(1, 0.3),
        legend.key = element_rect(fill = "white"),
        strip.text = element_text(face="bold", size=14),
        strip.background = element_rect(fill="white", colour="black",size=1),
        text = element_text(size=14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
ggsave(filename="views/map_discrimination_signif.png", height = 11, width = 13.1)




# By Cols --------------------------------------------------------------------
# ggplot() +
#   geom_sf(data=us_states,aes(geometry=geometry),color="gray48",fill="white", size = 0.2) +
#   geom_sf(data=regions_borders,aes(geometry=geometry), fill=NA, color="black",size = .6) +
#   geom_sf(data=dta_min         ,aes(geometry=geometry,fill=factor(region),size=factor(tam),alpha=factor(significant),color=factor(significant),shape = factor(significant))) +
#   #scale_fill_viridis(name="Region",option="turbo",discrete=TRUE) +
#   scale_fill_manual(name="Region",values=c("#30123BFF", "#1AE4B6FF", "#FABA39FF","#FDE725FF")) +
#   scale_color_manual(name="",values=c("black","white")) +
#   scale_size_manual("Response Rates \n  (Relative to White Identities)",values=c(20,15,12,9,5,3,0.5)) +
#   scale_alpha_discrete("Statistically",range=c(1, 0.4)) +
#   scale_shape_manual("Statistically",values=c(21,23)) +
#   guides(fill = guide_legend(override.aes = list(size = 10,shape=21),order = 1),
#          size  = guide_legend(override.aes = list(shape=21),order = 2),
#          alpha = guide_legend(override.aes = list(size = 10), order = 3),
#          color = "none",
#          shape=  guide_legend(override.aes = list(size = 10), order = 3)) +
#   geom_label_repel(data=regions_borders2         ,aes(geometry=geometry,label = RR_region),
#                    box.padding   = 0.35,
#                    point.padding = 0.5,
#                    size          = 3,
#                    segment.color = 'grey50',
#                    stat = "sf_coordinates",
#                    nudge_x=-3,
#                    nudge_y=1,
#                    min.segment.length = .25) +
#   facet_grid(cols= vars(Race), switch="y")+
#   theme(plot.margin=grid::unit(c(0,0,0,0), "cm"),
#         legend.position="bottom",
#         #legend.position = c(1, 0.3),
#         legend.key = element_rect(fill = "white"),
#         strip.text = element_text(face="bold", size=14),
#         strip.background = element_rect(fill="white", colour="black",size=1),
#         text = element_text(size=14),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border = element_blank(),
#         panel.background = element_blank(),
#         axis.title.x=element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(),
#         axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank())
# ggsave(filename="views/map_discrimination_signif_cols.png", height = 7, width = 20)
# 
# 
# 
