library(sf)
library("lwgeom")
plot_maps<-function(data_cbsa=data.frame(dta_min),fill_map,cbsa_size,map=us_states,geography="region", name_scale_map="Response Rates Relative to Whites Regions", name_scale_cbsa="Response Rates Relative to Whites CBSAs",save=FALSE,path,name_output,ht=5.28,wd=13.1){
  
  
  breaks_cbsa <- round(as.numeric(quantile(data_cbsa[,cbsa_size],na.rm=TRUE)),2)
  
  #Colors 
  if(geography=="region"){
    blacks<- data_cbsa %>% dplyr::select(region,region_afam_coef) %>% distinct(.keep_all = TRUE)
    hispanics<- data_cbsa %>% dplyr::select(region,region_hispanic_coef) %>% distinct(.keep_all = TRUE)
    border<-map  %>% st_make_valid() %>% 
        dplyr::group_by(region) %>%
        dplyr::summarize(geom = st_union(geom),.groups="drop")
    
        
    
      
        
    
  }else if(geography=="division"){
    blacks<- data_cbsa %>% dplyr::select(division,div_afam_coef) %>% distinct(.keep_all = TRUE)
    hispanics<- data_cbsa %>% dplyr::select(division,div_hispanic_coef) %>% distinct(.keep_all = TRUE)
    border<-map  %>% st_make_valid() %>% 
      group_by(division) %>%
      dplyr::summarize(geom = st_union(geom),.groups="drop")
  }
  
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
  if(geography=="region") b_h<-left_join(b_h,or_reds2) %>% dplyr::select(region,race,colors)
  if(geography=="division") b_h<-left_join(b_h,or_reds2) %>% dplyr::select(division,race,colors)
  
  if(grepl("afam",fill_map)) b_h<- b_h %>% filter(race=="African American")
  if(grepl("hispanic",fill_map)) b_h<- b_h %>% filter(race=="Hispanic/LatinX")
  map<-left_join(map,b_h)
  
  data_cbsa<-st_as_sf(data_cbsa,coords=c("main_city_lon","main_city_lat"))
  st_crs(data_cbsa)<-st_crs(map)
  data_cbsa<-st_transform(data_cbsa,crs=st_crs(map))
  
  #put in albers projection
  map<-st_transform(map,crs="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
  border<-st_transform(border,crs="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
  data_cbsa<-st_transform(data_cbsa,crs="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
  
  p<-ggplot() +
    geom_sf(data=map,aes(geometry=geom,fill=colors),color="gray48", size = 0.2) +
    geom_sf(data=border,aes(geometry=geom), fill=NA, color="black",size = .8) +
    scale_fill_identity(name=name_scale_map,labels =names(table(st_drop_geometry(map[,fill_map]))) ,guide = "legend",na.translate = F) +
    geom_sf(data=data_cbsa,aes_string(geometry="geometry",size = cbsa_size),color="gray48",alpha=0.9) +
    scale_size_continuous(name=name_scale_cbsa, range=c(1,8), breaks=breaks_cbsa) +
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
  if(save==TRUE) ggsave(filename=paste0(path,name_output,".png"), height = ht, width = wd)
  p
  
}
