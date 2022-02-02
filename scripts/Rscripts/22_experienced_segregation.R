##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("tidyverse","here","fuzzyjoin")
lapply(pkg, require, character.only=T)
rm(pkg)


#load data
dta<-readRDS(here("stores/temp/data_maps_RR.RDS"))

blacks<-dta %>% dplyr::select(cbsatitle,cbsas_afam_coef)
colnames(blacks)[2]<-"RR"
blacks <- blacks %>% mutate(race="afam")
hispanics<-dta %>% dplyr::select(cbsatitle,cbsas_hispanic_coef)
colnames(hispanics)[2]<-"RR"
hispanics <- hispanics %>% mutate(race="hispanic")

#db<-rbind(blacks,hispanics)
#db<-db %>% mutate(race=factor(race,levels=c("afam","hispanic"),labels=c("African American","Hispanic/LatinX")))
db<-blacks %>% mutate(race=factor(race,levels=c("afam"),labels=c("African American")))


#Experienced Segregation
fls<-as.list(list.files(here("stores/Experienced_Discrimination/")))
fls<-lapply(fls,function(x){read_csv(here("stores/Experienced_Discrimination/",x))})
fls<-do.call(rbind,fls)
fls1<-fls[,1:3]
fls2<-fls[,4:6]
colnames(fls2)<-colnames(fls1)
fls<-rbind(fls1,fls2)
rm(fls1,fls2)

#Fix CBSAs names for matching
cbsas<-dta %>% dplyr::select(cbsatitle,cbsa1,cbsa_st) %>% rename(short_cbsa=cbsa1,label_cbsa=cbsa_st)
cbsas<- cbsas %>% separate(cbsatitle,into=c("cbsa_full","state"),sep=",",remove=FALSE)
cbsas$cbsa_full[cbsas$cbsa_full=="Sacramento--Roseville--Arden-Arcade"]<-"Sacramento-Roseville-Arden-Arcade"
cbsas$cbsa_full[cbsas$cbsa_full=="Nashville-Davidson--Murfreesboro--Franklin"]<-"Nashville-Davidson-Murfreesboro-Franklin"
cbsas<- cbsas %>% separate(cbsa_full,into=c("cbsa1","cbsa2","cbsa3","cbsa4"),sep="-",remove=FALSE)

cbsas<- cbsas %>% pivot_longer(!c(cbsatitle,cbsa_full,state,short_cbsa,label_cbsa), names_to = "var", values_to = "cbsa")
cbsas<- cbsas %>% select(cbsatitle,cbsa_full,state,cbsa,short_cbsa,label_cbsa)
cbsas<- cbsas %>% mutate(cbsa_st=paste0(cbsa,",",state))
cbsas<- cbsas %>% filter(!is.na(cbsa))  

cbsas$cbsa_st[cbsas$cbsa_st=="Boston, MA-NH"]<-"Boston, MA"
cbsas$cbsa_st[cbsas$cbsa_st=="Cambridge, MA-NH"]<-"Cambridge, MA"
cbsas$cbsa_st[cbsas$cbsa_st=="Newton, MA-NH"]<-"Newton, NH"
cbsas$cbsa_st[cbsas$cbsa_st=="Charlotte, NC-SC"]<-"Charlotte, NC"
cbsas$cbsa_st[cbsas$cbsa_st=="Concord,  NC-SC"]<-"Concord, NC"
cbsas$cbsa_st[cbsas$cbsa_st=="Gastonia,  NC-SC"]<-"Gastonia, NC"
cbsas$cbsa_st[cbsas$cbsa_st=="Chicago, IL-IN-WI"]<-"Chicago, IL" 
cbsas$cbsa_st[cbsas$cbsa_st=="Naperville, IL-IN-WI"]<-"Naperville, IL"
cbsas$cbsa_st[cbsas$cbsa_st=="Elgin, IL-IN-WI"]<-"Elgin, IL"
cbsas$cbsa_st[cbsas$cbsa_st=="Cincinnati, OH-KY-IN"]<-"Cincinnati, OH"
cbsas$cbsa_st[cbsas$cbsa_st=="Kansas City, MO-KS"]<-"Kansas City, MO"
cbsas$cbsa_st[cbsas$cbsa_st=="Louisville, KY-IN"]<-"Louisville, KY"
cbsas$cbsa_st[cbsas$cbsa_st=="Jefferson County, KY-IN"]<-"Jefferson County, KY"
cbsas$cbsa_st[cbsas$cbsa_st=="Memphis, TN-MS-AR"]<-"Memphis, TN"
cbsas$cbsa_st[cbsas$cbsa_st=="Minneapolis, MN-WI"]<-"Minneapolis, MN"
cbsas$cbsa_st[cbsas$cbsa_st=="Bloomington, MN-WI"]<-"Bloomington, MN"
cbsas$cbsa_st[cbsas$cbsa_st=="New York, NY-NJ-PA"]<-"New York, NY"
cbsas$cbsa_st[cbsas$cbsa_st=="Newark, NY-NJ-PA"]<-"Newark, NJ"
cbsas$cbsa_st[cbsas$cbsa_st=="Jersey City, NY-NJ-PA"]<-"Jersey City, NJ"
cbsas$cbsa_st[cbsas$cbsa_st=="Philadelphia, PA-NJ-DE-MD"]<-"Philadelphia, PA"
cbsas$cbsa_st[cbsas$cbsa_st=="Camden, PA-NJ-DE-MD"]<-"Camden, NJ"
cbsas$cbsa_st[cbsas$cbsa_st=="Wilmington, PA-NJ-DE-MD"]<-"Wilmington, DE"
cbsas$cbsa_st[cbsas$cbsa_st=="Portland, OR-WA"]<-"Portland, OR"
cbsas$cbsa_st[cbsas$cbsa_st=="Vancouver, OR-WA"]<-"Vancouver, WA"
cbsas$cbsa_st[cbsas$cbsa_st=="Hillsboro, OR-WA"]<-"Hillsboro, OR"
cbsas$cbsa_st[cbsas$cbsa_st=="Providence, RI-MA"]<-"Providence, RI"
cbsas$cbsa_st[cbsas$cbsa_st=="Warwick, RI-MA"]<-"Warwick, RI"
cbsas$cbsa_st[cbsas$cbsa_st=="St. Louis, MO-IL"]<-"St. Louis, MO"
cbsas$cbsa_st[cbsas$cbsa_st=="Virginia Beach, VA-NC"]<-"Virginia Beach, VA"
cbsas$cbsa_st[cbsas$cbsa_st=="Norfolk, VA-NC"]<-"Norfolk, VA"
cbsas$cbsa_st[cbsas$cbsa_st=="Newport News, VA-NC"]<-"Newport News, VA"
cbsas$cbsa_st[cbsas$cbsa_st=="Washington, DC-VA-MD-WV"]<-"Washington, DC"
cbsas$cbsa_st[cbsas$cbsa_st=="Arlington, DC-VA-MD-WV"]<-"Arlington, VA"
cbsas$cbsa_st[cbsas$cbsa_st=="Alexandria, DC-VA-MD-WV"]<-"Alexandria, VA"


cbsas<-stringdist_left_join(cbsas,fls,by = c(  "cbsa_st"="MSA"), max_dist = 0, distance_col = "match_score")

cbsas_g<- cbsas %>% group_by(cbsatitle,short_cbsa,label_cbsa) %>% summarize(Exp=median(Exp,na.rm=TRUE),
                                                    Res=median(Res,na.rm=TRUE),.groups = "drop")


db<- left_join(db,cbsas_g,by="cbsatitle")


require("ggrepel")
db<- db %>% mutate(labels=ifelse(label_cbsa%in%c("Louisville, KY" , "Minneapolis, MN","New York,NY","Boston, MA","Chicago, IL","Portland, OR","Detroit, MI","Jacksonville, FL","Providence, RI","Columbus, OH","Milwaukee, WI"),label_cbsa,NA))



ggplot(db ,aes(x=Exp,y=RR,group=race,col=race,shape=race)) +
  geom_point(size=4) +
  geom_smooth(method = "lm",size=1,lty="longdash",se = FALSE,level=.90) +
  scale_shape_manual(values=c(16,17)) +
  scale_color_manual(values=c("#3B4992FF", "#EE0000FF")) +
  scale_x_continuous("Experienced Isolation",breaks=scales::breaks_pretty()) + 
  ylab('Relative Responses') +
  geom_label_repel(aes(label = labels),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   size          = 3,
                   segment.color = 'grey50') +
  theme_bw() +
  theme(legend.title= element_blank() ,
        legend.position="bottom",
        #legend.justification=c(1,1),
        legend.direction="horizontal",
        legend.box="horizontal",
        legend.box.just = c("top"),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill='transparent'),
        axis.text.x =element_text(size=20, angle=0,hjust=1),
        axis.text.y =element_text(size=20),
        text = element_text(size=20),
        rect = element_rect(colour = "transparent", fill = "white"),
        plot.margin = unit(c(2,3.5,1,1), "lines"))   
ggsave(paste0("views/","exp_isolation",".pdf"),height = 8,width = 16)

