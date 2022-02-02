##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("sf","ggplot2","maps","ggthemes","stringr","haven","tidyverse","forcats")
lapply(pkg, require, character.only=T)
rm(pkg)


#WD
setwd("~/Dropbox/Research/cbsa_discrimination/github/Racial_Discrimination_US_Rental_Market/")

#load daa
dta<-readRDS("stores/temp/data_maps_RR.RDS")
dta<- dta %>% rename(short_cbsa=cbsa1,label_cbsa=cbsa_st)
blacks<-dta %>% dplyr::select(cbsatitle, diss_b_w,cbsas_afam_coef,label_cbsa)
colnames(blacks)[2]<-"diss"
colnames(blacks)[3]<-"RR"
blacks <- blacks %>% mutate(race="afam")



hispanics<-dta %>% dplyr::select(cbsatitle, diss_h_w,cbsas_hispanic_coef,label_cbsa)
colnames(hispanics)[2]<-"diss"
colnames(hispanics)[3]<-"RR"
hispanics <- hispanics %>% mutate(race="hispanic")




oppo_data<-read_dta("stores/matchedinquiries_opportunities_cz.dta")

sum_opp0<- oppo_data %>% dplyr::select(starts_with(c("CBSA","kfr","kir","jail")),
                                       contains(c("white","black","hispanic")),
                                       ends_with(c("p25","mean","p75"))
                                       )



sum_opp0<- sum_opp0 %>%
            group_by(CBSA) %>% 
              summarize_all(median, na.rm = TRUE)
                        

# diff_maker<-function(db,var,gender,percentile){
#   var<-enquo(var)
#   gender<-enquo(gender)
#   percentile<-enquo(percentile)
#   #paste("diff",!!var,"white","hispanic",!!gender,!!percentile,sep="_")
#   db <- db %>% mutate(x=paste(!!var,"white",!!gender,!!percentile,sep="_")-paste(!!var,"hispanic",!!gender,!!percentile,sep="_")
#                       #paste("diff",!!var,"white","black",!!gender,!!percentile,sep="_")=paste(!!var,"white",!!gender,!!percentile,sep="_")-paste(!!var,"black",!!gender,!!percentile,sep="_")
#   )
#   
#    
#   return(db)
#   
# }


# sum_opp0<-diff_maker(sum_opp0,"kfr","pooled","mean")

diff <- sum_opp0 %>% 
  mutate(diff_kfr_white_black_pooled_mean=kfr_white_pooled_mean-kfr_black_pooled_mean,
         diff_kfr_white_hisp_pooled_mean=kfr_white_pooled_mean-kfr_hisp_pooled_mean,
  ) %>% 
  select(CBSA,starts_with("diff"))


diff <- diff %>% 
          pivot_longer(!CBSA,names_to= "outcomes",values_to="difference_perc_white")

diff <- diff %>% 
        separate(col=outcomes,into=c("diff","outcome","white","race","gender","percentile")) %>% 
        dplyr::select( -white,-diff) %>% 
        mutate(race=case_when(race == "black" ~ "afam",
                         race == "hisp" ~ "hispanic"),
          race=factor(race,levels=c("afam","hispanic"),labels=c("African American","Hispanic/LatinX"))) %>%
        rename(cbsatitle=CBSA)


diff_w<- diff %>% pivot_wider(names_from="outcome",values_from="difference_perc_white") 


db<-rbind(blacks,hispanics)
db<-db %>% mutate(race=factor(race,levels=c("afam","hispanic"),labels=c("African American","Hispanic/LatinX")))

db<-left_join(db,diff_w)

require("ggpubr")
require("ggsci")

db<-db %>% filter(gender=="pooled",percentile=="mean")
require("ggrepel")
# db<- db %>% mutate(labels=ifelse(cbsatitle%in%c("Louisville-Jefferson County, KY-IN" , "Minneapolis-St. Paul-Bloomington, MN-WI","New York-Newark-Jersey City, NY-NJ-PA","Boston-Cambridge-Newton, MA-NH","Chicago-Naperville-Elgin, IL-IN-WI","Portland-Vancouver-Hillsboro, OR-WA","Detroit-Warren-Dearborn, MI","Jacksonville, FL","Providence-Warwick, RI-MA","Las Vegas-Henderson-Paradise, NV","Raleigh, NC","Richmond, VA"),cbsatitle,NA))
db<- db %>% mutate(labels=ifelse(label_cbsa%in%c("Louisville, KY" , "Minneapolis, MN","New York,NY","Boston, MA","Chicago, IL","Portland, OR","Detroit, MI","Jacksonville, FL","Providence, RI","Columbus, OH","Milwaukee, WI"),label_cbsa,NA))

db<- db %>% mutate(RR=RR*100)
  
ggplot(db ,aes(x=kfr,y=RR,group=race,col=race,shape=race)) +
    geom_point(size=4) +
    geom_smooth(method = "lm",size=1,lty="longdash",se = FALSE,level=.90) +
    scale_shape_manual(values=c(16,17)) +
    scale_color_manual(values=c("#3B4992FF", "#EE0000FF")) +
    scale_x_continuous(expression(paste(Delta,"Mean Percentile Rank (White-Minority)")),breaks=scales::breaks_pretty()) + 
    ylab('Relative Responses') +
    geom_label_repel(aes(label = labels),
                     box.padding   = 0.35, 
                     point.padding = 0.5,
                     size          = 3,
                     segment.color = 'grey50',
                     max.overlaps = 20) +
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
ggsave(paste0("views/","kfr_pooled_mean_color",".pdf"),height = 8,width = 16)

