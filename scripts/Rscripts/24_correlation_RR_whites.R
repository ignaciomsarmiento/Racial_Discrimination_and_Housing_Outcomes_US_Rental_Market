##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("tidyverse","ggplot2","ggpubr")
lapply(pkg, require, character.only=T)
rm(pkg)

#install.packages("ggpubr")
#WD
setwd("~/Dropbox/Research/cbsa_discrimination/github/Racial_Discrimination_US_Rental_Market/")

#load daa
dta<-readRDS("stores/temp/data_maps_RR.RDS")



# Odds Ratios -------------------------------------------------------------
dta_rr<-data.frame(dta) %>% dplyr::select(cbsatitle,
                                          cbsas_afam_lci,
                                          cbsas_afam_coef,
                                          cbsas_afam_uci,
                                          cbsas_hispanic_lci,
                                          cbsas_hispanic_coef,
                                          cbsas_hispanic_uci
                                          )  




dta_rr %>%   summarise(across(c(cbsas_afam_coef,cbsas_hispanic_coef), min))
dta_rr %>%   summarise(across(c(cbsas_afam_coef,cbsas_hispanic_coef), max))

summary(lm(cbsas_afam_coef~cbsas_resp_white,dta))
cor(dta$cbsas_afam_coef,dta$cbsas_resp_white)

 
dta1<- dta %>% select(cbsatitle,cbsas_afam_coef,cbsas_resp_white)
dta1<- dta1 %>% rename(RR=cbsas_afam_coef) %>% mutate(race="African American")
dta2<- dta %>% select(cbsatitle,cbsas_hispanic_coef,cbsas_resp_white)
dta2<- dta2 %>% rename(RR=cbsas_hispanic_coef) %>% mutate(race="Hispanic/LatinX")

dta_rr<-rbind(dta1,dta2) %>% mutate(across(where(is.numeric),~.x*100))

ggplot(dta_rr, aes(y =RR , x = cbsas_resp_white )) +
    geom_point(size=4) +
    xlab("White Response Rate") +
    ylab("Relative Responses") +
    theme_bw() +
  geom_smooth(method = "lm",size=1,lty="longdash",se = FALSE,level=.90,color="#3B4992FF") +
  expand_limits(y = c(-33,15),x = c(35,75)) +
  theme_bw() +
  facet_wrap(~ race, ncol=2) +
  stat_cor(label.x = 36,label.y=-30, size = 6 ,label.sep = "\n") +
  theme(legend.title= element_blank() ,
        legend.position="bottom",
        #legend.justification=c(1,1),
        legend.direction="horizontal",
        legend.box="horizontal",
        legend.box.just = c("top"),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill='transparent'),
        strip.background = element_rect(fill="white", colour="black",size=1),
        axis.text.x =element_text(size=20, angle=0,hjust=1),
        axis.text.y =element_text(size=20),
        text = element_text(size=20),
        rect = element_rect(colour = "transparent", fill = "white"),
        plot.margin = unit(c(2,3.5,1,1), "lines"))
ggsave(paste0("views/","correlationRR_white_black_hispanic",".pdf"),height = 8,width = 20)

#have to edit this functions to get rho and p-value
# trace(ggpubr:::get_p_label, edit=TRUE)
# trace(ggpubr:::get_corcoef_label, edit=TRUE) #rho
