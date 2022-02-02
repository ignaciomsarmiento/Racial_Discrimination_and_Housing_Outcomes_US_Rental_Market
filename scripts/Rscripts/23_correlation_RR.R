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
                                          )  %>% mutate(across(where(is.numeric),~.x*100))




dta_rr %>%   summarise(across(c(cbsas_afam_coef,cbsas_hispanic_coef), min))
dta_rr %>%   summarise(across(c(cbsas_afam_coef,cbsas_hispanic_coef), max))

summary(lm(cbsas_hispanic_coef~cbsas_afam_coef,dta_rr))
cor(dta_rr$cbsas_afam_coef,dta_rr$cbsas_hispanic_coef)

ggplot(dta_rr, aes(x = cbsas_afam_coef, y = cbsas_hispanic_coef)) +
    geom_point(size=4) +
    ylab("Relative Responses \n Hispanic/LatinX") +
    xlab("Relative Responses African American") +
    theme_bw() +
  geom_smooth(method = "lm",size=1,lty="longdash",se = FALSE,level=.90,color="#3B4992FF") +
  expand_limits(x = c(-33,15),y = c(-33,15)) +
  geom_abline(slope=1, intercept = 0,lty="longdash", col="gray45") +
  theme_bw() +
  stat_cor(label.y = -30,label.x=3, size = 6 ,label.sep = "\n") +
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
        plot.margin = unit(c(2,3.5,1,1), "lines")) +
        annotate(geom="text",label=expression(45~degree),x=-32,y=-34)
ggsave(paste0("views/","correlationRR",".pdf"),height = 8,width = 16)


dta<- dta %>% mutate(across(where(is.numeric),~.x*100))
ggplot(dta, aes(x = cbsas_afam_coef, y = cbsas_hispanic_coef)) +
  geom_point(aes(fill=region),size=4,shape=21) +
  ylab("Relative Responses \n Hispanic/LatinX") +
  xlab("Relative Responses African American") +
  theme_bw() +
  scale_fill_manual(name="Region",values=c("#30123BFF", "#1AE4B6FF", "#FABA39FF","#FDE725FF")) +
  facet_wrap(~ region, nrow=2) +
  geom_smooth(method = "lm",size=1,lty="longdash",se = FALSE,level=.90,color="#3B4992FF") +
  expand_limits(x = c(-33,15),y = c(-33,15)) +
  geom_abline(slope=1, intercept = 0,lty="longdash", col="gray45") +
  theme_bw() +
  stat_cor(label.y = -30,label.x=3, size = 6 ,label.sep = "\n") +
  theme(legend.title= element_blank() ,
        legend.position="bottom",
        strip.background = element_rect(fill="white", colour="black",size=1),
        legend.direction="horizontal",
        legend.box="horizontal",
        legend.box.just = c("top"),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill='transparent'),
        axis.text.x =element_text(size=20, angle=0,hjust=1),
        axis.text.y =element_text(size=20),
        text = element_text(size=20),
        rect = element_rect(colour = "transparent", fill = "white"),
        plot.margin = unit(c(2,3.5,1,1), "lines")) +
  annotate(geom="text",label=expression(45~degree),x=-32,y=-34)
ggsave(paste0("views/","correlationRR_regions",".pdf"),height = 8,width = 16)
