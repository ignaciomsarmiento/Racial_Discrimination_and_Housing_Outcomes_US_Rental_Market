##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("sf","ggplot2","ggthemes","stringr","haven","dplyr","forcats")
lapply(pkg, require, character.only=T)
rm(pkg)


#WD
setwd("~/Dropbox/Research/cbsa_discrimination/github/Racial_Discrimination_US_Rental_Market/")

#load daa
dta<-read_dta("stores/matrices/event_floyd.dta")
dta<-zap_formats(dta)

dta<- dta %>% mutate(n=n-21)
dta<- dta %>% filter(n!=0)
blacks<-dta[,c(1:3,7)]
colnames(blacks)<-c("lci","coef","uci","week")
blacks <- blacks %>% mutate(group="African American")
hispanics<-dta[,c(4:7)]
colnames(hispanics)<-c("lci","coef","uci","week")
hispanics <- hispanics %>% mutate(group="Hispanic/LatinX")



ggplot(blacks,aes(x=week,y=coef,group=group)) +
  geom_line( size=.5)+
  geom_ribbon(aes(ymin=lci, ymax=uci),alpha=0.2)+
  geom_line(data=blacks,aes(x=week, y=lci),alpha=0.5, linetype="dashed")+
  geom_line(data=blacks,aes(x=week, y=uci),alpha=0.5, linetype="dashed")+
  geom_hline(aes(yintercept=0), colour="black", linetype="twodash") +
  geom_vline(aes(xintercept=0), colour="black", linetype="twodash") +
  scale_x_continuous("Round Relative to G. Floyd Homicide",breaks=blacks$week) + 
  ylab('Relative Responses') +
  theme_bw() +
  theme(legend.title= element_blank() ,
        legend.position="bottom",
        #legend.justification=c(1,1),
        legend.direction="horizontal",
        legend.box="horizontal",
        legend.box.just = c("top"),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill='transparent'),
        axis.text.x =element_text( angle=45),
        rect = element_rect(colour = "transparent", fill = "white"),
        plot.margin = unit(c(2,3.5,1,1), "lines")) 
ggsave("views/event_afam_floyd_prev_week.pdf",height = 8,width = 16)




ggplot(hispanics,aes(x=week,y=coef,group=group)) +
  geom_line( size=.5)+
  geom_ribbon(aes(ymin=lci, ymax=uci),alpha=0.2)+
  geom_line(data=hispanics,aes(x=week, y=lci),alpha=0.5, linetype="dashed")+
  geom_line(data=hispanics,aes(x=week, y=uci),alpha=0.5, linetype="dashed")+
  geom_hline(aes(yintercept=0), colour="black", linetype="twodash") +
  geom_vline(aes(xintercept=0), colour="black", linetype="twodash") +
  scale_x_continuous("Round Relative to G. Floyd Homicide",breaks=hispanics$week) + 
  ylab('Relative Responses') +
  theme_bw() +
  theme(legend.title= element_blank() ,
        legend.position="bottom",
        #legend.justification=c(1,1),
        legend.direction="horizontal",
        legend.box="horizontal",
        legend.box.just = c("top"),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill='transparent'),
        axis.text.x =element_text( angle=45),
        rect = element_rect(colour = "transparent", fill = "white"),
        plot.margin = unit(c(2,3.5,1,1), "lines")) 
ggsave("views/event_hispanic_floyd_prev_week.pdf",height = 8,width = 16)


