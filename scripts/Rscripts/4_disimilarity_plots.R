##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("sf","ggplot2","maps","ggthemes","stringr","haven","dplyr","forcats")
lapply(pkg, require, character.only=T)
rm(pkg)


#WD
setwd("~/Dropbox/Research/cbsa_discrimination/github/Racial_Discrimination_US_Rental_Market/")

#load daa
dta<-readRDS("stores/temp/data_maps_RR.RDS")
blacks<-dta %>% dplyr::select(cbsatitle, diss_b_w,cbsas_afam_coef)
colnames(blacks)[2]<-"diss"
colnames(blacks)[3]<-"RR"
blacks <- blacks %>% mutate(race="afam")



hispanics<-dta %>% dplyr::select(cbsatitle, diss_h_w,cbsas_hispanic_coef)
colnames(hispanics)[2]<-"diss"
colnames(hispanics)[3]<-"RR"
hispanics <- hispanics %>% mutate(race="hispanic")


db<-rbind(blacks,hispanics)
# # -----------------------------------------------------------------------
# Isolation and Dissimilarity Plots ---------------------------------------
# # -----------------------------------------------------------------------
require("ggpubr")
require("ggrepel")
require("texreg")



db<-db %>% mutate(race=factor(race,levels=c("afam","hispanic"),labels=c("African American","Hispanic/LatinX")))

reg3<-lm(RR~diss+factor(race),db)

# ggplot(db,aes(x=diss,y=RR,group=race,shape=race,col=race)) +
#   geom_point(size=4) +
#   geom_smooth(aes(col=race,lty=race), method = "lm",size=.5,se = TRUE,level=.90) +
#   xlab("Disimilarity Index") + ylab('Relative Responses') +
#   theme_bw() +
#   theme(legend.title= element_blank() ,
#         #legend.position="none",
#         legend.justification=c(1,1),
#         legend.direction="vertical",
#         legend.box="horizontal",
#         legend.box.just = c("top"),
#         panel.grid.major.x = element_blank(),
#         legend.background = element_rect(fill='transparent'),
#         axis.text.x =element_text( angle=45),
#         rect = element_rect(colour = "transparent", fill = "white"),
#         plot.margin = unit(c(2,3.5,1,1), "lines")) 
# ggsave("views/diss_b_h_w_join.pdf",height = 8,width = 16)
require("ggrepel")
db<- db %>% mutate(labels=ifelse(cbsatitle%in%c("Louisville-Jefferson County, KY-IN" , "Minneapolis-St. Paul-Bloomington, MN-WI","New York-Newark-Jersey City, NY-NJ-PA","Boston-Cambridge-Newton, MA-NH","Chicago-Naperville-Elgin, IL-IN-WI","Portland-Vancouver-Hillsboro, OR-WA","Detroit-Warren-Dearborn, MI","Jacksonville, FL","Providence-Warwick, RI-MA","Las Vegas-Henderson-Paradise, NV","Raleigh, NC","Richmond, VA"),cbsatitle,NA))

ggplot(db,aes(x=diss,y=RR)) +
  geom_point(aes(group=race,shape=race),size=4) +
  geom_smooth(method = "lm",col="black",size=.5,lty="longdash",se = TRUE,level=.90) +
  scale_shape_manual(values=c(16,17)) +
  xlab("Disimilarity Index") + ylab('Relative Responses') +
  geom_label_repel(aes(label = labels),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   size          = 2,
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
        axis.text.x =element_text( angle=45),
        rect = element_rect(colour = "transparent", fill = "white"),
        plot.margin = unit(c(2,3.5,1,1), "lines")) 
ggsave("views/diss_b_h_w.pdf",height = 8,width = 16)



# Regressions --------------------------------------------------------------------



blacks<-dta %>% dplyr::select(cbsatitle, diss_m_w,cbsas_afam_coef)
colnames(blacks)[2]<-"diss_min"
colnames(blacks)[3]<-"RR"
blacks <- blacks %>% mutate(race="afam")
hispanics<-dta %>% dplyr::select(cbsatitle, diss_m_w,cbsas_hispanic_coef)
colnames(hispanics)[2]<-"diss_min"
colnames(hispanics)[3]<-"RR"
hispanics <- hispanics %>% mutate(race="hispanic")

reg1_min<-lm(RR~diss_min,blacks)
reg2_min<-lm(RR~diss_min,hispanics)

stargazer::stargazer(reg1_min,reg2_min,type="text")

db<-rbind(blacks,hispanics)
db<-db %>% mutate(race=factor(race,levels=c("afam","hispanic"),labels=c("African American","Hispanic/LatinX")))

reg1<-lm(cbsas_afam_coef~diss_b_w,dta)
reg2<-lm(cbsas_hispanic_coef~diss_h_w,dta)

reg4<-lm(RR~diss_min+factor(race),db)

regs<-list(reg1,reg2,reg3,reg4,reg1_min,reg2_min)

screenreg(regs,caption="Relative Response Rates, Dissimilarity Indexes",digits=4)

texreg(regs,file="views/dissimilarity_50.tex",caption="Relative Response Rates, Dissimilarity Indexes",digits=4,custom.model.names = c("AA-W","H-W","AA-H-W","M-W","M-W","M-W"),custom.coef.names = c("Constant","Dissimilarity Index: African American-Whites","Dissimilarity Index:  Hispanic/LatinX-Whites","Dissimilarity Index", "Hispanic/LatinX=1","Dissimilarity Index:  Minority-Whites"),stars = c(0.01, 0.05, 0.1))

