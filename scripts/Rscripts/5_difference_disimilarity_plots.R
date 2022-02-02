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

require("ggpubr")
require("ggrepel")
require("texreg")

#WD
setwd("~/Dropbox/Research/cbsa_discrimination/github/Racial_Discrimination_US_Rental_Market/")

#load daa
dta<-readRDS("stores/temp/data_maps_RR.RDS")

diff_reg<-lm(cbsas_afam_coef~cbsas_hispanic_coef:cbsatitle-1,dta)
summary(diff_reg)

dta<- dta %>% mutate(diff_afam_hispanic=cbsas_afam_coef-cbsas_hispanic_coef,
                     diss_diff=diss_b_w-diss_h_w)






require("ggrepel")
dta<- dta %>% mutate(labels=ifelse(cbsatitle%in%c("Louisville-Jefferson County, KY-IN" , "Minneapolis-St. Paul-Bloomington, MN-WI","New York-Newark-Jersey City, NY-NJ-PA","Boston-Cambridge-Newton, MA-NH","Chicago-Naperville-Elgin, IL-IN-WI","Portland-Vancouver-Hillsboro, OR-WA","Detroit-Warren-Dearborn, MI","Jacksonville, FL","Providence-Warwick, RI-MA","Las Vegas-Henderson-Paradise, NV","Raleigh, NC","Richmond, VA"),cbsatitle,NA))

ggplot(dta,aes(x=diss_diff,y=diff_afam_hispanic)) +
  geom_point(size=4) +
  geom_smooth(method = "lm",col="black",size=.5,lty="longdash",se = TRUE,level=.90) +
  scale_shape_manual(values=c(16,17)) +
  xlab("Difference Disimilarity Index \n (Af. Am. - Hispanic/LatinX)") + 
  ylab('Difference Relative Responses \n (Af. Am. - Hispanic/LatinX)') +
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
ggsave("views/diff_diss_b_h_w.pdf",height = 8,width = 16)

