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


#Disimilartiy Table

diss<-dta %>% dplyr::select(cbsatitle,diss_b_w,diss_h_w)
require("stargazer")
stargazer(data.frame(diss),type="latex",summary=FALSE,rownames = FALSE,out="views/dissimilarity_cities.tex",digits=4)


require("ggrepel")
diss<- diss %>% mutate(labels=ifelse(cbsatitle%in%c("Louisville-Jefferson County, KY-IN" , "Minneapolis-St. Paul-Bloomington, MN-WI","New York-Newark-Jersey City, NY-NJ-PA","Boston-Cambridge-Newton, MA-NH","Chicago-Naperville-Elgin, IL-IN-WI","Portland-Vancouver-Hillsboro, OR-WA","Detroit-Warren-Dearborn, MI","Jacksonville, FL","Providence-Warwick, RI-MA","Las Vegas-Henderson-Paradise, NV","Raleigh, NC","Richmond, VA"),cbsatitle,NA))

min(diss$diss_b_w)
min(diss$diss_h_w)

max(diss$diss_b_w)
max(diss$diss_h_w)

ggplot(diss,aes(y=diss_b_w,x=diss_h_w)) +
  geom_point(size=4) +
  geom_smooth(method = "lm",col="black",size=.5,lty="longdash",se = TRUE,level=.90) +
  geom_abline(slope = 1, intercept = 0) +
  geom_label_repel(aes(label = labels),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   size          = 2,
                   segment.color = 'grey50') +
  scale_shape_manual(values=c(16,17)) +
  xlab("Disimilarity Index \n  Hispanic/LatinX-Whites") + 
  ylab('Disimilarity Index \n African American-Whites') +
  # ylim(min(diss$diss_h_w),max(diss$diss_b_w)) +
  xlim(min(diss$diss_h_w),max(diss$diss_h_w)) +
  #coord_cartesian(xlim = c(min(diss$diss_h_w),max(diss$diss_b_w)), clip = 'off') +
  #coord_equal() +
  geom_text(aes(y=0.32,x=.38,label = "x=y", vjust = -1)) +
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
ggsave("views/diss_correlation.pdf",height = 8,width = 16)


