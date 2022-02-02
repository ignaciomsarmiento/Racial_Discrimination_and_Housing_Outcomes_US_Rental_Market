##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("tydiverse","ggplot2","haven")
lapply(pkg, require, character.only=T)
rm(pkg)
library(grid)
library(gridExtra)
#WD
setwd("~/Dropbox/Research/cbsa_discrimination/github/Racial_Discrimination_US_Rental_Market/")

dta<-read_dta("stores/matrices/temp/same_race.dta")
colnames(dta)<-c("coef","lci", "uci", "pval")
dta$name<-c("AfAm_choice", "Hispanic_choice", "AfAm", "Hispanic")


dta <- dta %>% mutate_at(vars("coef","lci", "uci"),exp)

dta <- dta %>% mutate(name=factor(name,levels=c("AfAm_choice", "AfAm", "Hispanic_choice", "Hispanic"),
                                  labels= c("African American x Response=1", 
                                            "African American x Response=0", 
                                            "Hispanic/LatinX x Response=1",
                                            "Hispanic/LatinX x Response=0"),
                                  ordered=TRUE)
                      )

#file<-tibble(name="Overall Results",coef=NA,lci=NA, uci=NA, pval=NA)
#dta<-rbind(file,dta)                      

source("scripts/Rscripts/aux_forest_plot/1_call_n_rename_whiterenter_share.R")
source("scripts/Rscripts/aux_forest_plot/2_call_n_rename_white_share.R")
source("scripts/Rscripts/aux_forest_plot/3_call_n_rename_blackrenter_share.R")
source("scripts/Rscripts/aux_forest_plot/4_call_n_rename_black_share.R")
source("scripts/Rscripts/aux_forest_plot/5_call_n_rename_hispanicrenter_share.R")
source("scripts/Rscripts/aux_forest_plot/6_call_n_rename_hispanic_share.R")
source("scripts/Rscripts/aux_forest_plot/7_call_n_rename_median_income.R")
source("scripts/Rscripts/aux_forest_plot/8_call_n_rename_rentsq.R")
source("scripts/Rscripts/aux_forest_plot/9_call_n_rename_downtown_regions.R")


  


db<-rbind(dta,
          w_rent_sh_1,
          w_rent_sh_2,
          w_sh_1,
          w_sh_2,
          bk_rent_sh_1,
          bk_rent_sh_2,
          bk_sh_1,
          bk_sh_2,
          hisp_rent_sh_1,
          hisp_rent_sh_2,
          hisp_sh_1,
          hisp_sh_2,
          medinc_1,
          medinc_2,
          rent_sqf_1,
          rent_sqf_2,
          Downtown,
          Suburb)


db$colour<-NA                 
db$colour <- c(rep(c(rep("white",4),rep("gray95",4)),9),rep("white",4))
#https://stackoverflow.com/questions/62246541/forest-plot-with-table-ggplot-coding

ggplot(db, aes(x = coef, y = name, xmin = lci, xmax = uci)) +
  geom_hline(aes(yintercept = name, colour = colour), size = 14) + 
  geom_pointrange(shape = 22, fill = "black",size=.7) +
  geom_vline(xintercept = 1, linetype = 3) +
  scale_x_log10() +
  xlab("Odds Ratio with 90% Confidence Interval") +
  ylab("")+
  theme_classic() +
  scale_colour_identity() +
  scale_y_discrete(limits = rev(levels(db$name)))  +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=20),
        axis.title=element_text(size=14)
  )
ggsave("views/infoUSA_ORs.pdf",height = 32,width = 24)  
   
#+  theme(axis.text.y = element_blank(), axis.title.y = element_blank())




db_regions<-rbind(Midwest,
                  Northeast,
                  South,
                  West)

db_regions$colour<-NA                 
db_regions$colour <- c(rep(c(rep("white",4),rep("gray95",4)),2))
#https://stackoverflow.com/questions/62246541/forest-plot-with-table-ggplot-coding

ggplot(db_regions, aes(x = coef, y = name, xmin = lci, xmax = uci)) +
  geom_hline(aes(yintercept = name, colour = colour), size = 14) + 
  geom_pointrange(shape = 22, fill = "black",size=.7) +
  geom_vline(xintercept = 1, linetype = 3) +
  scale_x_log10() +
  xlab("Odds Ratio with 90% Confidence Interval") +
  ylab("")+
  theme_classic() +
  scale_colour_identity() +
  scale_y_discrete(limits = rev(levels(db_regions$name)))  +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=20),
        axis.title=element_text(size=14)
  )
ggsave("views/infoUSA_ORs_regions.pdf",height = 7,width = 24)  

#+  theme(axis.text.y = element_blank(), axis.title.y = element_blank())


