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

whites<-dta %>% dplyr::select(cbsa_st, cbsas_resp_white, cbsas_resp_lci_white, cbsas_resp_uci_white)
whites<- whites %>% mutate(cbsa_st=fct_reorder(cbsa_st,-cbsas_resp_white)) %>% mutate(across(where(is.numeric),~.x*100))


ggplot(whites, aes(x = cbsas_resp_white, y = cbsa_st, xmin = cbsas_resp_lci_white, xmax = cbsas_resp_uci_white)) +
  geom_point() +
  geom_errorbarh(height=.2) +
  ylab("CBSAs") + xlab('Response White Identity') +
  theme_bw() +
  geom_vline(aes(xintercept=60), colour="black", linetype="twodash") +
  theme(legend.title= element_blank() ,
        legend.position="bottom",
        legend.justification=c(1,1),
        legend.direction="horizontal",
        legend.box="horizontal",
        legend.box.just = c("top"),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill='transparent'),
        axis.text.x =element_text( angle=45),
        rect = element_rect(colour = "transparent", fill = "white"),
        plot.margin = unit(c(2,16,1,1), "lines"))  #the second number gives me room to add the labels
ggsave("views/cbsas_response_whites.pdf",height = 16,width = 11)
knitr::plot_crop("views/cbsas_response_whites.pdf")



#Disimilartiy Table

diss<-dta %>% dplyr::select(cbsa_st,diss_b_w,diss_h_w)
require("stargazer")
stargazer(data.frame(diss),type="latex",summary=FALSE,rownames = FALSE,out="views/dissimilarity_cities.tex",digits=4)

