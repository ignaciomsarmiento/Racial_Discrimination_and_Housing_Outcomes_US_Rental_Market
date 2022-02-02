##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("tidyverse","ggplot2","haven","egg","grid","gridExtra","gtable")
lapply(pkg, require, character.only=T)
rm(pkg)

#WD
setwd("~/Dropbox/Research/cbsa_discrimination/github/Racial_Discrimination_US_Rental_Market/")








ggplot(dta1, aes(x = coef, y = var, xmin = lci, xmax = uci)) +
  #geom_pointrange(shape = 22, fill = "black",size=.3) +
  geom_point(size=2) +
  geom_text(aes(label=label_coef),hjust=.5, vjust=-1) +
  geom_errorbarh(height=.2) +
  geom_vline(xintercept = 100, linetype = 3) +
  scale_x_continuous(name="",breaks=c(0,25,50,75,100,125,150),limits=c(0,150)) +
  scale_y_discrete(name="",position = "bottom")  +
  xlab("") +
  theme(panel.border = element_blank(),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.25, linetype = "solid"), 
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour ='gray95'),
        strip.background = element_blank()) 



