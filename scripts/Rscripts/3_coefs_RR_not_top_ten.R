##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("ggplot2","ggthemes","stringr","dplyr","forcats","egg","ggsci")
lapply(pkg, require, character.only=T)
rm(pkg)
require()

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



dta_rr<- dta_rr %>% arrange(cbsas_hispanic_coef) %>% mutate(n_hispanic=row_number())
dta_rr<- dta_rr %>% arrange(cbsas_afam_coef) %>% mutate(n_afam=row_number())
dta_rr<- dta_rr %>% mutate(shade=ifelse(n_afam<=15 &n_hispanic<=15,"bad_both",
                                        ifelse(n_hispanic<=15,"bad_hispanics",
                                               ifelse(n_afam<=15,"bad_blacks","middle" ))))

# dta_rr<- dta_rr %>% mutate(shade=ifelse(cbsatitle%in%c("Louisville-Jefferson County, KY-IN",
#                                                        "Hartford-West Hartford-East Hartford, CT"#,
#                                                        #"Charlotte-Concord-Gastonia, NC-SC"#,
#                                                        #"Salt Lake City, UT",
#                                                        #"Houston-The Woodlands-Sugar Land, TX"
#                                                        ),"bad_both",
#                                                         ifelse(cbsatitle%in%c("Chicago-Naperville-Elgin, IL-IN-WI",
#                                                                               "Los Angeles-Long Beach-Anaheim, CA",
#                                                                               "Minneapolis-St. Paul-Bloomington, MN-WI",
#                                                                               "Virginia Beach-Norfolk-Newport News, VA-NC",
#                                                                               "Memphis, TN-MS-AR",
#                                                                               "San Francisco-Oakland-Hayward, CA",
#                                                                               "Riverside-San Bernardino-Ontario, CA",
#                                                                               #"New York-Newark-Jersey City, NY-NJ-PA",
#                                                                               "Pittsburgh, PA"#,
#                                                                               #"Cleveland-Elyria, OH",
#                                                                               #"Milwaukee-Waukesha-West Allis, WI",
#                                                                               #"Richmond, VA"
#                                                                               ),"bad_blacks",
#                                                                ifelse(cbsatitle%in%c(
#                                                                              "Houston-The Woodlands-Sugar Land, TX",
#                                                                              #"Louisville-Jefferson County, KY-IN",
#                                                                              "Providence-Warwick, RI-MA",
#                                                                              "Cincinnati, OH-KY-IN",
#                                                                              "Boston-Cambridge-Newton, MA-NH",
#                                                                              "Seattle-Tacoma-Bellevue, WA",
#                                                                              "Charlotte-Concord-Gastonia, NC-SC",
#                                                                              #"Hartford-West Hartford-East Hartford, CT",
#                                                                              "Indianapolis-Carmel-Anderson, IN",
#                                                                              "Nashville-Davidson--Murfreesboro--Franklin, TN"
#                                                                                      ),"bad_hispanics","middle"))))

#scales
# grey.colors(4, start = 0.2, end = .5)
# scale_color_aaas()
# discrete_scale("colour", "aaas")
# pal_aaas(palette ="default")(4)



dta_rr1<- dta_rr %>% filter(n_afam<16) %>% mutate(cbsatitle=fct_reorder(cbsatitle,-cbsas_afam_coef))
aa<-ggplot(dta_rr1, aes(x = cbsas_afam_coef, y = cbsatitle, xmin = cbsas_afam_lci, xmax = cbsas_afam_uci,col=shade,shape=shade)) +
  geom_point(size=2) +
  geom_errorbarh(height=.2) +
  scale_colour_manual(values=c("#3B4992FF", "#EE0000FF", "#631879FF", "#CCCCCC")) +
  scale_shape_manual(values=c(15,16,17,18)) +
  ylab("CBSAs") +
  xlab("African American") +
  theme_bw() +
  geom_vline(aes(xintercept=0), colour="black", linetype="twodash") +
  coord_cartesian(xlim = c(-.38,.23), clip = 'off') +
  theme(legend.title= element_blank() ,
        legend.position="none",
        legend.justification=c(1,1),
        legend.direction="horizontal",
        legend.box="horizontal",
        legend.box.just = c("top"),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill='transparent'),
        axis.text.x =element_text(size=16),
        axis.text.y =element_text(size=16),
        text=element_text(size=16),
        rect = element_rect(colour = "transparent", fill = "white"))  

dta_rr2<- dta_rr %>% filter(n_hispanic<16) %>% mutate(cbsatitle=fct_reorder(cbsatitle,-cbsas_hispanic_coef))
hisp<-ggplot(dta_rr2, aes(x = cbsas_hispanic_coef, y = cbsatitle, xmin = cbsas_hispanic_lci, xmax = cbsas_hispanic_uci,col=shade,shape=shade)) +
  geom_point(size=2) +
  geom_errorbarh(height=.2) +
  scale_colour_manual(values=c(  "#EE0000FF","#631879FF")) +
  scale_shape_manual(values=c(16,17,18)) +
  scale_y_discrete("CBSAs", position = "right") +
  xlab("Hispanic/LatinX") +
  theme_bw() +
  geom_vline(aes(xintercept=0), colour="black", linetype="twodash") +
  coord_cartesian(xlim = c(-.38,.23), clip = 'off') +
  theme(legend.title= element_blank() ,
        legend.position="none",
        legend.justification=c(1,1),
        legend.direction="horizontal",
        legend.box="horizontal",
        legend.box.just = c("top"),
        panel.grid.major.x = element_blank(),
        legend.background = element_rect(fill='transparent'),
        axis.text.x =element_text(size=16),
        axis.text.y =element_text(size=16),
        text=element_text(size=16),
        rect = element_rect(colour = "transparent", fill = "white"))  



cmb<-egg::ggarrange(aa,hisp ,nrow=1)
ggsave("views/RR_cbsas_not_top15.pdf",cmb,height = 5,width = 20)
