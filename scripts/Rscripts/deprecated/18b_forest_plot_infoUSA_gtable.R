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

# dta<-read_dta("stores/matrices/temp/same_race.dta")
# colnames(dta)<-c("coef","lci", "uci", "pval")
# dta$name<-c("AfAm_choice", "Hispanic_choice", "AfAm", "Hispanic")
# 
# 
# dta <- dta %>% mutate_at(vars("coef","lci", "uci"),exp)
# 
# dta <- dta %>% mutate(name=factor(name,levels=c("AfAm_choice", "AfAm", "Hispanic_choice", "Hispanic"),
#                                   labels= c("African American x Response=1", 
#                                             "African American x Response=0", 
#                                             "Hispanic/LatinX x Response=1",
#                                             "Hispanic/LatinX x Response=0"),
#                                   ordered=TRUE)
#                       )
# 
# 
# source("scripts/Rscripts/aux_forest_plot/1_call_n_rename_whiterenter_share.R")
# source("scripts/Rscripts/aux_forest_plot/2_call_n_rename_white_share.R")
# source("scripts/Rscripts/aux_forest_plot/3_call_n_rename_blackrenter_share.R")
#source("scripts/Rscripts/aux_forest_plot/4_call_n_rename_black_share.R")
#source("scripts/Rscripts/aux_forest_plot/5_call_n_rename_hispanicrenter_share.R")
#source("scripts/Rscripts/aux_forest_plot/6_call_n_rename_hispanic_share.R")
#source("scripts/Rscripts/aux_forest_plot/7_call_n_rename_median_income.R")
source("scripts/Rscripts/aux_forest_plot/8_call_n_rename_rentsq.R")
# source("scripts/Rscripts/aux_forest_plot/9_call_n_rename_downtown_regions.R")

dta_afam<- dta %>% filter(grepl("African",name)) %>% mutate(name=str_remove(name,"African American x "),
                                                            name=ifelse(name=="Response=1","Response","No Response"))
dta_hisp<- dta %>% filter(grepl("Hisp",name)) %>% mutate(name=str_remove(name,"Hispanic/LatinX x "),
                                                         name=ifelse(name=="Response=1","Response","No Response"))




ggplot_pers<-function(dta,color_fondo="white",color_lineas="gray95",posicion="left",labx="",choice_axis=TRUE){
  p<-ggplot(dta, aes(x = coef, y = name, xmin = lci, xmax = uci)) +
    geom_pointrange(shape = 22, fill = "black",size=.7) +
    geom_vline(xintercept = 1, linetype = 3) +
    scale_x_log10(limits=c(0.05,2)) +
    scale_y_discrete(name="",position = posicion)  +
    xlab(labx) +
    theme(panel.border = element_blank(),
          panel.background = element_rect(fill = color_fondo,
                                                  colour = "white",
                                                  size = 0.25, linetype = "solid"), 
          panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                                  colour = color_lineas))
  
  if(choice_axis==TRUE) p<- p + theme(axis.text.x=element_blank())
  p  
}

p_dta_afam<-ggplot_pers(dta_afam) 
p_dta_hisp<-ggplot_pers(dta_hisp,posicion="right") 

p_w_rent_sh_below_afam<-ggplot_pers(w_rent_sh_below_afam,color_fondo="gray95",color_lineas="white",posicion="left") 
p_w_rent_sh_below_hisp<-ggplot_pers(w_rent_sh_below_afam,color_fondo="gray95",color_lineas="white",posicion="right") 

p_w_rent_sh_above_afam<-ggplot_pers(w_rent_sh_above_afam) 
p_w_rent_sh_above_hisp<-ggplot_pers(w_rent_sh_above_hisp,posicion="right") 

p_w_sh_below_afam<-ggplot_pers(w_sh_below_afam,color_fondo="gray95",color_lineas="white",posicion="left") 
p_w_sh_below_hisp<-ggplot_pers(w_sh_below_afam,color_fondo="gray95",color_lineas="white",posicion="right")

p_w_sh_above_afam<-ggplot_pers(w_sh_above_afam,labx="African American",choice_axis=FALSE) 
p_w_sh_above_hisp<-ggplot_pers(w_sh_above_hisp,posicion="right",labx="Hispanic/LatinX",choice_axis=FALSE)


g_dta_afam <- ggplotGrob(p_dta_afam)
g_dta_hisp <- ggplotGrob(p_dta_hisp)
g_w_rent_sh_below_afam <- ggplotGrob(p_w_rent_sh_below_afam)
g_w_rent_sh_below_hisp <- ggplotGrob(p_w_rent_sh_below_hisp)
g_w_rent_sh_above_afam <- ggplotGrob(p_w_rent_sh_above_afam)
g_w_rent_sh_above_hisp <- ggplotGrob(p_w_rent_sh_above_hisp)
g_w_sh_below_afam<-ggplotGrob(p_w_sh_below_afam)
g_w_sh_below_hisp<-ggplotGrob(p_w_sh_below_hisp)
g_w_sh_above_hisp<-ggplotGrob(p_w_sh_above_hisp)
g_w_sh_above_afam<- ggplotGrob(p_w_sh_above_afam)
g_w_sh_above_hisp<- ggplotGrob(p_w_sh_above_hisp)


#g$widths <- unit.pmax(p_dta_afam$widths, p_w_rent_sh_below_afam$widths,p_w_rent_sh_above_afam$widths)
# grid.newpage()
# grid.draw(g)



gtext_full<-  textGrob("Full Sample", rot = 90,  
                       gp = gpar(cex = .9, fontface = 'bold', col = "black",fill="gray95"))
gtext_full <- grobTree( rectGrob(gp=gpar(fill=NA,col=NA)), gtext_full)
gtext_space<-  textGrob(" ", rot = 90,  
                       gp = gpar(cex = .9, fontface = 'bold', col = "black"))
gtext_white_renter<-  textGrob("White Renter Share", rot = 90,  
                        gp = gpar(cex = .9, fontface = 'bold', col = "black"))
gtext_white_renter<- grobTree( rectGrob(gp=gpar(fill="gray95",col=NA)), gtext_white_renter)
gtext_below<-  textGrob("Below Median", rot = 90,  
                        gp = gpar(cex = .9, fontface = 'bold', col = "black"))
gtext_above<-  textGrob("Above Median", rot = 90,  
                        gp = gpar(cex = .9, fontface = 'bold', col = "black"))

gtext_white<-  textGrob("White Population Share", rot = 90,  
                               gp = gpar(cex = .9, fontface = 'bold', col = "black"))
gtext_white<- grobTree( rectGrob(gp=gpar(fill=NA,col=NA)), gtext_white)
times<-8
layout <- rbind(c(11,12,rep(1,times),rep(2,times)),
                c(13,14,rep(3,times),rep(4,times)),
                c(13,15,rep(5,times),rep(6,times)),
                c(16,17,rep(7,times),rep(8,times)),
                c(16,18,rep(9,times),rep(10,times))
                )

grob
grob <- arrangeGrob(g_dta_afam, g_dta_hisp,
                    g_w_rent_sh_below_afam, g_w_rent_sh_below_hisp,
                    g_w_rent_sh_above_afam, g_w_rent_sh_above_hisp,
                    g_w_sh_below_afam,g_w_sh_below_hisp,
                    g_w_sh_above_afam,g_w_sh_above_hisp,
                    gtext_full,gtext_space,
                    gtext_white_renter,gtext_below,gtext_above,
                    gtext_white,gtext_below,gtext_above,
                    layout_matrix=layout)



#padding.v = unit(4, "mm"),
grid.newpage()
grid.draw(grob)
#ggsave(plot=grob,"views/infoUSA_forest.pdf",height = 16,width = 12)

gs<-list(g_dta_afam, g_dta_hisp,
         g_w_rent_sh_below_afam, g_w_rent_sh_below_hisp,
         g_w_rent_sh_above_afam, g_w_rent_sh_above_hisp,
         g_w_sh_below_afam,g_w_sh_below_hisp,
         g_w_sh_above_afam,g_w_sh_above_hisp,
         gtext_full,gtext_space,
         gtext_white_renter,gtext_below,gtext_above,
         gtext_white,gtext_below,gtext_above)

grid.arrange(grob)
             
