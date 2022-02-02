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


dta_afam<- dta %>% filter(grepl("African",name)) %>% mutate(name=str_remove(name,"African American x "),
                                                            name=ifelse(name=="Response=1","Response","No Response"))
dta_hisp<- dta %>% filter(grepl("Hisp",name)) %>% mutate(name=str_remove(name,"Hispanic/LatinX x "),
                                                         name=ifelse(name=="Response=1","Response","No Response"))


#source("scripts/Rscripts/aux_forest_plot/1_call_n_rename_whiterenter_share.R")
source("scripts/Rscripts/aux_forest_plot/2_call_n_rename_white_share.R")
# #source("scripts/Rscripts/aux_forest_plot/3_call_n_rename_blackrenter_share.R")
source("scripts/Rscripts/aux_forest_plot/4_call_n_rename_black_share.R")
# #source("scripts/Rscripts/aux_forest_plot/5_call_n_rename_hispanicrenter_share.R")
source("scripts/Rscripts/aux_forest_plot/6_call_n_rename_hispanic_share.R")
source("scripts/Rscripts/aux_forest_plot/7_call_n_rename_median_income.R")
source("scripts/Rscripts/aux_forest_plot/8_call_n_rename_rentsq.R")
source("scripts/Rscripts/aux_forest_plot/9_call_n_rename_downtown_regions.R")
source("scripts/Rscripts/aux_forest_plot/10_call_n_rename_black_gap.R")
source("scripts/Rscripts/aux_forest_plot/11_call_n_rename_hisp_gap.R")
source("scripts/Rscripts/aux_forest_plot/12_call_n_rename_black_diss.R")
source("scripts/Rscripts/aux_forest_plot/13_call_n_rename_hisp_diss.R")





ggplot_pers<-function(dta,color_fondo="white",color_lineas="gray95",posicion="left",labx="",choice_axis=TRUE){
  p<-ggplot(dta, aes(x = coef, y = name, xmin = lci, xmax = uci)) +
    #geom_pointrange(shape = 22, fill = "black",size=.3) +
    geom_point(size=2) +
    geom_errorbarh(height=.2) +
    geom_vline(xintercept = 1, linetype = 3) +
    scale_x_log10(limits=c(0.02,2.5)) +
    scale_y_discrete(name="",position = posicion)  +
    xlab(labx) +
    theme(panel.border = element_blank(),
          panel.background = element_rect(fill = color_fondo,
                                                  colour = "white",
                                                  size = 0.25, linetype = "solid"), 
          panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                                  colour = color_lineas))
  
  if(choice_axis==TRUE) p<- p + theme(axis.text.x=element_blank(),
                                      axis.ticks.x =element_blank() )
  p  
}



#full sample
p_dta_afam<-ggplot_pers(dta_afam) 
p_dta_hisp<-ggplot_pers(dta_hisp,posicion="right") 

g_dta_afam         <- ggplotGrob(p_dta_afam)[-(4:6),]
g_dta_hisp         <- ggplotGrob(p_dta_hisp)[-(4:6),]



for(i in c("w_sh","bk_sh",'hisp_sh', "medinc", "rent_sqf", "diss_bk_w", "diss_hisp_w", "diff_w_bk", "diff_w_hisp")){

      eval(parse(text=paste("p_",i,"_below_afam<-ggplot_pers(",i,"_below_afam,color_fondo='gray95',color_lineas='white',posicion='left')",sep="")))
      eval(parse(text=paste("p_",i,"_below_hisp<-ggplot_pers(",i,"_below_hisp,color_fondo='gray95',color_lineas='white',posicion='right')",sep="")))
      eval(parse(text=paste("p_",i,"_above_afam<-ggplot_pers(",i,"_above_afam,posicion='left')",sep="")))
      eval(parse(text=paste("p_",i,"_above_hisp<-ggplot_pers(",i,"_above_hisp,posicion='right')",sep="")))
      
      
      for(j in c("afam","hisp")){
          eval(parse(text=paste("g_",i,"_below_",j,"<-ggplotGrob(p_",i,"_below_",j,")[-(1:2),]",sep="")))
          eval(parse(text=paste("g_",i,"_above_",j,"<-ggplotGrob(p_",i,"_above_",j,")[-(4:6),]",sep="")))
        
      }
}      



for(i in c("downtown",'midwest', "south")){
    eval(parse(text=paste("p_",i,"_afam<-ggplot_pers(",i,"_afam,color_fondo='gray95',color_lineas='white',posicion='left')",sep="")))
    eval(parse(text=paste("p_",i,"_hisp<-ggplot_pers(",i,"_hisp,color_fondo='gray95',color_lineas='white',posicion='right')",sep="")))
    for(j in c("afam","hisp")){
      eval(parse(text=paste("g_",i,"_",j,"<-ggplotGrob(p_",i,"_",j,")[-(1:2),]",sep="")))
      eval(parse(text=paste("g_",i,"_",j,"<-ggplotGrob(p_",i,"_",j,")[-(4:6),]",sep="")))
      
    }
}  


for(i in c("suburb", "northeast", "west")){
  
  eval(parse(text=paste("p_",i,"_afam<-ggplot_pers(",i,"_afam,posicion='left')",sep="")))
  eval(parse(text=paste("p_",i,"_hisp<-ggplot_pers(",i,"_hisp,posicion='right')",sep="")))
  for(j in c("afam","hisp")){
    eval(parse(text=paste("g_",i,"_",j,"<-ggplotGrob(p_",i,"_",j,")[-(1:2),]",sep="")))
    eval(parse(text=paste("g_",i,"_",j,"<-ggplotGrob(p_",i,"_",j,")[-(4:6),]",sep="")))
    
  }
}  



#Last Plot incorporates X axis labels
i<-"diff_w_hisp"
eval(parse(text=paste("p_",i,"_below_afam<-ggplot_pers(",i,"_below_afam,color_fondo='gray95',color_lineas='white',posicion='left')",sep="")))
eval(parse(text=paste("p_",i,"_below_hisp<-ggplot_pers(",i,"_below_hisp,color_fondo='gray95',color_lineas='white',posicion='right')",sep="")))
eval(parse(text=paste("p_",i,"_above_afam<-ggplot_pers(",i,"_above_afam,posicion='left',labx='African American',choice_axis=FALSE)",sep="")))
eval(parse(text=paste("p_",i,"_above_hisp<-ggplot_pers(",i,"_above_hisp,posicion='right',labx='Hispanic/LatinX',choice_axis=FALSE)",sep="")))

for(j in c("afam","hisp")){
  eval(parse(text=paste("g_",i,"_below_",j,"<-ggplotGrob(p_",i,"_below_",j,")[-(1:2),]",sep="")))
  eval(parse(text=paste("g_",i,"_above_",j,"<-ggplotGrob(p_",i,"_above_",j,")[-(4:6),]",sep="")))
  
}




resize_heights <- function(g, heights = rep(1, length(idpanels))){
  #https://stackoverflow.com/questions/35911121/equalizing-ggplot2-panel-heights-in-a-stacked-plot-with-arrangegrob
  idpanels <- unique(g$layout[grepl("panel",g$layout$name), "t"])
  g$heights[idpanels] <- unit.c(do.call(unit, list(heights, 'null')))
  g
}

#Full Sample
grop_funct<-function(text,fillrect=NA){
  gtext<-  textGrob(text, rot = 90,  
                         gp = gpar(cex = .9, fontface = 'bold', col = "black",fill="gray95"))
  gtext <- grobTree( rectGrob(gp=gpar(fill=fillrect,col=NA)), gtext)
  gtext
  }
# gtext_full<-  textGrob("Full Sample", rot = 90,  
#                        gp = gpar(cex = .9, fontface = 'bold', col = "black",fill="gray95"))
# gtext_full <- grobTree( rectGrob(gp=gpar(fill=NA,col=NA)), gtext_full)

gtext_full     <-  grop_funct("Full Sample")
gtext_white    <-  grop_funct("White \n Population Share",fillrect="gray95")
gtext_black    <-  grop_funct("African American \n Population Share")
gtext_hisp     <-  grop_funct("Hispanic/LatinX \n  Population Share",fillrect="gray95")
gtext_medinc   <-  grop_funct("Median Income")
gtext_rent_sqf <-  grop_funct("Rent per Sqft.",fillrect="gray95")
gtext_downtown <-  grop_funct("Downtown")
gtext_suburb   <-  grop_funct("Suburb")
gtext_region   <-  grop_funct("Region",fillrect="gray95")
gtext_diss_bk_w   <-   grop_funct("White - African American \n Dissimilarity Index")
gtext_diss_hisp_w   <-  grop_funct("White - Hispanic/LatinX \n Dissimilarity Index")
gtext_diff_bk_w   <-   grop_funct("White - African American \n Intergenerational Income Gap",fillrect="gray95")
gtext_diff_hisp_w   <-  grop_funct("White - Hispanic/LatinX  \n  Intergenerational Income Gap",fillrect="gray95")




gtext_space<-  textGrob(" ", rot = 90,  
                        gp = gpar(cex = .9, fontface = 'bold', col = "black"))

gtext_below<-  textGrob("Below Median", rot = 90,  
                        gp = gpar(cex = .9, fontface = 'bold', col = "black"))
gtext_above<-  textGrob("Above Median", rot = 90,  
                        gp = gpar(cex = .9, fontface = 'bold', col = "black"))


gtext_midwest<-  textGrob("Midwest", rot = 90,  
                        gp = gpar(cex = .9, fontface = 'bold', col = "black"))
gtext_northeast<-  textGrob("Northeast", rot = 90,  
                        gp = gpar(cex = .9, fontface = 'bold', col = "black"))
gtext_south<-  textGrob("South", rot = 90,  
                            gp = gpar(cex = .9, fontface = 'bold', col = "black"))
gtext_west<-  textGrob("West", rot = 90,  
                        gp = gpar(cex = .9, fontface = 'bold', col = "black"))






times<-10
layout <- rbind(c(51,52,rep(1,times),rep(2,times)),
                c(53,54,rep(3,times),rep(4,times)),
                c(53,55,rep(5,times),rep(6,times)),
                c(56,57,rep(7,times),rep(8,times)),
                c(56,58,rep(9,times),rep(10,times)),
                c(59,60,rep(11,times),rep(12,times)),
                c(59,61,rep(13,times),rep(14,times)),
                c(62,63,rep(15,times),rep(16,times)),
                c(62,64,rep(17,times),rep(18,times)),
                c(65,66,rep(19,times),rep(20,times)),
                c(65,67,rep(21,times),rep(22,times)),
                c(68,69,rep(23,times),rep(24,times)),
                c(70,71,rep(25,times),rep(26,times)),
                c(72,73,rep(27,times),rep(28,times)),
                c(72,74,rep(29,times),rep(30,times)),
                c(72,75,rep(31,times),rep(32,times)),
                c(72,76,rep(33,times),rep(34,times)),
                c(77,78,rep(35,times),rep(36,times)),
                c(77,79,rep(37,times),rep(38,times)),
                c(80,81,rep(39,times),rep(40,times)),
                c(80,82,rep(41,times),rep(42,times)),
                c(83,84,rep(43,times),rep(44,times)),
                c(83,85,rep(45,times),rep(46,times)),
                c(86,87,rep(47,times),rep(48,times)),
                c(86,88,rep(49,times),rep(50,times))
                )



grob <- arrangeGrob(g_dta_afam, g_dta_hisp,
                    g_w_sh_below_afam, g_w_sh_below_hisp,
                    g_w_sh_above_afam, g_w_sh_above_hisp,
                    g_bk_sh_below_afam, g_bk_sh_below_hisp,
                    g_bk_sh_above_afam, g_bk_sh_above_hisp,
                    g_hisp_sh_below_afam, g_hisp_sh_below_hisp,
                    g_hisp_sh_above_afam, g_hisp_sh_above_hisp,
                    g_medinc_below_afam, g_medinc_below_hisp,  
                    g_medinc_above_afam, g_medinc_above_hisp,  
                    g_rent_sqf_below_afam, g_rent_sqf_below_hisp, 
                    g_rent_sqf_above_afam, g_rent_sqf_above_hisp, 
                    g_downtown_afam,g_downtown_hisp,
                    g_suburb_afam,g_suburb_hisp,
                    g_midwest_afam,g_midwest_hisp,
                    g_northeast_afam,g_northeast_hisp,
                    g_south_afam, g_south_hisp,
                    g_west_afam, g_west_hisp, #17
                    #disimilarity
                    g_diss_bk_w_below_afam, g_diss_bk_w_below_hisp,  #18
                    g_diss_bk_w_above_afam, g_diss_bk_w_above_hisp,  #19
                    g_diss_hisp_w_below_afam, g_diss_hisp_w_below_hisp,  #20
                    g_diss_hisp_w_above_afam, g_diss_hisp_w_above_hisp, #21
                    #Intergen Gap
                    g_diff_w_bk_below_afam, g_diff_w_bk_below_hisp, #22
                    g_diff_w_bk_above_afam, g_diff_w_bk_above_hisp,  #23
                    g_diff_w_hisp_below_afam, g_diff_w_hisp_below_hisp, #24
                    g_diff_w_hisp_above_afam, g_diff_w_hisp_above_hisp, #25
                    #Labels (50)
                    gtext_full,gtext_space, #51,52
                    gtext_white,gtext_below,gtext_above, #53,54,55
                    gtext_black,gtext_below,gtext_above, #56,57,58
                    gtext_hisp,gtext_below,gtext_above, #59,60,61
                    gtext_medinc,gtext_below,gtext_above, #62,63,64
                    gtext_rent_sqf,gtext_below,gtext_above, #65,66,67
                    gtext_downtown,gtext_space, #68,69
                    gtext_suburb,gtext_space, #70,71
                    gtext_region,gtext_midwest,gtext_northeast,gtext_south,gtext_west, #72,73,74,75,76
                    gtext_diss_bk_w,gtext_below,gtext_above, #77,78,79
                    gtext_diss_hisp_w,gtext_below,gtext_above, #80,81,82
                    gtext_diff_bk_w,gtext_below,gtext_above, #83,84,85
                    gtext_diff_hisp_w,gtext_below,gtext_above, #86,87,88
                    layout_matrix=layout)

length(grob)

#padding.v = unit(4, "mm"),
# grid.newpage()
# grid.draw(resize_heights(grob,rep(.25,length(grob))))
ggsave(plot=grob,"views/infoUSA_forest_shares.pdf",height = 32,width = 12)


