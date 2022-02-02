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



# Full Sample -------------------------------------------------------------
dta1<-read_dta("stores/matrices/temp/RR_rel_full_sample.dta") %>% 
      mutate(sample="Full Sample")


# # -----------------------------------------------------------------------
# Population Shares -------------------------------------------------------
# # -----------------------------------------------------------------------
# Below Median ------------------------------------------------------------
#Minority share
dta20<-read_dta("stores/matrices/temp/min_sh_1.dta") %>% 
  mutate(sample="Same Race \n Population Share \n Below Median") %>% 
  filter(race=="min")
#AfAm share
dta21<-read_dta("stores/matrices/temp/bk_sh_1.dta") %>% 
      mutate(sample="Same Race \n Population Share \n Below Median") %>% 
      filter(race=="afam")
#Hispanic share
dta22<-read_dta("stores/matrices/temp/hisp_sh_1.dta") %>% 
  mutate(sample="Same Race \n Population Share \n Below Median") %>% 
  filter(race=="hisp")
#White share
dta23<-read_dta("stores/matrices/temp/hisp_sh_1.dta") %>% 
  mutate(sample="Same Race \n Population Share \n Below Median") %>% 
  filter(race=="white")


pop_share_below<-rbind(dta20,dta21,dta22,dta23)


# Above Median ------------------------------------------------------------
#Minority share
dta30<-read_dta("stores/matrices/temp/min_sh_2.dta") %>% 
  mutate(sample="Same Race \n Population Share \n Above Median") %>% 
  filter(race=="min")
#AfAm share
dta31<-read_dta("stores/matrices/temp/bk_sh_2.dta") %>% 
  mutate(sample="Same Race \n Population Share \n Above Median") %>% 
  filter(race=="afam")
#Hispanic share
dta32<-read_dta("stores/matrices/temp/hisp_sh_2.dta") %>% 
  mutate(sample="Same Race \n Population Share \n Above Median") %>% 
  filter(race=="hisp")
#White share
dta33<-read_dta("stores/matrices/temp/hisp_sh_2.dta") %>% 
  mutate(sample="Same Race \n Population Share \n Above Median")%>% 
  filter(race=="white")
pop_share_above<-rbind(dta30,dta31,dta32,dta33)






ggplot_pers<-function(dbp,color_fondo="white",color_lineas="gray95",posicion="left",labx="",choice_axis=TRUE,text_race=FALSE){
  p<-ggplot(dbp, aes(x = coef, y = var, xmin = lci, xmax = uci)) +
    #geom_pointrange(shape = 22, fill = "black",size=.3) +
    geom_point(size=2) +
    geom_text(aes(label=label_coef),hjust=.5, vjust=-1) +
    geom_errorbarh(height=.2) +
    geom_vline(xintercept = 100, linetype = 3) +
    scale_x_continuous(name="",breaks=c(0,25,50,75,100,125,150),limits=c(0,150)) +
    scale_y_discrete(name="",position = posicion)  +
    xlab(labx) +
    theme(panel.border = element_blank(),
          panel.background = element_rect(fill = color_fondo,
                                          colour = "white",
                                          size = 0.25, linetype = "solid"), 
          panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                          colour = color_lineas),
          strip.background = element_blank()) +
    facet_grid(. ~ race, switch="both")
  
  if(choice_axis==TRUE) p<- p + theme(axis.text.x=element_blank(), )
  
  if(text_race==FALSE) p <- p +theme(strip.text= element_blank())
  p  
}






for(i in c("pop_share")){
  
  eval(parse(text=paste(i,"_below<-",i,"_below %>% mutate(race=factor(race,levels=c('min', 'afam', 'hisp', 'white'),
                                       labels= c('Renters of Color',
                                                 'African American',
                                                 'Hispanic/LatinX',
                                                 'White'),
                                       ordered=TRUE),
                                        lci=ifelse(lci<0,0,lci), var='',
                                      coef=coef*100,
                                      uci=uci*100,
                                      lci=lci*100,
                                      label_coef=format(round(coef,1), nsmall = 1))",sep="")))
  eval(parse(text=paste(i,"_above<-",i,"_above %>% mutate(race=factor(race,levels=c('min', 'afam', 'hisp','choice'),
                                       labels= c('Renters of Color',
                                                 'African American',
                                                 'Hispanic/LatinX',
                                                 'White'),
                                       ordered=TRUE), 
                                      lci=ifelse(lci<0,0,lci), var='',
                                      coef=coef*100,
                                      uci=uci*100,
                                      lci=lci*100,
                                      label_coef=format(round(coef,1), nsmall = 1))",sep="")))
  
  
  eval(parse(text=paste("p_",i,"_below<-ggplot_pers(",i,"_below,posicion='left')",sep="")))
  eval(parse(text=paste("p_",i,"_above<-ggplot_pers(",i,"_above,color_fondo='gray95',color_lineas='white',posicion='left')",sep="")))
  
  eval(parse(text=paste("g_",i,"_below<-ggplotGrob(p_",i,"_below)[-(1:2),]",sep="")))
  eval(parse(text=paste("g_",i,"_above<-ggplotGrob(p_",i,"_above)[-(4:6),]",sep="")))
  

}      



for(i in c("dta1")){
  
  eval(parse(text=paste(i,"<-",i," %>% mutate(race=factor(race,levels=c('min', 'afam', 'hisp', 'white','choice'),
                                       labels= c('Renters of Color',
                                                 'African American',
                                                 'Hispanic/LatinX',
                                                 'White',
                                                  'Pooled'),
                                       ordered=TRUE),
                                      lci=ifelse(lci<0,0,lci), var='',
                                      coef=coef*100,
                                      uci=uci*100,
                                      lci=lci*100,
                                      label_coef=format(round(coef,1), nsmall = 1))",sep="")))
  
  
  eval(parse(text=paste("p_",i,"<-ggplot_pers(",i,", posicion='left',text_race=TRUE)",sep="")))
  eval(parse(text=paste("g_",i,"<-ggplotGrob(p_",i,")[-(4:6),]",sep="")))
  
  
  
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
gtext_same_pop    <-  grop_funct("Same Race \n Population Share")




gtext_space<-  textGrob(" ", rot = 90,  
                        gp = gpar(cex = .9, fontface = 'bold', col = "black"))

gtext_below<-  textGrob("Below Median", rot = 90,  
                        gp = gpar(cex = .9, fontface = 'bold', col = "black"))
gtext_above<-  textGrob("Above Median", rot = 90,  
                        gp = gpar(cex = .9, fontface = 'bold', col = "black"))





resize_heights <- function(g, heights = rep(1, length(idpanels))){
  #https://stackoverflow.com/questions/35911121/equalizing-ggplot2-panel-heights-in-a-stacked-plot-with-arrangegrob
  idpanels <- unique(g$layout[grepl("panel",g$layout$name), "t"])
  g$heights[idpanels] <- unit.c(do.call(unit, list(heights, 'null')))
  g
}

times<-10



# Minimal main section ----------------------------------------------------

layout_min <- rbind(c(1,2,rep(3,times)),
                c(4,5,rep(6,times)),
                c(4,7,rep(8,times))
              )



#Minority share
dta20<-read_dta("stores/matrices/temp/min_sh_1.dta") %>% 
  mutate(sample="Same Race \n Population Share \n Below Median") %>% 
  filter(race=="min")
#AfAm share
dta21<-read_dta("stores/matrices/temp/bk_sh_1.dta") %>% 
  mutate(sample="Same Race \n Population Share \n Below Median") %>% 
  filter(race=="afam")
#Hispanic share
dta22<-read_dta("stores/matrices/temp/hisp_sh_1.dta") %>% 
  mutate(sample="Same Race \n Population Share \n Below Median") %>% 
  filter(race=="hisp")
#White share
dta23<-read_dta("stores/matrices/temp/hisp_sh_1.dta") %>% 
  mutate(sample="Same Race \n Population Share \n Below Median") %>% 
  filter(race=="white")


pop_share_below<-rbind(dta20,dta21,dta22,dta23)


# Above Median ------------------------------------------------------------
#Minority share
dta30<-read_dta("stores/matrices/temp/min_sh_2.dta") %>% 
  mutate(sample="Same Race \n Population Share \n Above Median") %>% 
  filter(race=="min")
#AfAm share
dta31<-read_dta("stores/matrices/temp/bk_sh_2.dta") %>% 
  mutate(sample="Same Race \n Population Share \n Above Median") %>% 
  filter(race=="afam")
#Hispanic share
dta32<-read_dta("stores/matrices/temp/hisp_sh_2.dta") %>% 
  mutate(sample="Same Race \n Population Share \n Above Median") %>% 
  filter(race=="hisp")
#White share
dta33<-read_dta("stores/matrices/temp/hisp_sh_2.dta") %>% 
  mutate(sample="Same Race \n Population Share \n Above Median")%>% 
  filter(race=="white")
pop_share_above<-rbind(dta30,dta31,dta32,dta33)



#Last Plot incorporates X axis labels
i<-"pop_share_below"
eval(parse(text=paste(i,"<-",i," %>% mutate(race=factor(race,levels=c('min', 'afam', 'hisp', 'white','choice'),
                                       labels= c('Renters of Color',
                                                 'African American',
                                                 'Hispanic/LatinX',
                                                 'White',
                                                 'Pooled'),
                                       ordered=TRUE), 
                                      lci=ifelse(lci<0,0,lci), var='',
                                      coef=coef*100,
                                      uci=uci*100,
                                      lci=lci*100,
                                      label_coef=format(round(coef,1), nsmall = 1))",sep="")))
eval(parse(text=paste("p_",i,"<-ggplot_pers(",i,",posicion='left',choice_axis=FALSE,text_race=TRUE)",sep="")))
eval(parse(text=paste("g_",i,"<-ggplotGrob(p_",i,")[-(1:2),]",sep="")))



grob_min <- arrangeGrob(gtext_full,gtext_space,g_dta1, #Full sample
                    gtext_same_pop,gtext_above,g_pop_share_above,gtext_below,g_pop_share_below, #share  population
                    layout_matrix=layout_min)


ggsave(plot=grob_min,"views/infoUSA_forest_linear_min_5panels.pdf",height = 4,width = 12)
