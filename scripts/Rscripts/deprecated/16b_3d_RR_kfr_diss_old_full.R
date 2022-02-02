##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("sf","ggplot2","maps","ggthemes","stringr","haven","tidyverse","forcats")
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




oppo_data<-read_dta("stores/matchedinquiries_opportunities_cz.dta")

sum_opp0<- oppo_data %>% dplyr::select(starts_with(c("CBSA","kfr","kir","jail")),
                                       contains(c("white","black","hispanic")),
                                       ends_with(c("p25","mean","p75"))
                                       )



sum_opp0<- sum_opp0 %>%
            group_by(CBSA) %>% 
              summarize_all(median, na.rm = TRUE)
                        

# diff_maker<-function(db,var,gender,percentile){
#   var<-enquo(var)
#   gender<-enquo(gender)
#   percentile<-enquo(percentile)
#   #paste("diff",!!var,"white","hispanic",!!gender,!!percentile,sep="_")
#   db <- db %>% mutate(x=paste(!!var,"white",!!gender,!!percentile,sep="_")-paste(!!var,"hispanic",!!gender,!!percentile,sep="_")
#                       #paste("diff",!!var,"white","black",!!gender,!!percentile,sep="_")=paste(!!var,"white",!!gender,!!percentile,sep="_")-paste(!!var,"black",!!gender,!!percentile,sep="_")
#   )
#   
#    
#   return(db)
#   
# }


# sum_opp0<-diff_maker(sum_opp0,"kfr","pooled","mean")

diff <- sum_opp0 %>% 
  mutate(diff_kfr_white_black_pooled_mean=kfr_white_pooled_mean-kfr_black_pooled_mean,
         diff_kfr_white_hisp_pooled_mean=kfr_white_pooled_mean-kfr_hisp_pooled_mean
  ) %>% 
  select(CBSA,starts_with("diff"))


diff <- diff %>% 
          pivot_longer(!CBSA,names_to= "outcomes",values_to="difference_perc_white")

diff <- diff %>% 
        separate(col=outcomes,into=c("diff","outcome","white","race","gender","percentile")) %>% 
        dplyr::select( -white,-diff) %>% 
        mutate(race=case_when(race == "black" ~ "afam",
                         race == "hisp" ~ "hispanic"),
          race=factor(race,levels=c("afam","hispanic"),labels=c("African American","Hispanic/LatinX"))) %>%
        rename(cbsatitle=CBSA)


diff_w<- diff %>% pivot_wider(names_from="outcome",values_from="difference_perc_white") 


db<-rbind(blacks,hispanics)
db<-db %>% mutate(race=factor(race,levels=c("afam","hispanic"),labels=c("African American","Hispanic/LatinX")))

db<-left_join(db,diff_w)

require("plotly")

axy <- list(
  title = "Relative Responses"
)

axx <- list(
  title = "Dissimilartiy Index"
)

axz <- list(
  title = "Income Rank Gaps"
)


pal<-c("#d8b365","#5ab4ac")
shapes<-c(16,17)

fig <- plot_ly(y=db$RR, x=db$diss, z=db$kfr, type="scatter3d", mode="markers", 
               color=db$race,colors = pal, symbols = c('circle','triangle'),marker = list(symbol = 'circle'), text = ~paste('CBSA:', db$cbsatitle, '<br>RR:', round(db$RR,3), '<br>Diss:', round(db$diss,3), '<br>Gap:', round(db$kfr,3))) 
fig <- fig %>% layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))
        
fig

# fig <- db %>% 
#       plot_ly(type="scatter3d") %>% 
#       add_markers( y=~RR, x=~diss, z=~kfr, color=~race,colors = pal, symbol = ~race, symbols = c('circle','triangle'))%>% 
#       layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))
# fig
# 
# p <- plotly_build(fig)
# p$x$data[[2]]$marker$symbol <- "triangle"
# p



dbr<-db %>% filter(race=="African American")
reg<-lm(RR~poly(diss,2)+poly(kfr,2),dbr)
summary(reg)

diss<-seq(min(dbr$diss),max(dbr$diss),0.01)
kfr<-seq(min(dbr$kfr),max(dbr$kfr),0.01)
fx<- function(diss,kfr) reg$coefficients[1]+reg$coefficients[2]*diss+reg$coefficients[3]*diss^2+reg$coefficients[4]*kfr+reg$coefficients[5]*kfr^2

z <- outer(diss, kfr, fx)
col.pal<-colorRampPalette(c("blue", "red"))
colors<-col.pal(100)
# height of facets
z.facet.center <- (z[-1, -1] + z[-1, -ncol(z)] + z[-nrow(z), -1] + z[-nrow(z), -ncol(z)])/4
# Range of the facet center on a 100-scale (number of colors)
z.facet.range<-cut(z.facet.center, 100)


fig = plot_ly(z = ~z) %>% add_surface()
fig%>% 
  add_trace(y=db$RR, x=db$diss, z=db$kfr, type="scatter3d", mode="markers",
            marker = list(size = 5, color = "red", symbol = 104))

par(mar = rep(0,4))
persp(x, y, z, 
      theta = 25, phi = 45, expand = 0.5,
      shade=NA, col=colors[z.facet.range], #border="grey80",
      box=TRUE,
      border=NA,
      ticktype = "detailed",
      xlab = "diss", ylab = "kfr", zlab = "RR")

