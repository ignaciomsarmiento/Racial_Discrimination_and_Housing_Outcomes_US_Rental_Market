##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("sf","ggplot2","maps","ggthemes","stringr","haven","tidyverse","forcats","plotly")
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




diss_b<-db[db$race=="African American","diss"]
kfr_b<-db[db$race=="African American","kfr"]
diss_b_seq<-seq(min(diss_b),max(diss_b),0.01)
kfr_b_seq<-seq(min(kfr_b),max(kfr_b),0.01)



diss_h<-db[db$race=="Hispanic/LatinX","diss"]
kfr_h<-db[db$race=="Hispanic/LatinX","kfr"]
diss_h_seq<-seq(min(diss_h),max(diss_h),0.01)
kfr_h_seq<-seq(min(kfr_h),max(kfr_h),0.01)




power<-2
reg_afam<-lm(RR~poly(diss,power):poly(kfr,power),db %>% filter(race=="African American"))
reg_hisp<-lm(RR~poly(diss,power):poly(kfr,power),db %>% filter(race=="Hispanic/LatinX"))

fx_afam<- function(diss,kfr) predict(reg_afam,newdata = data.frame(diss=diss,kfr=kfr))
fx_hisp<- function(diss,kfr) predict(reg_hisp,newdata = data.frame(diss=diss,kfr=kfr))



z_afam <- outer(diss_b_seq, kfr_b_seq, fx_afam)
z_hisp <- outer(diss_h_seq, kfr_h_seq, fx_hisp)


# fig <- plot_ly(showscale = FALSE)
# fig <- fig %>% add_surface(z = ~z_afam)
# fig <- fig %>% add_surface(z = ~z_hisp, opacity = 0.98)
# fig <- fig %>% layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))

z_afam<-matrix(z_afam,nrow=length(diss_b_seq),ncol=length(kfr_b_seq))
rownames(z_afam)<-diss_b_seq
colnames(z_afam)<-kfr_b_seq

z_hisp<-matrix(z_hisp,nrow=length(diss_h_seq),ncol=length(kfr_h_seq))
rownames(z_hisp)<-diss_h_seq
colnames(z_hisp)<-kfr_h_seq

black <- plot_ly() %>% 
  add_trace(z = z_afam,
            x = diss_b_seq,
            y = kfr_b_seq,
            type = "surface", colorscale =  list(c(0, 1), c("blue", "blue")))  %>% 
  layout(scene = list(xaxis = list(title = "Dissimilartiy Index"),
                      yaxis = list(title = "Income Rank Gaps"),
                      zaxis = list(title = "Relative Responses")))
black
hispanic<-  plot_ly() %>% 
  add_trace(z = z_hisp,
            x = diss_h_seq,
            y = kfr_h_seq,
            type = "surface", colorscale =  list(c(0, 1), c("orange", "orange"))) %>% 
  layout(scene = list(xaxis = list(title = "Dissimilartiy Index"),
                      yaxis = list(title = "Income Rank Gaps"),
                      zaxis = list(title = "Relative Responses")))

hispanic

black %>% add_trace(z = z_hisp,
                    x = diss_h_seq,
                    y = kfr_h_seq,
                    type = "surface", colorscale =  list(c(0, 1), c("orange", "orange"))) %>% 
  layout(scene = list(xaxis = list(title = "Dissimilartiy Index"),
                      yaxis = list(title = "Income Rank Gaps"),
                      zaxis = list(title = "Relative Responses")))
