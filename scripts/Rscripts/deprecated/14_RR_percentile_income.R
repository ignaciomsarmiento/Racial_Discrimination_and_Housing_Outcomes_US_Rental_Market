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
         diff_kfr_white_hisp_pooled_mean=kfr_white_pooled_mean-kfr_hisp_pooled_mean,
         
         diff_kfr_white_black_pooled_p25=kfr_white_pooled_p25-kfr_black_pooled_p25,
         diff_kfr_white_hisp_pooled_p25=kfr_white_pooled_p25-kfr_hisp_pooled_p25,
         
         diff_kfr_white_black_pooled_p75=kfr_white_pooled_p75-kfr_black_pooled_p75,
         diff_kfr_white_hisp_pooled_p75=kfr_white_pooled_p75-kfr_hisp_pooled_p75,
         
         #Individual Ranking
         diff_kir_white_black_pooled_mean=kir_white_pooled_mean-kir_black_pooled_mean,
         diff_kir_white_hisp_pooled_mean=kir_white_pooled_mean-kir_hisp_pooled_mean,
         
         diff_kir_white_black_pooled_p25=kir_white_pooled_p25-kir_black_pooled_p25,
         diff_kir_white_hisp_pooled_p25=kir_white_pooled_p25-kir_hisp_pooled_p25,

         diff_kir_white_black_pooled_p75=kir_white_pooled_p75-kir_black_pooled_p75,
         diff_kir_white_hisp_pooled_p75=kir_white_pooled_p75-kir_hisp_pooled_p75,


          #Jailed
         diff_jail_white_black_pooled_mean=jail_white_pooled_mean-jail_black_pooled_mean,
         diff_jail_white_hisp_pooled_mean=jail_white_pooled_mean-jail_hisp_pooled_mean,
         
         diff_jail_white_black_pooled_p25=jail_white_pooled_p25-jail_black_pooled_p25,
         diff_jail_white_hisp_pooled_p25=jail_white_pooled_p25-jail_hisp_pooled_p25,
         
         diff_jail_white_black_pooled_p75=jail_white_pooled_p75-jail_black_pooled_p75,
         diff_jail_white_hisp_pooled_p75=jail_white_pooled_p75-jail_hisp_pooled_p75,
         
         #Jailed Males
         diff_jail_white_black_male_mean=jail_white_male_mean-jail_black_male_mean,
         diff_jail_white_hisp_male_mean=jail_white_male_mean-jail_hisp_male_mean,
         
         diff_jail_white_black_male_p25=jail_white_male_p25-jail_black_male_p25,
         diff_jail_white_hisp_male_p25=jail_white_male_p25-jail_hisp_male_p25,
         
         diff_jail_white_black_male_p75=jail_white_male_p75-jail_black_male_p75,
         diff_jail_white_hisp_male_p75=jail_white_male_p75-jail_hisp_male_p75,

         
         
         
         #married
         diff_married_white_black_pooled_p25=married_white_pooled_p25-married_black_pooled_p25,
         diff_married_white_hisp_pooled_p25=married_white_pooled_p25-married_hisp_pooled_p25,
         
         diff_married_white_black_pooled_mean=married_white_pooled_mean-married_black_pooled_mean,
         diff_married_white_hisp_pooled_mean=married_white_pooled_mean-married_hisp_pooled_mean,
         
         diff_married_white_black_pooled_p75=married_white_pooled_p75-married_black_pooled_p75,
         diff_married_white_hisp_pooled_p75=married_white_pooled_p75-married_hisp_pooled_p75,
         
         #Wage RAnk
         diff_wgflxrk_white_black_pooled_p25=wgflx_rk_white_pooled_p25-wgflx_rk_black_pooled_p25,
         diff_wgflxrk_white_hisp_pooled_p25=wgflx_rk_white_pooled_p25-wgflx_rk_hisp_pooled_p25,
         
         diff_wgflxrk_white_black_pooled_mean=wgflx_rk_white_pooled_mean-wgflx_rk_black_pooled_mean,
         diff_wgflxrk_white_hisp_pooled_mean=wgflx_rk_white_pooled_mean-wgflx_rk_hisp_pooled_mean,
         
         diff_wgflxrk_white_black_pooled_p75=wgflx_rk_white_pooled_p75-wgflx_rk_black_pooled_p75,
         diff_wgflxrk_white_hisp_pooled_p75=wgflx_rk_white_pooled_p75-wgflx_rk_hisp_pooled_p75,
         
         #Wage RAnk Males
         diff_wgflxrk_white_black_male_p25=wgflx_rk_white_male_p25-wgflx_rk_black_male_p25,
         diff_wgflxrk_white_hisp_male_p25=wgflx_rk_white_male_p25-wgflx_rk_hisp_male_p25,
         
         diff_wgflxrk_white_black_male_mean=wgflx_rk_white_male_mean-wgflx_rk_black_male_mean,
         diff_wgflxrk_white_hisp_male_mean=wgflx_rk_white_male_mean-wgflx_rk_hisp_male_mean,
         
         diff_wgflxrk_white_black_male_p75=wgflx_rk_white_male_p75-wgflx_rk_black_male_p75,
         diff_wgflxrk_white_hisp_male_p75=wgflx_rk_white_male_p75-wgflx_rk_hisp_male_p75,
         
         #Hours Worked
         diff_hourswk_white_black_pooled_p25=hours_wk_white_pooled_p25-hours_wk_black_pooled_p25,
         diff_hourswk_white_hisp_pooled_p25=hours_wk_white_pooled_p25-hours_wk_hisp_pooled_p25,
         
         diff_hourswk_white_black_pooled_mean=hours_wk_white_pooled_mean-hours_wk_black_pooled_mean,
         diff_hourswk_white_hisp_pooled_mean=hours_wk_white_pooled_mean-hours_wk_hisp_pooled_mean,
         
         diff_hourswk_white_black_pooled_p75=hours_wk_white_pooled_p75-hours_wk_black_pooled_p75,
         diff_hourswk_white_hisp_pooled_p75=hours_wk_white_pooled_p75-hours_wk_hisp_pooled_p75,
         
         #Hours Worked Males
         diff_hourswk_white_black_male_p25=hours_wk_white_male_p25-hours_wk_black_male_p25,
         diff_hourswk_white_hisp_male_p25=hours_wk_white_male_p25-hours_wk_hisp_male_p25,
         
         diff_hourswk_white_black_male_mean=hours_wk_white_male_mean-hours_wk_black_male_mean,
         diff_hourswk_white_hisp_male_mean=hours_wk_white_male_mean-hours_wk_hisp_male_mean,
         
         diff_hourswk_white_black_male_p75=hours_wk_white_male_p75-hours_wk_black_male_p75,
         diff_hourswk_white_hisp_male_p75=hours_wk_white_male_p75-hours_wk_hisp_male_p75,
         
         
         #Employment Rates
         diff_working_white_black_pooled_p25=working_white_pooled_p25-working_black_pooled_p25,
         diff_working_white_hisp_pooled_p25=working_white_pooled_p25-working_hisp_pooled_p25,
         
         diff_working_white_black_pooled_mean=working_white_pooled_mean-working_black_pooled_mean,
         diff_working_white_hisp_pooled_mean=working_white_pooled_mean-working_hisp_pooled_mean,
        
         diff_working_white_black_pooled_p75=working_white_pooled_p75-working_black_pooled_p75,
         diff_working_white_hisp_pooled_p75=working_white_pooled_p75-working_hisp_pooled_p75,
         
         #Employment Rates Males
         diff_working_white_black_male_p25=working_white_male_p25-working_black_male_p25,
         diff_working_white_hisp_male_p25=working_white_male_p25-working_hisp_male_p25,
         
         diff_working_white_black_male_mean=working_white_male_mean-working_black_male_mean,
         diff_working_white_hisp_male_mean=working_white_male_mean-working_hisp_male_mean,
         
         diff_working_white_black_male_p75=working_white_male_p75-working_black_male_p75,
         diff_working_white_hisp_male_p75=working_white_male_p75-working_hisp_male_p75
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

require("ggpubr")
require("ggsci")

plot_points<-function(dta,x,xlab){
  
  
  p<-ggplot(dta,aes_string(x=x,y="RR",group="race",col="race")) +
    geom_point(aes(group=race,shape=race),size=4) +
    geom_smooth(aes(lty=race),method = "lm",size=.5,se = TRUE,level=.95) +
    scale_shape_manual(values=c(16,17)) +
    xlab(xlab) + 
    ylab('Relative Responses') +
    scale_fill_manual(values=c("#d8b365","#5ab4ac")) +
    scale_color_manual(values=c("#d8b365","#5ab4ac")) +
    theme_bw() +
    theme(legend.title= element_blank() ,
          legend.position="bottom",
          #legend.justification=c(1,1),
          legend.direction="horizontal",
          legend.box="horizontal",
          legend.box.just = c("top"),
          panel.grid.major.x = element_blank(),
          legend.background = element_rect(fill='transparent'),
          axis.text.x =element_text(size=20, angle=45,hjust=1),
          axis.text.y =element_text(size=20),
          text = element_text(size=20),
          rect = element_rect(colour = "transparent", fill = "white"),
          plot.margin = unit(c(2,3.5,1,1), "lines"))   + stat_regline_equation() 
  return(p)
  #ggsave(p,paste0("views/",plot_name,".pdf"),height = 8,width = 16)
}



# Jailed ------------------------------------------------------------------
db %>% filter(gender=="pooled",percentile=="mean") %>%  plot_points(.,x="jail",xlab=expression(paste(Delta,"Fraction Incarcerated (White-Minority)")))
ggsave(paste0("views/","jail_pooled_mean",".pdf"),height = 8,width = 16)

db %>% filter(gender=="pooled",percentile=="p25") %>%  plot_points(.,x="jail",xlab=expression(paste(Delta,"Fraction Incarcerated (White-Minority)")))
ggsave(paste0("views/","jail_pooled_p25",".pdf"),height = 8,width = 16)

db %>% filter(gender=="pooled",percentile=="p75") %>%  plot_points(.,x="jail",xlab=expression(paste(Delta,"Fraction Incarcerated (White-Minority)")))
ggsave(paste0("views/","jail_pooled_p75",".pdf"),height = 8,width = 16)


db %>% filter(gender=="male",percentile=="p25") %>%  plot_points(.,x="jail",xlab=expression(paste(Delta,"Fraction Incarcerated (White-Minority)")))
ggsave(paste0("views/","jail_male_p25",".pdf"),height = 8,width = 16)

db %>% filter(gender=="male",percentile=="p75") %>%  plot_points(.,x="jail",xlab=expression(paste(Delta,"Fraction Incarcerated (White-Minority)")))
ggsave(paste0("views/","jail_male_p75",".pdf"),height = 8,width = 16)




db %>% filter(gender=="male",percentile=="mean") %>%  plot_points(.,x="jail",xlab=expression(paste(Delta,"Fraction Incarcerated (White-Minority)")))
ggsave(paste0("views/","jail_male_mean",".pdf"),height = 8,width = 16)



# Individal Income Rank ---------------------------------------------------

db %>% filter(gender=="pooled",percentile=="mean") %>%  plot_points(.,x="kir",xlab=expression(paste(Delta,"Individual Percentile Rank (White-Minority)")))
ggsave(paste0("views/","kir_pooled_mean",".pdf"),height = 8,width = 16)

db %>% filter(gender=="pooled",percentile=="p25") %>%  plot_points(.,x="kir",xlab=expression(paste(Delta,"Individual Percentile Rank (White-Minority)")))
ggsave(paste0("views/","kir_pooled_p25",".pdf"),height = 8,width = 16)

db %>% filter(gender=="pooled",percentile=="p75") %>%  plot_points(.,x="kir",xlab=expression(paste(Delta,"Individual Percentile Rank (White-Minority)")))
ggsave(paste0("views/","kir_pooled_p75",".pdf"),height = 8,width = 16)



# Marrriage Rates Income Rank ---------------------------------------------------

db %>% filter(gender=="pooled",percentile=="mean") %>%  plot_points(.,x="married",xlab=expression(paste(Delta,"Marriage Rates Percentile Rank (White-Minority)")))
ggsave(paste0("views/","married_pooled_mean",".pdf"),height = 8,width = 16)

db %>% filter(gender=="pooled",percentile=="p25") %>%  plot_points(.,x="married",xlab=expression(paste(Delta,"Marriage Percentile Rank (White-Minority)")))
ggsave(paste0("views/","married_pooled_p25",".pdf"),height = 8,width = 16)

db %>% filter(gender=="pooled",percentile=="p75") %>%  plot_points(.,x="married",xlab=expression(paste(Delta,"Marriage Percentile Rank (White-Minority)")))
ggsave(paste0("views/","married_pooled_p75",".pdf"),height = 8,width = 16)


# Mean Income Rank ---------------------------------------------------
db %>% filter(gender=="pooled",percentile=="mean") %>%  plot_points(.,x="kfr",xlab=expression(paste(Delta,"Mean Percentile Rank (White-Minority)")))
ggsave(paste0("views/","kfr_pooled_mean",".pdf"),height = 8,width = 16)

db %>% filter(gender=="pooled",percentile=="p25") %>%  plot_points(.,x="kfr",xlab=expression(paste(Delta,"Mean Percentile Rank (White-Minority)")))
ggsave(paste0("views/","kfr_pooled_p25",".pdf"),height = 8,width = 16)

db %>% filter(gender=="pooled",percentile=="p75") %>%  plot_points(.,x="kfr",xlab=expression(paste(Delta,"Mean Percentile Rank (White-Minority)")))
ggsave(paste0("views/","kfr_pooled_p75",".pdf"),height = 8,width = 16)



# Wage ------------------------------------------------------------------
db %>% filter(gender=="pooled",percentile=="mean") %>%  plot_points(.,x="wgflxrk",xlab=expression(paste(Delta,"Wage Rank (White-Minority)")))
ggsave(paste0("views/","wage_pooled_mean",".pdf"),height = 8,width = 16)

db %>% filter(gender=="pooled",percentile=="p25") %>%  plot_points(.,x="wgflxrk",xlab=expression(paste(Delta,"Wage Rank (White-Minority)")))
ggsave(paste0("views/","wage_pooled_p25",".pdf"),height = 8,width = 16)

db %>% filter(gender=="pooled",percentile=="p75") %>%  plot_points(.,x="wgflxrk",xlab=expression(paste(Delta,"Wage Rank (White-Minority)")))
ggsave(paste0("views/","wage_pooled_p75",".pdf"),height = 8,width = 16)


db %>% filter(gender=="male",percentile=="p25") %>%  plot_points(.,x="wgflxrk",xlab=expression(paste(Delta,"Wage Rank (White-Minority)")))
ggsave(paste0("views/","wage_male_p25",".pdf"),height = 8,width = 16)

db %>% filter(gender=="male",percentile=="p75") %>%  plot_points(.,x="wgflxrk",xlab=expression(paste(Delta,"Wage Rank (White-Minority)")))
ggsave(paste0("views/","wage_male_p75",".pdf"),height = 8,width = 16)


db %>% filter(gender=="male",percentile=="mean") %>%  plot_points(.,x="wgflxrk",xlab=expression(paste(Delta,"Wage Rank (White-Minority)")))
ggsave(paste0("views/","wage_male_mean",".pdf"),height = 8,width = 16)






# Hours ------------------------------------------------------------------
db %>% filter(gender=="pooled",percentile=="mean") %>%  plot_points(.,x="hourswk",xlab=expression(paste(Delta,"Hours Worked (White-Minority)")))
ggsave(paste0("views/","hours_pooled_mean",".pdf"),height = 8,width = 16)

db %>% filter(gender=="pooled",percentile=="p25") %>%  plot_points(.,x="hourswk",xlab=expression(paste(Delta,"Hours Worked  (White-Minority)")))
ggsave(paste0("views/","hours_pooled_p25",".pdf"),height = 8,width = 16)

db %>% filter(gender=="pooled",percentile=="p75") %>%  plot_points(.,x="hourswk",xlab=expression(paste(Delta,"Hours Worked  (White-Minority)")))
ggsave(paste0("views/","hours_pooled_p75",".pdf"),height = 8,width = 16)




db %>% filter(gender=="male",percentile=="p25") %>%  plot_points(.,x="hourswk",xlab=expression(paste(Delta,"Hours Worked  (White-Minority)")))
ggsave(paste0("views/","hours_male_p25",".pdf"),height = 8,width = 16)

db %>% filter(gender=="male",percentile=="p75") %>%  plot_points(.,x="hourswk",xlab=expression(paste(Delta,"Hours Worked  (White-Minority)")))
ggsave(paste0("views/","hours_male_p75",".pdf"),height = 8,width = 16)


db %>% filter(gender=="male",percentile=="mean") %>%  plot_points(.,x="hourswk",xlab=expression(paste(Delta,"Hours Worked  (White-Minority)")))
ggsave(paste0("views/","hours_male_mean",".pdf"),height = 8,width = 16)




# Employment Rates ------------------------------------------------------------------
db %>% filter(gender=="pooled",percentile=="mean") %>%  plot_points(.,x="working",xlab=expression(paste(Delta,"Employment Rates (White-Minority)")))
ggsave(paste0("views/","working_pooled_mean",".pdf"),height = 8,width = 16)

db %>% filter(gender=="pooled",percentile=="p25") %>%  plot_points(.,x="working",xlab=expression(paste(Delta,"Employment Rates  (White-Minority)")))
ggsave(paste0("views/","working_pooled_p25",".pdf"),height = 8,width = 16)

db %>% filter(gender=="pooled",percentile=="p75") %>%  plot_points(.,x="working",xlab=expression(paste(Delta,"Employment Rates  (White-Minority)")))
ggsave(paste0("views/","working_pooled_p75",".pdf"),height = 8,width = 16)


db %>% filter(gender=="male",percentile=="p25") %>%  plot_points(.,x="working",xlab=expression(paste(Delta,"Employment Rates  (White-Minority)")))
ggsave(paste0("views/","working_male_p25",".pdf"),height = 8,width = 16)


db %>% filter(gender=="male",percentile=="p75") %>%  plot_points(.,x="working",xlab=expression(paste(Delta,"Employment Rates  (White-Minority)")))
ggsave(paste0("views/","working_male_p75",".pdf"),height = 8,width = 16)


db %>% filter(gender=="male",percentile=="mean") %>%  plot_points(.,x="working",xlab=expression(paste(Delta,"Employment Rates  (White-Minority)")))
ggsave(paste0("views/","working_male_mean",".pdf"),height = 8,width = 16)

