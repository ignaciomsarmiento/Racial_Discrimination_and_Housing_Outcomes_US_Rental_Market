##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("tidyverse","here","haven","ggpubr")
lapply(pkg, require, character.only=T)
rm(pkg)



dta<-read_dta(here("stores/matchedinquiries_opportunities_tract.dta"))


tract_oportunity<-distinct(dta,id_tract,kfr_white_pooled_mean,kfr_hisp_pooled_mean,kfr_black_pooled_mean)


dta_tract<- dta %>%  mutate(choice_white=ifelse(race==1,choice,NA),
                            choice_black=ifelse(race==3,choice,NA),
                            choice_hisp=ifelse(race==2,choice,NA),
                            )
#View(dta_tract %>% select(Address,id_tract,choice,race,choice_white,choice_black,choice_hisp))
dta_tract<- dta_tract %>% 
              group_by(id_tract,CBSA) %>% 
              summarize(choice_white=mean(choice_white,na.rm=TRUE),
                        choice_black=mean(choice_black,na.rm=TRUE),
                        choice_hisp=mean(choice_hisp,na.rm=TRUE),.groups="drop")



dta_tract<-left_join(dta_tract,tract_oportunity)

tracts<-dta %>% group_by(id_tract) %>% tally()

dta_tract<-left_join(dta_tract,tracts)

#dta_tract<- dta_tract %>% filter(n>=12)

dta_tract<- dta_tract %>% mutate(diff_black=(choice_black-choice_white),
                                 diff_hisp=(choice_hisp-choice_white),
                                 RR_black=diff_black/choice_white,
                                 RR_hisp=diff_hisp/choice_white)



means_kfr_black<- dta_tract %>% 
              group_by(RR_black) %>% 
              summarize(kfr_white_pooled_mean=mean(kfr_white_pooled_mean,na.rm=TRUE),
                        kfr_black_pooled_mean=mean(kfr_black_pooled_mean,na.rm=TRUE),
                        .groups="drop")%>% 
              pivot_longer(.,cols=c(kfr_white_pooled_mean,kfr_black_pooled_mean),names_to="race_var") %>% 
              mutate(race=ifelse(race_var=="kfr_white_pooled_mean","white","black"))


means_kfr_hispanic<- dta_tract %>% 
  group_by(RR_hisp) %>% 
  summarize(kfr_white_pooled_mean=mean(kfr_white_pooled_mean,na.rm=TRUE),
            kfr_hisp_pooled_mean=mean(kfr_hisp_pooled_mean,na.rm=TRUE)) %>% 
  pivot_longer(.,cols=c(kfr_white_pooled_mean,kfr_hisp_pooled_mean),names_to="race_var")%>% 
  mutate(race=ifelse(race_var=="kfr_white_pooled_mean","white","hisp"))




ggplot(means_kfr_black,aes(x=RR_black,y=value,group=race,col=race))+
  geom_point() +
  xlab("Within Track Relative Responses") +
  ylab("Mean percentile rank") +
  geom_smooth(method = "lm",se=FALSE) +
  scale_fill_manual(values=c("#d8b365","#5ab4ac")) +
  scale_color_manual(values=c("#d8b365","#5ab4ac")) +
  ylim(c(.25,.72)) +
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
        plot.margin = unit(c(2,3.5,1,1), "lines")) 
ggsave(paste0("views/","track_rr_black_kfr",".pdf"),height = 10,width = 16)


ggplot(means_kfr_hispanic,aes(x=RR_hisp,y=value,group=race,col=race))+
  geom_point() +
  xlab("Within Track Relative Responses") +
  ylab("Mean percentile rank") +
  geom_smooth(method = "lm",se=FALSE) +
  scale_fill_manual(values=c("#d8b365","#5ab4ac")) +
  scale_color_manual(values=c("#d8b365","#5ab4ac")) +
  ylim(c(.25,.72)) +
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
        plot.margin = unit(c(2,3.5,1,1), "lines"))
ggsave(paste0("views/","track_rr_hisp_kfr",".pdf"),height = 10,width = 16)




# Differences -------------------------------------------------------------


means_kfr_black<- dta_tract %>% 
  group_by(diff_black) %>% 
  summarize(kfr_white_pooled_mean=mean(kfr_white_pooled_mean,na.rm=TRUE),
            kfr_black_pooled_mean=mean(kfr_black_pooled_mean,na.rm=TRUE),
            .groups="drop")%>% 
  pivot_longer(.,cols=c(kfr_white_pooled_mean,kfr_black_pooled_mean),names_to="race_var") %>% 
  mutate(race=ifelse(race_var=="kfr_white_pooled_mean","white","black"))


means_kfr_hispanic<- dta_tract %>% 
  group_by(diff_hisp) %>% 
  summarize(kfr_white_pooled_mean=mean(kfr_white_pooled_mean,na.rm=TRUE),
            kfr_hisp_pooled_mean=mean(kfr_hisp_pooled_mean,na.rm=TRUE)) %>% 
  pivot_longer(.,cols=c(kfr_white_pooled_mean,kfr_hisp_pooled_mean),names_to="race_var")%>% 
  mutate(race=ifelse(race_var=="kfr_white_pooled_mean","white","hisp"))




ggplot(means_kfr_black,aes(x=diff_black,y=value,group=race,col=race))+
  geom_point() +
  xlab("Within Track Av. Diferences in Responses") +
  ylab("Mean percentile rank") +
  geom_smooth(method = "lm",se=FALSE) +
  scale_fill_manual(values=c("#d8b365","#5ab4ac")) +
  scale_color_manual(values=c("#d8b365","#5ab4ac")) +
  theme_bw() +
  ylim(c(.25,.72)) +
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
        plot.margin = unit(c(2,3.5,1,1), "lines"))
ggsave(paste0("views/","track_diffr_black_kfr",".pdf"),height = 10,width = 16)


ggplot(means_kfr_hispanic,aes(x=diff_hisp,y=value,group=race,col=race))+
  geom_point() +
  xlab("Within Track Av. Diferences in Responses") +
  ylab("Mean percentile rank") +
  geom_smooth(method = "lm",se=FALSE) +
  scale_fill_manual(values=c("#d8b365","#5ab4ac")) +
  scale_color_manual(values=c("#d8b365","#5ab4ac")) +
  theme_bw() +
  ylim(c(.25,.72)) +
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
        plot.margin = unit(c(2,3.5,1,1), "lines"))
ggsave(paste0("views/","track_diffr_hisp_kfr",".pdf"),height = 10,width = 16)





# Gaps --------------------------------------------------------------------


dta_gap<- dta_tract %>% mutate(gap_black=kfr_white_pooled_mean-kfr_black_pooled_mean,
                                 gap_hisp=kfr_white_pooled_mean-kfr_hisp_pooled_mean)


dta_gap<- dta_gap %>% filter(!is.na(gap_black) )
dta_gap<- dta_gap %>% filter(!is.na(gap_hisp) )

#dta_gap<- dta_gap %>% select(id_tract,CBSA,diff_black,diff_hisp,gap_black,gap_hisp)
dta_diff<- dta_gap %>% 
          select(id_tract,CBSA,diff_black,diff_hisp) %>%
          pivot_longer(!(id_tract:CBSA),names_to="race",values_to="Diff") %>% 
          mutate(race=str_remove(race,"diff_"))

dta_RR<- dta_gap %>% 
  select(id_tract,CBSA,RR_black,RR_hisp) %>%
  pivot_longer(!(id_tract:CBSA),names_to="race",values_to="RR") %>% 
  mutate(race=str_remove(race,"RR_"))

dta_gap<- dta_gap %>% 
  select(id_tract,CBSA,gap_black,gap_hisp) %>%
  pivot_longer(!(id_tract:CBSA),names_to="race",values_to="gap") %>% 
  mutate(race=str_remove(race,"gap_"))

dta_gap<-left_join(dta_diff,dta_gap)
dta_gap<-left_join(dta_gap,dta_RR)



ggplot(dta_gap,aes(x=RR,y=gap,group=race,col=race)) +
  geom_point(aes(group=race,shape=race),size=4) +
  geom_smooth(aes(lty=race),method = "lm",size=.5,se = TRUE,level=.95) +
  scale_shape_manual(values=c(16,17)) +
  ylab(expression(paste(Delta,"Mean Percentile Rank (White-Minority)"))) + 
  xlab("Within Track Av. Relative Responses") +
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
        plot.margin = unit(c(2,3.5,1,1), "lines"))  +
   stat_regline_equation() 
ggsave(paste0("views/","track_RR_gap",".pdf"),height = 10,width = 16)



ggplot(dta_gap,aes(x=Diff,y=gap,group=race,col=race)) +
  geom_point(aes(group=race,shape=race),size=4) +
  geom_smooth(aes(lty=race),method = "lm",size=.5,se = TRUE,level=.95) +
  scale_shape_manual(values=c(16,17)) +
  ylab(expression(paste(Delta,"Mean Percentile Rank (White-Minority)"))) + 
  xlab("Within Track Av. Differences in Responses") +
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
        plot.margin = unit(c(2,3.5,1,1), "lines"))  + 
    stat_regline_equation() 
ggsave(paste0("views/","track_Diff_gap",".pdf"),height = 10,width = 16)
