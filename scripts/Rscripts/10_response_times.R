##########################################################
# author: Ignacio Sarmiento-Barbieri
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("haven","dplyr","lubridate","ggplot2")
lapply(pkg, require, character.only=T)
rm(pkg)


#WD
setwd("~/Dropbox/Research/cbsa_discrimination/")

dta<-read_dta("stores/matchedinquiries.dta")

#inquiry reveived
dta$time_response_received<-as.character(dta$time_response_received)
dta$response_time<-str_remove(dta$time_response_received,".[0-9][0-9][0-9]-[0-9][0-9]:[0-9][0-9]")
dta$response_time<-str_replace(dta$response_time,"T"," ")
dta$response_time<-ymd_hms(dta$response_time)

#Inquiry sent out
dta$time_inquiry_sent_out<-as.character(dta$time_inquiry_sent_out)
dta$time_inquiry_sent_out<-str_remove(dta$time_inquiry_sent_out,".[0-9][0-9][0-9]-[0-9][0-9]:[0-9][0-9]")
dta$time_inquiry_sent_out<-str_replace(dta$time_inquiry_sent_out,"T"," ")
dta$time_inquiry_sent_out<-ymd_hms(dta$time_inquiry_sent_out)

dta$response_days<-dta$response_time-dta$time_inquiry_sent_out
dta$response_days<-as.numeric(dta$response_days,units="days")


colors <- tibble::deframe(ggthemes::ggthemes_data[["fivethirtyeight"]])
base_size = 4
base_family = "sans"
dta4<-dta %>% filter(response_days>=0,response_days<8)
dta4 <- dta4 %>% mutate(hours=ifelse(response_days<=0.3,1,0),
                        day=ifelse(response_days<=1,1,0),
                        five_days=ifelse(response_days<=5,1,0))

prop.table(table(dta4$hours))
prop.table(table(dta4$day))
prop.table(table(dta4$five_days))

ggplot(dta4,aes(x=response_days)) +
  geom_histogram( colour="black", fill=colors["Medium Gray"], binwidth = .3) + #
  scale_x_continuous("Days between inquiry and response",breaks=c(0,1,2,3,4,5,6,7,8)) +
  ylab("Counts") +
  theme_bw() +
  theme_fivethirtyeight() + scale_color_fivethirtyeight("cyl") +
  theme(legend.title= element_blank() ,
        legend.position="none",
        legend.justification=c(1,1),
        legend.direction="vertical",
        legend.box="horizontal",
        legend.box.just = c("top"),
        legend.background = element_rect(fill='transparent'),
        axis.text.x =element_text( angle=0),
        rect = element_rect(colour = "transparent", fill = "white"),
        axis.title = element_text(), plot.margin = unit(c(2,2,1,1), "lines"))
ggsave(filename="views/response_days.png", height = 4, width = 6)
