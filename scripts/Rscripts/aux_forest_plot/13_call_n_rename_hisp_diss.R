
diss_h_w_1<-read_dta("stores/matrices/temp/diss_h_w_1.dta")
colnames(diss_h_w_1)<-c("coef","lci", "uci", "pval")
diss_h_w_1$name<-c("AfAm_choice", "Hispanic_choice", "AfAm", "Hispanic")
diss_h_w_1 <- diss_h_w_1 %>% mutate_at(vars("coef","lci", "uci"),exp)

be_ab<-"Below "
str_m<-"White - Hispanic/LatinX Dissimilarity Index x "
str_bk<-"African American x "
str_hisp<-"Hispanic/LatinX x "

str1<-paste0("Below ",str_m,str_bk)
str2<-paste0("Below ",str_m,str_hisp)
str3<-paste0("Above ",str_m,str_bk)
str4<-paste0("Above ",str_m,str_hisp)

diss_h_w_1 <- diss_h_w_1 %>% mutate(name=factor(name,levels=c("AfAm_choice", "AfAm", "Hispanic_choice", "Hispanic"),
                                  labels= c(paste0(str1, "Response=1"), 
                                            paste0(str1, "Response=0"), 
                                            paste0(str2, "Response=1"), 
                                            paste0(str2, "Response=0")),
                                  ordered=TRUE)
                                  )



diss_h_w_2<-read_dta("stores/matrices/temp/diss_h_w_2.dta")
colnames(diss_h_w_2)<-c("coef","lci", "uci", "pval")
diss_h_w_2$name<-c("AfAm_choice", "Hispanic_choice", "AfAm", "Hispanic")
diss_h_w_2 <- diss_h_w_2 %>% mutate_at(vars("coef","lci", "uci"),exp)
diss_h_w_2 <- diss_h_w_2 %>% mutate(name=factor(name,levels=c("AfAm_choice", "AfAm", "Hispanic_choice", "Hispanic"),
                                                  labels= c(paste0(str3, "Response=1"), 
                                                            paste0(str3, "Response=0"), 
                                                            paste0(str4, "Response=1"), 
                                                            paste0(str4, "Response=0")),
                                                            ordered=TRUE)
                                                  )




diss_hisp_w_below_afam<- diss_h_w_1 %>% filter(grepl(str1,name)) %>% mutate(name=str_remove(name,str1),
                                                                name=ifelse(name=="Response=1","Response","No Response"))
diss_hisp_w_below_hisp<- diss_h_w_1 %>% filter(grepl(str2,name)) %>% mutate(name=str_remove(name,str2),
                                                                name=ifelse(name=="Response=1","Response","No Response"))


diss_hisp_w_above_afam<- diss_h_w_2 %>% filter(grepl(str3,name)) %>% mutate(name=str_remove(name,str3),
                                                                name=ifelse(name=="Response=1","Response","No Response"))
diss_hisp_w_above_hisp<- diss_h_w_2 %>% filter(grepl(str4,name)) %>% mutate(name=str_remove(name,str4),
                                                                name=ifelse(name=="Response=1","Response","No Response"))


