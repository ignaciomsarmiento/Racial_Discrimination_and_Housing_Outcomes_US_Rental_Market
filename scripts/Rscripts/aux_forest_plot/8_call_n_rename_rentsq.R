
rent_sqf_1<-read_dta("stores/matrices/temp/rent_sqf_1.dta")
colnames(rent_sqf_1)<-c("coef","lci", "uci", "pval")
rent_sqf_1$name<-c("AfAm_choice", "Hispanic_choice", "AfAm", "Hispanic")
rent_sqf_1 <- rent_sqf_1 %>% mutate_at(vars("coef","lci", "uci"),exp)
rent_sqf_1 <- rent_sqf_1 %>% mutate(name=factor(name,levels=c("AfAm_choice", "AfAm", "Hispanic_choice", "Hispanic"),
                                  labels= c("Below Rent per Sqft. x African American x Response=1", 
                                            "Below Rent per Sqft. x African American x Response=0", 
                                            "Below Rent per Sqft. x Hispanic/LatinX x Response=1",
                                            "Below Rent per Sqft. x Hispanic/LatinX x Response=0"),
                                  ordered=TRUE)
                                  )



rent_sqf_2<-read_dta("stores/matrices/temp/rent_sqf_2.dta")
colnames(rent_sqf_2)<-c("coef","lci", "uci", "pval")
rent_sqf_2$name<-c("AfAm_choice", "Hispanic_choice", "AfAm", "Hispanic")
rent_sqf_2 <- rent_sqf_2 %>% mutate_at(vars("coef","lci", "uci"),exp)
rent_sqf_2 <- rent_sqf_2 %>% mutate(name=factor(name,levels=c("AfAm_choice", "AfAm", "Hispanic_choice", "Hispanic"),
                                                  labels= c("Above Rent per Sqft. x African American x Response=1", 
                                                            "Above Rent per Sqft. x African American x Response=0", 
                                                            "Above Rent per Sqft. x Hispanic/LatinX x Response=1",
                                                            "Above Rent per Sqft. x Hispanic/LatinX x Response=0"),
                                                  ordered=TRUE)
)




str1<-"Below Rent per Sqft. x African American x "
str2<-"Below Rent per Sqft. x Hispanic/LatinX x "
str3<-"Above Rent per Sqft. x African American x "
str4<-"Above Rent per Sqft. x Hispanic/LatinX x "

rent_sqf_below_afam<- rent_sqf_1 %>% filter(grepl(str1,name)) %>% mutate(name=str_remove(name,str1),
                                                                     name=ifelse(name=="Response=1","Response","No Response"))
rent_sqf_below_hisp<- rent_sqf_1 %>% filter(grepl(str2,name)) %>% mutate(name=str_remove(name,str2),
                                                                     name=ifelse(name=="Response=1","Response","No Response"))


rent_sqf_above_afam<- rent_sqf_2 %>% filter(grepl(str3,name)) %>% mutate(name=str_remove(name,str3),
                                                                     name=ifelse(name=="Response=1","Response","No Response"))
rent_sqf_above_hisp<- rent_sqf_2 %>% filter(grepl(str4,name)) %>% mutate(name=str_remove(name,str4),
                                                                     name=ifelse(name=="Response=1","Response","No Response"))
