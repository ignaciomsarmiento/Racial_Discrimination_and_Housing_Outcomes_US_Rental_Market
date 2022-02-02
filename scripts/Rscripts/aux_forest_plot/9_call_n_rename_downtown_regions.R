
Downtown<-read_dta("stores/matrices/temp/Downtown.dta")
colnames(Downtown)<-c("coef","lci", "uci", "pval")
Downtown$name<-c("AfAm_choice", "Hispanic_choice", "AfAm", "Hispanic")
Downtown <- Downtown %>% mutate_at(vars("coef","lci", "uci"),exp)
Downtown <- Downtown %>% mutate(name=factor(name,levels=c("AfAm_choice", "AfAm", "Hispanic_choice", "Hispanic"),
                                  labels= c("Downtown x African American x Response=1", 
                                            "Downtown x African American x Response=0", 
                                            "Downtown x Hispanic/LatinX x Response=1",
                                            "Downtown x Hispanic/LatinX x Response=0"),
                                  ordered=TRUE)
                                  )



Suburb<-read_dta("stores/matrices/temp/Suburb.dta")
colnames(Suburb)<-c("coef","lci", "uci", "pval")
Suburb$name<-c("AfAm_choice", "Hispanic_choice", "AfAm", "Hispanic")
Suburb <- Suburb %>% mutate_at(vars("coef","lci", "uci"),exp)
Suburb <- Suburb %>% mutate(name=factor(name,levels=c("AfAm_choice", "AfAm", "Hispanic_choice", "Hispanic"),
                                  labels= c("Suburb x African American x Response=1", 
                                            "Suburb x African American x Response=0", 
                                            "Suburb x Hispanic/LatinX x Response=1",
                                            "Suburb x Hispanic/LatinX x Response=0"),
                                  ordered=TRUE)
                                  )


str1<-"Downtown x African American x"
str2<-"Downtown x Hispanic/LatinX x "
str3<-"Suburb x African American x "
str4<-"Suburb x Hispanic/LatinX x "


downtown_afam<- Downtown %>% filter(grepl(str1,name)) %>% mutate(name=str_remove(name,str1),
                                                                       name=ifelse(name=="Response=1","Response","No Response"))
downtown_hisp<- Downtown %>% filter(grepl(str2,name)) %>% mutate(name=str_remove(name,str2),
                                                                       name=ifelse(name=="Response=1","Response","No Response"))


suburb_afam<- Suburb %>% filter(grepl(str3,name)) %>% mutate(name=str_remove(name,str3),
                                                                 name=ifelse(name=="Response=1","Response","No Response"))
suburb_hisp<- Suburb %>% filter(grepl(str4,name)) %>% mutate(name=str_remove(name,str4),
                                                                 name=ifelse(name=="Response=1","Response","No Response"))





Midwest<-read_dta("stores/matrices/temp/Midwest.dta")
colnames(Midwest)<-c("coef","lci", "uci", "pval")
Midwest$name<-c("AfAm_choice", "Hispanic_choice", "AfAm", "Hispanic")
Midwest <- Midwest %>% mutate_at(vars("coef","lci", "uci"),exp)
Midwest <- Midwest %>% mutate(name=factor(name,levels=c("AfAm_choice", "AfAm", "Hispanic_choice", "Hispanic"),
                                  labels= c("Midwest x African American x Response=1", 
                                            "Midwest x African American x Response=0", 
                                            "Midwest x Hispanic/LatinX x Response=1",
                                            "Midwest x Hispanic/LatinX x Response=0"),
                                  ordered=TRUE)
                                  )


str1<-"Midwest x African American x "
str2<-"Midwest x Hispanic/LatinX x "
midwest_afam<- Midwest %>% filter(grepl(str1,name)) %>% mutate(name=str_remove(name,str1),
                                                                 name=ifelse(name=="Response=1","Response","No Response"))
midwest_hisp<- Midwest %>% filter(grepl(str2,name)) %>% mutate(name=str_remove(name,str2),
                                                                 name=ifelse(name=="Response=1","Response","No Response"))



Northeast<-read_dta("stores/matrices/temp/Northeast.dta")
colnames(Northeast)<-c("coef","lci", "uci", "pval")
Northeast$name<-c("AfAm_choice", "Hispanic_choice", "AfAm", "Hispanic")
Northeast <- Northeast %>% mutate_at(vars("coef","lci", "uci"),exp)
Northeast <- Northeast %>% mutate(name=factor(name,levels=c("AfAm_choice", "AfAm", "Hispanic_choice", "Hispanic"),
                                  labels= c("Northeast x African American x Response=1", 
                                            "Northeast x African American x Response=0", 
                                            "Northeast x Hispanic/LatinX x Response=1",
                                            "Northeast x Hispanic/LatinX x Response=0"),
                                  ordered=TRUE)
                                  )


str1<-"Northeast x African American x "
str2<-"Northeast x Hispanic/LatinX x "
northeast_afam<- Northeast %>% filter(grepl(str1,name)) %>% mutate(name=str_remove(name,str1),
                                                               name=ifelse(name=="Response=1","Response","No Response"))
northeast_hisp<- Northeast %>% filter(grepl(str2,name)) %>% mutate(name=str_remove(name,str2),
                                                               name=ifelse(name=="Response=1","Response","No Response"))

South<-read_dta("stores/matrices/temp/South.dta")
colnames(South)<-c("coef","lci", "uci", "pval")
South$name<-c("AfAm_choice", "Hispanic_choice", "AfAm", "Hispanic")
South <- South %>% mutate_at(vars("coef","lci", "uci"),exp)
South <- South %>% mutate(name=factor(name,levels=c("AfAm_choice", "AfAm", "Hispanic_choice", "Hispanic"),
                                  labels= c("South x African American x Response=1", 
                                            "South x African American x Response=0", 
                                            "South x Hispanic/LatinX x Response=1",
                                            "South x Hispanic/LatinX x Response=0"),
                                  ordered=TRUE)
                                  ) 


str1<-"South x African American x "
str2<-"South x Hispanic/LatinX x "
south_afam<- South %>% filter(grepl(str1,name)) %>% mutate(name=str_remove(name,str1),
                                                                   name=ifelse(name=="Response=1","Response","No Response"))
south_hisp<- South %>% filter(grepl(str2,name)) %>% mutate(name=str_remove(name,str2),
                                                                   name=ifelse(name=="Response=1","Response","No Response"))


West<-read_dta("stores/matrices/temp/West.dta")
colnames(West)<-c("coef","lci", "uci", "pval")
West$name<-c("AfAm_choice", "Hispanic_choice", "AfAm", "Hispanic")
West <- West %>% mutate_at(vars("coef","lci", "uci"),exp)
West <- West %>% mutate(name=factor(name,levels=c("AfAm_choice", "AfAm", "Hispanic_choice", "Hispanic"),
                                  labels= c("West x African American x Response=1", 
                                            "West x African American x Response=0", 
                                            "West x Hispanic/LatinX x Response=1",
                                            "West x Hispanic/LatinX x Response=0"),
                                  ordered=TRUE)
                                  ) 

str1<-"West x African American x "
str2<-"West x Hispanic/LatinX x "

west_afam<- West %>% filter(grepl(str1,name)) %>% mutate(name=str_remove(name,str1),
                                                           name=ifelse(name=="Response=1","Response","No Response"))
west_hisp<- West %>% filter(grepl(str2,name)) %>% mutate(name=str_remove(name,str2),
                                                           name=ifelse(name=="Response=1","Response","No Response"))



