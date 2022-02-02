
medinc_1<-read_dta("stores/matrices/temp/medinc_1.dta")
colnames(medinc_1)<-c("coef","lci", "uci", "pval")
medinc_1$name<-c("AfAm_choice", "Hispanic_choice", "AfAm", "Hispanic")
medinc_1 <- medinc_1 %>% mutate_at(vars("coef","lci", "uci"),exp)
medinc_1 <- medinc_1 %>% mutate(name=factor(name,levels=c("AfAm_choice", "AfAm", "Hispanic_choice", "Hispanic"),
                                  labels= c("Below Median Income x African American x Response=1", 
                                            "Below Median Income x African American x Response=0", 
                                            "Below Median Income x Hispanic/LatinX x Response=1",
                                            "Below Median Income x Hispanic/LatinX x Response=0"),
                                  ordered=TRUE)
                                  )



medinc_2<-read_dta("stores/matrices/temp/medinc_2.dta")
colnames(medinc_2)<-c("coef","lci", "uci", "pval")
medinc_2$name<-c("AfAm_choice", "Hispanic_choice", "AfAm", "Hispanic")
medinc_2 <- medinc_2 %>% mutate_at(vars("coef","lci", "uci"),exp)
medinc_2 <- medinc_2 %>% mutate(name=factor(name,levels=c("AfAm_choice", "AfAm", "Hispanic_choice", "Hispanic"),
                                                  labels= c("Above Median Income x African American x Response=1", 
                                                            "Above Median Income x African American x Response=0", 
                                                            "Above Median Income x Hispanic/LatinX x Response=1",
                                                            "Above Median Income x Hispanic/LatinX x Response=0"),
                                                  ordered=TRUE)
)



str1<-"Below Median Income x African American x "
str2<-"Below Median Income x Hispanic/LatinX x "
str3<-"Above Median Income x African American x "
str4<-"Above Median Income x Hispanic/LatinX x "

medinc_below_afam<- medinc_1 %>% filter(grepl(str1,name)) %>% mutate(name=str_remove(name,str1),
                                                                    name=ifelse(name=="Response=1","Response","No Response"))
medinc_below_hisp<- medinc_1 %>% filter(grepl(str2,name)) %>% mutate(name=str_remove(name,str2),
                                                                    name=ifelse(name=="Response=1","Response","No Response"))


medinc_above_afam<- medinc_2 %>% filter(grepl(str3,name)) %>% mutate(name=str_remove(name,str3),
                                                                    name=ifelse(name=="Response=1","Response","No Response"))
medinc_above_hisp<- medinc_2 %>% filter(grepl(str4,name)) %>% mutate(name=str_remove(name,str4),
                                                                    name=ifelse(name=="Response=1","Response","No Response"))
