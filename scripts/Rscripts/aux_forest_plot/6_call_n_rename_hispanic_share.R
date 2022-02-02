
hisp_sh_1<-read_dta("stores/matrices/temp/hisp_sh_1.dta")
colnames(hisp_sh_1)<-c("coef","lci", "uci", "pval")
hisp_sh_1$name<-c("AfAm_choice", "Hispanic_choice", "AfAm", "Hispanic")
hisp_sh_1 <- hisp_sh_1 %>% mutate_at(vars("coef","lci", "uci"),exp)
hisp_sh_1 <- hisp_sh_1 %>% mutate(name=factor(name,levels=c("AfAm_choice", "AfAm", "Hispanic_choice", "Hispanic"),
                                  labels= c("Below Median Hispanic/LatinX Population Share x African American x Response=1", 
                                            "Below Median Hispanic/LatinX Population Share x African American x Response=0", 
                                            "Below Median Hispanic/LatinX Population Share x Hispanic/LatinX x Response=1",
                                            "Below Median Hispanic/LatinX Population Share x Hispanic/LatinX x Response=0"),
                                  ordered=TRUE)
                                  )



hisp_sh_2<-read_dta("stores/matrices/temp/hisp_sh_2.dta")
colnames(hisp_sh_2)<-c("coef","lci", "uci", "pval")
hisp_sh_2$name<-c("AfAm_choice", "Hispanic_choice", "AfAm", "Hispanic")
hisp_sh_2 <- hisp_sh_2 %>% mutate_at(vars("coef","lci", "uci"),exp)
hisp_sh_2 <- hisp_sh_2 %>% mutate(name=factor(name,levels=c("AfAm_choice", "AfAm", "Hispanic_choice", "Hispanic"),
                                                  labels= c("Above Median Hispanic/LatinX Population Share x African American x Response=1", 
                                                            "Above Median Hispanic/LatinX Population Share x African American x Response=0", 
                                                            "Above Median Hispanic/LatinX Population Share x Hispanic/LatinX x Response=1",
                                                            "Above Median Hispanic/LatinX Population Share x Hispanic/LatinX x Response=0"),
                                                  ordered=TRUE)
)


str1<-"Below Median Hispanic/LatinX Population Share x African American x "
str2<-"Below Median Hispanic/LatinX Population Share x Hispanic/LatinX x "
str3<-"Above Median Hispanic/LatinX Population Share x African American x "
str4<-"Above Median Hispanic/LatinX Population Share x Hispanic/LatinX x "

hisp_sh_below_afam<- hisp_sh_1 %>% filter(grepl(str1,name)) %>% mutate(name=str_remove(name,str1),
                                                                name=ifelse(name=="Response=1","Response","No Response"))
hisp_sh_below_hisp<- hisp_sh_1 %>% filter(grepl(str2,name)) %>% mutate(name=str_remove(name,str2),
                                                                name=ifelse(name=="Response=1","Response","No Response"))


hisp_sh_above_afam<- hisp_sh_2 %>% filter(grepl(str3,name)) %>% mutate(name=str_remove(name,str3),
                                                                name=ifelse(name=="Response=1","Response","No Response"))
hisp_sh_above_hisp<- hisp_sh_2 %>% filter(grepl(str4,name)) %>% mutate(name=str_remove(name,str4),
                                                                name=ifelse(name=="Response=1","Response","No Response"))
