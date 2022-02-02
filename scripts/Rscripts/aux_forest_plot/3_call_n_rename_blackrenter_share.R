bk_rent_sh_1<-read_dta("stores/matrices/temp/bk_rent_sh_1.dta")
colnames(bk_rent_sh_1)<-c("coef","lci", "uci", "pval")
bk_rent_sh_1$name<-c("AfAm_choice", "Hispanic_choice", "AfAm", "Hispanic")
bk_rent_sh_1 <- bk_rent_sh_1 %>% mutate_at(vars("coef","lci", "uci"),exp)
bk_rent_sh_1 <- bk_rent_sh_1 %>% mutate(name=factor(name,levels=c("AfAm_choice", "AfAm", "Hispanic_choice", "Hispanic"),
                                  labels= c("Below Median African American Renter Share x African American x Response=1", 
                                            "Below Median African American Renter Share x African American x Response=0", 
                                            "Below Median African American Renter Share x Hispanic/LatinX x Response=1",
                                            "Below Median African American Renter Share x Hispanic/LatinX x Response=0"),
                                  ordered=TRUE)
                                  )



bk_rent_sh_2<-read_dta("stores/matrices/temp/bk_rent_sh_2.dta")
colnames(bk_rent_sh_2)<-c("coef","lci", "uci", "pval")
bk_rent_sh_2$name<-c("AfAm_choice", "Hispanic_choice", "AfAm", "Hispanic")
bk_rent_sh_2 <- bk_rent_sh_2 %>% mutate_at(vars("coef","lci", "uci"),exp)
bk_rent_sh_2 <- bk_rent_sh_2 %>% mutate(name=factor(name,levels=c("AfAm_choice", "AfAm", "Hispanic_choice", "Hispanic"),
                                                  labels= c("Above Median African American Renter Share x African American x Response=1", 
                                                            "Above Median African American Renter Share x African American x Response=0", 
                                                            "Above Median African American Renter Share x Hispanic/LatinX x Response=1",
                                                            "Above Median African American Renter Share x Hispanic/LatinX x Response=0"),
                                                  ordered=TRUE)
)



str1<-"Below Median African American Renter Share x African American x "
str2<-"Below Median African American Renter Share x Hispanic/LatinX x "
str3<-"Above Median African American Renter Share x African American x "
str4<-"Above Median African American Renter Share x Hispanic/LatinX x "
bk_rent_below_afam<- bk_rent_sh_1 %>% filter(grepl(str1,name)) %>% mutate(name=str_remove(name,str1),
                                                                      name=ifelse(name=="Response=1","Response","No Response"))
bk_rent_below_hisp<- bk_rent_sh_1 %>% filter(grepl(str2,name)) %>% mutate(name=str_remove(name,str2),
                                                                   name=ifelse(name=="Response=1","Response","No Response"))


bk_rent_above_afam<- bk_rent_sh_2 %>% filter(grepl(str3,name)) %>% mutate(name=str_remove(name,str3),
                                                                      name=ifelse(name=="Response=1","Response","No Response"))
bk_rent_above_hisp<- bk_rent_sh_2 %>% filter(grepl(str4,name)) %>% mutate(name=str_remove(name,str4),
                                                                   name=ifelse(name=="Response=1","Response","No Response"))
