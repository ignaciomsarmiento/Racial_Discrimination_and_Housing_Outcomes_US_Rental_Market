
w_rent_sh_1<-read_dta("stores/matrices/temp/w_rent_sh_1.dta")
colnames(w_rent_sh_1)<-c("coef","lci", "uci", "pval")
w_rent_sh_1$name<-c("AfAm_choice", "Hispanic_choice", "AfAm", "Hispanic")
w_rent_sh_1 <- w_rent_sh_1 %>% mutate_at(vars("coef","lci", "uci"),exp)
w_rent_sh_1 <- w_rent_sh_1 %>% mutate(name=factor(name,levels=c("AfAm_choice", "AfAm", "Hispanic_choice", "Hispanic"),
                                  labels= c("Below Median White Renter Share x African American x Response=1", 
                                            "Below Median White Renter Share x African American x Response=0", 
                                            "Below Median White Renter Share x Hispanic/LatinX x Response=1",
                                            "Below Median White Renter Share x Hispanic/LatinX x Response=0"),
                                  ordered=TRUE)
                                  )



w_rent_sh_2<-read_dta("stores/matrices/temp/w_rent_sh_2.dta")
colnames(w_rent_sh_2)<-c("coef","lci", "uci", "pval")
w_rent_sh_2$name<-c("AfAm_choice", "Hispanic_choice", "AfAm", "Hispanic")
w_rent_sh_2 <- w_rent_sh_2 %>% mutate_at(vars("coef","lci", "uci"),exp)
w_rent_sh_2 <- w_rent_sh_2 %>% mutate(name=factor(name,levels=c("AfAm_choice", "AfAm", "Hispanic_choice", "Hispanic"),
                                                  labels= c("Above Median White Renter Share x African American x Response=1", 
                                                            "Above Median White Renter Share x African American x Response=0", 
                                                            "Above Median White Renter Share x Hispanic/LatinX x Response=1",
                                                            "Above Median White Renter Share x Hispanic/LatinX x Response=0"),
                                                  ordered=TRUE)
)




w_rent_sh_below_afam<- w_rent_sh_1 %>% filter(grepl("African",name)) %>% mutate(name=str_remove(name,"Below Median White Renter Share x African American x "),
                                                                                name=ifelse(name=="Response=1","Response","No Response"))
w_rent_sh_below_hisp<- w_rent_sh_1 %>% filter(grepl("Hisp",name)) %>% mutate(name=str_remove(name,"Below Median White Renter Share x Hispanic/LatinX x "),
                                                                             name=ifelse(name=="Response=1","Response","No Response"))


w_rent_sh_above_afam<- w_rent_sh_2 %>% filter(grepl("African",name)) %>% mutate(name=str_remove(name,"Above Median White Renter Share x African American x "),
                                                                                name=ifelse(name=="Response=1","Response","No Response"))
w_rent_sh_above_hisp<- w_rent_sh_2 %>% filter(grepl("Hisp",name)) %>% mutate(name=str_remove(name,"Above Median White Renter Share x Hispanic/LatinX x "),
                                                                             name=ifelse(name=="Response=1","Response","No Response"))
