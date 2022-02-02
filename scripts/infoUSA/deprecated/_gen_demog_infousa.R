##########################################################
# author: Ignacio Sarmiento-Barbieri

# generates proximities

##########################################################


gen_demog_infousa<-function(dta) {


# Generate Demog. variables -----------------------------------------------

  Black_codes<-c("B5", "Q6", "Q7", "Q8", "Q9","AO","A8","BJ","BA","BW","BF","BI","CM","C3","CF","TD","KM","CG",
                 "DJ","GQ","ET","GA","GM","GH","GW","GN","H2","CI","KE","LS", "LR","MG","MW","ML","MR","MZ","NA",
                 "NE","NG","RW","SN","SC","SL","SO","ZA","SD","S9","SZ","TZ","TG","UG","X5","CD","ZM","ZW","Z8")
  Hispanic_codes<-c("B3","H5","PT")
  Western_European_codes<-c("AT", "BE", "NL", "E5", "FR", "DE",  "IE", "K8","LI", "LU", "IM", "S3", "CH", "TR", "W4")
  Mediterranean_codes<-c( "CY", "GR","IT", "MT")
  Eastern_European_codes<-c("AL", "BA", "BY", "HR", "CZ", "EE", "HU", "LV", "MK", "MD", "PL", "RO", "RU", "CS", "SK", "SI")
  Scandinavian_codes<-c("NO", "IS", "FI", "DK")

  White_codes<-c(Western_European_codes,Mediterranean_codes,Eastern_European_codes,Scandinavian_codes)

  dta <- dta %>% mutate(renter=ifelse(owner_renter_status%in%c(0,1,2,3),1,0),
                        owner=ifelse(owner_renter_status%in%c(6,7,8,9),1,0),
                        married=ifelse(marital_status%in%c(5,6,7,8,9),1,0),
                        black=ifelse(Ethnicity_Code_1%in%Black_codes,1,0),
                        hispanic=ifelse(Ethnicity_Code_1%in%Hispanic_codes,1,0),
                        white=ifelse(Ethnicity_Code_1%in%White_codes,1,0),
                        unknown=ifelse(Ethnicity_Code_1%in%c(00),1,0),
                        ethnicity=ifelse(Ethnicity_Code_1%in%Black_codes,"Black",
                                         ifelse(Ethnicity_Code_1%in%Hispanic_codes,"Hispanic",
                                                ifelse(Ethnicity_Code_1%in%Western_European_codes,"Western_European",
                                                       ifelse(Ethnicity_Code_1%in%Mediterranean_codes,"Mediterranean",
                                                              ifelse(Ethnicity_Code_1%in%Eastern_European_codes,"Eastern_European",
                                                                     ifelse(Ethnicity_Code_1%in%Scandinavian_codes,"Scandinavian",
                                                                      ifelse(Ethnicity_Code_1%in%c(00),"Unknown",
                                                       "Other"))))))),
                        race=ifelse(Ethnicity_Code_1%in%Black_codes,"Black",
                                    ifelse(Ethnicity_Code_1%in%Hispanic_codes,"Hispanic",
                                           ifelse(Ethnicity_Code_1%in%White_codes,"White","Other"))),
                        HH_age=ifelse(head_hh_age_code=="A",22.5,
                                               ifelse(head_hh_age_code=="B",27,
                                                      ifelse(head_hh_age_code=="C",32,
                                                             ifelse(head_hh_age_code=="D",37,
                                                                    ifelse(head_hh_age_code=="E",42,
                                                                           ifelse(head_hh_age_code=="F",47,
                                                                                  ifelse(head_hh_age_code=="G",52,
                                                                                          ifelse(head_hh_age_code=="H",57,
                                                                                                ifelse(head_hh_age_code=="I",62,
                                                                                                      ifelse(head_hh_age_code=="J",67,
                                                                                                              ifelse(head_hh_age_code=="K",67,
                                                                                                                    ifelse(head_hh_age_code=="L",72,77)))))))))))))


  return(dta)
}
