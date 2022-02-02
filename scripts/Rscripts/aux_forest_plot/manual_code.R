#white share
p_w_sh_below_afam<-ggplot_pers(w_sh_below_afam,color_fondo="gray95",color_lineas="white",posicion="left") 
p_w_sh_below_hisp<-ggplot_pers(w_sh_below_afam,color_fondo="gray95",color_lineas="white",posicion="right")
p_w_sh_above_afam<-ggplot_pers(w_sh_above_afam,labx="") 
p_w_sh_above_hisp<-ggplot_pers(w_sh_above_hisp,posicion="right",labx="")



#black share
p_bk_sh_below_afam<-ggplot_pers(bk_sh_below_afam,color_fondo="gray95",color_lineas="white",posicion="left") 
p_bk_sh_below_hisp<-ggplot_pers(bk_sh_below_afam,color_fondo="gray95",color_lineas="white",posicion="right")
p_bk_sh_above_afam<-ggplot_pers(bk_sh_above_afam)
p_bk_sh_above_hisp<-ggplot_pers(bk_sh_above_hisp,posicion="right")

#hispanic share
p_hisp_sh_below_afam<-ggplot_pers(hisp_sh_below_afam,color_fondo="gray95",color_lineas="white",posicion="left") 
p_hisp_sh_below_hisp<-ggplot_pers(hisp_sh_below_afam,color_fondo="gray95",color_lineas="white",posicion="right")
p_hisp_sh_above_afam<-ggplot_pers(hisp_sh_above_afam) 
p_hisp_sh_above_hisp<-ggplot_pers(hisp_sh_above_hisp,posicion="right")


#(4) above/below median income, 
p_medinc_below_afam<-ggplot_pers(medinc_below_afam,color_fondo="gray95",color_lineas="white",posicion="left") 
p_medinc_below_hisp<-ggplot_pers(medinc_below_hisp,color_fondo="gray95",color_lineas="white",posicion="right")
p_medinc_above_afam<-ggplot_pers(medinc_above_afam) 
p_medinc_above_hisp<-ggplot_pers(medinc_above_hisp,posicion="right")


#(5) above/below median rent/sqft,  
p_rent_sqf_below_afam<-ggplot_pers(rent_sqf_below_afam,color_fondo="gray95",color_lineas="white",posicion="left") 
p_rent_sqf_below_hisp<-ggplot_pers(rent_sqf_below_hisp,color_fondo="gray95",color_lineas="white",posicion="right")
p_rent_sqf_above_afam<-ggplot_pers(rent_sqf_above_afam) 
p_rent_sqf_above_hisp<-ggplot_pers(rent_sqf_above_hisp,posicion="right")



#Above/below median black dissimilarity index, 
p_diss_bk_w_below_afam<-ggplot_pers(diss_bk_w_below_afam,color_fondo="gray95",color_lineas="white",posicion="left") 
p_diss_bk_w_below_hisp<-ggplot_pers(diss_bk_w_below_hisp,color_fondo="gray95",color_lineas="white",posicion="right")
p_diss_bk_w_above_afam<-ggplot_pers(rent_sqf_above_afam) 
p_diss_bk_w_above_hisp<-ggplot_pers(rent_sqf_above_hisp,posicion="right")



#Above/below median black hisp index, 
#(12) above/below median black intergenerational income gap
#(12) above/below median hisp intergenerational income gap


#

g_dta_afam         <- ggplotGrob(p_dta_afam)[-(4:6),]
g_dta_hisp         <- ggplotGrob(p_dta_hisp)[-(4:6),]
#white share
g_w_sh_below_afam  <- ggplotGrob(p_w_sh_below_afam)[-(1:2),]
g_w_sh_below_hisp  <- ggplotGrob(p_w_sh_below_hisp)[-(1:2),]
g_w_sh_above_afam  <- ggplotGrob(p_w_sh_above_afam)[-(4:6),]
g_w_sh_above_hisp  <- ggplotGrob(p_w_sh_above_hisp)[-(4:6),]
#black share
g_bk_sh_below_afam <- ggplotGrob(p_bk_sh_below_afam)[-(1:2),]
g_bk_sh_below_hisp <- ggplotGrob(p_bk_sh_below_hisp)[-(1:2),]
g_bk_sh_above_afam <- ggplotGrob(p_bk_sh_above_afam)[-(4:6),]
g_bk_sh_above_hisp <- ggplotGrob(p_bk_sh_above_hisp)[-(4:6),]

#hispanic share
g_hisp_sh_below_afam  <- ggplotGrob(p_hisp_sh_below_afam)[-(1:2),]
g_hisp_sh_below_hisp  <- ggplotGrob(p_hisp_sh_below_hisp)[-(1:2),]
g_hisp_sh_above_afam  <- ggplotGrob(p_hisp_sh_above_afam)[-(4:6),]
g_hisp_sh_above_hisp  <- ggplotGrob(p_hisp_sh_above_hisp)[-(4:6),]


#(4) above/below median income, 
g_medinc_below_afam  <- ggplotGrob(p_medinc_below_afam)[-(1:2),]
g_medinc_below_hisp  <- ggplotGrob(p_medinc_below_hisp)[-(1:2),]
g_medinc_above_afam  <- ggplotGrob(p_medinc_above_afam)[-(4:6),]
g_medinc_above_hisp  <- ggplotGrob(p_medinc_above_hisp)[-(4:6),]


#(5) above/below median rent/sqft,  
g_rent_sqf_below_afam  <- ggplotGrob(p_rent_sqf_below_afam)[-(1:2),]
g_rent_sqf_below_hisp  <- ggplotGrob(p_rent_sqf_below_hisp)[-(1:2),]
g_rent_sqf_above_afam  <- ggplotGrob(p_rent_sqf_above_afam)[-(4:6),]
g_rent_sqf_above_hisp  <- ggplotGrob(p_rent_sqf_above_hisp)[-(4:6),]


#(6) downtown/suburbs, 
g_downtown_afam  <- ggplotGrob(p_downtown_afam)[-(1:2),]
g_downtown_hisp  <- ggplotGrob(p_downtown_hisp)[-(1:2),]
g_suburb_afam    <- ggplotGrob(p_suburb_afam)[-(4:6),]
g_suburb_hisp    <- ggplotGrob(p_suburb_hisp)[-(4:6),]


#(7) Midwest, (8) Northeast, (9) South, (10) West
g_midwest_afam      <- ggplotGrob(p_midwest_afam)[-(1:2),]
g_midwest_hisp      <- ggplotGrob(p_midwest_hisp)[-(1:2),]
g_northeast_afam    <- ggplotGrob(p_northeast_afam)[-(4:6),]
g_northeast_hisp    <- ggplotGrob(p_northeast_hisp)[-(4:6),]

g_south_afam   <- ggplotGrob(p_south_afam)[-(1:2),]
g_south_hisp   <- ggplotGrob(p_south_hisp)[-(1:2),]
g_west_afam    <- ggplotGrob(p_west_afam)[-(4:6),]
g_west_hisp    <- ggplotGrob(p_west_hisp)[-(4:6),]

