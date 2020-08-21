
# 1. Librerias ------------------------------------------------------------
library(tidyverse)
library(purrr)
library(dplyr)
library(stringi)
library(ggplot2)
library(sf)

# 2.- Directorio de trabajo -----------------------------------------------
setwd("C:/Mi unidad/BID/bid2020.github.io/demography")
data_pais=readRDS("data_pais.rds")
# 3.- BBDD cepal ----------------------------------------------------------
setwd("C:/Mi unidad/BID/bid2020.github.io/demography/data_cepal")

data=read.csv2("pcepal_data.csv", encoding = "UTF-8")%>%
        mutate(nom_pais=as.character(X.U.FEFF.nom_pais),
               PN18=round((N18_519*100/T18),2),
               PN19=round((N19_519*100/T19),2),
               PN20=round((N20_519*100/T20),2),
               PM18=round((M18*100/T18),2),
               PM19=round((M19*100/T19),2),
               PM20=round((M20*100/T20),2),
               PE18=round(((M18+NH18_519)*100/T18),2),
               PE19=round(((M19+NH19_519)*100/T19),2),
               PE20=round(((M20+NH20_519)*100/T20),2),
               nom_pais=str_to_lower(nom_pais),
               nom_pais=stri_trans_general(nom_pais,"Latin-ASCII"))%>%
        left_join(data_pais,by=c("nom_pais"="pais"))%>%
        mutate(geo=replace_na(geo,-1))%>%
        select(geo,nom_pais,PN18,PN19,PN20,PM18,PM19,PM20,PE18,PE19,PE20)%>%
        filter(geo!=-1)

rm(data_pais)

# 4. Save BBDD ------------------------------------------------------------
#setwd("C:/BID_GH/bbdd_edit/data_rds")
#saveRDS(data,"data_pobcepal.rds")
# 5. Graphic Mapping FALTA EDITAR-------------------------------------------------------

data_grap=data%>%
        select(names_pais,PN18,PN19,PN20,PM18,PM19,PM20,geo)%>%
        mutate(geo=replace_na(geo,1))%>%
        filter(geo!=1)

setwd("C:/Mi unidad/BID/bid2020.github.io/demography")
#saveRDS(data,"data_pobcepal.rds")

mapa=read_sf("mapa_amc.shp")%>%
        select(CNTR_ID)%>%
        mutate(geo=as.character(CNTR_ID))%>%
        select(geo)%>%
        left_join(data_grap,by=c("geo"))%>%
        mutate(names_pais=replace_na(names_pais,1))%>%
        filter(names_pais!=1)%>%
        mutate(PN20_C=cut(PN20,breaks = quantile(mapa$PN20)),
               PM20_C=cut(PM20,breaks = quantile(mapa$PM20)))

write_sf(mapa,"mapa_cepal.shp")

levels(mapa$PN20_C) <- c("16.6% a 20.7%","20.7% a 23.6%", 
                         "23.6% a 27.6%","27.6% a 32.7%")

ggplot(data = mapa%>%filter(!is.na(PN20_C)))+
        geom_sf(aes(fill=PN20_C))+
        scale_fill_binned()+
        labs(title="Población entre 5 y 19 años. 2020", fill="") +
        coord_sf(datum = NA)+
        theme_minimal()+
        theme(legend.position ="left")

        coord_sf(datum = NA)+
        scale_fill_viridis_c()+
        theme_classic()
        
        

info=readRDS("data_inst.rds")
names(info)=c("pais","geo","inst","nom_pc","enf","pc","VD","CB")

info=info%>%
        group_by(pais,geo)%>%
        mutate(E_AR=ifelse(enf=="Atención y Respuesta",1,0),
               E_ADI=ifelse(enf=="Atención y Respuesta",0,1),
               T_INS=n_distinct(inst),
               T_INC=n_distinct(nom_pc),
               T_AR=sum(E_AR),
               T_ADI=sum(E_ADI),
               T_PC=sum(pc),
               T_VD=sum(VD),
               T_CB=sum(CB),
               T_PCVD=sum(ifelse(pc==1&VD==1,1,0)),
               T_PCCB=sum(ifelse(pc==1&CB==1,1,0)))%>%
        select(pais,geo,T_INS,T_INC,T_AR,T_ADI,T_PC,T_VD,
               T_CB,T_PCVD,T_PCCB)%>%
        distinct()

mapa_info=read_sf("mapa_amc.shp")%>%
        left_join(info,by=c("CNTR_ID"="geo"))

write_sf(mapa_info,"mapa_inst.shp")

