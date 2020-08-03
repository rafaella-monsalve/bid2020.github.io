# Librerias ---------------------------------------------------------------
library(remotes)
#install_version("gtrendsR", version = "1.4.5")
library(gtrendsR) 
library(lubridate)
library(tidyquant)
library(fs)
library(purrr)
library(dplyr)
library(stringr)
library(sf)
library(tidyverse)

# 5   Unir las Consultas ----------------------------------------------------------
#gtrend_full=rbind(gtrends_gral,gtrends_new,gtrends_ytb)

gtrend_full= readRDS("gtrend_full.rds")

mapa=st_read("mapa_amc.shp")
mapa=data.frame(mapa)%>%
  mutate(geo=as.character(CNTR_ID),
         location=as.character(location))%>%
  select(geo,location)%>%
  mutate(keyword=as.character("Cyberbullying"),
         gprop=as.character("web"),
         agno=as.character("2020"))
copia=mapa
copia$agno="2019"
mapa=rbind(copia,mapa)
copia=mapa
copia$gprop="news"
table(copia$agno,copia$gprop)
mapa=rbind(copia,mapa)
table(mapa$agno,mapa$gprop)
copia$gprop="youtube"
table(copia$agno,copia$gprop)
mapa=rbind(copia,mapa)
table(mapa$agno,mapa$gprop,mapa$keyword)
copia=mapa
copia$keyword="Violencia Doméstica"
mapa=rbind(copia,mapa)
table(mapa$agno,mapa$gprop,mapa$keyword)

rm(copia)


# 5.1 Gprop Time Interest data ------------------------------------------------

gtrend_full=gtrend_full%>%
  mutate(Tematica=substr(keyword, star=1, stop=1))%>%
  mutate(keyword=ifelse(Tematica=="V","Violencia Doméstica","Cyberbullying"))%>%
  select(-Tematica)%>%
  mutate(agno=as.character.Date(date))%>%
  mutate(agno=substr(agno, star=1, stop=4))

gtrend_full=left_join(mapa,gtrend_full,
                      by=c("geo","keyword","gprop","agno"))

interest_data=gtrend_full%>%
  select(keyword,gprop,agno,date,hits)%>%
  filter(!is.na(date))%>%
  distinct()%>%
  mutate(date=ymd(date))%>%
  distinct()%>%
  mutate(date=round_date(date,unit = "month"))%>%
  group_by(keyword,gprop,date)%>%
  mutate(inter_fec=sum(hits))%>%
  mutate(hits=as.numeric(inter_fec))%>%
  mutate(agno=as.character.Date(date))%>%
  mutate(agno=substr(agno, star=1, stop=4))%>%
  select(keyword,gprop,agno,date,hits)%>%
  distinct()%>%
  group_by(agno,keyword,gprop)%>%
  mutate(max_hits=max(hits))%>%
  mutate(min_hits=0)%>%
  mutate(rescal=trunc((hits-min_hits)/(max_hits-min_hits)*100))%>%
  mutate(hits = as.integer(rescal))%>% 
  select(keyword,gprop,agno,date,hits)%>%
  distinct()

# 5.1.2 Graphic -------------------------------------------------------------
interest_gprop= interest_data%>%
  ggplot(aes(date,hits, color=keyword))+
  geom_line()+
  theme_minimal() + 
  facet_wrap(~gprop,nrow = 3)+ 
  labs(title = "Keyword Trends - Over Time")

plot(interest_gprop)

# 5.2 Gprop Geography data -----------------------------------------------------
data_mapa=mapa

country_data=gtrend_full%>%
  select(keyword,gprop,agno,date,hits,location)%>%
  filter(!is.na(date))%>%
  distinct()%>%
  mutate(date=ymd(date))%>%
  distinct()%>%
  mutate(date=round_date(date,unit = "month"))%>%
  group_by(location,keyword,gprop,date)%>%
  mutate(inter_fec=sum(hits))%>%
  mutate(hits=as.numeric(inter_fec))%>%
  mutate(agno=as.character.Date(date))%>%
  mutate(agno=substr(agno, star=1, stop=4))%>%
  select(location,keyword,gprop,agno,date,hits)%>%
  distinct()%>%
  group_by(location,agno,keyword,gprop)%>%
  mutate(max_hits=max(hits))%>%
  mutate(min_hits=0)%>%
  mutate(rescal=trunc((hits-min_hits)/(max_hits-min_hits)*100))%>%
  mutate(hits = as.integer(rescal))%>% 
  select(location,keyword,gprop,agno,hits)%>%
  group_by(location,agno,keyword,gprop)%>%
  mutate(hits=sum(hits))%>%
  distinct()%>%
  mutate(hits=replace_na(hits,0))%>%
  group_by(agno,keyword,gprop)%>%
  mutate(max_hits=max(hits))%>%
  mutate(min_hits=0)%>%
  mutate(rescal=trunc((hits-min_hits)/(max_hits-min_hits)*100))%>%
  mutate(hits = as.integer(rescal))%>% 
  select(location,keyword,gprop,agno,hits)%>%
  distinct()

data_mapa=mapa%>%
  select(-geo)%>%
  left_join(country_data,by=c("location","keyword",
                              "gprop","agno"))%>%
  mutate(hits=replace_na(hits,0))

shape=st_read("mapa_amc.shp")
map_plot=shape%>%
  select(location)%>%
  mutate(location=as.character(location))%>%
  left_join(data_mapa,by=c("location"))

map_plot2019=map_plot%>%
  filter(agno==2019)

map_plot2020=map_plot%>%
  filter(agno==2020)

# 5.2.1 Mapping -------------------------------------------------------------
geo_2019=ggplot() +
  geom_sf(data = map_plot2019,aes(fill = hits),color = "whitesmoke",size=0) +
  scale_fill_viridis_c()+
  theme_tq()+
  facet_grid(keyword ~ gprop) +
  labs(title = "Keyword Trends - Word. 2019")

plot(geo_2019)

geo_2020= ggplot() +
  geom_sf(data = map_plot2020,aes(fill = hits),color = "whitesmoke",size=0) +
  scale_fill_viridis_c()+
  theme_tq()+
  facet_grid(keyword ~ gprop) +
  labs(title = "Keyword Trends - Word. 2020")

plot(geo_2020)