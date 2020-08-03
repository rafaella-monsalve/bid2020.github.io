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


# 3   Geography Topics -----------------------------------------------------

terminos=c("Ciberacoso","Ciberbullying","Cyberbullying",
           "Violencia Domestica","Violencia contra la mujer",
           "Violence Against Women")
country_tbl=NULL

for (p in terminos) {
  
  search_terms = p
  gtrends_lst =search_terms %>%
    gtrends(time = "2019-07-01 2020-07-01",
            low_search_volume = TRUE)
  
  country_time_tbl = gtrends_lst %>% 
    pluck("interest_by_country")%>% 
    mutate(hits=as.numeric(hits))
  
  country_tbl= union_all(country_tbl,country_time_tbl)
}
country_time_tbl=country_tbl

# 3.1 Save BBDD  ---------------------------------------
#Guardar información
saveRDS(country_time_tbl,"country_time_tbl.rds")

country_tbl=readRDS("country_time_tbl.rds")

country_tbl=country_tbl%>%
  mutate(Tematica=substr(keyword, star=1, stop=1))%>%
  mutate(keyword=ifelse(Tematica=="C","Cyberbullying",
                        "Violencia Doméstica"))%>%
  select(-Tematica)%>%
  mutate(hits=replace_na(hits,0))%>%
  group_by(keyword,location) %>%
  mutate(sum_hits=sum(hits))%>%
  mutate(hits = as.numeric(sum_hits))%>%
  select(-sum_hits)%>%
  distinct()


# 3.2 Mapping data  -----------------------------------------
mapa=st_read("mapa_amc.shp")

Tema1_mapa=data.frame(mapa)%>%
  mutate(location=as.character(location),
         keyword=as.character("Cyberbullying"))%>%
  select(location,keyword)

Tema2_mapa=data.frame(mapa)%>%
  mutate(location=as.character(location),
         keyword=as.character("Violencia Doméstica"))%>%
  select(location,keyword)

data_mapa=union_all(Tema1_mapa,Tema2_mapa)
rm(Tema1_mapa,Tema2_mapa)

country_map=left_join(data_mapa,country_tbl,by=c("location","keyword"))%>%
  mutate(geo ="world",gprop="web")%>%
  mutate(hits=replace_na(hits,0))%>%
  group_by(keyword,location) %>%
  mutate(sum_hits=sum(hits))%>%
  mutate(hits = as.numeric(sum_hits)) %>%
  group_by(keyword) %>%
  mutate(max_hits=max(hits))%>%
  mutate(min_hits=0)%>%
  mutate(rescal=trunc((hits-min_hits)/(max_hits-min_hits)*100))%>%
  mutate(hits = as.numeric(rescal))%>% 
  select(location,keyword,hits,geo,gprop)

#Guardar información
saveRDS(country_map,"country_map.rds")

map_plot=mapa%>%
  select(location)%>%
  mutate(location=as.character(location))%>%
  left_join(country_map,by=c("location"))

#Guardar información
write_sf(map_plot,"map_plot.shp")

# 3.2.1 Graphic - Mapping  -----------------------------------------------------------
map_plot=st_read("map_plot.shp")

geo_word= ggplot() +
  geom_sf(data = map_plot,aes(fill = hits),color = "whitesmoke",size=0) +
  scale_fill_viridis_c()+
  theme_tq()+
  facet_wrap(~ keyword, nrow = 1) +
  labs(title = "Keyword Trends - Word")

plot(geo_word)
