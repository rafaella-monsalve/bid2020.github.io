library(remotes)
install_version("gtrendsR", version = "1.4.5")

library(gtrendsR) 
library(lubridate)
library(tidyquant)
library(fs)
library(purrr)
library(dplyr)
library(stringr)
library(sf)
library(tidyverse)

setwd("C:/BID/Consultoria/gtrends_words")

# Search Topics Over Time -----------------------------------------------

terminos=c("Ciberacoso","Ciberbullying","Cyberbullying",
           "Violencia Domestica","Violencia contra la mujer",
           "Violence Against Women")
topics_tbl=NULL

for (p in terminos) {
  
  search_terms = p
  gtrends_lst =search_terms %>%
    gtrends(time = "2019-07-01 2020-07-01",
            low_search_volume = TRUE)
  
  n_term=10
  top_n_related_topics_tbl = gtrends_lst %>% 
    pluck("related_topics") %>% 
    as_tibble() %>% 
    filter(related_topics == "top") %>% 
    mutate(interest = as.numeric(subject)) %>% 
    select(keyword,value,interest)%>% 
    group_by(keyword) %>% 
    arrange(desc(interest))%>% 
    slice(1:n_term) %>% 
    ungroup() %>% 
    mutate(value = as_factor(value) %>%  fct_reorder(interest))
  
  topics_tbl= union_all(topics_tbl,top_n_related_topics_tbl)
}

topics_tbl=topics_tbl%>%
  mutate(Tematica=substr(keyword, star=1, stop=1))%>%
  mutate(keyword=ifelse(Tematica=="C","Cyberbullying","Violencia Doméstica"))%>%
  select(-Tematica)%>%
  group_by(keyword,value) %>%
  mutate(max_in=max(interest))%>%
  mutate(interest = as.numeric(max_in)) %>% 
  select(keyword,value,interest)%>%
  group_by(keyword) %>% 
  arrange(desc(interest))%>% 
  ungroup() %>% 
  mutate(value = as_factor(value) %>%  fct_reorder(interest))

topics_tbl %>%
  ggplot(aes(value, interest, color = keyword)) + 
  geom_segment(aes(xend = value, yend = 0))+
  geom_point()+
  coord_flip()+
  facet_wrap(~ keyword, nrow = 1, scales = "free_y")+
  theme_tq()+
  scale_color_tq()

setwd("C:/BID/Consultoria/gtrends_words")
saveRDS(topics_tbl,"topics_tbl.rds")

# Search Interest Over Time -----------------------------------------------
terminos=c("Ciberacoso","Ciberbullying","Cyberbullying",
           "Violencia Domestica","Violencia contra la mujer",
           "Violence Against Women")
interest_tbl=NULL

for (p in terminos) {
  
  search_terms = p
  gtrends_lst =search_terms %>%
    gtrends(time = "2019-07-01 2020-07-01",
            low_search_volume = TRUE)
  
  interest_time_tbl = gtrends_lst %>% 
    pluck("interest_over_time")
  
  interest_tbl= union_all(interest_tbl,interest_time_tbl)
}

interest_tbl=interest_tbl%>%
  mutate(Tematica=substr(keyword, star=1, stop=1))%>%
  mutate(keyword=ifelse(Tematica=="C","Cyberbullying","Violencia Doméstica"))%>%
  select(-Tematica)%>%
  group_by(keyword,date) %>%
  mutate(sum_hits=sum(hits))%>%
  mutate(hits = as.numeric(sum_hits)) %>%
  group_by(keyword) %>%
  mutate(max_hits=max(hits))%>%
  mutate(min_hits=0)%>%
  mutate(rescal=trunc((hits-min_hits)/(max_hits-min_hits)*100))%>%
  mutate(hits = as.numeric(rescal))%>% 
  select(date,hits,keyword,geo,time,gprop,category)

interest_tbl%>%
  ggplot(aes(date,hits, color=keyword))+
  geom_line()+
  theme_tq() + 
  labs(title = "Keyword Trends - Over Time")

setwd("C:/BID/Consultoria/gtrends_words")
saveRDS(interest_tbl,"interest_tbl.rds")
# Trends by Geography -----------------------------------------------------
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
country_tbl=country_tbl%>%
  mutate(Tematica=substr(keyword, star=1, stop=1))%>%
  mutate(keyword=ifelse(Tematica=="C","Cyberbullying","Violencia Doméstica"))%>%
  select(-Tematica)%>%
  mutate(hits=replace_na(hits,0))%>%
  group_by(keyword,location) %>%
  mutate(sum_hits=sum(hits))%>%
  mutate(hits = as.numeric(sum_hits))%>%
  select(-sum_hits)%>%
  distinct()

setwd("C:/BID/Consultoria/gtrends_words")
saveRDS(country_time_tbl,"country_time_tbl.rds")


mapa=st_read("mapa_amc.shp")

data_mapa=data.frame(mapa$location)
names(data_mapa)="location"
data_mapa$keyword="Cyberbullying"
data2=data_mapa
data2$keyword="Violencia Doméstica"
data_mapa=rbind(data_mapa,data2)
rm(data2)

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

setwd("C:/BID/Consultoria/gtrends_words")
saveRDS(country_map,"country_map.rds")

mapa2=mapa%>%
  select(location)%>%
  mutate(location=as.character(location))

mapa2=left_join(mapa2,country_map,by=c("location"))


ggplot() +
  geom_sf(data = mapa2,aes(fill = hits),color = "whitesmoke",size=0) +
  scale_fill_viridis_c()+
  theme_tq()+
  facet_wrap(~ keyword, nrow = 1) +
  labs(title = "Keyword Trends - Word")

setwd("C:/BID/Consultoria/gtrends_words")
write_sf(mapa2,"mapa2.shp")


# Gprop_AMC ---------------------------------------------------------------
rm(list = ls())


mapa=st_read("mapa_amc.shp")
mapa=data.frame(mapa)%>%
  select(CNTR_ID,location)%>%
  mutate(geo=as.character(CNTR_ID),
         location=as.character(location))%>%
  select(-CNTR_ID)

terminos=c("Ciberacoso","Ciberbullying","Cyberbullying",
           "Sexting","Child grooming","School bullying",
           "Violencia Domestica","Violencia contra la mujer",
           "Violence Against Women")

country_term=NULL

for (t in terminos) {
  dato=mapa
  dato$keywords=t
  
  country_term=union_all(country_term,dato)
}

x="2019-07-01"

gtrend_gral=data_frame()
gtrend_gral$date=as.POSIXct(x)
gtrend_gral$hits=as.integer()
gtrend_gral$keyword=as.character()
gtrend_gral$geo=as.character()
gtrend_gral$gprop=as.character()

gtrend_new=gtrend_gral
gtrend_ytb=gtrend_gral

rm(x,mapa,dato)

# Web_Gprop ---------------------------------------------------------------
for (i in 1:dim(country_term)[1]) {
  
  country=country_term[i,2]
  search_terms=country_term[i,3]
  
  for (p in country) {
    gtrends_lst = search_terms %>%
      gtrends(geo=p,time = "2019-07-01 2020-07-01",
              low_search_volume = TRUE)
    
    x=unique(gtrends_lst$interest_over_time)
    
    if (is.null(x)==FALSE){
      
      info= gtrends_lst %>%
        pluck("interest_over_time") %>%
        as_tibble() %>%
        select(date,hits,keyword,geo,gprop)
      
      info$hits=as.integer(info$hits)
      
      gtrend_gral=union(gtrend_gral,info)}
  }
}

setwd("C:/BID/Consultoria/gtrends_words")
saveRDS(gtrend_gral,"gtrends_gral.rds")


# News_Gprop --------------------------------------------------------------

for (i in 1:dim(country_term)[1]) {
  
  country=country_term[i,2]
  search_terms=country_term[i,3]
  
  for (p in country) {
    gtrends_lst = search_terms %>%
      gtrends(geo=p,time = "2019-07-01 2020-07-01",
              gprop = "news",
              low_search_volume = TRUE)
    
    x=unique(gtrends_lst$interest_over_time)
    
    if (is.null(x)==FALSE){
      
      info= gtrends_lst %>%
        pluck("interest_over_time") %>% 
        as_tibble() %>% 
        select(date,hits,keyword,geo,gprop)
      info$hits=as.integer(info$hits)
      
      gtrend_new=union(gtrend_new,info)}
  }
}

setwd("C:/BID/Consultoria/gtrends_words")
saveRDS(gtrend_new,"gtrends_new.rds")

# Youtube Gprop -----------------------------------------------------------

for (i in 1:dim(country_term)[1]) {
  country=country_term[i,2]
  search_terms=country_term[i,3]
  
  for (p in country) {
    gtrends_lst = search_terms %>%
      gtrends(geo=p,time = "2020-01-01 2020-07-01",
              gprop = "youtube",
              low_search_volume = TRUE)
    
    x=unique(gtrends_lst$interest_over_time)
    
    if (is.null(x)==FALSE){
      
      info= gtrends_lst %>%
        pluck("interest_over_time") %>% 
        as_tibble() %>%
        select(date,hits,keyword,geo,gprop)
      
      info$hits=as.integer(info$hits)
      
      gtrend_ytb=union(gtrend_ytb,info)
    }
  }
}

setwd("C:/Mi unidad/BID/Output")
saveRDS(gtrend_ytb,"2020-01-01_2020-07-01.rds")

rm(info,gtrends_lst,country_term)


# Unir las BBDGT ----------------------------------------------------------

gtrend_full=rbind(gtrend_gral,gtrend_new,gtrend_ytb)

mapa=st_read("mapa_amc.shp")
mapa=data.frame(mapa)%>%
  select(CNTR_ID,location)%>%
  mutate(geo=as.character(CNTR_ID),
         location=as.character(location))%>%
  select(-CNTR_ID)%>%
  mutate(keyword="Cyberbullying")
data2=mapa
data2$keyword="Violencia Doméstica"
mapa=rbind(mapa,data2)
rm(data2)
mapa$gprop="web"
data2=mapa
data2$gprop="news"
mapa=rbind(mapa,data2)
data2$gprop="youtube"
mapa=rbind(mapa,data2)
rm(data2)
mapa$agno=as.character(2019)
data2=mapa
data2$agno=as.character(2020)
mapa=rbind(mapa,data2)
rm(data2)


gtrend_full=gtrend_full%>%
  mutate(Tematica=substr(keyword, star=1, stop=1))%>%
  mutate(keyword=ifelse(Tematica=="V","Violencia Doméstica","Cyberbullying"))%>%
  select(-Tematica)%>%
  mutate(agno=as.character.Date(date))%>%
  mutate(agno=substr(agno, star=1, stop=4))

gtrend_full=left_join(mapa,gtrend_full,
                      by=c("geo","keyword","gprop","agno"))

z <- 0
b=as.POSIXct(z, origin = "0000-01-01", tz = "GMT")

interest_tbl=gtrend_full%>%
  select(keyword,gprop,agno,date,hits)%>%
  mutate(date=replace_na(date,b))%>%
  filter(date!=b)%>%
  distinct()%>%
  mutate(date=ymd(date))%>%
  distinct()%>%
  group_by(agno,gprop,keyword,date)%>%
  mutate(interest=max(hits))%>%
  mutate(hits=interest)%>%
  select(-interest)%>%
  distinct()%>%
  mutate(fec=ymd(date))

dia1=ymd(20190701)
dia2=ymd(20200701)
dias=seq(ymd(20190701),ymd(20200701),by=1)
fechas=data.frame()
for (i in 1: seq(dias)) {
  data=data.frame()
  data$fecha=i
  fechas=union(fechas,data)
  
}
fechas$fecha=dias
map$date=
  
  filter(max(hits))

interest_tbl%>%
  ggplot(aes(date,hits, color=keyword))+
  geom_line()+
  theme_tq() + 
  facet_wrap(~gprop,nrow = 3)+ 
  labs(title = "Keyword Trends - Over Time")




group_by(keyword,value) %>%
  mutate(max_in=max(interest))%>%
  mutate(interest = as.numeric(max_in)) %>% 
  select(keyword,value,interest)%>%
  
  group_by(keyword,date) %>%
  mutate(sum_hits=sum(hits))%>%
  mutate(hits = as.numeric(sum_hits)) %>%
  group_by(keyword,agno) %>%
  mutate(max_hits=max(hits))%>%
  mutate(min_hits=0)%>%
  mutate(rescal=trunc((hits-min_hits)/(max_hits-min_hits)*100))%>%
  mutate(hits = as.numeric(rescal))%>% 
  select(date,hits,keyword,geo,gprop,location)


interest_tbl%>%
  ggplot(aes(date,hits, color=keyword))+
  geom_line()+
  theme_tq() + 
  labs(title = "Keyword Trends - Over Time")

