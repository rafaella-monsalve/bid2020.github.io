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

# 2   Interest Over Time -----------------------------------------------
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

# 2.1 Save BBDD and plot -------------------------------
#Guardar información
saveRDS(interest_tbl,"interest_tbl.rds")

# 2.2 Graphic -------------------------------------------------------------
interest_tbl=readRDS("interest_tbl.rds")

Interest=interest_tbl%>%
  ggplot(aes(date,hits, color=keyword))+
  geom_line()+
  theme_tq() + 
  labs(title = "Keyword Trends - Over Time")

plot(Interest)