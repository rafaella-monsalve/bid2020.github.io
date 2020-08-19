# Librerias ---------------------------------------------------------------
library(remotes)
#install_version("gtrendsR", version = "1.4.5")
library(ggplot2) 
library(gtrendsR) 
library(lubridate)
library(tidyquant)
library(fs)
library(purrr)
library(dplyr)
library(stringr)
library(sf)
library(tidyverse)

# Directorio de Trabajo ---------------------------------------------------
setwd("C:/Mi unidad/BID/bid2020.github.io/GTrends_web_news_ytb")

# 1.1   Topics Over Time 2020-----------------------------------------------

terminos=c("Ciberacoso","Ciberbullying","Cyberbullying",
           "Violencia Domestica","Violencia contra la mujer",
           "Violence Against Women")
topics_tbl=NULL

time="2019-01-01 2019-07-01"

for (p in terminos) {
  search_terms = p
  gtrends_lst =search_terms %>%
    gtrends(time = time,
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

# 1.1.1   Eliminar Duplicados  --------------------------------------------------------
df_dups <- topics_tbl[c("keyword","value")]
topics_tbl<- topics_tbl[!duplicated(df_dups),]
# 1.1.2   Graphic -------------------------------------------------------------

topics=topics_tbl %>%
  ggplot(aes(value, interest, color = keyword)) + 
  geom_segment(aes(xend = value, yend = 0))+
  geom_point()+
  coord_flip()+
  facet_wrap(~ keyword, nrow = 1, scales = "free_y")+
  theme_tq()+
  scale_color_tq()+
  ggtitle("2019")+
  theme(plot.title = element_text(hjust = 0.5))

plot(topics)

rm(list = ls())

