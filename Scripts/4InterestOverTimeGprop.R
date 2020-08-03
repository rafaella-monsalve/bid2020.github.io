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


# 4   Interest Over Time for Gprop ---------------------------------------------------------------
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

# 4.1 Web_Gprop ---------------------------------------------------------------
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

saveRDS(gtrend_gral,"gtrends_gral.rds")

# 4.2 News_Gprop --------------------------------------------------------------

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

saveRDS(gtrend_new,"gtrends_new.rds")

# 4.3 Youtube Gprop -----------------------------------------------------------

for (i in 1:dim(country_term)[1]) {
  country=country_term[i,2]
  search_terms=country_term[i,3]
  
  for (p in country) {
    gtrends_lst = search_terms %>%
      gtrends(geo=p,time = "2019-07-01 2020-07-01",
              gprop = "youtube")[1]
    
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

saveRDS(gtrend_ytb,"gtrends_ytb.rds")

rm(info,gtrends_lst,country_term)