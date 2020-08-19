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

# 1. Data 2019 ------------------------------------------------------------
# 1.1   Topics Over Time 2019-----------------------------------------------

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

# 1.1.1   Save BBDD  --------------------------------------------------------
saveRDS(topics_tbl,"topics_tbl19.rds")
# 1.1.2   Graphic -------------------------------------------------------------

topics_tbl = readRDS("topics_tbl19.rds")
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

# 1.2   Queries Over Time 2019-----------------------------------------------

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
        
       x=unique(gtrends_lst$related_queries)
        
        if (is.null(x)==FALSE){
                n_term=10
                top_n_related_topics_tbl = gtrends_lst %>% 
                        pluck("related_queries") %>% 
                        as_tibble() %>% 
                        filter(related_queries == "top") %>% 
                        mutate(interest = as.numeric(subject)) %>% 
                        select(keyword,value,interest)%>% 
                        group_by(keyword) %>% 
                        arrange(desc(interest))%>% 
                        slice(1:n_term) %>% 
                        ungroup() %>% 
                        mutate(value = as_factor(value) %>%  fct_reorder(interest))
                
                topics_tbl= union_all(topics_tbl,top_n_related_topics_tbl)
        }
}

queries_tbl=topics_tbl%>%
        mutate(Tematica=substr(keyword, star=1, stop=1))%>%
        mutate(keyword=ifelse(Tematica=="C","Cyberbullying","Violencia Doméstica"),
               value = stri_trans_general(value, 'es'))%>%
        select(-Tematica)%>%
        group_by(keyword,value) %>%
        mutate(max_in=max(interest))%>%
        mutate(interest = as.numeric(max_in)) %>% 
        select(keyword,value,interest)%>%
        group_by(keyword) %>% 
        arrange(desc(interest))%>% 
        ungroup() %>% 
        mutate(value = as_factor(value) %>%  
                       fct_reorder(interest))

# 1.2.1   Save BBDD  --------------------------------------------------------
saveRDS(queries_tbl,"queries_tbl19.rds")
# 1.2.2   Graphic -------------------------------------------------------------

queries_tbl = readRDS("queries_tbl19.rds")
queries=queries_tbl %>%
        ggplot(aes(value, interest, color = keyword)) + 
        geom_segment(aes(xend = value, yend = 0))+
        geom_point()+
        coord_flip()+
        facet_wrap(~ keyword, nrow = 1, scales = "free_y")+
        theme_tq()+
        scale_color_tq()+
        ggtitle("2019")+
        theme(plot.title = element_text(hjust = 0.5))

plot(queries)


rm(list = ls())

# 1.3  Interest Over Time 2019 -----------------------------------------------
terminos=c("Ciberacoso","Ciberbullying","Cyberbullying",
           "Violencia Domestica","Violencia contra la mujer",
           "Violence Against Women")

time="2019-01-01 2019-07-01"
interest_tbl=NULL

for (p in terminos) {
        
        search_terms = p
        gtrends_lst =search_terms %>%
                gtrends(time = time,
                        low_search_volume = TRUE)
        
        interest_time_tbl = gtrends_lst %>% 
                pluck("interest_over_time")
        
        interest_tbl= union_all(interest_tbl,interest_time_tbl)
}

interest_tbl_edit=interest_tbl%>%
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
# 1.3.1   Save BBDD  --------------------------------------------------------
saveRDS(interest_tbl,"interest_tbl_org19.rds")
saveRDS(interest_tbl_edit,"interest_tbl19.rds")
# 1.3.2   Graphic -------------------------------------------------------------
interest_tbl=readRDS("interest_tbl19.rds")

Interest=interest_tbl%>%
        ggplot(aes(date,hits, color=keyword))+
        geom_point(aes(color=keyword), size=2) + 
        geom_smooth() +
        theme_minimal() + 
        labs(title = "Keyword Trends - Over Time - 2019") +
        theme(legend.position = "bottom")

plot(Interest)

rm(list = ls())

rm(list = ls())

# 1.4  Geography Topics Time 2019 -----------------------------------------------------

terminos=c("Ciberacoso","Ciberbullying","Cyberbullying",
                   "Violencia Domestica","Violencia contra la mujer",
                   "Violence Against Women")
country_tbl=NULL
time="2019-01-01 2019-07-01"

for (p in terminos) {
        search_terms = p
        gtrends_lst =search_terms %>%
                gtrends(time = time,
                        low_search_volume = TRUE)
        
        country_time_tbl = gtrends_lst %>% 
                pluck("interest_by_country")%>% 
                mutate(hits=as.numeric(hits))
        
        country_tbl= union_all(country_tbl,country_time_tbl)
}
country_time_tbl=country_tbl

country_tbl=readRDS("country_time_tbl19.rds")

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

# 1.4.1   Mapping data  -----------------------------------------
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

map_plot=mapa%>%
        select(location)%>%
        mutate(location=as.character(location))%>%
        left_join(country_map,by=c("location"))

# 1.4.2   Save BBDD  ---------------------------------------
saveRDS(country_time_tbl,"country_time_tbl19.rds")
saveRDS(country_tbl,"country_tbl19.rds")
saveRDS(country_map,"country_map19.rds")
write_sf(map_plot,"map_plot19.shp")

# 1.4.3   Graphic - Mapa -------------------------------------------------------------
map_plot=st_read("map_plot19.shp")

geo_word= ggplot() +
        geom_sf(data = map_plot,aes(fill = hits),color = "whitesmoke",size=0) +
        scale_fill_viridis_c()+
        theme_minimal()+
        facet_wrap(~ keyword, nrow = 1) +
        labs(title = "Keyword Trends - Word - 2019")

plot(geo_word)

rm(list = ls())

# 2. Data 2020 ------------------------------------------------------------
# 2.1   Topics Over Time 2020-----------------------------------------------

terminos=c("Ciberacoso","Ciberbullying","Cyberbullying",
           "Violencia Domestica","Violencia contra la mujer",
           "Violence Against Women")
topics_tbl=NULL

time="2020-01-01 2020-07-01"

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

# 2.1.1   Save BBDD  --------------------------------------------------------
saveRDS(topics_tbl,"topics_tbl20.rds")
# 2.1.2   Graphic -------------------------------------------------------------

topics_tbl = readRDS("topics_tbl20.rds")


topics=topics_tbl %>%
        ggplot(aes(value, interest, color = keyword)) + 
        geom_segment(aes(xend = value, yend = 0))+
        geom_point()+
        coord_flip()+
        facet_wrap(~ keyword, nrow = 1, scales = "free_y")+
        theme_tq()+
        scale_color_tq()+
        ggtitle("2020")+
        theme(plot.title = element_text(hjust = 0.5))

plot(topics)

rm(list = ls())

# 2.2   Queries Over Time 2020-----------------------------------------------

terminos=c("Ciberacoso","Ciberbullying","Cyberbullying",
           "Violencia Domestica","Violencia contra la mujer",
           "Violence Against Women")
topics_tbl=NULL

time="2020-01-01 2020-07-01"

for (p in terminos) {
        search_terms = p
        gtrends_lst =search_terms %>%
                gtrends(time = time,
                        low_search_volume = TRUE)
        
        x=unique(gtrends_lst$related_queries)
        
        if (is.null(x)==FALSE){
                n_term=10
                top_n_related_topics_tbl = gtrends_lst %>% 
                        pluck("related_queries") %>% 
                        as_tibble() %>% 
                        filter(related_queries == "top") %>% 
                        mutate(interest = as.numeric(subject)) %>% 
                        select(keyword,value,interest)%>% 
                        group_by(keyword) %>% 
                        arrange(desc(interest))%>% 
                        slice(1:n_term) %>% 
                        ungroup() %>% 
                        mutate(value = as_factor(value) %>%  fct_reorder(interest))
                
                topics_tbl= union_all(topics_tbl,top_n_related_topics_tbl)
        }
}

queries_tbl=topics_tbl%>%
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

# 2.2.1   Save BBDD  --------------------------------------------------------
saveRDS(queries_tbl,"queries_tbl20.rds")
# 2.2.2   Graphic -------------------------------------------------------------

topics_tbl = readRDS("queries_tbl20.rds")
topics=topics_tbl %>%
        ggplot(aes(value, interest, color = keyword)) + 
        geom_segment(aes(xend = value, yend = 0))+
        geom_point()+
        coord_flip()+
        facet_wrap(~ keyword, nrow = 1, scales = "free_y")+
        theme_tq()+
        scale_color_tq()

plot(topics)




rm(list = ls())

# 2.3  Interest Over Time 2020 -----------------------------------------------
terminos=c("Ciberacoso","Ciberbullying","Cyberbullying",
           "Violencia Domestica","Violencia contra la mujer",
           "Violence Against Women")

time="2020-01-01 2020-07-01"
interest_tbl=NULL

for (p in terminos) {
        
        search_terms = p
        gtrends_lst =search_terms %>%
                gtrends(time = time,
                        low_search_volume = TRUE)
        
        interest_time_tbl = gtrends_lst %>% 
                pluck("interest_over_time")
        
        interest_tbl= union_all(interest_tbl,interest_time_tbl)
}

interest_tbl_edit=interest_tbl%>%
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
# 2.3.1   Save BBDD  --------------------------------------------------------
saveRDS(interest_tbl,"interest_tbl_org20.rds")
saveRDS(interest_tbl_edit,"interest_tbl20.rds")

# 2.3.2   Graphic -------------------------------------------------------------
interest_tbl=readRDS("interest_tbl20.rds")

Interest=interest_tbl%>%
        ggplot(aes(date,hits, color=keyword))+
        geom_point(aes(color=keyword), size=2) + 
        geom_smooth() +
        theme_minimal() + 
        labs(title = "Keyword Trends - Over Time - 2020") +
        theme(legend.position = "bottom")

plot(Interest)





rm(list = ls())

# 2.4  Geography Topics Time 2020 -----------------------------------------------------

terminos=c("Ciberacoso","Ciberbullying","Cyberbullying",
           "Violencia Domestica","Violencia contra la mujer",
           "Violence Against Women")
country_tbl=NULL
time="2020-01-01 2020-07-01"

for (p in terminos) {
        search_terms = p
        gtrends_lst =search_terms %>%
                gtrends(time = time,
                        low_search_volume = TRUE)
        
        country_time_tbl = gtrends_lst %>% 
                pluck("interest_by_country")%>% 
                mutate(hits=as.numeric(hits))
        
        country_tbl= union_all(country_tbl,country_time_tbl)
}
country_time_tbl=country_tbl

country_tbl=readRDS("country_time_tbl20.rds")

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

# 2.4.1   Mapping data  -----------------------------------------
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

map_plot=mapa%>%
        select(location)%>%
        mutate(location=as.character(location))%>%
        left_join(country_map,by=c("location"))

# 2.4.2   Save BBDD  ---------------------------------------
saveRDS(country_time_tbl,"country_time_tbl20.rds")
saveRDS(country_tbl,"country_tbl20.rds")
saveRDS(country_map,"country_map20.rds")
write_sf(map_plot,"map_plot20.shp")

# 2.4.3   Graphic - Mapa -------------------------------------------------------------
map_plot=st_read("map_plot20.shp")

geo_word= ggplot() +
        geom_sf(data = map_plot,aes(fill = hits),color = "whitesmoke",size=0) +
        scale_fill_viridis_c()+
        theme_minimal()+
        facet_wrap(~ keyword, nrow = 1) +
        labs(title = "Keyword Trends - Word - 2020")

plot(geo_word)

rm(list = ls())

# 3   Interest Over Time GPROP ---------------------------------------------------------------
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
rm(x,mapa,dato,t)

# 3.1 Web gprop 2019----------------------------------------------------------
for (i in 1:dim(country_term)[1]) {
        country=country_term[i,2]
        search_terms=country_term[i,3]
        
        for (p in country) {
                gtrends_lst = search_terms %>%
                        gtrends(geo =p,
                                time = "2019-01-01 2019-07-01",
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
gtrend_gral19=gtrend_gral

# 3.1.1   Save BBDD  ---------------------------------------
setwd("C:/BID_GH/GTrends_web_news_ytb/data_trends")
saveRDS(gtrend_gral19,"gtrends_gral19.rds")

# 3.2 Web gprop 2020----------------------------------------------------------
gtrend_gral=gtrend_new

for (i in 1:dim(country_term)[1]) {
        country=country_term[i,2]
        search_terms=country_term[i,3]
        
        for (p in country) {
                gtrends_lst = search_terms %>%
                        gtrends(geo =p,
                                time = "2020-01-01 2020-07-01",
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
gtrend_gral20=gtrend_gral

# 3.2.1   Save BBDD  ---------------------------------------
setwd("C:/BID_GH/GTrends_web_news_ytb/data_trends")
saveRDS(gtrend_gral20,"gtrends_gral20.rds")
gtrend_gral=gtrend_new

# 3.3 News gprop 2019 --------------------------------------------------------------
for (i in 1:dim(country_term)[1]) {
        country=country_term[i,2]
        search_terms=country_term[i,3]
        for (p in country) {
                gtrends_lst = search_terms %>%
                gtrends(geo=p,
                        time = "2019-01-01 2019-07-01",
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
gtrend_new19=gtrend_new

# 3.3.1   Save BBDD  ---------------------------------------
setwd("C:/BID_GH/GTrends_web_news_ytb/data_trends")
saveRDS(gtrend_new19,"gtrend_new19.rds")

# 3.4 News gprop 2020 --------------------------------------------------------------
gtrend_new=gtrend_gral
for (i in 1:dim(country_term)[1]) {
        country=country_term[i,2]
        search_terms=country_term[i,3]
        for (p in country) {
                gtrends_lst = search_terms %>%
                        gtrends(geo=p,
                                time = "2020-01-01 2020-07-01",
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
gtrend_new20=gtrend_new

# 3.4.1   Save BBDD  ---------------------------------------
setwd("C:/BID_GH/GTrends_web_news_ytb/data_trends")
saveRDS(gtrend_new20,"gtrend_new20.rds")

# 3.5 Youtube gprop 2019 --------------------------------------------------------------
gtrend_new=gtrend_gral
for (i in 1:dim(country_term)[1]) {
        country=country_term[i,2]
        search_terms=country_term[i,3]
        for (p in country) {
                gtrends_lst = search_terms %>%
                        gtrends(geo=p,
                                time = "2019-01-01 2019-07-01",
                                gprop = "youtube",
                                low_search_volume = TRUE)
                
                x=unique(gtrends_lst$interest_over_time)
                
                if (is.null(x)==FALSE){
                        info= gtrends_lst %>%
                                pluck("interest_over_time") %>% 
                                as_tibble() %>% 
                                select(date,hits,keyword,geo,gprop)
                        
                        info$hits=as.integer(info$hits)
                        gtrend_ytb=union(gtrend_ytb,info)}
        }
}
gtrend_ytb19=gtrend_ytb
# 3.5.1   Save BBDD  ---------------------------------------
setwd("C:/BID_GH/GTrends_web_news_ytb/data_trends")
saveRDS(gtrend_ytb19,"gtrend_ytb19.rds")

# 3.6 Youtube gprop 2020 --------------------------------------------------------------
gtrend_new=gtrend_gral
for (i in 1:dim(country_term)[1]) {
        country=country_term[i,2]
        search_terms=country_term[i,3]
        for (p in country) {
                gtrends_lst = search_terms %>%
                        gtrends(geo=p,
                                time = "2020-01-01 2020-07-01",
                                gprop = "youtube",
                                low_search_volume = TRUE)
                
                x=unique(gtrends_lst$interest_over_time)
                
                if (is.null(x)==FALSE){
                        info= gtrends_lst %>%
                                pluck("interest_over_time") %>% 
                                as_tibble() %>% 
                                select(date,hits,keyword,geo,gprop)
                        
                        info$hits=as.integer(info$hits)
                        gtrend_ytb=union(gtrend_ytb,info)}
        }
}
gtrend_ytb20=gtrend_ytb
# 3.6.1   Save BBDD  ---------------------------------------
setwd("C:/BID_GH/GTrends_web_news_ytb/data_trends")
saveRDS(gtrend_ytb20,"gtrend_ytb20.rds")


# 4. Trends data_edit ----------------------------------------------------------
setwd("C:/BID_GH/GTrends_web_news_ytb/data_trends")
archivo=dir()
data_trends=NULL
for (i in archivo) {
        data=readRDS(i)%>%
                mutate(mes=as.character.Date(date),
                       mes=substr(mes,1,7),
                       keyword=str_to_lower(keyword))%>%
                group_by(mes,geo,keyword)%>%
                mutate(hits=sum(hits))%>%
                select(gprop,keyword,geo,mes,hits)%>%
                distinct()%>%
                mutate(agno=substr(mes,1,4),
                       mes=substr(mes,6,7))%>%
                group_by(agno,keyword,geo) %>%
                mutate(max_hits=max(hits))%>%
                mutate(min_hits=0)%>%
                mutate(rescal=trunc((hits-min_hits)/(max_hits-min_hits)*100))%>%
                mutate(hits = as.numeric(rescal))%>% 
                select(agno,mes,gprop,keyword,geo,hits)%>%
                distinct()
        
        data_trends=rbind(data_trends,data)
}

# 4.1 Save BBDD  --------------------------------------------------------------
setwd("C:/BID_GH/bbdd_edit/data_rds")
saveRDS(data_trends,"data_trends.rds")
