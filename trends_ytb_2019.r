# Librerias ---------------------------------------------------------------
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

setwd("C:/Mi unidad/BID/bid2020.github.io")

# 1   Topics Over Time -----------------------------------------------
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

# 1.1 Save BBDD and plot ----------------------------------------------
#Guardar información
saveRDS(topics_tbl,"topics_tbl.rds")
# 1.2 Graphic -------------------------------------------------------------

topics_tbl = readRDS("topics_tbl.rds")

topics=topics_tbl %>%
        ggplot(aes(value, interest, color = keyword)) + 
        geom_segment(aes(xend = value, yend = 0))+
        geom_point()+
        coord_flip()+
        facet_wrap(~ keyword, nrow = 1, scales = "free_y")+
        theme_tq()+
        scale_color_tq()

plot(topics)

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
                        gtrends(geo=p,time = "2019-01-01 2019-07-01",
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

# 5   Unir las BBDGT ----------------------------------------------------------
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

