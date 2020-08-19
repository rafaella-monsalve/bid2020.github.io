---
title: "Trends"
author: "Rafaella Monsalve"
date: "27-07-2020"
output: 
  html_document:
    toc: true
    toc_depth: 5
    toc_float:
      collapsed: false
      smooth_scroll: true
      
#Nivel 1
##Nivel 2
###Nivel 3
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```



Para obtener una aproximación sobre la situación de violencia de género, cyberbullying, acoso y violencia doméstica entre los años 2019 y 2020, en la región de Latinoamérica y el Caribe. En conjunto con el Banco Interamericano de Desarrollo -BID, se ha realizado una recopilación de datos de mediante una consulta masiva en línea, respecto a los siguientes tópicos (keywords): 

- "Ciberacoso"
- "Ciberbullying" o "Cyberbullying"
- "Violencia Domestica",
- "Violencia contra la mujer" o "Violence Against Women"

Contrastando la información, desde consultas que se hayan realizado a Google en: 
- Noticias
- Sitios web 
- Youtube 


##  Organización de la información {.tabset}

La presente documentación tiene como objetivo  facilitar la disposición del  *script*, *inputs* y *outputs*, entendidos como el código en Rstudio, la *data de entrada* y los *resultados* respectivamente. Esto, con la finalidad de que la información pueda ser ser reutilizada, reproducida o modificada de acuerdo al criterio de los investigadores y u otros objetivos que pudiesen surgir de  este proyecto.

### Output | Resultados {#output}

Cada uno de los resultados corresponde a una secuencia de 5 Scripts, que pueden ser revisados en detalle en el apartado denominado "Script". 

#### 1. Topics Over Time 2019-2020

Se realizó una comparación entre los años 2019 y 2020,  entre el 1 de enero y 1 de julio para cada año respectivamente, indagando en los siguientes términos:

- "Ciberacoso"
- "Ciberbullying" o "Cyberbullying"
- "Violencia Domestica",
- "Violencia contra la mujer" o "Violence Against Women"

Mediante esta comparación se exploró  la tendencia de los tópicos, como también de otros términos relacionados, como se observa en la siguiente gráfica:



```{r eval=FALSE}
topics_tbl = readRDS("topics_tbl19.rds")
topics=topics_tbl %>%
        ggplot(aes(value, interest, color = keyword)) + 
        geom_segment(aes(xend = value, yend = 0))+
        geom_point()+
        coord_flip()+
        facet_wrap(~ keyword, nrow = 1, scales = "free_y")+
        theme_tq()+
        scale_color_tq()

plot(topics)
```

![](images/Topics.png)

#### 2. Keyword Trends Over Time
 
```{r eval=FALSE}
interest_tbl=readRDS("interest_tbl.rds")

Interest=interest_tbl%>%
        ggplot(aes(date,hits, color=keyword))+
        geom_line()+
        theme_tq() + 
        labs(title = "Keyword Trends - Over Time")

plot(Interest)
```

 

![](images/Interest.png)


#### 3. Mapping Keyword Trends
 
```{r eval=FALSE}
map_plot=st_read("map_plot.shp")

geo_word= ggplot() +
        geom_sf(data = map_plot,aes(fill = hits),color = "whitesmoke",size=0) +
        scale_fill_viridis_c()+
        theme_tq()+
        facet_wrap(~ keyword, nrow = 1) +
        labs(title = "Keyword Trends - Word")

plot(geo_word)
```
 

![](images/Mapping.png)

#### 4. Interest Over Time for Gprop

Se especificó la consulta sobre las tendencias de Google, a través de la ubicación geográfica de las consultas de las palabras clave. Así como el período de tiempo que desea obtener datos, para los diferentes productos ("gprop") que desean consultar, en específico: Sitios Web, Noticias y Youtube. 

- En el caso de las consultas a sitios Web, arroja un resultado de 7488 observaciones:

```{r}
gtrends_gral <- readRDS("gtrends_gral.rds")
gtrends_gral <-gtrends_gral[order(-gtrends_gral$hits),]
head(gtrends_gral)
```


- En el caso de las consultas sobre noticias, arroja un resultado de 1716 observaciones:


```{r}
gtrends_new <- readRDS("gtrends_new.rds")
gtrends_new <-gtrends_new[order(-gtrends_new$hits),]
head(gtrends_new)
```
- En el caso de las consultas en Youtube, arroja un resultado de 2964 observaciones:
```{r}
gtrends_ytb <- readRDS("gtrends_ytb.rds")
gtrends_ytb <-gtrends_ytb[order(gtrends_ytb$hits),]
tail(gtrends_ytb)
```

#### 5. Unión bases de datos de Google Trends

```{r eval=FALSE}
interest_gprop= interest_data%>%
        ggplot(aes(date,hits, color=keyword))+
        geom_line()+
        theme_minimal() + 
        facet_wrap(~gprop,nrow = 3)+ 
        labs(title = "Keyword Trends - Over Time")

plot(interest_gprop)
```

![](images/All.png)

#### 6 Keywords Trens 2019-2020

![](images/Keywords2019.png)

![](images/Keywords2020.png)

### Script {#script}

Se ejecutan las siguientes librerías, una vez realizado este proceso, no es necesario hacerlo nuevamente.

```{r eval=FALSE, include=FALSE}

library(remotes)
library(gtrendsR) 
library(lubridate)
library(tidyquant)
library(fs)
library(purrr)
library(dplyr)
library(stringr)
library(sf)
library(tidyverse)

```

Luego se buscan los "keywords" en idioma inglés y español.

```{r eval=FALSE, include=FALSE}
terminos=c("Ciberacoso","Ciberbullying","Cyberbullying",
           "Violencia Domestica","Violencia contra la mujer",
           "Violence Against Women")
topics_tbl=NULL
```


```{r eval=FALSE, include=FALSE}
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
```

```{r eval=FALSE, include=FALSE}
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
```

```{r eval=FALSE, include=FALSE}
topics_tbl <- readRDS("topics_tbl.rds")
```

```{r eval=FALSE, include=FALSE}
class(topics_tbl$value)
```


```{r eval=FALSE, include=FALSE}

topics_tbl$value <- as.character(topics_tbl$value)
```


```{r eval=FALSE, include=FALSE}
topics_tbl$value[topics_tbl$value == "International Day for the Elimination of Violence against Women"] <- "International Day...Women"
```

```{r eval=FALSE, include=FALSE}
topics_tbl$value <- as.factor(topics_tbl$value)
```

```{r eval=FALSE, include=FALSE}
topics_tbl<- topics_tbl%>%
  mutate(value = as_factor(value)%>%  
           fct_reorder(interest))
```

```{r eval=FALSE, include=FALSE}

p <- topics_tbl %>%
  ggplot(aes(value, interest, color = keyword)) + 
  geom_segment(aes(xend = value, yend = 0))+
  geom_point()+
  coord_flip()+
  facet_wrap(~ keyword, nrow = 1, scales = "free_y")+
  theme_tq()+
  scale_color_tq()

plot(p)
```


```{r eval=FALSE, include=FALSE}

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
```


```{r eval=FALSE, include=FALSE}
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
```


```{r eval=FALSE, include=FALSE}
interest_tbl%>%
  ggplot(aes(date,hits, color=keyword))+
  geom_line()+
  theme_tq() + 
  labs(title = "Keyword Trends - Over Time")
```

Tendencias y geografía
```{r eval=FALSE, include=FALSE}
terminos=c("Ciberacoso","Ciberbullying","Cyberbullying",
           "Violencia Domestica","Violencia contra la mujer",
           "Violence Against Women")
country_tbl=NULL
```


```{r eval=FALSE, include=FALSE}
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
```


```{r eval=FALSE, include=FALSE}
mapa=st_read("mapa_amc.shp")

data_mapa=data.frame(mapa$location)
names(data_mapa)="location"
data_mapa$keyword="Cyberbullying"
data2=data_mapa
data2$keyword="Violencia Doméstica"
data_mapa=rbind(data_mapa,data2)
rm(data2)
```


```{r eval=FALSE, include=FALSE}
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
```


```{r eval=FALSE, include=FALSE}
mapa2=mapa%>%
  select(location)%>%
  mutate(location=as.character(location))

mapa2=left_join(mapa2,country_map,by=c("location"))
```


```{r eval=FALSE, include=FALSE}
ggplot() +
  geom_sf(data = mapa2,aes(fill = hits),color = "whitesmoke",size=0) +
  scale_fill_viridis_c()+
  theme_tq()+
  facet_wrap(~ keyword, nrow = 1) +
  labs(title = "Keyword Trends - Word")
```


```{r eval=FALSE, include=FALSE}
# Gprop_AMC ---------------------------------------------------------------
rm(list = ls())


mapa=st_read("mapa_amc.shp")
mapa=data.frame(mapa)%>%
  select(CNTR_ID,location)%>%
  mutate(geo=as.character(CNTR_ID),
         location=as.character(location))%>%
  select(-CNTR_ID)
```


```{r eval=FALSE, include=FALSE}
terminos=c("Ciberacoso","Ciberbullying","Cyberbullying",
           "Sexting","Child grooming","School bullying",
           "Violencia Domestica","Violencia contra la mujer",
           "Violence Against Women")

country_term=NULL
```


```{r eval=FALSE, include=FALSE}
for (t in terminos) {
  dato=mapa
  dato$keywords=t
  
  country_term=union_all(country_term,dato)
}
```


```{r eval=FALSE, include=FALSE}
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
```

Web Ggrop

```{r eval=FALSE, include=FALSE}
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
```


```{r eval=FALSE, include=FALSE}
```


```{r eval=FALSE, include=FALSE}
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


```

```{r eval=FALSE, include=FALSE}
gtrends_gral <- readRDS("gtrends_gral.rds")
gtrends_new <- readRDS("gtrends_new.rds")
gtrends_ytb <- readRDS("gtrends_ytb.rds")
```


```{r eval=FALSE, include=FALSE}
# Unir las BBDGT ----------------------------------------------------------

gtrend_full=rbind(gtrends_gral,gtrends_new,gtrends_ytb)

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
```
```{r eval=FALSE, include=FALSE}
for (i in seq(dias)) {
  data=data.frame()
  data$fecha=i
  fechas=union(fechas,data)
  
}
```


```{r eval=FALSE, include=FALSE}
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

```



### Tablas de salida

