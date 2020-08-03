library(tidyverse)
library(tidytext)
library(stopwords)
library(RColorBrewer)
library(wordcloud)
library(purrr)
library(dplyr)
library(stringi)
library(ggplot2)
library(sf)


# Tema : Cyberbullying ----------------------------------------------------
# Resumen -----------------------------------------------------------------


data <- readRDS("Scholar/cyberacoso.rds")
stop_words=readRDS("Scholar/stop_words.rds")
geo_tok=readRDS("Scholar/geo_tok.rds")
geo_tok$word=stri_trans_general(geo_tok$word,"Latin-ASCII")

data <-data %>%
        mutate(abstract=resumen)%>%
        arrange(titulo)

resumen <- separate_rows(data, resumen, sep = "\\\\n | \\|")
resumen <- resumen %>%
        group_by(titulo) %>% 
        mutate(linea = row_number()) %>% 
        ungroup()

data_tokenizado <- resumen %>% 
        unnest_tokens(word, resumen)

data_tokenizado <- data_tokenizado %>% 
        anti_join(stop_words, by = c("word" = "stopwords"))%>% 
        mutate(com=substr(word, star=1, stop=1)) %>% 
        arrange(com)

a=which(data_tokenizado$com=="a")[1]
a=a-1
data_tokenizado=data_tokenizado[-1:-a,]
data_tokenizado$word=stri_trans_general(data_tokenizado$word,"Latin-ASCII")

data_tokenizado <- data_tokenizado %>% 
        inner_join(geo_tok,c("word"))%>%
        arrange(titulo)

pais_data_r=data_tokenizado%>%
        filter(np==1)

paper_pais_r=pais_data_r%>%
        select(titulo)%>%
        distinct()

data_tok=data_tokenizado%>%
        anti_join(paper_pais_r,by="titulo")

cdd_data_r=data_tok%>%
        filter(nc==1)

paper_cdd_r=cdd_data_r%>%
        select(titulo)%>%
        distinct()

paper_id_r=union_all(paper_cdd_r,paper_pais_r)

rm(data,data_tok,data_tokenizado,geo_tok,resumen,stop_words)

# Titulo ------------------------------------------------------------------
data <- readRDS("Scholar/cyberacoso.rds")
stop_words=readRDS("Scholar/stop_words.rds")
geo_tok=readRDS("Scholar/geo_tok.rds")
geo_tok$word=stri_trans_general(geo_tok$word,"Latin-ASCII")

data <-data %>%
        mutate(title=titulo)%>%
        arrange(titulo)

titulo <- separate_rows(data, titulo, sep = "\\\\n | \\|")
titulo <- titulo %>%
        group_by(title) %>% 
        mutate(linea = row_number()) %>% 
        ungroup()

data_tokenizado <- titulo %>% 
        unnest_tokens(word, titulo)

data_tokenizado <- data_tokenizado %>% 
        anti_join(stop_words, by = c("word" = "stopwords"))%>% 
        mutate(com=substr(word, star=1, stop=1)) %>% 
        arrange(com)

a=which(data_tokenizado$com=="a")[1]
a=a-1
data_tokenizado=data_tokenizado[-1:-a,]
data_tokenizado$word=stri_trans_general(data_tokenizado$word,"Latin-ASCII")

data_tokenizado <- data_tokenizado %>% 
        inner_join(geo_tok,c("word"))%>%
        arrange(title)

pais_data_t=data_tokenizado%>%
        filter(np==1)

paper_pais_t=pais_data_t%>%
        select(title)%>%
        distinct()

data_tok=data_tokenizado%>%
        anti_join(paper_pais_t,by="title")

cdd_data_t=data_tok%>%
        filter(nc==1)

paper_cdd_t=cdd_data_t%>%
        select(title)%>%
        distinct()

paper_id_t=union_all(paper_cdd_t,paper_pais_t)%>%
        mutate(titulo=title)%>%
        select(titulo)

paper_id=union_all(paper_id_r,paper_id_t)
rm(data,data_tok,data_tokenizado,geo_tok,titulo,stop_words,
   paper_pais_r,paper_pais_t,paper_cdd_r,paper_cdd_t,
   paper_id_r,paper_id_t,a)


# data_grafico ------------------------------------------------------------
paper_id$ID=1
paper_id=data.frame(paper_id)
paper_id=paper_id[!duplicated(paper_id),]


# info_mapa ---------------------------------------------------------------

info_res=union_all(cdd_data_r,pais_data_r)%>%
        select(titulo,agno,geo)
info_tit=union_all(cdd_data_t,pais_data_t)%>%
        select(title,agno,geo)%>%
        mutate(titulo=title)%>%
        select(-title)

info=union_all(info_res,info_tit)%>%
        distinct()%>%
        select(-titulo)

rm(cdd_data_r,cdd_data_t,pais_data_r,pais_data_t,info_res,info_tit)

info_map=info%>%
        group_by(geo)%>%
        mutate(value=ifelse(agno=="2019",sum(agno == "2019"),
                            sum(agno == "2020")))%>%
        distinct()%>%
        group_by(agno)%>%
        mutate(max_hits=max(value))%>%
        mutate(min_hits=0)%>%
        mutate(rescal=trunc((value-min_hits)/(max_hits-min_hits)*100))%>%
        mutate(hits = as.numeric(rescal))%>%
        select(agno,geo,hits)

info_map=data.frame(info_map)

shape=st_read("mapa_amc.shp")

info_plot1=data.frame(shape)%>%
        select(CNTR_ID,location)%>%
        mutate(location=as.character(location),
               geo=as.character(CNTR_ID),
               agno="2019",
               tema="Cyberbullying")%>%
        select(geo,location,agno,tema)

info_plot2=data.frame(shape)%>%
        select(CNTR_ID,location)%>%
        mutate(location=as.character(location),
               geo=as.character(CNTR_ID),
               agno="2020",
               tema="Cyberbullying")%>%
        select(geo,location,agno,tema)

info_plot=union_all(info_plot1,info_plot2)%>%
        left_join(info_map,by=c("geo","agno"))%>%
        mutate(hits=replace_na(hits,0))



# Grafico info -----------------------------------------------------------
data <- readRDS("cyberacoso.rds")
data=arrange(data,titulo)

data_id=left_join(data,paper_id,by="titulo")%>%
        mutate(ID=replace_na(ID,0))%>%
        arrange(ID)

data_1=data_id%>%
        select(agno,ID)%>%
        mutate(value=ifelse(agno=="2019",sum(agno == "2019"),
                            sum(agno == "2020")),
               area="Total")

data_2=data_id%>%
        group_by(agno)%>%
        mutate(value=sum(ID),
               area="ALC")

data_graf=union_all(data_1,data_2)%>%
        select(agno,value,area)%>%
        distinct()

rm(data_1,data_2)

data_graf=data_graf
data_graf$porc=0
data_graf[1,4]=data_graf[1,2]-data_graf[3,2]
data_graf[2,4]=data_graf[2,2]-data_graf[4,2] 
data_graf[3,4]=data_graf[3,2]
data_graf[4,4]=data_graf[4,2]
data_graf[3,2]=data_graf[1,2] 
data_graf[4,2]=data_graf[2,2] 
data_graf$prc=round(data_graf$porc/data_graf$value,2)
data_graf$Tema="Ciberbullying"


# Grafico -----------------------------------------------------------------
rm(data, data_id, paper_id)

ggplot(data=data_graf,aes(x=factor(agno),fill=factor(area)))+
        geom_bar(aes(y=porc),stat='identity',position="fill")+
        geom_text(aes(label=scales::percent(..count../sum(..count..))),
                  stat='count')


ggplot(data=data_graf,aes(x=agno,fill=area)) + 
        geom_bar(aes(y=porc),
                 stat='identity', position="stack")+
        scale_fill_brewer(palette="Set1")+
        theme_bw()+
        labs(title = "Schoolar - 2019 / 2020 - Cyberbullying")

# Mapa -----------------------------------------------------------------
rm(info_plot1,info_plot2, info, info_map)

map_plot=shape%>%
        select(location)%>%
        mutate(location=as.character(location))%>%
        left_join(info_plot,by=c("location"))%>%
        mutate(hits=as.integer(hits))

geo_school=ggplot() +
        geom_sf(data = map_plot,aes(fill = hits),color = "whitesmoke",size=0) +
        scale_fill_viridis_c()+
        theme_bw()+
        facet_grid(~ agno) +
        labs(title = "Schoolar - 2019 / 2020 - Cyberbullying")

plot(geo_school)
