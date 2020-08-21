# Librerias ---------------------------------------------------------------
library(tidyverse)
library(tidyquant)
library(purrr)
library(dplyr)
library(stringr)
library(sf)
library(xml2)
library(rvest)
library(httr)
library(magrittr)
library(scales)
library(lubridate)
library(ggplot2)
library(purrr)
library(stringr)
library(stringi)
library(glue)

# Directorio de Trabajo ---------------------------------------------------
setwd("C:/Mi unidad/BID/bid2020.github.io/academic_journals")
# 1. Journals_SCIELO ---------------------------------------------------
setwd("C:/Mi unidad/BID/bid2020.github.io/academic_journals/data_of/Scielo")
ejemplo=NULL
archivos=dir()

for (i in archivos) {
        data <- read_csv(i)
        names(data)=c("pais","value")
        data$agno=substring(i,(nchar(i)-5),(nchar(i)-4))
        data$keyword=substring(i,1,(nchar(i)-6))
        data$journal="SCIELO"
        
        ejemplo=rbind(ejemplo,data)}


# 1.1 Save BBDD -----------------------------------------------------------
setwd("C:/BID_GH/academic_journals/data_of/data_rds")
saveRDS(ejemplo,"data_scielo.rds")
readRDS(data_scielo.rds)

rm(list = ls())

# 2. Journals_SCOPUS ---------------------------------------------------
setwd("C:/BID_GH/academic_journals/data_of/Scopus")
ejemplo=NULL
archivos=dir()

for (i in archivos) {
data <- read_csv2(i)%>% 
        select(COUNTRY,X22)%>%
        mutate(COUNTRY=replace_na(COUNTRY,0))%>%
        filter(COUNTRY!=0)

names(data)=c("pais","value")
data$agno=substring(i,(nchar(i)-5),(nchar(i)-4))
data$keyword=substring(i,1,(nchar(i)-6))
data$journal="SCOPUS"

ejemplo=rbind(ejemplo,data)}

# 2.1 Save BBDD -----------------------------------------------------------
setwd("C:/BID_GH/academic_journals/data_of/data_rds")
saveRDS(ejemplo,"data_scopus.rds")
rm(list = ls())

# 3. Journals_WEBOFSCIENCE ---------------------------------------------------
setwd("C:/BID_GH/academic_journals/data_of/WebofScience")
ejemplo=NULL
archivos=dir()

for (i in archivos) {
data <- read.delim(i,header = FALSE)%>% 
        select(V1,V2)%>% 
        mutate(V2=as.numeric(V2),
               V1=as.character(V1))
data=data[-1,]
data=data[-dim(data)[1],]
names(data)=c("pais","value")
data$agno=substring(i,(nchar(i)-5),(nchar(i)-4))
data$keyword=substring(i,1,(nchar(i)-6))
data$journal="WEBOFSCIENCE"

ejemplo=rbind(ejemplo,data)}

# 3.1 Save BBDD -----------------------------------------------------------
setwd("C:/BID_GH/academic_journals/data_of/data_rds")
saveRDS(ejemplo,"data_webofscience.rds")
rm(list = ls())

# 4. Journals_GSCHOLAR ------------------------------------------------------
setwd("C:/BID_GH/academic_journals/data_of/scholar")

# 4.1  GScholar 2019 -------------------------------------------------------
setwd("C:/BID_GH/academic_journals/data_of/scholar")

country <- readRDS("country.rds")
pais <- readRDS("pais.rds")
terminos <- readRDS("terminos.rds")
terminos[5] <- "Child+grooming"
paises <- c(country,pais)%>%
        data.frame()%>%
        distinct()
paises <- paises$.
rm(pais,country)

#Conjunto de URLS
df_seq <- expand.grid(terminos=terminos, paises=paises)
df_links <- df_seq %>%
        mutate(link=glue("https://scholar.google.com/scholar?start=&q={terminos}+{paises}&hl=es&as_sdt=1,5&as_ylo=2019&as_yhi=2019"))
data = NULL

for (i in 1: length(df_links$link)){
        
        wp <- xml2::read_html(df_links$link[i])
        contador <- rvest::html_text(rvest::html_nodes(wp, '#gs_ab_md'))
        name <- assign(paste0("contador",length(i)), data.frame(contador))
        name$terminos <- df_links$terminos[i]
        name$paises <- df_links$paises[i]
        name$link <- df_links$link[i]
        data= union_all(data,name)
        Sys.sleep(sample(50:150,1))
        
}

data$borrar <-substr(data$contador, star =1, stop = 1)
data$contador <- gsub('[.]', '', data$contador)
data$sep1 <- word(data$contador, 1)
data$sep2 <- word(data$contador, 2)
data$numero <- ifelse(data$borrar == "A", data$sep2, data$sep1)
data$sep1 <- NULL
data$sep2 <- NULL

# 4.1.1 Save BBDD ---------------------------------------------------------
saveRDS(data,"data2019.rds")
rm(list = ls())

# 4.2  GScholar 2020 --------------------------------------------------------
setwd("C:/BID_GH/academic_journals/data_of/scholar")

country <- readRDS("country.rds")
pais <- readRDS("pais.rds")
terminos <- readRDS("terminos.rds")
terminos[5] <- "Child+grooming"
paises <- c(country,pais)%>%
        data.frame()%>%
        distinct()
paises <- paises$.
rm(pais,country)

#Conjunto de URLS
df_seq <- expand.grid(terminos=terminos, paises=paises)
df_links <- df_seq %>%
        mutate(link=glue("https://scholar.google.com/scholar?start=&q={terminos}+{paises}&hl=es&as_sdt=1,5&as_ylo=2020&as_vis=1"))
data = NULL

for (i in 1: length(df_links$link)){
        
        wp <- xml2::read_html(df_links$link[i])
        contador <- rvest::html_text(rvest::html_nodes(wp, '#gs_ab_md'))
        name <- assign(paste0("contador",length(i)), data.frame(contador))
        name$terminos <- df_links$terminos[i]
        name$paises <- df_links$paises[i]
        name$link <- df_links$link[i]
        data= union_all(data,name)
        Sys.sleep(sample(50:150,1))
        
}

data$borrar <-substr(data$contador, star =1, stop = 1)
data$contador <- gsub('[.]', '', data$contador)
data$sep1 <- word(data$contador, 1)
data$sep2 <- word(data$contador, 2)
data$numero <- ifelse(data$borrar == "A", data$sep2, data$sep1)
data$sep1 <- NULL
data$sep2 <- NULL

# 4.2.1 Save BBDD ---------------------------------------------------------
saveRDS(data,"data2020.rds")
rm(list = ls())


# 4.3  BBDD_GSCHOLAR -------------------------------------------------------
setwd("C:/BID_GH/academic_journals/data_of/scholar")

data_scholar19 <- readRDS("data2019.rds")%>% 
        select(paises,terminos,numero,link)%>%
        mutate(numero=as.numeric(numero),
               terminos=as.character(terminos),
               paises=as.character(paises),
               numero=replace_na(numero,-1),
               paises=gsub("[+]", " ",paises),
               terminos=gsub("[+]", " ",terminos),
               link=as.character(link),
               agno=substring(link,(nchar(link)-15),(nchar(link)-12)))%>%
        filter(numero!=-1)%>% 
        select(paises,numero,agno,terminos)

data_scholar19$journal="GOOGLESCHOLAR"
names(data_scholar19)=c("pais","value","agno","keyword","journal")

data_scholar20 <- readRDS("data2020.rds")%>% 
        select(paises,terminos,numero,link)%>%
        mutate(numero=as.numeric(numero),
               terminos=as.character(terminos),
               paises=as.character(paises),
               numero=replace_na(numero,-1),
               paises=gsub("[+]", " ",paises),
               terminos=gsub("[+]", " ",terminos),
               link=as.character(link),
               agno=substring(link,(nchar(link)-12),(nchar(link)-9)))%>%
        filter(numero!=-1)%>% 
        select(paises,numero,agno,terminos)

data_scholar20$journal="GOOGLESCHOLAR"
names(data_scholar20)=c("pais","value","agno","keyword","journal")

data_scholar=rbind(data_scholar20,data_scholar19)

# 4.3.1 Save BBDD ---------------------------------------------------------
setwd("C:/BID_GH/academic_journals/data_of/data_rds")
saveRDS(data_scholar,"data_scholar.rds")
rm(list = ls())        

# 5. Journals data_edit ---------------------------------------------------
setwd("C:/BID_GH/academic_journals/data_of/data_rds")

scholar<- readRDS(dir()[1])
scielo<- readRDS(dir()[2])
scopus<- readRDS(dir()[3])
science<- readRDS(dir()[4])


data_journals=rbind(scielo,scopus,science)%>% 
        mutate(agno=ifelse(agno=="19","2019","2020"))

data_journals=rbind(data_journals,scholar)%>%
        mutate(keyword=str_to_lower(keyword),
               pais=str_to_lower(pais),
               journal=str_to_lower(journal),
               pais=stri_trans_general(pais,"Latin-ASCII"))
rm(scopus,scholar,scielo,science)

# 5.1 Save BBDD -----------------------------------------------------------
setwd("C:/BID_GH/bbdd_edit/data_rds")
saveRDS(data_journals,"data_journals.rds")


# 6. Graphic data academic ------------------------------------------------
data_pais <- readRDS("C:/BID_GH/bodega/data_pais.rds")

data_grafo=left_join(data_journals,data_pais,by=c("pais"))%>%
        mutate(geo=replace_na(geo,-1),
                N_AMC=ifelse(geo==-1,0,1),
               Tema=substr(keyword,1,1),
               Tema=ifelse(Tema=="v","Violencia Domestica",
                          "Cyberbullying"))%>%
        filter(geo!=-1)

data_grapf=data.frame(data_grafo)%>%
        select(Tema, agno,value)%>%
        group_by(Tema,agno)%>%
        mutate(value=sum(value))%>%
        distinct()

data_grapf=data.frame(data_grapf)

ggplot(data=data_grapf,aes(x=agno, y=value , fill=Tema)) + 
        geom_bar(stat="identity", position="dodge")+
        scale_fill_brewer(palette="Set1")+
        theme_minimal()+
        labs(title = "Academic Journal - 2019 / 2020")

rm(list = ls())


