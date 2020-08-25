# Instalar Packages -------------------------------------------------------
install.packages("leaflet")
install.packages("sf")
install.packages("viridis")
install.packages("RColorBrewer")
install.packages("dplyr")
install.packages("htmlwidgets")


# Librerias ---------------------------------------------------------------
library(leaflet)  # libreria para graficar mapas interactivos
library(sf)  # manejo de informacion geografica 
library(viridis)  # paletas de colores
library(RColorBrewer)  # mas paletas de colores
library(dplyr)  # manejo de bases de datos
library(htmlwidgets)
library(purrr)# para guardar el mapa


# directorio de trabajo ---------------------------------------------------


data=readRDS("data espacial/base_obs.rds")%>%
        select(-nom_pais)

shape=read_sf("mapa_base.shp")%>%
        left_join(data,by="geo")%>%
        filter(vweb_vd20>=0)

palQuantile <- colorQuantile("magma", domain = shape$vweb_vd20)

popup <- paste0("<b>", "País: ", "</b>", as.character(shape$nom_pais),"<br>")

# Funcion para crear el mapa
leaflet(shape)%>%
        addTiles("Esri.WorldTerrain")%>%
        # Funcion para agregar polígonos
        addPolygons(color = "#444444" ,
                    weight = 1, 
                    smoothFactor = 0.5,
                    opacity = 1.0,
                    fillOpacity = 0.5,
                    fillColor = ~palQuantile(shape$vweb_vd20),    # Color de llenado
                    layerId = ~shape$geo,                  
                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                        bringToFront = TRUE), #highlight cuando pasas el cursor
                    label = ~shape$nom_pais,                                  # etiqueta cuando pasas el cursor
                    labelOptions = labelOptions(direction = "auto"),
                    popup = popup) %>%                                        # mostrar el popup
        
        addLegend(position = "topright", pal = palQuantile, values = ~shape$vweb_vd20,
                  title = "%HITS Web")        


mapview::mapview(shape,labels=F)

library(tmap)
data("World")
