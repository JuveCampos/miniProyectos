# Mapa Región Sur Sureste

# Librerias
library(tidyverse)
library(leaflet)
library(sf)

# Base de datos
mx <- st_read("https://raw.githubusercontent.com/JuveCampos/MexicoSinIslas/master/Sin_islas.geojson",
              quiet = T)
mx$ENTIDAD

# Region
rss <- c("CAMPECHE",
         "CHIAPAS",
         "GUERRERO",
         "OAXACA",
         "PUEBLA",
         "QUINTANA ROO",
         "TABASCO",
         "VERACRUZ DE IGNACIO DE LA LLAVE",
         "YUCATAN")

# Variable de región
mx$region <- ifelse(test = mx$ENTIDAD %in% rss,
                    yes = "Región Sur Sureste",
                    no = "Resto del país")

pal = colorFactor(palette = c("purple", "gray"),
                  domain = mx$region)

# Mapa
leaflet(mx) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(color = "white",
              dashArray = c(1,1),
              fillOpacity = 0.85,
              weight = 2,
              fillColor = pal(mx$region)) %>%
  addLegend(title = "Leyenda",
            position = "bottomright",
            colors = "purple",
            labels = "Región<br>Sur-Sureste"
            )


# Multiples estados
mx$edoMultiple <- case_when(mx$region == "Región Sur Sureste" ~ str_to_title(mx$ENTIDAD),
                            mx$region != "Región Sur Sureste" ~ "Resto del País")

# Paleta
library(wesanderson)
pal2 <- colorFactor(palette = c(
                    "#928cbd",  #Campeche
                    "#6d5f99",  #Chiapas
                    "#9f8f12",  #Guerrero
                    "#3691a4",  #Oaxaca
                    "#ccb22b",  #Puebla
                    "#27697b",  #QROO
                    "gray",     #Resto de estados
                    "#5faa85",  #Tabasco
                    "#87c6c6",  #Veracruz
                    "#c0a521"), #Yucatán
                   domain = mx$edoMultiple)

levels(as.factor(mx$edoMultiple))

# Mapa
leaflet(mx, options = leafletOptions(zoomControl = FALSE)) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(color = "white",
              dashArray = c(1,1),
              fillOpacity = 0.95,
              weight = 2,
              fillColor = pal2(mx$edoMultiple))
# %>%
#   addLegend(title = "Entidades<br>Región Sur Sureste",
#             position = "bottomright",
#             opacity = 1,
#             pal = pal2, values = mx$edoMultiple[mx$edoMultiple != "Resto del País"]
#             #colors = "purple",
#             #labels = "Región<br>Sur-Sureste"
#   )









