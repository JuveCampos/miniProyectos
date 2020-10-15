# Atributos y geometrias. 

# Librerias ----
library(tidyverse)
library(sf)
library(leaflet)

# Abrimos base de datos ----

# ¿Qué son los atributos? 
bd_atributos <- read_csv("https://raw.githubusercontent.com/JuveCampos/30DayMapChallenge2019/master/07.%20Red/EstadosTomate.csv")

# # Modificacion de variables
# bd_atributos$Estado <- str_replace_all(bd_atributos$Estado,
#                                c(
#                                  "MÉXICO" = "MEXICO",
#                                  "MICHOACÁN DE OCAMPO" = "MICHOACAN DE OCAMPO",
#                                  "NUEVO LEÓN" = "NUEVO LEON",
#                                  "QUERÉTARO" = "QUERETARO DE ARTEAGA",
#                                  "SAN LUIS POTOSÍ" = "SAN LUIS POTOSI",
#                                  "YUCATÁN" = "YUCATAN",
#                                  "CIUDAD DE MEXICO" = "CIUDAD DE MÉXICO"
#                                )
# )

# ¿Qué son las geometrías? 
bd_geometrias <- st_read("https://raw.githubusercontent.com/JuveCampos/MexicoSinIslas/master/Sin_islas.geojson")

# ¿Por que acá no usamos curl::download_data()? 

# ¿Cómo unir las geometrías? 
datos_mapa <- merge(x = bd_geometrias, 
      y = bd_atributos, 
      by.x = "ENTIDAD", 
      by.y = "Estado")

# Como checar que registros no estan de un lado y si del otro 
# bd_geometrias[!(bd_geometrias$ENTIDAD %in% bd_atributos$Estado),]

class(bd_geometrias)
class(bd_atributos)

# ¿Siempre vienen separadas? 
# No

# ¿Con la base "datos_mapa" ya podemos hacer un mapa? Si/no/porque
# Si

# ¿De los dos tipos de mapa (est/int) cuales podemos hacer? 
# Si

# Hagamos un mapa en ggplot2()
datos_mapa %>% 
  ggplot(aes(fill = Nombre)) + 
  geom_sf() + 
  scale_fill_manual(values = c("red", 
                               "orange", "pink")) + 
  theme_bw() + 
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank())

plotly::ggplotly()


# Qué pasó con el mapa?  Cómo lo corregimos? 
# Corrigiendo los estados que tienen acentos, en el excel de los atributos. 

# Cómo hacemos el mismo mapa en leaflet? Trabajo en Equipo. 

datos_mapa # Ya esta armada la base de datos
st_crs(datos_mapa) # Si esta en crs = 4326
# st_transform(datos_mapa, crs = 4326)


pal <- colorFactor(palette = c("red", 
                        "orange", "pink"), 
            domain = datos_mapa$Nombre)

leaflet(datos_mapa) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(label = paste0("Entidad: ", datos_mapa$ENTIDAD), 
              fillColor = pal(datos_mapa$Nombre), 
              fillOpacity = 1,
              opacity = 1,
              color = "red")


