# Mapas Precipitacion

# Librerias y funciones
if (!require("pacman")) install.packages("pacman")
p_load(sf, tidyverse, leaflet, stringr, rebus, mapview)

# library(sf)          # Para leer informacion geografica
# library(tidyverse)   # Para manejar datos. 
# library(leaflet)     # Para hacer mapas interactivos
# library(stringr)     # Manejo de Texto
# library(rebus)       # Expresiones regulares

# Obtenemos la lista de nombres
files <- list.files("Bases de datos")

meses <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
           "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", 
           "Diciembre")

# Archivos de poligonos de lluvia (en desorden porque el Sistema los ordena de manera alfab'etica)
files_prec      <- paste0("Bases de datos/", files)
  
# Leemos los archivos...
prec     <- lapply(files_prec, st_read)

# Nos quedamos solo con CDMX
edos <- st_read("https://github.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/raw/master/Zona%20Metropolitana/EstadosZMVM.geojson", quiet = TRUE) 
edos <- edos[3,] 
class(edos) # Deben ser del tipo "sf"   "data.frame"

# Paleta de lluvias (de 0 a 325 mm de lluvia, lo maximo registrado en la CDMX)
paleta_lluvias <- colorNumeric('Blues', domain = 0:325)
leaflet::previewColors(paleta_lluvias, values = prec[[7]]$MAGNI_NUM) # ver muestra de como se ve la paleta

################
# PARA LLUVIAS #
###############

# Generamos un ciclo que nos elabore las 12 imagenes de los mapas de lluvias 
for (i in 1:12){
  
  # Creamos el mapa
  e <- leaflet(prec[[i]]) %>%
    addProviderTiles("CartoDB.Positron") %>%  # añadimos tiles (fondo)
    addPolygons(color = "#444444" ,           # Color de linea de los poligonos
                weight = 0.3,                 # grosor de linea
                smoothFactor = 0.5,
                opacity = 1.0,
                fillOpacity = 0.5,            # Opacidad de relleno
                fillColor = ~paleta_lluvias(prec[[i]]$MAGNI_NUM), # Color de relleno
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                    bringToFront = TRUE), # Opcion de resaltado cuando ocurre un hover
                label = ~paste0(prec[[i]]$MAGNI_NUM),                     # Etiqueta Hover
                labelOptions = labelOptions(direction = "auto")) %>%
    addPolygons(data = edos, color = 'black', weight = 3, fill = F) %>%   # Añadimos poligono de contorno 
    addLegend(position = "bottomright", pal = paleta_lluvias, values = 0:325, labFormat = labelFormat(suffix = " mm"),
              title = paste0("<h1>",meses[i],"</h1>", "Precipitación mensual")) # Leyenda de colores
  
  mapview::mapshot(e, file = paste0(meses[i], ".png"))            # Imprimimos la imagen en un *.png   
}
