# Mapas Precipitacion

# Librerias y funciones
library(sf)          # Para leer informacion geografica
library(tidyverse)   # Para manejar datos. 
library(leaflet)     # Para hacer mapas interactivos
library(stringr)     # Manejo de Texto
library(rebus)       # Expresiones regulares
niveles <- function(x) levels(as.factor(x))

# Obtenemos la lista de nombres
files <- getwd() %>% list.files()

meses <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
           "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

# Archivos de poligonos de lluvia (en desorden porque el Sistema los ordena de manera alfab'etica)
files_prec      <- files[str_detect(files, pattern = "Pol_Precipitacion")][c(4,5,8,1,9,7,6,2,12,11,10,3)]

# Leemos los archivos...
prec     <- lapply(files_prec, st_read)

# Nos quedamos solo con CDMX
edos <- st_read("https://github.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/raw/master/Zona%20Metropolitana/EstadosZMVM.geojson", quiet = TRUE) 
edos <- edos[3,] 
class(edos)

# Paleta de lluvias (de 0 a 325 mm de lluvia, lo maximo registrado en la CDMX)
paleta_lluvias <- colorNumeric('Blues', domain = 0:325)

################
# PARA LLUVIAS #
###############

# Generamos un ciclo que nos elabore las 12 imagenes de los mapas de lluvias 
for (i in 1:12){
  e <- leaflet(prec[[i]]
  ) %>%
    addProviderTiles("CartoDB.Positron") %>% 
    addPolygons(color = "#444444" ,
                weight = 0.3, 
                smoothFactor = 0.5,
                opacity = 1.0,
                fillOpacity = 0.5,
                fillColor = ~paleta_lluvias(prec[[i]]$MAGNI_NUM
                ),
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                    bringToFront = TRUE),
                label = ~paste0(prec[[i]]$MAGNI_NUM
                ),
                labelOptions = labelOptions(direction = "auto")) %>%
    addPolygons(data = edos, color = 'black', weight = 3, fill = F) %>%
    addLegend(position = "bottomright", pal = paleta_lluvias, values = 0:325, labFormat = labelFormat(suffix = " mm"),
              title = paste0("<h1>", 
                             meses[i], 
                             "</h1>", "Precipitación mensual"))
  mapview::mapshot(e, file = paste0(meses[i], ".png"))
}
