# Informacion meteorologica disponible en la Ciudad de México

# Librerias y funciones
if (!require("pacman")) install.packages("pacman")
pacman::p_load(stringr, 
               rebus, 
               dplyr, 
               ggplot2, 
               lubridate)
niveles <- function(x) levels(as.factor(x))
source("https://raw.githubusercontent.com/JuveCampos/DataEng/master/R/NormalClimToDataFrame.R")

# Catalogo de estaciones
catalogo <- c(9002,  9003,  9004,  9005,  9006,  9007,  9009,  9010,  9011,  9012,  9013,  9014,  9015,  9016,  9017, 
  9019,  9020,  9021,  9022,  9023,  9024,  9025,  9026,  9028,  9029,  9030,  9031,  9032,  9033,  9034, 
  9036,  9037,  9038,  9039,  9040,  9041,  9042,  9043,  9044,  9045,  9046,  9047,  9048,  9049,  9050, 
  9052,  9054,  9055,  9056,  9058,  9059,  9062,  9064,  9067,  9068,  9069,  9070,  9071)            

# Ejemplo 1 #
cat <- 9002
url <-  paste0("https://smn.cna.gob.mx/tools/RESOURCES/Diarios/",
               as.character(cat), ".txt")
datos_meteo_cdmx <- procesa_datos_diarios(url) %>% mutate(Estacion = as.character(cat))

# Datos de registros diarios
anios_inf <- datos_meteo_cdmx[-1] %>%
  group_by(lubridate::year(FECHA)) %>%
  summarise(dias = n())

# Realizamos graficas
plot_anual(datos_meteo_cdmx, paste(id_y_nombre_estacion[1], id_y_nombre_estacion[2]))
plot_mensuales(datos_meteo_cdmx, paste(id_y_nombre_estacion[1], id_y_nombre_estacion[2]))
plot_diarias(datos_meteo_cdmx, paste(id_y_nombre_estacion[1], id_y_nombre_estacion[2]))

# Ciclo 
for (cat in catalogo){
  url <-  paste0("https://smn.cna.gob.mx/tools/RESOURCES/Diarios/",
                                            as.character(cat), ".txt")
  tryCatch({
    if(RCurl::url.exists(url)){
      datos_meteo_cdmx <- rbind(datos_meteo_cdmx, procesa_datos_diarios(url) %>% mutate(Estacion = as.character(cat)))
    } 
  }, error = function(e){
    print(paste0("Error en ", as.character(cat)))
  })
  
}

# Exploramos los datos
levels(as.factor(datos_meteo_cdmx$Estacion))

# Guardamos los datos 
write.csv(datos_meteo_cdmx, "Datos_Meteorologicos.csv", fileEncoding = 'macintosh')

############
# Graficos #
############

# Creamos la grafica
#################################################################################
ggplot(data = datos_meteo_cdmx, mapping = aes(x = FECHA, y = Estacion)) +
  geom_point(colour = "red") + xlab("") +
  labs(title = "Información meteorológica disponible en la CDMX", 
       caption = "FUENTE: SMN - CONAGUA. Registros Diarios meteorológicos disponibles en febrero del 2019.\n https://smn.cna.gob.mx/es/climatologia/informacion-climatologica/informacion-estadistica-climatologica"
  ) + 
  scale_x_date(date_labels = "%Y", 
               date_breaks = "10 year") 
#################################################################################
