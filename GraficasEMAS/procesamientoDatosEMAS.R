##################################################################
# Plot Series de tiempo de Estaciones Meteorologicas Automaticas #
##################################################################
Sys.setlocale("LC_ALL", 'en_US.UTF-8')

# Codigo valido solo para estaciones con 10 variables (falta generalizar un poco más).
# Caso: Estacion C. Biologicas IPN (Más céntrica al centro de la CDMX disponible).

# Librerias y funciones
library(tidyverse)
library(stringr)
library(rebus)
captura <- function(x) capture(one_or_more(x))

# Leemos datos
datos <- read.delim("https://smn.cna.gob.mx/tools/GUI/EMAS_reporte_v2.php?id=212&formato=txt&tipo=90d", encoding = "UTF-8") %>% 
  mutate(Servicio.Meteorológico.Nacional = as.character(Servicio.Meteorológico.Nacional))

# pattern <- or1(c("Altitud", "\\d+\\-\\d+\\-\\d+"))
# str_detect(datos$Servicio.Meteorológico.Nacional, pattern = pattern)

# grupo <- c(1)
# for (i in 2:length(str_detect(datos$Servicio.Meteorológico.Nacional, pattern = pattern))) {
#   if(str_detect(datos$Servicio.Meteorológico.Nacional, pattern = pattern)[i] == T)  grupo[i] <- grupo[i-1] +1
#   if(str_detect(datos$Servicio.Meteorológico.Nacional, pattern = pattern)[i] == F) grupo[i] <- grupo[i-1]
# }

# Guardamos informacion general de la EMA 
(datos_estacion <- datos[1:6,] %>% as.character())

# Guardamos el nombre de las variables meteorologicas
(variables <- datos[7:16,] %>% as.character()) # Son 10 Variables meteorologicas las que se registran

# Procesamos la informacion (Procesamiento 1)
## 1. Nos quedamos solo con las filas que nos dan informacion
## 2. Convertimos a caracter
## 3. Removemos espacios largos generados por el formato .txt
## 4. Removemos espacios al principio y al final del dato
datos2 <- datos[17:dim(datos)[1],] %>%   
  as.character() %>% 
  str_replace_all(pattern = captura(SPC), replacement = " ") %>% 
  str_remove_all(pattern = or1(c(START %R% SPC, SPC %R% END)))

# Procesamos la informacion (Procesamiento 2)
## 1. Convertimos toda la informacion (que esta en vector) a
##  una matriz de length(datos2)/10 columnas y 10 filas, y trasponemos la matriz.
datos2 <- matrix(datos2, ncol = length(datos2)/10, nrow = 10) %>% t() 

# Procesamos la informacion (Procesamiento 3)
## 1. Cambiamos el nombre a las variables
## 2. Convertimos la primera columna a formato POSXIT (Corrigiendo al huso horario del centro de México)
## 3. Convertimos las demas variables a caracter y luego a valores numericos 
datos3 <- as.data.frame(datos2) %>% reshape::rename(c(V1 = variables[1], 
                                                      V2 = variables[2],
                                                      V3 = variables[3],
                                                      V4 = variables[4],
                                                      V5 = variables[5],
                                                      V6 = variables[6],
                                                      V7 = variables[7],
                                                      V8 = variables[8],
                                                      V9 = variables[9],
                                                      V10 = variables[10]
                                                      ))
datos3[,1] <- strptime(datos3[,1] %>% as.character(), 
              format = "%Y-%m-%d %H:%M")  - 21600 # Le restamos las 6 horas de diferencia con el Meridiano de Greenwich
datos3[2:10] <- lapply(datos3[2:10], as.character)
datos3[2:10] <- lapply(datos3[2:10], as.numeric)
str(datos3) # Checamos estructura

# Preparacion de datos para graficar
datos3$dia <-   lubridate::day(datos3$`AAAA/MM/DD HH:MM HORAZ`)
datos3$mes <-   lubridate::month(datos3$`AAAA/MM/DD HH:MM HORAZ`)
datos3$fecha <- paste0(datos3$dia, "-", datos3$mes, "-2019") %>% as.Date(format = "%d-%m-%Y")

# Precipitacion diaria acumulada
precip90 <- datos3 %>% 
  group_by(fecha) %>% 
  summarise(precip_por_dia = sum(na.omit(`Precipitación (mm)`))) 

############
# Graficas #
###########

# Tema propio 
tema_juve <- theme_bw() + theme(text = element_text(family = "Asap-Bold", color = "#25636e"), 
                                panel.grid.major = element_blank(),
                                panel.grid.minor = element_blank(), 
                                plot.caption=element_text(hjust=1,size=9,colour="grey30"),
                                plot.subtitle=element_text(face="italic",size=12,colour="grey40"),
                                plot.title=element_text(size=18,face="bold"),
                                axis.text.x = element_text(family = "Asap-Bold", color = "#25636e"),
                                axis.text.y = element_text(family = "Asap-Bold", color = "#25636e"))

# Grafica de radiación solar
ggplot(data = datos3, aes(x = `AAAA/MM/DD HH:MM HORAZ`, y = `Radiación Solar (W/m²)`)) + 
  geom_line(colour = "#ff975b") + 
  labs(title = paste0("Gráfica de radiación solar por hora"), 
       subtitle = paste0(datos_estacion[2], " (",datos_estacion[1], ")"),
       x = "Fecha (MM/DD HH:MM, Año: 2019)", 
       y = "Radiación Solar", 
       caption = "Elaboración propia con datos del SMN-Conagua. 2019. Datos obtenidos el 25-04-2019") +
  tema_juve + 
  scale_x_datetime(labels = scales::date_format("%d/%m %H:%M"), date_breaks = "1 week") + 
  scale_y_continuous(labels = scales::dollar_format(prefix = "", suffix = " W/m\U00B2"))

# Grafica de humedad relativa
ggplot(data = datos3, aes(x = `AAAA/MM/DD HH:MM HORAZ`, y = datos3$`Humedad relativa (%)`)) + 
  geom_point(colour = "#00baaa") + 
  labs(title = paste0("Gráfica de humedad relativa"), 
       subtitle = paste0(datos_estacion[2], " (",datos_estacion[1], ")"),
       x = "Fecha (DD/MM HH:MM, Año: 2019)", 
       y = "Humedad Relativa (%)", 
       caption = "Elaboración propia con datos del SMN-Conagua. 2019. Datos obtenidos al 25-04-2019") +
  tema_juve + 
  scale_x_datetime(labels = scales::date_format("%d/%m %H:%M"), date_breaks = "1 week") + 
  scale_y_continuous(labels = scales::dollar_format(prefix = "", suffix = " %"))

# Grafica de temperatura del aire
ggplot(data = datos3, aes(x = `AAAA/MM/DD HH:MM HORAZ`, y = datos3$`Temperatura del Aire (°C)`)) +
  geom_point(colour = "#cbff5b") +
  labs(title = paste0("Gráfica de temperatura del aire"),
       subtitle = paste0(datos_estacion[2], " (",datos_estacion[1], ")"),
       x = "Fecha (AAAA/MM/DD HH:MM)",
       y = "Temperatura del aire (°C)",
       caption = "Elaboración propia con datos del SMN-Conagua. 2019. Datos obtenidos al 25-04-2019") +
  tema_juve +
  scale_x_datetime(labels = scales::date_format("%d/%m %H:%M"), date_breaks = "1 week") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "", suffix = " °C"))

# Grafica de presion atmosferica
ggplot(data = datos3, aes(x = `AAAA/MM/DD HH:MM HORAZ`, y = datos3$`   Presión Atmosférica   `)) +
  geom_point(colour = "gray") + 
  labs(title = paste0("Gráfica de presión atmosférica"), 
       subtitle = paste0(datos_estacion[2], " (",datos_estacion[1], ")"),
       x = "Fecha (MM/DD, Año: 2019)", 
       y = "Presión atmosférica en mmHg (milímetros de mercurio)", 
       caption = "Elaboración propia con datos del SMN-Conagua. 2019. Datos obtenidos al 25-04-2019") +
  tema_juve  +   
  scale_x_datetime(labels = scales::date_format("%d/%m"), date_breaks = "1 week")  + 
  scale_y_continuous(labels = scales::dollar_format(prefix = "", suffix = " mmHg"), expand = c(0,0))

# Grafica de precipitacion diaria ultimos 90 dias
ggplot(data = precip90, aes(x = fecha, y = precip_por_dia)) + 
  geom_bar(stat = 'identity', fill = "#006fba") + 
  labs(title = paste0("Gráfica de precipitación acumulada a nivel diario"), 
       subtitle = paste0(datos_estacion[2], " (",datos_estacion[1], ")"),
       x = "Fecha (MM/DD, Año: 2019)", 
       y = "Precipitación diaria acumulada", 
       caption = "Elaboración propia con datos del SMN-Conagua. 2019. Datos obtenidos al 25-04-2019") +
       tema_juve  +   
       scale_x_date(labels = scales::date_format("%d/%m"), date_breaks = "1 week")  + 
scale_y_continuous(labels = scales::dollar_format(prefix = "", suffix = " mm"), expand = c(0,0))

