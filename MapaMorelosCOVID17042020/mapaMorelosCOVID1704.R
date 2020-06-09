#############################
### COVID-19 17 abril 2020 ##
#############################

# Conteo municipal y total estatal de casos con Resultado Positivo SARS-CoV-2

# Codigo elaborado por: Ami Gabriela Sosa Vera
# Modificado por Juvenal Campos el 23-4-2020

# Paquetes ----
library(pacman)
p_load(janitor, 
       tidyverse, 
       readr, 
       readxl, 
       dplyr, 
       ggplot2, 
       scales, 
       sf, 
       leaflet)

# Setup ----

Sys.setlocale("LC_ALL", "es_ES.UTF-8") 
options(scipen=999)

# Base de datos ----

# Datos Abiertos (SSA Federal)
curl::curl_download(url = "http://187.191.75.115/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip", 
                    destfile = "datosCovid.zip")

covid_17_04 <- read_csv("01_datos/23 abril/200423COVID19MEXICO.csv")

# Municipios de Hgo 
# catalogo_mun <- read_excel("01_datos/13abril2020/diccionario_datos_covid19/Catalogos_0412.xlsx", 
#                              sheet = "Catálogo MUNICIPIOS", range = "A466:C550", 
#                            col_names = c("clave_municipio", "MUNICIPIO", "clave_entidad"))

# MUNICIPIOS DE MORELOS 
catalogo_mun <- read_excel("01_datos/13abril2020/diccionario_datos_covid19/Catalogos_0412.xlsx", 
                           sheet = "Catálogo MUNICIPIOS") %>% 
  filter(CLAVE_ENTIDAD == "17") %>% 
  mutate(CVE_GEO = paste0(CLAVE_ENTIDAD, CLAVE_MUNICIPIO))


# Archivo geojson municipios Hgo

mapa_mor <- st_read("01_datos/muni_2016gw/muni_2016gw.shp") %>% 
  st_transform(crs = 4326) %>% 
  filter(CVE_ENT == 17)

morelosMuni <- st_read("01_datos/muni_2016gw/muni_2016gw.shp") %>% 
  st_transform(crs = 4326) %>% 
  filter(CVE_ENT == 17)

morelos <- st_read("01_datos/muni_2016gw/muni_2016gw.shp") %>% 
  st_transform(crs = 4326) %>% 
  filter(CVE_ENT == 17) %>% 
  st_union() %>% 
  st_as_sf()

plot(morelos, max.plot = 1)

# Juguemos ----

covid_17_04 %>% 
  group_by(ENTIDAD_RES) %>% 
  filter(RESULTADO == 1) %>% 
  count(RESULTADO) %>% 
  ungroup() %>% 
  summarise(total_casos = sum(n))

# 11,633 casos en total en el país

mor_dia1704 <- covid_17_04 %>% 
  filter(ENTIDAD_RES == 17 & RESULTADO == 1)

mor_dia1704 %>% 
  group_by(MUNICIPIO_RES) %>% 
  count(RESULTADO) %>% 
  ungroup() %>% 
  summarise(total_casos = sum(n))

# 75 casos confirmados COVID-19 en total en Hidalgo

# Numero de casos confirmados por municipio ----

casos_munimor <- merge(mor_dia1704, catalogo_mun, 
                       by.x="MUNICIPIO_RES", 
                       by.y = "CLAVE_MUNICIPIO") %>% 
  as_tibble()

# names(casos_munimor)
casos_munimor17 <- casos_munimor %>% 
  select(MUNICIPIO_RES, MUNICIPIO, RESULTADO, CVE_GEO) %>% 
  group_by(MUNICIPIO, CVE_GEO) %>% 
  count(RESULTADO) %>% 
  arrange(-n)

mapa_mor <- merge(mapa_mor, casos_munimor17, by.x = "CVEGEO", "CVE_GEO")

# casos_munimor17 

# Paletas

# RColorBrewer::display.brewer.all()

# Óptimos para variables numéricas (las primeras)
# Óptimos para categóricas (las segundas y terceras)

# Grafica de casos positivos por municipio ----

# casos_munimor17 %>% 
#   ggplot(aes(x = fct_reorder(MUNICIPIO, 
#                              -n),
#              y = n, fill = n)) +
#   geom_col() +
#   labs(title = "Casos confirmados de COVID-19 en Hidalgo",
#        subtitle = "CONTEO POR MUNICIPIO DE RESIDENCIA", 
#        x = NULL,
#        y = "Número de casos positivos",
#        caption = "Fuente: Datos Abiertos Secretaria de Salud\nElaboración propia. 17 Abril 2020") +
#   scale_x_discrete(labels = c("Pachuca de Soto", "Mineral de la Reforma", "Tulancingo de Bravo", "Tizayuca", "Emiliano Zapata", "Atotonilco de Tula", "San Agustin Tlaxiaca", "Tezontepec de Aldama", "Actopan", "Chilcuautla", "Epazoyucan", "Francisco I. Madero", "Huasca de Ocampo", "Huejutla de Reyes", "Mineral del Monte", "Santiago de Anaya", "Tepeapulco", "Tepeji del Río de Ocampo", "Tetepango", "Tula de Allende", "Zimapán")) +
#   scale_y_continuous(breaks = seq(0, 30, 5),
#                      limits = c(0, 30)) +
#   coord_flip() +
#   geom_text(aes(label = n), position=position_dodge(width=0.9), hjust = - 0.5, color = "black") +
#   theme_minimal()+
#   theme(legend.title = element_blank()) +
#   theme(panel.grid.minor = element_blank(), 
#         panel.grid.major = element_line(linetype = "solid"),
#         title = element_text(size = 12, hjust = 1, color = "black", face = "bold"),
#         plot.caption = element_text(size = 11, hjust = 1, color = "grey4", face = "italic"),
#         plot.subtitle = element_text(size = 11, color = "grey4", face = "plain"),
#         axis.text.y = element_text(size = 10, color = "grey4", face = "plain"), 
#         axis.title.x = element_text(size = 11, color = "grey4", face = "plain", hjust = 0.5)) +
#   scale_fill_distiller(palette = "YlGnBu", direction = 1) 


# Mapa municipios con casos confirmados ----

# Seleccion de municipios en el mapa

# casos_munimor17 %>% 
#   print(n = Inf)


# mapa_muni_mor4 <- mapa_mor %>%
#   mutate(corona= ifelse(NOMBRE %in% c("Pachuca de Soto", "Mineral de la Reforma", "Tulancingo de Bravo", "Tizayuca", "Emiliano Zapata", "Atotonilco de Tula", "San Agustín Tlaxiaca", "Tezontepec de Aldama", "Actopan", "Chilcuautla", "Epazoyucan", "Francisco I. Madero", "Huasca de Ocampo", "Huejutla de Reyes", "Mineral del Monte", "Santiago de Anaya", "Tepeapulco", "Tepeji del Río de Ocampo", "Tetepango", "Tula de Allende", "Zimapán"), 1, 0 ))

# Graficamos

# plot(mapa_muni_mor4, max.plot = 1)

# Colores

mapa_mor <- mapa_mor %>% 
  mutate(cat = case_when(n <= 10 ~ "De 1 a 10 casos", 
                         n >= 11 & n <= 20 ~ "de 11 a 20 casos", 
                         n > 20 ~ "Mayor a 20 casos<br>(46 casos, Cuernavaca)"))

palMuni <- colorFactor(palette = c("yellow", "orange", "red"), 
                       domain = mapa_mor$cat)


leaflet(mapa_mor, 
        options = leafletOptions(zoomControl = F)) %>% 
  addProviderTiles(leaflet::providers$CartoDB.Positron) %>% 
  addPolygons(data = morelos, fill = F, 
              weight = 3, color = "black", 
              opacity = 1) %>%
  addPolygons(data = morelosMuni, fill = F, 
              weight = 2, color = "black", 
              opacity = 1) %>% 
  addPolygons(color = "black",
              weight = 1, 
              fillColor = palMuni(mapa_mor$cat), 
              fillOpacity = 0.5) %>% 
  addLegend(title = "<b style = 'font-family: Poppins;'>Casos de Covid-19 por<br>Municipio del Estado<br>de Morelos (23/4/2020)</b>",
    position = "bottomleft", 
            pal = palMuni, 
            values = mapa_mor$cat,
            opacity = 1, 
    labFormat = labelFormat(suffix = "")) 

# Añadido 22 abril ---- 
# defunciones totales 

covid_17_04 %>% 
  filter(!is.na(FECHA_DEF)) %>% 
  filter(ENTIDAD_RES == 13 & RESULTADO == 1) %>% 
  count(FECHA_DEF) %>% 
  summarise(tot_def = sum(n))

# 11 def. tot. 
