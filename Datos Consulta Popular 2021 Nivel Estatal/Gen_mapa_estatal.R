# Librerias: 
library(tidyverse)
library(sf)

# Base de datos: 
bd <- read_delim("20210802-2130_INE-CONSULTA-POPULAR-2021/20210802-2130_COMPUTOS-INE-CP2021.csv", 
                 "|", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "WINDOWS-1252"),
                 skip = 5) %>% 
  mutate(across(OPINION_SI:LISTA_NOMINAL_MRCP, as.numeric)) %>%
  mutate(SECCION_SEDE = as.numeric(SECCION_SEDE)) %>% 
  mutate(across(OPINION_SI:LISTA_NOMINAL_MRCP, as.numeric)) %>%
  group_by(ID_ENTIDAD, ENTIDAD) %>% 
  summarise(TOTAL_OPINIONES = sum(TOTAL_OPINIONES, na.rm = T), 
            LISTA_NOMINAL_MRCP = sum(LISTA_NOMINAL_MRCP, na.rm = T), 
            OPINION_SI = sum(OPINION_SI, na.rm = T), 
            OPINION_NO = sum(OPINION_NO, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(Porcentaje_participacion = 100*(TOTAL_OPINIONES/LISTA_NOMINAL_MRCP), 
         ENTIDAD = str_to_title(ENTIDAD)) 

bd
openxlsx::write.xlsx(bd, "examen/participacion_estatal.xlsx")

# Info Geografica: 
shape <- read_sf("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/DivisionEstatal.geojson") %>% 
  mutate(ENTIDAD = str_replace_all(ENTIDAD, c("Coahuila de Zaragoza" = "Coahuila",
                                              "Ciudad de México" = "Ciudad De México",
                                              # "Ciudad de México" = "Ciudad de México",
                                              "Michoacan de Ocampo" = "Michoacán",
                                              "Querétaro de Arteaga" = "Querétaro",
                                              "San Luís Potosi" = "San Luis Potosí",
                                              "Veracruz de Ignacio de La Llave" = "Veracruz"))) %>% 
  mutate(ID_ENTIDAD = as.numeric(CVE_EDO)) %>% 
  select(ID_ENTIDAD, ENTIDAD)

shape
st_write(shape, "examen/shape_estados.geojson")

# NOTAS DE PROCESAMIENTO: 
# bd$ENTIDAD[!(bd$ENTIDAD %in% shape$ENTIDAD)]
# shape$ENTIDAD[!(shape$ENTIDAD %in% bd$ENTIDAD)]
# # writeLines(str_c('"', shape$ENTIDAD[!(shape$ENTIDAD %in% bd$ENTIDAD)], '" = ""'))



