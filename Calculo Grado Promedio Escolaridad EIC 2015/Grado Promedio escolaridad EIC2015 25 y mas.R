# Calculo grado promedio de escolaridad a partir de Microdatos de INEGI
# Jorge Juvenal Campos Ferreira
# Datos obtenidos de: https://www.inegi.org.mx/programas/intercensal/2015/default.html#Microdatos

# Librerias ----
library(tidyverse)
library(survey)

# Chiapas ----
bd <- read_csv("eic2015_07_csv/TR_PERSONA07.CSV") # Chiapas

b <- bd %>% 
  filter(ESCOACUM != 99) %>% 
  filter(EDAD >= 25 & EDAD != 999) 

sum(b$ESCOACUM * b$FACTOR)/sum(b$FACTOR)


# Tabasco ----

bd <- read_csv("eic2015_27_csv/TR_PERSONA27.CSV") # Tabasco

b <- bd %>% 
  filter(ESCOACUM != 99) %>% 
  filter(EDAD >= 25 & EDAD != 999) 

sum(b$ESCOACUM * b$FACTOR)/sum(b$FACTOR)

svy_design <- svydesign(id = b$ID_PERSONA, 
                        weights = b$FACTOR, 
                        strata = b$ESTRATO)

survey::svymean(x = b$ESCOACUM, design = svy_design)
