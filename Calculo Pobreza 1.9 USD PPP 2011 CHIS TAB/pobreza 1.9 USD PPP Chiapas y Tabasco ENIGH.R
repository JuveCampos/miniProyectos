options(scipen = 999)
library(tidyverse)

linea_pobreza = 1.9 * 8.045 * 0.94

## Numeros 
# 1.9   - Ingreso al día en dolares, según la medida de Banco Mundial
# 8.045 - Valor PPP extraído de https://data.oecd.org/conversion/purchasing-power-parities-ppp.htm
# 0.94  - Valor de un dolar de 2014 en 2011

# Hogares (ENIGH, 2014)
hog <- read_csv("hogares_enigh2014tradicional/conjunto_de_datos/hogares.csv")
hog$foliohog <- paste0(hog$folioviv, hog$foliohog)
hog$edo = str_sub(string = hog$folioviv, start = 1L, end = 2L) 
hog <- hog %>% 
  select(edo, foliohog, factor_hog)

# Personas (ENIGH, 2014)
per <- read_csv("poblacion_enigh2014tradicional/conjunto_de_datos/poblacion.csv")
per$foliohog <- paste0(per$folioviv, per$foliohog)
per$edo = str_sub(string = per$folioviv, start = 1L, end = 2L)
per <- per %>% 
  group_by(edo, foliohog) %>% 
  summarise(personasHogar = n())

# Personas por hogar (ENIGH, 2014)
hog_per <- merge(hog, per, by = c("edo", "foliohog")) %>% 
  as_tibble()

ing <- read_csv("ingresos_enigh2014tradicional/conjunto_de_datos/ingresos.csv")
ing$edo = str_sub(string = ing$folioviv, start = 1L, end = 2L)
ing <- ing %>% 
  mutate(foliohog = paste0(folioviv, foliohog)) %>% 
  arrange(foliohog, numren) %>% 
  group_by(foliohog) %>% 
  summarise(ingtrihog = sum(ing_tri)/90) 

ingresos_hogar <- merge(hog_per, ing, by = "foliohog") %>% 
  as_tibble() %>% 
  mutate(ing_persona = ingtrihog/personasHogar) %>% 
  mutate(menor_linea = ing_persona < linea_pobreza) %>% 
  filter(edo %in% c("07", "27")) %>% 
  mutate(pob = personasHogar * factor_hog) %>% 
  group_by(edo, menor_linea) %>% 
  summarise(poblacion = sum(pob)) %>% 
  ungroup() %>% 
  group_by(edo) %>% 
  mutate(pp_pob = 100*poblacion/sum(poblacion))

# Porcentaje de la población
ingresos_hogar
table(ingresos_hogar$menor_linea)

