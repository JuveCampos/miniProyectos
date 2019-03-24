# 1. Librerias Necesarias
library(pacman)
p_load(rebus, stringr, tidyverse, htmltools, readr)

# Librerias
# library(rebus)
# library(stringr)
# library(tidyverse)

# Fijamos el Locale
# CHECAR QUE TODOS TENGAN ESTO!!
Sys.setlocale("LC_ALL", 'en_US.UTF-8')
default_locale()

# <locale>
#   Numbers:  123,456.78
# Formats:  %AD / %AT
# Timezone: UTC
# Encoding: UTF-8
# <date_names>
#   Days:   Sunday (Sun), Monday (Mon), Tuesday (Tue), Wednesday (Wed), Thursday (Thu), Friday (Fri),
# Saturday (Sat)
# Months: January (Jan), February (Feb), March (Mar), April (Apr), May (May), June (Jun), July
# (Jul), August (Aug), September (Sep), October (Oct), November (Nov), December
# (Dec)
# AM/PM:  AM/PM

# 1. Leemos los datos con readr 
datos <- readr::read_csv("Noticias_inundaciones.csv") 
head(datos)

# datos <- datos %>%
#   select(names(datos)[-8])
# 
# write_csv(datos, "Noticias_inundaciones.csv")

# 2. Vemos los datos 
View(datos)

# EXPLORACION DE LOS DATOS!
# 3. Elaboramos una funcion exploratoria y exploramos las variables
niveles <- function(x) levels(as.factor(x))
niveles(datos$periodico) # 13 periodicos
niveles(datos$format) # Solo HTML
niveles(datos$title) # 1053 titulos
niveles(datos$body) # Es codigo html! Lo podemos visualizar con el str_view()
# Ver el HTML
str_view(datos$body[1], pattern = " ")
niveles(datos$size)
niveles(datos$pages)
str_length(datos$body)

# 4. Tabla de Titulos Repetidos
datos$title[duplicated(datos$title)] %>% 
  as_data_frame() %>% 
  group_by(value) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

# Alternativa dulce
table(datos$title[duplicated(datos$title)])

# 1. PLAN DE LIMPIEZA DE LA BASE: 
#     PASO # 1... QUé queremos lograr? 
#      En este caso, queremos analizar los eventos de inundaciones que han ocurrido en los ultimos 
#      años. Para eso necesitamos limpiar la base... 

# 2. Limpieza: 
# 2.1. Limpiamos los tags de HTML de los encabezados! 

# Declaramos el patron con <regex>
captura <- function(x) capture(one_or_more(x))

###################
# P A T R O N E S #
###################

# ELIMINAMOS LOS TAGS #
patron_sencillo <- or1(c('<span style="color: red">','</span>'))
# <regex> (?:<span style="color: red">|</span>)
patron_complejo <- '<' %R% optional('/') %R% captura(WRD %R% optional(SPC) %R% optional(char_class("\":=!"))) %R% ">"
# regex> <([\w[\s]?[[:punct:]]?[[:=]]?]+)>
str_extract_all(datos$title, patron_sencillo) %>% unlist() %>% niveles() # Nos damos cuenta de que solo hay 2 niveles... pero y si hubiera mas?
str_extract_all(datos$title, patron_complejo) %>% unlist() %>% niveles() # Caso General
str_view_all(datos$title, pattern = patron_sencillo, match = T)
datos$title <- str_remove(datos$title, pattern = patron_complejo) # No elimino todo!!!
datos$title <- str_remove_all(datos$title, pattern = patron_complejo)

# Y en otras variables
str_extract_all(datos$abstract[1], pattern = patron_complejo)
str_extract_all(datos$body[1], pattern = patron_complejo)

####################################################################
# QUITAMOS LAS DIFERENCIAS ENTRE LOS UNIVERSALES Y LOS ECONOMISTAS #
####################################################################

# 1. Detectamos que celdas tienen Economista o El Universal o el EXCELSIOR como periodico Principal
niveles(datos$periodico)

# 2. Detectamos los patrones
pat_periodico <- or1(c("El Economista", "Excelsior", "El Universal"))
#<regex> (?:El Economista|Excelsior|El Universal)

# Approach: Creando una nueva variable!
str_view(datos$periodico, pat_periodico, match = T)
str_detect(datos$periodico, pattern = pat_periodico) %>% table() %>% prop.table()
datos$compania <- str_extract(datos$periodico, pattern = pat_periodico)
datos$compania[!str_detect(datos$periodico, pattern = pat_periodico)] <- datos$periodico[!str_detect(datos$periodico, pattern = pat_periodico)]
niveles(datos$compania)

# Approach: Eliminando todo lo que este despues del signo 
pat_guion <-  "-" %R% captura(WRD %R% SPC) %R% END
str_view(datos$periodico, pat_guion)
datos$periodico <- str_remove_all(datos$periodico, pattern = pat_guion)
niveles(datos$periodico) # Se repite el UNIVERSAL! Por que?
# Rehacemos el patron
pat_guion <-  optional(SPC) %R% "-" %R% captura(WRD %R% SPC) %R% END
datos$periodico <- str_remove_all(datos$periodico, pattern = pat_guion)
niveles(datos$periodico) # Se repite el UNIVERSAL! Por que?
# Y si en vez de DPA queremos volver a tener DPA-NEWS? 
datos$periodico <- str_replace_all(datos$periodico, pattern = "DPA", replacement = "DPA - Political News")
datos$periodico <- str_replace_all(datos$periodico, pattern = "Excelsior Online", replacement = "Excelsior")
niveles(datos$periodico)

# Visualizamos datos 
ggplot(data = datos, aes(x = periodico)) + geom_bar() 
#ggplot(data = datos, aes(x = periodico, fill = periodico)) + geom_bar() + theme_minimal() + scale_color_brewer("Blues")

########################################################################## 
# ELIMINAR NOTICIAS CHAFAS Y QUEDARME SOLO CON LAS QUE DICEN INUNDACION #
########################################################################## 

FREC <- datos$title %>% 
  as_data_frame() %>% 
  group_by(value) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

# Visualizamos
FREC

# E L I M I N A R 

# Generamos una variable con datos en Minusculas
datos$minus <- tolower(datos$title)
palabras <- c( "pronóstico del tiempo" , 
               "pronostico del tiempo", 
               "chiapas", 
               "veracruz", 
               "crême", 
               "hermanos fuentes", 
               "cardenas visita")

pat_noticias <- or1(palabras)
str_view_all(datos$minus, pattern = pat_noticias, match = T)
# Generamos dummy!
datos$noticia_inservible <-  str_detect(datos$minus, pattern = pat_noticias) 

datos <- datos %>% 
  filter(noticia_inservible != TRUE)

# Quedarme solo con las que dicen inundacion # T i d y 
noticias_inundacion <- datos %>%
  mutate(tiene_inundacion = str_detect(minus, pattern = "inundaci")) %>%
  filter(tiene_inundacion == TRUE) %>% 
  select(minus, tiene_inundacion)

# Inundacion o lluvias
noticias_inundacion_lluvias <- datos %>%
  mutate(tiene_inundacion = str_detect(minus, pattern = or1(c("inundaci","lluvia")))) %>%
  filter(tiene_inundacion == TRUE) %>% 
  select(minus, tiene_inundacion, body)

# LIMPIAMOS TAGS
noticias_inundacion_lluvias$body <- str_remove_all(noticias_inundacion_lluvias$body, pattern = patron_complejo)

# Creamos nube de palabras
source("https://raw.githubusercontent.com/JuveCampos/DataEng/master/R/create_fast_wordCloud.R")
create_wordcloud(noticias_inundacion_lluvias$body, stop_words = c("méxico", "ciudad", "así"), num_words = 200)
# Frecuencia palabras
freq_palabras
