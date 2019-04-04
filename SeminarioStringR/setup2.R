# Checar si esta instalado Pacman
if (!require("pacman")) install.packages("pacman")
pacman::p_load(sf, tidyverse, rebus, readr, plotly, leaflet)
#Sys.setlocale("LC_ALL", 'en_US.UTF-8')
# default_locale() # Verificar que sean 

# Funciones 
source("https://raw.githubusercontent.com/JuveCampos/miniProyectos/master/SeminarioStringR/create_fast_wordCloud.R")
niveles <- function(x) levels(as.factor(x))

# Leemos Bases de Datos 
noticias <- read_csv("https://github.com/JuveCampos/miniProyectos/raw/master/SeminarioStringR2/BasesDeDatos/Noticias_inundaciones.csv") %>% select(abstract, body)


