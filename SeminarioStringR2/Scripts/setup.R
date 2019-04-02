# Checar si esta instalado Pacman
if (!require("pacman")) install.packages("pacman")
pacman::p_load(sf, tidyverse, rebus, readr)
Sys.setlocale("LC_ALL", 'en_US.UTF-8')
# default_locale() # Verificar que sean 

# Funciones 
source("https://raw.githubusercontent.com/JuveCampos/miniProyectos/master/SeminarioStringR/create_fast_wordCloud.R")
niveles <- function(x) levels(as.factor(x))

# Leemos Bases de Datos 
tweets_amlo <- readRDS("BasesDeDatos/datos_tweet_amlo.rds")
ernesto <- read.delim("BasesDeDatos/importance-of-being-earnest.txt")
mapa <- st_read("BasesDeDatos/presas_cdmx.geojson", quiet = T)
noticias <- read_csv("BasesDeDatos/Noticias_inundaciones.csv") %>% select(abstract, body)
