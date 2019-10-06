if (!require("pacman")) install.packages("pacman")
pacman::p_load(wordcloud2, tm, stringr)

# Leer datos 
data <- read.csv("db.csv") 
data$texto <- as.character(data$texto) %>% 
  str_replace_all(c("guason" = "guasón", 
                    "pelicula" = "película", 
                    "peli" = "película", 
                    "elguason" = "guasón", 
                    
                    ))
# Funcion para crear el wordcloud
# Nubes de palabras express
# Funcion para crear el wordcloud
create_wordcloud <- function(data, stop_words = c(), num_words = 100, background = "white",  mask = NULL) {
  # Checar si esta instalado Pacman
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(wordcloud2, tm, stringr)
  
  # Pre-Función para eliminar simbolos raros
  quitar_signos <- function(x)  stringr::str_remove_all(x, pattern = rebus::char_class("¿¡"))
  
  # If text is provided, convert it to a dataframe of word frequencies
  # Si se provee el texto, convertirlo a un dataframe de frecuencia de palabras 
  if (is.character(data)) {
    # Convertimos a Corpus
    corpus <- Corpus(VectorSource(data))
    # Convertimos el texto dentro del Corpus a Minusculas
    corpus <- tm_map(corpus, tolower)
    # Removemos la puntuacion (.,-!?)
    corpus <- tm_map(corpus, removePunctuation)
    # Removemos los numeros
    corpus <- tm_map(corpus, removeNumbers)
    # Removemos los signos de admiracion e interrogacion al reves
    corpus <- tm_map(corpus, quitar_signos)    
    # Removemos las stopwords (palabras muy muy comunes que se usan para dar coherencia
    # a las oraciones. Para saber cuales, escribir: stopwords("spanish))
    corpus <- tm_map(corpus, removeWords, c(stopwords("spanish"), stop_words))
    # Generamos una matriz para hacer el conteo
    tdm <- as.matrix(TermDocumentMatrix(corpus))
    # Obtenemos el numero de la frecuencia de cada palabra
    data <- sort(rowSums(tdm), decreasing = TRUE)
    # Generamos una tabla con la palabra en una columna y su frecuencia de uso en otra 
    data <- data.frame(word = names(data), freq = as.numeric(data))
  }
  
  freq_palabras <<- data
  
  # Make sure a proper num_words is provided
  # Nos aseguramos que un numero adecuado de palabras `num_provider` es generado`
  if (!is.numeric(num_words) || num_words < 3) {
    num_words <- 3
  }  
  
  # Grab the top n most common words
  # Recortamos la base de datos de palabras a un numero `n` especificado
  data <- head(data, n = num_words)
  if (nrow(data) == 0) {
    return(NULL)
  }
  wordcloud2(data, backgroundColor = background, color = "random-dark", fontFamily = "Asap", size = 2) 
}

# function(data, stop_words = c(), num_words = 100, background = "white",  mask = NULL)
create_wordcloud(data = data$texto %>% as.character(), num_words = 500, stop_words = c("this", "guasonguaido",
                                                                                       "...",
                                                                                       # "joker",
                                                                                       "more",
                                                                                        "guason",
                                                                                       # "guasón",
                                                                                       # "joaquinphoenix",
                                                                                        "phoenix",
                                                                                       # "película",
                                                                                       "retweeted",
                                                                                       "tras",
                                                                                       "added",
                                                                                       "verified", "account",
                                                                                       "replying",
                                                                                       "carlos",
                                                                                       "learn",
                                                                                       "contain",
                                                                                       "thread",
                                                                                       "https:",
                                                                                       "sensitive"
))


# Exploración de la base 
library(rebus)
detectar <- function(x = data$texto %>% tolower(), pattern) str_detect(x, pattern)
data$texto[detectar(pattern = or1("bromas"))] %>% as.character() %>% length()
data$texto[detectar(pattern = or1("ledger"))] %>% as.character() 
data$texto[detectar(pattern = or1("oscar"))] %>% as.character() 
data$texto[detectar(pattern = or1("maestra"))] %>% as.character() 


