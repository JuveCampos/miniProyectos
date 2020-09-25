# Descarga datos conagua
library(tidyverse)


"https://smn.conagua.gob.mx/tools/RESOURCES/Diarios/9036.txt"
"https://smn.conagua.gob.mx/tools/RESOURCES/Diarios/9068.txt"

estacion <- "06"

bd <- read_table2(paste0("https://smn.conagua.gob.mx/tools/RESOURCES/Diarios/90", estacion ,".txt"), 
                     locale = locale(encoding = "ISO-8859-1"), 
                     skip = 18)

write.csv(bd, paste0("01_Datos/Datos Conagua/90",estacion,".csv"))


c(paste0("0", 1:9), 10:99)

for(estacion in c(paste0("0", 1:9), 10:99)){
  print(estacion)
  tryCatch({
    
  bd <- read_table2(paste0("https://smn.conagua.gob.mx/tools/RESOURCES/Diarios/90", estacion ,".txt"), 
                    locale = locale(encoding = "ISO-8859-1"), 
                    skip = 18)
  
  write.csv(bd, paste0("01_Datos/Datos Conagua/90",estacion,".csv"))
  }, 
  error = function(e){
    print(paste0("Algo tronó en el paso", estacion))
  }
  )
}

