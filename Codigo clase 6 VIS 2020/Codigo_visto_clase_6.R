# Descarga datos conagua #
# Ejemplo visto en clase
# Jorge Juvenal Campos Ferreira 
library(tidyverse)

# La página en la cual puedes encontrar los datos es: https://smn.conagua.gob.mx/es/climatologia/informacion-climatologica/informacion-estadistica-climatologica
# SI tienes dudas sobre como se ve una instalación para medir estos datos, puedes consultar: 
#  https://es.wikipedia.org/wiki/Estación_meteorológica

# Para consultar los datos de las estaciones, puedes ver estos dos ejemplos: 
"https://smn.conagua.gob.mx/tools/RESOURCES/Diarios/9036.txt" # Playa Caleta 454, Colonia Marte, Iztacalco, Ciudad de México.
"https://smn.conagua.gob.mx/tools/RESOURCES/Diarios/9068.txt" # Puente La llave, Del. Cuauhtemoc. CDMX

# Usamos primero la estación de Iztacalco para el caso n = 1
estacion <- "36"

# Código generado con el asistente de importación "Import Dataset" de RStudio. 
bd <- read_table2(paste0("https://smn.conagua.gob.mx/tools/RESOURCES/Diarios/90", estacion ,".txt"), 
                     locale = locale(encoding = "ISO-8859-1"), 
                     skip = 18)

# Nota! Esto funciona si tenemos, dentro de nuestro proyecto, una carpeta llamada "01/Datos" y dentro una llamada "Datos Conagua". 
# Si no, borra esa parte: 01_Datos/Datos Conagua/
write.csv(bd, paste0("01_Datos/Datos Conagua/90",estacion,".csv"))

# ¿Qué contiene esta secuencia? 
c(paste0("0", 1:9), 10:99)

# Si no sabes para que funciona tryCatch(), revisa el video o checa la ayuda: ?tryCatch
for(estacion in c(paste0("0", 1:9), 10:99)){

  # ¿Para que sirve esta linea? ¿Qué pasa si la quito?
  print(estacion)
  tryCatch({
    
  bd <- read_table2(paste0("https://smn.conagua.gob.mx/tools/RESOURCES/Diarios/90", estacion ,".txt"), 
                    locale = locale(encoding = "ISO-8859-1"), 
                    skip = 18)
  
  write.csv(bd, paste0("01_Datos/Datos Conagua/90",estacion,".csv"))
  }, 
  error = function(e){
    print(paste0("Algo tronó en el paso ", estacion, " T__T"))
  }
  )
}

