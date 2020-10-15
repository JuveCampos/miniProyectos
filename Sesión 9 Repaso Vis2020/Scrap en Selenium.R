# Scrap de Datos de viaje
Sys.setlocale("LC_ALL", 'en_US.UTF-8')

# Inicio del Selenium ----
library("RSelenium")
rD <- rsDriver(verbose = FALSE, browser = "firefox")
rD
remDr <- rD$client

# Url de prueba
# url <- "https://www.despegar.com.mx/shop/flights/results/oneway/MEX/TYO/2020-12-01/1/0/0/NA/NA/NA/NA/?from=SB&di=1-0"

# Me creo un vector de fechas. 
fechas <- paste0("2020-12-", c(paste0("0", 1:9), 10:31))

# Creamos nuestra bolsita vacía
datos_final <- data.frame()

fecha <- fechas[1]

# fecha <- fechas[1]
for (fecha in fechas){

  # Creamos la url
  url <- paste0("https://www.despegar.com.mx/shop/flights/results/oneway/MEX/TYO/", 
                fecha,
                "/1/0/0/NA/NA/NA/NA/?from=SB&di=1-0")
  
  # Vamos a la pagina seleccionada
  remDr$navigate(url)
  Sys.sleep(6)
  webElem2 <- remDr$findElements("class", "price-amount")
  data <- unlist(lapply(webElem2, function(x){x$getElementText()}))
  data <- str_remove(data, pattern = ",") %>% as.numeric()
  tabla <- tibble(precio = data, 
                  fecha = fecha)
  
  # Juntamos todo en una tabla    
  datos_final <- rbind.data.frame(datos_final, tabla)
  
}

# Cerramos el Selenium para que no se queme la compu
remDr$close()

# Visualizamos los datos: 
datos_final

# Hacemos graficas: 
datos_final %>% 
  ggplot(aes(precio)) + 
  geom_density()

datos_final %>% 
  mutate(fecha = as.Date(fecha, "%Y-%m-%d")) %>% 
  group_by(fecha) %>% 
  summarise(promedio_precio = min(precio)) %>% 
  ggplot(aes(x = fecha,
             y = promedio_precio)) + 
  geom_col() + 
  geom_text(aes(label = round(promedio_precio, 2)), 
            vjust = -0.001)

