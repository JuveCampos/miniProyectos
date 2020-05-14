# Calculo del indicador de grado promedio de escolaridad.
options(scipen = 999)

# Librerias
library(tidyverse)
library(rebus)

# Calculamos el ponderador ----
# Leemos la BD
edad <- readxl::read_xlsx("PoblacionPorGruposDeEdad.xlsx", skip = 7)
# Nos quedamos con el renglon del pais
edad$pais[str_detect(edad$`País / Años`,pattern = "[A-Z]")] <-
  edad$`País / Años`[str_detect(edad$`País / Años`, pattern = "[A-Z]")]
# Rellenamos el renglon del pais
for (i in 1:length(edad$pais)){
  if(is.na(edad$pais[i])){
    edad$pais[i] <- edad$pais[i-1]
  }
}
# Limpieza de renglones vacios
edad <- edad %>%
  mutate(`Total edades` = as.numeric(`Total edades`)) %>%
  filter(!is.na(`Total edades`))

# Quitamos las columnas que no queremos
edad <- edad %>%
  select(-c(3,4,5,15:23))
names(edad)

for(i in 3:11){
  edad[,i] <- edad[,i] %>% pull() %>% as.numeric()
}

edad$pob_15_24 <- rowSums(edad[,3:4])
edad$pob_25_59 <- rowSums(edad[,5:11])
edad$pond_15_24 <- edad$pob_15_24/(edad$pob_15_24 + edad$pob_25_59)
edad$pond_25_59 <- edad$pob_25_59/(edad$pob_15_24 + edad$pob_25_59)

edad <- edad %>%
  select(`País / Años`, pais, pond_15_24, pond_25_59)

# Base de escolaridad promedio 15 a 24 ----
esc_15_24 <- readxl::read_xlsx("PromedioAñosEstudio15_24.xlsx",
                               skip = 7) %>%
  select(1,2) %>%
  mutate(Nacional = as.numeric(Nacional))

#
esc_15_24$pais[str_detect(esc_15_24$`País / Años`,pattern = "[A-Z]")] <-
  esc_15_24$`País / Años`[str_detect(esc_15_24$`País / Años`, pattern = "[A-Z]")]

# Rellenamos el renglon del pais
for (i in 1:length(esc_15_24$pais)){
  if(is.na(esc_15_24$pais[i])){
    esc_15_24$pais[i] <- esc_15_24$pais[i-1]
  }
}

esc_15_24$`País / Años`

# Limpieza de renglones vacios
esc_15_24 <- esc_15_24 %>%
  mutate(`País / Años` = as.numeric(str_remove(`País / Años`,
                                               pattern = capture(one_or_more(SPC))))) %>%
  filter(!is.na(`País / Años`)) %>%
  filter(!is.na(Nacional))


# Base de escolaridad promedio 25 a 29 ----
esc_25_59 <- readxl::read_xlsx("PromedioAñosEstudio25_59.xlsx",
                               skip = 7) %>%
  select(1,2)

#
esc_25_59$pais[str_detect(esc_25_59$`País / Años`,pattern = "[A-Z]")] <-
  esc_25_59$`País / Años`[str_detect(esc_25_59$`País / Años`, pattern = "[A-Z]")]

# Rellenamos el renglon del pais
for (i in 1:length(esc_25_59$pais)){
  if(is.na(esc_25_59$pais[i])){
    esc_25_59$pais[i] <- esc_25_59$pais[i-1]
  }
}

# Limpieza de renglones vacios
esc_25_59 <- esc_25_59 %>%
  mutate(`País / Años` = as.numeric(str_remove(`País / Años`,
                                               pattern = capture(one_or_more(SPC))))) %>%
  filter(!is.na(`País / Años`)) %>%
  filter(!is.na(Nacional...2)) %>%
  mutate(Nacional...2 = as.numeric(Nacional...2)) %>%
  filter(!is.na(Nacional...2)) %>%
  rename(Nacional = Nacional...2)

# Preparaciones para el merge
esc_15_24 <- esc_15_24 %>% rename(promedio_15_24 = Nacional, anio = `País / Años`)
esc_25_59 <- esc_25_59 %>% rename(promedio_25_59 = Nacional, anio = `País / Años`)
edad <- edad %>% rename(anio = `País / Años`) %>% mutate(anio = as.numeric(str_remove(anio,
                                                                                      pattern = capture(one_or_more(SPC)))))

# Mergeamos los datos
merge <- merge(esc_15_24, esc_25_59,
      by = c("anio", "pais")) %>%
  merge(edad, by = c("anio", "pais"),
        all.x = T,
        all.y = T)

# write.csv(merge, "grado_promedio.csv",
#           row.names = F,
#           na = "")

openxlsx::write.xlsx(merge, "grado_promedio.xlsx")
