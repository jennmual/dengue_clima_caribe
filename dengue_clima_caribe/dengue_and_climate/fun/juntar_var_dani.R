##################################
# AVERAGE TEMPERATURE DAILY MIN
##################################

library(readxl)
library(dplyr)
library(purrr)
library(lubridate)


rm(list = ls())


#---------------------------------------------------
#CARGAR ARCHIVOS

#Define la función procesar_archivo
procesar_archivo <- function(ruta) {
  nombre_var <- gsub("^annual_mean_(.*)\\.rds$", "\\1", basename(ruta))
  datos <- readRDS(ruta) %>%
    select(municipio, anio, value = contains("mean")) %>%
    rename(!!nombre_var := value)
  return(datos)
}

# Busca los archivos .rds en tu directorio
archivos_rds <- list.files("data_2007-2023_mundani", 
                           pattern = "^annual_mean_.*\\.rds$", 
                           full.names = TRUE)

# Llama a map_dfr para aplicar procesar_archivo a todos los archivos y combinarlos
datos_combinados <- map_dfr(archivos_rds, procesar_archivo)


#---------------------------------------------------
#JUNTAR LOS ARCHIVOS EN UN SOLO DATA SET
datos_finales <- datos_combinados %>%
  group_by(municipio, anio) %>%
  summarise(across(everything(), ~ mean(., na.rm = TRUE)), .groups = "drop")



#---------------------------------------------------------
#TRAER LOS NOMBRES Y CÓDIGOS DE LOS MUNICIPIOS
c_dani_mun <- read_excel("data/cod_mun_and_name_98_municipalities.xlsx") %>%
  select(guid_2, cod_mun, mun_name) %>%
  mutate(guid_2 = ifelse(
    grepl("^COL\\.\\d+\\.\\d+_\\d+$", guid_2),
    sapply(strsplit(guid_2, "\\."), function(x) {
      parts <- unlist(strsplit(x[3], "_"))
      paste0("COL_v410_", x[2], "_", parts[1], "_", parts[2])
    }),
    guid_2
  ))

var_clima_mun <- datos_finales %>%
  left_join(c_dani_mun, by = c("municipio" = "guid_2")) 


#-------------------------------------------------
#GUARDAR ARCHIVO

saveRDS(var_clima_mun, file = "data_2007-2023_mundani/var_clima_mun.rds")  


