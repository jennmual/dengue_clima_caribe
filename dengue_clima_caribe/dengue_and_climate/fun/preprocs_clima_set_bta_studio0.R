#==================================================================
#
# DATA SET - CLIMATE VARIABLES
# Preprocesamiento desde data set de sally
# 13 diciembre 2024
#
#----------------------------------------------
# Parte 1: 
#---------------------------------------------
#
#==================================================================




rm(list=ls())

# Cargar las librerías necesarias
library(dplyr)
library(lubridate)


# ========================================
# CIUDADES SELECCIONADAS
# ========================================

# Crear data frame con las ciudades seleccionadas
ciudades <- data.frame(
  GID_2 = c("COL.5.1_2"),
  NAME_2 = c("Bogota_DC")
)

# Crear data frame con las fuentes y variables
items_id <- data.frame(
  carpeta = c("CHIRPS", "ERA5_Land_hum", "ERA5_Land_hum", "ERA5_Land_temp", "ERA5_Land_temp", "ERA5_Land_temp", "PERSIANN"),
  fuente = c("CHIRPS", "ERA5_Land", "ERA5_Land", "ERA5_Land", "ERA5_Land", "ERA5_Land", "PERSIANN_CDR"),
  variable = c("total_precipitation", "relative_humidity", "specific_humidity", "2m_temperature", "2m_temperature_daymin", "2m_temperature_daymax", "total_precipitation")
)

# Ajustar los identificadores de ciudades según el formato de los archivos
ciudades$GID_2 <- gsub("COL\\.", "COL_v410_", ciudades$GID_2)
ciudades$GID_2 <- gsub("\\.", "_", ciudades$GID_2)

# Directorio base donde están las subcarpetas de datos
base_dir <- "data"

# Crear la subcarpeta 'bta_data' dentro de 'data'
bta_data_dir <- file.path(base_dir, "bta_data")
if (!dir.exists(bta_data_dir)) {
  dir.create(bta_data_dir, recursive = TRUE)
}


# ========================================
# FUNCIÓN ÚNICA
# ========================================

procesar_y_guardar_ciudad <- function(ciudad_id, ciudad_nombre, items, base_dir, output_dir) {
  # Lista para almacenar las rutas de los datasets procesados
  datasets_generados <- list()
  
  # Iterar sobre cada fila del dataframe de items
  for (i in seq_len(nrow(items))) {
    fuente <- items$fuente[i]
    carpeta <- items$carpeta[i]
    variable <- items$variable[i]
    
    # Construir la ruta de la carpeta de la fuente
    fuente_dir <- file.path(base_dir, carpeta)
    
    # Buscar archivos que coincidan con el GID y la variable
    archivos <- list.files(fuente_dir, pattern = paste0(ciudad_id, "_", fuente, "_", variable, "_"), full.names = TRUE)
    
    # Lista para almacenar los dataframes de esta variable
    variable_data <- list()
    
    # Leer y procesar cada archivo
    for (archivo in archivos) {
      # Leer el archivo
      data <- read.csv(archivo)
      
      # Agregar los datos al listado de esta variable
      variable_data[[length(variable_data) + 1]] <- data
    }
    
    # Unir todos los dataframes de esta variable hacia abajo
    if (length(variable_data) > 0) {
      combinado <- do.call(rbind, variable_data)
      
      # Renombrar las columnas (excepto 'Date') agregando el nombre de la variable
      colnames(combinado)[-1] <- paste0(variable, "_", colnames(combinado)[-1])
      
      # Guardar el dataset combinado para esta variable
      output_file <- file.path(output_dir, paste0(ciudad_nombre, "_", fuente, "_", variable, "_data_1950_2023.rds"))
      saveRDS(combinado, output_file)
      
      # Agregar el dataset procesado a la lista
      datasets_generados[[length(datasets_generados) + 1]] <- combinado
      message("Archivo guardado para ", variable, " en ", ciudad_nombre, ": ", output_file)
    } else {
      message("No se encontraron datos para ", variable, " en ", ciudad_nombre)
    }
  }
  
  # Paso final: Combinar todas las bases generadas por la columna 'Date'
  if (length(datasets_generados) > 0) {
    combinado_final <- Reduce(function(x, y) merge(x, y, by = "Date", all = TRUE), datasets_generados)
    
    # Guardar el dataset combinado final
    output_file_final <- file.path(output_dir, paste0(ciudad_nombre, "_clima_data_combinado_1950_2023.rds"))
    saveRDS(combinado_final, output_file_final)
    message("Archivo combinado final guardado para ", ciudad_nombre, ": ", output_file_final)
  } else {
    message("No se generaron datasets para combinar en ", ciudad_nombre)
  }
}







# ========================================
# PROCESAR TODAS LAS CIUDADES
# ========================================
for (i in seq_len(nrow(ciudades))) {
  procesar_y_guardar_ciudad(ciudades$GID_2[i], ciudades$NAME_2[i], items_id, base_dir, bta_data_dir)
}


#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------

#Función 1:

guardar_dataset_por_fuente_variable <- function(ciudad_id, ciudad_nombre, items, base_dir, output_dir) {
  # Iterar sobre cada fila del dataframe de items
  for (i in seq_len(nrow(items))) {
    fuente <- items$fuente[i]
    carpeta <- items$carpeta[i]
    variable <- items$variable[i]
    
    # Construir la ruta de la carpeta de la fuente
    fuente_dir <- file.path(base_dir, carpeta)
    
    # Buscar archivos que coincidan con el GID y la variable
    archivos <- list.files(fuente_dir, pattern = paste0(ciudad_id, "_", fuente, "_", variable, "_"), full.names = TRUE)
    
    # Lista para almacenar los dataframes de esta variable
    variable_data <- list()
    
    # Leer y procesar cada archivo
    for (archivo in archivos) {
      # Leer el archivo
      data <- read.csv(archivo)
      
      # Agregar los datos al listado de esta variable
      variable_data[[length(variable_data) + 1]] <- data
    }
    
    # Unir todos los dataframes de esta variable hacia abajo
    if (length(variable_data) > 0) {
      combinado <- do.call(rbind, variable_data)
      
      # Renombrar las columnas (excepto 'Date') agregando el nombre de la variable
      colnames(combinado)[-1] <- paste0(variable, "_", colnames(combinado)[-1])
      
      # Guardar el dataset combinado para esta variable
      output_file <- file.path(output_dir, paste0(ciudad_nombre, "_", fuente, "_", variable, "_data_1950_2023.rds"))
      saveRDS(combinado, output_file)
      message("Archivo guardado para ", variable, " en ", ciudad_nombre, ": ", output_file)
    } else {
      message("No se encontraron datos para ", variable, " en ", ciudad_nombre)
    }
  }
}




#funcion 2:
juntar_datasets_por_fecha <- function(ciudad_nombre, output_dir) {
  # Obtener todos los archivos .rds generados en la carpeta de salida
  archivos_datasets <- list.files(output_dir, pattern = "_data_1950_2023.rds", full.names = TRUE)
  
  # Lista para almacenar los datasets cargados
  datasets_generados <- lapply(archivos_datasets, readRDS)
  
  # Paso final: Combinar todos los datasets generados por la columna 'Date'
  if (length(datasets_generados) > 0) {
    combinado_final <- Reduce(function(x, y) merge(x, y, by = "Date", all = TRUE), datasets_generados)
    
    # Guardar el dataset combinado final
    output_file_final <- file.path(output_dir, paste0(ciudad_nombre, "_clima_data_combinado_1950_2023.rds"))
    saveRDS(combinado_final, output_file_final)
    message("Archivo combinado final guardado para ", ciudad_nombre, ": ", output_file_final)
  } else {
    message("No se generaron datasets para combinar en ", ciudad_nombre)
  }
}


#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------










##################
# BOGOTÁ CREADA
##################

Bogota_DC_clima_data_1950_2023 <- readRDS("data/bta_data/Bogota_DC_clima_data_combinado_1950_2023.rds")
dataset_clima=Bogota_DC_clima_data_1950_2023

# Seleccionar variable según fecha
dataset_clima$Date <- as.Date(dataset_clima$Date)
dataset_clima$anio <- as.numeric(format(dataset_clima$Date, "%Y"))

# Filtrar las filas cuyo año sea mayor o igual a 2008
dataset_clima <- dataset_clima[dataset_clima$anio >= 2003, ]

# Crear las nuevas variables
dataset_clima$`2m_temperature_daymax` <- with(dataset_clima, 
                                              ifelse(anio < 2010, `2m_temperature_daymax_ERA5_Land_pop_2005`,
                                                     ifelse(anio >= 2010 & anio <= 2014, `2m_temperature_daymax_ERA5_Land_pop_2010`,
                                                            ifelse(anio >= 2015 & anio <= 2019, `2m_temperature_daymax_ERA5_Land_pop_2015`,
                                                                   `2m_temperature_daymax_ERA5_Land_pop_2020`))))

dataset_clima$`2m_temperature_daymin` <- with(dataset_clima, 
                                              ifelse(anio < 2010, `2m_temperature_daymin_ERA5_Land_pop_2005`,
                                                     ifelse(anio >= 2010 & anio <= 2014, `2m_temperature_daymin_ERA5_Land_pop_2010`,
                                                            ifelse(anio >= 2015 & anio <= 2019, `2m_temperature_daymin_ERA5_Land_pop_2015`,
                                                                   `2m_temperature_daymin_ERA5_Land_pop_2020`))))

dataset_clima$`2m_temperature` <- with(dataset_clima, 
                                       ifelse(anio < 2010, `2m_temperature_ERA5_Land_pop_2005`,
                                              ifelse(anio >= 2010 & anio <= 2014, `2m_temperature_ERA5_Land_pop_2010`,
                                                     ifelse(anio >= 2015 & anio <= 2019, `2m_temperature_ERA5_Land_pop_2015`,
                                                            `2m_temperature_ERA5_Land_pop_2020`))))

dataset_clima$specific_humidity <- with(dataset_clima, 
                                        ifelse(anio < 2010, `specific_humidity_ERA5_Land_pop_2005`,
                                               ifelse(anio >= 2010 & anio <= 2014, `specific_humidity_ERA5_Land_pop_2010`,
                                                      ifelse(anio >= 2015 & anio <= 2019, `specific_humidity_ERA5_Land_pop_2015`,
                                                             `specific_humidity_ERA5_Land_pop_2020`))))

dataset_clima$relative_humidity <- with(dataset_clima, 
                                        ifelse(anio < 2010, `relative_humidity_ERA5_Land_pop_2005`,
                                               ifelse(anio >= 2010 & anio <= 2014, `relative_humidity_ERA5_Land_pop_2010`,
                                                      ifelse(anio >= 2015 & anio <= 2019, `relative_humidity_ERA5_Land_pop_2015`,
                                                             `relative_humidity_ERA5_Land_pop_2020`))))

dataset_clima$total_precipitation <- with(dataset_clima, 
                                          ifelse(anio < 2010, `total_precipitation_PERSIANN_CDR_pop_2005`,
                                                 ifelse(anio >= 2010 & anio <= 2014, `total_precipitation_PERSIANN_CDR_pop_2010`,
                                                        ifelse(anio >= 2015 & anio <= 2019, `total_precipitation_PERSIANN_CDR_pop_2015`,
                                                               `total_precipitation_PERSIANN_CDR_pop_2020`))))



# Crear un nuevo dataframe con solo la variable Date y las nuevas variables
dataset_clima_2003_2023 <- dataset_clima[, c("Date","2m_temperature_daymax", "2m_temperature_daymin", "2m_temperature", "specific_humidity","relative_humidity","total_precipitation")]
# Cambiar el nombre de la ciudad y guardar el nuevo dataframe
saveRDS(bta_clima_2003_2023_semanal, file = "data/bta_data/Bogota_DC_clima_data_1950_2023.rds")

