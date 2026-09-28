##################################
# AVERAGE TEMPERATURE
##################################

rm(list = ls())
library(readxl)
library(dplyr)
library(dplyr)
library(lubridate)

cod_mun_and_name_98_municipalities <- read_excel("data/cod_mun_and_name_98_municipalities.xlsx")


# Crear data frame con las ciudades seleccionadas
ciudades <- data.frame(
  GID_2 = cod_mun_and_name_98_municipalities$guid_2,
  NAME_2 = cod_mun_and_name_98_municipalities$mun_name
)

# Ajustar los identificadores de ciudades según el formato de los archivos
ciudades$GID_2 <- gsub("COL\\.", "COL_v410_", ciudades$GID_2)
ciudades$GID_2 <- gsub("\\.", "_", ciudades$GID_2)

# Directorio base donde están las subcarpetas de datos
base_dir <- "data"

# ========================================
# FUNCIÓN ÚNICA
# ========================================

extraer_variables <- function(carpeta, nombre_var, municipios) {
  base_dir <- "data"
  fuente_dir <- file.path(base_dir, carpeta)
  
  # Lista para almacenar todos los dataframes
  lista_dfs <- list()
  
  # Iterar sobre cada municipio
  for(municipio in municipios) {
    # Patrón modificado para coincidencia exacta del nombre_var
    archivos <- list.files(fuente_dir, 
                           pattern = paste0("^", municipio, ".*_", nombre_var, "_.*\\d{4}_\\d{4}\\.csv$"), 
                           full.names = TRUE,
                           ignore.case = FALSE)
    
    if(length(archivos) > 0) {
      for(archivo in archivos) {
        df <- read.csv(archivo)
        
        # Extraer información del nombre del archivo
        nombre_archivo <- basename(archivo)
        partes <- unlist(strsplit(nombre_archivo, "_"))
        n <- length(partes)
        
        # Extraer periodo temporal (últimos números antes de .csv)
        periodo <- paste(partes[(n-3):(n-2)], collapse = "_")
        periodo <- gsub("\\.csv$", "", periodo)
        
        # Añadir columnas identificadoras
        df$municipio <- municipio
        df$variable <- nombre_var
        df$periodo <- periodo
        
        lista_dfs[[length(lista_dfs) + 1]] <- df
      }
    } else {
      warning(paste("No se encontraron archivos para el municipio:", municipio))
    }
  }
  
  # Combinar todos los dataframes
  if(length(lista_dfs) > 0) {
    df_final <- do.call(rbind, lista_dfs)
    return(df_final)
  } else {
    stop("No se encontraron archivos para ningún municipio")
  }
}

# Uso correcto de la función (ahora pasando municipios como argumento)
carpeta <- "ERA5_Land_temp"
nombre_var <- "2m_temperature"
municipios <- ciudades$GID_2

datos_combinados <- extraer_variables(carpeta, nombre_var, municipios)

#--------------------------------------------------------------------------------
#FILTRAR RESULTADOS
datos_def <- datos_combinados %>% 
  filter(Date >= "2007-01-01") %>%
  mutate(
    !!nombre_var := case_when(  # Usamos := y !! para evaluar nombre_var
      # 2007-2009
      Date >= "2007-01-01" & Date <= "2009-12-31" ~ ERA5_Land_pop_2005,
      # 2010-2014
      Date >= "2010-01-01" & Date <= "2014-12-31" ~ ERA5_Land_pop_2010,
      # 2015-2019
      Date >= "2015-01-01" & Date <= "2019-12-31" ~ ERA5_Land_pop_2015,
      # 2020-2023
      Date >= "2020-01-01" & Date <= "2023-12-31" ~ ERA5_Land_pop_2020,
      # Para cualquier otra fecha
      TRUE ~ NA_real_
    )
  ) %>% 
  select(Date, !!nombre_var, municipio, variable)  # Seleccionamos con el nuevo nombre


#--------------------------------------------------------------------------------
#GUARDAR DATOS

# Crear la ruta completa del archivo
ruta_guardado <- file.path("data_2007-2023_mundani", "datos_tempdaymin.rds")

# Asegurarse de que el directorio existe, si no, crearlo
if(!dir.exists("data_2007-2023_mundani")) {
  dir.create("data_2007-2023_mundani", recursive = TRUE)
}

# Guardar el dataframe en formato RDS
saveRDS(datos_def, file = ruta_guardado)



#--------------------------------------------------------------------------------
#LIMPIAR AMBIENTE
rm(list = setdiff(ls(), c("datos_def", "nombre_var")))


#--------------------------------------------------------------------------------
#PROMEDIO ANUAL

# Extraer el año de la columna Date y calcular promedios anuales
datos_def_annual <- datos_def %>%
  mutate(anio = year(as.Date(Date))) 

# Convertir el nombre de la variable a símbolo
nombre_sym <- sym(nombre_var)

# Calcular promedio anual por municipio
annual_mean <- datos_def_annual %>%
  group_by(anio, municipio) %>%
  summarise(annual_mean = mean(!!nombre_sym, na.rm = TRUE), .groups = "drop")


# Crear nombre del archivo de salida
output_file <- paste0("annual_mean_", nombre_var, ".rds")
output_path <- file.path("data_2007-2023_mundani", output_file)
# Guardar el archivo
saveRDS(annual_mean, file = output_path)
# Mensaje de confirmación
message(paste("Archivo guardado exitosamente en:", output_path))
