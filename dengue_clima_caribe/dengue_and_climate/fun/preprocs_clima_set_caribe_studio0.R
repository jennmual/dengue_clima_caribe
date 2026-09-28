#==================================================================
#
# DATA SET - CLIMATE VARIABLES
# Preprocesamiento desde data set de sally
# 20 noviembre 2024
#
#------------------------------------------
# Parte 0: Creación de set de variables climáticas por ciudad
#------------------------------------------
#
#==================================================================


rm(list=ls())

# ========================================
# CIUDADES SELECCIONADAS
# ========================================

# Crear data frame con las ciudades seleccionadas
ciudades <- data.frame(
  GID_2 = c("COL.4.2_2", "COL.6.9_2", "COL.20.26_2"),
  NAME_2 = c("Barranquilla", "Cartagena_de_Indias", "Santa_Marta")
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

# Crear la subcarpeta 'caribe_data' dentro de 'data'
caribe_data_dir <- file.path(base_dir, "caribe_data")
if (!dir.exists(caribe_data_dir)) {
  dir.create(caribe_data_dir, recursive = TRUE)
}

# ========================================
# FUNCIÓN ÚNICA
# ========================================
procesar_y_guardar_ciudad <- function(ciudad_id, ciudad_nombre, items, base_dir, output_dir) {
  # Lista para almacenar los dataframes combinados
  ciudad_data <- list()
  
  for (i in seq_len(nrow(items))) {
    fuente <- items$fuente[i]
    carpeta <- items$carpeta[i]
    variable <- items$variable[i]
    
    # Construir la ruta de la carpeta de la fuente
    fuente_dir <- file.path(base_dir, carpeta)
    
    # Buscar archivos que coincidan con el GID y la variable
    archivos <- list.files(fuente_dir, pattern = paste0(ciudad_id, "_", fuente, "_", variable, "_"), full.names = TRUE)
    
    # Leer y procesar cada archivo
    for (archivo in archivos) {
      # Leer el archivo
      data <- read.csv(archivo)
      
      # Renombrar las columnas agregando el nombre de la variable, excepto la primera columna
      colnames(data)[-1] <- paste0(variable, "_", colnames(data)[-1])
      
      # Agregar los datos a la lista de la ciudad
      ciudad_data[[length(ciudad_data) + 1]] <- data
    }
  }
  
  # Combinar todos los dataframes por la columna 'Date'
  if (length(ciudad_data) > 0) {
    combinado <- Reduce(function(x, y) merge(x, y, by = "Date", all = TRUE), ciudad_data)
    
    # Guardar el dataframe combinado en un único archivo por ciudad
    output_file <- file.path(output_dir, paste0(ciudad_nombre, "_clima_data_1950_2023.rds"))
    saveRDS(combinado, output_file)
    message("Archivo combinado guardado para ", ciudad_nombre, ": ", output_file)
  } else {
    message("No se encontraron datos para ", ciudad_nombre)
  }
}

# ========================================
# PROCESAR TODAS LAS CIUDADES
# ========================================
for (i in seq_len(nrow(ciudades))) {
  procesar_y_guardar_ciudad(ciudades$GID_2[i], ciudades$NAME_2[i], items_id, base_dir, caribe_data_dir)
}









