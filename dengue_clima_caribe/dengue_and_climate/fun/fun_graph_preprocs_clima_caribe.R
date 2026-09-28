#==================================================================
#
# DATA SET - CLIMATE VARIABLES
# Preprocesamiento desde data set de sally
# 17 de enero de 2025
#
#------------------------------------------------
# Gráficas de las variables Satelitales vs IDEAM
#------------------------------------------------
#
#==================================================================

# Cargar las librerías necesarias
library(dplyr) 
library(lubridate)
library(readr)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(patchwork)


#======================================
# FUNCION PARA GRAFICA DE COMPARACIÓN
# VARIABLES CLIMÁTICAS POR FUENTE
#=====================================

graficar_varclima <- function(data, cols_clima_data, grupos, colores, titulo, eje_y, nombre_grafica) {
  
  # Verificar que el número de columnas es múltiplo del número de grupos
  if (length(cols_clima_data) %% length(grupos) != 0) {
    stop("El número de columnas no es divisible entre el número de grupos.")
  }
  
  # Verificar que la longitud de los colores coincida con el número de grupos
  if (length(colores) != length(grupos)) {
    stop("El número de colores debe coincidir con el número de grupos.")
  }
  
  # Calcular el número de columnas por grupo
  n_cols_por_grupo <- length(cols_clima_data) / length(grupos)
  
  data_transformada_list <- list()
  
  for (i in seq_along(grupos)) {
    columnas <- cols_clima_data[((i - 1) * n_cols_por_grupo + 1):(i * n_cols_por_grupo)]
    
    # Transformar las columnas del grupo
    data_transformada <- data %>%
      pivot_longer(
        cols = columnas,
        names_to = paste0("ciudad_", grupos[i]),
        values_to = paste0("val_", grupos[i])
      ) %>%
      mutate(ciudad = case_when(
        !!sym(paste0("ciudad_", grupos[i])) == columnas[1] ~ "Santa Marta",
        !!sym(paste0("ciudad_", grupos[i])) == columnas[2] ~ "Barranquilla",
        !!sym(paste0("ciudad_", grupos[i])) == columnas[3] ~ "Cartagena"
      )) %>%
      select(Año, Semana_Epidemiologica, ciudad, !!sym(paste0("val_", grupos[i]))) %>%
      rename(valor = !!sym(paste0("val_", grupos[i]))) %>%
      mutate(grupo = grupos[i])
    
    data_transformada_list[[grupos[i]]] <- data_transformada
  }
  
   data_combinada <- bind_rows(data_transformada_list)
  
  # Crear la gráfica
  grafica <- ggplot(data_combinada, aes(x = Año + Semana_Epidemiologica / 52, y = valor, color = grupo)) +
    geom_line(size = 1, linetype = "solid") +
    scale_color_manual(values = colores) + # Uso de los colores definidos por el usuario
    labs(
      x = "Year",
      y = eje_y,
      title = titulo
    ) +
    facet_wrap(~ciudad, ncol = 1, scales = "free_y") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      strip.text = element_text(size = 12),
      title = element_text(face = "bold")
    )
  
  
  ggsave(nombre_grafica, plot = grafica, width = 10, height = 7)
  
  return(grafica)
}


