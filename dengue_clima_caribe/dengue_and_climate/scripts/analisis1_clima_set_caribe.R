#==================================================================
#
# DATA SET - CLIMATE VARIABLES
# Preprocesamiento desde data set de sally
# 22 noviembre 2024
#
#----------------------------------------------
# Parte 1: Descriptivos y gráficas
#---------------------------------------------
#
#==================================================================


rm(list=ls())

# Cargar las librerías necesarias
library(dplyr)
library(lubridate)
library(readr)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(patchwork)


#Base de datos
Datosdengueclima <- read_csv("data/caribe_data/Datosdengueclima.csv")


#-------------------------------------------------------------------------------
# Descriptivos
#-------------------------------------------------------------------------------

# Lista de variables de interés
variables_interes <- c(
  "BAQ_max_temperature", "BAQ_min_temperature", "BAQ_avg_temperature",
  "BAQ_avg_specific_humidity", "BAQ_avg_relative_humidity", "BAQ_total_precipitation",
  "CTG_max_temperature", "CTG_min_temperature", "CTG_avg_temperature",
  "CTG_avg_specific_humidity", "CTG_avg_relative_humidity", "CTG_total_precipitation",
  "SMR_max_temperature", "SMR_min_temperature", "SMR_avg_temperature",
  "SMR_avg_specific_humidity", "SMR_avg_relative_humidity", "SMR_total_precipitation"
)

# Filtrar solo las columnas relevantes
Datos_filtrados <- Datosdengueclima[, variables_interes]

# Calcular las estadísticas
resumen <- sapply(Datos_filtrados, function(col) {
  c(
    Mean = mean(col, na.rm = TRUE),
    Minimum = min(col, na.rm = TRUE),
    Maximum = max(col, na.rm = TRUE),
    SD = sd(col, na.rm = TRUE),
    Percentil_25 = quantile(col, 0.25, na.rm = TRUE),
    Percentil_50 = quantile(col, 0.50, na.rm = TRUE), # También es la mediana
    Percentil_75 = quantile(col, 0.75, na.rm = TRUE)
  )
})

# Convertir a dataframe
resumen_df <- as.data.frame(t(resumen))

# Agregar columna de ciudad
resumen_df$Ciudad <- ifelse(grepl("^BAQ", rownames(resumen_df)), "Barranquilla",
                            ifelse(grepl("^CTG", rownames(resumen_df)), "Cartagena", "Santa Marta"))




#-------------------------------------------------------------------------------
#Gráficas
#-------------------------------------------------------------------------------

#===============
#Barranquilla
#===============
Datosdenguedefi_Barranquilla <- Datosdengueclima %>%
                                select(Año, Semana_Epidemiologica, 
                                       Casos_Barranquilla, 
                                       BAQ_max_temperature, BAQ_min_temperature, BAQ_avg_temperature, 
                                       BAQ_total_precipitation,
                                       BAQ_avg_specific_humidity, BAQ_avg_relative_humidity)

# Casos de Dengue
grcasos <- ggplot(Datosdenguedefi_Barranquilla, aes(x = Año + Semana_Epidemiologica / 52, y = Casos_Barranquilla)) + 
  geom_line(color = "red", size = 0.5) +
  geom_point(color = "red", size = 0.5) +  
  labs(x = "Año", y = "No.", title = "Casos de Dengue") +
  scale_x_continuous(breaks = seq(min(Datosdenguedefi_Barranquilla$Año), max(Datosdenguedefi_Barranquilla$Año), by = 1)) +
  theme_minimal()
grcasos


# Precipitación Total
grprecipt <- ggplot(Datosdenguedefi_Barranquilla, aes(x = Año + Semana_Epidemiologica / 52, y = BAQ_total_precipitation)) + 
  geom_line(color = "#008B00", size = 0.5) +
  geom_point(color = "#008B00", size = 0.5) +  
  labs(x = "Año", y = "(mm)", title = "Precipitación Total") +
  scale_x_continuous(breaks = seq(min(Datosdenguedefi_Barranquilla$Año), max(Datosdenguedefi_Barranquilla$Año), by = 1)) +
  theme_minimal()
grprecipt



# Temperaturas (promedio, máxima y mínima) en la misma gráfica

# Temperatura mínima, media y máxima
grtemp <- ggplot(Datosdenguedefi_Barranquilla, aes(x = Año + Semana_Epidemiologica / 52)) +
  geom_line(aes(y = BAQ_avg_temperature, color = "Temp Prom"), size = 0.5, linetype = "solid") +
  geom_point(aes(y = BAQ_avg_temperature, color = "Temp Prom"), size = 0.5) +  
  geom_line(aes(y = BAQ_max_temperature, color = "Temp Max"), size = 0.5, linetype = "solid") +
  geom_point(aes(y = BAQ_max_temperature, color = "Temp Max"), size = 0.5) +  
  geom_line(aes(y = BAQ_min_temperature, color = "Temp Min"), size = 0.5, linetype = "solid") +
  geom_point(aes(y = BAQ_min_temperature, color = "Temp Min"), size = 0.5) +  
  labs(x = "Año", y = "(°C)", title = "Temperatura mínima, media y máxima") +
  scale_x_continuous(breaks = seq(min(Datosdenguedefi_Barranquilla$Año), max(Datosdenguedefi_Barranquilla$Año), by = 1)) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  scale_color_manual(values = c("Temp Prom" = "#9A32CD", "Temp Max" = "#B22222", "Temp Min" = "#1E90FF"))

grtemp



# Humedad relativa (%)

grhumrel <- ggplot(Datosdenguedefi_Barranquilla, aes(x = Año + Semana_Epidemiologica / 52, y=BAQ_avg_relative_humidity)) +
  geom_line(color = "#53868B", size = 0.5) +
  geom_point(color = "#53868B", size = 0.5) +
  labs(x = "Año", y = "(%)", title = "Humedad relativa") +
  scale_x_continuous(breaks = seq(min(Datosdenguedefi_Barranquilla$Año), max(Datosdenguedefi_Barranquilla$Año), by = 1)) +
  theme_minimal()

grhumrel


# Humedad específica (%)
grhumesp <- ggplot(Datosdenguedefi_Barranquilla, aes(x = Año + Semana_Epidemiologica / 52, y=BAQ_avg_specific_humidity)) +
  geom_line(color = "#2F4F4F", size = 0.5) +
  geom_point(color = "#2F4F4F", size = 0.5) +
  labs(x = "Año", y = "(g/kg)", title = "Humedad específica") +
  scale_x_continuous(breaks = seq(min(Datosdenguedefi_Barranquilla$Año), max(Datosdenguedefi_Barranquilla$Año), by = 1)) +
  theme_minimal()

grhumesp



# UNIR LAS GRÁFICAS
# Combinar las tres gráficas en una imagen vertical
grid.arrange(grcasos, grprecipt, grhumrel, grhumesp, grtemp, ncol = 1,
             heights = c(1, 1, 1, 1, 1.5),
             title= "Barranquilla")

BQL_climaycasos <- grcasos / grprecipt / grhumrel / grhumesp / grtemp +
                plot_annotation(
                  title = "Barranquilla",
                  theme = theme(plot.title = element_text(face = "bold"))
                )

BQL_climaycasos


#===============
#Cartagena
#===============
Datosdenguedefi_Cartagena <- Datosdengueclima %>%
  select(Año, Semana_Epidemiologica, 
         Casos_Cartagena, 
         CTG_max_temperature,CTG_min_temperature, CTG_avg_temperature, 
         CTG_total_precipitation,
         CTG_avg_specific_humidity, CTG_avg_relative_humidity)

# Casos de Dengue
grcasos <- ggplot(Datosdenguedefi_Cartagena, aes(x = Año + Semana_Epidemiologica / 52, y = Casos_Cartagena)) + 
  geom_line(color = "red", size = 0.5) +
  geom_point(color = "red", size = 0.5) +
  labs(x = "Año", y = "No.", title = "Casos de Dengue") +
  scale_x_continuous(breaks = seq(min(Datosdenguedefi_Cartagena$Año), max(Datosdenguedefi_Cartagena$Año), by = 1)) +
  theme_minimal()
grcasos


# Precipitación Total
grprecipt <- ggplot(Datosdenguedefi_Cartagena, aes(x = Año + Semana_Epidemiologica / 52, y = CTG_total_precipitation)) + 
  geom_line(color = "#008B00", size = 0.5) +
  geom_point(color = "#008B00", size = 0.5) + 
  labs(x = "Año", y = "(mm)", title = "Precipitación Total") +
  scale_x_continuous(breaks = seq(min(Datosdenguedefi_Cartagena$Año), max(Datosdenguedefi_Cartagena$Año), by = 1)) +
  theme_minimal()
grprecipt



# Temperaturas (promedio, máxima y mínima) en la misma gráfica

grtemp <- ggplot(Datosdenguedefi_Cartagena, aes(x = Año + Semana_Epidemiologica / 52)) +
  geom_line(aes(y = CTG_avg_temperature, color = "Temp Prom"), size = 0.5, linetype = "solid") +
  geom_point(aes(y = CTG_avg_temperature, color = "Temp Prom"), size = 0.5) +  
  geom_line(aes(y = CTG_max_temperature, color = "Temp Max"), size = 0.5, linetype = "solid") +
  geom_point(aes(y = CTG_max_temperature, color = "Temp Max"), size = 0.5) +  
  geom_line(aes(y = CTG_min_temperature, color = "Temp Min"), size = 0.5, linetype = "solid") +
  geom_point(aes(y = CTG_min_temperature, color = "Temp Min"), size = 0.5) +  
  labs(x = "Año", y = "(°C)", title = "Temperatura mínima, media y máxima") +
  scale_x_continuous(breaks = seq(min(Datosdenguedefi_Cartagena$Año), max(Datosdenguedefi_Cartagena$Año), by = 1)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank())+
  scale_color_manual(values = c("Temp Prom" =  "#9A32CD", "Temp Max" = "#B22222", "Temp Min" = "#1E90FF"))
grtemp


# Humedad relativa (%)

grhumrel <- ggplot(Datosdenguedefi_Cartagena, aes(x = Año + Semana_Epidemiologica / 52, y=CTG_avg_relative_humidity)) +
  geom_line(color = "#53868B", size = 0.5) +
  geom_point(color = "#53868B", size = 0.5) +
  labs(x = "Año", y = "(%)", title = "Humedad relativa") +
  scale_x_continuous(breaks = seq(min(Datosdenguedefi_Cartagena$Año), max(Datosdenguedefi_Cartagena$Año), by = 1)) +
  theme_minimal()

grhumrel


# Humedad específica (%)
grhumesp <- ggplot(Datosdenguedefi_Cartagena, aes(x = Año + Semana_Epidemiologica / 52, y=CTG_avg_specific_humidity)) +
  geom_line(color = "#2F4F4F", size = 0.5) +
  geom_point(color = "#2F4F4F", size = 0.5) +
  labs(x = "Año", y = "(g/kg)", title = "Humedad específica") +
  scale_x_continuous(breaks = seq(min(Datosdenguedefi_Cartagena$Año), max(Datosdenguedefi_Cartagena$Año), by = 1)) +
  theme_minimal()

grhumesp



# UNIR LAS GRÁFICAS
CTG_climaycasos <- grcasos / grprecipt / grhumrel / grhumesp / grtemp +
  plot_annotation(
    title = "Cartagena",
    theme = theme(plot.title = element_text(face = "bold"))
  )

CTG_climaycasos






#===============
#Santa Marta
#===============
Datosdenguedefi_Samarta <- Datosdengueclima %>%
  select(Año, Semana_Epidemiologica, 
         Casos_SantaMarta, 
         SMR_max_temperature, SMR_min_temperature, SMR_avg_temperature, 
         SMR_total_precipitation,
         SMR_avg_specific_humidity, SMR_avg_relative_humidity)

# Casos de Dengue
grcasos <- ggplot(Datosdenguedefi_Samarta, aes(x = Año + Semana_Epidemiologica / 52, y = Casos_SantaMarta)) + 
  geom_line(color = "red", size = 0.5) +
  geom_point(color = "red", size = 0.5) +
  labs(x = "Año", y = "No.", title = "Casos de Dengue") +
  scale_x_continuous(breaks = seq(min(Datosdenguedefi_Samarta$Año), max(Datosdenguedefi_Samarta$Año), by = 1)) +
  theme_minimal()
grcasos


# Precipitación Total
grprecipt <- ggplot(Datosdenguedefi_Samarta, aes(x = Año + Semana_Epidemiologica / 52, y = SMR_total_precipitation)) + 
  geom_line(color = "#008B00", size = 0.5) +
  geom_point(color = "#008B00", size = 0.5) +
  labs(x = "Año", y = "(mm)", title = "Precipitación Total") +
  scale_x_continuous(breaks = seq(min(Datosdenguedefi_Samarta$Año), max(Datosdenguedefi_Samarta$Año), by = 1)) +
  theme_minimal()
grprecipt



# Temperaturas (promedio, máxima y mínima) en la misma gráfica

grtemp <- ggplot(Datosdenguedefi_Samarta, aes(x = Año + Semana_Epidemiologica / 52)) +
  geom_line(aes(y = SMR_avg_temperature, color = "Temp Prom"), size = 0.5, linetype = "solid") +
  geom_point(aes(y = SMR_avg_temperature, color = "Temp Prom"), size = 0.5) + 
  geom_line(aes(y = SMR_max_temperature, color = "Temp Max"), size = 0.5, linetype = "solid") +
  geom_point(aes(y = SMR_max_temperature, color = "Temp Max"), size = 0.5) + 
  geom_line(aes(y = SMR_min_temperature, color = "Temp Min"), size = 0.5, linetype = "solid") +
  geom_point(aes(y = SMR_min_temperature, color = "Temp Min"), size = 0.5) + 
  labs(x = "Año", y = "(°C)", title = "Temperatura mínima, media y máxima") +
  scale_x_continuous(breaks = seq(min(Datosdenguedefi_Samarta$Año), max(Datosdenguedefi_Samarta$Año), by = 1)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank())+
  scale_color_manual(values = c("Temp Prom" =  "#9A32CD", "Temp Max" = "#B22222", "Temp Min" = "#1E90FF"))
grtemp


# Humedad relativa (%)

grhumrel <- ggplot(Datosdenguedefi_Samarta, aes(x = Año + Semana_Epidemiologica / 52, y=SMR_avg_relative_humidity)) +
  geom_line(color = "#53868B", size = 0.5) +
  geom_point(color = "#53868B", size = 0.5) +
  labs(x = "Año", y = "(%)", title = "Humedad relativa") +
  scale_x_continuous(breaks = seq(min(Datosdenguedefi_Samarta$Año), max(Datosdenguedefi_Samarta$Año), by = 1)) +
  theme_minimal()

grhumrel


# Humedad específica (%)
grhumesp <- ggplot(Datosdenguedefi_Samarta, aes(x = Año + Semana_Epidemiologica / 52, y=SMR_avg_specific_humidity)) +
  geom_line(color = "#2F4F4F", size = 0.5) +
  geom_point(color = "#2F4F4F", size = 0.5) +
  labs(x = "Año", y = "(g/kg)", title = "Humedad específica") +
  scale_x_continuous(breaks = seq(min(Datosdenguedefi_Samarta$Año), max(Datosdenguedefi_Samarta$Año), by = 1)) +
  theme_minimal()

grhumesp



# UNIR LAS GRÁFICAS
STM_climaycasos <- grcasos / grprecipt / grhumrel / grhumesp / grtemp +
  plot_annotation(
    title = "Santa Marta",
    theme = theme(plot.title = element_text(face = "bold"))
  )

STM_climaycasos



#=======================
# Todas las gráficas
#======================
BQL_climaycasos
CTG_climaycasos
STM_climaycasos

ggsave("outputs/BQL_climaycasos.png", plot = BQL_climaycasos, width = 10, height = 8)
ggsave("outputs/CTG_climaycasos.png", plot = CTG_climaycasos, width = 10, height = 8)
ggsave("outputs/STM_climaycasos.png", plot = STM_climaycasos, width = 10, height = 8)



ggsave("outputs/clima_y_casos_dengue.png", plot = imagen_completa, width = 10, height = 15)
