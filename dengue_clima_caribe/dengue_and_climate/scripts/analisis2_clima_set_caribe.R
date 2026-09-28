#==================================================================
#
# DATA SET - CLIMATE VARIABLES
# Preprocesamiento desde data set de sally
# 27 noviembre 2024
#
#----------------------------------------------
# Parte 2: Análisis estadístico analíticos
# Correlaciones con lags y modelo 
#---------------------------------------------
#
#==================================================================


rm(list=ls())

# Cargar las librerías necesarias
library(dplyr)
library(lubridate)
library(readr)
library(ggplot2)
library(gridExtra)
library(patchwork)
library(tidyr)
library(dlnm)
library(MASS)
library(readr)
library(readxl)

##################
# Base de datos
##################
Datosdengueclima <- read_csv("data/caribe_data/Datosdengueclima.csv")



#-------------------------------------------------------------------------------
# 1. Evaluar sobredispersión
#-------------------------------------------------------------------------------

mean_cases <- mean(Datosdengueclima$Casos_Barranquilla, na.rm = TRUE)
var_cases <- var(Datosdengueclima$Casos_Barranquilla, na.rm = TRUE)

dispersion_index <- var_cases / mean_cases
dispersion_index #Alta dispersión



#-------------------------------------------------------------------------------
# 2. Evaluar LAGS
#-------------------------------------------------------------------------------

Datosdengueclima <- Datosdengueclima %>%
                   arrange(Año, Semana_Epidemiologica)


lags <- c(0, 2, 4, 8, 12, 14, 16, 18, 20, 22, 24)

calcular_correlacion <- function(data, lag, casos, varclima) {
  data <- data %>%
    mutate(varclima_lagged = lag(!!sym(varclima), lag))  # Lag de la temperatura
  
  correlation <- cor(data[[casos]], data$varclima_lagged, method = "spearman", use = "complete.obs")
  
  return(correlation)
}


#--------------------------------------
#TEMPERATURA MEDIA:

# Barranquilla vs temperatura media
bql_temmedia_cor <- sapply(lags, function(lag) calcular_correlacion(Datosdengueclima, lag, "Casos_Barranquilla", "BAQ_avg_temperature"))
bql_temmedia_cor

# Cartagena vs  temperatura media
ctg_temmedia_cor <- sapply(lags, function(lag) calcular_correlacion(Datosdengueclima, lag, "Casos_Cartagena", "CTG_avg_temperature"))
ctg_temmedia_cor

# Santa Marta vs  temperatura media
stm_temmedia_cor <- sapply(lags, function(lag) calcular_correlacion(Datosdengue_Bql, lag, "Casos_SantaMarta", "SMR_avg_temperature"))
stm_temmedia_cor

#--------------------------------------
#TEMPERATURA MÍNIMA:

# Barranquilla vs temperatura mínima
bql_temminima_cor <- sapply(lags, function(lag) calcular_correlacion(Datosdengueclima, lag, "Casos_Barranquilla", "BAQ_avg_temperature"))
bql_temminima_cor

# Cartagena vs  temperatura mínima
ctg_temminima_cor <- sapply(lags, function(lag) calcular_correlacion(Datosdengueclima, lag, "Casos_Cartagena", "CTG_avg_temperature"))
ctg_temminima_cor

# Santa Marta vs  temperatura mínima
stm_temminima_cor <- sapply(lags, function(lag) calcular_correlacion(Datosdengue_Bql, lag, "Casos_SantaMarta", "SMR_avg_temperature"))
stm_temminima_cor


#--------------------------------------
#TEMPERATURA MÁXIMA:

# Barranquilla vs temperatura máxima
bql_temaxima_cor <- sapply(lags, function(lag) calcular_correlacion(Datosdengueclima, lag, "Casos_Barranquilla", "BAQ_avg_temperature"))
bql_temmaxima_cor

# Cartagena vs  temperatura máxima
ctg_temmaxima_cor <- sapply(lags, function(lag) calcular_correlacion(Datosdengueclima, lag, "Casos_Cartagena", "CTG_avg_temperature"))
ctg_temmaxima_cor

# Santa Marta vs  temperatura máxima
stm_temmaxima_cor <- sapply(lags, function(lag) calcular_correlacion(Datosdengue_Bql, lag, "Casos_SantaMarta", "SMR_avg_temperature"))
stm_temmaxima_cor

#--------------------------------------
#PRECIPITACIÓN TOTAL:

# Barranquilla vs precipitación total
bql_precitotal_cor <- sapply(lags, function(lag) calcular_correlacion(Datosdengueclima, lag, "Casos_Barranquilla", "BAQ_avg_temperature"))
bql_precitotal_cor

# Cartagena vs  precipitación total
ctg_precitotal_cor <- sapply(lags, function(lag) calcular_correlacion(Datosdengueclima, lag, "Casos_Cartagena", "CTG_avg_temperature"))
ctg_precitotal_cor

# Santa Marta vs  precipitación total
stm_precitotal_cor <- sapply(lags, function(lag) calcular_correlacion(Datosdengue_Bql, lag, "Casos_SantaMarta", "SMR_avg_temperature"))
stm_precitotal_cor

#--------------------------------------
#HUMEDAD RELATIVA:

# Barranquilla vs humedad relativa
bql_humrelativ_cor <- sapply(lags, function(lag) calcular_correlacion(Datosdengueclima, lag, "Casos_Barranquilla", "BAQ_avg_temperature"))
bql_humrelativ_cor

# Cartagena vs  humedad relativa
ctg_humrelativ_cor <- sapply(lags, function(lag) calcular_correlacion(Datosdengueclima, lag, "Casos_Cartagena", "CTG_avg_temperature"))
ctg_humrelativ_cor

# Santa Marta vs  humedad relativa
stm_humrelativ_cor <- sapply(lags, function(lag) calcular_correlacion(Datosdengue_Bql, lag, "Casos_SantaMarta", "SMR_avg_temperature"))
stm_humrelativ_cor

#--------------------------------------
#HUMEDAD ESPECÍFICA:

# Barranquilla vs humedad específica
bql_humespc_cor <- sapply(lags, function(lag) calcular_correlacion(Datosdengueclima, lag, "Casos_Barranquilla", "BAQ_avg_temperature"))
bql_humespc_cor

# Cartagena vs  humedad específica
ctg_humespc_cor <- sapply(lags, function(lag) calcular_correlacion(Datosdengueclima, lag, "Casos_Cartagena", "CTG_avg_temperature"))
ctg_humespc_cor

# Santa Marta vs  humedad específica
stm_humespc_cor <- sapply(lags, function(lag) calcular_correlacion(Datosdengue_Bql, lag, "Casos_SantaMarta", "SMR_avg_temperature"))
stm_humespc_cor

#--------------------------------------------------------------------------------------------------------
#GRÁFICAS DE LAGS
#--------------------

lags_corr <- read_excel("data/lags_corr.xlsx")

# Graficar ideam
graph_corlags_ideam<-ggplot(lags_corr, aes(x = Lag)) +
                geom_line(aes(y = Temperatura_media_ideam, color = "Temperature average IDEAM"), size = 1, linetype = "solid") +
                geom_line(aes(y = Precipitacion_total_ideam, color = "Total Precipitation IDEAM"), size = 1, linetype = "solid") +
                geom_line(aes(y = Humedad_Relativa_ideam, color = "Relative humidity IDEAM"), size = 1, linetype = "solid") +
                geom_point(aes(y = Temperatura_media_ideam, color = "Temperature average IDEAM"), size = 2) +
                geom_point(aes(y = Precipitacion_total_ideam, color = "Total Precipitation IDEAM"), size = 2) +
                geom_point(aes(y = Humedad_Relativa_ideam, color = "Relative humidity IDEAM"), size = 2) +
                ylim(-0.15,0.45)+
                labs(
                  title = "Correlation between dengue cases and climate variables (IDEAM)",
                  x = "Lag (weeks)",
                  y = "Spearman correlation"
                ) +
                facet_wrap(~ Ciudad) +
                theme_minimal(base_size = 14)+
                theme(legend.title = element_blank())+
                scale_color_manual(
                  values = c(
                    "Temperature average IDEAM" = "#CD0000",  
                    "Total Precipitation IDEAM" = "#2E8B57",  
                    "Relative humidity IDEAM" = "#36648B"    
                  )
                )

graph_corlags_ideam
#Guardar gráfico
png ("outputs/graph_corlags_ideam.png", width = 850, height = 450)
print(graph_corlags_ideam)
dev.off()


# Graficar satelite data
graph_corlags_sally<-ggplot(lags_corr, aes(x = Lag)) +
                    geom_line(aes(y = Temperatura_media, color = "Temperature average"), size = 1, linetype = "solid") +
                    geom_line(aes(y = Temperatura_minima, color = "Minimum temperature"), size = 1, linetype = "solid") +
                    geom_line(aes(y = Temperatura_maxima, color = "Maximum temperature"), size = 1, linetype = "solid") +
                    geom_line(aes(y = Precipitacion_total, color = "Total precipitation"), size = 1, linetype = "solid") +
                    geom_line(aes(y = Humedad_relativa, color = "Relative humidity"), size = 1, linetype = "solid") +
                    geom_line(aes(y = Humedad_especifica, color = "Specific humidity"), size = 1, linetype = "solid") +
                    geom_point(aes(y = Temperatura_media, color = "Temperature average"), size = 2) +
                    geom_point(aes(y = Temperatura_minima, color = "Minimum temperature"), size = 2) +
                    geom_point(aes(y = Temperatura_maxima, color = "Maximum temperature"), size = 2) +
                    geom_point(aes(y = Precipitacion_total, color = "Total precipitation"), size = 2) +
                    geom_point(aes(y = Humedad_relativa, color = "Relative humidity"), size = 2) +
                    geom_point(aes(y = Humedad_especifica, color = "Specific humidity"), size = 2) +
                    #ylim(-0.15,0.45)+
                    labs(
                      title = "Correlation between dengue cases and climate variables (CHIRPS & ERA5)",
                      x = "Lag (weeks)",
                      y = "Spearman correlation"
                    ) +
                    facet_wrap(~ Ciudad) +
                    theme_minimal(base_size = 14)+
                    theme(legend.title = element_blank())+
                    scale_color_manual(
                      values = c(
                        "Temperature average" = "#CD8162",  
                        "Minimum temperature" = "#CD0000",  
                        "Maximum temperature" = "#8B3626",    
                        "Total precipitation" = "#2E8B57", 
                        "Relative humidity" = "#36648B",  
                        "Specific humidity" = "#6CA6CD"     
                      )
                    )

graph_corlags_sally
#Guardar gráfico
png ("outputs/graph_corlags_sally.png", width = 850, height = 450)
print(graph_corlags_sally)
dev.off()


#----------------------------------------------------------------
#combinar gráficas
gra_lags_ambos<- graph_corlags_ideam / graph_corlags_sally
gra_lags_ambos


#Guardar gráfico
png ("outputs/graph_corlags.png", width = 800, height = 350)
print(graph_corlags)
dev.off()



#-------------------------------------------------------------------------------
# 3. Ajuste del modelo
#-------------------------------------------------------------------------------

#BARRANQUILLA:

# Crear las variables con los lags correspondientes
Datosdeng_Bql <- Datosdengueclima %>%
                                select(Año, Semana_Epidemiologica, Casos_Barranquilla,
                                       BAQ_avg_temperature, BAQ_min_temperature, BAQ_max_temperature,
                                       BAQ_total_precipitation, BAQ_avg_specific_humidity, BAQ_avg_relative_humidity) %>%
                                arrange(Año, Semana_Epidemiologica) %>%
                                mutate(
                                  Temp_avg_lag20 = lag(BAQ_avg_temperature, 20),
                                  Temp_min_lag24 = lag(BAQ_min_temperature, 24),
                                  Temp_max_lag20 = lag(BAQ_max_temperature, 20),
                                  Precip_lag12 = lag(BAQ_total_precipitation, 12),
                                  Humidity_relative_lag12 = lag(BAQ_avg_relative_humidity, 12),
                                  Humidity_specific_lag14 = lag(BAQ_avg_specific_humidity, 14)
                                )


Datosdeng_Bql <- na.omit(Datosdeng_Bql) #Eliminar valores faltantes por los lags


#Modelo binomial negativo:
modelo_bineg <- glm.nb(Casos_Barranquilla ~ Temp_avg_lag20 + Temp_min_lag24 + Temp_max_lag20 +
                        Precip_lag12 + Humidity_relative_lag12 + Humidity_specific_lag14,
                        data = Datosdeng_Bql)
summary(modelo_bineg)


#Modelo definitivo - seleccionado en equipo
modelo_bineg_def <- glm.nb(Casos_Barranquilla ~ Temp_avg_lag20 + Temp_max_lag20 +
                         Precip_lag12 + Humidity_relative_lag12 + Humidity_specific_lag14,
                       data = Datosdeng_Bql)
summary(modelo_bineg_def)


# Verificación del modelo




#============
# Modelo 2
#===========
datos=Datosdengueclima
datos <- datos %>%
  mutate(Tiempo = (Año - min(Año)) * 52 + Semana_Epidemiologica)  # Crear una columna de tiempo continuo

lags <- 0:24  # variables de retraso (lags) para variables climáticas

# Matriz cruzada para precipitación total
cross_precip <- crossbasis(datos$BAQ_total_precipitation, lag = max(lags), 
                           argvar = list(fun = "bs", degree = 3),  # Spline para precipitación
                           arglag = list(fun = "bs", degree = 3))  # Spline para retrasos

plot(cross_precip)

# Matriz cruzada para temperatura promedio
cross_temp <- crossbasis(datos$BAQ_avg_temperature, lag = max(lags), 
                         argvar = list(fun = "bs", degree = 3),
                         arglag = list(fun = "bs", degree = 3))
plot(cross_temp)

# Matriz cruzada para temperatura mínima
cross_tempmin <- crossbasis(datos$BAQ_min_temperature, lag = max(lags), 
                         argvar = list(fun = "bs", degree = 3),
                         arglag = list(fun = "bs", degree = 3))
plot(cross_tempmin)

# Matriz cruzada para temperatura máxima
cross_tempmax <- crossbasis(datos$BAQ_max_temperature, lag = max(lags), 
                         argvar = list(fun = "bs", degree = 3),
                         arglag = list(fun = "bs", degree = 3))
plot(cross_tempmax)

# Matriz cruzada para humedad relativa
cross_humr <- crossbasis(datos$BAQ_avg_relative_humidity, lag = max(lags), 
                         argvar = list(fun = "bs", degree = 3),
                         arglag = list(fun = "bs", degree = 3))
plot(cross_humr)

# Matriz cruzada para humedad específica
cross_hume <- crossbasis(datos$BAQ_avg_specific_humidity, lag = max(lags), 
                         argvar = list(fun = "bs", degree = 3),
                         arglag = list(fun = "bs", degree = 3))
plot(cross_hume)


# Ajuste del modelo binomial negativo
modelo_baq <- glm.nb(
  Casos_Barranquilla ~ 
    cross_precip + 
    cross_temp + 
    cross_tempmin+
    cross_tempax+
    cross_humr+
    cross_hume+
    s(Semana_Epidemiologica, bs = "cc", k = 52) +  # Ciclo estacional anual
    s(Tiempo, k = 10),                            # Tendencia a largo plazo
  data = datos
)

# Resumen del modelo
summary(modelo_baq)





#--------------------------------------------------------
#CARTAGENA:

# Crear las variables con los lags correspondientes
Datosdeng_Bql <- Datosdengueclima %>%
  select(Año, Semana_Epidemiologica, Casos_Barranquilla,
         BAQ_avg_temperature, BAQ_min_temperature, BAQ_max_temperature,
         BAQ_total_precipitation, BAQ_avg_specific_humidity, BAQ_avg_relative_humidity) %>%
  arrange(Año, Semana_Epidemiologica) %>%
  mutate(
    Temp_avg_lag20 = lag(BAQ_avg_temperature, 20),
    Temp_min_lag24 = lag(BAQ_min_temperature, 24),
    Temp_max_lag20 = lag(BAQ_max_temperature, 20),
    Precip_lag12 = lag(BAQ_total_precipitation, 12),
    Humidity_relative_lag12 = lag(BAQ_avg_relative_humidity, 12),
    Humidity_specific_lag14 = lag(BAQ_avg_specific_humidity, 14)
  )


Datosdeng_Bql <- na.omit(Datosdeng_Bql) #Eliminar valores faltantes por los lags


#Modelo binomial negativo:
modelo_bineg <- glm.nb(Casos_Barranquilla ~ Temp_avg_lag20 + Temp_min_lag24 + Temp_max_lag20 +
                         Precip_lag12 + Humidity_relative_lag12 + Humidity_specific_lag14,
                       data = Datosdeng_Bql)
summary(modelo_bineg)


#Modelo definitivo - seleccionado en equipo
modelo_bineg_def <- glm.nb(Casos_Barranquilla ~ Temp_avg_lag20 + Temp_max_lag20 +
                             Precip_lag12 + Humidity_relative_lag12 + Humidity_specific_lag14,
                           data = Datosdeng_Bql)
summary(modelo_bineg_def)


# Verificación del modelo




#--------------------------------------------------------
#SANTA MARTA:

# Crear las variables con los lags correspondientes
Datosdeng_Bql <- Datosdengueclima %>%
  select(Año, Semana_Epidemiologica, Casos_Barranquilla,
         BAQ_avg_temperature, BAQ_min_temperature, BAQ_max_temperature,
         BAQ_total_precipitation, BAQ_avg_specific_humidity, BAQ_avg_relative_humidity) %>%
  arrange(Año, Semana_Epidemiologica) %>%
  mutate(
    Temp_avg_lag20 = lag(BAQ_avg_temperature, 20),
    Temp_min_lag24 = lag(BAQ_min_temperature, 24),
    Temp_max_lag20 = lag(BAQ_max_temperature, 20),
    Precip_lag12 = lag(BAQ_total_precipitation, 12),
    Humidity_relative_lag12 = lag(BAQ_avg_relative_humidity, 12),
    Humidity_specific_lag14 = lag(BAQ_avg_specific_humidity, 14)
  )


Datosdeng_Bql <- na.omit(Datosdeng_Bql) #Eliminar valores faltantes por los lags


#Modelo binomial negativo:
modelo_bineg <- glm.nb(Casos_Barranquilla ~ Temp_avg_lag20 + Temp_min_lag24 + Temp_max_lag20 +
                         Precip_lag12 + Humidity_relative_lag12 + Humidity_specific_lag14,
                       data = Datosdeng_Bql)
summary(modelo_bineg)


#Modelo definitivo - seleccionado en equipo
modelo_bineg_def <- glm.nb(Casos_Barranquilla ~ Temp_avg_lag20 + Temp_max_lag20 +
                             Precip_lag12 + Humidity_relative_lag12 + Humidity_specific_lag14,
                           data = Datosdeng_Bql)
summary(modelo_bineg_def)


# Verificación del modelo

