#==================================================================
#
# DATA SET - CLIMATE VARIABLES
# Preprocesamiento desde data set de sally
# 22 noviembre 2024
#
#----------------------------------------------
# Parte 3: Análisis estadístico-
# GRÁFICA TRIDIMENSIONAL
#---------------------------------------------
#
#==================================================================


rm(list=ls())

library(dlnm)
library(MASS) # Para el modelo binomial negativo
library(splines) # Para bases spline
library(ggplot2)


##################
# Base de datos
##################
Datosdengueclima <- read_csv("data/caribe_data/Datosdengueclima.csv")
datos_dlnm=Datosdengueclima
datos_dlnm <- datos_dlnm[datos_dlnm$Año >= 2009 & datos_dlnm$Año <= 2023, ]


datos_dlnm$Fecha <- as.Date(paste(datos_dlnm$Año, datos_dlnm$Semana_Epidemiologica, 1, sep = "-"), format = "%Y-%U-%u")

#Modelo Barranquilla
modelo_baq <- glm.nb(
  Casos_Barranquilla ~ 
    ns(BAQ_avg_temperature, df = 4) + # Temperatura media
    ns(BAQ_avg_specific_humidity, df = 4) + # Humedad específica
    ns(BAQ_total_precipitation, df = 4) + # Precipitación total
    ns(Semana_Epidemiologica, df = 4) + # Efecto estacional
    factor(Año), # Efecto del año
  data = datos_dlnm
)


# Modelos Cartagena 
modelo_ctg <- glm.nb(
  Casos_Cartagena ~ 
    ns(CTG_avg_temperature, df = 4) + 
    ns(CTG_avg_specific_humidity, df = 4) + 
    ns(CTG_total_precipitation, df = 4) + 
    ns(Semana_Epidemiologica, df = 4) + 
    factor(Año),
  data = datos_dlnm
)


# Modelos Santa Marta
modelo_smr <- glm.nb(
  Casos_SantaMarta ~ 
    ns(SMR_avg_temperature, df = 4) + 
    ns(SMR_avg_specific_humidity, df = 4) + 
    ns(SMR_total_precipitation, df = 4) + 
    ns(Semana_Epidemiologica, df = 4) + 
    factor(Año),
  data = datos_dlnm
)

summary(modelo_baq)
summary(modelo_ctg)
summary(modelo_smr)


#-------------------------------------------------------------------------------
#BARRANQUILLA
#-------------------------------------------------------------------------------

# Crear una base de retardos (lags) para la temperatura mínima en Barranquilla
basis_temp_baq <- crossbasis(
                  datos_dlnm$BAQ_min_temperature, 
                  lag = 30, # Hasta 20 semanas de retardo
                  argvar = list(fun = "ns", df = 4), # Relación no lineal con la temperatura
                  arglag = list(fun = "ns", df = 4) # Relación no lineal con el lag
                )

# Ajustar el modelo para incluir la base de retardos
modelo_baq <- glm.nb(
              Casos_Barranquilla ~ basis_temp_baq + 
                ns(BAQ_avg_specific_humidity, df = 4) + 
                ns(BAQ_total_precipitation, df = 4) + 
                ns(Semana_Epidemiologica, df = 4) + 
                factor(Año),
              data = datos_dlnm
            )

# Predicción del riesgo relativo (RR) según temperatura y desfase
pred_temp_baq <- crosspred(
                  basis_temp_baq, 
                  model = modelo_baq, 
                  cen = mean(datos_dlnm$BAQ_min_temperature, na.rm = TRUE), # Centrar en la temperatura promedio
                  bylag = 1, # Resolución del lag
                  by = 0.1 # Resolución de temperatura
                )
                

# Verifica que las dimensiones de la matriz z coincidan con x e y
z <- pred_temp_baq$matRRfit  # Matriz RR (53x31)

# Generar gráfico 3D con persp()
tem_min <- persp(
  x = pred_temp_baq$predvar,   # Eje X: Temperatura
  y = seq(0, 30, by = 1),      # Eje Y: Lag (21 valores)
  z = z,                       # Eje Z: Riesgo Relativo (53x21)
  xlab = "Temperatura (°C)", 
  ylab = "Lag (semanas)", 
  zlab = "RR", 
  theta = 60, phi = 40,        # Ángulos de perspectiva
  col = "skyblue",             # Color
  shade = 0.5,                 # Sombras
  ticktype = "detailed",       # Estilo de ticks
  main = "Efecto de la Temperatura mínima y el Lag en Barranquilla"
)



# Crear una base de retardos (lags) para la temperatura máxima en Barranquilla
basis_temp_baq <- crossbasis(
  datos_dlnm$BAQ_max_temperature, 
  lag = 30, # Hasta 20 semanas de retardo
  argvar = list(fun = "ns", df = 4), # Relación no lineal con la temperatura
  arglag = list(fun = "ns", df = 4) # Relación no lineal con el lag
)

# Ajustar el modelo para incluir la base de retardos
modelo_baq <- glm.nb(
  Casos_Barranquilla ~ basis_temp_baq + 
    ns(BAQ_avg_specific_humidity, df = 4) + 
    ns(BAQ_total_precipitation, df = 4) + 
    ns(Semana_Epidemiologica, df = 4) + 
    factor(Año),
  data = datos_dlnm
)

# Predicción del riesgo relativo (RR) según temperatura y desfase
pred_temp_baq <- crosspred(
  basis_temp_baq, 
  model = modelo_baq, 
  cen = mean(datos_dlnm$BAQ_max_temperature, na.rm = TRUE), # Centrar en la temperatura promedio
  bylag = 1, # Resolución del lag
  by = 0.1 # Resolución de temperatura
)


# Verifica que las dimensiones de la matriz z coincidan con x e y
z <- pred_temp_baq$matRRfit  # Matriz RR (53x31)

# Generar gráfico 3D con persp()
tem_min <- persp(
  x = pred_temp_baq$predvar,   # Eje X: Temperatura
  y = seq(0, 30, by = 1),      # Eje Y: Lag (21 valores)
  z = z,                       # Eje Z: Riesgo Relativo (53x21)
  xlab = "Temperatura (°C)", 
  ylab = "Lag (semanas)", 
  zlab = "RR", 
  theta = 60, phi = 40,        # Ángulos de perspectiva
  col = "skyblue",             # Color
  shade = 0.5,                 # Sombras
  ticktype = "detailed",       # Estilo de ticks
  main = "Efecto de la Temperatura máxima y el Lag en Barranquilla"
)



################################################################################




#-------------------------------------------------------------------------------
#CARTAGENA
#-------------------------------------------------------------------------------
# Crear una base de retardos (lags) para la temperatura máxima en Barranquilla
basis_temp_car <- crossbasis(
  datos_dlnm$CTG_min_temperature, 
  lag = 30, # Hasta 20 semanas de retardo
  argvar = list(fun = "ns", df = 4), # Relación no lineal con la temperatura
  arglag = list(fun = "ns", df = 4) # Relación no lineal con el lag
)

# Ajustar el modelo para incluir la base de retardos
modelo_car <- glm.nb(
  Casos_Cartagena ~ basis_temp_car + 
    ns(CTG_avg_specific_humidity, df = 4) + 
    ns(CTG_total_precipitation, df = 4) + 
    ns(Semana_Epidemiologica, df = 4) + 
    factor(Año),
  data = datos_dlnm
)

# Predicción del riesgo relativo (RR) según temperatura y desfase
pred_temp_car <- crosspred(
  basis_temp_car, 
  model = modelo_car, 
  cen = mean(datos_dlnm$CTG_min_temperature, na.rm = TRUE), # Centrar en la temperatura promedio
  bylag = 1, # Resolución del lag
  by = 0.1 # Resolución de temperatura
)


# Verifica que las dimensiones de la matriz z coincidan con x e y
z <- pred_temp_car$matRRfit  # Matriz RR (53x31)

# Generar gráfico 3D con persp()
tem_min <- persp(
  x = pred_temp_car$predvar,   # Eje X: Temperatura
  y = seq(0, 30, by = 1),      # Eje Y: Lag (21 valores)
  z = z,                       # Eje Z: Riesgo Relativo (53x21)
  xlab = "Temperatura (°C)", 
  ylab = "Lag (semanas)", 
  zlab = "RR", 
  theta = 60, phi = 40,        # Ángulos de perspectiva
  col = "skyblue",             # Color
  shade = 0.5,                 # Sombras
  ticktype = "detailed",       # Estilo de ticks
  main = "Efecto de la Temperatura mínima y el Lag en Cartagena"
)

#------------------------------------------------------------------------------

# Crear una base de retardos (lags) para la temperatura máxima en Barranquilla
basis_temp_car <- crossbasis(
  datos_dlnm$CTG_max_temperature, 
  lag = 30, # Hasta 20 semanas de retardo
  argvar = list(fun = "ns", df = 4), # Relación no lineal con la temperatura
  arglag = list(fun = "ns", df = 4) # Relación no lineal con el lag
)

# Ajustar el modelo para incluir la base de retardos
modelo_car <- glm.nb(
  Casos_Cartagena ~ basis_temp_car + 
    ns(CTG_avg_specific_humidity, df = 4) + 
    ns(CTG_total_precipitation, df = 4) + 
    ns(Semana_Epidemiologica, df = 4) + 
    factor(Año),
  data = datos_dlnm
)

# Predicción del riesgo relativo (RR) según temperatura y desfase
pred_temp_car <- crosspred(
  basis_temp_car, 
  model = modelo_car, 
  cen = mean(datos_dlnm$CTG_max_temperature, na.rm = TRUE), # Centrar en la temperatura promedio
  bylag = 1, # Resolución del lag
  by = 0.1 # Resolución de temperatura
)


# Verifica que las dimensiones de la matriz z coincidan con x e y
z <- pred_temp_car$matRRfit  # Matriz RR (53x31)

# Generar gráfico 3D con persp()
tem_min <- persp(
  x = pred_temp_car$predvar,   # Eje X: Temperatura
  y = seq(0, 30, by = 1),      # Eje Y: Lag (21 valores)
  z = z,                       # Eje Z: Riesgo Relativo (53x21)
  xlab = "Temperatura (°C)", 
  ylab = "Lag (semanas)", 
  zlab = "RR", 
  theta = 60, phi = 40,        # Ángulos de perspectiva
  col = "skyblue",             # Color
  shade = 0.5,                 # Sombras
  ticktype = "detailed",       # Estilo de ticks
  main = "Efecto de la Temperatura máxima y el Lag en Cartagena"
)

#------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#SANTA MARTA
#-------------------------------------------------------------------------------
# Crear una base de retardos (lags) para la temperatura máxima en Barranquilla
basis_temp_stm <- crossbasis(
  datos_dlnm$SMR_min_temperature, 
  lag = 30, # Hasta 20 semanas de retardo
  argvar = list(fun = "ns", df = 4), # Relación no lineal con la temperatura
  arglag = list(fun = "ns", df = 4) # Relación no lineal con el lag
)

# Ajustar el modelo para incluir la base de retardos
modelo_stm <- glm.nb(
  Casos_SantaMarta ~ basis_temp_stm + 
    ns(SMR_avg_specific_humidity, df = 4) + 
    ns(SMR_total_precipitation, df = 4) + 
    ns(Semana_Epidemiologica, df = 4) + 
    factor(Año),
  data = datos_dlnm
)

# Predicción del riesgo relativo (RR) según temperatura y desfase
pred_temp_stm <- crosspred(
  basis_temp_stm, 
  model = modelo_stm, 
  cen = mean(datos_dlnm$SMR_min_temperature, na.rm = TRUE), # Centrar en la temperatura promedio
  bylag = 1, # Resolución del lag
  by = 0.13 # Resolución de temperatura ajustada
)

# Verifica que las dimensiones de la matriz z coincidan con x e y
z <- pred_temp_stm$matRRfit  # Matriz RR
dim(z)
# Generar gráfico 3D con persp()
tem_min <- persp(
  x = pred_temp_car$predvar,   # Eje X: Temperatura
  y = seq(0, 30, by = 1),      # Eje Y: Lag (21 valores)
  z = z,                       # Eje Z: Riesgo Relativo (53x21)
  xlab = "Temperatura (°C)", 
  ylab = "Lag (semanas)", 
  zlab = "RR", 
  theta = 60, phi = 40,        # Ángulos de perspectiva
  col = "skyblue",             # Color
  shade = 0.5,                 # Sombras
  ticktype = "detailed",       # Estilo de ticks
  main = "Efecto de la Temperatura mínima y Lag en Santa Marta"
)










####################
#-------------------------------------------------------------------------------
#BARRANQUILLA
#-------------------------------------------------------------------------------

# Crear una base de retardos (lags) para la temperatura mínima en Barranquilla
basis_temp_baq <- crossbasis(
  datos_dlnm$BAQ_min_temperature, 
  lag = 30, # Hasta 20 semanas de retardo
  argvar = list(fun = "ns", df = 4), # Relación no lineal con la temperatura
  arglag = list(fun = "ns", df = 4) # Relación no lineal con el lag
)

# Ajustar el modelo para incluir la base de retardos
modelo_baq <- glm.nb(
  Casos_Barranquilla ~ basis_temp_baq + 
    ns(BAQ_avg_specific_humidity, df = 4) + 
    ns(BAQ_total_precipitation, df = 4) + 
    ns(Semana_Epidemiologica, df = 4) + 
    factor(Año),
  data = datos_dlnm
)

# Predicción del riesgo relativo (RR) según temperatura y desfase
pred_temp_baq <- crosspred(
  basis_temp_baq, 
  model = modelo_baq, 
  cen = mean(datos_dlnm$BAQ_min_temperature, na.rm = TRUE), # Centrar en la temperatura promedio
  bylag = 1, # Resolución del lag
  by = 0.1 # Resolución de temperatura
)


# Verifica que las dimensiones de la matriz z coincidan con x e y
z <- pred_temp_baq$matRRfit  # Matriz RR (53x31)

# Generar gráfico 3D con persp()
tem_min <- persp(
  x = pred_temp_baq$predvar,   # Eje X: Temperatura
  y = seq(0, 30, by = 1),      # Eje Y: Lag (21 valores)
  z = z,                       # Eje Z: Riesgo Relativo (53x21)
  xlab = "Temperatura (°C)", 
  ylab = "Lag (semanas)", 
  zlab = "RR", 
  theta = 60, phi = 40,        # Ángulos de perspectiva
  col = "skyblue",             # Color
  shade = 0.5,                 # Sombras
  ticktype = "detailed",       # Estilo de ticks
  main = "Efecto de la Temperatura mínima y el Lag en Barranquilla"
)



# Crear una base de retardos (lags) para la temperatura máxima en Barranquilla
basis_temp_baq <- crossbasis(
  datos_dlnm$BAQ_max_temperature, 
  lag = 30, # Hasta 20 semanas de retardo
  argvar = list(fun = "ns", df = 4), # Relación no lineal con la temperatura
  arglag = list(fun = "ns", df = 4) # Relación no lineal con el lag
)

# Ajustar el modelo para incluir la base de retardos
modelo_baq <- glm.nb(
  Casos_Barranquilla ~ basis_temp_baq + 
    ns(BAQ_avg_specific_humidity, df = 4) + 
    ns(BAQ_total_precipitation, df = 4) + 
    ns(Semana_Epidemiologica, df = 4) + 
    factor(Año),
  data = datos_dlnm
)

# Predicción del riesgo relativo (RR) según temperatura y desfase
pred_temp_baq <- crosspred(
  basis_temp_baq, 
  model = modelo_baq, 
  cen = mean(datos_dlnm$BAQ_max_temperature, na.rm = TRUE), # Centrar en la temperatura promedio
  bylag = 1, # Resolución del lag
  by = 0.1 # Resolución de temperatura
)


# Verifica que las dimensiones de la matriz z coincidan con x e y
z <- pred_temp_baq$matRRfit  # Matriz RR (53x31)

# Generar gráfico 3D con persp()
tem_min <- persp(
  x = pred_temp_baq$predvar,   # Eje X: Temperatura
  y = seq(0, 30, by = 1),      # Eje Y: Lag (21 valores)
  z = z,                       # Eje Z: Riesgo Relativo (53x21)
  xlab = "Temperatura (°C)", 
  ylab = "Lag (semanas)", 
  zlab = "RR", 
  theta = 60, phi = 40,        # Ángulos de perspectiva
  col = "skyblue",             # Color
  shade = 0.5,                 # Sombras
  ticktype = "detailed",       # Estilo de ticks
  main = "Efecto de la Temperatura máxima y el Lag en Barranquilla"
)


