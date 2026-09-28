# Cargar librerías necesarias
library(dlnm)
library(MASS) # Para modelo binomial negativo
library(splines) # Para splines naturales
library(tidyr)
library(dplyr)
library(readr)
library(pscl)
library(lme4)

##################
# Base de datos
##################
rm(list=ls())
Datosdengueclimav <- read_csv("data/caribe_data/Datosdengueclimav.csv")
data <- Datosdengueclimav
data<-rename(data,BAQ_Casos=Casos_Barranquilla,CTG_Casos=Casos_Cartagena, SMR_Casos=Casos_SantaMarta)

# Paso 1: Transformar los datos al formato largo
data_long <- data %>%
  # Seleccionar las columnas relevantes
  select(Año, Semana_Epidemiologica,
         BAQ_Casos, CTG_Casos, SMR_Casos,
         BAQ_max_temperature, BAQ_min_temperature, BAQ_avg_temperature,
         BAQ_avg_specific_humidity, BAQ_avg_relative_humidity, BAQ_total_precipitation,
         CTG_max_temperature, CTG_min_temperature, CTG_avg_temperature,
         CTG_avg_specific_humidity, CTG_avg_relative_humidity, CTG_total_precipitation,
         SMR_max_temperature, SMR_min_temperature, SMR_avg_temperature,
         SMR_avg_specific_humidity, SMR_avg_relative_humidity, SMR_total_precipitation) %>%
  # Transformar las columnas de clima al formato largo
  pivot_longer(
    cols = c(BAQ_Casos, CTG_Casos, SMR_Casos,
             BAQ_max_temperature, BAQ_min_temperature, BAQ_avg_temperature,
             BAQ_avg_specific_humidity, BAQ_avg_relative_humidity, BAQ_total_precipitation,
             CTG_max_temperature, CTG_min_temperature, CTG_avg_temperature,
             CTG_avg_specific_humidity, CTG_avg_relative_humidity, CTG_total_precipitation,
             SMR_max_temperature, SMR_min_temperature, SMR_avg_temperature,
             SMR_avg_specific_humidity, SMR_avg_relative_humidity, SMR_total_precipitation), # Columnas seleccionadas
    names_to = c("city", "variable"),  # Definir nuevos nombres de columna
    names_pattern = "([^_]+)_(.*)"  # Dividir en "city" (antes del primer "_") y "variable" (después del primer "_")
  )


# Paso 2: Separar cada variable en columnas distintas
data_wide <- data_long %>%
  pivot_wider(names_from = "variable", values_from = "value")


# Paso 3: Renombrar columnas para que coincidan con lo solicitado
data_final <- data_wide %>%
              rename(year = Año,
                     week = Semana_Epidemiologica,
                     dengue_cases = Casos,
                     temp_max = max_temperature,
                     temp_min = min_temperature,
                     temp_mean = avg_temperature,
                     humidity_mean_specific = avg_specific_humidity,
                     humidity_mean_relative = avg_relative_humidity,
                     precipitation = total_precipitation) %>%
              # Convertir `city` en factor
              mutate(city = factor(city))

#GUARDAR BASE CREADA
#saveRDS(data_final, "data/caribe_data/datosdenguetodasc.rds")


#data_final<-data_final %>% filter(year>=2021)



#==================================================================
# Examinar correlaciones entre variables climáticas (sin desfase)
#==================================================================
cor_matrix <- cor(data_final %>%
                    select(temp_max, temp_min, temp_mean, 
                           humidity_mean_relative, 
                           humidity_mean_specific, 
                           precipitation), 
                  use = "complete.obs")
print(cor_matrix)

# Visualizar la matriz de correlación
library(ggcorrplot)
ggcorrplot(cor_matrix, hc.order = TRUE, type = "lower", lab = TRUE)

#multicolinealidad
# Revisa las correlaciones simples
cor(data_final[, c("temp_max", "temp_min", "temp_mean", 
                   "humidity_mean_relative", "humidity_mean_specific", 
                   "precipitation")], use = "complete.obs")

# Calcula los VIF para identificar multicolinealidad
model_simple <- lm(dengue_cases ~ temp_max + temp_min + temp_mean +
                     humidity_mean_relative + humidity_mean_specific +
                     precipitation, 
                   data = data_final)
vif(model_simple)


model_reduced <- lm(dengue_cases ~ temp_mean +
                      humidity_mean_relative +
                      precipitation, 
                    data = data_final)# se eligen estas variables para el modelo
vif(model_reduced)


#===========================================
# Crear variables para la base cruzada
#===========================================

# Crear bases cruzadas para cada covariable climática
cb_temp_max <- crossbasis(data_final$temp_max,      # Variable climática
                          lag = 24,                       # Desfase de 0 a 12 semanas
                          argvar = list(fun = "ns", df = 3),  # Natural splines con 4 grados de libertad para la variable
                          arglag = list(fun = "ns", df = 2)   # Natural splines con 3 grados de libertad para el desfase
                          )
#summary(cb_temp_max)

cb_temp_min <- crossbasis(data_final$temp_min, 
                          lag = 24,
                          argvar = list(fun = "ns", df = 3),  # Natural splines con 4 grados de libertad para la variable
                          arglag = list(fun = "ns", df = 2)   # Natural splines con 3 grados de libertad para el desfase
                          )
#summary(cb_temp_min)

cb_temp_mean <- crossbasis(data_final$temp_mean, 
                           lag = 24,
                           argvar = list(fun = "ns", df = 3),  # Natural splines con 4 grados de libertad para la variable
                           arglag = list(fun = "ns", df = 2)   # Natural splines con 3 grados de libertad para el desfase
                           )
#summary(cb_temp_mean)


cb_humidity_relative <- crossbasis(data_final$humidity_mean_relative, 
                                   lag = 24,
                                   argvar = list(fun = "ns", df = 3),
                                   arglag = list(fun = "ns", df = 2))
#summary(cb_humidity_relative)


cb_humidity_specific <- crossbasis(data_final$humidity_mean_specific, 
                                   lag = 24,
                                   argvar = list(fun = "ns", df = 3), 
                                   arglag = list(fun = "ns", df = 2))
#summary(cb_humidity_specific)


cb_precip <- crossbasis(data_final$precipitation, 
                        lag = 24,
                        argvar = list(fun = "ns", df = 3),
                        arglag = list(fun = "poly", degree = 2))
#summary(cb_precip)



#===========================================
# Ajustar el modelo 
#===========================================
hist(data_final$dengue_cases) #Si hay muchos ceros o valores muy altos, es asimétrica positiva 
#podrías probar con un modelo de regresión binomial negativa cero-inflada.


model_bineg_0 <- glm.nb(dengue_cases ~ cb_temp_max + cb_temp_min + cb_temp_mean +
                  cb_humidity_relative + cb_humidity_specific + cb_precip +
                  ns(week, df = 7) + # Ajuste estacional por semana del año
                  factor(year)  +            # Tendencia por año
                  factor(city),             # Efecto fijo por ciudad
                  data = data_final)


model_bineg_1 <- glm.nb(dengue_cases ~ cb_temp_mean + 
                          cb_humidity_relative + 
                          cb_precip +
                          ns(week, df = 7) + 
                          factor(year) + 
                          factor(city),  # Término adicional para la ciudad
                        data = data_final)


# # Ajustar el modelo con efectos aleatorios para la ciudad
# model_bineg_random <- lmer(dengue_cases ~ cb_temp_mean + 
#                              cb_humidity_relative + 
#                              cb_precip +
#                              ns(week, df = 7) + 
#                              factor(year) + 
#                              (1 | city),  # Efecto aleatorio para la ciudad
#                            data = data_final)


model_bineg_2 <- glm.nb(dengue_cases ~ cb_temp_max +
                          cb_humidity_relative + cb_precip +
                          ns(week, df = 7) + 
                          factor(year)  +            
                          factor(city),             
                        data = data_final) #sin cb_temp_mean + cb_temp_min + cb_humidity_specific


lrtest(model_bineg_4, model_zinb) #p-valor pequeño sugiere que el ZINB es mejor

#model_bineg_0 AIC: 17040
#model_bineg_1 glm.fit: 17160
#model_bineg_2 AIC: 17190

model=model_bineg_1


#===========================================
#Validación del modelo
#===========================================
vif(model_bineg_1)


#=====================================================
#Validación CRUZADA del modelo (diferente a temporal)
#=====================================================

library(caret)

# Crear particiones de entrenamiento y prueba por año
set.seed(123)
train_index <- createDataPartition(data_final$year, p = 0.8, list = FALSE)
train_data <- data_final[train_index, ]
test_data <- data_final[-train_index, ]

# Ajustar el modelo en el conjunto de entrenamiento
model_cv <- glm.nb(dengue_cases ~ cb_temp_max + cb_temp_min + cb_temp_mean +
                     cb_humidity_relative + cb_humidity_specific + cb_precip +
                     ns(week, df = 7) + year + city,
                   data = train_data)

# Predicciones en el conjunto de prueba
test_data$predicted_cases <- predict(model_cv, newdata = test_data, type = "response")

# Evaluar el error de predicción
MAE <- mean(abs(test_data$dengue_cases - test_data$predicted_cases))
print(MAE)



#===========================================================================================
#Crear predicciones para cada base cruzada y visualizar el efecto de los lags y la variable
#===========================================================================================

#--------------------
# Temp Máxima
#--------------------
# Generar predicciones basadas en el modelo ajustado
pred_temp_max <- crosspred(
  basis = cb_temp_max,      # Base cruzada utilizada en el modelo
  model = model,            # Modelo ajustado
  cen = mean(data_final$temp_max, na.rm = TRUE),  # Valor de referencia para calcular RR
  by = 0.1                  # Incremento para evaluar efectos
)


#Efecto tridimensional (variable vs lag):
plot(pred_temp_max, 
     xlab = "Temperatura máxima (°C)", 
     zlab = "RR", 
     main = "Efecto de temperatura máxima y desfases", 
     phi = 30, theta = 230)  # Ángulos para la vista 3D

#Efecto para un desfase específico:
plot(pred_temp_max, "slices", 
     var = 35,  # Cambia según el valor de temperatura que te interese
     xlab = "Lag (semanas)", 
     ylab = "RR", 
     main = "Efecto de los desfases en temperatura máxima")


#Efecto global de la variable:
plot(pred_temp_max, 
     "overall", 
     xlab = "Temperatura máxima (°C)", 
     ylab = "RR", 
     main = "Efecto global de la temperatura máxima")


#--------------------
# Temp Media
#--------------------
# Generar predicciones basadas en el modelo ajustado
pred_temp_media <- crosspred(
  basis = cb_temp_mean,      # Base cruzada utilizada en el modelo
  model = model_bineg_1,            # Modelo ajustado
  cen = mean(data_final$temp_mean, na.rm = TRUE),  # Valor de referencia para calcular RR
  by = 0.1                  # Incremento para evaluar efectos
)

#Efecto tridimensional (variable vs lag):
plot(pred_temp_media, 
     xlab = "Temperatura media (°C)", 
     zlab = "RR", 
     main = "Efecto de temperatura media y desfases", 
     phi = 30, theta = 230)  # Ángulos para la vista 3D


#Efecto para un desfase específico:
plot(pred_temp_media, "slices", 
     var =  30.2,  # Cambia según el valor de temperatura que te interese
     xlab = "Lag (semanas)", 
     ylab = "RR", 
     main = "Efecto de los desfases en temperatura media")


#Efecto global de la variable:
plot(pred_temp_media, 
     "overall", 
     xlab = "Temperatura máxima (°C)", 
     ylab = "RR", 
     main = "Efecto global de la temperatura máxima")


#--------------------
# Humedad relativa
#--------------------
# Generar predicciones basadas en el modelo ajustado
pred_humidity_mean_relative <- crosspred(
  basis = cb_humidity_relative,      # Base cruzada utilizada en el modelo
  model = model_bineg_1,            # Modelo ajustado
  cen = mean(data_final$humidity_mean_relative, na.rm = TRUE),  # Valor de referencia para calcular RR
  by = 0.5                  # Incremento para evaluar efectos
)


#Efecto tridimensional (variable vs lag):
plot(pred_humidity_mean_relative, 
     xlab = "Humedad relativa media (%)", 
     zlab = "RR", 
     main = "Efecto de humedad relativa media  y desfases", 
     phi = 30, theta = 230)  # Ángulos para la vista 3D

#Efecto para un desfase específico:
plot(pred_humidity_mean_relative, "slices", 
     var = 90,  # Cambia según el valor de temperatura que te interese
     xlab = "Lag (semanas)", 
     ylab = "RR", 
     main = "Efecto de los desfases en humedad relativa media ")

#Efecto global de la variable:
plot(pred_humidity_mean_relative, 
     "overall", 
     xlab = "Humedad relativa media  (%)", 
     ylab = "RR", 
     main = "Efecto global de la humedad relativa media ")



#--------------------
# Precip total
#--------------------
# Generar predicciones basadas en el modelo ajustado
pred_precipitation <- crosspred(
  basis = cb_precip,      # Base cruzada utilizada en el modelo
  model = model_bineg_1,            # Modelo ajustado
  cen = mean(data_final$precipitation, na.rm = TRUE),  # Valor de referencia para calcular RR
  by = 1                  # Incremento para evaluar efectos
)


#Efecto tridimensional (variable vs lag):
plot(pred_precipitation, 
     xlab = "Precipitación total", 
     zlab = "RR", 
     main = "Efecto de precipitación total y desfases", 
     phi = 30, theta = 230)  # Ángulos para la vista 3D

#Efecto para un desfase específico:
plot(pred_precipitation, "slices", 
     var = 35,  # Cambia según el valor de temperatura que te interese
     xlab = "Lag (semanas)", 
     ylab = "RR", 
     main = "Efecto de los desfases en precipitación total")


#Efecto global de la variable:
plot(pred_precipitation, 
     "overall", 
     xlab = "Precipitación total", 
     ylab = "RR", 
     main = "Efecto global de la precipitación total")



###############################################################################
# POR CIUDAD
################################################################################

# Filtrar el modelo por ciudad y generar predicciones
pred_city_interaction <- list()  # Crear una lista para almacenar las predicciones por ciudad

# Generar predicciones por cada ciudad
for (city in cities) {
  # Filtrar los datos por ciudad (filtrar correctamente usando data_final$city)
  data_city <- subset(data_final, city == city)
  
  # Generar las predicciones para la base cruzada (ejemplo con cb_temp_mean)
  pred_city_interaction[[city]] <- crosspred(
    basis = cb_temp_mean,             # Base cruzada para la variable
    model = model_bineg_1,            # Modelo ajustado con interacción
    cen = mean(data_city$temp_mean, na.rm = TRUE),  # Centro de la variable para cada ciudad
    by = 0.1                           # Incremento para evaluar los efectos
  )
}

# Establecer la disposición de las gráficas (1 fila, 3 columnas)
par(mfrow = c(1, 3))

# Generar las gráficas para cada ciudad
for (city in cities) {
  plot(pred_city_interaction[[city]], 
       main = paste("Efectos de la temperatura media en", city),
       xlab = "Lag (semanas)", 
       ylab = "Riesgo relativo (RR)")
}
