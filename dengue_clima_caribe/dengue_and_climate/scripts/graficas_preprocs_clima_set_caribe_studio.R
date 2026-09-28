#==================================================================
#
# DATA SET - CLIMATE VARIABLES
# Preprocesamiento desde data set de sally
# 25 noviembre 2024
#
#------------------------------------------------
# Gráficas de las variables Satelitales vs IDEAM
#------------------------------------------------
#
#==================================================================

rm(list=ls())


#############################
# GRÁFICAS IDEAM VS SALLY
############################

#Base de datos:
source("scripts/preprocess_clima_caribe.R")
source("fun/fun_graph_preprocs_clima_caribe.R")

#Datosdengueclimav <- read_csv("data/caribe_data/Datosdengueclimav.csv")
Datosdengueclima <- readRDS("data/Datosdenguecliman2025.rds")

#-----------------------------
# Variable: Humedad relativa
#-----------------------------
gr_humrel2 <- graficar_varclima(
  data = Datosdengueclima,
  cols_clima_data = c("HR_SantaMarta", "HR_Barranquilla", "HR_Cartagena",
                      "HR_SantaMarta_AerSimonB","HR_Barranquilla_Flores","HR_Cartagena_AerRafaelN"), #OJO: Debe ir en el siguiente orden: Santa Marta, Barranquilla y cartagena
  grupos = c("diego_data", "airport_data"),
  colores = c( "#2E8B57","#EEC900"), # Colores para los grupos
  titulo = "Relative Humidity - Diego vs aeropuerto",
  eje_y = "(%)",
  nombre_grafica = "humedad_relativa.png"
)


#Guardar gráfico
png ("outputs/gr_humrel2.png", width = 800, height = 550)
print(gr_humrel2)
dev.off()

#----
gr_humrel3 <- graficar_varclima(
  data = Datosdengueclima,
  cols_clima_data = c("HR_SantaMarta", "HR_Barranquilla", "HR_Cartagena",
                      "SMR_avg_relative_humidity","BAQ_avg_relative_humidity", "CTG_avg_relative_humidity",
                      "HR_SantaMarta_AerSimonB","HR_Barranquilla_Flores","HR_Cartagena_AerRafaelN",
                      "HR_SantaMarta_todase","HR_Barranquilla_todase" , "HR_Cartagena_todase"), #OJO: Debe ir en el siguiente orden: Santa Marta, Barranquilla y cartagena
  grupos = c("diego_data", "sally_data", "airport_data","todas_estaciones_data"),
  colores = c("#A52A2A","#EEC900","#00688B","#2E8B57"), # Colores para los grupos
  titulo = "Relative Humidity",
  eje_y = "(%)",
  nombre_grafica = "humedad_relativa.png"
)


#Guardar gráfico
png ("outputs/gr_humrel3.png", width = 800, height = 550)
print(gr_humrel3)
dev.off()
#################################################################################


#--------------------------------
# Variable: Precipitación total
#--------------------------------

gr_precip2 <- graficar_varclima(
  data = Datosdengueclima,
  cols_clima_data = c(
    "PT_SantaMarta", "PT_Barranquilla", "PT_Cartagena",
    "PT_SantaMarta_AerSimonB", "PT_Barranquilla_Flores", "PT_Cartagena_AerRafaelN"
  ),
  grupos = c("diego_data", "airport_data"),
  colores = c( "#2E8B57","#EEC900"), # Colores para los grupos
  titulo = "Total Precipitation - Diego vs aeropuerto",
  eje_y = "(mm)",
  nombre_grafica = "total_precipitation.png"
)


#Guardar gráfico
png ("outputs/gr_precip2.png", width = 800, height = 550)
print(gr_precip2)
dev.off()

#-----------
gr_precip3 <- graficar_varclima(
  data = Datosdengueclima,
  cols_clima_data = c(
    "PT_SantaMarta", "PT_Barranquilla", "PT_Cartagena",
    "SMR_total_precipitation", "BAQ_total_precipitation", "CTG_total_precipitation",
    "PT_SantaMarta_AerSimonB", "PT_Barranquilla_Flores", "PT_Cartagena_AerRafaelN",
    "PT_SantaMarta_todase" ,"PT_Barranquilla_todase", "PT_Cartagena_AerRafaelN" 
  ),
  grupos = c("diego_data", "sally_data", "airport_data","todas_estaciones_data"),
  colores = c("#A52A2A","#EEC900","#00688B","#2E8B57"), # Colores para los grupos
  titulo = "Total Precipitation",
  eje_y = "(mm)",
  nombre_grafica = "total_precipitation.png"
)


#Guardar gráfico
png ("outputs/gr_precip3.png", width = 800, height = 550)
print(gr_precip3)
dev.off()

#################################################################################



#--------------------------------
# Variable: Temperatura media
#--------------------------------

gr_temp2 <- graficar_varclima(
  data = Datosdengueclima,
  cols_clima_data = c("TM_SantaMarta", "TM_Barranquilla", "TM_Cartagena",
                      "TM_SantaMarta_AerSimonB","TM_Barranquilla_Flores","TM_Cartagena_AerRafaelN"), #OJO: Debe ir en el siguiente orden: Santa Marta, Barranquilla y cartagena
  grupos = c("diego_data", "airport_data"),
  colores = c( "#2E8B57","#EEC900"), # Colores para los grupos
  titulo = "Average temperture - Diego vs aeropuerto",
  eje_y = "Average temperature (°C)",
  nombre_grafica = "avg_temperature.png"
)

#Guardar gráfico
png ("outputs/gr_temp2.png", width = 800, height = 550)
print(gr_temp2)
dev.off()

#-------------
gr_temp3 <- graficar_varclima(
  data = Datosdengueclima,
  cols_clima_data = c("TM_SantaMarta", "TM_Barranquilla", "TM_Cartagena",
                       "SMR_avg_temperature", "BAQ_avg_temperature","CTG_avg_temperature",
                       "TM_SantaMarta_AerSimonB","TM_Barranquilla_Flores","TM_Cartagena_AerRafaelN",
                      "TM_SantaMarta_todase","TM_Barranquilla_todase","TM_Cartagena_todase" ), #OJO: Debe ir en el siguiente orden: Santa Marta, Barranquilla y cartagena
  grupos = c("diego_data", "sally_data", "airport_data","todas_estaciones_data"),
  colores = c("#A52A2A","#EEC900","#00688B","#2E8B57"), # Colores para los grupos
  titulo = "Average temperture",
  eje_y = "Average temperature (°C)",
  nombre_grafica = "avg_temperature.png"
)

#Guardar gráfico
png ("outputs/gr_temp3.png", width = 800, height = 550)
print(gr_temp3)
dev.off()
#################################################################################

# Combinar las tres gráficas en una imagen vertical
climacomb <- gr_humrel / gr_precip / gr_temp
#Guardar gráfico
png ("outputs/climacomb.png", width = 1000, height = 1250)
print(climacomb)
dev.off()


ggplot(data = data.frame(x = 0:10, y = 20:30)) +
  geom_line(aes(x, y))  +
  scale_y_continuous(transform = "exp")



