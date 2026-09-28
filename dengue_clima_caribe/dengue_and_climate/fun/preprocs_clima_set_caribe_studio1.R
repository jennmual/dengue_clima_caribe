#==================================================================
#
# DATA SET - CLIMATE VARIABLES
# Preprocesamiento desde data set de sally
# 22 noviembre 2024
#
#----------------------------------------------
# Parte 1: Criterios de inclusión del estudio
#---------------------------------------------
#
#==================================================================


rm(list=ls())

# Cargar las librerías necesarias
library(dplyr)
library(lubridate)

##################
# LAS 3 CIUDADES
##################

# Cargar y leer el archivo
Barranquilla_clima_data_1950_2023 <- readRDS("data/caribe_data/Barranquilla_clima_data_1950_2023.rds")
Cartagena_de_Indias_clima_data_1950_2023 <- readRDS("data/caribe_data/Cartagena_de_Indias_clima_data_1950_2023.rds")
Santa_Marta_clima_data_1950_2023 <- readRDS("data/caribe_data/Santa_Marta_clima_data_1950_2023.rds")

dataset_clima<-Cartagena_de_Indias_clima_data_1950_2023

# Seleccionar variable según fecha
dataset_clima$Date <- as.Date(dataset_clima$Date)
dataset_clima$anio <- as.numeric(format(dataset_clima$Date, "%Y"))

# Filtrar las filas cuyo año sea mayor o igual a 2008
dataset_clima <- dataset_clima[dataset_clima$anio >= 2008, ]

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
dataset_clima_2008_2023 <- dataset_clima[, c("Date","2m_temperature_daymax", "2m_temperature_daymin", "2m_temperature", "specific_humidity","relative_humidity","total_precipitation")]



#------------------------------------------
# Obtener valor por semana epidemiológica
#------------------------------------------

# Crear la columna de semana epidemiológica y año
dataset_clima_2008_2023 <- dataset_clima_2008_2023 %>%
                            mutate(
                              epi_week = epiweek(Date),  # Calcula la semana epidemiológica (epiweek)
                              year = year(Date)          # Extrae el año
                            )

#dataset_clima_2008_2023$epi_week <- ifelse(epi_week==53, 1, epi_week ) #2009, 2010,2015,2016,2021 no 2020

# Agrupar por año y semana, y calcular los valores semanales
dataset_clima_2008_2023_semanal <- dataset_clima_2008_2023 %>%
                                    group_by(year, epi_week) %>%
                                    summarise(
                                      max_temperature = max(`2m_temperature_daymax`, na.rm = TRUE),  # Máximo semanal
                                      min_temperature = min(`2m_temperature_daymin`, na.rm = TRUE),  # Mínimo semanal
                                      avg_temperature = mean(`2m_temperature`, na.rm = TRUE),        # Promedio semanal
                                      avg_specific_humidity = mean(specific_humidity, na.rm = TRUE), # Promedio semanal
                                      avg_relative_humidity = mean(relative_humidity, na.rm = TRUE), # Promedio semanal
                                      total_precipitation = sum(total_precipitation, na.rm = TRUE),  # Suma semanal
                                      .groups = "drop" # Evita mensajes sobre agrupamientos
                                    )

# Cambiar el nombre de la ciudad y guardar el nuevo dataframe
saveRDS(dataset_clima_2008_2023_semanal, file = "data/caribe_data/Sem_cartagena_clima_data_2008_2023.rds")





#--------------------------------------------------------------------------------

##################
# BARRANQUILLA
##################

# Cargar y leer el archivo
Barranquilla_clima_data_1950_2023 <- readRDS("data/caribe_data/Barranquilla_clima_data_1950_2023.rds")

# Seleccionar variable según fecha
Barranquilla_clima_data_1950_2023$Date <- as.Date(Barranquilla_clima_data_1950_2023$Date)
Barranquilla_clima_data_1950_2023$anio <- as.numeric(format(Barranquilla_clima_data_1950_2023$Date, "%Y"))

# Filtrar las filas cuyo año sea mayor o igual a 2009
Barranquilla_clima_data_1950_2023 <- Barranquilla_clima_data_1950_2023[Barranquilla_clima_data_1950_2023$anio >= 2008, ]
Barranquilla_clima_data_1950_2023 <- Barranquilla_clima_data_1950_2023 %>%
                          mutate(
                            total_precipitation_PERSIANN_CDR_cos = if_else(total_precipitation_PERSIANN_CDR_cos == -9999 , NA, total_precipitation_PERSIANN_CDR_cos),
                            total_precipitation_PERSIANN_CDR_pop_2000 = if_else(total_precipitation_PERSIANN_CDR_pop_2000 == -9999 , NA, total_precipitation_PERSIANN_CDR_pop_2000),
                            total_precipitation_PERSIANN_CDR_pop_2005 = if_else(total_precipitation_PERSIANN_CDR_pop_2005 == -9999 , NA, total_precipitation_PERSIANN_CDR_pop_2005),
                            total_precipitation_PERSIANN_CDR_pop_2010 = if_else(total_precipitation_PERSIANN_CDR_pop_2010 == -9999 , NA, total_precipitation_PERSIANN_CDR_pop_2010),
                            total_precipitation_PERSIANN_CDR_pop_2015 = if_else(total_precipitation_PERSIANN_CDR_pop_2015 == -9999 , NA, total_precipitation_PERSIANN_CDR_pop_2015),
                            total_precipitation_PERSIANN_CDR_pop_2020 = if_else(total_precipitation_PERSIANN_CDR_pop_2020 == -9999 , NA, total_precipitation_PERSIANN_CDR_pop_2020)
                          )

# Crear las nuevas variables
Barranquilla_clima_data_1950_2023$`2m_temperature_daymax` <- with(Barranquilla_clima_data_1950_2023, 
                                                                  ifelse(anio < 2010, `2m_temperature_daymax_ERA5_Land_pop_2005`,
                                                                         ifelse(anio >= 2010 & anio <= 2014, `2m_temperature_daymax_ERA5_Land_pop_2010`,
                                                                                ifelse(anio >= 2015 & anio <= 2019, `2m_temperature_daymax_ERA5_Land_pop_2015`,
                                                                                       `2m_temperature_daymax_ERA5_Land_pop_2020`))))

Barranquilla_clima_data_1950_2023$`2m_temperature_daymin` <- with(Barranquilla_clima_data_1950_2023, 
                                                                  ifelse(anio < 2010, `2m_temperature_daymin_ERA5_Land_pop_2005`,
                                                                         ifelse(anio >= 2010 & anio <= 2014, `2m_temperature_daymin_ERA5_Land_pop_2010`,
                                                                                ifelse(anio >= 2015 & anio <= 2019, `2m_temperature_daymin_ERA5_Land_pop_2015`,
                                                                                       `2m_temperature_daymin_ERA5_Land_pop_2020`))))

Barranquilla_clima_data_1950_2023$`2m_temperature` <- with(Barranquilla_clima_data_1950_2023, 
                                                           ifelse(anio < 2010, `2m_temperature_ERA5_Land_pop_2005`,
                                                                  ifelse(anio >= 2010 & anio <= 2014, `2m_temperature_ERA5_Land_pop_2010`,
                                                                         ifelse(anio >= 2015 & anio <= 2019, `2m_temperature_ERA5_Land_pop_2015`,
                                                                                `2m_temperature_ERA5_Land_pop_2020`))))

Barranquilla_clima_data_1950_2023$specific_humidity <- with(Barranquilla_clima_data_1950_2023, 
                                                            ifelse(anio < 2010, `specific_humidity_ERA5_Land_pop_2005`,
                                                                   ifelse(anio >= 2010 & anio <= 2014, `specific_humidity_ERA5_Land_pop_2010`,
                                                                          ifelse(anio >= 2015 & anio <= 2019, `specific_humidity_ERA5_Land_pop_2015`,
                                                                                 `specific_humidity_ERA5_Land_pop_2020`))))

Barranquilla_clima_data_1950_2023$relative_humidity <- with(Barranquilla_clima_data_1950_2023, 
                                                            ifelse(anio < 2010, `relative_humidity_ERA5_Land_pop_2005`,
                                                                   ifelse(anio >= 2010 & anio <= 2014, `relative_humidity_ERA5_Land_pop_2010`,
                                                                          ifelse(anio >= 2015 & anio <= 2019, `relative_humidity_ERA5_Land_pop_2015`,
                                                                                 `relative_humidity_ERA5_Land_pop_2020`))))

Barranquilla_clima_data_1950_2023$total_precipitation <- with(Barranquilla_clima_data_1950_2023, 
                                                              ifelse(anio < 2010, `total_precipitation_PERSIANN_CDR_pop_2005`,
                                                                     ifelse(anio >= 2010 & anio <= 2014, `total_precipitation_PERSIANN_CDR_pop_2010`,
                                                                            ifelse(anio >= 2015 & anio <= 2019, `total_precipitation_PERSIANN_CDR_pop_2015`,
                                                                                   `total_precipitation_PERSIANN_CDR_pop_2020`))))



# Crear un nuevo dataframe con solo la variable Date y las nuevas variables
Barranquilla_clima_data_2008_2023 <- Barranquilla_clima_data_1950_2023[, c("Date","2m_temperature_daymax", "2m_temperature_daymin", "2m_temperature", "specific_humidity","relative_humidity","total_precipitation")]



#------------------------------------------
# Obtener valor por semana epidemiológica
#------------------------------------------

# Crear la columna de semana epidemiológica y año
Barranquilla_clima_data_2008_2023 <- Barranquilla_clima_data_2008_2023 %>%
  mutate(
    epi_week = epiweek(Date),  # Calcula la semana epidemiológica 
    year = year(Date)          # Extrae el año
  )


#==========================
# Ajuste de semanas epi
#==========================

Barranquilla_clima_data_2008_2023 <- Barranquilla_clima_data_2008_2023 %>%
  mutate(
    epi_week = case_when(
      Date %in% as.Date(c("2009-01-01", "2009-01-02", "2009-01-03",
                          "2015-01-01", "2015-01-02", "2015-01-03")) ~ 53,
      Date %in% as.Date(c("2010-01-01", "2010-01-02",
                          "2011-01-01",
                          "2016-01-01","2016-01-01",
                          "2022-01-01")) ~ 52,
      Date %in% as.Date(c("2010-01-03",
                          "2012-01-01", "2012-01-02", "2012-01-03",
                          "2012-12-30","2012-12-31",
                          "2013-12-29","2013-12-30","2013-12-31",
                          "2017-12-31",
                          "2018-12-30","2018-12-31",
                          "2019-12-29","2019-12-30","2019-12-31")) ~ 1,
      TRUE ~ epi_week
    ),
    year = case_when(
      Date %in% as.Date(c("2009-01-01", "2009-01-02", "2009-01-03")) ~ 2008,
      Date %in% as.Date(c("2010-01-01", "2010-01-02")) ~ 2009,
      Date %in% as.Date("2011-01-01") ~ 2010,
      Date %in% as.Date(c("2012-12-30","2012-12-31")) ~ 2013,
      Date %in% as.Date(c("2013-12-29","2013-12-30", "2013-12-31")) ~ 2014,
      Date %in% as.Date(c("2015-01-01", "2015-01-02", "2015-01-03")) ~ 2014,
      Date %in% as.Date(c("2016-01-01","2016-01-02")) ~ 2015,
      Date %in% as.Date("2017-12-31") ~ 2018,
      Date %in% as.Date(c("2018-12-30","2018-12-31")) ~ 2019,
      Date %in% as.Date(c("2019-12-29","2019-12-30","2019-12-31")) ~ 2020,
      Date %in% as.Date(c("2021-01-01","2021-01-02")) ~ 2020,
      Date %in% as.Date("2022-01-01") ~ 2021,
      Date %in% as.Date("2023-12-31") ~ 2024,
      TRUE ~ year
    )
  )
    

#Barranquilla_clima_data_trun<-Barranquilla_clima_data_2008_2023 %>%  filter(year>=2021 & year<=2024)


# Agrupar por año y semana, y calcular los valores semanales
Barranquilla_clima_data_2008_2023_semanal <- Barranquilla_clima_data_2008_2023 %>%
  group_by(year, epi_week) %>%
  summarise(
    BAQ_max_temperature = max(`2m_temperature_daymax`, na.rm = TRUE),  # Máximo semanal
    BAQ_min_temperature = min(`2m_temperature_daymin`, na.rm = TRUE),  # Mínimo semanal
    BAQ_avg_temperature = mean(`2m_temperature`, na.rm = TRUE),        # Promedio semanal
    BAQ_avg_specific_humidity = mean(specific_humidity, na.rm = TRUE), # Promedio semanal
    BAQ_avg_relative_humidity = mean(relative_humidity, na.rm = TRUE), # Promedio semanal
    BAQ_total_precipitation = sum(total_precipitation, na.rm = TRUE),  # Suma semanal
    .groups = "drop" # Evita mensajes sobre agrupamientos
  )

# Guardar el nuevo dataframe
saveRDS(Barranquilla_clima_data_2008_2023_semanal, file = "data/caribe_data/Sem_Barranquilla_clima_data_2008_2023.rds")



##################
# CARTAGENA
##################

# Cargar y leer el archivo
Cartagena_de_Indias_clima_data_1950_2023 <- readRDS("data/caribe_data/Cartagena_de_Indias_clima_data_1950_2023.rds")
Cartagena_de_Indias_clima_data_1950_2023 <- Cartagena_de_Indias_clima_data_1950_2023 %>%
  mutate(
    total_precipitation_PERSIANN_CDR_cos = if_else(total_precipitation_PERSIANN_CDR_cos == -9999 , NA, total_precipitation_PERSIANN_CDR_cos),
    total_precipitation_PERSIANN_CDR_pop_2000 = if_else(total_precipitation_PERSIANN_CDR_pop_2000 == -9999 , NA, total_precipitation_PERSIANN_CDR_pop_2000),
    total_precipitation_PERSIANN_CDR_pop_2005 = if_else(total_precipitation_PERSIANN_CDR_pop_2005 == -9999 , NA, total_precipitation_PERSIANN_CDR_pop_2005),
    total_precipitation_PERSIANN_CDR_pop_2010 = if_else(total_precipitation_PERSIANN_CDR_pop_2010 == -9999 , NA, total_precipitation_PERSIANN_CDR_pop_2010),
    total_precipitation_PERSIANN_CDR_pop_2015 = if_else(total_precipitation_PERSIANN_CDR_pop_2015 == -9999 , NA, total_precipitation_PERSIANN_CDR_pop_2015),
    total_precipitation_PERSIANN_CDR_pop_2020 = if_else(total_precipitation_PERSIANN_CDR_pop_2020 == -9999 , NA, total_precipitation_PERSIANN_CDR_pop_2020)
  )

# Seleccionar variable según fecha
Cartagena_de_Indias_clima_data_1950_2023$Date <- as.Date(Cartagena_de_Indias_clima_data_1950_2023$Date)
Cartagena_de_Indias_clima_data_1950_2023$anio <- as.numeric(format(Cartagena_de_Indias_clima_data_1950_2023$Date, "%Y"))

# Filtrar las filas cuyo año sea mayor o igual a 2009
Cartagena_de_Indias_clima_data_1950_2023 <- Cartagena_de_Indias_clima_data_1950_2023[Cartagena_de_Indias_clima_data_1950_2023$anio >= 2008, ]

# Crear las nuevas variables
Cartagena_de_Indias_clima_data_1950_2023$`2m_temperature_daymax` <- with(Cartagena_de_Indias_clima_data_1950_2023, 
                                                                  ifelse(anio < 2010, `2m_temperature_daymax_ERA5_Land_pop_2005`,
                                                                         ifelse(anio >= 2010 & anio <= 2014, `2m_temperature_daymax_ERA5_Land_pop_2010`,
                                                                                ifelse(anio >= 2015 & anio <= 2019, `2m_temperature_daymax_ERA5_Land_pop_2015`,
                                                                                       `2m_temperature_daymax_ERA5_Land_pop_2020`))))

Cartagena_de_Indias_clima_data_1950_2023$`2m_temperature_daymin` <- with(Cartagena_de_Indias_clima_data_1950_2023, 
                                                                  ifelse(anio < 2010, `2m_temperature_daymin_ERA5_Land_pop_2005`,
                                                                         ifelse(anio >= 2010 & anio <= 2014, `2m_temperature_daymin_ERA5_Land_pop_2010`,
                                                                                ifelse(anio >= 2015 & anio <= 2019, `2m_temperature_daymin_ERA5_Land_pop_2015`,
                                                                                       `2m_temperature_daymin_ERA5_Land_pop_2020`))))

Cartagena_de_Indias_clima_data_1950_2023$`2m_temperature` <- with(Cartagena_de_Indias_clima_data_1950_2023, 
                                                           ifelse(anio < 2010, `2m_temperature_ERA5_Land_pop_2005`,
                                                                  ifelse(anio >= 2010 & anio <= 2014, `2m_temperature_ERA5_Land_pop_2010`,
                                                                         ifelse(anio >= 2015 & anio <= 2019, `2m_temperature_ERA5_Land_pop_2015`,
                                                                                `2m_temperature_ERA5_Land_pop_2020`))))

Cartagena_de_Indias_clima_data_1950_2023$specific_humidity <- with(Cartagena_de_Indias_clima_data_1950_2023, 
                                                            ifelse(anio < 2010, `specific_humidity_ERA5_Land_pop_2005`,
                                                                   ifelse(anio >= 2010 & anio <= 2014, `specific_humidity_ERA5_Land_pop_2010`,
                                                                          ifelse(anio >= 2015 & anio <= 2019, `specific_humidity_ERA5_Land_pop_2015`,
                                                                                 `specific_humidity_ERA5_Land_pop_2020`))))

Cartagena_de_Indias_clima_data_1950_2023$relative_humidity <- with(Cartagena_de_Indias_clima_data_1950_2023, 
                                                            ifelse(anio < 2010, `relative_humidity_ERA5_Land_pop_2005`,
                                                                   ifelse(anio >= 2010 & anio <= 2014, `relative_humidity_ERA5_Land_pop_2010`,
                                                                          ifelse(anio >= 2015 & anio <= 2019, `relative_humidity_ERA5_Land_pop_2015`,
                                                                                 `relative_humidity_ERA5_Land_pop_2020`))))

Cartagena_de_Indias_clima_data_1950_2023$total_precipitation <- with(Cartagena_de_Indias_clima_data_1950_2023, 
                                                              ifelse(anio < 2010, `total_precipitation_PERSIANN_CDR_pop_2005`,
                                                                     ifelse(anio >= 2010 & anio <= 2014, `total_precipitation_PERSIANN_CDR_pop_2010`,
                                                                            ifelse(anio >= 2015 & anio <= 2019, `total_precipitation_PERSIANN_CDR_pop_2015`,
                                                                                   `total_precipitation_PERSIANN_CDR_pop_2020`))))



# Crear un nuevo dataframe con solo la variable Date y las nuevas variables
Cartagena_de_Indias_clima_data_2008_2023 <- Cartagena_de_Indias_clima_data_1950_2023[, c("Date","2m_temperature_daymax", "2m_temperature_daymin", "2m_temperature", "specific_humidity","relative_humidity","total_precipitation")]



#------------------------------------------
# Obtener valor por semana epidemiológica
#------------------------------------------

# Crear la columna de semana epidemiológica y año
Cartagena_de_Indias_clima_data_2008_2023 <- Cartagena_de_Indias_clima_data_2008_2023 %>%
                                            mutate(
                                              epi_week = epiweek(Date),  # Calcula la semana epidemiológica (epiweek)
                                              year = year(Date)          # Extrae el año
                                            )

#==========================
# Ajuste de semanas epi
#==========================

Cartagena_de_Indias_clima_data_2008_2023 <- Cartagena_de_Indias_clima_data_2008_2023 %>%
  mutate(
    epi_week = case_when(
      Date %in% as.Date(c("2009-01-01", "2009-01-02", "2009-01-03",
                          "2015-01-01", "2015-01-02", "2015-01-03")) ~ 53,
      Date %in% as.Date(c("2010-01-01", "2010-01-02",
                          "2011-01-01",
                          "2016-01-01","2016-01-01",
                          "2022-01-01")) ~ 52,
      Date %in% as.Date(c("2010-01-03",
                          "2012-01-01", "2012-01-02", "2012-01-03",
                          "2012-12-30","2012-12-31",
                          "2013-12-29","2013-12-30","2013-12-31",
                          "2017-12-31",
                          "2018-12-30","2018-12-31",
                          "2019-12-29","2019-12-30","2019-12-31")) ~ 1,
      TRUE ~ epi_week
    ),
    year = case_when(
      Date %in% as.Date(c("2009-01-01", "2009-01-02", "2009-01-03")) ~ 2008,
      Date %in% as.Date(c("2010-01-01", "2010-01-02")) ~ 2009,
      Date %in% as.Date("2011-01-01") ~ 2010,
      Date %in% as.Date(c("2012-12-30","2012-12-31")) ~ 2013,
      Date %in% as.Date(c("2013-12-29","2013-12-30", "2013-12-31")) ~ 2014,
      Date %in% as.Date(c("2015-01-01", "2015-01-02", "2015-01-03")) ~ 2014,
      Date %in% as.Date(c("2016-01-01","2016-01-02")) ~ 2015,
      Date %in% as.Date("2017-12-31") ~ 2018,
      Date %in% as.Date(c("2018-12-30","2018-12-31")) ~ 2019,
      Date %in% as.Date(c("2019-12-29","2019-12-30","2019-12-31")) ~ 2020,
      Date %in% as.Date(c("2021-01-01","2021-01-02")) ~ 2020,
      Date %in% as.Date("2022-01-01") ~ 2021,
      Date %in% as.Date("2023-12-31") ~ 2024,
      TRUE ~ year
    )
  )




# Agrupar por año y semana, y calcular los valores semanales
Cartagena_de_Indias_clima_data_2008_2023_semanal <- Cartagena_de_Indias_clima_data_2008_2023 %>%
                                                    group_by(year, epi_week) %>%
                                                    summarise(
                                                      CTG_max_temperature = max(`2m_temperature_daymax`, na.rm = TRUE),  # Máximo semanal
                                                      CTG_min_temperature = min(`2m_temperature_daymin`, na.rm = TRUE),  # Mínimo semanal
                                                      CTG_avg_temperature = mean(`2m_temperature`, na.rm = TRUE),        # Promedio semanal
                                                      CTG_avg_specific_humidity = mean(specific_humidity, na.rm = TRUE), # Promedio semanal
                                                      CTG_avg_relative_humidity = mean(relative_humidity, na.rm = TRUE), # Promedio semanal
                                                      CTG_total_precipitation = sum(total_precipitation, na.rm = TRUE),  # Suma semanal
                                                      .groups = "drop" # Evita mensajes sobre agrupamientos
                                                    )

# Guardar el nuevo dataframe
saveRDS(Cartagena_de_Indias_clima_data_2008_2023_semanal, file = "data/caribe_data/Sem_Cartagena_clima_data_2008_2023.rds")



##################
# SANTA MARTA
##################

# Cargar y leer el archivo
Santa_Marta_clima_data_1950_2023 <- readRDS("data/caribe_data/Santa_Marta_clima_data_1950_2023.rds")
Santa_Marta_clima_data_1950_2023 <- Santa_Marta_clima_data_1950_2023 %>%
  mutate(
    total_precipitation_PERSIANN_CDR_cos = if_else(total_precipitation_PERSIANN_CDR_cos == -9999 , NA, total_precipitation_PERSIANN_CDR_cos),
    total_precipitation_PERSIANN_CDR_pop_2000 = if_else(total_precipitation_PERSIANN_CDR_pop_2000 == -9999 , NA, total_precipitation_PERSIANN_CDR_pop_2000),
    total_precipitation_PERSIANN_CDR_pop_2005 = if_else(total_precipitation_PERSIANN_CDR_pop_2005 == -9999 , NA, total_precipitation_PERSIANN_CDR_pop_2005),
    total_precipitation_PERSIANN_CDR_pop_2010 = if_else(total_precipitation_PERSIANN_CDR_pop_2010 == -9999 , NA, total_precipitation_PERSIANN_CDR_pop_2010),
    total_precipitation_PERSIANN_CDR_pop_2015 = if_else(total_precipitation_PERSIANN_CDR_pop_2015 == -9999 , NA, total_precipitation_PERSIANN_CDR_pop_2015),
    total_precipitation_PERSIANN_CDR_pop_2020 = if_else(total_precipitation_PERSIANN_CDR_pop_2020 == -9999 , NA, total_precipitation_PERSIANN_CDR_pop_2020)
  )


# Seleccionar variable según fecha
Santa_Marta_clima_data_1950_2023$Date <- as.Date(Santa_Marta_clima_data_1950_2023$Date)
Santa_Marta_clima_data_1950_2023$anio <- as.numeric(format(Santa_Marta_clima_data_1950_2023$Date, "%Y"))

# Filtrar las filas cuyo año sea mayor o igual a 2009
Santa_Marta_clima_data_1950_2023 <- Santa_Marta_clima_data_1950_2023[Santa_Marta_clima_data_1950_2023$anio >= 2008, ]

# Crear las nuevas variables
Santa_Marta_clima_data_1950_2023$`2m_temperature_daymax` <- with(Santa_Marta_clima_data_1950_2023, 
                                                                  ifelse(anio < 2010, `2m_temperature_daymax_ERA5_Land_pop_2005`,
                                                                         ifelse(anio >= 2010 & anio <= 2014, `2m_temperature_daymax_ERA5_Land_pop_2010`,
                                                                                ifelse(anio >= 2015 & anio <= 2019, `2m_temperature_daymax_ERA5_Land_pop_2015`,
                                                                                       `2m_temperature_daymax_ERA5_Land_pop_2020`))))

Santa_Marta_clima_data_1950_2023$`2m_temperature_daymin` <- with(Santa_Marta_clima_data_1950_2023, 
                                                                  ifelse(anio < 2010, `2m_temperature_daymin_ERA5_Land_pop_2005`,
                                                                         ifelse(anio >= 2010 & anio <= 2014, `2m_temperature_daymin_ERA5_Land_pop_2010`,
                                                                                ifelse(anio >= 2015 & anio <= 2019, `2m_temperature_daymin_ERA5_Land_pop_2015`,
                                                                                       `2m_temperature_daymin_ERA5_Land_pop_2020`))))

Santa_Marta_clima_data_1950_2023$`2m_temperature` <- with(Santa_Marta_clima_data_1950_2023, 
                                                           ifelse(anio < 2010, `2m_temperature_ERA5_Land_pop_2005`,
                                                                  ifelse(anio >= 2010 & anio <= 2014, `2m_temperature_ERA5_Land_pop_2010`,
                                                                         ifelse(anio >= 2015 & anio <= 2019, `2m_temperature_ERA5_Land_pop_2015`,
                                                                                `2m_temperature_ERA5_Land_pop_2020`))))

Santa_Marta_clima_data_1950_2023$specific_humidity <- with(Santa_Marta_clima_data_1950_2023, 
                                                            ifelse(anio < 2010, `specific_humidity_ERA5_Land_pop_2005`,
                                                                   ifelse(anio >= 2010 & anio <= 2014, `specific_humidity_ERA5_Land_pop_2010`,
                                                                          ifelse(anio >= 2015 & anio <= 2019, `specific_humidity_ERA5_Land_pop_2015`,
                                                                                 `specific_humidity_ERA5_Land_pop_2020`))))

Santa_Marta_clima_data_1950_2023$relative_humidity <- with(Santa_Marta_clima_data_1950_2023, 
                                                            ifelse(anio < 2010, `relative_humidity_ERA5_Land_pop_2005`,
                                                                   ifelse(anio >= 2010 & anio <= 2014, `relative_humidity_ERA5_Land_pop_2010`,
                                                                          ifelse(anio >= 2015 & anio <= 2019, `relative_humidity_ERA5_Land_pop_2015`,
                                                                                 `relative_humidity_ERA5_Land_pop_2020`))))

Santa_Marta_clima_data_1950_2023$total_precipitation <- with(Santa_Marta_clima_data_1950_2023, 
                                                              ifelse(anio < 2010, `total_precipitation_PERSIANN_CDR_pop_2005`,
                                                                     ifelse(anio >= 2010 & anio <= 2014, `total_precipitation_PERSIANN_CDR_pop_2010`,
                                                                            ifelse(anio >= 2015 & anio <= 2019, `total_precipitation_PERSIANN_CDR_pop_2015`,
                                                                                   `total_precipitation_PERSIANN_CDR_pop_2020`))))



# Crear un nuevo dataframe con solo la variable Date y las nuevas variables
Santa_Marta_clima_data_2008_2023 <- Santa_Marta_clima_data_1950_2023[, c("Date","2m_temperature_daymax", "2m_temperature_daymin", "2m_temperature", "specific_humidity","relative_humidity","total_precipitation")]



#------------------------------------------
# Obtener valor por semana epidemiológica
#------------------------------------------

# Crear la columna de semana epidemiológica y año
Santa_Marta_clima_data_2008_2023 <- Santa_Marta_clima_data_2008_2023 %>%
                                    mutate(
                                      epi_week = epiweek(Date),  # Calcula la semana epidemiológica (epiweek)
                                      year = year(Date)          # Extrae el año
                                    )


#==========================
# Ajuste de semanas epi
#==========================

Santa_Marta_clima_data_2008_2023 <- Santa_Marta_clima_data_2008_2023 %>%
  mutate(
    epi_week = case_when(
      Date %in% as.Date(c("2009-01-01", "2009-01-02", "2009-01-03",
                          "2015-01-01", "2015-01-02", "2015-01-03")) ~ 53,
      Date %in% as.Date(c("2010-01-01", "2010-01-02",
                          "2011-01-01",
                          "2016-01-01","2016-01-01",
                          "2022-01-01")) ~ 52,
      Date %in% as.Date(c("2010-01-03",
                          "2012-01-01", "2012-01-02", "2012-01-03",
                          "2012-12-30","2012-12-31",
                          "2013-12-29","2013-12-30","2013-12-31",
                          "2017-12-31",
                          "2018-12-30","2018-12-31",
                          "2019-12-29","2019-12-30","2019-12-31")) ~ 1,
      TRUE ~ epi_week
    ),
    year = case_when(
      Date %in% as.Date(c("2009-01-01", "2009-01-02", "2009-01-03")) ~ 2008,
      Date %in% as.Date(c("2010-01-01", "2010-01-02")) ~ 2009,
      Date %in% as.Date("2011-01-01") ~ 2010,
      Date %in% as.Date(c("2012-12-30","2012-12-31")) ~ 2013,
      Date %in% as.Date(c("2013-12-29","2013-12-30", "2013-12-31")) ~ 2014,
      Date %in% as.Date(c("2015-01-01", "2015-01-02", "2015-01-03")) ~ 2014,
      Date %in% as.Date(c("2016-01-01","2016-01-02")) ~ 2015,
      Date %in% as.Date("2017-12-31") ~ 2018,
      Date %in% as.Date(c("2018-12-30","2018-12-31")) ~ 2019,
      Date %in% as.Date(c("2019-12-29","2019-12-30","2019-12-31")) ~ 2020,
      Date %in% as.Date(c("2021-01-01","2021-01-02")) ~ 2020,
      Date %in% as.Date("2022-01-01") ~ 2021,
      Date %in% as.Date("2023-12-31") ~ 2024,
      TRUE ~ year
    )
  )


semanas_Ver<-Santa_Marta_clima_data_2008_2023 %>% select(Date,epi_week,year)
# Agrupar por año y semana, y calcular los valores semanales
Santa_Marta_clima_data_2008_2023_semanal <- Santa_Marta_clima_data_2008_2023 %>%
                                            group_by(year, epi_week) %>%
                                            summarise(
                                              SMR_max_temperature = max(`2m_temperature_daymax`, na.rm = TRUE),  # Máximo semanal
                                              SMR_min_temperature = min(`2m_temperature_daymin`, na.rm = TRUE),  # Mínimo semanal
                                              SMR_avg_temperature = mean(`2m_temperature`, na.rm = TRUE),        # Promedio semanal
                                              SMR_avg_specific_humidity = mean(specific_humidity, na.rm = TRUE), # Promedio semanal
                                              SMR_avg_relative_humidity = mean(relative_humidity, na.rm = TRUE), # Promedio semanal
                                              SMR_total_precipitation = sum(total_precipitation, na.rm = TRUE),  # Suma semanal
                                              .groups = "drop" # Evita mensajes sobre agrupamientos
                                            )

# Guardar el nuevo dataframe
saveRDS(Santa_Marta_clima_data_2008_2023_semanal, file = "data/caribe_data/Sem_Santa_arta_clima_data_2008_2023.rds")





#=============================================
# Unir los tres dataframes con full_join
#=============================================

sem_caribe_clima_data_2008_2023 <- Sem_Barranquilla_clima_data_2008_2023 %>%
                                  full_join(Sem_Cartagena_clima_data_2008_2023, by = c("year", "epi_week")) %>%
                                  full_join(Sem_Santa_arta_clima_data_2008_2023, by = c("year", "epi_week"))


#=======================================================
# FILTRAR PARA COGER LAS ÚLTIMAS 24 SEMANANAS DEL 2008
#=======================================================

sem_caribe_clima_data_2008_2023 <- sem_caribe_clima_data_2008_2023 %>%
                                   filter(year > 2008 | (year == 2008 & epi_week >= 29))

# Guardar el nuevo dataframe
saveRDS(sem_caribe_clima_data_2008_2023, file = "data/caribe_data/sem_caribe_clima_data_2008_2023.rds")
