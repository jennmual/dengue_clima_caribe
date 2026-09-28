Datos_Hidromet_Crudos_RedIDEAM_Temperatura<- read_csv("C:/Users/jemurillo/Downloads/Datos_Hidrometeorol_gicos_Crudos_-_Red_de_Estaciones_IDEAM___Temperatura_20241127.csv")

library(lubridate)
library(ggplot2)
library(tidyverse)

#Descriptivo
skim(Datos_Hidromet_Crudos_RedIDEAM_Temperatura)

Datos_Hidromet_Crudos_far<-Datos_Hidromet_Crudos_RedIDEAM_Temperatura %>% filter(NombreEstacion== "FARALLONES") %>% select("FechaObservacion")
Datos_Hidromet_Crudos_sil<-Datos_Hidromet_Crudos_RedIDEAM_Temperatura %>% filter(NombreEstacion== "SILOE") %>% select("FechaObservacion")




# Graficar la serie de tiempo
ggplot(Datos_Hidromet_Crudos_RedIDEAM_Temperatura, aes(x = FechaObservacion, y = ValorObservado)) +
  geom_line(color = "#00688B") +  # Línea de color azul preferido
  labs(
    title = "Serie de Tiempo de Valor Observado",
    x = "Fecha y Hora",
    y = "Valor Observado"
  ) +
  theme_minimal(base_size = 14)








########################
# base .gov 3 ciudades
########################

rm(list=ls())

Datos_HidrometeoroIDEAM_Temp<- read_csv("C:/Users/jemurillo/Downloads/Datos_Hidrometeorol_gicos_Crudos_-_Red_de_Estaciones_IDEAM___Temperatura_20241127.csv")
Datos_HidrometeoroIDEAM_Temp <- Datos_HidrometeoroIDEAM_Temp %>%
                                mutate(Fecha = as.Date(mdy_hms(FechaObservacion)))


######## SANTA MARTA
STM_HidromIDEAM_Temp_Sem <- Datos_HidrometeoroIDEAM_Temp %>% 
                                filter(Municipio=="SANTA MARTA") %>%
                                group_by(Fecha) %>% 
                                summarise(STM_Temprom_IDEAMgov=mean(ValorObservado)) %>%
                                mutate(
                                  epi_week = epiweek(Fecha),  # Calcula la semana epidemiológica (epiweek)
                                  year = year(Fecha)          # Extrae el año
                                )%>%
  mutate(
    epi_week = case_when(
      Fecha %in% as.Date(c("2009-01-01", "2009-01-02", "2009-01-03",
                          "2015-01-01", "2015-01-02", "2015-01-03")) ~ 53,
      Fecha %in% as.Date(c("2010-01-01", "2010-01-02",
                          "2011-01-01",
                          "2016-01-01","2016-01-01",
                          "2022-01-01")) ~ 52,
      Fecha %in% as.Date(c("2010-01-03",
                          "2012-01-01", "2012-01-02", "2012-01-03",
                          "2012-12-30","2012-12-31",
                          "2013-12-29","2013-12-30","2013-12-31",
                          "2017-12-31",
                          "2018-12-30","2018-12-31",
                          "2019-12-29","2019-12-30","2019-12-31")) ~ 1,
      TRUE ~ epi_week
    ),
    year = case_when(
      Fecha %in% as.Date(c("2009-01-01", "2009-01-02", "2009-01-03")) ~ 2008,
      Fecha %in% as.Date(c("2010-01-01", "2010-01-02")) ~ 2009,
      Fecha %in% as.Date("2011-01-01") ~ 2010,
      Fecha %in% as.Date(c("2012-12-30","2012-12-31")) ~ 2013,
      Fecha %in% as.Date(c("2013-12-29","2013-12-30", "2013-12-31")) ~ 2014,
      Fecha %in% as.Date(c("2015-01-01", "2015-01-02", "2015-01-03")) ~ 2014,
      Fecha %in% as.Date(c("2016-01-01","2016-01-02")) ~ 2015,
      Fecha %in% as.Date("2017-12-31") ~ 2018,
      Fecha %in% as.Date(c("2018-12-30","2018-12-31")) ~ 2019,
      Fecha %in% as.Date(c("2019-12-29","2019-12-30","2019-12-31")) ~ 2020,
      Fecha %in% as.Date(c("2021-01-01","2021-01-02")) ~ 2020,
      Fecha %in% as.Date("2022-01-01") ~ 2021,
      Fecha %in% as.Date("2023-12-31") ~ 2024,
      TRUE ~ year
    )
  )

Datosdenguedefi <- read_csv("data/caribe_data/Datosdenguedefi.csv") %>% 
                   select(Año,Semana_Epidemiologica,
                          ,TM_Barranquilla,TM_Cartagena,TM_SantaMarta, 
                          BAQ_max_temperature,BAQ_min_temperature,BAQ_avg_temperature,
                          CTG_max_temperature,CTG_min_temperature,CTG_avg_temperature,
                          SMR_max_temperature,SMR_min_temperature,SMR_avg_temperature)

comp_govideam_era <- STM_HidromIDEAM_Temp_Sem %>%
  full_join(Datosdenguedefi, by = c("year"="Año", "epi_week"="Semana_Epidemiologica")) 


# GRAFICAS Temperaturas prom

grtemp1 <- ggplot(comp_govideam_era, aes(x = year + epi_week / 52)) +
  geom_line(aes(y = STM_Temprom_IDEAMgov, color = "IDEAM .gov"), size = 0.5, linetype = "solid") +
  geom_line(aes(y = TM_SantaMarta, color = "IDEAM Diego"), size = 0.5, linetype = "solid") +
  geom_line(aes(y = SMR_avg_temperature, color = "ERA5"), size = 0.5, linetype = "solid") +
  labs(x = "Año", y = "Temperatura (°C)", title = "Temperatura en Santa Marta") +
  scale_x_continuous(breaks = seq(min(comp_govideam_era$year), max(comp_govideam_era$year), by = 1)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank())+
  scale_color_manual(values = c("IDEAM .gov" =  "#9A32CD", "IDEAM Diego" = "#B22222", "ERA5" = "#1E90FF"))
grtemp1







###### BARRANQUILLA

BQL_HidromIDEAM_Temp_Sem <- Datos_HidrometeoroIDEAM_Temp %>% 
  filter(Municipio=="BARRANQUILLA") %>%
  group_by(Fecha) %>% 
  summarise(BQL_Temprom_IDEAMgov=mean(ValorObservado)) %>%
  mutate(
    epi_week = epiweek(Fecha),  # Calcula la semana epidemiológica (epiweek)
    year = year(Fecha)          # Extrae el año
  )%>%
  mutate(
    epi_week = case_when(
      Fecha %in% as.Date(c("2009-01-01", "2009-01-02", "2009-01-03",
                           "2015-01-01", "2015-01-02", "2015-01-03")) ~ 53,
      Fecha %in% as.Date(c("2010-01-01", "2010-01-02",
                           "2011-01-01",
                           "2016-01-01","2016-01-01",
                           "2022-01-01")) ~ 52,
      Fecha %in% as.Date(c("2010-01-03",
                           "2012-01-01", "2012-01-02", "2012-01-03",
                           "2012-12-30","2012-12-31",
                           "2013-12-29","2013-12-30","2013-12-31",
                           "2017-12-31",
                           "2018-12-30","2018-12-31",
                           "2019-12-29","2019-12-30","2019-12-31")) ~ 1,
      TRUE ~ epi_week
    ),
    year = case_when(
      Fecha %in% as.Date(c("2009-01-01", "2009-01-02", "2009-01-03")) ~ 2008,
      Fecha %in% as.Date(c("2010-01-01", "2010-01-02")) ~ 2009,
      Fecha %in% as.Date("2011-01-01") ~ 2010,
      Fecha %in% as.Date(c("2012-12-30","2012-12-31")) ~ 2013,
      Fecha %in% as.Date(c("2013-12-29","2013-12-30", "2013-12-31")) ~ 2014,
      Fecha %in% as.Date(c("2015-01-01", "2015-01-02", "2015-01-03")) ~ 2014,
      Fecha %in% as.Date(c("2016-01-01","2016-01-02")) ~ 2015,
      Fecha %in% as.Date("2017-12-31") ~ 2018,
      Fecha %in% as.Date(c("2018-12-30","2018-12-31")) ~ 2019,
      Fecha %in% as.Date(c("2019-12-29","2019-12-30","2019-12-31")) ~ 2020,
      Fecha %in% as.Date(c("2021-01-01","2021-01-02")) ~ 2020,
      Fecha %in% as.Date("2022-01-01") ~ 2021,
      Fecha %in% as.Date("2023-12-31") ~ 2024,
      TRUE ~ year
    )
  )


comp_govideam_era2 <- BQL_HidromIDEAM_Temp_Sem %>%
  full_join(Datosdenguedefi, by = c("year"="Año", "epi_week"="Semana_Epidemiologica")) 

comp_govideam_era2 <- comp_govideam_era2 %>%
                      mutate(BQL_Temprom_IDEAMgov = if_else(BQL_Temprom_IDEAMgov <= 10, NaN, BQL_Temprom_IDEAMgov))

# GRAFICAS Temperaturas prom

grtemp2 <- ggplot(comp_govideam_era2, aes(x = year + epi_week / 52)) +
  geom_line(aes(y = BQL_Temprom_IDEAMgov, color = "IDEAM .gov"), size = 0.5, linetype = "solid") +
  geom_line(aes(y = TM_Barranquilla, color = "IDEAM Diego"), size = 0.5, linetype = "solid") +
  geom_line(aes(y = BAQ_avg_temperature, color = "ERA5"), size = 0.5, linetype = "solid") +
  labs(x = "Año", y = "Temperatura (°C)", title = "Temperatura en Barranquilla") +
  scale_x_continuous(breaks = seq(min(comp_govideam_era2$year), max(comp_govideam_era2$year), by = 1)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank())+
  scale_color_manual(values = c("IDEAM .gov" =  "#9A32CD", "IDEAM Diego" = "#B22222", "ERA5" = "#1E90FF"))
grtemp2



###### CARTAGENA

CTG_HidromIDEAM_Temp_Sem <- Datos_HidrometeoroIDEAM_Temp %>% 
  filter(Municipio=="CARTAGENA DE INDIAS") %>%
  group_by(Fecha) %>% 
  summarise(CTG_Temprom_IDEAMgov=mean(ValorObservado)) %>%
  mutate(
    epi_week = epiweek(Fecha),  # Calcula la semana epidemiológica (epiweek)
    year = year(Fecha)          # Extrae el año
  )%>%
  mutate(
    epi_week = case_when(
      Fecha %in% as.Date(c("2009-01-01", "2009-01-02", "2009-01-03",
                           "2015-01-01", "2015-01-02", "2015-01-03")) ~ 53,
      Fecha %in% as.Date(c("2010-01-01", "2010-01-02",
                           "2011-01-01",
                           "2016-01-01","2016-01-01",
                           "2022-01-01")) ~ 52,
      Fecha %in% as.Date(c("2010-01-03",
                           "2012-01-01", "2012-01-02", "2012-01-03",
                           "2012-12-30","2012-12-31",
                           "2013-12-29","2013-12-30","2013-12-31",
                           "2017-12-31",
                           "2018-12-30","2018-12-31",
                           "2019-12-29","2019-12-30","2019-12-31")) ~ 1,
      TRUE ~ epi_week
    ),
    year = case_when(
      Fecha %in% as.Date(c("2009-01-01", "2009-01-02", "2009-01-03")) ~ 2008,
      Fecha %in% as.Date(c("2010-01-01", "2010-01-02")) ~ 2009,
      Fecha %in% as.Date("2011-01-01") ~ 2010,
      Fecha %in% as.Date(c("2012-12-30","2012-12-31")) ~ 2013,
      Fecha %in% as.Date(c("2013-12-29","2013-12-30", "2013-12-31")) ~ 2014,
      Fecha %in% as.Date(c("2015-01-01", "2015-01-02", "2015-01-03")) ~ 2014,
      Fecha %in% as.Date(c("2016-01-01","2016-01-02")) ~ 2015,
      Fecha %in% as.Date("2017-12-31") ~ 2018,
      Fecha %in% as.Date(c("2018-12-30","2018-12-31")) ~ 2019,
      Fecha %in% as.Date(c("2019-12-29","2019-12-30","2019-12-31")) ~ 2020,
      Fecha %in% as.Date(c("2021-01-01","2021-01-02")) ~ 2020,
      Fecha %in% as.Date("2022-01-01") ~ 2021,
      Fecha %in% as.Date("2023-12-31") ~ 2024,
      TRUE ~ year
    )
  )



comp_govideam_era3 <- CTG_HidromIDEAM_Temp_Sem %>%
  full_join(Datosdenguedefi, by = c("year"="Año", "epi_week"="Semana_Epidemiologica")) 


# GRAFICAS Temperaturas prom

grtemp3 <- ggplot(comp_govideam_era3, aes(x = year + epi_week / 52)) +
  geom_line(aes(y = CTG_Temprom_IDEAMgov, color = "IDEAM .gov"), size = 0.5, linetype = "solid") +
  geom_line(aes(y = TM_Cartagena, color = "IDEAM Diego"), size = 0.5, linetype = "solid") +
  geom_line(aes(y = CTG_avg_temperature, color = "ERA5"), size = 0.5, linetype = "solid") +
  labs(x = "Año", y = "Temperatura (°C)", title = "Temperatura en Cartagena") +
  ylim(20,35)+
  scale_x_continuous(breaks = seq(min(comp_govideam_era3$year), max(comp_govideam_era3$year), by = 1)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank())+
  scale_color_manual(values = c("IDEAM .gov" =  "#9A32CD", "IDEAM Diego" = "#B22222", "ERA5" = "#1E90FF"))
grtemp3

# Combinar las tres gráficas en una imagen vertical
grid.arrange(grtemp1, grtemp2, grtemp3, ncol = 1)


