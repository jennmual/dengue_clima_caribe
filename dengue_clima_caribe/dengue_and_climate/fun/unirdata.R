rm(list=ls())

sem_caribe_clima_data_2009_2023 <- readRDS("data/caribe_data/sem_caribe_clima_data_2009_2023.rds")

Datosdenguef15 <- read_csv("C:/Users/jemurillo/Escritorio/Dengue_clima/Datosdenguef15.csv")
Consolidado_Semanal_Datos_Dengue_y_Clima_2009_2022_28022024= Datosdenguef15
#Consolidado_Semanal_Datos_Dengue_y_Clima_2009_2022_28022024 <- read_excel("C:/Users/jemurillo/Escritorio/Dengue_clima/Consolidado_Semanal_Datos_Dengue y Clima_2009-2022_28022024.xlsx")


# Cambiar los nombres de las columnas en Consolidado_Semanal_Datos_Dengue_y_Clima_2009_2022_28022024
Consolidado_Semanal_Datos_Dengue_y_Clima_2009_2022_28022024 <- Consolidado_Semanal_Datos_Dengue_y_Clima_2009_2022_28022024 %>%
  rename(year = Año, 
         epi_week = Semana_Epidemiologica, 
         `SMR-Humedad Relativa`=HR_SantaMarta, 
         `BAQ-Humedad Relativa`=HR_Barranquilla, 
         `CTG-Humedad Relativa`=HR_Cartagena,
         `BAQ-Temperatura media`=T_Barranquilla,
         `CTG-Temperatura media`=T_Cartagena,
         `SMR-Temperatura media`=T_SantaMarta,
         `BAQ-Lluvia acumulada`=P_Barranquilla,
         `CTG-Lluvia acumulada`=P_Cartagena,
         `SMR-Lluvia acumulada`=P_SantaMarta)

# Unir los dataframes por las columnas comunes
Consolidado_Clima_Dengue <- Consolidado_Semanal_Datos_Dengue_y_Clima_2009_2022_28022024 %>%
  left_join(sem_caribe_clima_data_2009_2023, by = c("year", "epi_week"))

# Verificar la unión
head(Consolidado_Clima_Dengue)




#-----------------------------------------

# Seleccionar las columnas necesarias y organizar los datos
data_baq_humedad <- Consolidado_Clima_Dengue %>%
  select(year, epi_week, `BAQ-Humedad Relativa`, `BAQ_avg_relative_humidity`) %>%
  mutate(SemanaEpi = year + (epi_week - 1) / 52) %>% # Crear una columna de tiempo continuo
  pivot_longer(cols = c(`BAQ-Humedad Relativa`, `BAQ_avg_relative_humidity`),
               names_to = "Variable",
               values_to = "Humedad_Relativa") %>%
  mutate(Variable = case_when(
    Variable == "BAQ-Humedad Relativa" ~ "Humedad Relativa-IDEAM",
    Variable == "BAQ_avg_relative_humidity" ~ "Humedad Relativa-ERA5",
    TRUE ~ Variable
  ))

# Crear el gráfico
ggplot(data_baq_humedad, aes(x = SemanaEpi, y = Humedad_Relativa, color = Variable)) +
  geom_line(size = 1) +
  labs(
    title = "Barranquilla (2009-2023)",
    x = "Año (Semana Epidemiológica)",
    y = "Humedad Relativa (%)",
    color = "Variable"
  ) +
  scale_color_manual(values = c(
    "Humedad Relativa-IDEAM" = "#0072B2",
    "Humedad Relativa-ERA5" = "#E69F00"
  )) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "top"
  )




# Seleccionar las columnas necesarias y organizar los datos
data_ctg_humedad <- Consolidado_Clima_Dengue %>%
  select(year, epi_week, `CTG-Humedad Relativa`, `CTG_avg_relative_humidity`) %>%
  mutate(SemanaEpi = year + (epi_week - 1) / 52) %>% # Crear una columna de tiempo continuo
  pivot_longer(cols = c(`CTG-Humedad Relativa`, `CTG_avg_relative_humidity`),
               names_to = "Variable",
               values_to = "Humedad_Relativa") %>%
  mutate(Variable = case_when(
    Variable == "CTG-Humedad Relativa" ~ "Humedad Relativa-IDEAM",
    Variable == "CTG_avg_relative_humidity" ~ "Humedad Relativa-ERA5",
    TRUE ~ Variable
  ))

# Crear el gráfico
ggplot(data_ctg_humedad, aes(x = SemanaEpi, y = Humedad_Relativa, color = Variable)) +
  geom_line(size = 1) +
  labs(
    title = "Cartagena (2009-2023)",
    x = "Año (Semana Epidemiológica)",
    y = "Humedad Relativa (%)",
    color = "Variable"
  ) +
  scale_color_manual(values = c(
    "Humedad Relativa-IDEAM" = "#0072B2",
    "Humedad Relativa-ERA5" = "#E69F00"
  )) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "top"
  )






# Seleccionar las columnas necesarias y organizar los datos
data_smr_humedad <- Consolidado_Clima_Dengue %>%
  select(
    year, epi_week,
    `SMR-Humedad Relativa`, `SMR_avg_relative_humidity`
  ) %>%
  mutate(SemanaEpi = year + (epi_week - 1) / 52) %>% # Crear una columna de tiempo continuo
  pivot_longer(
    cols = c(`SMR-Humedad Relativa`, `SMR_avg_relative_humidity`),
    names_to = "Variable",
    values_to = "Humedad_Relativa"
  ) %>%
  mutate(Variable = case_when(
    Variable == "SMR-Humedad Relativa" ~ "Humedad Relativa-IDEAM",
    Variable == "SMR_avg_relative_humidity" ~ "Humedad Relativa-ERA5",
    TRUE ~ Variable
  )) 

# Crear el gráfico
ggplot(data_smr_humedad, aes(x = SemanaEpi, y = Humedad_Relativa, color = Variable)) +
  geom_line(size = 1) +
  labs(
    title = "Santa Marta (2009-2023)",
    x = "Año (Semana Epidemiológica)",
    y = "Humedad Relativa (%)",
    color = "Variable",
    linetype = "Ciudad"
  ) +
  scale_color_manual(values = c(
    "Humedad Relativa-IDEAM" = "#0072B2",
    "Humedad Relativa-ERA5" = "#E69F00"
  )) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "top"
  )









# Seleccionar las columnas necesarias y organizar los datos
data_comparativo <- Consolidado_Clima_Dengue %>%
  select(
    year, epi_week,
    `CTG-Temperatura media`, `CTG_avg_temperature`
  ) %>%
  mutate(SemanaEpi = year + (epi_week - 1) / 52) %>% # Crear una columna de tiempo continuo
  pivot_longer(
    cols = c(`CTG-Temperatura media`, `CTG_avg_temperature`),
    names_to = "Variable",
    values_to = "Valor"
  ) %>%
  mutate(Variable = case_when(
    Variable == "CTG-Temperatura media" ~ "Temperatura media-IDEAM",
    Variable == "CTG_avg_temperature" ~ "Temperatura media-ERA5",
    TRUE ~ Variable
  )) 

# Crear el gráfico
ggplot(data_comparativo, aes(x = SemanaEpi, y = Valor, color = Variable)) +
  geom_line(size = 1) +
  labs(
    title = "Cartagena (2009-2023)",
    x = "Año (Semana Epidemiológica)",
    y = "Valor (°C)",
    color = "Variable"
  ) +
  scale_color_manual(values = c(
    "Temperatura media-IDEAM" = "#0072B2",
    "Temperatura media-ERA5" = "#E69F00" 
  )) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "top"
  )






# Seleccionar las columnas necesarias y organizar los datos
data_comparativo_BAQ <- Consolidado_Clima_Dengue %>%
  select(
    year, epi_week,
    `BAQ-Temperatura media`, `BAQ_avg_temperature`
  ) %>%
  mutate(SemanaEpi = year + (epi_week - 1) / 52) %>% # Crear una columna de tiempo continuo
  pivot_longer(
    cols = c(`BAQ-Temperatura media`, `BAQ_avg_temperature`),
    names_to = "Variable",
    values_to = "Valor"
  ) %>%
  mutate(Variable = case_when(
    Variable == "BAQ-Temperatura media" ~ "Temperatura media-IDEAM",
    Variable == "BAQ_avg_temperature" ~ "Temperatura media-ERA5",
    TRUE ~ Variable
  )) 

#data_comparativo_BAQ <- data_comparativo_BAQ %>%
                          filter(Valor < 50)


# Crear el gráfico
ggplot(data_comparativo_BAQ, aes(x = SemanaEpi, y = Valor, color = Variable)) +
  geom_line(size = 1) +
  labs(
    title = "Barranquilla (2009-2023)",
    x = "Año (Semana Epidemiológica)",
    y = "Valor (°C)",
    color = "Variable"
  ) +
  scale_color_manual(values = c(
    "Temperatura media-IDEAM" = "#0072B2",
    "Temperatura media-ERA5" = "#E69F00" 
  )) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "top"
  )



# Seleccionar las columnas necesarias y organizar los datos
data_comparativo_SMR <- Consolidado_Clima_Dengue %>%
  select(
    year, epi_week,
    `SMR-Temperatura media`, `SMR_avg_temperature`
  ) %>%
  mutate(SemanaEpi = year + (epi_week - 1) / 52) %>% # Crear una columna de tiempo continuo
  pivot_longer(
    cols = c(`SMR-Temperatura media`, `SMR_avg_temperature`),
    names_to = "Variable",
    values_to = "Valor"
  ) %>%  
  mutate(Variable = case_when(
    Variable == "SMR-Temperatura media" ~ "Temperatura media-IDEAM",
    Variable == "SMR_avg_temperature" ~ "Temperatura media-ERA5",
    TRUE ~ Variable
  )) 

# Crear el gráfico
ggplot(data_comparativo_SMR, aes(x = SemanaEpi, y = Valor, color = Variable)) +
  geom_line(size = 1) +
  labs(
    title = "Santa Marta (2009-2023)",
    x = "Año (Semana Epidemiológica)",
    y = "Valor (°C)",
    color = "Variable"
  ) +
  scale_color_manual(values = c(
    "Temperatura media-IDEAM" = "#0072B2",
    "Temperatura media-ERA5" = "#E69F00" 
  )) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "top"
  )




# Asegúrate de cargar los paquetes necesarios
library(dplyr)
library(ggplot2)
library(tidyr)

# Seleccionar las columnas necesarias y organizar los datos
data_comparativo_BAQ_precipitacion <- Consolidado_Clima_Dengue %>%
  select(
    year, epi_week,
    `BAQ-Lluvia acumulada`, `BAQ_total_precipitation`
  ) %>%
  mutate(SemanaEpi = year + (epi_week - 1) / 52) %>% # Crear una columna de tiempo continuo
  pivot_longer(
    cols = c(`BAQ-Lluvia acumulada`, `BAQ_total_precipitation`),
    names_to = "Variable",
    values_to = "Valor"
  )  %>% 
  mutate(Variable = case_when(
    Variable == "BAQ-Lluvia acumulada" ~ "Lluvia acumulada-IDEAM",
    Variable == "BAQ_total_precipitation" ~ "total_precipitation-CHIRPS",
    TRUE ~ Variable
  )) 

# Crear el gráfico
ggplot(data_comparativo_BAQ_precipitacion, aes(x = SemanaEpi, y = Valor, color = Variable)) +
  geom_line(size = 1) +
  labs(
    title = "Barranquilla (2009-2023)",
    x = "Año (Semana Epidemiológica)",
    y = "Valor (mm)",
    color = "Variable"
  ) +
  scale_color_manual(values = c(
    "Lluvia acumulada-IDEAM" = "#0072B2",
    "total_precipitation-CHIRPS" = "#E69F00" 
  )) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "top"
  )







# Seleccionar las columnas necesarias y organizar los datos
data_comparativo_CTG_precipitacion <- Consolidado_Clima_Dengue %>%
  select(
    year, epi_week,
    `CTG-Lluvia acumulada`, `CTG_total_precipitation`
  ) %>%
  mutate(SemanaEpi = year + (epi_week - 1) / 52) %>% # Crear una columna de tiempo continuo
  pivot_longer(
    cols = c(`CTG-Lluvia acumulada`, `CTG_total_precipitation`),
    names_to = "Variable",
    values_to = "Valor"
  )  %>% 
  mutate(Variable = case_when(
    Variable == "CTG-Lluvia acumulada" ~ "Lluvia acumulada-IDEAM",
    Variable == "CTG_total_precipitation" ~ "total_precipitation-CHIRPS",
    TRUE ~ Variable
  )) 

# Crear el gráfico
ggplot(data_comparativo_CTG_precipitacion, aes(x = SemanaEpi, y = Valor, color = Variable)) +
  geom_line(size = 1) +
  labs(
    title = "Cartagena (2009-2023)",
    x = "Año (Semana Epidemiológica)",
    y = "Valor (mm)",
    color = "Variable"
  ) +
  scale_color_manual(values = c(
    "Lluvia acumulada-IDEAM" = "#0072B2",
    "total_precipitation-CHIRPS" = "#E69F00" 
  )) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "top"
  )



# Seleccionar las columnas necesarias y organizar los datos
data_comparativo_SMR_precipitacion <- Consolidado_Clima_Dengue %>%
  select(
    year, epi_week,
    `SMR-Lluvia acumulada`, `SMR_total_precipitation`
  ) %>%
  mutate(SemanaEpi = year + (epi_week - 1) / 52) %>% # Crear una columna de tiempo continuo
  pivot_longer(
    cols = c(`SMR-Lluvia acumulada`, `SMR_total_precipitation`),
    names_to = "Variable",
    values_to = "Valor"
  )  %>% 
  mutate(Variable = case_when(
    Variable == "SMR-Lluvia acumulada" ~ "Lluvia acumulada-IDEAM",
    Variable == "SMR_total_precipitation" ~ "total_precipitation-CHIRPS",
    TRUE ~ Variable
  )) 

# Crear el gráfico
ggplot(data_comparativo_SMR_precipitacion, aes(x = SemanaEpi, y = Valor, color = Variable)) +
  geom_line(size = 1) +
  labs(
    title = "Santa Marta (2009-2023)",
    x = "Año (Semana Epidemiológica)",
    y = "Valor (mm)",
    color = "Variable"
  ) +
  scale_color_manual(values = c(
    "Lluvia acumulada-IDEAM" = "#0072B2",
    "total_precipitation-CHIRPS" = "#E69F00" 
  )) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "top"
  )
