################################
# GRÁFICAS CLIMA
################################
#GRAFICAR DATOS PARA LOS 98 MUNICIPIOS EN EL TIEMPO

library(ggplot2)
library(dplyr)
library(tidyr)


rm(list = ls())
var_clima_mun<-readRDS("data_2007-2023_mundani/var_clima_98mun")


#=================================================
# GRÁFICA PRECIPITACIÓN
#=================================================

# 1. Preparar los datos 
datos_grafica <- var_clima_98mun %>%
  select(anio, mun_name, total_precipitation) %>%  
  filter(!is.na(mun_name))  

# 2. Crear el gráfico
precipitacion<-ggplot(datos_grafica, aes(x = anio, y = total_precipitation)) +
  geom_line(color = "#1f77b4", linewidth = 0.7) +  
  geom_point(color = "#ff7f0e", size = 1.5) +      
  facet_wrap(~ mun_name, ncol = 5) +  
  labs(
    title = "Average total annual precipitation by municipality (2007-2023)",
    x = "Year",
    y = "Total annual precipitation (mm)",
    caption = "Source: CHIRPS data"
  ) +
  theme_minimal(base_size = 8) +  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    strip.text = element_text(size = 6),  
    panel.spacing = unit(0.5, "lines")    
  )

ggsave("outputs/precipitacion_total.jpg",
       plot=precipitacion, 
       height = 12,
       width = 7,
       dpi=300)




#=================================================
# GRÁFICA HUMEDAD RELATIVA
#=================================================

# 1. Preparar los datos 
datos_grafica <- var_clima_98mun %>%
  select(anio, mun_name, relative_humidity) %>%  
  filter(!is.na(mun_name))  

# 2. Crear el gráfico
hum_relativa<-ggplot(datos_grafica, aes(x = anio, y = relative_humidity)) +
  geom_line(color = "#1f77b4", linewidth = 0.7) +  
  geom_point(color = "#ff7f0e", size = 1.5) +      
  facet_wrap(~ mun_name, ncol = 5) +  
  labs(
    title = "Annual average of relative humidity by municipality (2007-2023)",
    x = "Year",
    y = "Relative humidity (%)",
    caption = "Source: ERA5 data"
  ) +
  theme_minimal(base_size = 8) +  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  
    strip.text = element_text(size = 6),  
    panel.spacing = unit(0.5, "lines")    
  )

ggsave("outputs/humedad_relativa.jpg",
       plot=hum_relativa, 
       height = 12,
       width = 7,
       dpi=300)




#=================================================
# GRÁFICA HUMEDAD ESPECÍFICA
#=================================================

# 1. Preparar los datos 
datos_grafica <- var_clima_98mun %>%
  select(anio, mun_name, specific_humidity) %>%  
  filter(!is.na(mun_name)) 

# 2. Crear el gráfico
hum_especif<-ggplot(datos_grafica, aes(x = anio, y = specific_humidity)) +
  geom_line(color = "#1f77b4", linewidth = 0.7) +  
  geom_point(color = "#ff7f0e", size = 1.5) +      
  facet_wrap(~ mun_name,  ncol = 5) +  
  labs(
    title = "Annual average of specific humidity by municipality (2007-2023)",
    x = "Year",
    y = "Specific humidity (mm)",
    caption =  "Source: ERA5 data"
  ) +
  theme_minimal(base_size = 8) +  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  
    strip.text = element_text(size = 6),  
    panel.spacing = unit(0.5, "lines")    
  )

ggsave("outputs/hum_especifa.jpg",
       plot=hum_especif, 
       height = 12,
       width = 7,
       dpi=300)


#=================================================
# GRÁFICA TEMPERATURAS
#=================================================


# 1. Preparar los datos en formato largo (long format)
datos_grafica <- var_clima_mun %>%
  select(anio, mun_name, 
         `2m_temperature`, 
         `2m_temperature_daymin`, 
         `2m_temperature_daymax`) %>%  # Ajusta estos nombres según tus columnas
  pivot_longer(
    cols = c(`2m_temperature`, `2m_temperature_daymin`, `2m_temperature_daymax`),
    names_to = "tipo_temperatura",
    values_to = "valor"
  ) %>%
  mutate(tipo_temperatura = factor(tipo_temperatura,
                                   levels = c("2m_temperature", 
                                              "2m_temperature_daymin", 
                                              "2m_temperature_daymax"),
                                   labels = c("Average","Minimum", "Maximum")))


# 2. Crear el gráfico
temperaturas<-ggplot(datos_grafica, aes(x = anio, y = valor, color = tipo_temperatura)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 0.8) +
  facet_wrap(~ mun_name, ncol = 5) +  
  scale_color_manual(
    values = c("Minimum" = "#377eb8", "Average" = "#4daf4a", "Maximum" = "#e41a1c")
  ) +
  labs(
    title = "Annual average temperatures by municipality (2007-2023)",
    x = "Year",
    y = "temperature (°C)",
    color = "Type of temperature",
    caption = "Source: ERA5-Land data"
  ) +
  theme_minimal(base_size = 8) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 6),
    panel.spacing = unit(0.5, "lines"),
    legend.position = "bottom"
  ) +
  guides(color = guide_legend(override.aes = list(linewidth = 2)))  # Hacer leyenda más legible


# 3. Guardar el gráfico (tamaño grande para 98 municipios)
ggsave("outputs/temperaturas_municipios.jpg",
       plot=temperaturas,
       height = 12,
       width = 7,
       dpi=300)



#=================================================
# GRÁFICA TODAS VARIABLES PARA UN SOLO MUNICIPIO
#=================================================

# 1. Filtrar datos para NEIVA y convertir a formato largo
datos_neiva <- var_clima_mun %>%
  filter(mun_name == "NEIVA") %>%  # Asegúrate que el nombre coincida exactamente
  select(anio, 
         precipitation = total_precipitation,  # Ajusta nombres según tus datos
         temp_mean = `2m_temperature`,
         temp_min = `2m_temperature_daymin`,
         temp_max = `2m_temperature_daymax`,
         humidity = relative_humidity,
         spec_humidity = specific_humidity) %>%
  pivot_longer(
    cols = -anio,
    names_to = "variable",
    values_to = "valor"
  ) %>%
  mutate(
    variable = factor(variable,
                      levels = c("precipitation", "temp_mean", "temp_min", "temp_max", "humidity", "spec_humidity"),
                      labels = c("Precipitación (mm)", "Temp. Media (°C)", "Temp. Mínima (°C)", 
                                 "Temp. Máxima (°C)", "Humedad Relativa (%)", "Humedad Específica (kg/kg)"))
  )

# 2. Crear gráfico facetado
ggplot(datos_neiva, aes(x = anio, y = valor)) +
  geom_line(color = "#1f77b4", linewidth = 0.8) +
  geom_point(color = "#ff7f0e", size = 1.5) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2,
             labeller = labeller(variable = label_wrap_gen(width = 20))) +
  labs(
    title = "Annual average of climatic variables - NEIVA",
    subtitle = paste("Periodo", min(datos_neiva$anio), "-", max(datos_neiva$anio)),
    x = "Year",
    y = "Value",
    caption = "Source: ERA5-Land/CHIRPS data"
  ) +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 10, face = "bold"),
    strip.background = element_rect(fill = "#f0f0f0"),
    panel.spacing = unit(1, "lines"),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14)
  ) +
  scale_x_continuous(breaks = seq(floor(min(datos_neiva$anio)), ceiling(max(datos_neiva$anio)), by = 2))

# 3. Guardar gráfico
ggsave("outputs/clima_neiva.png", width = 12, height = 10, dpi = 300)
