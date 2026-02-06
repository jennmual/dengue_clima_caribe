####################################
#  MAPA CHOCÓ
####################################

#LIBRERÍAS
library(sf)
library(ggplot2)
library(dplyr)

# Cargar el shapefile y corregir los códigos
mapa_choco_mpio <- st_read("data/shapefiles_choco/choco_municipios.shp") %>% 
  select("DPTO_CCDGO",
         "MPIO_CCDGO",
         "MPIO_CNMBR",
         "VERSION",
         "AREA",
         "LATITUD",
         "LONGITUD",
         "Shape_Leng",
         "Shape_Area",
         "geometry") %>% 
  mutate(cod_dep = as.character(DPTO_CCDGO),
         cod_mpio = as.character(MPIO_CCDGO))


# Obtener centroides para etiquetas
centroides_choco <- st_centroid(mapa_choco_mpio)

# Graficar
ggplot(data = mapa_choco_mpio) +
  geom_sf(fill = "lightgreen", color = "black", size = 0.2) +
  geom_text(data = centroides_choco, aes(x = st_coordinates(geometry)[,1],
                                         y = st_coordinates(geometry)[,2],
                                         label = MPIO_CNMBR),
            size = 2.5, color = "darkgreen") +
  labs(title = "Municipios del Departamento del Chocó",
       #caption = "Fuente: DANE - MGN 2018",
       y= "",
       x= "",
  ) +
  theme_minimal()

