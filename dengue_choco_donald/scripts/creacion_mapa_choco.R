
# Cargar el shapefile y corregir los códigos
mapa_colombia_mpio <- st_read("data/data_descriptivo//datos_clima/SHP_MGN2018_INTGRD_MPIO/MGN_ANM_MPIOS.shp") %>% 
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

# Filtrar solo el departamento del Chocó (código 27)
mapa_choco <- mapa_colombia_mpio %>% 
  filter(cod_dep == "27")

# Obtener centroides para etiquetas
centroides_choco <- st_centroid(mapa_choco)

# Graficar
ggplot(data = mapa_choco) +
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


#Guardar shapefile
library(sf)

# Guardar shapefile del Chocó
dir.create("data/shapefiles_choco", recursive = TRUE, showWarnings = FALSE)

# Guardar el shapefile dentro de esa carpeta
st_write(mapa_choco, "data/shapefiles_choco/choco_municipios.shp", delete_layer = TRUE)
