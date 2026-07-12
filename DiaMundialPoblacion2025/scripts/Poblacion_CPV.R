
library(tidyverse)
library(openxlsx)
library(viridis)
library(sf)
library(ggspatial)
library(ggrepel)
library(grid, quietly=T) #make grid
library(gridExtra, quietly=T) #make grid
library(giscoR, quietly=T) #shapefile of Europe
library(classInt, quietly=T) #bins
library(sp)
library(extrafont)
library(rcartocolor)
library(scales)  # Asegúrate de cargar la librería scales


# CARGA DE DATOS --------------------------------------------------------
## Archivos shapefile
shp_provincias <- st_read("Data/SHP PERU/PROVINCIAS/PROVINCIAS_inei_geogpsperu_suyopomalia.shp")
poblacion25 <- read.xlsx("data/Población_Proyectada_2025_cpv.xlsx")

# MANIPULACIÓN DE DATOS ---------------------------------------------------

# Unir datos de población proyectada al shapefile de provincia
shp_provincia_pob_cpv <- shp_provincias %>% left_join(poblacion25, by = c("IDPROV"="ID_PROV"))

#centroides
# Inundaciones
cents <- shp_provincias |> filter(NOMBDEP=="AMAZONAS") |> st_centroid() %>%  
  as_Spatial() %>%                  
  as.data.frame()

# Definir una paleta de colores personalizada
colores_personalizados <- c("#ffffcc", "#a1dab4", "#41b6c4", "#9aacb8", "#727794")

# 1. MAPA POBLACIONAL DE AMAZONAS -----------------------------------------

# Crear el gráfico con la paleta de colores personalizada en escala continua
shp_provincia_pob_cpv |>  
  filter(NOMBDEP == "AMAZONAS") |> 
  ggplot() + 
  geom_sf(aes(fill = POBLACION.TOTAL.2025), color = "white", size = 0.3) +
  scale_fill_gradientn(
    name = "Población Total 2025", 
    colors = colores_personalizados, 
    labels = label_comma()  # Formato con separadores de miles en la leyenda
  ) +  # Usar la paleta continua personalizada
  geom_sf_text(aes(label = paste(NOMBPROV, "\n", comma(POBLACION.TOTAL.2025, format = "d"))), 
               size = 4, color = "black", fontface = "bold", 
               check_overlap = FALSE,  # Evitar superposición
               nudge_x = -0.0, nudge_y = -0.00) +  # Ajuste para evitar superposición
  labs(
    title = "PROVINCIAS DE AMAZONAS: POBLACIÓN TOTAL 2025",
    caption = "Fuente:INEI - Censos Nacionales 2025: XIII de Población, VIII de Vivienda y IV de Comunidades Indígenas.\nElaboración. www.denis-rodriguez.com",
    x = ""
  ) +
  theme(
    panel.background = element_blank(), 
    legend.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(size = 12, color = '#05204d', hjust = 0.5, vjust = 0, face = "bold"),
    plot.subtitle = element_text(size = 14, color = '#ac63a0', hjust = 0.0, vjust = 0),
    plot.caption = element_text(size = 10, color = "grey60", hjust = 0.0, vjust = 0),
    axis.title.x = element_text(size = 10, color = "grey20", hjust = 0.5, vjust = -6),
    legend.text = element_text(size = 10, color = "grey20"),
    legend.title = element_text(size = 10, color = "grey20"),
    strip.text = element_text(size = 12),
    plot.margin = unit(c(t = 1, r = -2, b = 1, l = -2), "lines"),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  ggsave("imagenes/CPV_2025/amazonas.png", width = 10, height = 13, dpi = 320, units = "in")




# 2. MAPA POBLACIONAL DE LA ANCASH -----------------------------------------
shp_provincia_pob_cpv |>  
  filter(NOMBDEP == "ANCASH") |> 
  ggplot() + 
  geom_sf(aes(fill = POBLACION.TOTAL.2025), color = "#05204d", size = 0.3) +
  scale_fill_gradientn(
    name = "Población Total 2025", 
    colors = colores_personalizados, 
    labels = label_comma()  # Formato con separadores de miles en la leyenda
  ) +  # Usar la paleta continua personalizada
  geom_sf_text(aes(label = paste(NOMBPROV, "\n", comma(POBLACION.TOTAL.2025, format = "d"))), 
               size = 4, color = "black", fontface = "bold", 
               check_overlap = FALSE,  # Evitar superposición
               nudge_x = -0.0, nudge_y = -0.00) +  # Ajuste para evitar superposición
  labs(
    title = "PROVINCIAS DE ÁNCASH: POBLACIÓN TOTAL 2025",
    caption = "Fuente:INEI - Censos Nacionales 2025: XIII de Población, VIII de Vivienda y IV de Comunidades Indígenas.\nElaboración. www.denis-rodriguez.com",
    x = ""
  ) +
  theme(
    panel.background = element_blank(), 
    legend.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(size = 12, color = '#05204d', hjust = 0.5, vjust = 0, face = "bold"),
    plot.subtitle = element_text(size = 14, color = '#ac63a0', hjust = 0.0, vjust = 0),
    plot.caption = element_text(size = 10, color = "grey60", hjust = 0.0, vjust = 0),
    axis.title.x = element_text(size = 10, color = "grey20", hjust = 0.5, vjust = -6),
    legend.text = element_text(size = 10, color = "grey20"),
    legend.title = element_text(size = 10, color = "grey20"),
    strip.text = element_text(size = 12),
    plot.margin = unit(c(t = 1, r = -2, b = 1, l = -2), "lines"),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  ggsave("imagenes/CPV_2025/ancash.png", width = 10, height = 13, dpi = 320, units = "in")





# 3. MAPA POBLACIONAL DE LA APURIMAC -----------------------------------------
shp_provincia_pob_cpv |>  
  filter(NOMBDEP == "APURIMAC") |> 
  ggplot() + 
  geom_sf(aes(fill = POBLACION.TOTAL.2025), color = "#05204d", size = 0.3) +
  scale_fill_gradientn(
    name = "Población Total 2025", 
    colors = colores_personalizados, 
    labels = label_comma()  # Formato con separadores de miles en la leyenda
  ) +  # Usar la paleta continua personalizada
  geom_sf_text(aes(label = paste(NOMBPROV, "\n", comma(POBLACION.TOTAL.2025, format = "d"))), 
               size = 4, color = "black", fontface = "bold", 
               check_overlap = FALSE,  # Evitar superposición
               nudge_x = -0.0, nudge_y = -0.00) +  # Ajuste para evitar superposición
  labs(
    title = "PROVINCIAS DE APURÍMAC: POBLACIÓN TOTAL 2025",
    caption = "Fuente:INEI - Censos Nacionales 2025: XIII de Población, VIII de Vivienda y IV de Comunidades Indígenas.\nElaboración. www.denis-rodriguez.com",
    x = ""
  ) +
  theme(
    panel.background = element_blank(), 
    legend.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(size = 12, color = '#05204d', hjust = 0.5, vjust = 0, face = "bold"),
    plot.subtitle = element_text(size = 14, color = '#ac63a0', hjust = 0.0, vjust = 0),
    plot.caption = element_text(size = 10, color = "grey60", hjust = 0.0, vjust = 0),
    axis.title.x = element_text(size = 10, color = "grey20", hjust = 0.5, vjust = -6),
    legend.text = element_text(size = 10, color = "grey20"),
    legend.title = element_text(size = 10, color = "grey20"),
    strip.text = element_text(size = 12),
    plot.margin = unit(c(t = 1, r = -2, b = 1, l = -2), "lines"),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  ggsave("imagenes/CPV_2025/apurimac.png", width = 16, height = 13, dpi = 320, units = "in")



# 4. MAPA POBLACIONAL DE LA AREQUIPA -----------------------------------------
shp_provincia_pob_cpv |>  
  filter(NOMBDEP == "AREQUIPA") |> 
  ggplot() + 
  geom_sf(aes(fill = POBLACION.TOTAL.2025), color = "#05204d", size = 0.3) +
  scale_fill_gradientn(
    name = "Población Total 2025", 
    colors = colores_personalizados, 
    labels = label_comma()  # Formato con separadores de miles en la leyenda
  ) +  # Usar la paleta continua personalizada
  geom_sf_text(aes(label = paste(NOMBPROV, "\n", comma(POBLACION.TOTAL.2025, format = "d"))), 
               size = 4, color = "black", fontface = "bold", 
               check_overlap = FALSE,  # Evitar superposición
               nudge_x = -0.0, nudge_y = -0.00) +  # Ajuste para evitar superposición
  labs(
    title = "PROVINCIAS DE AREQUIPA: POBLACIÓN TOTAL 2025",
    caption = "Fuente:INEI - Censos Nacionales 2025: XIII de Población, VIII de Vivienda y IV de Comunidades Indígenas.\nElaboración. www.denis-rodriguez.com",
    x = ""
  ) +
  theme(
    panel.background = element_blank(), 
    legend.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(size = 12, color = '#05204d', hjust = 0.5, vjust = 0, face = "bold"),
    plot.subtitle = element_text(size = 14, color = '#ac63a0', hjust = 0.0, vjust = 0),
    plot.caption = element_text(size = 10, color = "grey60", hjust = 0.0, vjust = 0),
    axis.title.x = element_text(size = 10, color = "grey20", hjust = 0.5, vjust = -6),
    legend.text = element_text(size = 10, color = "grey20"),
    legend.title = element_text(size = 10, color = "grey20"),
    strip.text = element_text(size = 12),
    plot.margin = unit(c(t = 1, r = -2, b = 1, l = -2), "lines"),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  ggsave("imagenes/CPV_2025/arequipa.png", width = 22, height = 14, dpi = 320, units = "in")





# 5. MAPA POBLACIONAL DE LA AYACUCHO--------------------------------------
shp_provincia_pob_cpv |>  
  filter(NOMBDEP == "AYACUCHO") |> 
  ggplot() + 
  geom_sf(aes(fill = POBLACION.TOTAL.2025), color = "#05204d", size = 0.3) +
  scale_fill_gradientn(
    name = "Población Total 2025", 
    colors = colores_personalizados, 
    labels = label_comma()  # Formato con separadores de miles en la leyenda
  ) +  # Usar la paleta continua personalizada
  geom_sf_text(aes(label = paste(NOMBPROV, "\n", comma(POBLACION.TOTAL.2025, format = "d"))), 
               size = 4, color = "black", fontface = "bold", 
               check_overlap = FALSE,  # Evitar superposición
               nudge_x = -0.0, nudge_y = -0.00) +  # Ajuste para evitar superposición
  labs(
    title = "PROVINCIAS DE AYACUCHO: POBLACIÓN TOTAL 2025",
    caption = "Fuente:INEI - Censos Nacionales 2025: XIII de Población, VIII de Vivienda y IV de Comunidades Indígenas.\nElaboración. www.denis-rodriguez.com",
    x = ""
  ) +
  theme(
    panel.background = element_blank(), 
    legend.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(size = 12, color = '#05204d', hjust = 0.5, vjust = 0, face = "bold"),
    plot.subtitle = element_text(size = 14, color = '#ac63a0', hjust = 0.0, vjust = 0),
    plot.caption = element_text(size = 10, color = "grey60", hjust = 0.0, vjust = 0),
    axis.title.x = element_text(size = 10, color = "grey20", hjust = 0.5, vjust = -6),
    legend.text = element_text(size = 10, color = "grey20"),
    legend.title = element_text(size = 10, color = "grey20"),
    strip.text = element_text(size = 12),
    plot.margin = unit(c(t = 1, r = -2, b = 1, l = -2), "lines"),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  ggsave("imagenes/CPV_2025/ayacucho.png", width = 11, height = 12, dpi = 320, units = "in")





# 6. MAPA POBLACIONAL DE CAJAMARCA -----------------------------------------
shp_provincia_pob_cpv |>  
  filter(NOMBDEP == "CAJAMARCA") |> 
  ggplot() + 
  geom_sf(aes(fill = POBLACION.TOTAL.2025), color = "white", size = 0.3) +
  scale_fill_gradientn(
    name = "Población Total 2025", 
    colors = colores_personalizados, 
    labels = label_comma()  # Formato con separadores de miles en la leyenda
  ) +  # Usar la paleta continua personalizada
  geom_sf_text(aes(label = paste(NOMBPROV, "\n", comma(POBLACION.TOTAL.2025, format = "d"))), 
               size = 4, color = "black", fontface = "bold", 
               check_overlap = FALSE,  # Evitar superposición
               nudge_x = -0.0, nudge_y = -0.00) +  # Ajuste para evitar superposición
  labs(
    title = "PROVINCIAS DE CAJAMARCA: POBLACIÓN TOTAL 2025",
    caption = "Fuente:INEI - Censos Nacionales 2025: XIII de Población, VIII de Vivienda y IV de Comunidades Indígenas.\nElaboración. www.denis-rodriguez.com",
    x = ""
  ) +
  theme(
    panel.background = element_blank(), 
    legend.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(size = 12, color = '#05204d', hjust = 0.5, vjust = 0, face = "bold"),
    plot.subtitle = element_text(size = 14, color = '#ac63a0', hjust = 0.0, vjust = 0),
    plot.caption = element_text(size = 10, color = "grey60", hjust = 0.0, vjust = 0),
    axis.title.x = element_text(size = 10, color = "grey20", hjust = 0.5, vjust = -6),
    legend.text = element_text(size = 10, color = "grey20"),
    legend.title = element_text(size = 10, color = "grey20"),
    strip.text = element_text(size = 12),
    plot.margin = unit(c(t = 1, r = -2, b = 1, l = -2), "lines"),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  ggsave("imagenes/CPV_2025/cajamarca.png", width = 13, height = 13, dpi = 320, units = "in")







# 7. MAPA POBLACIONAL DE CUSCO -----------------------------------------
shp_provincia_pob_cpv |>  
  filter(NOMBDEP == "CUSCO") |> 
  ggplot() + 
  geom_sf(aes(fill = POBLACION.TOTAL.2025), color = "#05204d", size = 0.3) +
  scale_fill_gradientn(
    name = "Población Total 2025", 
    colors = colores_personalizados, 
    labels = label_comma()  # Formato con separadores de miles en la leyenda
  ) +  # Usar la paleta continua personalizada
  geom_sf_text(aes(label = paste(NOMBPROV, "\n", comma(POBLACION.TOTAL.2025, format = "d"))), 
               size = 4, color = "black", fontface = "bold", 
               check_overlap = FALSE,  # Evitar superposición
               nudge_x = -0.0, nudge_y = -0.00) +  # Ajuste para evitar superposición
  labs(
    title = "PROVINCIAS DE CUSCO: POBLACIÓN TOTAL 2025",
    caption = "Fuente:INEI - Censos Nacionales 2025: XIII de Población, VIII de Vivienda y IV de Comunidades Indígenas.\nElaboración. www.denis-rodriguez.com",
    x = ""
  ) +
  theme(
    panel.background = element_blank(), 
    legend.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(size = 12, color = '#05204d', hjust = 0.5, vjust = 0, face = "bold"),
    plot.subtitle = element_text(size = 14, color = '#ac63a0', hjust = 0.0, vjust = 0),
    plot.caption = element_text(size = 10, color = "grey60", hjust = 0.0, vjust = 0),
    axis.title.x = element_text(size = 10, color = "grey20", hjust = 0.5, vjust = -6),
    legend.text = element_text(size = 10, color = "grey20"),
    legend.title = element_text(size = 10, color = "grey20"),
    strip.text = element_text(size = 12),
    plot.margin = unit(c(t = 1, r = -2, b = 1, l = -2), "lines"),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  ggsave("imagenes/CPV_2025/cusco.png", width = 11, height = 11, dpi = 320, units = "in")







# 8. MAPA POBLACIONAL DE HUANCAVELICA -----------------------------------------
shp_provincia_pob_cpv |>  
  filter(NOMBDEP == "HUANCAVELICA") |> 
  ggplot() + 
  geom_sf(aes(fill = POBLACION.TOTAL.2025), color = "#05204d", size = 0.3) +
  scale_fill_gradientn(
    name = "Población Total 2025", 
    colors = colores_personalizados, 
    labels = label_comma()  # Formato con separadores de miles en la leyenda
  ) +  # Usar la paleta continua personalizada
  geom_sf_text(aes(label = paste(NOMBPROV, "\n", comma(POBLACION.TOTAL.2025, format = "d"))), 
               size = 4, color = "black", fontface = "bold", 
               check_overlap = FALSE,  # Evitar superposición
               nudge_x = -0.0, nudge_y = -0.00) +  # Ajuste para evitar superposición
  labs(
    caption = "Fuente:INEI - Censos Nacionales 2025: XIII de Población, VIII de Vivienda y IV de Comunidades Indígenas.\nElaboración. www.denis-rodriguez.com",
    x = ""
  ) +
  theme(
    panel.background = element_blank(), 
    legend.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(size = 12, color = '#05204d', hjust = 0.5, vjust = 0, face = "bold"),
    plot.subtitle = element_text(size = 14, color = '#ac63a0', hjust = 0.0, vjust = 0),
    plot.caption = element_text(size = 10, color = "grey60", hjust = 0.0, vjust = 0),
    axis.title.x = element_text(size = 10, color = "grey20", hjust = 0.5, vjust = -6),
    legend.text = element_text(size = 10, color = "grey20"),
    legend.title = element_text(size = 10, color = "grey20"),
    strip.text = element_text(size = 12),
    plot.margin = unit(c(t = 1, r = -2, b = 1, l = -2), "lines"),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  ggsave("imagenes/CPV_2025/huancavelica.png", width = 10, height = 10, dpi = 320, units = "in")







# 9. MAPA POBLACIONAL DE HUANUCO -----------------------------------------
shp_provincia_pob_cpv |>  
  filter(NOMBDEP == "HUANUCO") |> 
  ggplot() + 
  geom_sf(aes(fill = POBLACION.TOTAL.2025), color = "#05204d", size = 0.3) +
  scale_fill_gradientn(
    name = "Población Total 2025", 
    colors = colores_personalizados, 
    labels = label_comma()  # Formato con separadores de miles en la leyenda
  ) +  # Usar la paleta continua personalizada
  geom_sf_text(aes(label = paste(NOMBPROV, "\n", comma(POBLACION.TOTAL.2025, format = "d"))), 
               size = 4, color = "black", fontface = "bold", 
               check_overlap = FALSE,  # Evitar superposición
               nudge_x = -0.0, nudge_y = -0.00) +  # Ajuste para evitar superposición
  labs(
    title = "PROVINCIAS DE HUÁNUCO: POBLACIÓN TOTAL 2025",
    caption = "Fuente:INEI - Censos Nacionales 2025: XIII de Población, VIII de Vivienda y IV de Comunidades Indígenas.\nElaboración. www.denis-rodriguez.com",
    x = ""
  ) +
  theme(
    panel.background = element_blank(), 
    legend.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(size = 12, color = '#05204d', hjust = 0.5, vjust = 0, face = "bold"),
    plot.subtitle = element_text(size = 14, color = '#ac63a0', hjust = 0.0, vjust = 0),
    plot.caption = element_text(size = 10, color = "grey60", hjust = 0.0, vjust = 0),
    axis.title.x = element_text(size = 10, color = "grey20", hjust = 0.5, vjust = -6),
    legend.text = element_text(size = 10, color = "grey20"),
    legend.title = element_text(size = 10, color = "grey20"),
    strip.text = element_text(size = 12),
    plot.margin = unit(c(t = 1, r = -2, b = 1, l = -2), "lines"),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  ggsave("imagenes/CPV_2025/huanuco.png", width = 15, height = 11, dpi = 320, units = "in")







# 10. MAPA POBLACIONAL DE ICA -----------------------------------------
shp_provincia_pob_cpv |>  
  filter(NOMBDEP == "ICA") |> 
  ggplot() + 
  geom_sf(aes(fill = POBLACION.TOTAL.2025), color = "#05204d", size = 0.3) +
  scale_fill_gradientn(
    name = "Población Total 2025", 
    colors = colores_personalizados, 
    labels = label_comma()  # Formato con separadores de miles en la leyenda
  ) +  # Usar la paleta continua personalizada
  geom_sf_text(aes(label = paste(NOMBPROV, "\n", comma(POBLACION.TOTAL.2025, format = "d"))), 
               size = 4, color = "black", fontface = "bold", 
               check_overlap = FALSE,  # Evitar superposición
               nudge_x = -0.0, nudge_y = -0.00) +  # Ajuste para evitar superposición
  labs(
    title = "PROVINCIAS DE ICA: POBLACIÓN TOTAL 2025",
    caption = "Fuente:INEI - Censos Nacionales 2025: XIII de Población, VIII de Vivienda y IV de Comunidades Indígenas.\nElaboración. www.denis-rodriguez.com",
    x = ""
  ) +
  theme(
    panel.background = element_blank(), 
    legend.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(size = 12, color = '#05204d', hjust = 0.5, vjust = 0, face = "bold"),
    plot.subtitle = element_text(size = 14, color = '#ac63a0', hjust = 0.0, vjust = 0),
    plot.caption = element_text(size = 10, color = "grey60", hjust = 0.0, vjust = 0),
    axis.title.x = element_text(size = 10, color = "grey20", hjust = 0.5, vjust = -6),
    legend.text = element_text(size = 10, color = "grey20"),
    legend.title = element_text(size = 10, color = "grey20"),
    strip.text = element_text(size = 12),
    plot.margin = unit(c(t = 1, r = -2, b = 1, l = -2), "lines"),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  ggsave("imagenes/CPV_2025/ica.png", width = 11, height = 11, dpi = 320, units = "in")





# 11. MAPA POBLACIONAL DE JUNIN -----------------------------------------
shp_provincia_pob_cpv |>  
  filter(NOMBDEP == "JUNIN") |> 
  ggplot() + 
  geom_sf(aes(fill = POBLACION.TOTAL.2025), color = "#05204d", size = 0.3) +
  scale_fill_gradientn(
    name = "Población Total 2025", 
    colors = colores_personalizados, 
    labels = label_comma()  # Formato con separadores de miles en la leyenda
  ) +  # Usar la paleta continua personalizada
  geom_sf_text(aes(label = paste(NOMBPROV, "\n", comma(POBLACION.TOTAL.2025, format = "d"))), 
               size = 4, color = "black", fontface = "bold", 
               check_overlap = FALSE,  # Evitar superposición
               nudge_x = -0.0, nudge_y = -0.00) +  # Ajuste para evitar superposición
  labs(
    title = "PROVINCIAS DE JUNÍN: POBLACIÓN TOTAL 2025",
    caption = "Fuente:INEI - Censos Nacionales 2025: XIII de Población, VIII de Vivienda y IV de Comunidades Indígenas.\nElaboración. www.denis-rodriguez.com",
    x = ""
  ) +
  theme(
    panel.background = element_blank(), 
    legend.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(size = 12, color = '#05204d', hjust = 0.5, vjust = 0, face = "bold"),
    plot.subtitle = element_text(size = 14, color = '#ac63a0', hjust = 0.0, vjust = 0),
    plot.caption = element_text(size = 10, color = "grey60", hjust = 0.0, vjust = 0),
    axis.title.x = element_text(size = 10, color = "grey20", hjust = 0.5, vjust = -6),
    legend.text = element_text(size = 10, color = "grey20"),
    legend.title = element_text(size = 10, color = "grey20"),
    strip.text = element_text(size = 12),
    plot.margin = unit(c(t = 1, r = -2, b = 1, l = -2), "lines"),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  ggsave("imagenes/CPV_2025/junin.png", width = 14, height = 9, dpi = 320, units = "in")







# 12. MAPA POBLACIONAL DE LA LIBERTAD -----------------------------------------
shp_provincia_pob_cpv |>  
  filter(NOMBDEP == "LA LIBERTAD") |> 
  ggplot() + 
  geom_sf(aes(fill = POBLACION.TOTAL.2025), color = "white", size = 0.3) +
  scale_fill_gradientn(
    name = "Población Total 2025", 
    colors = colores_personalizados, 
    labels = label_comma()  # Formato con separadores de miles en la leyenda
  ) +  # Usar la paleta continua personalizada
  geom_sf_text(aes(label = paste(NOMBPROV, "\n", comma(POBLACION.TOTAL.2025, format = "d"))), 
               size = 4, color = "black", fontface = "bold", 
               check_overlap = FALSE,  # Evitar superposición
               nudge_x = -0.0, nudge_y = -0.00) +  # Ajuste para evitar superposición
  labs(
    title = "PROVINCIAS DE LA LIBERTAD: POBLACIÓN TOTAL 2025",
    caption = "Fuente:INEI - Censos Nacionales 2025: XIII de Población, VIII de Vivienda y IV de Comunidades Indígenas.\nElaboración. www.denis-rodriguez.com",
    x = ""
  ) +
  theme(
    panel.background = element_blank(), 
    legend.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(size = 12, color = '#05204d', hjust = 0.5, vjust = 0, face = "bold"),
    plot.subtitle = element_text(size = 14, color = '#ac63a0', hjust = 0.0, vjust = 0),
    plot.caption = element_text(size = 10, color = "grey60", hjust = 0.0, vjust = 0),
    axis.title.x = element_text(size = 10, color = "grey20", hjust = 0.5, vjust = -6),
    legend.text = element_text(size = 10, color = "grey20"),
    legend.title = element_text(size = 10, color = "grey20"),
    strip.text = element_text(size = 12),
    plot.margin = unit(c(t = 1, r = -2, b = 1, l = -2), "lines"),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  ggsave("imagenes/CPV_2025/la_libertad.png", width = 20, height = 14, dpi = 320, units = "in")

# 13. MAPA POBLACIONAL DE LAMBAYEQUE -----------------------------------------
shp_provincia_pob_cpv |>  
  filter(NOMBDEP == "LAMBAYEQUE") |> 
  ggplot() + 
  geom_sf(aes(fill = POBLACION.TOTAL.2025), color = "white", size = 0.3) +
  scale_fill_gradientn(
    name = "Población Total 2025", 
    colors = colores_personalizados, 
    labels = label_comma()  # Formato con separadores de miles en la leyenda
  ) +  # Usar la paleta continua personalizada
  geom_sf_text(aes(label = paste(NOMBPROV, "\n", comma(POBLACION.TOTAL.2025, format = "d"))), 
               size = 4, color = "black", fontface = "bold", 
               check_overlap = FALSE,  # Evitar superposición
               nudge_x = -0.0, nudge_y = -0.00) +  # Ajuste para evitar superposición
  labs(
    title = "PROVINCIAS DE LAMBAYEQUE: POBLACIÓN TOTAL 2025",
    caption = "Fuente:INEI - Censos Nacionales 2025: XIII de Población, VIII de Vivienda y IV de Comunidades Indígenas.\nElaboración. www.denis-rodriguez.com",
    x = ""
  ) +
  theme(
    panel.background = element_blank(), 
    legend.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(size = 12, color = '#05204d', hjust = 0.5, vjust = 0, face = "bold"),
    plot.subtitle = element_text(size = 14, color = '#ac63a0', hjust = 0.0, vjust = 0),
    plot.caption = element_text(size = 10, color = "grey60", hjust = 0.0, vjust = 0),
    axis.title.x = element_text(size = 10, color = "grey20", hjust = 0.5, vjust = -6),
    legend.text = element_text(size = 10, color = "grey20"),
    legend.title = element_text(size = 10, color = "grey20"),
    strip.text = element_text(size = 12),
    plot.margin = unit(c(t = 1, r = -2, b = 1, l = -2), "lines"),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  ggsave("imagenes/CPV_2025/lambayeque.png", width = 10, height = 10, dpi = 320, units = "in")

# 14. MAPA POBLACIONAL DE LIMA -------------------------------
shp_provincia_pob_cpv |>  
  filter(NOMBDEP == "LIMA") |> 
  ggplot() + 
  geom_sf(aes(fill = POBLACION.TOTAL.2025), color = "white", size = 0.3) +
  scale_fill_gradientn(
    name = "Población Total 2025", 
    colors = colores_personalizados, 
    labels = label_comma()  # Formato con separadores de miles en la leyenda
  ) +  # Usar la paleta continua personalizada
  geom_sf_text(aes(label = paste(NOMBPROV, "\n", comma(POBLACION.TOTAL.2025, format = "d"))), 
               size = 4, color = "black", fontface = "bold", 
               check_overlap = FALSE,  # Evitar superposición
               nudge_x = -0.0, nudge_y = -0.00) +  # Ajuste para evitar superposición
  labs(
    title = "LIMA METROPOLITANA: POBLACIÓN TOTAL 2025",
    caption = "Fuente:INEI - Censos Nacionales 2025: XIII de Población, VIII de Vivienda y IV de Comunidades Indígenas.\nElaboración. www.denis-rodriguez.com",
    x = ""
  ) +
  theme(
    panel.background = element_blank(), 
    legend.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(size = 12, color = '#05204d', hjust = 0.5, vjust = 0, face = "bold"),
    plot.subtitle = element_text(size = 14, color = '#ac63a0', hjust = 0.0, vjust = 0),
    plot.caption = element_text(size = 10, color = "grey60", hjust = 0.0, vjust = 0),
    axis.title.x = element_text(size = 10, color = "grey20", hjust = 0.5, vjust = -6),
    legend.text = element_text(size = 10, color = "grey20"),
    legend.title = element_text(size = 10, color = "grey20"),
    strip.text = element_text(size = 12),
    plot.margin = unit(c(t = 1, r = -2, b = 1, l = -2), "lines"),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  ggsave("imagenes/CPV_2025/lima.png", width = 10, height = 10, dpi = 320, units = "in")



# 15. MAPA POBLACIONAL DE REGIÓN LIMA -------------------------------
shp_provincia_pob_cpv |>  
  filter(NOMBDEP == "LIMA" & NOMBPROV != "LIMA") |> 
  ggplot() + 
  geom_sf(aes(fill = POBLACION.TOTAL.2025), color = "white", size = 0.3) +
  scale_fill_gradientn(
    name = "Población Total 2025", 
    colors = colores_personalizados, 
    labels = label_comma()  # Formato con separadores de miles en la leyenda
  ) +  # Usar la paleta continua personalizada
  geom_sf_text(aes(label = paste(NOMBPROV, "\n", comma(POBLACION.TOTAL.2025, format = "d"))), 
               size = 4, color = "black", fontface = "bold", 
               check_overlap = FALSE,  # Evitar superposición
               nudge_x = -0.0, nudge_y = -0.00) +  # Ajuste para evitar superposición
  labs(
    title = "PROVINCIAS DE REGIÓN LIMA: POBLACIÓN TOTAL 2025",
    caption = "Fuente:INEI - Censos Nacionales 2025: XIII de Población, VIII de Vivienda y IV de Comunidades Indígenas.\nElaboración. www.denis-rodriguez.com",
    x = ""
  ) +
  theme(
    panel.background = element_blank(), 
    legend.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(size = 12, color = '#05204d', hjust = 0.5, vjust = 0, face = "bold"),
    plot.subtitle = element_text(size = 14, color = '#ac63a0', hjust = 0.0, vjust = 0),
    plot.caption = element_text(size = 10, color = "grey60", hjust = 0.0, vjust = 0),
    axis.title.x = element_text(size = 10, color = "grey20", hjust = 0.5, vjust = -6),
    legend.text = element_text(size = 10, color = "grey20"),
    legend.title = element_text(size = 10, color = "grey20"),
    strip.text = element_text(size = 12),
    plot.margin = unit(c(t = 1, r = -2, b = 1, l = -2), "lines"),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  ggsave("imagenes/CPV_2025/region_lima.png", width = 10, height = 10, dpi = 320, units = "in")

# 16. MAPA POBLACIONAL DE LORETO -------------------------------
shp_provincia_pob_cpv |>  
  filter(NOMBDEP == "LORETO") |> 
  ggplot() + 
  geom_sf(aes(fill = POBLACION.TOTAL.2025), color = "white", size = 0.3) +
  scale_fill_gradientn(
    name = "Población Total 2025", 
    colors = colores_personalizados, 
    labels = label_comma()  # Formato con separadores de miles en la leyenda
  ) +  # Usar la paleta continua personalizada
  geom_sf_text(aes(label = paste(NOMBPROV, "\n", comma(POBLACION.TOTAL.2025, format = "d"))), 
               size = 4, color = "black", fontface = "bold", 
               check_overlap = FALSE,  # Evitar superposición
               nudge_x = -0.0, nudge_y = -0.00) +  # Ajuste para evitar superposición
  labs(
    title = "PROVINCIAS DE LORETO: POBLACIÓN TOTAL 2025",
    caption = "Fuente:INEI - Censos Nacionales 2025: XIII de Población, VIII de Vivienda y IV de Comunidades Indígenas.\nElaboración. www.denis-rodriguez.com",
    x = ""
  ) +
  theme(
    panel.background = element_blank(), 
    legend.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(size = 12, color = '#05204d', hjust = 0.5, vjust = 0, face = "bold"),
    plot.subtitle = element_text(size = 14, color = '#ac63a0', hjust = 0.0, vjust = 0),
    plot.caption = element_text(size = 10, color = "grey60", hjust = 0.0, vjust = 0),
    axis.title.x = element_text(size = 10, color = "grey20", hjust = 0.5, vjust = -6),
    legend.text = element_text(size = 10, color = "grey20"),
    legend.title = element_text(size = 10, color = "grey20"),
    strip.text = element_text(size = 12),
    plot.margin = unit(c(t = 1, r = -2, b = 1, l = -2), "lines"),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  ggsave("imagenes/CPV_2025/loreto.png", width = 10, height = 10, dpi = 320, units = "in")

# 17. MAPA POBLACIONAL DE MADRE DE DIOS -------------------------------
shp_provincia_pob_cpv |>  
  filter(NOMBDEP == "MADRE DE DIOS") |> 
  ggplot() + 
  geom_sf(aes(fill = POBLACION.TOTAL.2025), color = "white", size = 0.3) +
  scale_fill_gradientn(
    name = "Población Total 2025", 
    colors = colores_personalizados, 
    labels = label_comma()  # Formato con separadores de miles en la leyenda
  ) +  # Usar la paleta continua personalizada
  geom_sf_text(aes(label = paste(NOMBPROV, "\n", comma(POBLACION.TOTAL.2025, format = "d"))), 
               size = 4, color = "black", fontface = "bold", 
               check_overlap = FALSE,  # Evitar superposición
               nudge_x = -0.0, nudge_y = -0.00) +  # Ajuste para evitar superposición
  labs(
    title = "MADRE DE DIOS: POBLACIÓN TOTAL 2025",
    caption = "Fuente:INEI - Censos Nacionales 2025: XIII de Población, VIII de Vivienda y IV de Comunidades Indígenas.\nElaboración. www.denis-rodriguez.com",
    x = ""
  ) +
  theme(
    panel.background = element_blank(), 
    legend.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(size = 12, color = '#05204d', hjust = 0.5, vjust = 0, face = "bold"),
    plot.subtitle = element_text(size = 14, color = '#ac63a0', hjust = 0.0, vjust = 0),
    plot.caption = element_text(size = 10, color = "grey60", hjust = 0.0, vjust = 0),
    axis.title.x = element_text(size = 10, color = "grey20", hjust = 0.5, vjust = -6),
    legend.text = element_text(size = 10, color = "grey20"),
    legend.title = element_text(size = 10, color = "grey20"),
    strip.text = element_text(size = 12),
    plot.margin = unit(c(t = 1, r = -2, b = 1, l = -2), "lines"),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  ggsave("imagenes/CPV_2025/madre_de_dios.png", width = 12, height = 10, dpi = 320, units = "in")

# 18. MAPA POBLACIONAL DE MOQUEGUA -------------------------------
shp_provincia_pob_cpv |>  
  filter(NOMBDEP == "MOQUEGUA") |> 
  ggplot() + 
  geom_sf(aes(fill = POBLACION.TOTAL.2025), color = "white", size = 0.3) +
  scale_fill_gradientn(
    name = "Población Total 2025", 
    colors = colores_personalizados, 
    labels = label_comma()  # Formato con separadores de miles en la leyenda
  ) +  # Usar la paleta continua personalizada
  geom_sf_text(aes(label = paste(NOMBPROV, "\n", comma(POBLACION.TOTAL.2025, format = "d"))), 
               size = 4, color = "black", fontface = "bold", 
               check_overlap = FALSE,  # Evitar superposición
               nudge_x = -0.0, nudge_y = -0.00) +  # Ajuste para evitar superposición
  labs(
    title = "PROVINCIAS DE MOQUEGUA: POBLACIÓN TOTAL 2025",
    caption = "Fuente:INEI - Censos Nacionales 2025: XIII de Población, VIII de Vivienda y IV de Comunidades Indígenas.\nElaboración. www.denis-rodriguez.com",
    x = ""
  ) +
  theme(
    panel.background = element_blank(), 
    legend.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(size = 12, color = '#05204d', hjust = 0.5, vjust = 0, face = "bold"),
    plot.subtitle = element_text(size = 14, color = '#ac63a0', hjust = 0.0, vjust = 0),
    plot.caption = element_text(size = 10, color = "grey60", hjust = 0.0, vjust = 0),
    axis.title.x = element_text(size = 10, color = "grey20", hjust = 0.5, vjust = -6),
    legend.text = element_text(size = 10, color = "grey20"),
    legend.title = element_text(size = 10, color = "grey20"),
    strip.text = element_text(size = 12),
    plot.margin = unit(c(t = 1, r = -2, b = 1, l = -2), "lines"),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  ggsave("imagenes/CPV_2025/moquegua.png", width = 10, height = 10, dpi = 320, units = "in")

# 19. MAPA POBLACIONAL DE PASCO -------------------------------
shp_provincia_pob_cpv |>  
  filter(NOMBDEP == "PASCO") |> 
  ggplot() + 
  geom_sf(aes(fill = POBLACION.TOTAL.2025), color = "white", size = 0.3) +
  scale_fill_gradientn(
    name = "Población Total 2025", 
    colors = colores_personalizados, 
    labels = label_comma()  # Formato con separadores de miles en la leyenda
  ) +  # Usar la paleta continua personalizada
  geom_sf_text(aes(label = paste(NOMBPROV, "\n", comma(POBLACION.TOTAL.2025, format = "d"))), 
               size = 4, color = "black", fontface = "bold", 
               check_overlap = FALSE,  # Evitar superposición
               nudge_x = -0.0, nudge_y = -0.00) +  # Ajuste para evitar superposición
  labs(
    title = "PROVINCIAS DE PASCO: POBLACIÓN TOTAL 2025",
    caption = "Fuente:INEI - Censos Nacionales 2025: XIII de Población, VIII de Vivienda y IV de Comunidades Indígenas.\nElaboración. www.denis-rodriguez.com",
    x = ""
  ) +
  theme(
    panel.background = element_blank(), 
    legend.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(size = 12, color = '#05204d', hjust = 0.5, vjust = 0, face = "bold"),
    plot.subtitle = element_text(size = 14, color = '#ac63a0', hjust = 0.0, vjust = 0),
    plot.caption = element_text(size = 10, color = "grey60", hjust = 0.0, vjust = 0),
    axis.title.x = element_text(size = 10, color = "grey20", hjust = 0.5, vjust = -6),
    legend.text = element_text(size = 10, color = "grey20"),
    legend.title = element_text(size = 10, color = "grey20"),
    strip.text = element_text(size = 12),
    plot.margin = unit(c(t = 1, r = -2, b = 1, l = -2), "lines"),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  ggsave("imagenes/CPV_2025/pasco.png", width = 16, height = 10, dpi = 320, units = "in")

# 20. MAPA POBLACIONAL DE PIURA -------------------------------
shp_provincia_pob_cpv |>  
  filter(NOMBDEP == "PIURA") |> 
  ggplot() + 
  geom_sf(aes(fill = POBLACION.TOTAL.2025), color = "white", size = 0.3) +
  scale_fill_gradientn(
    name = "Población Total 2025", 
    colors = colores_personalizados, 
    labels = label_comma()  # Formato con separadores de miles en la leyenda
  ) +  # Usar la paleta continua personalizada
  geom_sf_text(aes(label = paste(NOMBPROV, "\n", comma(POBLACION.TOTAL.2025, format = "d"))), 
               size = 4, color = "black", fontface = "bold", 
               check_overlap = FALSE,  # Evitar superposición
               nudge_x = -0.0, nudge_y = -0.00) +  # Ajuste para evitar superposición
  labs(
    title = "PROVINCIAS DE PIURA: POBLACIÓN TOTAL 2025",
    caption = "Fuente:INEI - Censos Nacionales 2025: XIII de Población, VIII de Vivienda y IV de Comunidades Indígenas.\nElaboración. www.denis-rodriguez.com",
    x = ""
  ) +
  theme(
    panel.background = element_blank(), 
    legend.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(size = 12, color = '#05204d', hjust = 0.5, vjust = 0, face = "bold"),
    plot.subtitle = element_text(size = 14, color = '#ac63a0', hjust = 0.0, vjust = 0),
    plot.caption = element_text(size = 10, color = "grey60", hjust = 0.0, vjust = 0),
    axis.title.x = element_text(size = 10, color = "grey20", hjust = 0.5, vjust = -6),
    legend.text = element_text(size = 10, color = "grey20"),
    legend.title = element_text(size = 10, color = "grey20"),
    strip.text = element_text(size = 12),
    plot.margin = unit(c(t = 1, r = -2, b = 1, l = -2), "lines"),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  ggsave("imagenes/CPV_2025/piura.png", width = 11, height = 10, dpi = 320, units = "in")

# 21. MAPA POBLACIONAL DE PUNO -------------------------------
shp_provincia_pob_cpv |>  
  filter(NOMBDEP == "PUNO") |> 
  ggplot() + 
  geom_sf(aes(fill = POBLACION.TOTAL.2025), color = "white", size = 0.3) +
  scale_fill_gradientn(
    name = "Población Total 2025", 
    colors = colores_personalizados, 
    labels = label_comma()  # Formato con separadores de miles en la leyenda
  ) +  # Usar la paleta continua personalizada
  geom_sf_text(aes(label = paste(NOMBPROV, "\n", comma(POBLACION.TOTAL.2025, format = "d"))), 
               size = 4, color = "black", fontface = "bold", 
               check_overlap = FALSE,  # Evitar superposición
               nudge_x = -0.0, nudge_y = -0.00) +  # Ajuste para evitar superposición
  labs(
    title = "PROVINCIAS DE PUNO: POBLACIÓN TOTAL 2025",
    caption = "Fuente:INEI - Censos Nacionales 2025: XIII de Población, VIII de Vivienda y IV de Comunidades Indígenas.\nElaboración. www.denis-rodriguez.com",
    x = ""
  ) +
  theme(
    panel.background = element_blank(), 
    legend.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(size = 12, color = '#05204d', hjust = 0.5, vjust = 0, face = "bold"),
    plot.subtitle = element_text(size = 14, color = '#ac63a0', hjust = 0.0, vjust = 0),
    plot.caption = element_text(size = 10, color = "grey60", hjust = 0.0, vjust = 0),
    axis.title.x = element_text(size = 10, color = "grey20", hjust = 0.5, vjust = -6),
    legend.text = element_text(size = 10, color = "grey20"),
    legend.title = element_text(size = 10, color = "grey20"),
    strip.text = element_text(size = 12),
    plot.margin = unit(c(t = 1, r = -2, b = 1, l = -2), "lines"),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  ggsave("imagenes/CPV_2025/puno.png", width = 10, height = 10, dpi = 320, units = "in")

# 22. MAPA POBLACIONAL DE SAN MARTIN -------------------------------
shp_provincia_pob_cpv |>  
  filter(NOMBDEP == "SAN MARTIN") |> 
  ggplot() + 
  geom_sf(aes(fill = POBLACION.TOTAL.2025), color = "white", size = 0.3) +
  scale_fill_gradientn(
    name = "Población Total 2025", 
    colors = colores_personalizados, 
    labels = label_comma()  # Formato con separadores de miles en la leyenda
  ) +  # Usar la paleta continua personalizada
  geom_sf_text(aes(label = paste(NOMBPROV, "\n", comma(POBLACION.TOTAL.2025, format = "d"))), 
               size = 4, color = "black", fontface = "bold", 
               check_overlap = FALSE,  # Evitar superposición
               nudge_x = -0.0, nudge_y = -0.00) +  # Ajuste para evitar superposición
  labs(
    title = "PROVINCIAS DE SAN MARTÍN: POBLACIÓN TOTAL 2025",
    caption = "Fuente:INEI - Censos Nacionales 2025: XIII de Población, VIII de Vivienda y IV de Comunidades Indígenas.\nElaboración. www.denis-rodriguez.com",
    x = ""
  ) +
  theme(
    panel.background = element_blank(), 
    legend.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(size = 12, color = '#05204d', hjust = 0.5, vjust = 0, face = "bold"),
    plot.subtitle = element_text(size = 14, color = '#ac63a0', hjust = 0.0, vjust = 0),
    plot.caption = element_text(size = 10, color = "grey60", hjust = 0.0, vjust = 0),
    axis.title.x = element_text(size = 10, color = "grey20", hjust = 0.5, vjust = -6),
    legend.text = element_text(size = 10, color = "grey20"),
    legend.title = element_text(size = 10, color = "grey20"),
    strip.text = element_text(size = 12),
    plot.margin = unit(c(t = 1, r = -2, b = 1, l = -2), "lines"),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  ggsave("imagenes/CPV_2025/san_martin.png", width = 10, height = 10, dpi = 320, units = "in")

# 23. MAPA POBLACIONAL DE TACNA -------------------------------
shp_provincia_pob_cpv |>  
  filter(NOMBDEP == "TACNA") |> 
  ggplot() + 
  geom_sf(aes(fill = POBLACION.TOTAL.2025), color = "white", size = 0.3) +
  scale_fill_gradientn(
    name = "Población Total 2025", 
    colors = colores_personalizados, 
    labels = label_comma()  # Formato con separadores de miles en la leyenda
  ) +  # Usar la paleta continua personalizada
  geom_sf_text(aes(label = paste(NOMBPROV, "\n", comma(POBLACION.TOTAL.2025, format = "d"))), 
               size = 4, color = "black", fontface = "bold", 
               check_overlap = FALSE,  # Evitar superposición
               nudge_x = -0.0, nudge_y = -0.00) +  # Ajuste para evitar superposición
  labs(
    title = "PROVINCIAS DE TACNA: POBLACIÓN TOTAL 2025",
    caption = "Fuente:INEI - Censos Nacionales 2025: XIII de Población, VIII de Vivienda y IV de Comunidades Indígenas.\nElaboración. www.denis-rodriguez.com",
    x = ""
  ) +
  theme(
    panel.background = element_blank(), 
    legend.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(size = 12, color = '#05204d', hjust = 0.5, vjust = 0, face = "bold"),
    plot.subtitle = element_text(size = 14, color = '#ac63a0', hjust = 0.0, vjust = 0),
    plot.caption = element_text(size = 10, color = "grey60", hjust = 0.0, vjust = 0),
    axis.title.x = element_text(size = 10, color = "grey20", hjust = 0.5, vjust = -6),
    legend.text = element_text(size = 10, color = "grey20"),
    legend.title = element_text(size = 10, color = "grey20"),
    strip.text = element_text(size = 12),
    plot.margin = unit(c(t = 1, r = -2, b = 1, l = -2), "lines"),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  ggsave("imagenes/CPV_2025/tacna.png", width = 12, height = 10, dpi = 320, units = "in")

# 24. MAPA POBLACIONAL DE TUMBES -------------------------------
shp_provincia_pob_cpv |>  
  filter(NOMBDEP == "TUMBES") |> 
  ggplot() + 
  geom_sf(aes(fill = POBLACION.TOTAL.2025), color = "white", size = 0.3) +
  scale_fill_gradientn(
    name = "Población Total 2025", 
    colors = colores_personalizados, 
    labels = label_comma()  # Formato con separadores de miles en la leyenda
  ) +  # Usar la paleta continua personalizada
  geom_sf_text(aes(label = paste(NOMBPROV, "\n", comma(POBLACION.TOTAL.2025, format = "d"))), 
               size = 4, color = "black", fontface = "bold", 
               check_overlap = FALSE,  # Evitar superposición
               nudge_x = -0.0, nudge_y = -0.00) +  # Ajuste para evitar superposición
  labs(
    title = "PROVINCIAS DE TUMBES: POBLACIÓN TOTAL 2025",
    caption = "Fuente:INEI - Censos Nacionales 2025: XIII de Población, VIII de Vivienda y IV de Comunidades Indígenas.\nElaboración. www.denis-rodriguez.com",
    x = ""
  ) +
  theme(
    panel.background = element_blank(), 
    legend.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(size = 12, color = '#05204d', hjust = 0.5, vjust = 0, face = "bold"),
    plot.subtitle = element_text(size = 14, color = '#ac63a0', hjust = 0.0, vjust = 0),
    plot.caption = element_text(size = 10, color = "grey60", hjust = 0.0, vjust = 0),
    axis.title.x = element_text(size = 10, color = "grey20", hjust = 0.5, vjust = -6),
    legend.text = element_text(size = 10, color = "grey20"),
    legend.title = element_text(size = 10, color = "grey20"),
    strip.text = element_text(size = 12),
    plot.margin = unit(c(t = 1, r = -2, b = 1, l = -2), "lines"),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  ggsave("imagenes/CPV_2025/tumbes.png", width = 12, height = 10, dpi = 320, units = "in")

# 25. MAPA POBLACIONAL DE UCAYALI -------------------------------
shp_provincia_pob_cpv |>  
  filter(NOMBDEP == "UCAYALI") |> 
  ggplot() + 
  geom_sf(aes(fill = POBLACION.TOTAL.2025), color = "white", size = 0.3) +
  scale_fill_gradientn(
    name = "Población Total 2025", 
    colors = colores_personalizados, 
    labels = label_comma()  # Formato con separadores de miles en la leyenda
  ) +  # Usar la paleta continua personalizada
  geom_sf_text(aes(label = paste(NOMBPROV, "\n", comma(POBLACION.TOTAL.2025, format = "d"))), 
               size = 4, color = "black", fontface = "bold", 
               check_overlap = FALSE,  # Evitar superposición
               nudge_x = -0.0, nudge_y = -0.00) +  # Ajuste para evitar superposición
  labs(
    title = "PROVINCIAS DE UCAYALI: POBLACIÓN TOTAL 2025",
    caption = "Fuente:INEI - Censos Nacionales 2025: XIII de Población, VIII de Vivienda y IV de Comunidades Indígenas.\nElaboración. www.denis-rodriguez.com",
    x = ""
  ) +
  theme(
    panel.background = element_blank(), 
    legend.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(size = 12, color = '#05204d', hjust = 0.5, vjust = 0, face = "bold"),
    plot.subtitle = element_text(size = 14, color = '#ac63a0', hjust = 0.0, vjust = 0),
    plot.caption = element_text(size = 10, color = "grey60", hjust = 0.0, vjust = 0),
    axis.title.x = element_text(size = 10, color = "grey20", hjust = 0.5, vjust = -6),
    legend.text = element_text(size = 10, color = "grey20"),
    legend.title = element_text(size = 10, color = "grey20"),
    strip.text = element_text(size = 12),
    plot.margin = unit(c(t = 1, r = -2, b = 1, l = -2), "lines"),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  ggsave("imagenes/CPV_2025/ucayali.png", width = 14, height = 10, dpi = 320, units = "in")

# 22. MAPA POBLACIONAL DE SAN CALLAO -------------------------------
shp_provincia_pob_cpv |>  
  filter(NOMBDEP == "CALLAO") |> 
  ggplot() + 
  geom_sf(aes(fill = POBLACION.TOTAL.2025), color = "white", size = 0.3) +
  scale_fill_gradientn(
    name = "Población Total 2025", 
    colors = colores_personalizados, 
    labels = label_comma()  # Formato con separadores de miles en la leyenda
  ) +  # Usar la paleta continua personalizada
  geom_sf_text(aes(label = paste(NOMBPROV, "\n", comma(POBLACION.TOTAL.2025, format = "d"))), 
               size = 4, color = "black", fontface = "bold", 
               check_overlap = FALSE,  # Evitar superposición
               nudge_x = -0.0, nudge_y = -0.00) +  # Ajuste para evitar superposición
  labs(
    title = "CALLAO: POBLACIÓN TOTAL, 2025",
    caption = "Fuente:INEI - Censos Nacionales 2025: XIII de Población, VIII de Vivienda y IV de Comunidades Indígenas.\nElaboración. www.denis-rodriguez.com",
    x = ""
  ) +
  theme(
    panel.background = element_blank(), 
    legend.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(size = 12, color = '#05204d', hjust = 0.5, vjust = 0, face = "bold"),
    plot.subtitle = element_text(size = 14, color = '#ac63a0', hjust = 0.0, vjust = 0),
    plot.caption = element_text(size = 10, color = "grey60", hjust = 0.0, vjust = 0),
    axis.title.x = element_text(size = 10, color = "grey20", hjust = 0.5, vjust = -6),
    legend.text = element_text(size = 10, color = "grey20"),
    legend.title = element_text(size = 10, color = "grey20"),
    strip.text = element_text(size = 12),
    plot.margin = unit(c(t = 1, r = -2, b = 1, l = -2), "lines"),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  ggsave("imagenes/CPV_2025/callao.png", width = 10, height = 10, dpi = 320, units = "in")
