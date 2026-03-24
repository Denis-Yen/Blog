# CARGA DE LIBRERIAS ------------------------------------------------------
library(tidyverse)
library(openxlsx)
library(viridis)
library(sf)
library(ggspatial)
library(classInt)
library(grid)


# CARGA DE DATOS --------------------------------------------------------
## Archivos shapefile
shp_distritos <- st_read("data/DISTRITOS/DISTRITOS_inei_geogpsperu_suyopomalia.shp") |> 
  dplyr::select(
  UBIGEO, NOMBDEP, NOMBPROV, NOMBDIST, geometry
)

## Bases de datos de tuberculosis:
db_tbc <- read.xlsx("data/tbc.xlsx")

# MANIPULACIÓN DE DATOS ---------------------------------------------------

# Unir datos de casos de tuberculosis al shapefile de distritos
shp_dis_tbc <- shp_distritos |> left_join(db_tbc, by = c("UBIGEO"="Ubigeo")) 
# Clasificación de varibale Tasa de incidencia
jenks <- classIntervals(shp_dis_tbc$Morbilidad, n = 5, style = "jenks")

# Crear etiquetas automáticas con rangos boxjenks
etiquetas <- paste0(
  round(jenks$brks[-length(jenks$brks)], 1),
  "–",
  round(jenks$brks[-1], 1)
)


shp_dis_tbc$clase_inc_tbc <- cut(
  shp_dis_tbc$Morbilidad,
  breaks = jenks$brks,
  include.lowest = TRUE,
  labels = etiquetas
)

# Asignar colores específicos a cada categoría
colores_categorias <- c("1136–3118" = "#ee3d45", "579–1136" = "#f1646a", "290–579" = "#f58b8f", "71–290" = "#f8b1b5", "1–71" = "#fcd8da")


#centroides
# tbc
cents <- shp_dis_tbc %>% st_centroid() %>%  
  as_Spatial() %>%                  
  as.data.frame()

# 1. MAPA DE INCIDENCIA DE LA TUBERCULOSIS ---------------------------------
shp_dis_tbc|>  
  ggplot() + 
  geom_sf(aes(fill = factor(clase_inc_tbc)), color = "white", size = 0.0) +
  scale_fill_manual(
    values =  colores_categorias,
    name = "Tasa de incidencia"
  ) +
  labs(
    title = stringr::str_trim("Perú: Tasa de incidencias de la tuberculosis"),
    subtitle = "(n = 32, 950)",
    caption = "Fuente:  Acceder al tablero de datos estadísticos sobre tuberculosis (TB) en el Perú, 2024*.\n
    Elaboración: Denis Rodríguez (www.denis-rodriguez.com)")+
  guides(fill=guide_legend(
  direction = "vertical",
  keyheight = unit(4, "mm"),
  keywidth = unit(6, "mm"),
  title.position = 'top',
  title.hjust = 0.5,
  label.hjust = .5,
  reverse = F,
  label.position = "right"
))+ 
  geom_text(data = cents |> filter(Tasa_incidencia>=100), 
            aes(coords.x1, coords.x2, 
                label=NOMBDEP),
            size=0,
            fontface = "bold",
            color="white",
            family="georg", nudge_x = 0) +
  theme(
    panel.background = element_blank(), 
    legend.background = element_blank(),
    legend.position = "right",
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(size=22, color='#05204d', hjust=0.3, vjust=1, face = "bold"),
    plot.subtitle = element_text(size=18, color='#ac63a0', hjust=0.5, vjust=-1, face = "bold"),
    plot.caption = element_text(size=11, color="grey60", hjust=0.0, vjust=-1, lineheight = 0.4),
    axis.title.x = element_text(size=18, color="grey20", hjust=0.5, vjust=-6),
    legend.text = element_text(size=12, color="grey20"),
    legend.title = element_text(size=12, color="grey20"),
    strip.text = element_text(size=12),
    plot.margin = unit(c(t=1, r=-2, b=1, l=-2),"lines"),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  labs(x ="") -> map_niv

ggsave(plot = map_niv, "imagenes/map_tbc_niv.png", width = 2500, height = 2000, units = "px")
