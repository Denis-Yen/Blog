# CARGA DE LIBRERIAS ------------------------------------------------------
library(tidyverse)
library(openxlsx)
library(viridis)
library(sf)
library(ggspatial)
library(classInt)
library(grid)


# 1. CARGA DE DATOS --------------------------------------------------------
## Bases de datos de poblacion y tuberculosis a nivel distrital:
pob_dist <- read.xlsx("data/pob_dist_24.xlsx")
db_tbc_dist <- read.xlsx("data/tbc.xlsx") 

## Archivos shapefile de provincias
shp_prov<- st_read("data/PROVINCIAS/PROVINCIAS_inei_geogpsperu_suyopomalia.shp")|> 
  dplyr::select(
  IDPROV, NOMBDEP, NOMBPROV, geometry
)

# 2. MANIPULACIÓN DE DATOS ---------------------------------------------------
## Uniendo la base de datos de tuberculosis con la de población  nivel distrito:
  
db_tbc_dist_join <- db_tbc_dist |> 
  left_join( pob_dist, by = c("Ubigeo" = "UBIGEO"))

# Agregamos a nivel de provincia

db_tbc_pro <- db_tbc_dist_join |> 
  mutate(
  Ubigeo_prov = substring(Ubigeo,first = 1, last = 4)) |>
    group_by(Ubigeo_prov, Departamento, Provincia) |> 
    summarise(
      Poblacion = sum(POBLACION_24),
      Morbilidad = sum(Morbilidad),
      Incidencia_A = sum(Incidencia_A),
      Incidencia_B = sum(Incidencia_B),
      Tasa_morbilidad = (Morbilidad/Poblacion)*100000,
      Tasa_incidencia = (Incidencia_A/Poblacion)*100000
    )

## Unir datos de casos de tuberculosis al shapefile de provincias
shp_prov_tbc <- shp_prov |> left_join(db_tbc_pro, by = c("IDPROV"="Ubigeo_prov"))
 
# Clasificación de varibale Tasa de incidencia
jenks <- classIntervals(shp_prov_tbc$Tasa_incidencia, n = 5, style = "jenks")

# Crear etiquetas automáticas con rangos boxjenks o cortes naturales
etiquetas <- paste0(
  round(jenks$brks[-length(jenks$brks)], 1),
  "–",
  round(jenks$brks[-1], 1)
)

shp_prov_tbc$clase_inc_tbc <- cut(
  shp_prov_tbc$Tasa_incidencia,
  breaks = jenks$brks,
  include.lowest = TRUE,
  labels = etiquetas
)

# Asignar colores específicos a cada categoría
colores_categorias <- c(
  "5.7–31.9"    = "#fcd8da",
  "31.9–58.4"   = "#f8b1b5",
  "58.4–97.7"   = "#f58b8f",
  "97.7–170.5"  = "#f1646a",
  "170.5–307.1" = "#ee3d45"
)

#centroides
# tbc
cents <- shp_prov_tbc %>%
  st_make_valid() %>%
  filter(!is.na(Tasa_incidencia)) %>%
  st_point_on_surface() %>%
  as_Spatial() %>%
  as.data.frame()

# 3. MAPA DE INCIDENCIA DE LA TUBERCULOSIS ---------------------------------
shp_prov_tbc|>  
  ggplot() + 
  geom_sf(aes(fill = factor(clase_inc_tbc)), color = "white", size = 0.0) +
  scale_fill_manual(
    values =  colores_categorias,
    name = "Tasa de incidencia"
  ) +
  labs(
    title = stringr::str_trim("Perú: Tasa de incidencia de la tuberculosis"),
    subtitle = "(n = 32, 950)",
    caption = "Fuente: MINSA. Tablero de datos estadísticos sobre tuberculosis (TB) en el Perú, 2024*.\nElaboración: Denis Rodríguez (www.denis-rodriguez.com)")+
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
  geom_text(
  data = cents |> filter(Tasa_incidencia >= 240), aes(coords.x1, coords.x2, label = paste0(NOMBPROV, "\n", round(Tasa_incidencia, 1))
  ),
  size = 3,
  fontface = "bold",
  color = "#000000",
  family = "georg") +
  theme(
    panel.background = element_blank(), 
    legend.background = element_blank(),
    legend.position = "right",
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(size=22, color='#05204d', hjust=0.3, vjust=1, face = "bold"),
    plot.subtitle = element_text(size=18, color='#ac63a0', hjust=0.5, vjust=-1, face = "bold"),
    plot.caption = element_text(size=11, color="grey60", hjust=0.0, vjust=-1, lineheight = 0.8),
    axis.title.x = element_text(size=18, color="grey20", hjust=0.5, vjust=-6),
    legend.text = element_text(size=12, color="grey20"),
    legend.title = element_text(size=12, color="grey20"),
    strip.text = element_text(size=12),
    plot.margin = unit(c(t=1, r=-2, b=1, l=-2),"lines"),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  labs(x ="") -> map_niv_prov

ggsave(plot = map_niv_prov, "imagenes/map_tbc_niv_prov.png", width = 2500, height = 2000, units = "px")

