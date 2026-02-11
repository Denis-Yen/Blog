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
shp_departamentos <- st_read("data/SHP PERU/DEPARTAMENTOS/DEPARTAMENTOS_inei_geogpsperu_suyopomalia.shp")

## Bases de datos de:
investigadores <- read.xlsx("data/invest_mujeres.xlsx")


shp_dep_inves <- shp_departamentos |> left_join(investigadores, by = c("CCDD"="IID_REG"))


# MANIPULACIÓN DE DATOS ---------------------------------------------------

# Unir datos de INVESTIGADORESinvestigadores al shapefile de departamento
shp_dep_inves <- shp_departamentos |> left_join(investigadores, by = c("CCDD"="IID_REG"))

# Clasificación de varibale mujer
jenks <- classIntervals(shp_dep_inves$MUJER, n = 5, style = "jenks")

# Crear etiquetas automáticas con rangos boxjenks
etiquetas <- paste0(
  round(jenks$brks[-length(jenks$brks)], 1),
  "–",
  round(jenks$brks[-1], 1)
)


shp_dep_inves$clase_mujer <- cut(
  shp_dep_inves$MUJER,
  breaks = jenks$brks,
  include.lowest = TRUE,
  labels = etiquetas
)

# Asignar colores específicos a cada categoría
colores_categorias <- c("312–2103" = "#5a056c", "207–312" = "#c628a1", "115–207" = "#f281ac", "65–115" = "#fda7b4", "20–65" = "#ffc6c5")


#centroides
# Mujeres
cents <- shp_dep_inves %>% st_centroid() %>%  
  as_Spatial() %>%                  
  as.data.frame()

# 1. MAPA DE INVESTIGADORES MUJERES ----------------------------------------------------------
shp_dep_inves|>  
  ggplot() + 
  geom_sf(aes(fill = factor(clase_mujer)), color = "white", size = 0.0) +
  scale_fill_manual(
    values =  colores_categorias,
    name = "Mujeres investigadores"
  ) +
  labs(
    title = stringr::str_trim("Perú: Mujeres investigadores según departamento"),
    subtitle = "(n = 4,647)",
    caption = "Fuente:  Registro Nacional Científico, Tecnológico y de Innovación Tecnológica (RENACYT). Al 10/02/2026.
    Elaboración: Denis Rodríguez (https://denis-rodriguez.com)")+
  guides(fill=guide_legend(
    direction = "vertical",
    keyheight = unit(1.15, units = "mm"),
    keywidth = unit(12, units = "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.hjust = .5,
    nrow = 1,
    byrow = T,
    reverse = F,
    label.position = "bottom"
  )) + 
  
  geom_text(data = cents |> filter(MUJER>=115), 
            aes(coords.x1, coords.x2, 
                label=NOMBDEP),
            size=0,
            fontface = "bold",
            color="white",
            family="georg", nudge_x = 0) +
  theme(
    panel.background = element_blank(), 
    legend.background = element_blank(),
    legend.position = c(.55, .001),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(size=60, color='#05204d', hjust=0.5, vjust=-1, face = "bold"),
    plot.subtitle = element_text(size=55, color='#ac63a0', hjust=0.5, vjust=-1, face = "bold"),
    plot.caption = element_text(size=40, color="grey60", hjust=0.0, vjust=-1, lineheight = 0.3),
    axis.title.x = element_text(size=18, color="grey20", hjust=0.5, vjust=-6),
    legend.text = element_text(size=35, color="grey20"),
    legend.title = element_text(size=35, color="grey20"),
    strip.text = element_text(size=12),
    plot.margin = unit(c(t=1, r=-2, b=1, l=-2),"lines"),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  labs(x ="") -> map

  #map + ggsave("imagenes/map_mujeres.png", width = 8, height = 8, dpi = 320, units = "in")

# GRÁFICO DE BARRAS

shp_dep_inves |> 
  ggplot(aes(x=reorder(NOMBDEP, MUJER), 
             y=MUJER, fill = clase_mujer)) + 
  geom_bar(stat='identity') +
  coord_flip()+
  geom_text(data=subset(shp_dep_inves, MUJER<294 &MUJER>100),
            aes(label = MUJER),
            position = position_stack(vjust = 0.8), 
            hjust=0.5,
            size=11,
            fontface = "bold",
            color="#4b4848ff",
            family="georg") +
  geom_text(data=subset(shp_dep_inves, MUJER>=294),
            aes(label = MUJER),
            position = position_stack(vjust = 0.5), 
            hjust=0.5,
            size=11,
            color="white",
            fontface = "bold",
            family="georg")+
  scale_fill_manual( values = rev(c("#5a056c", "#c628a1", "#f281ac", "#fda7b4","#ffc6c5")),
                     name="" 
  ) +
   theme_void() +
        theme(
            axis.text.y = element_text(
                margin = unit(c(3, 0, 0, 0), "mm"),
                color = "grey10",
                size = 30,
                hjust = 1
            ),
            legend.position = "none"
        ) -> b
  
#b+ggsave("imagenes/inves_muj_bar.png", width = 8, height = 8, dpi = 320, units = "in")

# COMBINAR MAPA

create_inset_graph_map <- function(){

  suppressWarnings({

    vp <- grid::viewport(
      width = .30,
      height = .92,
      x = .25,
      y = .45
    )

    if(!dir.exists("imagenes")){
      dir.create("imagenes")
    }

    png("imagenes/img_muj_joined.png",
        width = 3600,
        height = 3600,
        res = 350
    )

    grid::grid.newpage()

    print(map)
    print(b, vp = vp)

    dev.off()

  })

  invisible(TRUE)
}

create_inset_graph_map()

