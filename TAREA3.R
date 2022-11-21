# cargamos librerias
library(tidyverse)
library(plotly)
library(sf)
library(DT)
library(leaflet)
library(terra)
library(leaflet.extras)
library(leafem)
library(raster)
library(rgdal)





#set working directory
setwd("~/UCR/maestria/II Semestre 2022/progra/tarea3")


# caragmso felinos
felinos <-
  st_read(
    "C:/Users/josel/Downloads/felinos.csv",
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude", # columna de longitud decimal
      "Y_POSSIBLE_NAMES=decimalLatitude"   # columna de latitud decimal
    ),
    quiet = TRUE
  )


###########################################pregunta 1#########################################

# tabla, ojo porque sale la columna de geometry

## Filtramos columnas que requerimos y lo guardamos como un objeto
tabla_felinos <-
  subset (felinos, select = c(species,
                              stateProvince,
                              locality,
                              eventDate))

# Tabla interativa de Felinos
tabla_felinos |>
  datatable(options = list(
    pageLength = 5,
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
  ))                            




#######################################PREGUNTA 2#########################################

# VOLVEMOS A CARGAR FELINOS, PERO ESTA VEZ USANDO LA paquete DE TIDYVERSE
felinos2 <- read_csv("felinos2.csv")



# Gráfico de barras apiladas por especie en cada mes

grafico_barras_felinos <-
  felinos2 |>
  ggplot(aes(x = month, fill = species, na.rm = TRUE)) +
  geom_bar() +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)) +
  ggtitle("Cantidad de registros por especie en cada mes") +
  xlab("Mes") +
  ylab("Cantidad") +
  labs(fill = "Especie") +
  theme_classic() +
  theme(legend.position = "top")

# Gráfico de barras plotly
ggplotly(grafico_barras_felinos) |> 
  config(locale = 'es')
 

##############################pregunta 3############################3

#cargamos Shape de provincias

provincias <-
  st_read(
    "C:/Users/josel/OneDrive/Documentos/ATLAS2014_CRTM05/Provincias2014crtm05/Provincias2014crtm05.shp",
    quiet = TRUE # para evitar el despliegue de mensajes
  )

#Caragmos raster de altitud
altitud <-
  rast(
    "altitud.tif"
  )

# Asignación de un CRS al objeto felinos
st_crs(felinos) <- 4326

# Transformación del CRS del objeto provincias a WGS84 (EPSG = 4326)
provincias <-
  provincias |>
  st_transform(4326)



# Paleta de colores de altitud de Costa Rica
colores_altitud <-
  colorNumeric(topo.colors(25),
               values(altitud),
               na.color = "transparent")



#Procedemos a Mapear

leaflet() |>
  
  setView(# centro y nivel inicial de acercamiento
    lng = -84.19452,
    lat = 9.572735,
    zoom = 7) |>
  addTiles(group = "OpenStreetMap") |> # capa base de OSM |>
  addRasterImage( # capa raster
    raster(altitud), # conversión de SpatRaster a RasterLayer 
    colors = colores_altitud, # paleta de colores
    opacity = 0.6,
    group = "Altitud",
  ) |>
  addLegend(
    title = "Altitud",
    values = values(altitud),
    pal = colores_altitud,
    position = "bottomleft",
    group = "Altitud"
  ) |>
  addProviderTiles(providers$Stamen, group = "Stamen") |>
  addProviderTiles(providers$Esri.NatGeoWorldMap,  group = "Esri.NatGeoWorldMap" ) |>
  addPolygons(
    # capa de provincias (polígonos)
    data = provincias,
    color = "black",
    fillColor = "transparent",
    stroke = TRUE,
    weight = 1.0,
    group = "Provincias",
    popup = paste(
      paste0("<strong>Provincia: </strong>", provincias$PROVINCIA),
      sep = '<br/>'
    )) |>
  
  addCircleMarkers(
    # capa de registros de presencia (puntos)
    data = felinos,
    stroke = F,
    radius = 4,
    fillColor = 'Black',
    fillOpacity = 1,
    group = "Felinos",
    popup = paste(
      paste0("<strong>Especie: </strong>", felinos$species),
      paste0("<strong>Localidad: </strong>", felinos$locality),
      paste0("<strong>Fecha: </strong>", felinos$eventDate),
      paste0("<strong>Fuente: </strong>", felinos$institutionCode),
      paste0("<a href='", felinos$occurrenceID, "'>Más información</a>"),
      sep = '<br/>'
    )
  ) |>
  addLayersControl(
    baseGroups = c("OpenStreetMap", "Stamen", "Esri.NatGeoWorldMap"),
    overlayGroups = c("Altitud","Provincias", "Felinos")) |> # control de capas
  addResetMapButton() |> # botón de reinicio
  addSearchOSM() |> # búsqueda en OSM
  addMouseCoordinates() |> # coordenadas del puntero del ratón
  addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE)) |> # barra de escala
  addMiniMap(position = "bottomleft") # mapa de ubicación







