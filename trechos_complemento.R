library(tidyverse)
library(sf)
library(osmdata)


radares <- getbb('São Paulo') |> 
  opq(bbox = _) |> 
  add_osm_feature(key = 'highway', value = c("speed_camera")) |> 
  osmdata_sf() |> 
  (\(radar) as_tibble(radar$osm_points))() |> 
  select(id_osm_radar = osm_id,
         limite_velocidade = maxspeed,
         limite_velocidade_pesados = "maxspeed:hgv",
         geometry)

st_write(radares, "dados_tratados/radares.gpkg")
radares <- read_sf("dados_tratados/radares.gpkg")


