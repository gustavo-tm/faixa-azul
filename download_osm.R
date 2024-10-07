library(tidyverse)
library(osmdata)
library(sf)
library(mapview)

# https://cran.r-project.org/web/packages/osmdata/vignettes/osmdata.html
# https://rspatialdata.github.io/osm.html

# https://wiki.openstreetmap.org/wiki/Map_features
available_features()

osm <- getbb('São Paulo') |> 
  opq(bbox = _) |> 
  add_osm_feature(key = 'highway', value = c(
    "motorway", "trunk", "primary", "secondary", "tertiary", "unclassified", "residential", "service",
    "motorway_link", "trunk_link", "primary_link", "secondary_link", "motorway_junction",
    "speed_camera"))  |> 
  osmdata_sf()

gg <- osm$osm_lines |> 
  ggplot() +
  geom_sf(lwd = .1) +
  theme_void()

ggsave("output/vias_osm.pdf", gg, width = 30, height = 40)

logradouros.OSM <- as_tibble(osm$osm_lines) |> 
  select(id_osm = osm_id,
         logradouro = name,
         logradouro_alt1 = alt_name,
         logradouro_alt2 = alt_name1,
         logradouro_alt3 = alt_name_1,
         logradouro_ref = ref,
         tipo_via = highway,
         faixas = lanes,
         limite_velocidade = maxspeed,
         limite_velocidade_pesados = "maxspeed:hgv",
         motocicleta = motorcycle,
         mao_unica = oneway,
         superficie = surface,
         geometry) 

logradouros.OSM |> st_write("dados_tratados/logradouros_osm.gpkg")

logradouros.OSM |>  
  st_as_sf() |>  
  mapview() |> 
  mapshot(url = "output/logradouros_osm.html")

distritos <- read_sf("dados_tratados/distrito/SIRGAS_SHP_distrito.shp") |> 
  st_set_crs("epsg:31983") |> 
  filter(ds_nome == "ITAIM BIBI")

logradouros.OSM |> 
  st_as_sf() |> st_transform("epsg:31983") |> 
  st_crop(distritos) |>
  mapview() |> 
  mapshot(url = "output/logradouros_osm_itaim.html")
