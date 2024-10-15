library(tidyverse)
library(sf)
library(mapview)

distrito <- read_sf("dados_tratados/distrito/SIRGAS_SHP_distrito.shp") |> 
  st_set_crs("epsg:31983") |> 
  filter(ds_subpref == "SE") |> 
  summarize(geom = st_union(geometry))

logradouros <- st_read("dados_tratados/logradouros_osm.gpkg") |> 
  st_transform("epsg:31983") |> 
  mutate(logradouro = logradouro |> 
           stringi::stri_trans_general("latin-ascii") |> 
           str_to_upper() |> 
           str_replace_all("[[:punct:]]", "")) |> 
  filter(as.logical(st_intersects(geom, distrito)),
         tipo_via != "service")



acidentes <- read_csv("dados_tratados/acidentes.csv") |> 
  drop_na() |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> 
  st_transform("epsg:31983") |> 
  filter(as.logical(st_intersects(geometry, distrito)))

acidentes |> mutate(test = st_nearest_feature(geometry, logradouros |> st_line_sample(density = units::set_units(50, m))))
acidentes |> mutate(test = st_nearest_feature(geometry, logradouros))


# A) VORONOI COM LINESTRING
voronoi.A <- logradouros |>
  st_cast("MULTIPOINT") |>
  summarize(x = st_union(geom)) |>
  st_voronoi() |>
  st_collection_extract()

# B) VORONOI COM INTERPOLACAO
voronoi.B <- logradouros |> 
  st_line_sample(density = units::set_units(50, m)) |> 
  st_as_sf() |> 
  summarize(x = st_union(x)) |> 
  st_voronoi() |> 
  st_collection_extract()

# C) VORONOI COM AMBOS
voronoi.C <- logradouros |> 
  mutate(interpolacao = st_line_sample(x = geom, density = units::set_units(50, m))) |> 
  st_cast("MULTIPOINT") |> 
  summarize(x = st_union(st_union(interpolacao), st_union(geom))) |> 
  st_voronoi() |>
  st_collection_extract()

# D) VORONOI COM B + começo e fim de A
voronoi.D <- logradouros |> 
  mutate(interpolacao = st_line_sample(x = geom, density = units::set_units(50, m)),
         ends = st_line_sample(x = geom, sample = c(0, 1))) |> 
  st_cast("MULTIPOINT") |> 
  summarize(x = st_union(st_union(interpolacao), st_union(ends))) |> 
  st_voronoi() |>
  st_collection_extract()

mapview(voronoi.A)
mapview(voronoi.B)
mapview(voronoi.C)
mapview(voronoi.D)




mapview(logradouros |> 
          st_cast("MULTIPOINT")) +

mapview(logradouros)
mapview(logradouros |> 
          st_line_sample(density = units::set_units(50, m)) |> 
          st_as_sf() |> 
          summarize(x = st_union(x))) + mapview(voronoi)


