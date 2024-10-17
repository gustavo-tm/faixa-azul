library(tidyverse)
library(sf)
library(mapview)

distrito <- read_sf("dados_tratados/distrito/SIRGAS_SHP_distrito.shp") |> 
  st_set_crs("epsg:31983") |> 
  # filter(ds_subpref == "SE") |> 
  summarize(geom = st_union(geometry) |> st_simplify(dTolerance = 100))

logradouros <- st_read("dados_tratados/logradouros_osm.gpkg") |> 
  st_transform("epsg:31983") |> 
  filter(tipo_via != "service",
         tipo_via |> str_detect("link", negate = TRUE),
         as.logical(st_intersects(geom, distrito)))

acidentes <- read_csv("dados_tratados/acidentes.csv") |> 
  drop_na() |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> 
  st_transform("epsg:31983") |> 
  filter(as.logical(st_intersects(geometry, distrito)))

obitos <- read_csv("dados_tratados/obitos.csv") |> 
  drop_na() |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> 
  st_transform("epsg:31983") |> 
  filter(as.logical(st_intersects(geometry, distrito)))
  
match <- obitos |> 
  mutate(nearest = st_nearest_feature(geometry, logradouros),
         id_acidente = row_number()) |> 
  left_join(logradouros |> 
              st_drop_geometry() |> 
              mutate(nearest = row_number()) |> 
              separate_wider_delim(logradouro_ref, ";", names_sep = "_", too_few = "align_start") |> 
              pivot_longer(cols = starts_with("logradouro")) |> 
              select(nearest, match = value) |> 
              drop_na() |> 
              mutate(match = match |> 
                       stringi::stri_trans_general("latin-ascii") |> 
                       str_to_upper() |> 
                       str_replace_all("[[:punct:]]", ""))) |> 
  mutate(semelhanca = stringdist::stringsim(logradouro, match)) |> 
  group_by(id_acidente, logradouro, veiculo) |>
  filter(semelhanca == max(semelhanca))

mapview(match, zcol = "semelhanca") |> 
  mapshot(url = "output/mapas/join_distancia.html")


match |> 
  st_drop_geometry() |> 
  ggplot() +
  geom_histogram(aes(x = semelhanca))

