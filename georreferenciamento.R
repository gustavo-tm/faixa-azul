library(tidyverse)
library(sf)
library(mapview)

distrito <- read_sf("dados_tratados/distrito/SIRGAS_SHP_distrito.shp") |> 
  st_set_crs("epsg:31983") |> 
  filter(!ds_nome %in% c("PARELHEIROS", "MARSILAC", "RIO PEQUENO", "JAGUARE"))

logradouros <- st_read("dados_tratados/logradouros_osm.gpkg") |> 
  st_transform("epsg:31983") |> 
  mutate(logradouro = logradouro |> 
           stringi::stri_trans_general("latin-ascii") |> 
           str_to_upper() |> 
           str_replace_all("[[:punct:]]", ""))


faixa_azul <- readxl::read_excel("dados_tratados/vias_faixa_azul.xlsx") |> 
  select(logradouro = logradouro_osm, ano, mes) |> 
  mutate(data = make_date(year = ano, month = mes)) |> 
  select(-ano, -mes)

logradouros |> 
  right_join(faixa_azul) |>
  filter(st_intersects(geom, distrito |> st_union()) |> as.logical()) |>
  mutate(data = as.factor(data)) |> 
  mapview(zcol = "data") |> 
  mapshot(url = "output/logradouros_faixa_azul.html")

mapa <- logradouros |> 
  right_join(faixa_azul) |>
  filter(st_intersects(geom, distrito |> st_union()) |> as.logical()) |>
  mutate(data = as.factor(data),
         buffer = st_buffer(geom, 500)) |> 
  (\(dados) 
    mapview(distrito) + mapview(dados |> summarize(geom = st_union(buffer))) + 
    mapview(dados |> st_set_geometry("geom"), zcol = "data"))(dados = _) 

(mapa + logradouros |> 
  right_join(faixa_azul) |>
  filter(st_intersects(geom, distrito |> st_union()) |> as.logical()) |>
  mutate(data = as.factor(data),
         buffer = st_buffer(geom, 500)) |> 
  summarize(area = st_union(buffer)) |> 
  (\(buffer) logradouros |> filter(st_intersects(geom, buffer) |> as.logical(), 
                                   !tipo_via %in% c("unclassified", "service")))(buffer = _) |> 
  mapview()) |> 
  mapshot(url = "output/logradouros_buffer_faixa_azul.html")


id.buffer <- logradouros |> 
  right_join(faixa_azul) |>
  filter(st_intersects(geom, distrito |> st_union()) |> as.logical()) |>
  mutate(data = as.factor(data),
         buffer = st_buffer(geom, 500)) |> 
  summarize(area = st_union(buffer)) |> 
  (\(buffer) logradouros |> filter(st_intersects(geom, buffer) |> as.logical(), 
                                   !tipo_via %in% c("unclassified", "service")))(buffer = _) |> 
  st_drop_geometry() |> 
  pull(id_osm)

id.faixa <- logradouros |> 
  right_join(faixa_azul) |>
  filter(st_intersects(geom, distrito |> st_union()) |> as.logical()) |> 
  pull(id_osm)

saveRDS(list("faixa_azul" = id.faixa, "buffer" = id.buffer), "osm_id.rds")






