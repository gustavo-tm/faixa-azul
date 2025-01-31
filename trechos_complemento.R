library(tidyverse)
library(sf)
library(osmdata)

trechos <- read_sf("banco_dados/trechos.gpkg")

# Radar próximo ----
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

radar_proximo <- trechos |> 
  st_join(radares |> 
            st_buffer(100)) |> 
  st_drop_geometry() |> 
  filter(!is.na(id_osm_radar), limite_velocidade.x == limite_velocidade.y) |> 
  distinct(id_osm) |> 
  mutate(radar_proximo = TRUE) |> 
  right_join(trechos |> st_drop_geometry() |> select(id_osm)) |> 
  mutate(radar_proximo = replace_na(radar_proximo, 0))

# Intersecções ----

osm.token <- read_csv("dados_tratados/osm-token.csv", col_types = list("id_osm" = "c")) |> 
  select(id_osm, logradouro_limpo) |> 
  group_by(id_osm) |> 
  filter(row_number() == 1)

intersec <- trechos |> 
  filter(tipo_via != "service") |> 
  select(id_osm) |> 
  left_join(osm.token) |> 
  (\(df) st_join(df, df))() |> 
  st_drop_geometry() |> 
  filter(id_osm.x != id_osm.y,
         logradouro_limpo.x != logradouro_limpo.y) |> 
  group_by(id_osm = id_osm.x) |> 
  summarize(intersec = n()) |> 
  select(id_osm, intersec)

# POIs ----


amenidades <- getbb('São Paulo') |> 
  opq(bbox = _) |> 
  add_osm_feature(key = 'amenity')  |> 
  osmdata_sf() |> 
  (\(amenidades) bind_rows(
    as_tibble(amenidades$osm_points) |> filter(!is.na(amenity)),
    as_tibble(amenidades$osm_polygons) |> filter(!is.na(amenity)),
    as_tibble(amenidades$osm_multipolygons) |> filter(!is.na(amenity))))() |> 
  select(id_osm_amenidade = osm_id, nome = name, tipo_amenidade = amenity, geometry)


# Output ----

complemento <- trechos |> 
  st_drop_geometry() |> 
  select(id_osm) |> 
  left_join(intersec) |> 
  left_join(radar_proximo)

write_csv(complemento, "banco_dados/trechos_complemento.csv")

