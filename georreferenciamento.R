library(tidyverse)
library(sf)
library(mapview)
library(gt)

distrito <- read_sf("dados_tratados/distrito/SIRGAS_SHP_distrito.shp") |> 
  st_set_crs("epsg:31983") |> 
  # filter(ds_subpref == "SE") |> 
  summarize(geom = st_union(geometry) |> st_simplify(dTolerance = 100))

logradouros <- st_read("dados_tratados/logradouros_osm.gpkg") |> 
  st_transform("epsg:31983") |> 
  filter(tipo_via != "service",
         tipo_via |> str_detect("link", negate = TRUE),
         as.logical(st_intersects(geom, distrito)))

sinistros <- read_csv("dados_tratados/sinistros.csv")

sinistros.geo <- sinistros |> 
  select(-quantidade) |> 
  drop_na() |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> 
  st_transform("epsg:31983") |> 
  filter(as.logical(st_intersects(geometry, distrito))) |> 
  mutate(tipo = as.factor(tipo))

sinistros.na <- sinistros |> 
  filter(is.na(latitude) | is.na(longitude)) |> 
  mutate(recuperavel = ifelse(logradouro != "NAO DISPONIVEL" & numero != 0, TRUE, FALSE))


match <- sinistros.geo |> 
  mutate(nearest = st_nearest_feature(geometry, logradouros),
         id_acidente = row_number()) |> 
  left_join(logradouros |> 
              st_drop_geometry() |> 
              mutate(nearest = row_number()) |> 
              separate_wider_delim(logradouro_ref, ";", names_sep = "_", too_few = "align_start") |> 
              pivot_longer(cols = starts_with("logradouro")) |> 
              select(id_osm, nearest, match = value) |> 
              drop_na() |> 
              mutate(match = match |> 
                       stringi::stri_trans_general("latin-ascii") |> 
                       str_to_upper() |> 
                       str_replace_all("[[:punct:]]", ""))) |> 
  mutate(semelhanca = stringdist::stringsim(logradouro, match)) |> 
  group_by(id_acidente, logradouro) |>
  filter(semelhanca == max(semelhanca)) |> 
  ungroup()

mapview(match |> sample_n(10000), zcol = "semelhanca") |> 
  mapshot(url = "output/mapas/join_distancia.html")


match |> 
  st_drop_geometry() |> 
  ggplot() +
  geom_histogram(aes(x = semelhanca))


# match |> 
#   left_join(read_csv("dados_tratados/faixa_azul_selecao.csv") |> 
#               mutate(faixa_azul = TRUE, id_osm = as.character(id_osm))) |> 
#   st_drop_geometry() |> 
#   mutate(faixa_azul = replace_na(faixa_azul, FALSE)) |> 
#   group_by(ano = year(data), mes = month(data), faixa_azul) |> 
#   summarize(sinistros = n()) |> 
#   ggplot(aes(x = make_date(year = ano, month = mes), y = sinistros, colour = faixa_azul)) +
#   geom_line()




df <- logradouros |> 
  st_drop_geometry() |> 
  as_tibble() |> 
  select(id_osm, faixas, limite_velocidade, mao_unica, superficie, tipo_via) |> 
  left_join(read_csv("dados_tratados/faixa_azul_selecao.csv") |>
              mutate(trecho_faixa_azul = TRUE, id_osm = as.character(id_osm))) |>
  mutate(trecho_faixa_azul = replace_na(trecho_faixa_azul, FALSE)) |> 
  right_join(match) |> 
  filter(tipo != "NOTIFICACAO") |> 
  left_join(readxl::read_excel("dados_tratados/vias_faixa_azul.xlsx") |>
              mutate(data = make_date(year = ano, month = mes), via_faixa_azul = TRUE) |> 
              select(match = logradouro_osm, data_faixa_azul = data, via_faixa_azul)) |> 
  mutate(via_faixa_azul = replace_na(via_faixa_azul, FALSE),
         tratamento = ifelse(trecho_faixa_azul == TRUE & data_faixa_azul < data, TRUE, FALSE),
         fatal = tipo == "SINISTRO FATAL")

df |> distinct(tipo_via)

lm(fatal ~ as.numeric(limite_velocidade) + as.numeric(faixas) + tipo_via + mao_unica + superficie + motocicleta + tratamento + via_faixa_azul + trecho_faixa_azul, data = df) |> 
  summary()

glm(fatal ~ as.numeric(limite_velocidade) + as.numeric(faixas) + tipo_via + mao_unica + superficie + motocicleta + tratamento + via_faixa_azul + trecho_faixa_azul, 
    data = df, family = "binomial") |> 
  summary()





temp <- logradouros |> 
  mutate(tamanho = st_length(geom)) |> 
  st_drop_geometry() |> 
  select(id_osm, faixas, limite_velocidade, mao_unica, superficie, tipo_via, tamanho) |> 
  left_join(read_csv("dados_tratados/faixa_azul_selecao.csv") |>
              mutate(trecho_faixa_azul = TRUE, id_osm = as.character(id_osm))) |> 
  left_join(match |> 
              st_drop_geometry() |> 
              group_by(id_osm) |> 
              summarize(semelhanca = mean(semelhanca),
                        sinistros = n(),
                        sinistros_moto = sum(motocicleta))) |> 
  mutate(across(c(trecho_faixa_azul, semelhanca, sinistros, sinistros_moto), ~ replace_na(.x, 0))) |> 
  group_by(trecho_faixa_azul) |> 
  summarize(
    across(
      c(sinistros, sinistros_moto, faixas, limite_velocidade, tamanho, semelhanca), 
      ~ .x |> as.numeric() |> mean(na.rm = TRUE) |> round(2)),
    across(
      c(mao_unica, superficie, tipo_via), 
      ~ fct_infreq(.x) |> levels() |> first())) |> 
  mutate(across(everything(), ~ as.character(.x))) |>
  pivot_longer(2:10) |> 
  pivot_wider(names_from = trecho_faixa_azul, id_cols = name, values_from = value)

temp |> 
  gt() |> 
  gtsave("output/comparativo-grupos.html")





