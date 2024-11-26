library(tidyverse)
library(sf)
# library(mapview)
# library(gt)


trechos <- st_read("banco_dados/trechos.gpkg") |> 
  st_transform("epsg:31983") |> 
  filter(tipo_via != "service")

sinistros <- read_csv("banco_dados/sinistros.csv") |> 
  filter(!is.na(longitude), !is.na(latitude)) |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> 
  st_transform("epsg:31983")

match <- sinistros |> 
  mutate(nearest = st_nearest_feature(geometry, trechos),
         distancia_geografica = st_distance(geometry, trechos$geom[nearest], by_element = TRUE) |> as.numeric()) |> 
  left_join(trechos |> 
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
  mutate(distancia_semantica = stringdist::stringsim(logradouro, match)) |> 
  group_by(id_sinistro, logradouro) |>
  filter(distancia_semantica == max(distancia_semantica)) |> 
  ungroup()

match |> 
  st_drop_geometry() |> 
  select(id_sinistro, id_osm, data, distancia_geografica, distancia_semantica) |> 
  write_csv("banco_dados/match_geografico.csv")


# mapview(match |> sample_n(10000), zcol = "semelhanca") |> 
#   mapshot(url = "output/mapas/join_distancia.html")
# 
# match |> 
#   st_drop_geometry() |> 
#   ggplot() +
#   geom_histogram(aes(x = semelhanca))
# 
# 
# match |> 
#   write_csv("dados_tratados/match_sinistros.csv")
# 
# 
# match |>  
#   st_drop_geometry() |> 
#   semi_join(logradouros |> 
#               filter(tipo_via %in% c("trunk", "primary", "secondary", "tertiary")) |> 
#               select(id_osm)) |> 
#   group_by(ano = year(data), mes = month(data), id_osm) |> 
#   summarize(sinistros = n(), .groups = "drop") |> 
#   complete(ano, mes, id_osm, 
#            fill = list(sinistros = 0)) |> 
#   write_csv("dados_tratados/match_trechos.csv")













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
  left_join(readxl::read_excel("dados_tratados/faixa_azul_vias.xlsx") |>
              mutate(data = make_date(year = ano, month = mes), via_faixa_azul = TRUE) |> 
              select(match = logradouro_osm, data_faixa_azul = data, via_faixa_azul)) |> 
  mutate(via_faixa_azul = replace_na(via_faixa_azul, FALSE),
         tratamento = ifelse(trecho_faixa_azul == TRUE & data_faixa_azul < data, TRUE, FALSE),
         fatal = tipo == "SINISTRO FATAL")


lm(fatal ~ as.numeric(limite_velocidade) + as.numeric(faixas) + tipo_via + mao_unica + superficie + motocicleta + tratamento + via_faixa_azul + trecho_faixa_azul + factor(ano) + factor(mes), 
   data = df |> mutate(ano = year(data), mes = month(data)) |> filter(ano > 2018)) |> 
  summary()

glm(fatal ~ as.numeric(limite_velocidade) + as.numeric(faixas) + tipo_via + mao_unica + superficie + motocicleta * tratamento + via_faixa_azul + trecho_faixa_azul + factor(ano) + factor(mes), 
    data = df |> mutate(ano = year(data), mes = month(data)) |> filter(ano > 2018), family = "binomial") |> 
  summary()



df |> 
  distinct(id_osm, tipo_via, trecho_faixa_azul) |> 
  group_by(trecho_faixa_azul) |> 
  count(tipo_via)

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


logradouros <- st_read("dados_brutos/logradouros/SIRGAS_SHP_logradouronbl_line.shp")



