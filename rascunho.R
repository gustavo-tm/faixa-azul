library(sf)
library(tidyverse)
library(targets)

tar_meta(fields = "error") |>View()


library(qs)

df <- tar_read(did_df, branches = 1)

library(AER)
library(MASS)
library(lmtest)

df |> filter(sinistros_pre2022 == 0)


segmentos_filtrado |>
  pivot_longer(starts_with("id")) |> 
  drop_na(value) |> 
  pivot_wider(id_cols = everything(), names_from = name, values_from = value) |> 
  mutate(ID = row_number())


segmentos_filtrado |> 
  group_by()
  pivot_longer(starts_with("id")) |>
  drop_na(value) |> 
  distinct(value)
  View()

df |> 
  distinct(ID, sinistros_pre2022) |> 
  ggplot() +
  geom_histogram(aes(x = sinistros_pre2022))




glm.nb(y ~ x1 + x2, data = df)

dispersiontest(glm(sinistros ~ 1, family = poisson, data = df|> 
                     filter(sinistros_pre2022 > 60)))

nb <- glm.nb(sinistros ~ 1, data = df)
pois <- glm(sinistros ~ 1, family = poisson, data = df)

AIC(pois, nb)
lrtest(pois, nb)


library(did)

fit_m10 <- fit_did(df |> filter(sinistros_pre2022 > 10), log_delta = 1)

fit_m10 |> 
  aggte(type = "dynamic", min_e = -12  / intervalo_meses, max_e = 12 / intervalo_meses, na.rm = TRUE) |> 
  ggdid() +
  # scale_y_continuous(expand = expansion(mult = expand_grid)) +
  scale_x_continuous("Meses até data da implementação", breaks = c(0:9-4)*3) +
  scale_colour_manual(values = c("red", "blue"), labels = c("Pré faixa azul", "Pós faixa azul")) +
  # labs(title = title) +
  theme_minimal() +
  theme(legend.position = "top")


df |> 
  filter(sinistros_pre2022 > 30) |>
  group_by(sinistros) |> 
  summarize(n = n()) |> 
  ggplot() +
  geom_col(aes(x = sinistros, y = n)) +
  xlim(c(NA, 10))

df |> 
  filter(sinistros_pre2022 > 1) |>
  summarize(mean = mean(sinistros),
            var = var(sinistros))

df |> 
  distinct(ID,  comprimento) |> 
  filter(comprimento < 2000) |> 
  ggplot() +
  geom_histogram(aes(x = comprimento))

trechos <- tar_read(dado_trechos_bruto)


st_read("dados_tratados/20251029_camadas_202503.shp") |> 
  st_set_crs("EPSG:4674") |> 
  mapview::mapview()

st_read("dados_tratados/20251029_camadas_202507.shp") |> 
  st_set_crs("EPSG:4326") |> 
  mapview::mapview()

m <- tar_read(dado_trechos_bruto) |> 
  filter(!tipo_via %in% c("unclassified", "service")) |> 
  st_write("mariah.gpkg")
st_read("mariah.gpkg")

mariah <- bind_rows(
  bind_rows(
    st_read("dados_tratados/20251029_camadas_202503.shp", quiet = T),
    tibble(id_osm = "186371501")
  ) |> mutate(data = make_date(year = 2025, month = 03, day = 26)),
  st_read("dados_tratados/20251029_camadas_202507.shp", quiet = T) |> mutate(data = make_date(year = 2025, month = 07, day = 10))
) |> 
  st_drop_geometry() |> 
  select(id_osm, data) |> 
  
  bind_rows(
    tibble(id_osm = c("14408299", 
                      "936445516",
                      "153242233",
                      "273593639",
                      "565528400",
                      "901219749") |> as.character(), 
           data = make_date(year = 2024, month = 03, day = 01))
  ) |> 
  
  bind_rows(
    tibble(id_osm = c("901220871", 
                      "901221674",
                      "273593636",
                      "273593637"
    ) |> as.character(), 
    data = make_date(year = 2024, month = 09, day = 02))
  ) |> 
  
  bind_rows(
    tibble(id_osm = c("292219530", 
                      "230636450",
                      "230636453"
    ) |> as.character(), 
    data = make_date(year = 2023, month = 10, day = 01))
  ) |> 
  
  bind_rows(
    tibble(id_osm = c("1340256378",
                      "1340256383",
                      "237745082"
    ) |> as.character(), 
    data = make_date(year = 2024, month = 04, day = 01))
  ) |> 
  
  bind_rows(
    tibble(id_osm = c("1094556966"
    ) |> as.character(), 
    data = make_date(year = 2024, month = 05, day = 01))
  ) |> 
  
  (\(df) semi_join(trechos, df))()


faixa_azul <- tar_read(dado_faixa_azul)

tar_read(dado_faixa_azul) |> 
  group_by(id_osm) |> 
  filter(row_number() == 1) |> 
  write_csv("dados_brutos/faixa_azul.csv")

tar_read(dado_faixa_azul) |> 
  group_by(id_osm) |> 
  filter(row_number() == 1) |> 

  (\(df) right_join(trechos, df))() |> 
  mapview::mapview()

mariah


bind_rows(faixa_azul,
          mariah |> rename(data_implementacao = data)) |> 
  distinct() |> 
  arrange(data_implementacao, id_osm) |> 
  write_csv("dados_brutos/faixa_azul.csv")

list(mariah,
trechos |> 
  semi_join(tar_read(dado_faixa_azul)) |>
  left_join(tar_read(dado_faixa_azul))) |> 
  mapview::mapview()

trechos |> 
  filter(tipo_via %in% c("primary", "secondary", "trunk")) |> 
  # filter(str_detect(str_to_lower(logradouro), "anhangabau")) |> 
  mapview::mapview()




trechos |> 
  filter(id_osm == 14408299) |> 
  mapview::mapview()

trechos |> 
  filter(id_osm %in% c(
         "14408299", 
         "936445516","153242233",
         "273593639","565528400")
         ) |>
  # filter(logradouro |> str_detect("Parque Anhan")) |> 
  mapview::mapview()

273593636


Adicionar
186371501
14408299
936445516
153242233
273593639
565528400


Remover
941852309
942297822
941842697
336125988

tar_meta() |> View()

vitimas <- tar_read(dado_vitimas)
sinistros <- tar_read(dado_sinistros) |> 
  as_tibble() |> 
  filter(tipo != "NOTIFICACAO", logradouro != "NAO DISPONIVEL") |> 
  filter(!is.na(longitude), !is.na(latitude)) |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> 
  st_transform("epsg:31983")
  
sinistros_token <- tar_read(dado_token_infosiga) 
trechos  <- tar_read(dado_trechos_bruto)|> st_transform("epsg:31983")
trechos_token <- tar_read(dado_token_osm)

trechos |> 
  filter(str_detect(str_to_lower(logradouro), "jacu") | str_detect(str_to_lower(logradouro), "nova trabalhadores") |
         str_detect(str_to_lower(logradouro_alt1), "jacu") | str_detect(str_to_lower(logradouro_alt1), "nova trabalhadores") |
         str_detect(logradouro_ref, "SP-017")) |> 
  st_write("mariah-jacu.gpkg")

faixa_azul |> 
  mutate(data_implementacao = make_date(year = year(data_implementacao), month = month(data_implementacao))) |> 
  write_csv("dados_brutos/faixa_azul.csv")

frota <- read_csv("dados_brutos/legacy/frota_sp.csv") 
frota |> 
  mutate(moto = tipo_veiculo %in% c("motocicleta", "motoneta" , "ciclomotor")) |> 
  filter(mes == 12) |> 
  group_by(ano, moto) |> 
  summarize(n  =  sum(quantidade)) |> 
  mutate(percent = n / sum(n)) |> View()


sinistros <- sinistros |> 
  as_tibble() |> 
  filter(tipo != "NOTIFICACAO", logradouro != "NAO DISPONIVEL") |> 
  filter(!is.na(longitude), !is.na(latitude)) |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> 
  st_transform("epsg:31983")

trechos  <- trechos |> st_transform("epsg:31983")

# INDEXING: juntando todos os candidatos no raio de 300 metros
join <- sinistros |> 
  select(id_sinistro, geometry) |> 
  st_join(trechos |> select(id_osm, geometry), join = st_is_within_distance, dist = 300) |> 
  st_drop_geometry()

# Match pelo nome mais próximo
match_nome <- join |> 
  left_join(sinistros_token, by = join_by(id_sinistro)) |> 
  left_join(trechos_token, by = join_by(id_osm)) |> 
  mutate(similaridade = stringdist::stringsim(logradouro_limpo.x, logradouro_limpo.y),
         match_tipo = tipo.x == tipo.y,
         match_titulo = titulo.x == titulo.y) |> 
  drop_na() |> 
  group_by(id_sinistro) |> 
  filter(similaridade == max(similaridade)) |> 
  mutate(distancia_nome = stringdist::stringdist(logradouro_limpo.x, logradouro_limpo.y)) 

# Seleção do trecho mais próximo geograficamente quando há empate de proximidade de nome
match_grafico <- match_nome |> 
  select(ID_sinistro = id_sinistro, ID_osm = id_osm) |> 
  group_by(ID_sinistro) |> 
  reframe(id_osm = {
    
    ponto <- sinistros |> 
      filter(id_sinistro == first(ID_sinistro)) |> 
      select(ponto = geometry)
    
    linhas <- trechos |> 
      filter(id_osm %in% ID_osm) |> 
      select(id_osm, linha = geometry)
    
    linhas$id_osm[st_nearest_feature(ponto, linhas)]
  }
  ) |> 
  rename(id_sinistro = ID_sinistro) |> 
  left_join(trechos |> select(id_osm, geometria_trecho = geometry)) |> 
  left_join(sinistros |> select(id_sinistro, geometria_ponto = geometry)) |> 
  mutate(distancia = st_distance(geometria_ponto, geometria_trecho, by_element = T))


match <- match_grafico |> 
  select(id_sinistro, id_osm, distancia_geografica = distancia) |> 
  left_join(match_nome) |> 
  select(id_sinistro, id_osm, logradouro = logradouro_limpo.y, similaridade, distancia_geografica, distancia_nome, match_tipo, match_titulo) |> 
  
  # Verificação de haver apenas um match por sinistro
  group_by(id_sinistro) |> 
  filter(match_tipo + match_titulo == max(match_tipo + match_titulo), #Selecionar o que tem maior match de título e tipo
         match_titulo == max(match_titulo), #Se um tiver match e título, e o outro match no tipo, manter apenas match no título
         row_number() == 1) #Garantir que sobra apenas uma linha no matter what

match <- match |> 
  left_join(sinistros |> 
              mutate(numero_zero = as.numeric(numero) == 0) |> 
              select(id_sinistro, numero_zero))


return(match)

# Example data: project to meters (replace with your datasets and CRS)
dot_proj <- sinistros |> head(100)
geoms_proj <- trechos

# A: Direct distance filter (optimized)
time_A <- system.time({
  result_A <- geoms_proj %>%
    filter(lengths(st_is_within_distance(., dot_proj, dist = 300)) > 0)
})

# B: Buffer + intersect
time_B <- system.time({
  result_B <- sinistros %>%
    mutate(geometry = st_buffer(geometry, 300)) |>
    select(id_sinistro, geometry) |> 
    st_join(trechos)
})

# C: Spatial join
time_C <- system.time({
  result_C <- dot_proj %>%
    st_join(geoms_proj, join = st_is_within_distance, dist = 300)
})

# C: Spatial join
time_D <- system.time({
  result_D <- dot_proj %>%
    st_join(geoms_proj, join = st_nearest_feature)
})

print(time_A)
print(time_B)
print(time_C)
print(time_D)

join <- sinistros |> 
  head(10000) |> 
  select(id_sinistro, geometry) |> 
  st_join(trechos |> select(id_osm, geometry), join = st_is_within_distance, dist = 300) |> 
  st_drop_geometry()


match_nome <- join |> 
  left_join(sinistros_token, by = join_by(id_sinistro)) |> 
  left_join(trechos_token, by = join_by(id_osm)) |> 
  mutate(similaridade = stringdist::stringsim(logradouro_limpo.x, logradouro_limpo.y),
         match_tipo = tipo.x == tipo.y,
         match_titulo = titulo.x == titulo.y) |> 
  drop_na() |> 
  group_by(id_sinistro) |> 
  filter(similaridade == max(similaridade)) |> 
  mutate(distancia_nome = stringdist::stringdist(logradouro_limpo.x, logradouro_limpo.y)) 


time_E <- system.time({
  teste <- match_nome |> 
    select(ID_sinistro = id_sinistro, ID_osm = id_osm) |> 
    group_by(ID_sinistro) |> 
    reframe(id_osm = {
      
      ponto <- sinistros |> 
        filter(id_sinistro == first(ID_sinistro)) |> 
        select(ponto = geometry)
      
      linhas <- trechos |> 
        filter(id_osm %in% ID_osm) |> 
        select(id_osm, linha = geometry)

      linhas$id_osm[st_nearest_feature(ponto, linhas)]
    }
    ) |> 
    rename(id_sinistro = ID_sinistro) |> 
    left_join(trechos |> select(id_osm, geometria_trecho = geometry)) |> 
    left_join(sinistros |> select(id_sinistro, geometria_ponto = geometry)) |> 
    mutate(distancia = st_distance(geometria_ponto, geometria_trecho)[,1])
}) 



time_F <- system.time({
  # Seleção do trecho mais próximo geograficamente quando há empate de proximidade de nome
  teste2 <- match_nome |> 
    select(id_sinistro, id_osm) |> 
    left_join(sinistros |>  select(id_sinistro, geometria_ponto = geometry)) |> 
    left_join(trechos |> select(id_osm, geometria_trecho = geometry)) |> 
    filter(!st_is_empty(geometria_ponto), !st_is_empty(geometria_trecho)) |>
    
    # Encontrar o vizinho mais próximo e depois calcular a distância é significativamente mais rápido do que calcular todas as distâncias e depois pegar a menor, 
    # mas essa parte demora bastante para rodar mesmo (são 8 milhões de comparações)
    group_by(id_sinistro) |> 
    filter(row_number() == st_nearest_feature(nth(geometria_ponto, 1), geometria_trecho)) |> 
    mutate(distancia = st_distance(geometria_ponto, geometria_trecho, by_element = TRUE) |> as.numeric()) |> 
    st_drop_geometry() |> 
    ungroup()
  
})

print(time_E)
print(time_F)


match <- match_grafico |> 
  select(id_sinistro, id_osm, distancia_geografica = distancia) |> 
  left_join(match_nome) |> 
  select(id_sinistro, id_osm, logradouro = logradouro_limpo.y, similaridade, distancia_geografica, distancia_nome, match_tipo, match_titulo) |> 
  
  # Verificação de haver apenas um match por sinistro
  group_by(id_sinistro) |> 
  filter(match_tipo + match_titulo == max(match_tipo + match_titulo), #Selecionar o que tem maior match de título e tipo
         match_titulo == max(match_titulo), #Se um tiver match e título, e o outro match no tipo, manter apenas match no título
         row_number() == 1) #Garantir que sobra apenas uma linha no matter what

match <- match |> 
  left_join(sinistros |> 
              mutate(numero_zero = as.numeric(numero) == 0) |> 
              select(id_sinistro, numero_zero))


return(match)


fit <- tar_read(did_fit, branches = 1)$did_fit_5e506ed9825313fa

fit |> 
  aggte(type = "calendar", bstrap = T, biters = 100000) |> 
  ggdid()




