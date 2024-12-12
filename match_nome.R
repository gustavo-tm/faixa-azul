library(tidyverse)
library(stringdist)
library(fuzzyjoin)
# library(stringr)
# library(stringi)
library(sf)

# PREPARAÇÃO DADOS ----

limpar <- function(coluna){
  coluna |> 
    # Removing Unwanted Characters and Tokens
    replace_na(" ") |> 
    stringi::stri_trans_general("latin-ascii") |> 
    str_to_upper() |> 
    str_replace_all("[[:punct:]]", " ") |> 
    str_squish() |> 
    (\(col) str_glue(" {col} "))(col = _) |> 
    as.character()
} 
padronizar <- function(coluna){
  coluna |> 
    limpar() |> 
    # Standardisation
    str_replace_all(setNames(rep(" ", length(preposicao$preposicao)), limpar(preposicao$preposicao))) |> #remover preposicao
    str_replace_all("(BR|SP)\\s(\\d+)", "\\1\\2") |> #correção estradas SP 015 -> SP015
    str_replace_all(setNames(tipo$tipo, tipo$tipo_abrev)) |> #correcao tipo via (av -> avenida)
    str_replace_all(setNames(numero$numero_ext, numero$numero)) |> #correcao numero (23 -> vinte e tres)
    str_replace_all(setNames(titulo$titulo_ext, titulo$titulo)) |> #correcao titulo (BRIG -> BRIGADEIRO)
    limpar()
}
tokenizar <- function(df, id){
  id <- sym(id)

  df |> 
    #Encontrar tokens e remover da string do logradouro
    mutate(logradouro_limpo = logradouro |> 
             str_replace_all(string = _, setNames(rep(" ", length(titulo$titulo_ext)), titulo$titulo_ext)) |> 
             str_replace_all(string = _, setNames(rep(" ", length(tipo$tipo)), tipo$tipo)) |> 
             limpar()) |> 
    regex_left_join(tipo |> select(tipo), by = join_by(logradouro == tipo)) |> 
    regex_left_join(titulo |> select(titulo = titulo_ext), by = join_by(logradouro == titulo)) |> 
    mutate(across(c(logradouro_limpo, tipo, titulo), ~ replace_na(.x, ""))) |> 
    
    #Corrigir quando aparecem 2 tokens no mesmo campo
    group_by(!!id, logradouro) |> 
    summarize(logradouro_limpo = nth(logradouro_limpo, 1),
              tipo = tipo |> str_split(" ") |> unlist() |> unique() |> paste(collapse = " ") |> limpar(),
              titulo = titulo |> str_split(" ") |> unlist() |> unique() |> paste(collapse = " ") |> limpar())
}

preposicao <- read_csv("dados_tratados/logradouros/preposicao.csv") |> 
  mutate(across(everything(), ~ limpar(.x)))
numero <- read_csv("dados_tratados/logradouros/numero.csv") |> 
  mutate(across(everything(), ~ limpar(.x)))
tipo <- read_csv("dados_tratados/logradouros/tipo.csv") |> 
  mutate(across(everything(), ~ limpar(.x)))
titulo <- read_csv("dados_tratados/logradouros/titulo.csv") |> 
  mutate(across(everything(), ~ limpar(.x)))


infosiga <- read_csv("banco_dados/sinistros.csv") |> 
  filter(tipo != "NOTIFICACAO", logradouro != "NAO DISPONIVEL")

infosiga.token <- infosiga |> 
  select(id_sinistro, logradouro) |> 
  mutate(logradouro = padronizar(logradouro)) |> 
  tokenizar(id = "id_sinistro")

osm <- st_read("banco_dados/trechos.gpkg")

osm.token <- osm |> 
  st_drop_geometry() |> 
  select(id_osm, starts_with("logradouro")) |>
  mutate(across(starts_with("logradouro"), ~ str_replace_all(.x, ",", ";"))) |> 
  separate_wider_delim(logradouro_ref, delim = ";", too_few = "align_start", names_sep = "_") |>
  separate_wider_delim(logradouro_alt1, delim = ";", too_few = "align_start", names_sep = "_") |>
  pivot_longer(starts_with("logradouro")) |>
  drop_na() |> 
  select(id_osm, logradouro = value) |> 
  mutate(logradouro = padronizar(logradouro)) |> 
  tokenizar(id = "id_osm")

# MATCH NOME->DISTANCIA ----

join <- infosiga.token |> 
  stringdist_left_join(osm.token, 
                       method = "osa", distance_col = "distancia", max_dist = 2, by = join_by(logradouro_limpo)) |> 
  mutate(across(c(tipo.x, tipo.y, titulo.x, titulo.y), ~ replace_na(.x, "")),
         match_tipo = tipo.x == tipo.y,
         match_titulo = titulo.x == titulo.y) |> 
  select(id_sinistro, id_osm, distancia, match_tipo, match_titulo) |> 
  distinct() |>
  
  # Se o sinistro apresentar um match perfeito, remover todos os outros
  mutate(gold_match = distancia == 0 & match_tipo == TRUE & match_titulo == TRUE) |> 
  group_by(id_sinistro) |> 
  filter(gold_match == max(gold_match)) |> 
  ungroup() |> 
  
  # Se não houver match perfeito, selecionar menor distância
  group_by(id_sinistro) |> 
  filter(distancia == min(distancia)) |> 
  ungroup() |> 

  # Se houver impate de distância, desempatar usando match tipo e título
  group_by(id_sinistro) |>
  filter((match_tipo + match_titulo) == max(match_tipo + match_titulo)) |>
  ungroup() |>

  select(id_sinistro, id_osm, distancia_nome = distancia)

trechos <- st_read("banco_dados/trechos.gpkg") |> 
  st_transform("epsg:31983") |> 
  select(id_osm, geometria_trecho = geom)

sinistros <- read_csv("banco_dados/sinistros.csv") |> 
  filter(!is.na(longitude), !is.na(latitude)) |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> 
  st_transform("epsg:31983") |> 
  select(id_sinistro, geometria_ponto = geometry)

distancias <- match_nome |> 
  left_join(sinistros) |> 
  left_join(trechos) |>
  mutate(distancia_geografica = st_distance(geometria_trecho, geometria_ponto, by_element = TRUE))

match_geografico <- distancias |> 
  select(-geometria_ponto, -geometria_trecho) |> 
  arrange(id_sinistro, distancia_geografica) |> 
  group_by(id_sinistro) |> 
  top_n(-1) |> 
  ungroup()

match_geografico |> 
  write_csv("dados_tratados/match.csv")

match_geografico |> 
  mutate(quantil = ntile(distancia_geografica, 100)) |>
  group_by(quantil) |> 
  summarize(distancia_geografica = median(as.numeric(distancia_geografica)),
            distancia_nome = mean(distancia_nome)) |> 
  ggplot(aes(x = quantil, y = as.numeric(distancia_geografica))) +
  geom_line(aes(colour = distancia_nome), lwd = 3) +
  scale_y_continuous(transform = "log10", labels = scales::number, breaks = 10^(0:6)) +
  scale_colour_gradient(low = "darkred", high = "grey95", limits = c(0.95, 1)) +
  theme_minimal()


match_geografico |> 
  ggplot(aes(y = as.numeric(distancia_geografica))) + 
  geom_histogram(bins = 50) +
  scale_y_continuous(transform = "log10", labels = scales::number, breaks = 10^(0:6)) +
  facet_wrap(~distancia_nome) +
  theme_minimal()

match_geografico |> 
  mutate(quantil = ntile(distancia_geografica, 100)) |> 
  ggplot(aes(y = as.numeric(distancia_geografica))) + 
  geom_histogram(aes(after_stat(density)), bins = 50) +
  scale_y_continuous(transform = "log10", labels = scales::number, breaks = 10^(0:6)) +
  facet_wrap(~distancia_nome) +
  theme_minimal()


# MATCH DISTANCIA->NOME ----


sinistros <- read_csv("banco_dados/sinistros.csv")
trechos <- st_read("banco_dados/trechos.gpkg") |> st_transform("epsg:31983") |> select(id_osm, starts_with("logradouro"), geometry = geom)

sinistros  <- read_csv("banco_dados/sinistros.csv") |> 
  filter(!is.na(longitude), !is.na(latitude)) |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> 
  st_transform("epsg:31983") |> 
  mutate(geometry = st_buffer(geometry, 200))

match_geografico <- sinistros |>
  select(id_sinistro, geometry) |> 
  st_join(trechos |> select(id_osm, geometry))

match_nome <- match_geografico |> 
  st_drop_geometry() |> 
  left_join(sinistros.token) |> 
  left_join(trechos.token)






# MATCH COMPARAÇÃO ----


