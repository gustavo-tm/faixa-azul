library(tidyverse)
library(stringdist)
library(fuzzyjoin)
library(stringr)
library(stringi)
library(sf)


# 3.5 DATA PRE-PROCESSING ----
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

# logradouros <- read_csv("banco_dados/logradouros.csv")
logradouros.infosiga <- read_csv("banco_dados/sinistros.csv") |> filter(tipo != "NOTIFICACAO") |> count(logradouro)

preposicao <- read_csv("dados_tratados/logradouros/preposicao.csv") |> 
  mutate(across(everything(), ~ limpar(.x)))
numero <- read_csv("dados_tratados/logradouros/numero.csv") |> 
  mutate(across(everything(), ~ limpar(.x)))
tipo <- read_csv("dados_tratados/logradouros/tipo.csv") |> 
  mutate(across(everything(), ~ limpar(.x)))
titulo <- read_csv("dados_tratados/logradouros/titulo.csv") |> 
  mutate(across(everything(), ~ limpar(.x)))



logradouros.infosiga |> 
  filter(str_detect(str_to_upper(logradouro), " MAIO | FARIA ")) |> 
  mutate(padronizar(logradouro)) |> 
  View()



logradouros <- read_csv("banco_dados/logradouros.csv")
logradouros.osm <- st_read("banco_dados/trechos.gpkg") |> st_drop_geometry() |> filter(tipo_via != "service") |> as_tibble()

#171381 = 100%
logradouros.infosiga |> summarize(n = sum(n)) 

# DEDUPLICATION: 20,192 -> 19,380 ()
logradouros.infosiga |> 
  mutate(logradouro = padronizar(logradouro)) |> 
  group_by(logradouro) |> 
  summarize(n = sum(n))


#136281 = 79,5%
logradouros.infosiga |> 
  mutate(logradouro = limpar(logradouro)) |> 
  semi_join(logradouros.osm |> 
              select(logradouro) |> 
              mutate(logradouro = limpar(logradouro))) |> 
  summarize(n = sum(n)) 

#138905 = 81,0%
logradouros.infosiga |> 
  mutate(logradouro = padronizar(logradouro)) |> 
  semi_join(logradouros.osm |> 
              select(logradouro) |> 
              mutate(logradouro = padronizar(logradouro))) |> 
  summarize(n = sum(n)) 

#140579 = 82,0%
logradouros.infosiga |>
  mutate(logradouro = logradouro |> limpar()) |>
  semi_join(logradouros.osm |>
              select(id_osm, starts_with("logradouro")) |>
              separate_wider_delim(logradouro_ref, delim = ";", too_few = "align_start", names_sep = "_") |>
              pivot_longer(starts_with("logradouro")) |>
              drop_na() |>
              mutate(logradouro = value |> limpar())) |> 
  summarize(n = sum(n))

#143814 = 83,9%
logradouros.osm.clean <- logradouros.osm |>
  select(id_osm, starts_with("logradouro")) |>
  separate_wider_delim(logradouro_ref, delim = ";", too_few = "align_start", names_sep = "_") |>
  pivot_longer(starts_with("logradouro")) |>
  drop_na() |>
  mutate(logradouro = value |> padronizar()) |> 
  select(logradouro) |> distinct() |> drop_na()

logradouros.infosiga |>
  mutate(logradouro = logradouro |> padronizar()) |>
  semi_join(logradouros.osm.clean) |> 
  summarize(n = sum(n))


# FUZZY JOIN
logradouros.potencial <- logradouros.infosiga |>
  mutate(logradouro = logradouro |> limpar() |> padronizar()) |>
  anti_join(logradouros.osm.clean) |> 
  group_by(logradouro) |> summarize(n = sum(n)) |> 
  arrange(-n)



join <- logradouros.potencial |> 
  stringdist_left_join(logradouros.osm.clean, method = "osa", distance_col = "distancia", max_dist = 5)

#143814 + 7430 = 151244 = 88,2%
join |> 
  filter(distancia <= 1) |> 
  group_by(logradouro.x) |>
  filter(distancia == min(distancia)) |>
  ungroup() |> 
  summarize(n = sum(n))

#143814 + 8963 = 152777 = 89,1%
join |> 
  filter(distancia <= 2) |> 
  group_by(logradouro.x) |>
  filter(distancia == min(distancia)) |>
  ungroup() |>
  summarize(n = sum(n))


#152777 + 10243 = 95,1%
join.pelado <- logradouros.potencial |> 
  anti_join(join |> 
              filter(distancia <= 2),
            by = join_by(logradouro == logradouro.x)) |> 
  mutate(logradouro = str_replace_all(logradouro, setNames(rep(" ", length(tipo$tipo)), tipo$tipo))) |> 
  stringdist_left_join(y= logradouros.osm.clean |> 
                         mutate(logradouro = str_replace_all(logradouro, setNames(rep(" ", length(tipo$tipo)), tipo$tipo))) |> 
                         distinct(),
                       method = "osa", distance_col = "distancia", max_dist = 2)

join.pelado |> 
  filter(distancia <= 1) |> 
  group_by(logradouro.x) |>
  filter(distancia == min(distancia)) |>
  ungroup() |> summarize(n = sum(n))
