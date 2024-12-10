library(tidyverse)
library(stringdist)
library(fuzzyjoin)
# library(stringr)
# library(stringi)
library(sf)


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
tokenizar <- function(df){
  df |> 
    mutate(logradouro_limpo = logradouro |> 
             str_replace_all(string = _, setNames(rep(" ", length(titulo$titulo_ext)), titulo$titulo_ext)) |> 
             str_replace_all(string = _, setNames(rep(" ", length(tipo$tipo)), tipo$tipo)) |> 
             limpar()) |> 
    regex_left_join(tipo |> select(tipo), by = join_by(logradouro == tipo)) |> 
    regex_left_join(titulo |> select(titulo = titulo_ext), by = join_by(logradouro == titulo))
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
  filter(tipo != "NOTIFICACAO", logradouro != "NAO DISPONIVEL") |> 
  select(id_sinistro, logradouro) |> 
  mutate(logradouro = padronizar(logradouro))

infosiga.token <- infosiga |> tokenizar()

osm <- st_read("banco_dados/trechos.gpkg") |> 
  st_drop_geometry() |> 
  filter(tipo_via != "service") |> 
  select(id_osm, starts_with("logradouro")) |>
  separate_wider_delim(logradouro_ref, delim = ";", too_few = "align_start", names_sep = "_") |>
  pivot_longer(starts_with("logradouro")) |>
  drop_na() |> 
  select(id_osm, logradouro = value) |> 
  mutate(logradouro = padronizar(logradouro))

osm.token <- osm |> tokenizar()


join <- infosiga.token |> 
  stringdist_left_join(osm.token, 
                       method = "osa", distance_col = "distancia", max_dist = 2, by = join_by(logradouro_limpo))


join.reduce <- join |> 
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
  
  # Se não houver match perfeito, selecionar o match com menor distancia
  group_by(id_sinistro) |> 
  filter(distancia == min(distancia)) |> 
  ungroup() |> 

  # Se houver impate de distância, desempatar usando match tipo e título
  group_by(id_sinistro) |> 
  filter((match_tipo + match_titulo) == max(match_tipo + match_titulo)) |> 
  ungroup() |> 
  
  select(id_sinistro, id_osm, distancia_nome = distancia)


# Formato para reduzir o tamanho do arquivo
join.reduce |> 
  group_by(id_sinistro) |> 
  summarize(id_osm = paste(id_osm, collapse = ";"),
            distancia_nome = nth(distancia_nome, 1)) |> 
  write_csv("banco_dados/match_nome.csv")
















