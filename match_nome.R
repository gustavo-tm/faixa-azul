library(tidyverse)
library(stringdist)
library(fuzzyjoin)
library(stringr)
library(stringi)
library(sf)

# logradouros <- read_csv("banco_dados/logradouros.csv")
logradouros.infosiga <- read_csv("banco_dados/sinistros.csv") |> count(logradouro)

preposicao <- read_csv("dados_tratados/logradouros/preposicao.csv") |> 
  mutate(across(everything(), ~ preparacao.match(.x)))
numero <- read_csv("dados_tratados/logradouros/numero.csv") |> 
  mutate(across(everything(), ~ preparacao.match(.x)))
tipo <- read_csv("dados_tratados/logradouros/tipo.csv") |> 
  mutate(across(everything(), ~ preparacao.match(.x)))
titulo <- read_csv("dados_tratados/logradouros/titulo.csv") |> 
  mutate(across(everything(), ~ preparacao.match(.x)))





# 3.5 DATA PRE-PROCESSING ----

standardize <- function(coluna){
  coluna |> 
    # Removing Unwanted Characters and Tokens
    replace_na(" ") |> 
    stringi::stri_trans_general("latin-ascii") |> 
    str_to_upper() |> 
    str_replace_all("[[:punct:]]", " ") |> 
    str_squish() |> 
    
    # Standardisation
    (\(col) str_glue(" {col} "))(col = _) |> 
    as.character() |> 
    str_replace_all("(BR|SP)\\s(\\d+)", "\\1\\2") |> #correção estradas SP 015 -> SP015
    str_replace_all(setNames(tipo$tipo, tipo$tipo_abrev)) |> #correcao tipo via (av -> avenida)
    str_replace_all(setNames(numero$numero_ext, numero$numero)) |> #correcao numero (23 -> vinte e tres)
    str_replace_all(setNames(titulo$titulo_ext, titulo$titulo)) #correcao titulo (BRIG -> BRIGADEIRO)
}

logradouros.infosiga |> 
  filter(str_detect(str_to_upper(logradouro), " MAIO | FARIA ")) |> 
  mutate(standardize(logradouro)) |> 
  View()



logradouros <- read_csv("banco_dados/logradouros.csv")
logradouros.infosiga <- read_csv("banco_dados/sinistros.csv") |> count(logradouro) |> limpar.logradouro()
logradouros.osm <- st_read("banco_dados/trechos.gpkg") |> st_drop_geometry() |> filter(tipo_via != "service") |> as_tibble()

logradouros.infosiga |> summarize(n = sum(n))
logradouros.infosiga |> semi_join(logradouros.osm |> select(logradouro)) |> summarize(n = sum(n))
logradouros.infosiga |> semi_join(logradouros) |> summarize(n = sum(n))


logradouros.infosiga |> 
  mutate(logradouro = logradouro |> estender.abreviacoes() |> preparacao.match()) |> 
  semi_join(logradouros.osm |> 
              mutate(logradouro = logradouro |> estender.abreviacoes() |> preparacao.match())) |> 
  summarize(n = sum(n))




logradouros.infosiga |> 
  mutate(logradouro = logradouro |> standardize()) |> 
  anti_join(logradouros.osm |> 
              select(id_osm, starts_with("logradouro")) |> 
              separate_wider_delim(logradouro_ref, delim = ";", too_few = "align_start", names_sep = "_") |>   
              pivot_longer(starts_with("logradouro")) |> 
              drop_na() |>
              mutate(logradouro = value |> standardize())) |> View()
  summarize(n = sum(n))



logradouros.osm |> 
  select(id_osm, starts_with("logradouro")) |> 
  separate_wider_delim(logradouro_ref, delim = ";", too_few = "align_start", names_sep = "_") |>   
  pivot_longer(starts_with("logradouro")) |> 
  drop_na() |>
  mutate(logradouro = value |> standardize()) |> View()



