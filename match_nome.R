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



# 3.5.1 Removing Unwanted Characters and Tokens
preparar.match <- function(coluna){
  coluna |> 
    replace_na(" ") |> 
    stringi::stri_trans_general("latin-ascii") |> 
    str_to_upper() |> 
    str_replace_all("[[:punct:]]", " ") |> 
    str_squish() |> 
    (\(col) str_glue(" {col} "))(col = _) |> 
    as.character()
}

# 3.5.2 Standardisation and Tokenisation ----

estender.abreviacoes <- function(coluna){
  coluna |> 
    preparar.match() |> 
    str_replace_all("(BR|SP)\\s(\\d+)", "\\1\\2") |> #correção estradas
    str_replace_all(setNames(tipo$tipo, tipo$tipo_abrev)) |> 
    str_replace_all(setNames(numero$numero_ext, numero$numero)) |> 
    str_replace_all(setNames(titulo$titulo_ext, titulo$titulo))
}

logradouros.infosiga |> 
  mutate(logradouro = preparacao.match(logradouro)) |> 
  filter(str_detect(logradouro, " MAIO | FARIA ")) |> 
  mutate(estender.abreviacoes(logradouro)) |> 
  View()










# 
# limpar.logradouro <- function(df){
#   df |> 
#     mutate(logradouro = logradouro |> 
#     stringi::stri_trans_general("latin-ascii") |> 
#     str_to_upper() |> 
#     str_replace_all("[[:punct:]]", "") |> 
#     str_squish())
# }
# 
# 
# 
# logradouros <- read_csv("banco_dados/logradouros.csv") 
# logradouros.infosiga <- read_csv("banco_dados/sinistros.csv") |> count(logradouro) |> limpar.logradouro()
# logradouros.osm <- st_read("banco_dados/trechos.gpkg") |> st_drop_geometry() |> filter(tipo_via != "service") |> as_tibble() |> limpar.logradouro() |> distinct(logradouro) 
# 
# logradouros.infosiga |> summarize(n = sum(n))
# logradouros.infosiga |> semi_join(logradouros.osm) |> summarize(n = sum(n))
# logradouros.infosiga |> semi_join(logradouros) |> summarize(n = sum(n))
# 
# remover.componente <- function(df, componente = "tipo"){
#   
#   pattern <- logradouros |> 
#     select(logradouro = all_of(componente)) |> 
#     limpar.logradouro() |> 
#     distinct(logradouro) |> 
#     drop_na() |>
#     pull(logradouro) |> 
#     paste(collapse = " | ") |> 
#     paste(" ", collapse = "", sep = "")
#   
#   df |> 
#     mutate(logradouro = paste(" ", logradouro, sep = "") |> 
#              str_replace_all(pattern, " "))
# }
# 
# 
# logradouros.infosiga |> 
#   remover.componente("preposicao") |> 
#   remover.componente("titulo") |> 
#   remover.componente("titulo_abrev") |> 
#   remover.componente("tipo") |> 
#   remover.componente("tipo_abrev") |> 
#   limpar.logradouro() |> 
#   semi_join(logradouros.osm |> 
#               remover.componente("preposicao") |> 
#               remover.componente("titulo") |> 
#               remover.componente("titulo_abrev") |> 
#               remover.componente("tipo") |> 
#               remover.componente("tipo_abrev") |> 
#               limpar.logradouro()) |> summarize(n = sum(n))
# 
#   
# 
# 
# 
# 
# 
# 
# 
# 
# logradouros.mod <- logradouros |> 
#   filter(!is.na(nome)) |> 
#   mutate(across(c(tipo, tipo_abrev, titulo, preposicao, nome), ~ replace_na(.x, ""))) |> 
#   mutate(logradouro1 = paste(tipo_abrev, nome),
#          logradouro2 = paste(tipo_abrev, titulo, nome),
#          logradouro3 = paste(tipo_abrev, preposicao, nome),
#          logradouro4 = paste(tipo, nome),
#          logradouro5 = paste(tipo, titulo, nome),
#          logradouro6 = paste(tipo, preposicao, nome),
#          logradouro7 = paste(titulo, preposicao, nome),
#          logradouro8 = paste(titulo, nome),
#          logradouro9 = nome) |> 
#   select(starts_with("logradouro")) |> 
#   pivot_longer(everything()) |> 
#   select(logradouro = value) |> 
#   limpar.logradouro() |> 
#   distinct()
# 
# logradouros.infosiga |> anti_join(logradouros.mod) |> 
#   sample_n(100)
# 
# 
# 
# 
# 
# trechos <- st_read("banco_dados/trechos.gpkg") |> st_drop_geometry() |> filter(tipo_via != "service") |> as_tibble()
# 
# logradouros.infosiga |> semi_join(trechos |> 
#                                     separate_wider_delim(logradouro_ref, delim = ";", too_few = "align_start", names_sep = "_") |> 
#                                     pivot_longer(starts_with("logradouro")) |> 
#                                     select(id_osm, logradouro = value) |> 
#                                     limpar.logradouro() |> 
#                                     drop_na() |> 
#                                     distinct()) |> summarize(n = sum(n))
# 
# 
# 
# trechos |> 
#   separate_wider_delim(logradouro_ref, delim = ";", too_few = "align_start", names_sep = "_") |> 
#   pivot_longer(starts_with("logradouro")) |> 
#   select(id_osm, logradouro = value) |> 
#   limpar.logradouro() |> 
#   drop_na() |> 
#   distinct()












