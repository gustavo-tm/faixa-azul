library(tidyverse)
library(sf)

logradouros <- st_read("dados_brutos/logradouros/SIRGAS_SHP_logradouronbl_line.shp") |> 
  st_drop_geometry() 

logradouros <- logradouros |> 
  distinct(lg_titulo) |> 
  drop_na() |> 
  mutate(titulo_ext = readLines("dados_tratados/titulo_extenso.txt") |> 
           str_split_1(", ")) |> 
  right_join(logradouros)



logradouros |>
  left_join(readxl::read_excel("dados_brutos/logradouros/Dicionário-de-Dados-Logradouro.xlsx", sheet = 2) |> 
              select(lg_tipo = "Sigla", tipo = "Tipo de logradouro")) |> 
  mutate(across(c(tipo, lg_titulo, titulo_ext, titulo_ext, lg_prep, lg_nome, lg_tipo), ~ replace_na(.x, "")),
         logradouro = paste(tipo, titulo_ext, lg_prep, lg_nome),
         across(c(tipo, lg_titulo, titulo_ext, lg_prep, lg_nome, lg_tipo, logradouro), ~ .x |>
                  stringi::stri_trans_general("latin-ascii") |> 
                  str_to_upper() |> 
                  str_replace_all("[[:punct:]]", "") |> 
                  str_squish())) |> 
  select(id_logradouro = lg_codlog, logradouro, tipo_abrev = lg_tipo, tipo, titulo_abrev = lg_titulo, titulo = titulo_ext, preposicao = lg_prep, nome = lg_nome) |> 
  distinct() |> 
  write_csv("banco_dados/logradouros.csv")
