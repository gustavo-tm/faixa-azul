library(tidyverse)
library(sf)

logradouros <- st_read("dados_brutos/logradouros/SIRGAS_SHP_logradouronbl_line.shp") |> 
  st_drop_geometry() 

logradouros |> 
  distinct(lg_titulo) |> 
  write_csv("titulos.csv")


logradouros |>
  left_join(readxl::read_excel("dados_brutos/logradouros/Dicionário-de-Dados-Logradouro.xlsx", sheet = 2) |> 
              select(lg_tipo = "Sigla", tipo = "Tipo de logradouro")) |> 
  mutate(across(c(tipo, lg_titulo, lg_prep, lg_nome, lg_tipo), ~ replace_na(.x, "")),
         logradouro = paste(tipo, lg_titulo, lg_prep, lg_nome) |> 
           stringi::stri_trans_general("latin-ascii") |> 
           str_to_upper() |> 
           str_replace_all("[[:punct:]]", "") |> 
           str_squish()) |> 
  select(id_logradouro = lg_codlog, logradouro, tipo_abrev = lg_tipo, tipo, titulo = lg_titulo, preposicao = lg_prep, nome = lg_nome) |> 
  distinct() |> 
  write_csv("banco_dados/logradouros.csv")
