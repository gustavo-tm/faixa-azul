library(tidyverse)

faixa_azul.vias <- readxl::read_excel("dados_tratados/faixa_azul_vias.xlsx")
faixa_azul.selecao <- read_csv("dados_tratados/faixa_azul_selecao.csv", col_types = list(id_osm = "c"))
trechos <- st_read("banco_dados/trechos.gpkg")


faixa_azul.selecao |> 
  left_join(trechos) |> 
  mutate(logradouro = logradouro |> 
           stringi::stri_trans_general("latin-ascii") |> 
           str_to_upper() |> 
           str_replace_all("[[:punct:]]", "")) |> 
  select(id_osm, logradouro) |> 
  left_join(faixa_azul.vias |> 
              mutate(data_implementacao = make_date(year = ano, month = mes)) |> 
              select(logradouro = logradouro_osm, data_implementacao) |> 
              distinct() |>
              group_by(logradouro) |> 
              filter(n() == 1)) |> 
  select(-logradouro) |> 
  write_csv("banco_dados/faixa_azul.csv")

