library(tidyverse)
library(sf)
library(mapview)

logradouros <- st_read("dados_tratados/logradouros_osm.gpkg") |> 
  mutate(logradouro = logradouro |> 
           stringi::stri_trans_general("latin-ascii") |> 
           str_to_upper() |> 
           str_replace_all("[[:punct:]]", ""))


faixa_azul <- readxl::read_excel("dados_tratados/vias_faixa_azul.xlsx") |> 
  select(logradouro = logradouro_osm, ano, mes) |> 
  mutate(data = make_date(year = ano, month = mes)) |> 
  select(-ano, -mes)

logradouros |> 
  right_join(faixa_azul) |>
  mutate(data = as.factor(data)) |> 
  mapview(zcol = "data") |> 
  mapshot(url = "output/logradouros_faixa_azul.html")

