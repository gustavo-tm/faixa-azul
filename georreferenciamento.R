library(tidyverse)
library(sf)
library(mapview)

logradouros <- st_read("dados_tratados/logradouros_osm.gpkg") |> 
  mutate(logradouro = logradouro |> 
           stringi::stri_trans_general("latin-ascii") |> 
           str_to_upper() |> 
           str_replace_all("[[:punct:]]", ""))


faixa_azul <- readxl::read_excel("dados_tratados/vias_faixa_azul.xlsx") |> 
  select(logradouro = logradouro_osm)

logradouros |> 
  semi_join(faixa_azul) |> 
  mapview() |> 
  mapshot(url = "output/logradouros_faixa_azul.html")

