library(tidyverse)
library(sf)

sf_use_s2(FALSE)

distrito <- st_read("dados_tratados/distrito/SIRGAS_SHP_distrito.shp", quiet = TRUE) |> 
  st_set_crs("epsg:31983") |> 
  st_buffer(100) |> 
  st_transform("epsg:4326") |> 
  st_union()

trechos <- tar_read(dado_trechos_bruto) |> 
  filter(!tipo_via %in% c("unclassified",  "service")) |> 
  st_intersection(distrito)

logradouros <- tar_read(dado_logradouros)
logradouros_id <- tar_read(dado_id_logradouros)
faixa_azul <- tar_read(dado_faixa_azul)

save.image("app/dados.RData")
