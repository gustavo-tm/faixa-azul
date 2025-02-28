library(tidyverse)
library(sf)


tidy_faixa_azul <- function(trechos){
  faixa_azul.vias <- readxl::read_excel("dados_tratados/faixa_azul_vias.xlsx")
  faixa_azul.selecao <- read_csv("dados_tratados/faixa_azul_selecao.csv", col_types = list(id_osm = "c"))

  
  faixa_azul <- faixa_azul.selecao |> 
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
    mutate(data_implementacao = case_when(logradouro == "AVENIDA SANTOS DUMONT" & id_osm == "4331480" ~ make_date(year = 2023, month = 10),
                                          logradouro == "AVENIDA SANTOS DUMONT" & id_osm != "4331480" ~ make_date(year = 2024, month = 4),
                                          .default = data_implementacao)) |> 
    select(-logradouro) 
  
  return(faixa_azul)
  # faixa_azul |> 
  #   write_csv("banco_dados/faixa_azul.csv")
}




# Plot mapa -----
# 
# distrito <- st_read("dados_tratados/distrito/SIRGAS_SHP_distrito.shp") |> 
#   st_set_crs("epsg:31983") |> 
#   summarize(geometry = st_union(geometry) |> st_simplify(dTolerance = 100))
# 
# trechos.mapa <- trechos |> 
#   filter(!tipo_via %in% c("service", "unclassified")) |> 
#   st_transform("epsg:31983") |> 
#   st_intersection(distrito)
#   
#   
# gg <- ggplot() +
#   geom_sf(data = distrito,
#           aes(geometry = geometry), colour = "grey25", fill = "grey98") +
#   geom_sf(data = trechos.mapa |> 
#             filter(!tipo_via %in% c("trunk", "primary", "secondary")),
#           aes(geometry = st_simplify(geom, dTolerance = 10)), colour = "grey50", lwd = .15, alpha = .8) +
#   geom_sf(data = trechos.mapa |> 
#             filter(tipo_via %in% c("trunk", "primary", "secondary")),
#           aes(geometry = st_simplify(geom, dTolerance = 10)), colour = "#3c3744", lwd = .3, alpha = .8) +
#   geom_sf(data = trechos.mapa |> 
#             semi_join(faixa_azul),
#           aes(geometry = geom), colour = "white", lwd = 1.4, alpha = .9) +
#   geom_sf(data = trechos.mapa |> 
#             semi_join(faixa_azul),
#           aes(geometry = geom), colour = "#090c9b", lwd = 1) +
#   theme_void()
# 
# ggsave("output/mapa-faixa-azul.pdf", gg, width = 30, height = 40)
