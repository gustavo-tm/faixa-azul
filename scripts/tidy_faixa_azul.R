library(tidyverse)
library(sf)


tidy_faixa_azul <- function(trechos){

  faixa_azul <- read_csv("dados_tratados/faixa_azul.csv", col_types = list(id_osm = "c"))
  
  return(faixa_azul)
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
