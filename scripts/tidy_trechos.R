library(tidyverse)
library(osmdata)
library(sf)
# library(mapview)

# https://cran.r-project.org/web/packages/osmdata/vignettes/osmdata.html
# https://rspatialdata.github.io/osm.html

# https://wiki.openstreetmap.org/wiki/Map_features
# available_features()

download_osm <- function(){
  osm <- getbb('São Paulo') |> 
    opq(bbox = _) |> 
    add_osm_feature(key = 'highway', value = c(
      "motorway", "trunk", "primary", "secondary", "tertiary", "unclassified", "residential", "service",
      "motorway_link", "trunk_link", "primary_link", "secondary_link", "motorway_junction",
      "speed_camera"))  |> 
    osmdata_sf()
  
  # saveRDS(osm, "dados_brutos/osm.rds")
  # 
  # gg <- osm$osm_lines |> 
  #   ggplot() +
  #   geom_sf(lwd = .1) +
  #   theme_void()
  # 
  # ggsave("output/vias_osm.pdf", gg, width = 30, height = 40)
  
  osm <- as_tibble(osm$osm_lines) |> 
    select(id_osm = osm_id,
           logradouro = name,
           logradouro_alt1 = alt_name,
           logradouro_alt2 = alt_name1,
           logradouro_alt3 = alt_name_1,
           logradouro_ref = ref,
           tipo_via = highway,
           faixas = lanes,
           limite_velocidade = maxspeed,
           limite_velocidade_pesados = "maxspeed:hgv",
           motocicleta = motorcycle,
           mao_unica = oneway,
           superficie = surface,
           elevado = bridge,
           geometry) |> 
    mutate(comprimento = st_length(geometry) |> as.numeric())
  
  return(osm)
}




tidy_trechos <- function(osm){
  
  # Interpolação 
  logradouro <- osm |> 
    filter(!logradouro |> is.na()) |> 
    select(-geometry) |> 
    group_by(logradouro) |> 
    
    #Pegar o valor que mais se repete naquele logradouro
    summarize(
      across(
        c(everything(), - comprimento),
        ~ fct_infreq(.x) |> levels() |> first())) |> 
    pivot_longer(c(everything(), - logradouro))
  
  trechos <- osm |> 
    # Selecionar apenas as linhas e colunas que devem ser interpoladas
    filter(!logradouro |> is.na()) |> 
    select(-comprimento, -geometry) |> st_drop_geometry() |>
    
    #Completar apenas células com NA
    pivot_longer(c(everything(), - logradouro,  - id_osm)) |> 
    left_join(logradouro, by = join_by(logradouro, name)) |> 
    mutate(value = ifelse(test = is.na(value.x), yes = value.y, no = value.x)) |> 
    pivot_wider(id_cols = c(id_osm, logradouro), names_from = name, values_from = value) |> 
    
    #Incluir o restante da base de volta
    left_join(osm |> select(id_osm, comprimento, geometry)) |> 
    (\(df) bind_rows(df, osm |> anti_join(df, by = join_by(id_osm))))()
  
  # Compreender quanto foi preenchido
  # left_join(
  #   osm |> 
  #     st_drop_geometry() |> 
  #     summarize(across(everything(), ~ .x |> is.na() |> sum())) |> 
  #     pivot_longer(everything()),
  #   trechos |> 
  #     st_drop_geometry() |> 
  #     summarize(across(everything(), ~ .x |> is.na() |> sum())) |> 
  #     pivot_longer(everything()),
  #   by = join_by(name)
  # ) |> mutate(preenchimento = value.x - value.y,
  #             preenchimento_percent = preenchimento / value.x) |> 
  #   write_csv("output/interpolacao_trechos_resultado.csv")
  
  return(trechos)
  
  # trechos |> 
  #   st_write("banco_dados/trechos.gpkg")
}

