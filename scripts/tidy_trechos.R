library(tidyverse)
library(osmdata)
library(sf)
# library(mapview)
library(igraph)
library(circlize)

# https://cran.r-project.org/web/packages/osmdata/vignettes/osmdata.html
# https://rspatialdata.github.io/osm.html

# https://wiki.openstreetmap.org/wiki/Map_features
# available_features()

download_osm <- function(){
  assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))
  osm <- getbb('São Paulo') |> 
    opq(bbox = _) |> 
    add_osm_feature(key = 'highway', value = c(
      "motorway", "trunk", "primary", "secondary", "tertiary", "unclassified", "residential", "service",
      "motorway_link", "trunk_link", "primary_link", "secondary_link", "motorway_junction"))  |>
    # add_osm_feature(key = 'highway', value = c(
    #   "trunk", "primary", "secondary"))  |>
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
    # separate_wider_delim(alt_name, ";", names_sep = "_", too_few = "align_start") |> 
    select(id_osm = osm_id,
           logradouro = name,
           logradouro_alt1 = alt_name,
           logradouro_alt2 = alt_name1,
           logradouro_alt3 = alt_name_1,
           # logradouro_alt1 = alt_name_1,
           # logradouro_alt2 = alt_name_2,
           # logradouro_alt3 = alt_name_3,
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
    (\(df) bind_rows(df, osm |> anti_join(df, by = join_by(id_osm))))() |> 
    st_set_geometry("geometry")
  
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


agrupar_logradouros <- function(trechos, token_osm){
  
  token_osm <- token_osm |> 
    group_by(id_osm) |> 
    filter(row_number() == 1)
  
  conexoes <- trechos |> 
    filter(tipo_via %in% c("trunk", "primary", "secondary")) |> 
    arrange(logradouro) |> 
    select(id_osm) |> 
    
    # Join geográfico da base de vias com ela mesma
    st_buffer(10) |> 
    (\(df) st_join(df, df))() |> 
    st_drop_geometry() |> 
    
    # Identificar todas as vias que se conectam, mas tem o mesmo nome
    left_join(token_osm |> select(id_osm, log.x = logradouro_limpo), 
              by = join_by(id_osm.x == id_osm)) |> 
    left_join(token_osm |> select(id_osm, log.y = logradouro_limpo), 
              by = join_by(id_osm.y == id_osm)) |> 
    filter(log.x == log.y)
  
  grafico <- conexoes |> 
    select(id_osm.x, id_osm.y) |> 
    adjacencyList2Matrix(square = TRUE) |> 
    graph_from_adjacency_matrix()
  
  # grafico |> 
  #   visIgraph(idToLabel = FALSE) |> 
  #   htmltools::save_html("grafico.html")
  
  id_logradouros <- grafico |> 
    components() |> 
    membership() |> 
    (\(df) tibble(id_osm = names(df), id_logradouro = df))() |> 
    left_join(token_osm |> 
                group_by(id_osm) |> 
                filter(row_number() == 1) |> 
                select(id_osm, logradouro_limpo)) |> 
    group_by(id_logradouro = factor(id_logradouro)) |> 
    summarize(trechos = id_osm |> 
                as.character() |> 
                list(),
              logradouro = first(logradouro_limpo))
  
  return(id_logradouros)
}


tidy_logradouros <- function(id_logradouros, trechos, trechos_complemento, faixa_azul){
  
  logradouros <- id_logradouros |> 
    unnest(trechos) |> 
    left_join(trechos |> st_drop_geometry(), by = join_by(trechos == id_osm)) |> 
    left_join(trechos_complemento, by = join_by(trechos == id_osm)) |> 
    left_join(faixa_azul, by = join_by(trechos == id_osm)) |> 
    rename(nome = logradouro.x) |> 
    group_by(id_logradouro) |> 
    summarize(
      across(
        c(nome, data_implementacao),
        ~ .x |> sort() |> first()),
      across(
        c(faixas, limite_velocidade),
        ~ .x |> as.numeric() |> mean(na.rm = TRUE)),
      across(
        c(amenidades, intersec, comprimento),
        ~ .x |> as.numeric() |> sum(na.rm = TRUE)),
      across(
        c(mao_unica, superficie, tipo_via),
        ~ fct_infreq(.x) |> levels() |> first()),
      radar_proximo = max(radar_proximo),
      trechos = n())
  return(logradouros)
}



