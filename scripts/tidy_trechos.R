library(tidyverse)
library(osmdata)
library(sf)
# library(mapview)
library(igraph)
# library(circlize)

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
  
  # Escolher apenas o nome principal da via
  token_osm <- token_osm |> 
    arrange(alias) |> 
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
    circlize::adjacencyList2Matrix(square = TRUE) |> 
    graph_from_adjacency_matrix(mode = "undirected")
  
  # grafico |> 
  #   visIgraph(idToLabel = FALSE) |> 
  #   htmltools::save_html("grafico.html")
  
  id_logradouros <- grafico |> 
    components() |> 
    membership() |> 
    (\(df) tibble(id_osm = names(df), id_logradouro = df))() |> 
    left_join(token_osm) |> 
    group_by(id_logradouro = factor(id_logradouro)) |> 
    summarize(trechos = id_osm |> 
                as.character() |> 
                list(),
              logradouro = first(logradouro_limpo))
  
  return(id_logradouros)
}

ggplot() + 
  geom_histogram(aes(x = igraph::degree(grafico))) +
  scale_x_continuous(breaks = 1:30)

# g <- make_ring(10)
# igraph::degree(g, mode = c("all")) |> View()
# g2 <- sample_gnp(100, 10 / 1000)
# igraph::degree(g2)
# degree_distribution(grafico)
# plot(g2)


#CAMINHO!!!

#igraph::dfs


#igraph::shortest_paths


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

agregar.trechos <- function(trechos, metros = 500){
  
  # LISTA DE ADJACÊNCIA ----
  conexoes <- trechos |> 
    filter(tipo_via %in% c("trunk", "primary", "secondary")) |> 
    arrange(logradouro) |> 
    select(id_osm, logradouro) |> 
    
    # Join geográfico da base de vias com ela mesma
    (\(df) st_join(df, df))() |> 
    st_drop_geometry() |> 
    
    # Identificar todas as vias que se conectam, mas tem o mesmo nome
    filter(logradouro.x == logradouro.y,
           id_osm.x != id_osm.y)
  
  # GRAFO ----
  grafo <- conexoes |>
    select(id_osm.x, id_osm.y) |>
    circlize::adjacencyList2Matrix(square = TRUE) |>
    graph_from_adjacency_matrix(mode = "undirected")
  
  # GRAU E MEMBERSHIP ----
  logradouros <- tibble(
    id_osm = grafo |> 
      components() |> 
      membership() |> 
      names(),
    id_logradouro = grafo |> 
      components() |> 
      membership(),
    grau = grafo |> 
      igraph::degree()
  )
  
  # logradouros |> 
  #   ggplot() +
  #   geom_histogram(aes(x = grau)) +
  #   scale_x_continuous(breaks = 0:6) +
  #   theme_minimal() +
  #   labs(x = "Grau do vértice", y = "Frequência")
  
  
  # subgrafo <- induced_subgraph(
  #   grafo,
  #   vids = logradouros |>
  #     filter(id_logradouro == 1026) |>
  #     pull(id_osm)
  # )
  # 
  # grafo.dfs(subgrafo, grafico = T)
  
  # 
  # subgrafo2 <- induced_subgraph(
  #   grafo,
  #   vids = logradouros |> 
  #     filter(id_logradouro == 64) |> 
  #     pull(id_osm)
  # )
  # 
  # logradouros |> 
  #   filter(id_logradouro == 263) |> 
  #   left_join(trechos) |> 
  #   st_set_geometry("geometry") |> 
  #   mapview::mapview(zcol = "id_osm", legend = F) |> 
  #   mapview::mapshot2("_temp/rua 263.html")
  
  # plot.grafo <- function(grafo){
  #   set.seed(420)
  #   grafo |> 
  #     as_tbl_graph()  |> 
  #     ggraph(layout = 'fr') +  # Fruchterman-Reingold layout
  #     geom_edge_link(width = 1.5) +
  #     geom_node_point(size = 6, color = "steelblue") +
  #     # geom_node_text(aes(label = name), color = "black", size = 4) +
  #     theme_graph()
  # }
  
  # subgrafico |> plot.grafo()
  # ggsave("_temp/grafo.pdf", width = 6, height = 6)
  
  # Rodar dfs para encontrar ordem
  grafo.dfs <- function(subgrafo, grafico = FALSE){
    
    # Inicializar o dfs no melhor vértice possível
    if (sum(igraph::degree(subgrafo) == 1) > 0){
      root <- V(subgrafo)[igraph::degree(subgrafo) == 1][1]
    }else if(sum(igraph::degree(subgrafo) %% 2 == 1) > 0){
      root <- V(subgrafo)[igraph::degree(subgrafo) %% 2 == 1][1]
    }else{
      root <- V(subgrafo)[1]
    }
    
    resultado_dfs <- dfs(subgrafo, 
                         root = root,
                         order = TRUE,
                         unreachable = TRUE,
                         dist = TRUE)
    
    # Detectar quando o dfs reinicializa para separar o logradouro
    subids <- enframe(resultado_dfs$order, name = "id_osm", value = "id_grafo") |> 
      mutate(ordem = 1, ordem = cumsum(ordem)) |> 
      left_join(enframe(resultado_dfs$dist, name = "id_osm", value = "dist"),
                by = join_by(id_osm)) |> 
      mutate(naumentou = replace_na(dist <= lag(dist), FALSE),
             subid = cumsum(naumentou)) |> 
      select(id_osm, ordem, dist, subid)
    
    # Desenhar o diagrama com resultado
    if (grafico == FALSE){return(subids)}
    
    ordem <- resultado_dfs$order[!is.na(resultado_dfs$order)]
    
    dfs_edge_ids <- get.edge.ids(subgrafo, vp = as.vector(t(cbind(
      ordem[-length(ordem)],
      ordem[-1]
    ))))
    
    # Create an edge attribute to indicate DFS path
    E(subgrafo)$is_dfs <- FALSE
    E(subgrafo)$is_dfs[dfs_edge_ids] <- TRUE
    
    set.seed(420)
    subgrafo |> 
      as_tbl_graph() |> 
      left_join(subids, by = join_by(name == id_osm))  |> 
      ggraph(layout = 'fr') +  # Fruchterman-Reingold layout
      geom_edge_link(aes(color = is_dfs), width = 1.5) +
      geom_node_point(aes(colour = factor(subid)), size = 4) +
      # geom_node_text(aes(label = name), color = "black", size = 2) +
      scale_edge_color_manual(values = c("gray80", "gray20")) +
      theme_graph() +
      theme(legend.position = "none")
  }
  
  # Construção dos subgrafos para cada logradouro e roda dfs
  eulerian <- logradouros |>
    nest(.by = id_logradouro) |>
    mutate(grafo = map(
      data,
      \(x) induced_subgraph(grafo,
                            vids = pull(x, id_osm))),
      eulerian = map_lgl(grafo, has_eulerian_path),
      sub_ids = map(grafo, grafo.dfs))
  
  # Para identificar quanto % das vias tem um desenho complexo
  # eulerian |> 
  #   summarize(prop = mean(eulerian))
  
  # Juntar trechos vizinhos para tentar chegar perto de 500m
  trechos_agregado <- eulerian |> 
    select(id_logradouro, sub_ids) |> 
    unnest(sub_ids) |> 
    mutate(id_logradouro = str_c(str_pad(id_logradouro, 4, pad = "0"), str_pad(subid, 3, pad = "0"), sep = "-")) |> 
    left_join(trechos |> st_drop_geometry() |> select(id_osm, comprimento)) |> 
    group_by(id_logradouro) |> 
    mutate(comprimento_total = sum(comprimento),
           comprimento_cum = cumsum(comprimento),
           agrupamentos = comprimento_total %/% metros + 1,
           grupo = comprimento_cum %/% metros,
           id_logradouro = str_c(id_logradouro, str_pad(grupo, 3, pad = "0"), sep = "-")) |> 
    group_by(id_trecho_agregado = id_logradouro) |> 
    summarize(id_osm = list(id_osm), 
              comprimento = sum(comprimento),
              trechos = n())
  
  # Plotar os diagramas
  # eulerian |>
  #   filter(eulerian == FALSE) |> 
  #   sample_n(15) |> 
  #   mutate(grafico = map(grafo, \(x) grafo.dfs(x, T))) 
  #   # mutate(save = map(grafico, 
  #   #                   \(x) ggsave(str_c("output/agregar-trechos/", round(runif(1)*1000), ".pdf", sep = ""), 
  #   #                               x, width = 10, height = 10)))
  
  return(trechos_agregado)
}


tidy_agregados <- function(id_agregados, trechos, trechos_complemento, faixa_azul) {
  
  trechos_agregado <- id_agregados |> 
    unnest(id_osm) |> 
    left_join(trechos |> 
                st_drop_geometry() |> 
                rename(comprimento_trecho = comprimento)) |> 
    left_join(trechos_complemento) |> 
    left_join(faixa_azul) |> 
    group_by(id_trecho_agregado, trechos, comprimento) |> 
    summarize(
      across(
        c(data_implementacao),
        ~ .x |> sort() |> first()),
      across(
        c(faixas, limite_velocidade),
        ~ .x |> as.numeric() |> mean(na.rm = TRUE)),
      across(
        c(amenidades, intersec),
        ~ .x |> as.numeric() |> sum(na.rm = TRUE)),
      across(
        c(mao_unica, superficie, tipo_via),
        ~ fct_infreq(.x) |> levels() |> first()),
      radar_proximo = max(radar_proximo))
  return(trechos_agregado)
}
