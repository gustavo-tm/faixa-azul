obter_logradouro <- function(logradouro_selecionado){
  logradouros_id |> 
    filter(logradouro %in% logradouro_selecionado) |> 
    unnest(trechos) |> 
    select(id_osm = trechos) |> 
    left_join(trechos, by = join_by(id_osm)) |> 
    left_join(faixa_azul, by = join_by(id_osm)) |> 
    mutate(faixa_azul = !is.na(data_implementacao)) |> 
    st_as_sf()
}


plotar_logradouro <- function(geometria, pontos = NULL){
  pal_faixa_azul <- colorNumeric(c("grey40", "darkblue"), domain = c(0,1))
  mapa <- geometria |> 
    leaflet() |> 
    addProviderTiles("CartoDB.Positron") |> 
    addPolylines(color = ~pal_faixa_azul(faixa_azul))
  
  if(!is.null(pontos)){
    pal_obito <- colorFactor(c("red", "black"), pontos$tipo)
    mapa <- mapa |> 
      addCircles(data = pontos, 
                 stroke = FALSE,
                 fillColor = ~pal_obito(tipo),
                 fillOpacity = ~  replace_na(((max(distancia) - distancia) / (max(distancia))) ^2, 1)
                 # radius = 12
                 )
  }
  return(mapa)
}

coletar_pontos_entorno <- function(geometria, sinistros){
  geometria |> 
      summarize(geometry = st_union(geometry)) |> 
      st_buffer(300) |> 
      st_simplify(dTolerance = 100) |> 
      (\(logradouro) st_filter(sinistros, logradouro, .predicate = st_within))() |> 
      mutate(distancia = st_distance(geometry, st_union(geometria))[,1])
}

coletar_pontos_golden <- function(logradouro_selecionado, sinistros){
  logradouros_id |>
    filter(logradouro %in% logradouro_selecionado) |>
    unnest(trechos) |>
    select(id_osm = trechos) |>
    semi_join(trechos, by = join_by(id_osm)) |>
    left_join(match, by = join_by(id_osm)) |>
    filter(golden_match) |>
    left_join(sinistros, by = join_by(id_sinistro)) |>
    mutate(distancia = 0) |> 
    st_as_sf()
}




