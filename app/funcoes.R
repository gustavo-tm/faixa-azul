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

plotar_logradouro <- function(geometria){
  geometria |> 
    leaflet() |> 
    addProviderTiles("CartoDB.Positron") |> 
    addPolylines()
}
