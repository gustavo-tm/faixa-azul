calcular_radares <- function(trechos){
  # Radar próximo ----
  radares <- getbb('São Paulo') |> 
    opq(bbox = _) |> 
    add_osm_feature(key = 'highway', value = c("speed_camera")) |> 
    osmdata_sf() |> 
    (\(radar) as_tibble(radar$osm_points))() |> 
    select(id_osm_radar = osm_id,
           limite_velocidade = maxspeed,
           limite_velocidade_pesados = "maxspeed:hgv",
           geometry) |> 
    st_set_geometry("geometry")
  
  # st_write(radares, "dados_tratados/radares.gpkg")
  # radares <- read_sf("dados_tratados/radares.gpkg")
  
  radar_proximo <- trechos |> 
    #Detecta ruas no raio de 100m
    st_join(radares |> 
              st_buffer(100)) |> 
    st_drop_geometry() |> 
    filter(!is.na(id_osm_radar), limite_velocidade.x == limite_velocidade.y) |> 
    distinct(id_osm) |> 
    mutate(radar_proximo = TRUE) |> 
    right_join(trechos |> st_drop_geometry() |> select(id_osm)) |> 
    mutate(radar_proximo = replace_na(radar_proximo, 0)) |> 
    select(id_osm, radar_proximo)
  
  return(radar_proximo)
}


calcular_interseccao <- function(trechos, osm_token){
  osm_token |> 
    select(id_osm, logradouro_limpo) |> 
    group_by(id_osm) |> 
    filter(row_number() == 1)
  
  intersec <- trechos |> 
    filter(tipo_via != "service") |> 
    mutate(elevado = is.na(elevado) == FALSE) |> 
    select(id_osm, elevado) |> 
    
    #Join geográfico da base de vias com ela mesma
    left_join(osm_token) |> 
    (\(df) st_join(df, df))() |> 
    st_drop_geometry() |> 
    
    #Apenas intersecção se não tiver o mesmo nome, mesmo ID e se tiver na mesma altura
    filter(id_osm.x != id_osm.y,
           elevado.x == elevado.y, #ponte não tem intersecção com rua
           logradouro_limpo.x != logradouro_limpo.y) |> 
    group_by(id_osm = id_osm.x) |> 
    summarize(intersec = n()) |> 
    select(id_osm, intersec) |>  
    right_join(trechos |> filter(tipo_via != "service") |> select(id_osm)) |> 
    mutate(intersec = replace_na(intersec, 0)) |> 
    st_drop_geometry() |> 
    select(id_osm, intersec)
  
  return(intersec)
}


calcular_amenidades <- function(trechos){
  amenidades <- getbb('São Paulo') |> 
    opq(bbox = _) |> 
    add_osm_feature(key = 'amenity')  |> 
    osmdata_sf() |> 
  (\(amenidades) bind_rows(
    as_tibble(amenidades$osm_points) |> filter(!is.na(amenity)),
    as_tibble(amenidades$osm_polygons) |> filter(!is.na(amenity)),
    as_tibble(amenidades$osm_multipolygons) |> filter(!is.na(amenity))))() |> 
    select(id_osm_amenidade = osm_id, nome = name, tipo_amenidade = amenity, geometry) |> 
    mutate(tipo_geometria = st_geometry_type(geometry))
  
  # amenidades |> 
  #   filter(tipo_geometria == "POINT") |> 
  #   st_set_geometry("geometry") |> 
  #   st_transform(crs = "epsg:31983") |>
  #   st_intersection(distrito) |> 
  #   st_coordinates() |> 
  #   ggplot() +
  #   geom_sf(data = distrito, aes(geometry = geometry), colour = NA) +
  #   # geom_point(aes(x = X, y = Y), stroke = 0, size = .1, alpha = .2) +
  #   geom_hex(aes(x = X, y = Y), alpha = .9, bins = 60) +
  #   # scale_fill_viridis_c(limits = c(40, 10^2), na.value = "transparent") +
  #   scale_fill_gradient("Número de amenidades", low = "grey90", high = "darkblue") +
  #   theme_void()
  # 
  # ggsave("output/amenidades.pdf", width = 10, height = 12.5)
  
  amenidade_proxima <- trechos |> 
    filter(tipo_via %in% c("trunk", "primary", "secondary")) |> 
    st_simplify(dTolerance = 10) |> 
    
    #Detecta amenidades em um buffer de 30m da rua
    st_buffer(30) |> 
    st_join(amenidades |> filter(tipo_geometria == "POINT") |> st_set_geometry("geometry")) |> 
    st_drop_geometry() |> 
    group_by(id_osm) |> 
    summarize(amenidades = n()) |> 
    right_join(trechos |> 
                 filter(tipo_via %in% c("trunk", "primary", "secondary"))) |> 
    mutate(amenidades = replace_na(amenidades, 0)) |> 
    right_join(trechos |> 
                 st_drop_geometry() |> 
                 filter(tipo_via %in% c("trunk", "primary", "secondary")) |> 
                 select(id_osm)) |> 
    mutate(amenidades = replace_na(amenidades, 0)) |>
    select(id_osm, amenidades)
  
  return(amenidade_proxima)
}
  

tidy_complemento_trecho <- function(trechos, radar, intereseccao, amenidades){
  complemento <- trechos |> 
    st_drop_geometry() |> 
    select(id_osm) |> 
    left_join(intereseccao) |> 
    left_join(radar) |> 
    left_join(amenidades)
  
  # write_csv(complemento, "banco_dados/trechos_complemento.csv")
}
  




