plot_obitos_ano <- function(sinistros, vitimas){
  gg <- vitimas |> 
    left_join(sinistros |> select(id_infosiga, data)) |> 
    filter(gravidade_lesao == "FATAL", year(data) >= 2015, year(data) <= 2024) |> 
    mutate(tipo_veiculo_vitima = str_to_upper(tipo_veiculo_vitima),
           veiculo = fct_collapse(tipo_veiculo_vitima,
                                  other_level = "outros",
                                  motocicleta = "MOTOCICLETA",
                                  nao_disponivel = "NAO DISPONIVEL") |>
             factor(levels = c("outros", "nao_disponivel", "motocicleta"))) |> 
    group_by(data = make_date(year = year(data)), veiculo) |> 
    summarize(obitos = n()) |>
    mutate(y = case_when(veiculo == "motocicleta" ~ obitos, 
                         veiculo == "outros" ~ sum(obitos)),
           label = case_when(veiculo == "motocicleta" ~ scales::percent(obitos / sum(obitos)), 
                             veiculo == "outros" ~ sum(obitos) |> as.character())) |> 
    ggplot(aes(x = data)) +
    geom_col(aes(y = obitos, fill = veiculo)) +
    geom_text(aes(y = y, label = label), nudge_y = -50, colour = "white") +
    scale_fill_manual("Modo de transporte\nda vítima", 
                      values = c("grey40", 
                                 "grey80", 
                                 "#4472C4"), 
                      labels = c("Outros", "Não disponível", "Motocicleta")) +
    scale_x_date(NULL, date_breaks = "years", date_labels = "%Y") +
    labs(y = "Total de óbitos") +
    theme_minimal()
  
  ggsave("output/plot - obitos anuais por modo.pdf", gg, width = 9, height = 5)
}




plot_datas_FA <- function(logradouros, logradouros_id, match, sinistros){
  
  max_data <- max(sinistros$data)
  
  gg <- logradouros |> 
    filter(!is.na(data_implementacao)) |> 
    group_by(nome) |> 
    filter(row_number(data_implementacao) == 1) |> 
    ungroup() |> 
    mutate(nome = factor(nome) |> fct_reorder(desc(data_implementacao))) |> 
    ggplot(aes(y = nome)) +
    geom_rect(aes(fill = "Antes da implementação",
                  xmin = as.Date(-Inf), 
                  xmax = data_implementacao, 
                  ymin = as.numeric(nome) - .15, 
                  ymax = as.numeric(nome) + .15)) +
    geom_rect(aes(fill = "Depois da implementação",
                  xmin = data_implementacao, 
                  xmax = as.Date(Inf), 
                  ymin = as.numeric(nome) - .25, 
                  ymax = as.numeric(nome) + .25), alpha = .75) +
    scale_x_date(limits = c(make_date(year = 2021, month = 6), max_data)) +
    scale_fill_manual(NULL, values = c("Antes da implementação" = "grey90", "Depois da implementação" = "#333F48FF")) +
    labs(y = NULL, x = NULL) +
    theme_minimal()
  ggsave("output/plot - ordem de implementacao.pdf", gg, width = 11, height = 7)
  
  ggg <- gg +
    new_scale_fill() +
    geom_point(aes(x = data, fill = motocicleta, shape = motocicleta),
               position = position_jitter(width = 0, height = .1),
               alpha = .9, stroke = .15, colour = "white", size = 2.5,
               data = logradouros_id |> 
                 semi_join(logradouros |> 
                             filter(!is.na(data_implementacao)) |> 
                             select(id_logradouro)) |> 
                 unnest(trechos) |> 
                 rename(id_osm = trechos, nome = logradouro) |> 
                 inner_join(match |> filter(golden_match) |> select(id_sinistro, id_osm)) |> 
                 left_join(sinistros |> 
                             select(id_sinistro, data, tipo, tp_veiculo_motocicleta)) |> 
                 mutate(motocicleta = replace_na(tp_veiculo_motocicleta, 0) > 0) |> 
                 arrange(motocicleta) |> 
                 filter(tipo == "SINISTRO FATAL")) +
    # geom_jitter() +
    scale_fill_manual("Modo de transporte da vítima", values = c("TRUE" = "#BA0C2FFF", "FALSE" = "#C6AA76FF"), labels = c("TRUE" = "Motocicleta", "FALSE" = "Outros")) +
    scale_shape_manual("Modo de transporte da vítima", values = c("TRUE" = 21, "FALSE" = 22), labels = c("TRUE" = "Motocicleta", "FALSE" = "Outros"))
  
  ggsave("output/plot - obitos em cada via.pdf", ggg, width = 11, height = 8)
  
}


# TAMANHO VIAS ----
plot_tamanho_FA <- function(logradouros_id, logradouros, faixa_azul, trechos){
  gg <- logradouros_id |> 
    semi_join(logradouros |> 
                filter(!is.na(data_implementacao)) |> 
                select(logradouro = nome)) |> 
    unnest(trechos) |> 
    rename(id_osm = trechos) |> 
    left_join(faixa_azul |> select(id_osm) |> mutate(FA = TRUE)) |> 
    left_join(trechos |> select(id_osm, comprimento)) |> 
    mutate(FA = replace_na(FA, FALSE)) |> 
    group_by(logradouro, FA) |>
    summarize(comprimento = sum(comprimento) / 1000) |> 
    mutate(ordem = sum(comprimento)) |> 
    ggplot(aes(x = comprimento, y = reorder(logradouro,ordem))) +
    geom_col(aes(fill = FA),  width = .8) +
    scale_fill_manual("", values = c("FALSE" = "#A6A6A6", "TRUE" = "#4472C4"),
                      labels = c("TRUE" = "Com faixa azul", "FALSE" = "Sem faixa azul")) +
    labs(x = "Extensão (km) do logradouro", y = NULL) +
    theme_minimal()
  ggsave("output/plot - extensao faixa azul.pdf", gg, width = 11, height = 7.5)
  
} 




# PANFLETO CET ----

plot_obitos_tempo <- function(sinistros, vitimas, match, agregados){
  gg <- vitimas |>
    left_join(sinistros) |> 
    filter(gravidade_lesao == "FATAL", year(data) > 2015, year(data) < 2025) |> 
    left_join(match, by = join_by(id_sinistro)) |> 
    filter(golden_match) |> 
    left_join(agregados) |> 
    filter(!is.na(data_implementacao)) |> 
    mutate(tipo_veiculo_vitima = str_to_upper(tipo_veiculo_vitima),
           veiculo = fct_collapse(tipo_veiculo_vitima,
                                  other_level = "outros",
                                  motocicleta = "MOTOCICLETA",
                                  nao_disponivel = "NAO DISPONIVEL") |>
             factor(levels = c("outros", "nao_disponivel", "motocicleta"))) |> 
    group_by(data = make_date(year = year(data)), veiculo) |> 
    summarize(obitos = n()) |> 
    mutate(y = case_when(veiculo == "motocicleta" ~ obitos, 
                         veiculo == "outros" ~ sum(obitos)),
           label = case_when(veiculo == "motocicleta" ~ scales::percent(obitos / sum(obitos)), 
                             veiculo == "outros" ~ sum(obitos) |> as.character())) |> 
    ggplot(aes(x = data)) +
    geom_col(aes(y = obitos, fill = veiculo)) +
    geom_text(aes(y = y, label = label), nudge_y = -3, colour = "white") +
    scale_fill_manual("Modo de transporte\nda vítima", 
                      values = c("grey40", 
                                 "grey80", 
                                 "#4472C4"), 
                      labels = c("Outros", "Não disponível", "Motocicleta")) +
    scale_x_date(NULL, date_breaks = "years", date_labels = "%Y") +
    labs(y = "Total de óbitos") +
    theme_minimal()
  ggsave("output/plot - obitos anuais por modo (faixa azul).pdf", gg, width = 9, height = 5)
}

# SINISTROS EM CADA HORA DO DIA ----

plot_hora_sinistro <- function(sinistros){
  gg <- sinistros |>
    filter(year(data) > 2018, year(data) <= 2024) |>
    mutate(mes = fct_collapse(month(data) |> factor(),
                              "Jan-Mar" = 1:3,
                              "Abr-Jun" = 4:6,
                              "Jul-Set" = 7:9,
                              "Out-Dez" = 10:12,
                              other_level = "teste")) |>
    group_by(hora, dia = day(data), mes) |>
    summarize(sinistros = n()) |>
    ggplot(aes(x = dia, y = hora, fill = sinistros)) +
    geom_tile() +
    facet_grid(cols = vars(mes)) +
    theme_minimal() +
    scale_fill_viridis_c("Total de sinistros") +
    scale_y_continuous("Horário", breaks = 0:11*2) +
    scale_x_continuous("Dia do mês", breaks = NULL)
  
  ggsave("output/plot - sinistros por horario.pdf", gg, width = 10, height =4)
}



plot_datas_trechos <- function(faixa_azul, trechos, token_osm){
  df <- faixa_azul |>
    left_join(trechos |> select(id_osm, comprimento)) |>
    left_join(token_osm |> 
                arrange(alias) |> 
                group_by(id_osm) |> 
                filter(row_number() == 1) |> 
                select(id_osm, logradouro = logradouro_limpo)) |> 
    mutate(logradouro = logradouro |> 
             factor() |> 
             fct_reorder(desc(data_implementacao))) |>
    group_by(data_implementacao, logradouro) |>
    summarize(trechos = sum(comprimento, na.rm = T) / 1000) |> ungroup() |>
    complete(data_implementacao = seq(min(data_implementacao), max(data_implementacao), by = "1 month"), logradouro, fill = list(trechos = 0)) |>
    group_by(logradouro) |>
    mutate(trechos_total = cumsum(trechos)) |> 
    drop_na()
  
  gg <- df |> 
    ggplot(aes(x= data_implementacao)) +
    geom_col(aes(y = trechos_total, fill = logradouro)) +
    labs(x = NULL, y = "Extensão (km) dos trechos com faixa azul",
         title = NULL) +
    theme_minimal() +
    # scale_fill_viridis_d(direction = -1) +
    scale_fill_manual("Logradouros que receberam faixa azul (em ordem de implementação)\n", 
                      values = paletteer::paletteer_d("ggsci::default_igv"),
                      breaks = df |> pull(logradouro) |> unique() |> rev()) +
    theme(legend.position = "bottom",
          legend.title.position = "top",
          legend.text=element_text(size=8))
  
  ggsave("output/plot - extensao da faixa azul por mes.pdf", gg, width = 12, height = 10)
  
}



# # QUALIDADE DO MATCH ----
plot_qualidade_match <- function(sinistros, match){
  
  
  tabela <- sinistros |>
    filter(tipo != "NOTIFICACAO", year(data) > 2018) |>
    select(id_sinistro, data, logradouro, numero, latitude, longitude, tipo) |>
    mutate(numero_zero = as.numeric(numero) == 0) |>
    select(id_sinistro, data, numero_zero) |>
    left_join(match) |>
    mutate(qualidade_match = case_when(similaridade == 1 & distancia_geografica < 50 & match_tipo & match_titulo & numero_zero == FALSE ~ "Perfeito",
                                       similaridade > .85 & distancia_geografica < 150 & (match_tipo | match_titulo) & numero_zero == FALSE ~ "Excelente",
                                       similaridade > .7 & distancia_geografica < 250 ~ "Aceitável",
                                       is.na(id_osm) ~ "Não encontrou match",
                                       .default = "Ruim") |>
             factor(levels = c("Perfeito", "Excelente", "Aceitável", "Ruim", "Não encontrou match") |> rev()))
  
  gg <- tabela |>
    group_by(qualidade_match) |>
    summarize(n = n()) |>
    mutate(percent = n / sum(n)) |>
    ggplot(aes(y = qualidade_match)) +
    geom_col(aes(x = n)) +
    geom_text(aes(x = n, label = percent |> round(3) |> scales::percent()), nudge_x = 7500) +
    scale_x_continuous(labels = scales::number, expand = expansion(mult = c(0.05, 0.075))) +
    theme_minimal() +
    theme(legend.position = "none",
          plot.caption = element_text(hjust = 0),
          plot.margin = margin(10,30,10,10)) +
    labs(x = "Quantidade de sinistros", y = "Qualidade do match")
  
  
  ggsave("output/plot - qualidade do match.pdf", gg, width = 6, height = 6)
}






# MAPA SINISTROS ----
plot_mapas <- function(sinistros, trechos, faixa_azul){
  distrito <- st_read("dados_tratados/distrito/SIRGAS_SHP_distrito.shp") |>
    st_set_crs("epsg:31983") |>
    summarize(geometry = st_union(geometry) |> st_simplify(dTolerance = 100))
  
  trechos.mapa <- trechos |> 
    filter(!tipo_via %in% c("service", "unclassified")) |> 
    st_transform("epsg:31983") |> 
    st_intersection(distrito)
  
  gg <- sinistros |>
    filter(tipo != "NOTIFICACAO", !is.na(longitude), !is.na(latitude)) |>
    st_as_sf(coords = c("longitude", "latitude"), crs = "EPSG:4326") |>
    st_transform(crs = "epsg:31983") |>
    st_intersection(distrito) |>
    st_coordinates() |>
    ggplot() +
    geom_sf(data = distrito,
            aes(geometry = geometry), colour = NA, fill = "grey98") +
    geom_sf(data = trechos.mapa |>
              filter(tipo_via %in% c("trunk", "primary", "secondary")),
            aes(geometry = geometry), colour = "#3c3744", lwd = .3, alpha = .7) +
    geom_hex(aes(x = X, y = Y), alpha = .7, bins = 40) +
    geom_sf(data = distrito,
            aes(geometry = geometry), colour = "grey25", fill = NA, alpha = .7) +
    scale_fill_gradient("Total de sinistros", low = "grey98", high = "darkred") +
    theme_void()
  
  ggsave("output/mapa - sinistros.pdf", gg, width = 10, height = 15)
  
  gg <- ggplot() +
    geom_sf(data = distrito,
            aes(geometry = geometry), colour = "grey25", fill = "grey98") +
    geom_sf(data = trechos.mapa |> 
              filter(tipo_via %in% c("residential", "tertiary")),
            aes(geometry = st_simplify(geometry, dTolerance = 10)), colour = "grey50", lwd = .15, alpha = .8) +
    geom_sf(data = trechos.mapa |> 
              filter(!tipo_via %in% c("residential", "tertiary")),
            aes(geometry = st_simplify(geometry, dTolerance = 10)), colour = "#3c3744", lwd = .3, alpha = .8) +
    geom_sf(data = trechos.mapa |> 
              semi_join(faixa_azul),
            aes(geometry = geometry), colour = "#090c9b", lwd = 3, alpha = .1) +
    geom_sf(data = trechos.mapa |> 
              semi_join(faixa_azul),
            aes(geometry = geometry), colour = "#090c9b", lwd = 1, alpha = .1) +
    geom_sf(data = trechos.mapa |> 
              semi_join(faixa_azul),
            aes(geometry = geometry), colour = "#090c9b", lwd = .5, alpha = .8) +
    theme_void()
  
  ggg <- gg +
    ggspatial::annotation_scale(
      location = "br",
      bar_cols = c("grey60", "white"),
    ) +
    ggspatial::annotation_north_arrow(
      location = "br", which_north = "true",
      pad_x = unit(0.75, "in"), pad_y = unit(0.4, "in"))
  
  ggsave("output/mapa - faixa azul.pdf", ggg, width = 10, height = 15)
  
  df <- faixa_azul |> 
    left_join(trechos |> st_transform("epsg:31983")) |> 
    group_by(data_implementacao = data_implementacao) |> 
    summarize(geometry = st_union(geometry)) |> 
    (\(df) df |>
       select(data_implementacao) |> 
       mutate(grupo = rep(list(1:length(data_implementacao)), length(data_implementacao))) |> 
       unnest(grupo) |> 
       left_join(df |> 
                   mutate(grupo = 1:length(data_implementacao)) |> 
                   select(-data_implementacao)))() |> 
    filter(grupo <= data_implementacao |> factor() |> as.numeric())
  
  gg <- ggplot(mapping = aes(geometry = geometry)) +
    geom_sf(data = distrito, colour = "grey25", fill = "grey98") +
    geom_sf(data = df, aes(colour = factor(grupo)), lwd = 1, alpha = .8) +
    transition_time(data_implementacao) +
    scale_colour_viridis_d() +
    labs(title = "{frame_time}") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, size = 80),
          legend.position = "none")
  
  anim_save("output/mapa - faixa azul.gif", gg, 
            renderer = gifski_renderer(loop = T), 
            width = 2250, height = 3000, 
            duration = 10, end_pause = 20)
}


# AGREGAÇÃO TRECHOS ----

plot_agregacao_trechos <- function(trechos, id_logradouros, agregados){
  # Estatística descritiva do tamanho dos trechos
  g <- bind_rows(list(
    trechos |> 
      st_drop_geometry() |> 
      select(comprimento) |> 
      mutate(grupo = "Trecho (microdado)"),
    
    id_logradouros |> 
      unnest(trechos) |>
      rename(id_osm = trechos) |>
      select(-logradouro) |> 
      left_join(trechos)  |>  
      st_drop_geometry() |> 
      group_by(id_logradouro) |> 
      summarize(comprimento = sum(comprimento),
                n = n()) |> 
      select(comprimento) |> 
      mutate(grupo = "Logradouro"),
    
    agregados |> 
      select(comprimento) |> 
      mutate(grupo = "Trecho agregado")
  ))
  
  gg <- g |> 
    ggplot() +
    geom_density(aes(fill = grupo, x = comprimento, colour = grupo), alpha = .6) +
    geom_segment(data = g |> 
                   group_by(grupo) |> 
                   summarize(mean = mean(comprimento)),
                 aes(x = mean, xend = mean, y = 0, yend = 2.1, colour = grupo),
                 linetype = "dashed") +
    geom_text(data = g |> 
                group_by(grupo) |> 
                summarize(mean = mean(comprimento)) |> 
                mutate(label = mean |> round()),
              aes(x = mean, label = label, y= 2.3, colour = grupo)) +
    scale_x_continuous("Comprimento da via (em metros) - escala log10", trans = "log10") +
    scale_y_continuous("Densidade de probabilidade", breaks = NULL) +
    labs(fill = "Nível de agregação", colour = "Nível de agregação") +
    theme_minimal()
  
  ggsave("output/plot - extensao dos trechos por agregacao.pdf", gg, width = 9, height = 4.5)
}


plot_antes_depois <- function(sinistros, vitimas, agregados, match){

  gg <- sinistros |>
    left_join(vitimas |> 
                filter(gravidade_lesao == "FATAL") |> 
                mutate(grupo = fct_collapse(str_to_upper(tipo_veiculo_vitima),
                                            motocicleta = "MOTOCICLETA",
                                            pedestre = "PEDESTRE",
                                            nao_disponivel = "NAO DISPONIVEL",
                                            other_level = "outros")) |> 
                group_by(id_infosiga, grupo) |> 
                summarize(obitos = n()) |> 
                ungroup() |> 
                pivot_wider(id_cols = id_infosiga, names_from = grupo, values_from = obitos, 
                            names_prefix = "obitos_", values_fill = 0)) |> 
    select(id_sinistro,  data, starts_with("obitos")) |> 
    left_join(match) |> 
    filter(golden_match) |> 
    left_join(agregados |> select(id_trecho_agregado, data_implementacao)) |> 
    filter(!is.na(data_implementacao)) |> 
    mutate(data = make_date(year = year(data), month = month(data)),
           dist = time_length(data - data_implementacao, "months") |> round(),
           periodo = case_when(dist > 0 ~ "pos", dist < 0 ~ "pre")) |>
    filter(abs(dist) <= 12) |> 
    group_by(data_implementacao, dist) |> 
    summarize(across(c(starts_with("obitos")), ~ sum(.x, na.rm = T))) |> 
    select(-obitos_nao_disponivel) |> 
    pivot_longer(starts_with("obito")) |> 
    ungroup() |> 
    complete(data_implementacao, dist, name, fill = list(value = 0)) |> 
    (\(df) bind_rows(
      df |> 
        filter(abs(dist) <= 6, 
               data_implementacao <= make_date(year = 2024, month = 10)) |> 
        mutate(p = "06 meses antes e depois (vias implementadas até 10/24)"),
      
      df |> 
        filter(abs(dist) <= 12, 
               data_implementacao <= make_date(year = 2024, month = 4)) |> 
        mutate(p = "12 meses antes e depois (vias implementadas até 04/24)")
    ))() |> 
    group_by(p, dist, name) |> 
    summarize(value = sum(value)) |>
    mutate(name = factor(name, levels = rev(levels(factor(name))))) |> 
    ggplot(aes(x = dist, y = value, fill = name)) +
    geom_vline(xintercept = 0, linetype = "dashed")+ 
    geom_vline(xintercept = 6.5, alpha = .3, colour = "grey60", lwd = 2)+ 
    geom_vline(xintercept = -6.5, alpha = .3, colour = "grey60", lwd = 2)+ 
    geom_vline(xintercept = 12.5, alpha = .3, colour = "grey60", lwd = 2)+ 
    geom_vline(xintercept = -12.5, alpha = .3, colour = "grey60", lwd = 2)+ 
    geom_col() +
    facet_wrap(~p, nrow = 3) + 
    scale_x_continuous(breaks = 0:24-12) +
    scale_fill_manual("Meio de transporte da vítima", 
                      labels = c(obitos_motocicleta = "Motocicleta", obitos_outros = "Outros", obitos_pedestre = "Pedestre"),
                      values = c(obitos_outros = "grey40", obitos_pedestre = "grey80", obitos_motocicleta = "#4472C4")) +
    labs(x = "Meses até o tratamento", y = "Total de óbitos") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  ggsave("output/plot - obitos pre vs pos.pdf", gg, width = 10, height = 6)
  
  gg <- sinistros |>
    mutate(moto = tp_veiculo_motocicleta > 0 & !is.na(tp_veiculo_motocicleta),
           fatal = gravidade_fatal > 0 & !is.na(gravidade_fatal)) |>
    select(id_sinistro,  data, moto) |> 
    left_join(match) |> 
    filter(golden_match) |> 
    left_join(agregados |> select(id_trecho_agregado, data_implementacao)) |> 
    filter(!is.na(data_implementacao)) |> 
    mutate(data = make_date(year = year(data), month = month(data)),
           dist = time_length(data - data_implementacao, "months") |> round(),
           periodo = case_when(dist > 0 ~ "pos", dist < 0 ~ "pre")) |>
    filter(abs(dist) <= 12) |> 
    group_by(data_implementacao, dist, moto) |> 
    summarize(n = n()) |> 
    ungroup() |> 
    complete(data_implementacao, dist, moto, fill = list(n = 0)) |> 
    mutate(n = ifelse(make_date(year = 2025, month = 5) - months(dist) < data_implementacao, NA, n)) |> 
    (\(df) bind_rows(
      df |> 
        filter(abs(dist) <= 6, 
               data_implementacao <= make_date(year = 2024, month = 10)) |> 
        mutate(p = "06 meses antes e depois (vias implementadas até 10/24)"),
      
      df |> 
        filter(abs(dist) <= 12, 
               data_implementacao <= make_date(year = 2024, month = 4)) |> 
        mutate(p = "12 meses antes e depois (vias implementadas até 04/24)")
    ))() |> 
    group_by(p, dist, moto) |>
    summarize(n = sum(n)) |>
    ggplot(aes(x = dist, y = n, fill = moto)) +
    geom_vline(xintercept = 0, linetype = "dashed")+ 
    geom_vline(xintercept = 6.5, alpha = .3, colour = "grey60", lwd = 2)+ 
    geom_vline(xintercept = -6.5, alpha = .3, colour = "grey60", lwd = 2)+ 
    geom_vline(xintercept = 12.5, alpha = .3, colour = "grey60", lwd = 2)+ 
    geom_vline(xintercept = -12.5, alpha = .3, colour = "grey60", lwd = 2)+ 
    geom_col() +
    facet_wrap(~p, nrow = 3, scales = "free_y") + 
    scale_x_continuous(breaks = 0:24-12) +
    scale_fill_manual("Veículos envolvidos", labels = c("Não envolveu motocicleta",  "Envolveu motocicleta"), 
                      values =  c("grey60", "#4472C4")) +
    labs(x = "Meses até o tratamento", y = "Total de sinistros") +
    theme_minimal() +
    theme(legend.position = "bottom")  

  ggsave("output/plot - sinistros pre vs pos.pdf", gg, width = 10, height = 6)
  
}
