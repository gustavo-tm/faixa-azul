library(tidyverse)
library(sf)


dado_trecho_mes <- function(sinistros, match, trechos, tipo_vias = c("trunk", "primary", "secondary")){
  df <- sinistros |> 
    rename_with(~ str_replace(., "tp_veiculo", "veiculo"), contains("tp_veiculo")) |> 
    filter(year(data) >= 2019 & year(data) <= 2024) |> 
    select(id_sinistro, data, hora, contains("veiculo"), contains("gravidade"), tipo_acidente) |> #quantidade_envolvidos
    mutate(across(matches("veiculo|gravidade"), ~ replace_na(.x, 0)),
           qtd_envolvidos = gravidade_leve + gravidade_grave + gravidade_fatal + gravidade_nao_disponivel) |> 
    
    # dummy de veiculo envolvido
    mutate(across(starts_with("veiculo_"), ~ . > 0, .names = "sinistros_{.col}"),
           sinistros_apenas_moto = 
             sinistros_veiculo_motocicleta & 
             if_all(contains("sinistros_veiculo") & !matches("sinistros_veiculo_motocicleta"), ~ . == FALSE),
           sinistros_moto_carro = 
             sinistros_veiculo_motocicleta &
             sinistros_veiculo_automovel,
           sinistros_moto_outros = 
             sinistros_veiculo_motocicleta &
             if_any(matches("sinistros_veiculo") & !matches("sinistros_veiculo_motocicleta"), ~ . == TRUE),
           sinistros_apenas_outros = !sinistros_apenas_moto) |> 
    
    # dummy de tipo de acidente
    mutate(acidente = 1,
           tipo_acidente = tipo_acidente |> str_to_lower() |> str_replace(" ", "_")) |> 
    pivot_wider(names_from = tipo_acidente, values_from = acidente, values_fill = 0, names_prefix = "acidente_") |> 
    
    mutate(sinistros_choque_apenas_moto    = acidente_choque  & sinistros_apenas_moto,
           sinistros_choque_moto_carro     = acidente_choque  & sinistros_moto_carro,
           sinistros_choque_moto_outros    = acidente_choque  & sinistros_moto_outros,
           sinistros_choque_apenas_outros  = acidente_choque  & sinistros_apenas_outros,
           sinistros_colisao_apenas_moto   = acidente_colisao & sinistros_apenas_moto,
           sinistros_colisao_moto_carro    = acidente_colisao & sinistros_moto_carro,
           sinistros_colisao_moto_outros   = acidente_colisao & sinistros_moto_outros,
           sinistros_colisao_apenas_outros = acidente_colisao & sinistros_apenas_outros) |> 
    
    # dummy de gravidade
    mutate(sinistros_gravidade_fatal = gravidade_fatal > 0,
           sinistros_gravidade_grave = gravidade_grave > 0 & !sinistros_gravidade_fatal,
           sinistros_gravidade_leve  = gravidade_leve  > 0 & !sinistros_gravidade_grave) |> 
    mutate(sinistros_gravidade_fatal_moto = sinistros_gravidade_fatal & sinistros_veiculo_motocicleta,
           sinistros_gravidade_grave_moto = sinistros_gravidade_grave & sinistros_veiculo_motocicleta,
           sinistros_gravidade_leve_moto  = sinistros_gravidade_leve  & sinistros_veiculo_motocicleta) |> 
    
    # dummy de horário
    mutate(sinistros_hora_05_10 = hora >= 5  & hora <= 10,
           sinistros_hora_11_16 = hora >= 11 & hora <= 16,
           sinistros_hora_12_22 = hora >= 12 & hora <= 22,
           sinistros_hora_17_20 = hora >= 17 & hora <= 20,
           sinistros_hora_18_19 = hora >= 18 & hora <= 19) |> 
    mutate(sinistros_hora_05_10_moto = hora >= 5  & hora <= 10 & sinistros_veiculo_motocicleta,
           sinistros_hora_11_16_moto = hora >= 11 & hora <= 16 & sinistros_veiculo_motocicleta,
           sinistros_hora_12_22_moto = hora >= 12 & hora <= 22 & sinistros_veiculo_motocicleta,
           sinistros_hora_17_20_moto = hora >= 17 & hora <= 20 & sinistros_veiculo_motocicleta,
           sinistros_hora_18_19_moto = hora >= 18 & hora <= 19 & sinistros_veiculo_motocicleta) |> 
    mutate(sinistros_hora_17_20_choque         = hora >= 17 & hora <= 20 & acidente_choque,
           sinistros_hora_17_20_colisao        = hora >= 17 & hora <= 20 & acidente_colisao,
           sinistros_hora_17_20_choque_colisao = hora >= 17 & hora <= 20 & (acidente_colisao | acidente_colisao),
           sinistros_hora_17_20_atropelamento  = hora >= 17 & hora <= 20 & acidente_atropelamento,
           sinistros_hora_17_20_outros         = hora >= 17 & hora <= 20 & acidente_outros) |> 
    
    left_join(match |>
                mutate(golden_match =
                         similaridade > .85 &
                         distancia_geografica < 150 &
                         (match_titulo == TRUE | match_tipo == TRUE) & 
                         numero_zero == FALSE)) |>
    semi_join(trechos |>
                st_drop_geometry() |>
                filter(tipo_via %in% tipo_vias), by = join_by(id_osm)) |>
    group_by(id_osm,
             data = make_date(year = year(data), month = month(data)),
             golden_match = golden_match) |>
    summarize(sinistros = n(),
              qtd_envolvidos = sum(qtd_envolvidos),
              across(matches("sinistros_|veiculo|acidente|gravidade"), sum)) |>
    ungroup() 
  
  zero_cols <- c(
    "sinistros", "qtd_envolvidos",
    grep("^veiculo_|^acidente_|^gravidade_|^sinistros_", names(df), value = TRUE)
  )
  
  fill_list <- as.list(setNames(rep(0, length(zero_cols)), zero_cols))
  
  df <- df |>
    complete(data, id_osm, golden_match, fill = fill_list) |> # Painel balanceado
    select(data, id_osm, golden_match, qtd_envolvidos, contains("sinistros"),
           contains("veiculo"), contains("acidente"), contains("gravidade"))
  
  return(df)
}

prepara_trecho_did <- function(dado_did, trechos, trechos_complemento, faixa_azul, filtrar_por = c(golden_match),
                               vizinhos = FALSE) {
  dado_did <- dado_did |> 
    filter(if_all({{ filtrar_por }}, ~ .x == TRUE)) |> 
    group_by(id_osm, data) |>
    summarize(sinistros = sum(sinistros),
              qtd_envolvidos = sum(qtd_envolvidos),
              across(matches("sinistros_|veiculo|acidente|gravidade"), ~ sum(.x, na.rm = TRUE))) |>
    select(data, id_osm, sinistros, qtd_envolvidos, 
           matches("sinistros_|veiculo|acidente|gravidade")) 
  
  if (vizinhos) {
    dado_did <- dado_did |>
      left_join(trechos |>
                  st_drop_geometry() |> 
                  left_join(trechos_complemento) |>
                  left_join(faixa_azul |> distinct(id_osm_vizinho, data_implementacao),
                            by = join_by(id_osm == id_osm_vizinho))) |> 
      anti_join(faixa_azul, by = join_by(id_osm == id_osm_tratado))
  } else {
    dado_did <- dado_did |>
      left_join(trechos |>
                  st_drop_geometry() |> 
                  left_join(trechos_complemento) |>
                  left_join(faixa_azul |> distinct(id_osm, data_implementacao)))
  }
  
  # transformacao da data em valor numerico (na ordem)
  dado_did <- dado_did |> 
    mutate(mes = data) |>
    pivot_longer(c(mes, data_implementacao)) |>
    mutate(value = value |> reorder(value) |> factor() |> as.numeric()) |>
    pivot_wider(names_from = name, values_from = value) |>
    mutate(data_implementacao = replace_na(data_implementacao, 0),
           id_osm = as.numeric(id_osm),
           across(c(tipo_via:elevado, radar_proximo), ~ factor(.x))) |>
    ungroup() |> 
    rename(id = id_osm)
  
  return(dado_did)
  
}

consolida_trecho_did <- function(did_trecho, meses = 1) {
  did_trecho |> 
    ungroup() |> 
    mutate(data = make_date(
      year = year(data), 
      month = ((month(data) - 1) %/% meses + 1) * meses)) |> 
    group_by(id, data) |> 
    summarise(across(matches("sinistros|veiculo|acidente|gravidade|qtd"), sum))
}


dado_agregado_mes <- function(sinistros, match, id_trechos, trechos_agregado, 
                                     tipo_vias = c("trunk", "primary", "secondary")) {
  df <- sinistros |> 
    rename_with(~ str_replace(., "tp_veiculo", "veiculo"), contains("tp_veiculo")) |> 
    filter(year(data) >= 2019 & year(data) <= 2024) |> 
    select(id_sinistro, data, hora, contains("veiculo"), contains("gravidade"), tipo_acidente) |> #quantidade_envolvidos
    mutate(across(matches("veiculo|gravidade"), ~ replace_na(.x, 0)),
           qtd_envolvidos = gravidade_leve + gravidade_grave + gravidade_fatal + gravidade_nao_disponivel) |> 
    
    # dummy de veiculo envolvido
    mutate(across(starts_with("veiculo_"), ~ . > 0, .names = "sinistros_{.col}"),
           sinistros_apenas_moto = 
             sinistros_veiculo_motocicleta & 
             if_all(contains("sinistros_veiculo") & !matches("sinistros_veiculo_motocicleta"), ~ . == FALSE),
           sinistros_moto_carro = 
             sinistros_veiculo_motocicleta &
             sinistros_veiculo_automovel,
           sinistros_moto_outros = 
             sinistros_veiculo_motocicleta &
             if_any(matches("sinistros_veiculo") & !matches("sinistros_veiculo_motocicleta"), ~ . == TRUE),
           sinistros_apenas_outros = !sinistros_apenas_moto) |> 
    
    # dummy de tipo de acidente
    mutate(acidente = 1,
           tipo_acidente = tipo_acidente |> str_to_lower() |> str_replace(" ", "_")) |> 
    pivot_wider(names_from = tipo_acidente, values_from = acidente, values_fill = 0, names_prefix = "acidente_") |> 
    
    mutate(sinistros_choque_apenas_moto    = acidente_choque  & sinistros_apenas_moto,
           sinistros_choque_moto_carro     = acidente_choque  & sinistros_moto_carro,
           sinistros_choque_moto_outros    = acidente_choque  & sinistros_moto_outros,
           sinistros_choque_apenas_outros  = acidente_choque  & sinistros_apenas_outros,
           sinistros_colisao_apenas_moto   = acidente_colisao & sinistros_apenas_moto,
           sinistros_colisao_moto_carro    = acidente_colisao & sinistros_moto_carro,
           sinistros_colisao_moto_outros   = acidente_colisao & sinistros_moto_outros,
           sinistros_colisao_apenas_outros = acidente_colisao & sinistros_apenas_outros) |> 
    
    # dummy de gravidade
    mutate(sinistros_gravidade_fatal = gravidade_fatal > 0,
           sinistros_gravidade_grave = gravidade_grave > 0 & !sinistros_gravidade_fatal,
           sinistros_gravidade_leve  = gravidade_leve  > 0 & !sinistros_gravidade_grave) |> 
    mutate(sinistros_gravidade_fatal_moto = sinistros_gravidade_fatal & sinistros_veiculo_motocicleta,
           sinistros_gravidade_grave_moto = sinistros_gravidade_grave & sinistros_veiculo_motocicleta,
           sinistros_gravidade_leve_moto  = sinistros_gravidade_leve  & sinistros_veiculo_motocicleta) |> 
    
    # dummy de horário
    mutate(sinistros_hora_05_10 = hora >= 5  & hora <= 10,
           sinistros_hora_11_16 = hora >= 11 & hora <= 16,
           sinistros_hora_12_22 = hora >= 12 & hora <= 22,
           sinistros_hora_17_20 = hora >= 17 & hora <= 20,
           sinistros_hora_18_19 = hora >= 18 & hora <= 19) |> 
    mutate(sinistros_hora_05_10_moto = hora >= 5  & hora <= 10 & sinistros_veiculo_motocicleta,
           sinistros_hora_11_16_moto = hora >= 11 & hora <= 16 & sinistros_veiculo_motocicleta,
           sinistros_hora_12_22_moto = hora >= 12 & hora <= 22 & sinistros_veiculo_motocicleta,
           sinistros_hora_17_20_moto = hora >= 17 & hora <= 20 & sinistros_veiculo_motocicleta,
           sinistros_hora_18_19_moto = hora >= 18 & hora <= 19 & sinistros_veiculo_motocicleta) |> 
    mutate(sinistros_hora_17_20_choque         = hora >= 17 & hora <= 20 & acidente_choque,
           sinistros_hora_17_20_colisao        = hora >= 17 & hora <= 20 & acidente_colisao,
           sinistros_hora_17_20_choque_colisao = hora >= 17 & hora <= 20 & (acidente_colisao | acidente_colisao),
           sinistros_hora_17_20_atropelamento  = hora >= 17 & hora <= 20 & acidente_atropelamento,
           sinistros_hora_17_20_outros         = hora >= 17 & hora <= 20 & acidente_outros) |> 
    
    left_join(match |>
                mutate(numero_zero = replace_na(numero_zero, FALSE),
                       golden_match =
                         similaridade > .85 &
                         distancia_geografica < 150 &
                         (match_titulo == TRUE | match_tipo == TRUE) & 
                         numero_zero == FALSE)) |>
    inner_join(id_trechos |> 
                unnest(id_osm) |> 
                select(id_trecho_agregado, id_osm) |> 
                left_join(trechos_agregado |>
                            filter(tipo_via %in% tipo_vias))) |>
    group_by(id_trecho_agregado,
             data = make_date(year = year(data), month = month(data)),
             golden_match = golden_match) |>
    summarize(sinistros = n(),
              qtd_envolvidos = sum(qtd_envolvidos),
              across(matches("sinistros_|veiculo|acidente|gravidade"), sum)) |>
    ungroup() 
  
  zero_cols <- c(
    "sinistros", "qtd_envolvidos",
    grep("^veiculo_|^acidente_|^gravidade_|^sinistros_", names(df), value = TRUE)
  )
  
  fill_list <- as.list(setNames(rep(0, length(zero_cols)), zero_cols))
  
  df <- df |>
    complete(data, id_trecho_agregado, golden_match, fill = fill_list) |> # Painel balanceado
    select(data, id_trecho_agregado, golden_match, qtd_envolvidos, contains("sinistros"),
           contains("veiculo"), contains("acidente"), contains("gravidade"))
  
  return(df)
}

prepara_agregado_did <- function(dado_did, trechos_agregado, filtrar_por = c(golden_match)) {
  dado_did |>
    filter(if_all({{ filtrar_por }}, ~ .x == TRUE)) |>
    group_by(id_trecho_agregado, data) |>
    summarize(sinistros = sum(sinistros),
              qtd_envolvidos = sum(qtd_envolvidos),
              across(matches("sinistros_|veiculo|acidente|gravidade"), ~ sum(.x, na.rm = TRUE))) |>
    select(data, id_trecho_agregado, sinistros, qtd_envolvidos, 
           matches("sinistros_|veiculo|acidente|gravidade")) |>
    left_join(trechos_agregado) |>
    
    #trasformacao da data em valor numerico (na ordem)
    mutate(mes = data) |>
    pivot_longer(c(mes, data_implementacao)) |>
    mutate(value = value |> reorder(value) |> factor() |> as.numeric()) |>
    pivot_wider(names_from = name, values_from = value) |>
    mutate(data_implementacao = replace_na(data_implementacao, 0),
           # id_trecho_agregado = as.numeric(id_trecho_agregado),
           across(c(mao_unica:radar_proximo), ~ factor(.x))) |>
    ungroup() |> 
    rename(id = id_trecho_agregado)
}


dado_logradouro_mes <- function(sinistros, match, id_logradouros){
  df <- sinistros |> 
    rename_with(~ str_replace(., "tp_veiculo", "veiculo"), contains("tp_veiculo")) |> 
    filter(year(data) >= 2019 & year(data) <= 2024) |> 
    select(id_sinistro, data, hora, contains("veiculo"), contains("gravidade"), tipo_acidente) |> #quantidade_envolvidos
    mutate(across(matches("veiculo|gravidade"), ~ replace_na(.x, 0)),
           qtd_envolvidos = gravidade_leve + gravidade_grave + gravidade_fatal + gravidade_nao_disponivel) |> 
    
    # dummy de veiculo envolvido
    mutate(across(starts_with("veiculo_"), ~ . > 0, .names = "sinistros_{.col}"),
           sinistros_apenas_moto = 
             sinistros_veiculo_motocicleta & 
             if_all(contains("sinistros_veiculo") & !matches("sinistros_veiculo_motocicleta"), ~ . == FALSE),
           sinistros_moto_carro = 
             sinistros_veiculo_motocicleta &
             sinistros_veiculo_automovel,
           sinistros_moto_outros = 
             sinistros_veiculo_motocicleta &
             if_any(matches("sinistros_veiculo") & !matches("sinistros_veiculo_motocicleta"), ~ . == TRUE),
           sinistros_apenas_outros = !sinistros_apenas_moto) |> 
    
    # dummy de tipo de acidente
    mutate(acidente = 1,
           tipo_acidente = tipo_acidente |> str_to_lower() |> str_replace(" ", "_")) |> 
    pivot_wider(names_from = tipo_acidente, values_from = acidente, values_fill = 0, names_prefix = "acidente_") |> 
    
    mutate(sinistros_choque_apenas_moto  = acidente_choque  & sinistros_apenas_moto,
           sinistros_choque_moto_carro   = acidente_choque  & sinistros_moto_carro,
           sinistros_choque_moto_outros  = acidente_choque  & sinistros_moto_outros,
           sinistros_colisao_apenas_moto = acidente_colisao & sinistros_apenas_moto,
           sinistros_colisao_moto_carro  = acidente_colisao & sinistros_moto_carro,
           sinistros_colisao_moto_outros = acidente_colisao & sinistros_moto_outros) |> 
    
    # dummy de gravidade
    mutate(sinistros_gravidade_fatal = gravidade_fatal > 0,
           sinistros_gravidade_grave = gravidade_grave > 0 & !sinistros_gravidade_fatal,
           sinistros_gravidade_leve  = gravidade_leve  > 0 & !sinistros_gravidade_grave) |> 
    mutate(sinistros_gravidade_fatal_moto = sinistros_gravidade_fatal & sinistros_veiculo_motocicleta,
           sinistros_gravidade_grave_moto = sinistros_gravidade_grave & sinistros_veiculo_motocicleta,
           sinistros_gravidade_leve_moto  = sinistros_gravidade_leve  & sinistros_veiculo_motocicleta) |> 
    
    # dummy de horário
    mutate(sinistros_hora_05_10 = hora >= 5  & hora <= 10,
           sinistros_hora_11_16 = hora >= 11 & hora <= 16,
           sinistros_hora_12_22 = hora >= 12 & hora <= 22,
           sinistros_hora_17_20 = hora >= 17 & hora <= 20,
           sinistros_hora_18_19 = hora >= 18 & hora <= 19) |> 
    mutate(sinistros_hora_05_10_moto = hora >= 5  & hora <= 10 & sinistros_veiculo_motocicleta,
           sinistros_hora_11_16_moto = hora >= 11 & hora <= 16 & sinistros_veiculo_motocicleta,
           sinistros_hora_12_22_moto = hora >= 12 & hora <= 22 & sinistros_veiculo_motocicleta,
           sinistros_hora_17_20_moto = hora >= 17 & hora <= 20 & sinistros_veiculo_motocicleta,
           sinistros_hora_18_19_moto = hora >= 18 & hora <= 19 & sinistros_veiculo_motocicleta) |> 
    mutate(sinistros_hora_17_20_choque         = hora >= 17 & hora <= 20 & acidente_choque,
           sinistros_hora_17_20_colisao        = hora >= 17 & hora <= 20 & acidente_colisao,
           sinistros_hora_17_20_choque_colisao = hora >= 17 & hora <= 20 & (acidente_colisao | acidente_colisao),
           sinistros_hora_17_20_atropelamento  = hora >= 17 & hora <= 20 & acidente_atropelamento,
           sinistros_hora_17_20_outros         = hora >= 17 & hora <= 20 & acidente_outros) |>
    
    left_join(match |>
                mutate(golden_match =
                         similaridade > .75 &
                         distancia_geografica < 100 &
                         match_titulo == TRUE &
                         match_tipo == TRUE)) |>
    tibble() |>
    left_join(id_logradouros |>
                unnest(trechos) |>
                select(id_logradouro, id_osm = trechos)) |>
    group_by(id_logradouro,
             data = make_date(year = year(data), month = month(data)),
             golden_match = golden_match) |>
    summarize(sinistros = n(),
              qtd_envolvidos = sum(qtd_envolvidos),
              across(matches("sinistros_|veiculo|acidente|gravidade"), sum)) |>
    ungroup() 
  
  zero_cols <- c(
    "sinistros", "qtd_envolvidos",
    grep("^veiculo_|^acidente_|^gravidade_|^sinistros_", names(df), value = TRUE)
  )
  
  fill_list <- as.list(setNames(rep(0, length(zero_cols)), zero_cols))
  
  df <- df |>
    complete(data, id_logradouro, golden_match, fill = fill_list) |> # Painel balanceado
    select(data, id_logradouro, golden_match, qtd_envolvidos, contains("sinistros"),
           contains("veiculo"), contains("acidente"), contains("gravidade"))
  
  return(df)
}

prepara_logradouro_did <- function(dado_did, logradouros, filtrar_por = c(golden_match)) {
  dado_did |>
    filter(if_all({{ filtrar_por }}, ~ .x == TRUE)) |>
    group_by(id_logradouro, data) |>
    summarize(sinistros = sum(sinistros),
              qtd_envolvidos = sum(qtd_envolvidos),
              across(matches("sinistros_|veiculo|acidente|gravidade"), ~ sum(.x, na.rm = TRUE))) |>
    select(data, id_logradouro, sinistros, qtd_envolvidos, 
           matches("sinistros_|veiculo|acidente|gravidade")) |>
    left_join(logradouros) |>
    
    #trasformacao da data em valor numerico (na ordem)
    mutate(mes = data) |>
    pivot_longer(c(mes, data_implementacao)) |>
    mutate(value = value |> reorder(value) |> factor() |> as.numeric()) |>
    pivot_wider(names_from = name, values_from = value) |>
    mutate(data_implementacao = replace_na(data_implementacao, 0),
           id_logradouro = as.numeric(id_logradouro),
           across(c(mao_unica:radar_proximo), ~ factor(.x))) |>
    ungroup() |> 
    rename(id = id_logradouro)
}


definir_cohort <- function(dado, faixa_azul = NULL, logradouros = NULL, agregados = NULL, trechos = NULL, 
                           por_logradouro = FALSE, por_agregado = FALSE){
  cohort <- dado |> 
    filter(year(data) <= 2024,
           mes == data_implementacao) |> 
    distinct(data, data_implementacao) |> 
    arrange(data_implementacao)
  
  if (por_logradouro){
    logradouros |> 
      group_by(data = data_implementacao) |> 
      summarize(n_trechos = sum(trechos),
                n_logradouros = n(),
                comprimento = (sum(comprimento) / 1000) |> round()) |> 
      right_join(cohort)
  } else if (por_agregado) {
    agregados |> 
      group_by(data = data_implementacao) |> 
      summarize(n_trechos = sum(trechos),
                n_logradouros = n(),
                comprimento = (sum(comprimento) / 1000) |> round()) |> 
      right_join(cohort)
  } else {
    cohort |> 
      left_join(faixa_azul |> 
                  left_join(trechos |>
                              st_drop_geometry() |> 
                              select(id_osm, comprimento)) |> 
                  group_by(data_implementacao) |> 
                  summarize(n_trechos = n(),
                            comprimento = (sum(comprimento) / 1000) |> round()) |> 
                  rename(data = data_implementacao)) |> 
      left_join(logradouros |> 
                  group_by(data= data_implementacao) |> 
                  summarize(n_logradouros = n())) 
  }
}

definir_cohort_vizinho <- function(dado, faixa_azul = NULL, logradouros = NULL, trechos = NULL, por_logradouro = FALSE){
  cohort <- dado |> 
    filter(year(data) <= 2024,
           mes == data_implementacao) |> 
    distinct(data, data_implementacao) |> 
    arrange(data_implementacao)
  
  if (por_logradouro){
    logradouros |> 
      group_by(data = data_implementacao) |> 
      summarize(n_trechos = sum(trechos),
                n_logradouros = n(),
                comprimento = (sum(comprimento) / 1000) |> round()) |> 
      right_join(cohort)
  } else {
    cohort |> 
      left_join(faixa_azul |> 
                  select(id_osm = id_osm_vizinho, data_implementacao) |> 
                  distinct() |> 
                  left_join(trechos |>
                              st_drop_geometry() |> 
                              select(id_osm, comprimento)) |> 
                  group_by(data_implementacao) |> 
                  summarize(n_trechos = n(),
                            comprimento = (sum(comprimento) / 1000) |> round()) |> 
                  rename(data = data_implementacao)) |> 
      left_join(logradouros |> 
                  group_by(data= data_implementacao) |> 
                  summarize(n_logradouros = n())) 
  }
}