library(tidyverse)
library(sf)
library(MatchIt)


dado_trecho_mes <- function(sinistros, match, trechos, tipo_vias = c("trunk", "primary", "secondary")) {
  df <- sinistros |> 
    rename_with(~ str_replace(., "tp_veiculo", "veiculo"), contains("tp_veiculo")) |> 
    filter(year(data) >= 2019) |> 
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
    
    left_join(match) |>
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


dado_agregado_mes <- function(sinistros, match, id_trechos, trechos_agregado, tipo_vias = c("trunk", "primary", "secondary"),
                              tipo_join = "padrao") {
  df <- sinistros |> 
    rename_with(~ str_replace(., "tp_veiculo", "veiculo"), contains("tp_veiculo")) |> 
    filter(year(data) >= 2019) |> 
    select(id_sinistro, data, hora, contains("veiculo"), contains("gravidade"), tipo_acidente) |> #quantidade_envolvidos
    mutate(across(matches("veiculo|gravidade"), ~ replace_na(.x, 0)),
           qtd_envolvidos = gravidade_leve + gravidade_grave + gravidade_fatal + gravidade_nao_disponivel,
           qtd_feridos = gravidade_leve + gravidade_grave + gravidade_fatal) |> 
    
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
    
    mutate(sinistros_feridos = qtd_feridos > 0,
           sinistros_feridos_moto = sinistros_feridos & sinistros_veiculo_motocicleta) |> 
    
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
    
    left_join(match) |>
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
    ungroup() |> 
    mutate(taxa_fatalidade = if_else(sinistros != 0, sinistros_gravidade_fatal / sinistros, 0),
           taxa_fatalidade_moto = if_else(sinistros != 0, sinistros_gravidade_fatal_moto / sinistros, 0),
           taxa_feridos = if_else(sinistros != 0, sinistros_feridos / sinistros, 0),
           taxa_feridos_moto = if_else(sinistros != 0, sinistros_feridos_moto / sinistros, 0))
  
  
  if (tipo_join == "completo") {
    df <- df |>
      right_join(trechos_agregado |>
                   ungroup() |>
                   select(id_trecho_agregado)) |>
      mutate(golden_match = replace_na(golden_match, TRUE),
             data = replace_na(data, make_date(year = 2019)))
  }
  
  
  zero_cols <- c(
    "sinistros", "qtd_envolvidos",
    grep("^veiculo_|^acidente_|^gravidade_|^sinistros_|^taxa_", names(df), value = TRUE)
  )
  
  fill_list <- as.list(setNames(rep(0, length(zero_cols)), zero_cols))
  
  df <- df |>
    complete(data, id_trecho_agregado, golden_match, fill = fill_list) |> # Painel balanceado
    select(data, id_trecho_agregado, golden_match, qtd_envolvidos, 
           matches("sinistros|veiculo|acidente|gravidade|taxa"))
  
  return(df)
}

prepara_agregado_did <- function(dado_did, trechos_agregado, filtrar_por = c(golden_match)) {
  dado_did |>
    filter(if_all({{ filtrar_por }}, ~ .x == TRUE)) |>
    group_by(id_trecho_agregado, data) |>
    summarize(sinistros = sum(sinistros),
              qtd_envolvidos = sum(qtd_envolvidos),
              across(matches("sinistros_|veiculo|acidente|gravidade|taxa"), ~ sum(.x, na.rm = TRUE))) |>
    select(data, id_trecho_agregado, sinistros, qtd_envolvidos, 
           matches("sinistros_|veiculo|acidente|gravidade|taxa")) |>
    left_join(trechos_agregado) |>
    
    #trasformacao da data em valor numerico (na ordem)
    mutate(mes = data) |>
    pivot_longer(c(mes, data_implementacao)) |>
    mutate(value = value |> reorder(value) |> factor() |> as.numeric()) |>
    pivot_wider(names_from = name, values_from = value) |>
    mutate(data_implementacao = replace_na(data_implementacao, 0),
           comprimento_sqrt = sqrt(comprimento),
           comprimento_cbrt = comprimento^(1/3),
           # id_trecho_agregado = as.numeric(id_trecho_agregado),
           across(c(mao_unica:radar_proximo), ~ factor(.x))) |>
    ungroup() |> 
    rename(id = id_trecho_agregado)
}


dado_logradouro_mes <- function(sinistros, match, id_logradouros){
  df <- sinistros |> 
    rename_with(~ str_replace(., "tp_veiculo", "veiculo"), contains("tp_veiculo")) |> 
    filter(year(data) >= 2019) |> 
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
    
    left_join(match) |>
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
    filter(mes == data_implementacao) |> 
    distinct(data, data_implementacao) |> 
    arrange(data_implementacao)
  
  if (por_logradouro) {
    logradouros |> 
      group_by(data = data_implementacao) |> 
      summarize(n_trechos = sum(trechos),
                n_logradouros = n(),
                comprimento = (sum(comprimento) / 1000) |> round()) |> 
      right_join(cohort) |> 
      mutate(n_agregados = "-")
  } else if (por_agregado) {
    agregados |> 
      group_by(data = data_implementacao) |> 
      summarize(n_trechos = sum(trechos),
                n_agregados = n(),
                comprimento = (sum(comprimento) / 1000) |> round()) |> 
      left_join(logradouros |> 
                  group_by(data= data_implementacao) |> 
                  summarize(n_logradouros = n())) |> 
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
                  summarize(n_logradouros = n())) |> 
      mutate(n_agregados = "-")
  }
}

definir_cohort_vizinho <- function(dado, faixa_azul = NULL, logradouros = NULL, trechos = NULL, por_logradouro = FALSE){
  cohort <- dado |> 
    filter(mes == data_implementacao) |> 
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


consolida_trecho_did <- function(did_trecho, meses = 1) {
  did_trecho |> 
    ungroup() |> 
    mutate(
      data = make_date(
        year = year(data), 
        month = ((month(data) - 1) %/% meses + 1) * meses),
      data_implementacao = ifelse(data_implementacao == 0, 0, ((data_implementacao - 1) %/% meses + 1)),
      mes = (mes - 1) %/% meses + 1,
    ) |> 
    group_by(id, data) |> 
    summarise(across(matches("sinistros|veiculo|acidente|gravidade|qtd"), sum),
              across(!matches("sinistros|veiculo|acidente|gravidade|qtd"), last)) |> 
    ungroup()
}


psm_agregados <- function(agregados, id_agregados, sinistros, match, file, 
                          inclui_medias = FALSE, filtro_min = NULL) {
  
  obitos <- id_agregados |> 
    unnest(id_osm) |> 
    left_join(match |> 
                select(id_sinistro, id_osm, golden_match)) |> 
    left_join(sinistros |> 
                filter(year(data) >= 2019,
                       year(data) <= 2021)) |> 
    filter(golden_match) |> 
    mutate(obito = !is.na(gravidade_fatal) & gravidade_fatal > 0) |> 
    group_by(id_trecho_agregado) |> 
    summarise(obitos = sum(obito),
              sinistros = n())
  
  
  # Preparação da base para o PSM ----
  df <- agregados |>
    ungroup() |> 
    left_join(obitos) |> 
    mutate(faixa_azul = as.integer(!is.na(data_implementacao)),
           across(c(faixas), ~ round(.x)),
           limite_velocidade = (round(limite_velocidade / 10) * 10),
           across(c(amenidades, intersec), ~ .x / comprimento),
           across(c(faixas, limite_velocidade, mao_unica), ~ ifelse(is.na(.x), "NA", .x)),
           across(c(obitos, sinistros), ~ replace_na(.x, 0)))
  
  # Propensity score matching ----
  if (inclui_medias) {
    PSM <- df |>
      matchit(faixa_azul ~ trechos + comprimento + faixas + limite_velocidade + amenidades + 
                intersec + tipo_via + radar_proximo + obitos + sinistros,
              data = _,
              method = "nearest",
              distance = "glm", link = "logit")
  } else {
    PSM <- df |>
      matchit(faixa_azul ~ trechos + comprimento + faixas + limite_velocidade + amenidades + 
                intersec + tipo_via + radar_proximo,
              data = _,
              method = "nearest",
              distance = "glm", link = "logit")
  }
  
  # Algumas descritivas do PSM ----
  # PSM |> 
  #   summary() |> 
  #   plot()
  # 
  # plot(PSM, type = "density", interactive = FALSE,
  #      which.xs = ~ trechos + comprimento + limite_velocidade + amenidades + 
  #        intersec + tipo_via + radar_proximo) 
  
  # Agrupando resultados do PSM em uma tabela ----
  resultado <- df |> 
    select(id_trecho_agregado, faixa_azul) |> 
    
    # Quando encontra um vizinho, resultado_match = 1, caso contrário 0 (deve ser removido)
    mutate(resultado_match = PSM$weights,
           propensity_score = PSM$distance)
  
  if (!is.null(filtro_min)) {
    resultado <- resultado |> 
      mutate(resultado_match = if_else(propensity_score <= 0.05, 0, resultado_match))
  }
  
  # Gráfico com o as distribuições de propensity score ----
  gg <- bind_rows(
    resultado |> filter(resultado_match == 1) |> mutate(matched = "b) Pós PSM"),
    resultado |> mutate(matched = "a) Pré PSM")
  ) |>
    ggplot() +
    geom_density(aes(x = propensity_score, fill = factor(faixa_azul)), alpha = .5) +
    facet_wrap(~ matched) +
    theme_minimal() +
    labs(x = "Propensity Score", y = NULL) +
    scale_fill_manual("Grupo",
                      labels = c("Controle", "Tratamento"),
                      values = c("darkblue", "darkred"))
  
  ggsave(paste0("output/PSM/", file, ".pdf"), gg, width = 7, height = 4)
  
  # Resultado do logit ----
  # PSM$model |> summary()
  
  return(resultado)
}


prepara_psm_agregados_did <- function(dado, psm, file) {
  dado <- dado |> 
    semi_join(psm |> 
                filter(resultado_match == 1), 
              by = join_by(id == id_trecho_agregado))
  
  dado |> 
    mutate(id = as.integer(as_factor(id))) |> 
    write_csv(file = paste0("dados_tratados/", file, "_psm.csv"))
  
  dado |> 
    mutate(id = as.integer(as_factor(id)),
           across(c(matches("sinistro|gravidade|acidente"),  qtd_envolvidos, intersec, amenidades), 
                  ~ .x * 1000 / comprimento)) |> 
    write_csv(file = paste0("dados_tratados/", file, "-km_psm.csv"))
  
  return(dado)
}


dado_sinistro_mes <- function() {
  
}


# pareamento_manual <- function(did) {
#   did |> 
#     mutate(faixas2 = round(faixas),
#            limite_velocidade2 = round(limite_velocidade/10)*10) |> 
#     filter(faixas2 %in% 3:5,
#            limite_velocidade2 %in% 40:60,
#            tipo_via %in% c("primary", "trunk"))
# }