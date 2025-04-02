library(tidyverse)
library(sf)
library(did)

dado_trecho_mes <- function(sinistros, match, trechos){
  df <- sinistros |> 
    rename_with(~ str_replace(., "tp_veiculo", "veiculo"), contains("tp_veiculo")) |> 
    filter(year(data) >= 2019 & year(data) <= 2024) |> 
    select(id_sinistro, data, contains("veiculo"), contains("gravidade"), tipo_acidente) |> #quantidade_envolvidos
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
  
    left_join(match |>
                mutate(golden_match =
                         similaridade > .85 &
                         distancia_geografica < 150 &
                         (match_titulo == TRUE | match_tipo == TRUE) & 
                         numero_zero == FALSE)) |>
    semi_join(trechos |>
                st_drop_geometry() |>
                filter(tipo_via %in% c("trunk", "primary", "secondary")), by = join_by(id_osm)) |>
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

prepara_trecho_did <- function(dado_did, trechos, trechos_complemento, faixa_azul, filtrar_por = c(golden_match)){
  dado_did |> 
    filter(if_all({{ filtrar_por }}, ~ .x == TRUE)) |> 
    group_by(id_osm, data) |>
    summarize(sinistros = sum(sinistros),
              qtd_envolvidos = sum(qtd_envolvidos),
              across(matches("sinistros_|veiculo|acidente|gravidade"), ~ sum(.x, na.rm = TRUE))) |>
    select(data, id_osm, sinistros, qtd_envolvidos, 
           matches("sinistros_|veiculo|acidente|gravidade")) |>
    left_join(trechos |>
                st_drop_geometry() |> 
                left_join(trechos_complemento) |>
                left_join(faixa_azul |> distinct(id_osm, data_implementacao))) |>

    #trasformacao da data em valor numerico (na ordem)
    mutate(mes = data) |>
    pivot_longer(c(mes, data_implementacao)) |>
    mutate(value = value |> reorder(value) |> factor() |> as.numeric()) |>
    pivot_wider(names_from = name, values_from = value) |>
    mutate(data_implementacao = replace_na(data_implementacao, 0),
           id_osm = as.numeric(id_osm),
           across(c(tipo_via:elevado, radar_proximo), ~ factor(.x))) |>
    ungroup() |> 
    rename(id = id_osm)
  
}

dado_logradouro_mes <- function(sinistros, match, id_logradouros){
  df <- sinistros |> 
    rename_with(~ str_replace(., "tp_veiculo", "veiculo"), contains("tp_veiculo")) |> 
    filter(year(data) >= 2019 & year(data) <= 2024) |> 
    select(id_sinistro, data, contains("veiculo"), contains("gravidade"), tipo_acidente) |> #quantidade_envolvidos
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


definir_cohort <- function(dado, faixa_azul = NULL, logradouros = NULL, trechos = NULL, por_logradouro = FALSE){
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


### Rodar did

gerar_tabela_cohort <- function(fit, cohort, controle, titulo, filename){
  if (controle) {
    titulo <- paste0(titulo, ", com controles")
    filename <- paste0(filename, "-tabela-controles")
  } else {
    titulo <- paste0(titulo, ", sem controles")
    filename <- paste0(filename, "-tabela-simples")
  }
  
  t <- aggte(fit, type = "group", na.rm = T) |> 
    (\(result) tibble(egt = result$egt,
                      att = result$att.egt,
                      se = result$se.egt,
                      crit_val = result$crit.val.egt))(result = _) |>
    mutate(ci_low = att - crit_val * se,
           ci_high = att + crit_val * se) |>
    left_join(cohort, by = join_by(egt == data_implementacao)) |> 
    mutate(significante = ifelse(ci_low < 0 & ci_high > 0, "", "*")) |> 
    select(cohort = data, n_trechos, n_logradouros, comprimento, att, ci_low, ci_high, significante) |> 
    gt() |> 
    fmt_number(columns = c(ci_low, ci_high, att)) |> 
    tab_spanner("Intervalo 95%", columns = 4:5) |> 
    cols_label(cohort = "Coorte",
               att = "ATE",
               n_trechos = "Trechos",
               n_logradouros = "Logradouros",
               comprimento = "Comprimento (km)",
               significante = "*",
               ci_low = "[",
               ci_high = "]") |> 
    tab_header(title = paste("Tabela -", titulo)) 
  t |> 
    gtsave(filename |> paste0(".png"), 
           path = "output/did/")
}

preparar_grafico_did <- function(fit, controle){
  aggte(fit, type = "dynamic", na.rm = T) |> 
    (\(result) tibble(egt = result$egt,
                      att = result$att.egt,
                      se = result$se.egt,
                      crit_val = result$crit.val.egt))(result = _) |>
    mutate(ci_low = att - crit_val * se,
           ci_high = att + crit_val * se,
           controle = controle |> as_factor())
}



fit_did <- function(df, titulo, filename, cohorts, por_km = FALSE, yname = "sinistros",
                    clustervars = c("id"), control_group = "notyettreated", idname = "id",
                    formula = ~ tipo_via + faixas + limite_velocidade + amenidades + intersec + radar_proximo){
  if (por_km == TRUE){
    df <- df |> 
      mutate(across(c(contains("sinistro"), contains("gravidade"), contains("acidente"), 
                      qtd_envolvidos, intersec, amenidades), ~ .x * 1000 / comprimento))
    titulo <- paste(titulo, "- por km")
    filename <- paste0(filename, "-km")
  } else {
    titulo <- paste(titulo, "- total")
    filename <- paste0(filename, "-total")
  }
  
  fit <- att_gt(
    yname = yname,
    tname = "mes",
    idname = idname,
    gname = "data_implementacao",
    data = df,
    clustervars = clustervars,
    control_group = control_group,
    xformla = ~ 1)
  fit.c <- att_gt(
    yname = yname,
    tname = "mes",
    idname = idname,
    gname = "data_implementacao",
    data = df,
    clustervars = clustervars,
    control_group = control_group,
    xformla = formula)
  
  gerar_tabela_cohort(fit, cohorts, controle = FALSE, titulo = titulo, filename = filename)
  gerar_tabela_cohort(fit.c, cohorts, controle = TRUE, titulo = titulo, filename = filename)
  
  
  out <- aggte(fit.c, type = "simple", na.rm = TRUE)
  ATT <- out$overall.att
  ci_low = ATT - out$overall.se * 1.96
  ci_high = ATT + out$overall.se * 1.96
  significance <- if(ci_low < 0 & ci_high > 0){""}else{"*"}
  
  
  # Gerar gráfico
  (bind_rows(preparar_grafico_did(fit, controle = "Não"),
             preparar_grafico_did(fit.c, controle = "Sim")) |> 
      filter(abs(egt) <= 12) |> 
      ggplot(aes(x = factor(egt), colour = controle, y = att)) +
      geom_pointrange(aes(ymin = ci_low, ymax = ci_high), position = position_dodge(width = .5), fatten = 1) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_vline(xintercept = factor(0), alpha = .05, lwd = 3) +
      theme_minimal() +
      labs(x = "Meses", y = "Efeito",
           title = titulo, colour = "Controles",
           subtitle = str_glue("Overall ATT com controle: {round(ATT,3)}{significance} ({round(ci_low,2)}; {round(ci_high, 2)})"))) |> 
    ggsave(filename = filename |> paste0("-plot.png"), 
           path = "output/did/",
           plot = _,
           width = 10, height = 7.5, dpi = 300)
  
 (fit.c |> 
    ggdid(xgap = 4, ncol = 3, title = paste(titulo, "- grupos"))) |> 
    ggsave(filename = filename |> paste0("-plot-groups.png"), 
           path = "output/did/",
           plot = _,
           width = 18, height = 9, dpi = 300)
}


fit_did_all <- function(df, titulo, filename, cohorts, yname = "sinistros",
                        clustervars = c("id"), control_group = "notyettreated", idname = "id",
                        formula = ~ tipo_via + faixas + limite_velocidade + amenidades + intersec + radar_proximo,
                        formula_km = ~ tipo_via + faixas + limite_velocidade + amenidades + intersec + radar_proximo + comprimento) {
  fit_did(
    df = df,
    cohorts = cohorts,
    yname = yname,
    titulo = titulo,
    filename = filename,
    clustervars = clustervars,
    control_group = control_group,
    idname = idname,
    formula = formula,
    por_km = TRUE)
  
  fit_did(
    df = df,
    cohorts = cohorts,
    yname = yname,
    titulo = titulo,
    filename = filename,
    clustervars = clustervars,
    control_group = control_group,
    idname = idname,
    formula = formula_km,
    por_km = FALSE)
}


did_het_tempo_tratado <- function(
    df, titulo, filename, cohorts, yname = "sinistros",
    clustervars = c("id"), control_group = "notyettreated", idname = "id",
    formula = ~ tipo_via + faixas + limite_velocidade + amenidades + intersec + radar_proximo,
    formula_km = ~ tipo_via + faixas + limite_velocidade + amenidades + intersec + radar_proximo + comprimento,
    meses = 12, mais_que = TRUE) {
  
  max_mes = interval(df$data |> min() |> as_date(), df$data |> max() |> as_date()) %/% months(1) + 1
  logradouros <- df |>
    filter(data_implementacao < max_mes - meses,
           data_implementacao != 0) |>
    distinct(logradouro) |>
    pull(logradouro)
  
  if (mais_que) {
    df.reg <- df |>
      filter(logradouro %in% logradouros | data_implementacao == 0)
  } else {
    df.reg <- df |>
      filter(!(logradouro %in% logradouros)) 
  }
  
  fit_did_all(df.reg, titulo, filename, cohorts, yname, clustervars, control_group, idname, formula, formula_km)
}

did_het_num_faixas <- function(
    df, titulo, filename, cohorts, yname = "sinistros",
    clustervars = c("id"), control_group = "notyettreated", idname = "id",
    formula = ~ tipo_via + limite_velocidade + amenidades + intersec + radar_proximo,
    formula_km = ~ tipo_via + limite_velocidade + amenidades + intersec + radar_proximo + comprimento,
    num_faixas = 2, mais_que = TRUE) {
  
  if (mais_que) {
    df.reg <- df |>
      mutate(faixas = as.integer(as.character(faixas))) |>
      filter(faixas > num_faixas)
  } else {
    df.reg <- df |>
      mutate(faixas = as.integer(as.character(faixas))) |>
      filter(faixas <= num_faixas)
  }
  
  fit_did_all(df.reg, titulo, filename, cohorts, yname, clustervars, control_group, idname, formula, formula_km)
}


did_het_tipo_via <- function(
    df, titulo, filename, cohorts, yname = "sinistros",
    clustervars = c("id"), control_group = "notyettreated", idname = "id",
    formula = ~ faixas + limite_velocidade + amenidades + intersec + radar_proximo,
    formula_km = ~ faixas + limite_velocidade + amenidades + intersec + radar_proximo + comprimento,
    tipo_via = "primary") {
  
  df.reg <- df |>
    filter(tipo_via == tipo_via)
  
  fit_did_all(df.reg, titulo, filename, cohorts, yname, clustervars, control_group, idname, formula, formula_km)
}


did_het_vel_maxima <- function(
    df, titulo, filename, cohorts, yname = "sinistros",
    clustervars = c("id"), control_group = "notyettreated", idname = "id",
    formula = ~ tipo_via + faixas + amenidades + intersec + radar_proximo,
    formula_km = ~ tipo_via + faixas + amenidades + intersec + radar_proximo + comprimento,
    vel_maxima = 40, mais_que = TRUE) {
  
  if (mais_que) {
    df.reg <- df |>
      mutate(limite_velocidade = as.integer(as.character(limite_velocidade))) |>
      filter(limite_velocidade > vel_maxima)
  } else {
    df.reg <- df |>
      mutate(limite_velocidade = as.integer(as.character(limite_velocidade))) |>
      filter(limite_velocidade <= vel_maxima)
  }
  
  fit_did_all(df.reg, titulo, filename, cohorts, yname, clustervars, control_group, idname, formula, formula_km)
}


