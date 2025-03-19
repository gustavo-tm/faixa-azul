library(tidyverse)
library(sf)
library(did)

dado_trecho_mes <- function(sinistros, match, trechos){
  sinistros |> 
    filter(year(data) > 2018) |> 
    select(id_sinistro, data, motocicletas, contains("gravidade"), tipo_acidente) |> #quantidade_envolvidos
    mutate(across(contains("gravidade"), ~ replace_na(.x, 0)),
           qtd_envolvidos = gravidade_leve + gravidade_grave + gravidade_fatal + gravidade_nao_disponivel,
           acidente = 1,
           tipo_acidente = tipo_acidente |> str_to_lower() |> str_replace(" ", "_")) |> 
    pivot_wider(names_from = tipo_acidente, values_from = acidente, values_fill = 0, names_prefix = "acidente_") |> 
    mutate(sinistro_fatal = gravidade_fatal > 0,
           sinistro_grave = gravidade_grave > 0 & !sinistro_fatal,
           sinistro_leve  = gravidade_leve  > 0 & !sinistro_grave) |> 
    left_join(match |> 
                mutate(golden_match =
                         similaridade > .75 &
                         distancia_geografica < 100 &
                         match_titulo == TRUE &
                         match_tipo == TRUE)) |>
    semi_join(trechos |> 
                st_drop_geometry() |> 
                filter(tipo_via %in% c("trunk", "primary", "secondary")), by = join_by(id_osm)) |>
    group_by(id_osm,
             data = make_date(year = year(data), month = month(data)),
             golden_match = golden_match, 
             motocicleta_envolvida = motocicletas > 0) |>
    summarize(sinistros = n(),
              qtd_envolvidos = sum(qtd_envolvidos),
              across(contains("gravidade"), sum),
              across(contains("acidente"), sum),
              across(contains("sinistro_"), sum)) |>
    ungroup() |>
    complete(data, id_osm, golden_match, motocicleta_envolvida, 
             fill = list(sinistros = 0, qtd_envolvidos = 0, 
                         gravidade_leve = 0, gravidade_grave = 0, gravidade_fatal = 0, gravidade_nao_disponivel = 0,
                         sinistro_fatal = 0, sinistro_grave = 0, sinistro_leve = 0)) # Painel balanceado
}

prepara_trecho_did <- function(dado_did, trechos, trechos_complemento, faixa_azul, filtrar_por = c(golden_match, motocicleta_envolvida)){
  dado_did |> 
    filter(if_all({{ filtrar_por }}, ~ .x == TRUE)) |> 
    group_by(id_osm, data) |>
    summarize(sinistros = sum(sinistros),
              qtd_envolvidos = sum(qtd_envolvidos),
              across(contains("gravidade"), ~ sum(.x, na.rm = TRUE)),
              across(contains("acidente"), ~ sum(.x, na.rm = TRUE)),
              across(contains("sinistro_"), ~ sum(.x, na.rm = TRUE))) |>
    select(data, id_osm, sinistros, qtd_envolvidos, 
           contains("gravidade"), contains("acidente"), contains("sinistro_")) |>
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
  sinistros |> 
    filter(year(data) > 2018) |> 
    select(id_sinistro, data, motocicletas, contains("gravidade"), tipo_acidente) |> #quantidade_envolvidos
    mutate(across(contains("gravidade"), ~ replace_na(.x, 0)),
           qtd_envolvidos = gravidade_leve + gravidade_grave + gravidade_fatal + gravidade_nao_disponivel,
           acidente = 1,
           tipo_acidente = tipo_acidente |> str_to_lower() |> str_replace(" ", "_")) |> 
    pivot_wider(names_from = tipo_acidente, values_from = acidente, values_fill = 0, names_prefix = "acidente_") |> 
    mutate(sinistro_fatal = gravidade_fatal > 0,
           sinistro_grave = gravidade_grave > 0 & !sinistro_fatal,
           sinistro_leve  = gravidade_leve  > 0 & !sinistro_grave) |> 
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
             golden_match = golden_match, 
             motocicleta_envolvida = motocicletas > 0) |>
    summarize(sinistros = n(),
              qtd_envolvidos = sum(qtd_envolvidos),
              across(contains("gravidade"), sum),
              across(contains("acidente"), sum),
              across(contains("sinistro_"), sum)) |>
    ungroup() |> 
    complete(data, id_logradouro, golden_match, motocicleta_envolvida, 
             fill = list(sinistros = 0, qtd_envolvidos = 0, 
                         gravidade_leve = 0, gravidade_grave = 0, gravidade_fatal = 0, gravidade_nao_disponivel = 0,
                         sinistro_fatal = 0, sinistro_grave = 0, sinistro_leve = 0)) # Painel balanceado
}

prepara_logradouro_did <- function(dado_did, logradouros, filtrar_por = c(golden_match, motocicleta_envolvida)){
  dado_did |>
    filter(if_all({{ filtrar_por }}, ~ .x == TRUE)) |>
    group_by(id_logradouro, data) |>
    summarize(sinistros = sum(sinistros),
              qtd_envolvidos = sum(qtd_envolvidos),
              across(contains("gravidade"), ~ sum(.x, na.rm = TRUE)),
              across(contains("acidente"), ~ sum(.x, na.rm = TRUE)),
              across(contains("sinistro_"), ~ sum(.x, na.rm = TRUE))) |>
    select(data, id_logradouro, sinistros, qtd_envolvidos, 
           contains("gravidade"), contains("acidente"), contains("sinistro_")) |>
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

# df <- dado_trecho_mes(tar_read(dado_sinistros), tar_read(dado_match), tar_read(dado_trechos))
# 
# df.total <- prepara_dado_did(dado_did = df, filtrar_por = NULL)
# 
# df.moto <- prepara_dado_did(dado_did = df, filtrar_por = motocicleta_envolvida)
# 
# df.golden <- prepara_dado_did(dado_did = df, filtrar_por = golden_match)
# 
# df.golden_moto <- prepara_dado_did(dado_did = df, filtrar_por = c(motocicleta_envolvida, golden_match))

# definir_cohort_trecho <- function(dado, faixa_azul){
#   cohort <- dado |> 
#     filter(mes == data_implementacao) |> 
#     distinct(data, data_implementacao) |> 
#     arrange(data_implementacao) |> 
#     left_join(faixa_azul |> 
#                 group_by(data_implementacao) |> 
#                 summarize(n_trechos = n()) |> 
#                 rename(data = data_implementacao))
#   return(cohort)
# }


definir_cohort <- function(dado, faixa_azul = NULL, logradouros = NULL, trechos = NULL, por_logradouro = FALSE){
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
                    clustervars = c("id"), control_group = c("nevertreated", "notyettreated"), idname = "id",
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
    data = df,# |> head(70*1000) |> mutate(tipo_via = as.factor(tipo_via)),
    clustervars = clustervars,
    control_group = control_group,
    xformla = formula)
  
  gerar_tabela_cohort(fit, cohorts, controle = FALSE, titulo = titulo, filename = filename)
  gerar_tabela_cohort(fit.c, cohorts, controle = TRUE, titulo = titulo, filename = filename)
  
  
  out <- aggte(fit.c, type = "simple")
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
}



