library(tidyverse)
library(sf)
library(did)

dado_trecho_mes <- function(sinistros, match, trechos){
  sinistros |> 
    filter(year(data) > 2018) |> 
    select(id_sinistro, data, quantidade_envolvidos, motocicletas) |>
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
    summarize(sinistros = n()) |> 
    ungroup() |> 
    complete(data, id_osm, golden_match, motocicleta_envolvida, fill = list(sinistros = 0)) # Painel balanceado
}

prepara_dado_did <- function(dado_did, trechos, trechos_complemento, faixa_azul, filtrar_por = c(golden_match, motocicleta_envolvida)){
  dado_did |> 
    filter(if_all({{ filtrar_por }}, ~ .x == TRUE)) |> 
    group_by(id_osm, data) |>
    summarize(sinistros = sum(sinistros)) |>
    select(data, id_osm, sinistros) |>
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
    ungroup()
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

definir_cohort <- function(dado, faixa_azul){
  cohort <- dado |> 
    filter(mes == data_implementacao) |> 
    distinct(data, data_implementacao) |> 
    arrange(data_implementacao) |> 
    left_join(faixa_azul |> 
                group_by(data_implementacao) |> 
                summarize(n_trechos = n()) |> 
                rename(data = data_implementacao))
  return(cohort)
}






rodar_did <- function(df, titulo, cohorts, por_km = FALSE, clustervars = c("id_osm"), control_group = c("nevertreated", "notyettreated"),
                    formula = ~ tipo_via + faixas + limite_velocidade + amenidades + intersec + radar_proximo, idname = "id_osm"){
  
  if (por_km == TRUE){
    df <- df |> 
      mutate(across(c(sinistros, intersec, amenidades), ~ .x * 1000 / comprimento))
    titulo <- paste(titulo, "(por km de via)")
  }
  
  resultado <- att_gt(
    yname = "sinistros",
    tname = "mes",
    idname = "id_osm",
    gname = "data_implementacao",
    data = df,
    clustervars = clustervars,
    control_group = control_group,
    xformla = ~ 1)
  
  resultado.controle <- att_gt(
    yname = "sinistros",
    tname = "mes",
    idname = "id_osm",
    gname = "data_implementacao",
    data = df |> head(70*1000) |> mutate(tipo_via = as.factor(tipo_via)),
    clustervars = c("id_osm"),
    control_group = control_group,
    xformla = ~ tipo_via + faixas)
  
  gerar_tabela_cohort <- function(resultado, cohort, controle, titulo){
    if(controle){
      titulo <- paste(titulo, ",com variáveis de controle")
    }else{
      titulo <- paste(titulo, ",sem variáveis de controle")
    }
    aggte(resultado, type = "group", na.rm = T) |> 
      (\(result) tibble(egt = result$egt,
                        att = result$att.egt,
                        se = result$se.egt,
                        crit_val = result$crit.val.egt))(result = _) |>
      mutate(ci_low = att - crit_val * se,
             ci_high = att + crit_val * se) |>
      left_join(cohort, by = join_by(egt == data_implementacao)) |> 
      mutate(significante = ifelse(ci_low < 0 & ci_high > 0, "", "*")) |> 
      select(cohort = data, n_trechos, att, ci_low, ci_high, significante) |> 
      gt() |> 
      fmt_number(columns = c(ci_low, ci_high, att)) |> 
      tab_spanner("Intervalo 95%", columns = 4:5) |> 
      cols_label(cohort = "Coorte",
                 att = "Efeito médio estimado",
                 n_trechos = "Trechos tratados",
                 significante = "*",
                 ci_low = "[",
                 ci_high = "]") |> 
      tab_header(title = paste("tabela -", titulo)) |> 
      gtsave(titulo |> 
               stringi::stri_trans_general("latin-ascii") |> 
               str_to_lower() |> 
               paste0(".png"), path = "output/did/")
  }
  
  gerar_tabela_cohort(resultado, cohorts, controle = FALSE, titulo = titulo)
  gerar_tabela_cohort(resultado.controle, cohorts, controle = TRUE, titulo = titulo)
  
  preparar_grafico_did <- function(resultado, controle){
    aggte(resultado, type = "dynamic", na.rm = T) |> 
      (\(result) tibble(egt = result$egt,
                        att = result$att.egt,
                        se = result$se.egt,
                        crit_val = result$crit.val.egt))(result = _) |>
      mutate(ci_low = att - crit_val * se,
             ci_high = att + crit_val * se,
             controle = controle)
  }
  
  # Gerar gráfico
  (bind_rows(preparar_grafico_did(resultado, controle = FALSE),
             preparar_grafico_did(resultado.controle, controle = TRUE)) |> 
      filter(abs(egt) <= 12) |> 
      ggplot(aes(x = factor(egt), colour = controle, y = att)) +
      geom_pointrange(aes(ymin = ci_low, ymax = ci_high), position = position_dodge(width = .5), fatten = 1) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_vline(xintercept = factor(0), alpha = .05, lwd = 3) +
      theme_minimal() +
      labs(x = "Meses", y = "Efeito do tratamento",
           title = titulo, colour = "Controles")) |> 
    ggsave(filename = paste("grafico -", titulo |> 
                              stringi::stri_trans_general("latin-ascii") |> 
                              str_to_lower() |> 
                              paste0(".png")), 
           path = "output/did/",
           plot = _,
           width = 10, height = 7.5, dpi = 300)
  
}



