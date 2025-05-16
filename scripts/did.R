library(tidyverse)
library(sf)
library(did)


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
                    clustervars = c("id"), control_group = "notyettreated", idname = "id", ylim = 5,
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
  df <- df |> 
    mutate(id = as_factor(id) |> as.numeric())
  
  fit <- att_gt(
    yname = yname,
    tname = "mes",
    idname = idname,
    gname = "data_implementacao",
    data = df,
    clustervars = clustervars,
    control_group = control_group,
    xformla = ~ 1,
    base_period = "universal")
  fit.c <- att_gt(
    yname = yname,
    tname = "mes",
    idname = idname,
    gname = "data_implementacao",
    data = df,
    clustervars = clustervars,
    control_group = control_group,
    xformla = formula,
    base_period = "universal")
  
  gerar_tabela_cohort(fit, cohorts, controle = FALSE, titulo = titulo, filename = filename)
  gerar_tabela_cohort(fit.c, cohorts, controle = TRUE, titulo = titulo, filename = filename)
  
  
  out <- aggte(fit.c, type = "simple", na.rm = TRUE)
  ATT <- out$overall.att
  ci_low = ATT - out$overall.se * 1.96
  ci_high = ATT + out$overall.se * 1.96
  significance <- if(ci_low < 0 & ci_high > 0){""}else{"*"}
  
  
  # Gerar gráficos
  g1 <- bind_rows(
      preparar_grafico_did(fit, controle = "Não"),
      preparar_grafico_did(fit.c, controle = "Sim")) |> 
    filter(abs(egt) <= 12) |> 
    ggplot(aes(x = factor(egt), colour = controle, y = att)) +
    geom_pointrange(aes(ymin = ci_low, ymax = ci_high), position = position_dodge(width = .5), fatten = 1) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = factor(0), alpha = .05, lwd = 3) +
    ylim(-ylim, ylim) +
    theme_minimal() +
    labs(x = "Meses", y = "Efeito",
         title = titulo, colour = "Controles",
         subtitle = str_glue("Overall ATT com controle: {round(ATT,3)}{significance} ({round(ci_low,2)}; {round(ci_high, 2)})"))
  g1 |> 
    ggsave(filename = filename |> paste0("-plot.png"), 
           path = "output/did/",
           plot = _,
           width = 10, height = 7.5, dpi = 300)
  
 g2 <- fit.c |> 
    ggdid(xgap = 4, ncol = 3, title = paste(titulo, "- grupos"))
 g2 |> 
    ggsave(filename = filename |> paste0("-plot-groups.png"), 
           path = "output/did/",
           plot = _,
           width = 18, height = 9, dpi = 300)
}


fit_did_all <- function(df, titulo, filename, cohorts, yname = "sinistros", clustervars = c("id"),
                        control_group = "notyettreated", idname = "id", ylim = 5, apenas_km = TRUE, apenas_total = FALSE,
                        formula = ~ tipo_via + faixas + limite_velocidade + amenidades + intersec + radar_proximo + comprimento,
                        formula_km = ~ tipo_via + faixas + limite_velocidade + amenidades + intersec + radar_proximo) {
  if (apenas_km) {
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
      ylim = ylim,
      por_km = TRUE)
  }
  
  if (apenas_total) {
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
      ylim = ylim,
      por_km = FALSE)
  }
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
    tipo_via = c("primary")) {
  
  df.reg <- df |>
    filter(tipo_via %in% tipo_via)
  
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


did_filtro_avenidas <- function(
    df, titulo, filename, cohorts, yname = "sinistros",
    clustervars = c("id"), control_group = "notyettreated", idname = "id",
    formula = ~ tipo_via + faixas + amenidades + intersec + radar_proximo,
    formula_km = ~ tipo_via + faixas + amenidades + intersec + radar_proximo + comprimento,
    sem_av23maio = TRUE, sem_grupo63 = TRUE) {
  
  if (sem_av23maio) {
    df <- df |>
      filter(data_implementacao != 37)
  } 
  
  if (sem_grupo63) {
    df <- df |>
      filter(data_implementacao != 63)
  }
  
  fit_did_all(df, titulo, filename, cohorts, yname, clustervars, control_group, idname, formula, formula_km)
}


did_het_comprimento_trecho <- function(
    df, titulo, filename, cohorts, yname = "sinistros",
    clustervars = c("id"), control_group = "notyettreated", idname = "id",
    formula = ~ faixas + limite_velocidade + amenidades + intersec + radar_proximo,
    formula_km = ~ faixas + limite_velocidade + amenidades + intersec + radar_proximo + comprimento,
    comprimento_trecho = 50) {
  
  df.reg <- df |>
    filter(comprimento >= comprimento_trecho)
  
  fit_did_all(df.reg, titulo, filename, cohorts, yname, clustervars, control_group, idname, formula, formula_km)
}