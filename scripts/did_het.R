library(tidyverse)
library(sf)
library(did)


gerar_tabela_cohort_het <- function(fit, cohort, controle, titulo, filename){
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
           path = "output/did_het/")
}


preparar_grafico_did_het <- function(fit, controle){
  aggte(fit, type = "dynamic", na.rm = T) |> 
    (\(result) tibble(egt = result$egt,
                      att = result$att.egt,
                      se = result$se.egt,
                      crit_val = result$crit.val.egt))(result = _) |>
    mutate(ci_low = att - crit_val * se,
           ci_high = att + crit_val * se,
           controle = controle |> as_factor())
}


fit_did_het <- function(df, titulo, filename, cohorts, por_km = FALSE, yname = "sinistros",
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
  
  gerar_tabela_cohort_het(fit, cohorts, controle = FALSE, titulo, filename)
  gerar_tabela_cohort_het(fit.c, cohorts, controle = TRUE, titulo, filename)
  
  
  out <- aggte(fit.c, type = "simple")
  ATT <- out$overall.att
  ci_low = ATT - out$overall.se * 1.96
  ci_high = ATT + out$overall.se * 1.96
  significance <- if(ci_low < 0 & ci_high > 0){""}else{"*"}
  
  
  # Gerar gráfico
  (bind_rows(preparar_grafico_did_het(fit, controle = "Não"),
             preparar_grafico_did_het(fit.c, controle = "Sim")) |> 
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
           path = "output/did_het/",
           plot = _,
           width = 10, height = 7.5, dpi = 300)
}


did_by_group <- function(df, titulo, filename, cohorts, group, yname = "sinistros",
                         clustervars = c("id"), control_group = c("nevertreated", "notyettreated"), idname = "id"){
  df <- df |> 
    filter(data_implementacao == 0 | data_implementacao == group)
  
  fit_did_het(df = df,
              titulo = titulo,
              filename = filename,
              yname = yname,
              cohorts = cohorts,
              control_group = control_group,
              por_km = FALSE)
  
  fit_did_het(df = df,
              titulo = titulo,
              filename = filename,
              yname = yname,
              cohorts = cohorts,
              control_group = control_group,
              por_km = TRUE)
  
}


did_yname <- function(df, titulo, filename, cohorts, yname, clustervars = c("id"), 
                      control_group = "notyettreated", idname = "id"){
  fit_did_het(df = df,
              titulo = titulo,
              filename = filename,
              yname = yname,
              cohorts = cohorts,
              control_group = control_group,
              por_km = FALSE)
  
  fit_did_het(df = df,
              titulo = titulo,
              filename = filename,
              yname = yname,
              cohorts = cohorts,
              control_group = control_group,
              por_km = TRUE)
}




# ############ TESTES NOVOS CONTROLES OSM ----
# 
# fit1 <- bind_rows(
#   df.trecho |> 
#     fit.did(y = "sinistros_moto_golden_km", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não"),
#   df.trecho |> 
#     fit.did(y = "sinistros_moto_golden_km", formula = ~ tipo_via + faixas + limite_velocidade) |> 
#     preparar.grafico() |> 
#     mutate(controle = "OSM base")) 
# fit1 |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Sinistros totais")
# 
# fit2 <- bind_rows(
#   df.trecho |> 
#     fit.did(y = "sinistros_moto_golden_km", formula = ~ intersec_f + radar_proximo + amenidades_f) |> 
#     preparar.grafico() |> 
#     mutate(controle = "OSM complemento"),
#   df.trecho |> 
#     fit.did(y = "sinistros_moto_golden_km", formula = ~ tipo_via + faixas + limite_velocidade + intersec_f + radar_proximo + amenidades_f) |> 
#     preparar.grafico() |> 
#     mutate(controle = "OSM tudo")) 
# fit2 |> 
#   filter(abs(egt) <= horizon, controle != "OSM interseções") |> 
#   plotar.grafico(titulo = "Sinistros totais")
# 
# fit3 <- bind_rows(
#   df.trecho |> 
#     fit.did(y = "sinistros_moto_golden_km", formula = ~ intersec_km + radar_proximo + amenidades_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "OSM complemento (km)"),
#   df.trecho |> 
#     fit.did(y = "sinistros_moto_golden_km", formula = ~ tipo_via + faixas + limite_velocidade + intersec_km + radar_proximo + amenidades_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "OSM tudo (km)")) 
# fit3 |> 
#   filter(abs(egt) <= horizon, controle != "OSM interseções (km)") |> 
#   plotar.grafico(titulo = "Sinistros totais")
# 
# bind_rows(fit1, fit2, fit3) |> 
#   filter(abs(egt) <= horizon, controle %in% c("Não", "OSM base", "OSM tudo", "OSM tudo (km)")) |> 
#   mutate(controle = as_factor(controle)) |> 
#   plotar.grafico(titulo = "Sinistros golden por km")
# ggsave("output/did_het/golden_km_controles.png", width = 10, height = 7.5)
# 
# 
# ############ FILTRO IFOOD ----
# 
# df.reg <- df.trecho |> 
#   semi_join(ifood)
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden_km", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = FALSE),
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden_km", formula = ~ tipo_via + faixas + limite_velocidade + intersec_km + radar_proximo + amenidades_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = TRUE)) 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Sinistros golden por km (iFood)")
# ggsave("output/did_het/golden_km_ifood.png", width = 10, height = 7.5)
# 
# 
# ############ BANDEIRANTES + 23 DE MAIO + JACU PESSEGO vs RESTO ----
# 
# avenidas <- c("BANDEIRANTES", "BANDEIRANTES SUL", "JACU PESSEGO", "VINTE E TRES MAIO")
# 
# 
# ### Apenas ----
# df.reg <- df.trecho |> 
#   filter(logradouro_limpo %in% avenidas | data_implementacao == 0)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Todos os sinistros - total")
# ggsave("output/did_het/trecho/Band + 23 + Jacu/apenas-todos-total.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Band + 23 + Jacu/apenas-todos-total.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_km", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Todos os sinistros - por km")
# ggsave("output/did_het/trecho/Band + 23 + Jacu/apenas-todos-km.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Band + 23 + Jacu/apenas-todos-km.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Sinistros de moto golden - total")
# ggsave("output/did_het/trecho/Band + 23 + Jacu/apenas-golden-total.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Band + 23 + Jacu/apenas-golden-total.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden_km", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Sinistros de moto golden - por km")
# ggsave("output/did_het/trecho/Band + 23 + Jacu/apenas-golden-km.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Band + 23 + Jacu/apenas-golden-km.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# ### Exceto ----
# 
# df.reg <- df.trecho |> 
#   filter(!(logradouro_limpo %in% avenidas))
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Todos os sinistros - total")
# ggsave("output/did_het/trecho/Band + 23 + Jacu/exceto-todos-total.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Band + 23 + Jacu/exceto-todos-total.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_km", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Todos os sinistros - por km")
# ggsave("output/did_het/trecho/Band + 23 + Jacu/exceto-todos-km.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Band + 23 + Jacu/exceto-todos-km.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Sinistros de moto golden - total")
# ggsave("output/did_het/trecho/Band + 23 + Jacu/exceto-golden-total.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Band + 23 + Jacu/exceto-golden-total.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden_km", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Sinistros de moto golden - por km")
# ggsave("output/did_het/trecho/Band + 23 + Jacu/exceto-golden-km.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Band + 23 + Jacu/exceto-golden-km.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# ### Sem Jacu Pessego ----
# 
# df.reg <- df.trecho |> 
#   filter(logradouro_limpo != "JACU PESSEGO")
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_km", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Todos os sinistros - por km")
# ggsave("output/did_het/trecho/Band + 23 + Jacu/exceto-jacu-todos-km.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Band + 23 + Jacu/exceto-jacu-todos-km.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden_km", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Sinistros de moto golden - por km")
# ggsave("output/did_het/trecho/Band + 23 + Jacu/exceto-jacu-golden-km.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Band + 23 + Jacu/exceto-jacu-golden-km.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# ############ AVENIDAS POR MESES TRATADOS - 12 ----
# max_mes = interval(df.trecho$data |> min() |> as_date(), df.trecho$data |> max() |> as_date()) %/% months(1) + 1
# avenidas <- df.trecho |> 
#   filter(data_implementacao < max_mes - 12,
#          data_implementacao != 0) |> 
#   distinct(logradouro_limpo) |> 
#   pull(logradouro_limpo)
# 
# 
# ### Mais que ----
# 
# df.reg <- df.trecho |> 
#   filter(logradouro_limpo %in% avenidas | data_implementacao == 0)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Todos os sinistros - total")
# ggsave("output/did_het/trecho/Meses tratados/12-apenas-todos-total.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Meses tratados/12-apenas-todos-total.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_km", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Todos os sinistros - por km")
# ggsave("output/did_het/trecho/Meses tratados/12-apenas-todos-km.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Meses tratados/12-apenas-todos-km.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Sinistros de moto golden - total")
# ggsave("output/did_het/trecho/Meses tratados/12-apenas-golden-total.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Meses tratados/12-apenas-golden-total.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden_km", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Sinistros de moto golden - por km")
# ggsave("output/did_het/trecho/Meses tratados/12-apenas-golden-km.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Meses tratados/12-apenas-golden-km.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# ### Ate ----
# 
# df.reg <- df.trecho |> 
#   filter(!(logradouro_limpo %in% avenidas))
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Todos os sinistros - total")
# ggsave("output/did_het/trecho/Meses tratados/12-exceto-todos-total.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Meses tratados/12-exceto-todos-total.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_km", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Todos os sinistros - por km")
# ggsave("output/did_het/trecho/Meses tratados/12-exceto-todos-km.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Meses tratados/12-exceto-todos-km.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Sinistros de moto golden - total")
# ggsave("output/did_het/trecho/Meses tratados/12-exceto-golden-total.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Meses tratados/12-exceto-golden-total.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden_km", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Sinistros de moto golden - por km")
# ggsave("output/did_het/trecho/Meses tratados/12-exceto-golden-km.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Meses tratados/12-exceto-golden-km.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# ############ AVENIDAS POR MESES TRATADOS - 15 ----
# max_mes = interval(df.trecho$data |> min() |> as_date(), df.trecho$data |> max() |> as_date()) %/% months(1) + 1
# avenidas <- df.trecho |> 
#   filter(data_implementacao < max_mes - 15,
#          data_implementacao != 0) |> 
#   distinct(logradouro_limpo) |> 
#   pull(logradouro_limpo)
# 
# 
# ### Mais que ----
# 
# df.reg <- df.trecho |> 
#   filter(logradouro_limpo %in% avenidas | data_implementacao == 0)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Todos os sinistros - total")
# ggsave("output/did_het/trecho/Meses tratados/15-apenas-todos-total.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Meses tratados/15-apenas-todos-total.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_km", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Todos os sinistros - por km")
# ggsave("output/did_het/trecho/Meses tratados/15-apenas-todos-km.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Meses tratados/15-apenas-todos-km.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Sinistros de moto golden - total")
# ggsave("output/did_het/trecho/Meses tratados/15-apenas-golden-total.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Meses tratados/15-apenas-golden-total.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden_km", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Sinistros de moto golden - por km")
# ggsave("output/did_het/trecho/Meses tratados/15-apenas-golden-km.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Meses tratados/15-apenas-golden-km.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# ### Ate ----
# 
# df.reg <- df.trecho |> 
#   filter(!(logradouro_limpo %in% avenidas))
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Todos os sinistros - total")
# ggsave("output/did_het/trecho/Meses tratados/15-exceto-todos-total.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Meses tratados/15-exceto-todos-total.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_km", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Todos os sinistros - por km")
# ggsave("output/did_het/trecho/Meses tratados/15-exceto-todos-km.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Meses tratados/15-exceto-todos-km.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Sinistros de moto golden - total")
# ggsave("output/did_het/trecho/Meses tratados/15-exceto-golden-total.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Meses tratados/15-exceto-golden-total.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden_km", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Sinistros de moto golden - por km")
# ggsave("output/did_het/trecho/Meses tratados/15-exceto-golden-km.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Meses tratados/15-exceto-golden-km.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# ############ AVENIDAS POR NÚMERO DE FAIXAS - 2 ----
# 
# ### Ate ----
# 
# df.reg <- df.trecho |> 
#   mutate(faixas = as.integer(faixas)) |> 
#   filter(faixas <= 2)
# 
# df.reg |> filter(data == "2024-12-01") |> count(data_implementacao)
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Todos os sinistros - total")
# ggsave("output/did_het/trecho/Numero de faixas/2-ate-todos-total.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Numero de faixas/2-ate-todos-total.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_km", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Todos os sinistros - por km")
# ggsave("output/did_het/trecho/Numero de faixas/2-ate-todos-km.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Numero de faixas/2-ate-todos-km.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Sinistros de moto golden - total")
# ggsave("output/did_het/trecho/Numero de faixas/2-ate-golden-total.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Numero de faixas/2-ate-golden-total.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden_km", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Sinistros de moto golden - por km")
# ggsave("output/did_het/trecho/Numero de faixas/2-ate-golden-km.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Numero de faixas/2-ate-golden-km.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# ### Mais que ----
# 
# df.reg <- df.trecho |> 
#   mutate(faixas = as.integer(faixas)) |> 
#   filter(faixas > 2)
# 
# df.reg |> filter(data == "2024-12-01") |> count(data_implementacao)
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Todos os sinistros - total")
# ggsave("output/did_het/trecho/Numero de faixas/2-maisQue-todos-total.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Numero de faixas/2-maisQue-todos-total.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_km", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Todos os sinistros - por km")
# ggsave("output/did_het/trecho/Numero de faixas/2-maisQue-todos-km.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Numero de faixas/2-maisQue-todos-km.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Sinistros de moto golden - total")
# ggsave("output/did_het/trecho/Numero de faixas/2-maisQue-golden-total.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Numero de faixas/2-maisQue-golden-total.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden_km", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Sinistros de moto golden - por km")
# ggsave("output/did_het/trecho/Numero de faixas/2-maisQue-golden-km.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Numero de faixas/2-maisQue-golden-km.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# ############ AVENIDAS POR NÚMERO DE FAIXAS - 3 ----
# 
# ### Ate ----
# 
# df.reg <- df.trecho |> 
#   mutate(faixas = as.integer(faixas)) |> 
#   filter(faixas <= 3)
# 
# df.reg |> filter(data == "2024-12-01") |> count(data_implementacao)
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Todos os sinistros - total")
# ggsave("output/did_het/trecho/Numero de faixas/3-ate-todos-total.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Numero de faixas/3-ate-todos-total.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_km", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Todos os sinistros - por km")
# ggsave("output/did_het/trecho/Numero de faixas/3-ate-todos-km.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Numero de faixas/3-ate-todos-km.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Sinistros de moto golden - total")
# ggsave("output/did_het/trecho/Numero de faixas/3-ate-golden-total.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Numero de faixas/3-ate-golden-total.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden_km", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Sinistros de moto golden - por km")
# ggsave("output/did_het/trecho/Numero de faixas/3-ate-golden-km.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Numero de faixas/3-ate-golden-km.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# ### Mais que ----
# 
# df.reg <- df.trecho |> 
#   mutate(faixas = as.integer(faixas)) |> 
#   filter(faixas > 3)
# 
# df.reg |> filter(data == "2024-12-01") |> count(data_implementacao)
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Todos os sinistros - total")
# ggsave("output/did_het/trecho/Numero de faixas/3-maisQue-todos-total.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Numero de faixas/3-maisQue-todos-total.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_km", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Todos os sinistros - por km")
# ggsave("output/did_het/trecho/Numero de faixas/3-maisQue-todos-km.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Numero de faixas/3-maisQue-todos-km.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Sinistros de moto golden - total")
# ggsave("output/did_het/trecho/Numero de faixas/3-maisQue-golden-total.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Numero de faixas/3-maisQue-golden-total.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden_km", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Sinistros de moto golden - por km")
# ggsave("output/did_het/trecho/Numero de faixas/3-maisQue-golden-km.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Numero de faixas/3-maisQue-golden-km.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# ############ TIPO DE VIA ----
# 
# ### Primary ----
# 
# df.reg <- df.trecho |> 
#   filter(tipo_via == "primary")
# 
# df.reg |> filter(data == "2024-12-01") |> count(data_implementacao)
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros", formula = ~ faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Todos os sinistros - total")
# ggsave("output/did_het/trecho/Tipo de via/primary-todos-total.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Tipo de via/primary-todos-total.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_km", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_km", formula = ~ faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Todos os sinistros - por km")
# ggsave("output/did_het/trecho/Tipo de via/primary-todos-km.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Tipo de via/primary-todos-km.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden", formula = ~ faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Sinistros de moto golden - total")
# ggsave("output/did_het/trecho/Tipo de via/primary-golden-total.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Tipo de via/primary-golden-total.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden_km", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden_km", formula = ~ faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Sinistros de moto golden - por km")
# ggsave("output/did_het/trecho/Tipo de via/primary-golden-km.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Tipo de via/primary-golden-km.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# ### Secondary ----
# 
# # df.reg <- df.trecho |> 
# #   filter(tipo_via == "secondary")
# # 
# # df.reg |> filter(data == "2024-12-01") |> count(data_implementacao)
# # 
# # fit <- bind_rows(
# #   df.reg |> 
# #     fit.did(y = "sinistros", formula = ~ 1) |> 
# #     preparar.grafico() |> 
# #     mutate(controle = "Não" |> as_factor()),
# #   df.reg |> 
# #     fit.did(y = "sinistros", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
# #     preparar.grafico() |> 
# #     mutate(controle = "Sim" |> as_factor())) 
# # 
# # fit |> 
# #   filter(abs(egt) <= horizon) |> 
# #   plotar.grafico(titulo = "Todos os sinistros - total")
# # ggsave("output/did_het/trecho/Tipo de via/secondary-todos-total.pdf", width = 10, height = 7.5)
# # ggsave("output/did_het/trecho/Tipo de via/secondary-todos-total.png", width = 10, height = 7.5, dpi = 600)
# # 
# # 
# # fit <- bind_rows(
# #   df.reg |> 
# #     fit.did(y = "sinistros_km", formula = ~ 1) |> 
# #     preparar.grafico() |> 
# #     mutate(controle = "Não" |> as_factor()),
# #   df.reg |> 
# #     fit.did(y = "sinistros_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
# #     preparar.grafico() |> 
# #     mutate(controle = "Sim" |> as_factor())) 
# # 
# # fit |> 
# #   filter(abs(egt) <= horizon) |> 
# #   plotar.grafico(titulo = "Todos os sinistros - por km")
# # ggsave("output/did_het/trecho/Tipo de via/secondary-todos-km.pdf", width = 10, height = 7.5)
# # ggsave("output/did_het/trecho/Tipo de via/secondary-todos-km.png", width = 10, height = 7.5, dpi = 600)
# # 
# # 
# # fit <- bind_rows(
# #   df.reg |> 
# #     fit.did(y = "sinistros_moto_golden", formula = ~ 1) |> 
# #     preparar.grafico() |> 
# #     mutate(controle = "Não" |> as_factor()),
# #   df.reg |> 
# #     fit.did(y = "sinistros_moto_golden", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
# #     preparar.grafico() |> 
# #     mutate(controle = "Sim" |> as_factor())) 
# # 
# # fit |> 
# #   filter(abs(egt) <= horizon) |> 
# #   plotar.grafico(titulo = "Sinistros de moto golden - total")
# # ggsave("output/did_het/trecho/Tipo de via/secondary-golden-total.pdf", width = 10, height = 7.5)
# # ggsave("output/did_het/trecho/Tipo de via/secondary-golden-total.png", width = 10, height = 7.5, dpi = 600)
# # 
# # 
# # fit <- bind_rows(
# #   df.reg |> 
# #     fit.did(y = "sinistros_moto_golden_km", formula = ~ 1) |> 
# #     preparar.grafico() |> 
# #     mutate(controle = "Não" |> as_factor()),
# #   df.reg |> 
# #     fit.did(y = "sinistros_moto_golden_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
# #     preparar.grafico() |> 
# #     mutate(controle = "Sim" |> as_factor())) 
# # 
# # fit |> 
# #   filter(abs(egt) <= horizon) |> 
# #   plotar.grafico(titulo = "Sinistros de moto golden - por km")
# # ggsave("output/did_het/trecho/Tipo de via/secondary-golden-km.pdf", width = 10, height = 7.5)
# # ggsave("output/did_het/trecho/Tipo de via/secondary-golden-km.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# ### Trunk ----
# 
# df.reg <- df.trecho |> 
#   filter(tipo_via == "trunk")
# 
# df.reg |> filter(data == "2024-12-01") |> count(data_implementacao)
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros", formula = ~ faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Todos os sinistros - total")
# ggsave("output/did_het/trecho/Tipo de via/trunk-todos-total.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Tipo de via/trunk-todos-total.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_km", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_km", formula = ~ faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Todos os sinistros - por km")
# ggsave("output/did_het/trecho/Tipo de via/trunk-todos-km.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Tipo de via/trunk-todos-km.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden", formula = ~ faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Sinistros de moto golden - total")
# ggsave("output/did_het/trecho/Tipo de via/trunk-golden-total.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Tipo de via/trunk-golden-total.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden_km", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden_km", formula = ~ faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Sinistros de moto golden - por km")
# ggsave("output/did_het/trecho/Tipo de via/trunk-golden-km.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Tipo de via/trunk-golden-km.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# ############ VELOCIDADE MAXIMA - 40km/h ----
# 
# ### Ate ----
# 
# df.reg <- df.trecho |> 
#   mutate(limite_velocidade = as.integer(limite_velocidade)) |> 
#   filter(limite_velocidade <= 40)
# 
# df.reg |> filter(data == "2024-12-01") |> count(data_implementacao)
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros", formula = ~ faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Todos os sinistros - total")
# ggsave("output/did_het/trecho/Velocidade maxima/40-ate-todos-total.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Velocidade maxima/40-ate-todos-total.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_km", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_km", formula = ~ faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Todos os sinistros - por km")
# ggsave("output/did_het/trecho/Velocidade maxima/40-ate-todos-km.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Velocidade maxima/40-ate-todos-km.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden", formula = ~ faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Sinistros de moto golden - total")
# ggsave("output/did_het/trecho/Velocidade maxima/40-ate-golden-total.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Velocidade maxima/40-ate-golden-total.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden_km", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden_km", formula = ~ faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Sinistros de moto golden - por km")
# ggsave("output/did_het/trecho/Velocidade maxima/40-ate-golden-km.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Velocidade maxima/40-ate-golden-km.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# ### Mais que ----
# 
# df.reg <- df.trecho |> 
#   mutate(limite_velocidade = as.integer(limite_velocidade)) |> 
#   filter(limite_velocidade > 40)
# 
# df.reg |> filter(data == "2024-12-01") |> count(data_implementacao)
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros", formula = ~ faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Todos os sinistros - total")
# ggsave("output/did_het/trecho/Velocidade maxima/40-maisQue-todos-total.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Velocidade maxima/40-maisQue-todos-total.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_km", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_km", formula = ~ faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Todos os sinistros - por km")
# ggsave("output/did_het/trecho/Velocidade maxima/40-maisQue-todos-km.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Velocidade maxima/40-maisQue-todos-km.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden", formula = ~ faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Sinistros de moto golden - total")
# ggsave("output/did_het/trecho/Velocidade maxima/40-maisQue-golden-total.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Velocidade maxima/40-maisQue-golden-total.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden_km", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden_km", formula = ~ faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Sinistros de moto golden - por km")
# ggsave("output/did_het/trecho/Velocidade maxima/40-maisQue-golden-km.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Velocidade maxima/40-maisQue-golden-km.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# ############ VELOCIDADE MAXIMA - 50km/h ----
# 
# ### Ate ----
# 
# df.reg <- df.trecho |> 
#   mutate(limite_velocidade = as.integer(limite_velocidade)) |> 
#   filter(limite_velocidade <= 50)
# 
# df.reg |> filter(data == "2024-12-01") |> count(data_implementacao)
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros", formula = ~ faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Todos os sinistros - total")
# ggsave("output/did_het/trecho/Velocidade maxima/50-ate-todos-total.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Velocidade maxima/50-ate-todos-total.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_km", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_km", formula = ~ faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Todos os sinistros - por km")
# ggsave("output/did_het/trecho/Velocidade maxima/50-ate-todos-km.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Velocidade maxima/50-ate-todos-km.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden", formula = ~ faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Sinistros de moto golden - total")
# ggsave("output/did_het/trecho/Velocidade maxima/50-ate-golden-total.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Velocidade maxima/50-ate-golden-total.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden_km", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden_km", formula = ~ faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Sinistros de moto golden - por km")
# ggsave("output/did_het/trecho/Velocidade maxima/50-ate-golden-km.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Velocidade maxima/50-ate-golden-km.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# ### Mais que ----
# 
# df.reg <- df.trecho |> 
#   mutate(limite_velocidade = as.integer(limite_velocidade)) |> 
#   filter(limite_velocidade > 50)
# 
# df.reg |> filter(data == "2024-12-01") |> count(data_implementacao)
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros", formula = ~ faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Todos os sinistros - total")
# ggsave("output/did_het/trecho/Velocidade maxima/50-maisQue-todos-total.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Velocidade maxima/50-maisQue-todos-total.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_km", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_km", formula = ~ faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Todos os sinistros - por km")
# ggsave("output/did_het/trecho/Velocidade maxima/50-maisQue-todos-km.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Velocidade maxima/50-maisQue-todos-km.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden", formula = ~ faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Sinistros de moto golden - total")
# ggsave("output/did_het/trecho/Velocidade maxima/50-maisQue-golden-total.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Velocidade maxima/50-maisQue-golden-total.png", width = 10, height = 7.5, dpi = 600)
# 
# 
# fit <- bind_rows(
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden_km", formula = ~ 1) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Não" |> as_factor()),
#   df.reg |> 
#     fit.did(y = "sinistros_moto_golden_km", formula = ~ faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
#     preparar.grafico() |> 
#     mutate(controle = "Sim" |> as_factor())) 
# 
# fit |> 
#   filter(abs(egt) <= horizon) |> 
#   plotar.grafico(titulo = "Sinistros de moto golden - por km")
# ggsave("output/did_het/trecho/Velocidade maxima/50-maisQue-golden-km.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Velocidade maxima/50-maisQue-golden-km.png", width = 10, height = 7.5, dpi = 600)
# 
