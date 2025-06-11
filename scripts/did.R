library(tidyverse)
library(sf)
library(did)
library(gt)


tabela_cohort_csdid <- function(fit, fit.c, cohort, titulo, filename) {
  filename <- paste0(filename, "-tabela-cohort")
  
  t1 <- aggte(fit, type = "group", na.rm = T) |>
    (\(result) tibble(egt = result$egt,
                      att = result$att.egt,
                      se = result$se.egt,
                      crit_val = result$crit.val.egt))(result = _)
  t2 <- aggte(fit.c, type = "group", na.rm = T) |>
    (\(result) tibble(egt = result$egt,
                      att = result$att.egt,
                      se = result$se.egt,
                      crit_val = result$crit.val.egt))(result = _)
  
  t <- bind_rows(t1, t2) |>
    mutate(ci_low = att - crit_val * se,
           ci_high = att + crit_val * se) |>
    left_join(cohort, by = join_by(egt == data_implementacao)) |>
    mutate(significante = ifelse(ci_low < 0 & ci_high > 0, "", "*")) |>
    select(cohort = data, n_trechos, n_agregados, n_logradouros, comprimento, att, ci_low, ci_high, significante) |>
    gt() |>
    fmt_number(columns = c(ci_low, ci_high, att)) |>
    tab_spanner("Intervalo 95%", columns = 4:5) |>
    cols_label(cohort = "Coorte",
               att = "ATE",
               n_trechos = "Trechos",
               n_agregados = "Trechos agregados",
               n_logradouros = "Logradouros",
               comprimento = "Comprimento (km)",
               significante = "*",
               ci_low = "[",
               ci_high = "]") |>
    tab_row_group(label = md("**Com controles**"), rows = (nrow(t1)+1):(nrow(t1)+nrow(t2))) |>
    tab_row_group(label = md("**Simples**"), rows = 1:nrow(t1)) |>
    tab_header(title = paste("Tabela -", titulo))
  t |>
    gtsave(filename |> paste0(".png"),
           path = "output/did/")
}


prepara_grafico_csdid <- function(fit, controle){
  aggte(fit, type = "dynamic", na.rm = T) |> 
    (\(result) tibble(egt = result$egt,
                      att = result$att.egt,
                      se = result$se.egt,
                      crit_val = result$crit.val.egt))(result = _) |>
    mutate(ci_low = att - crit_val * se,
           ci_high = att + crit_val * se,
           controle = controle |> as_factor())
}


fit_csdid <- function(
    df, por_km = TRUE,
    yname = "sinistros", clustervars = c("id"), control_group = "nevertreated", 
    idname = "id", base_period = "universal", weightsname = NULL, est_method = "dr",
    formula = ~ tipo_via + faixas + limite_velocidade + amenidades + intersec + radar_proximo,
    log_delta = NULL) {
  
  if (por_km == TRUE) {
    df <- df |> 
      mutate(across(c(matches("sinistro|gravidade|acidente"),  qtd_envolvidos, intersec, amenidades), 
                    ~ .x * 1000 / comprimento))
  } else {
    formula = ~ tipo_via + faixas + limite_velocidade + amenidades + intersec + radar_proximo + comprimento
  }
  
  if (!is.null(log_delta)) {
    df <- df |> 
      mutate(across(c(matches("sinistro|gravidade|acidente"), qtd_envolvidos), 
                    ~ log(.x + log_delta)))
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
    base_period = base_period,
    weightsname = weightsname,
    est_method = est_method)
  fit.c <- att_gt(
    yname = yname,
    tname = "mes",
    idname = idname,
    gname = "data_implementacao",
    data = df,
    clustervars = clustervars,
    control_group = control_group,
    xformla = formula,
    base_period = base_period,
    weightsname = weightsname,
    est_method = est_method)
  
  return(list(
    "fit" = fit, 
    "fit.c" = fit.c,
    "por_km" = por_km,
    "log_delta" = log_delta,
    "yname" = yname,
    "clustervars" = clustervars,
    "control_group" = control_group,
    "weightsname" = weightsname,
    "est_method" = est_method))
}


res_csdid <- function(fit, cohort, titulo, filename, ylim = 4, xlim = 12) {
  
  test_aggte <- function(obj) {
    tryCatch({
      aggte(obj)
      return(TRUE)
    }, error = function(e) {
      return(FALSE)
    })
  }
  
  # If aggte doesn't work, try to fix it
  if (!test_aggte(fit$fit)) {
    requireNamespace("did", quietly = TRUE)
    
    # Refresh class attributes
    class(fit$fit) <- c("MP", class(fit$fit)[class(fit$fit) != "MP"])
    class(fit$fit.c) <- c("MP", class(fit$fit.c)[class(fit$fit.c) != "MP"])
    
    # If still failing, recreate minimal environment
    if (!test_aggte(fit$fit)) {
      warning("Could not restore aggte functionality. Object may be corrupted.")
    }
  }
  
  # # Debug information
  # cat("Class of fit:", class(fit), "\n")
  # cat("Class of fit$fit:", class(fit$fit), "\n")
  # cat("Class of fit$fit.c:", class(fit$fit.c), "\n")
  # 
  # # Check if methods are available
  # cat("Methods for fit$fit:", methods(class = class(fit$fit)), "\n")
  # 
  # # Try the aggregation
  # tryCatch({
  #   result1 <- did::aggte(fit$fit)
  #   print(result1)
  #   cat("First aggte succeeded\n")
  # }, error = function(e) {
  #   cat("First aggte failed:", e$message, "\n")
  # })
  
  
  if (fit$por_km == TRUE) {
    titulo <- paste(titulo, "- por km")
    filename <- paste0(filename, "-km")
  } else {
    titulo <- paste(titulo, "- total")
    filename <- paste0(filename, "-total")
  }
  
  if (!is.null(fit$weightsname)) {
    titulo <- paste(titulo, ", ponderado")
    filename <- paste0(filename, "-weighted")
    
    if (str_detect(fit$weightsname, pattern = "sqrt")) {
      titulo <- paste(titulo, " (raiz quad.)")
      filename <- paste0(filename, "2")
    } else if (str_detect(fit$weightsname, pattern = "cbrt")) {
      titulo <- paste(titulo, " (raiz cub.)")
      filename <- paste0(filename, "3")
    } else {
      filename <- paste0(filename, "1")
    }
  }
  
  tabela_cohort_csdid(fit$fit, fit$fit.c, cohort, titulo, filename)
  
  out <- aggte(fit$fit.c, type = "simple", na.rm = TRUE)
  ATT <- out$overall.att
  ci_low = ATT - out$overall.se * 1.96
  ci_high = ATT + out$overall.se * 1.96
  significance <- if(ci_low < 0 & ci_high > 0){""}else{"*"}
  
  g1 <- bind_rows(
    prepara_grafico_csdid(fit$fit, controle = "Não"),
    prepara_grafico_csdid(fit$fit.c, controle = "Sim")) |>
    filter(abs(egt) <= xlim) |>
    ggplot(aes(x = factor(egt), colour = controle, y = att)) +
    geom_pointrange(aes(ymin = ci_low, ymax = ci_high), position = position_dodge(width = .5), fatten = 1) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = factor(0), alpha = .1, lwd = 3) +
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
  
  g2 <- fit$fit.c |>
    ggdid(xgap = 5, ncol = 3, title = paste(titulo, "- grupos"))
  g2 <- fit$fit.c |>
    ggdid(xgap = 5, ncol = 3, title = paste(titulo, "- grupos"))
  g2 |>
    ggsave(filename = filename |> paste0("-plot-groups.png"),
           path = "output/did/",
           plot = _,
           width = 18, height = 9, dpi = 300, bg = "white")
}


# fit_csdid_full <- function(df, cohort, titulo, filename, apenas_km = TRUE, ylim = 4,
#                            yname = "sinistros", clustervars = c("id"), control_group = "nevertreated", 
#                            idname = "id", base_period = "universal", weightsname = "comprimento", est_method = "dr",
#                            formula = ~ tipo_via + faixas + limite_velocidade + amenidades + intersec + radar_proximo) {
#   
#   fit_csdid(df, cohort, titulo, filename, 
#             por_km = TRUE, 
#             ylim = ylim, 
#             yname = yname, 
#             clustervars = clustervars, 
#             control_group = control_group, 
#             idname = idname, 
#             base_period = base_period, 
#             weightsname = NULL, 
#             est_method = est_method, 
#             formula = formula)
#   fit_csdid(df, cohort, titulo, filename, 
#             por_km = TRUE, 
#             ylim = ylim, 
#             yname = yname, 
#             clustervars = clustervars, 
#             control_group = control_group, 
#             idname = idname, 
#             base_period = base_period, 
#             weightsname = weightsname, 
#             est_method = est_method, 
#             formula = formula)
#   
#   if (!apenas_km) {
#     fit_csdid(df, cohort, titulo, filename, 
#               por_km = FALSE, 
#               ylim = ylim, 
#               yname = yname, 
#               clustervars = clustervars, 
#               control_group = control_group, 
#               idname = idname, 
#               base_period = base_period, 
#               weightsname = NULL, 
#               est_method = est_method, 
#               formula = formula)
#     fit_csdid(df, cohort, titulo, filename, 
#               por_km = FALSE, 
#               ylim = ylim, 
#               yname = yname, 
#               clustervars = clustervars, 
#               control_group = control_group, 
#               idname = idname, 
#               base_period = base_period, 
#               weightsname = weightsname, 
#               est_method = est_method, 
#               formula = formula)
#   }
# }


het_csdid_comprimento <- function(
    df, por_km = TRUE,
    yname = "sinistros", clustervars = c("id"), control_group = "nevertreated", 
    idname = "id", base_period = "universal", weightsname = NULL, est_method = "dr",
    formula = ~ tipo_via + faixas + limite_velocidade + amenidades + intersec + radar_proximo,
    comprimento_min = 50, comprimento_max = 2000) {
  
  df.reg <- df |>
    filter(comprimento >= comprimento_min,
           comprimento <= comprimento_max)
  
  fit_csdid(df.reg, por_km, yname, clustervars, control_group, idname, 
            base_period, weightsname, est_method, formula)
}


# het_csdid_num_faixas <- function(
    #     df, cohort, titulo, filename, apenas_km = TRUE, ylim = 4,
#     yname = "sinistros", clustervars = c("id"), control_group = "nevertreated", 
#     idname = "id", base_period = "universal", weightsname = "comprimento", est_method = "dr",
#     formula = ~ tipo_via + limite_velocidade + amenidades + intersec + radar_proximo,
#     num_faixas = 2, ate = TRUE) {
#   
#   if (ate) {
#     df.reg <- df |> 
#       filter(faixas <= num_faixas) 
#   } else {
#     df.reg <- df |> 
#       filter(faixas > num_faixas) 
#   }
#   
#   fit_csdid_full(df.reg, cohort, titulo, filename, apenas_km, ylim, yname, clustervars, control_group, 
#                  idname, base_period, weightsname, est_method, formula)
# }
# 
# 
# het_csdid_tipo_vias <- function(
    #     df, cohort, titulo, filename, apenas_km = TRUE, ylim = 4,
#     yname = "sinistros", clustervars = c("id"), control_group = "nevertreated", 
#     idname = "id", base_period = "universal", weightsname = "comprimento", est_method = "dr",
#     formula = ~ faixas + limite_velocidade + amenidades + intersec + radar_proximo,
#     tipo_vias = c("primary", "trunk")) {
#   
#   df.reg <- df |> 
#     filter(tipo_via %in% tipo_vias)
#   
#   fit_csdid_full(df.reg, cohort, titulo, filename, apenas_km, ylim, yname, clustervars, control_group, 
#                  idname, base_period, weightsname, est_method, formula)
# }
# 
# 
# het_csdid_vel_maxima <- function(
    #     df, cohort, titulo, filename, apenas_km = TRUE, ylim = 4,
#     yname = "sinistros", clustervars = c("id"), control_group = "nevertreated", 
#     idname = "id", base_period = "universal", weightsname = "comprimento", est_method = "dr",
#     formula = ~ tipo_via + faixas + amenidades + intersec + radar_proximo,
#     vel_maxima = 40, ate = TRUE) {
#   
#   if (ate) {
#     df.reg <- df |> 
#       filter(limite_velocidade <= vel_maxima) 
#   } else {
#     df.reg <- df |> 
#       filter(limite_velocidade > vel_maxima)
#   }
#   
#   fit_csdid_full(df.reg, cohort, titulo, filename, apenas_km, ylim, yname, clustervars, control_group, 
#                  idname, base_period, weightsname, est_method, formula)
# }
# 
# 
# het_csdid_tempo_tratado <- function(
    #     df, cohort, titulo, filename, apenas_km = TRUE, ylim = 4,
#     yname = "sinistros", clustervars = c("id"), control_group = "nevertreated", 
#     idname = "id", base_period = "universal", weightsname = "comprimento", est_method = "dr",
#     formula = ~ tipo_via + faixas + limite_velocidade + amenidades + intersec + radar_proximo,
#     meses = 12, ate = TRUE) {
#   
#   df <- df |> 
#     mutate(logr = str_split_i(id, "-", i = 1))
#     
#   
#   max_mes = interval(df$data |> min() |> as_date(), df$data |> max() |> as_date()) %/% months(1) + 1
#   logradouros <- df |>
#     filter(data_implementacao < max_mes - meses,
#            data_implementacao != 0) |>
#     distinct(logr) |>
#     pull(logr)
#   
#   if (ate) {
#     df.reg <- df |>
#       filter(!(logr %in% logradouros))
#   } else { 
#     df.reg <- df |>
#       filter(logr %in% logradouros | data_implementacao == 0)
#   }
#   
#   fit_csdid_full(df.reg, cohort, titulo, filename, apenas_km, ylim, yname, clustervars, control_group, 
#                  idname, base_period, weightsname, est_method, formula)
# }