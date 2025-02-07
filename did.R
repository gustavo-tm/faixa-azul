library(tidyverse)
library(sf)
library(did)

match <- read_csv("banco_dados/match.csv", col_types = list(id_osm = "c")) |> 
  mutate(golden_match = 
           similaridade > .75 & 
           distancia_geografica < 100 &
           match_titulo == TRUE &
           match_tipo == TRUE)

trechos <- st_read("banco_dados/trechos.gpkg") |> 
  st_drop_geometry() |> 
  as_tibble() |> 
  filter(tipo_via %in% c("trunk", "primary", "secondary")) |> 
  
  #Limpar os nomes dos logradouros
  left_join(read_csv("dados_tratados/osm-token.csv", col_types = list(id_osm = "c")) |> 
              select(id_osm, logradouro_limpo) |> 
              mutate(len = str_length(logradouro_limpo)) |> 
              group_by(id_osm) |> 
              arrange(-len) |> 
              summarize(logradouro_limpo = nth(logradouro_limpo, 1), .groups = "drop")) |> 
  select(id_osm, logradouro, logradouro_limpo, tipo_via, faixas, limite_velocidade, mao_unica, superficie, comprimento)

faixa_azul <- read_csv("banco_dados/faixa_azul.csv", col_types = list(id_osm = "c"))

sinistros <- read_csv("banco_dados/sinistros.csv") |> 
  filter(year(data) >= 2019, tipo != "NOTIFICACAO") |> # Antes de 2019 há apenas sinistros com óbito
  select(id_sinistro, data, quantidade_envolvidos, motocicletas)


fit.did <- function(df, y = "sinistros", clustervars = c("id_osm", "logradouro_limpo"), control_group = "nevertreated", 
                    formula = ~ tipo_via + faixas + limite_velocidade, idname = "id_osm"){
  att_gt(yname = y,
         tname = "mes",
         idname = idname,
         gname = "data_implementacao",
         clustervars = clustervars,
         control_group = control_group,
         data = df,
         xformla = formula) |>
    aggte(type = "dynamic", na.rm = T)
}

preparar.grafico <- function(fit){
  fit |> 
    (\(result) tibble(egt = result$egt,
                      att = result$att.egt,
                      se = result$se.egt,
                      crit_val = result$crit.val.egt))(result = _) |>
    mutate(ci_low = att - crit_val * se,
           ci_high = att + crit_val * se)
}

plotar.grafico <- function(df, titulo = NULL){
  df |> 
    ggplot(aes(x = factor(egt), colour = controle, y = att)) +
    geom_pointrange(aes(ymin = ci_low, ymax = ci_high), position = position_dodge(width = .5), fatten = 1) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = factor(0), alpha = .05, lwd = 3) +
    theme_minimal() +
    labs(x = "Meses ao tratamento", y = "Efeito do tratamento",
         title = titulo) 
}

horizon <- 18


### POR TRECHO

df.trecho <- sinistros |> 
  left_join(match) |> 
  semi_join(trechos, by = join_by(id_osm)) |> 
  group_by(data = make_date(year = year(data), month = month(data)), id_osm) |> 
  summarize(sinistros = n(), 
            sinistros_moto = sum(motocicletas > 0),
            sinistros_moto_golden = sum(motocicletas > 0 & golden_match == TRUE),
            .groups = "drop") |> 
  complete(data, id_osm, fill = list(sinistros = 0, sinistros_moto = 0, sinistros_moto_golden = 0)) |> # Painel balanceado
  left_join(trechos |> 
              left_join(faixa_azul |> distinct())) |> 
  
  #trasformacao da data em valor numerico (na ordem)
  mutate(mes = data) |> 
  pivot_longer(c(mes, data_implementacao)) |> 
  mutate(value = value |> reorder(value) |> factor() |> as.numeric()) |> 
  pivot_wider(names_from = name, values_from = value) |> 
  mutate(data_implementacao = replace_na(data_implementacao, 0),
         id_osm = as.numeric(id_osm)) |> 
  mutate(sinistros_km             = sinistros * 1000 / comprimento,
         sinistros_moto_km        = sinistros_moto * 1000 / comprimento,
         sinistros_moto_golden_km = sinistros_moto_golden * 1000 / comprimento) |> 
  mutate(sinistros_diff           = sinistros_moto - sinistros,
         sinistros_diff_km        = sinistros_diff * 1000 / comprimento,
         sinistros_diff_golden    = sinistros_moto_golden - sinistros,
         sinistros_diff_golden_km = sinistros_diff_golden * 1000 / comprimento)


# TOTAL

bind_rows(
  df.trecho |> 
    fit.did(y = "sinistros", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = FALSE),
  df.trecho |> 
    fit.did(y = "sinistros", formula = ~ tipo_via + faixas + limite_velocidade + comprimento) |> 
    preparar.grafico() |> 
    mutate(controle = TRUE)) |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros totais")

ggsave("output/did/sinistros-total-2.pdf", width = 10, height = 7.5)


bind_rows(
  df.trecho |> 
    fit.did(y = "sinistros_km", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = FALSE),
  df.trecho |> 
    fit.did(y = "sinistros_km", formula = ~ tipo_via + faixas + limite_velocidade) |> 
    preparar.grafico() |> 
    mutate(controle = TRUE)) |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros por km")

ggsave("output/did/sinistros-km.pdf", width = 10, height = 7.5)


# MOTOS

bind_rows(
  df.trecho |> 
    fit.did(y = "sinistros_moto", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = FALSE),
  df.trecho |> 
    fit.did(y = "sinistros_moto", formula = ~ tipo_via + faixas + limite_velocidade + comprimento) |> 
    preparar.grafico() |> 
    mutate(controle = TRUE)) |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto totais")

ggsave("output/did/sinistros-moto-total.pdf", width = 10, height = 7.5)


bind_rows(
  df.trecho |> 
    fit.did(y = "sinistros_moto_km", control_group = "notyettreated", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = FALSE),
  df.trecho |> 
    fit.did(y = "sinistros_moto_km", control_group = "notyettreated", formula = ~ tipo_via + faixas + limite_velocidade) |> 
    preparar.grafico() |> 
    mutate(controle = TRUE)) |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto por km")

ggsave("output/did/sinistros-moto-km.pdf", width = 10, height = 7.5)


# GOLDEN

bind_rows(
  df.trecho |> 
    fit.did(y = "sinistros_moto_golden", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = FALSE),
  df.trecho |> 
    fit.did(y = "sinistros_moto_golden", formula = ~ tipo_via + faixas + limite_velocidade) |> 
    preparar.grafico() |> 
    mutate(controle = TRUE)) |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros golden totais")

ggsave("output/did/sinistros-golden-total.pdf", width = 10, height = 7.5)


bind_rows(
  df.trecho |> 
    fit.did(y = "sinistros_moto_km", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = FALSE),
  df.trecho |> 
    fit.did(y = "sinistros_moto_km", formula = ~ tipo_via + faixas + limite_velocidade) |> 
    preparar.grafico() |> 
    mutate(controle = TRUE)) |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros golden por km")

ggsave("output/did/sinistros-golden-km.pdf", width = 10, height = 7.5)


# DIFERENÇA

bind_rows(
  df.trecho |> 
    fit.did(y = "sinistros_diff", formula = ~ 1) |> 
    preparar.grafico() |>
    mutate(controle = FALSE),
  df.trecho |> 
    fit.did(y = "sinistros_diff", formula = ~ tipo_via + faixas + limite_velocidade + comprimento) |> 
    preparar.grafico() |>
    mutate(controle = TRUE)) |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Diferença entre sinistros com e sem moto")

ggsave("output/did/sinistros-diff-total.pdf", width = 10, height = 7.5)


bind_rows(
  df.trecho |> 
    fit.did(y = "sinistros_diff_km", formula = ~ 1) |> 
    preparar.grafico() |>
    mutate(controle = FALSE),
  df.trecho |> 
    fit.did(y = "sinistros_diff_km", formula = ~ tipo_via + faixas + limite_velocidade + comprimento) |> 
    preparar.grafico() |>
    mutate(controle = TRUE)) |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Diferença entre sinistros com e sem moto por km")

ggsave("output/did/sinistros-diff-km.pdf", width = 10, height = 7.5)


bind_rows(
  df.trecho |> 
    fit.did(y = "sinistros_diff_golden", formula = ~ 1) |> 
    preparar.grafico() |>
    mutate(controle = FALSE),
  df.trecho |> 
    fit.did(y = "sinistros_diff_golden", formula = ~ tipo_via + faixas + limite_velocidade + comprimento) |> 
    preparar.grafico() |>
    mutate(controle = TRUE)) |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Diferença entre sinistros com e sem moto (golden)")

ggsave("output/did/sinistros-diff-golden-total.pdf", width = 10, height = 7.5)


bind_rows(
  df.trecho |> 
    fit.did(y = "sinistros_diff_golden_km", formula = ~ 1) |> 
    preparar.grafico() |>
    mutate(controle = FALSE),
  df.trecho |> 
    fit.did(y = "sinistros_diff_golden_km", formula = ~ tipo_via + faixas + limite_velocidade + comprimento) |> 
    preparar.grafico() |>
    mutate(controle = TRUE)) |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Diferença entre sinistros com e sem moto por km (golden)")

ggsave("output/did/sinistros-diff-golden-km.pdf", width = 10, height = 7.5)


### POR LOGRADOURO

logradouros <- trechos |>
  left_join(faixa_azul) |>
  group_by(logradouro_limpo, data_implementacao) |>
  summarize(
    across(
      c(faixas, limite_velocidade),
      ~ .x |> as.numeric() |> mean(na.rm = TRUE)),
    across(
      c(mao_unica, superficie, tipo_via),
      ~ fct_infreq(.x) |> levels() |> first()),
    comprimento = sum(comprimento) |> as.numeric(),
    trechos = n()) |> 
  group_by(logradouro_limpo) |> 
  mutate(logradouro = case_when(n() > 1 ~ str_c(logradouro_limpo, " (", row_number(), ")"),
                                TRUE ~ logradouro_limpo))

df.logradouro <- sinistros |> 
  left_join(match) |> 
  semi_join(trechos, by = join_by(id_osm)) |>
  
  group_by(data = make_date(year = year(data), month = month(data)), logradouro) |>
  summarize(sinistros = n(), 
            sinistros_moto = sum(motocicletas > 0),
            sinistros_moto_golden = sum(motocicletas > 0 & golden_match == TRUE),
            .groups = "drop") |> 
  complete(data, logradouro, fill = list(sinistros = 0, sinistros_moto = 0, sinistros_moto_golden = 0)) |> # Painel balanceado
  left_join(logradouros) |>

  #trasformacao da data em valor numerico (na ordem)
  mutate(mes = data) |>
  pivot_longer(c(mes, data_implementacao)) |>
  mutate(value = value |> reorder(value) |> factor() |> as.numeric()) |>
  pivot_wider(names_from = name, values_from = value) |>
  mutate(data_implementacao = replace_na(data_implementacao, 0),
         logradouro = logradouro |> as.factor() |> as.numeric()) |> 
  mutate(sinistros_km             = sinistros * 1000 / comprimento,
         sinistros_moto_km        = sinistros_moto * 1000 / comprimento,
         sinistros_moto_golden_km = sinistros_moto_golden * 1000 / comprimento) |> 
  mutate(sinistros_diff           = sinistros_moto - sinistros,
         sinistros_diff_km        = sinistros_diff * 1000 / comprimento,
         sinistros_diff_golden    = sinistros_moto_golden - sinistros,
         sinistros_diff_golden_km = sinistros_diff_golden * 1000 / comprimento)


# TOTAL

bind_rows(
  df.logradouro |> 
    fit.did(y = "sinistros", idname = "logradouro", formula = ~ 1, clustervars = c("logradouro")) |> 
    preparar.grafico() |> 
    mutate(controle = FALSE),
  df.logradouro |> 
    fit.did(y = "sinistros", idname = "logradouro", formula = ~ tipo_via + faixas + limite_velocidade + comprimento, clustervars = c("logradouro")) |> 
    preparar.grafico() |> 
    mutate(controle = TRUE)) |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros totais")

ggsave("output/did/logradouro-sinistros-total.pdf", width = 10, height = 7.5)


bind_rows(
  df.logradouro |> 
    fit.did(y = "sinistros_km", idname = "logradouro", formula = ~ 1, clustervars = c("logradouro")) |> 
    preparar.grafico() |> 
    mutate(controle = FALSE),
  df.logradouro |> 
    fit.did(y = "sinistros_km", idname = "logradouro", formula = ~ tipo_via + faixas + limite_velocidade + comprimento, clustervars = c("logradouro")) |> 
    preparar.grafico() |> 
    mutate(controle = TRUE)) |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros por km")

ggsave("output/did/logradouro-sinistros-km.pdf", width = 10, height = 7.5)


# APENAS MOTOS

bind_rows(
  df.logradouro |> 
    fit.did(y = "sinistros_moto", idname = "logradouro", formula = ~ 1, clustervars = c("logradouro")) |> 
    preparar.grafico() |> 
    mutate(controle = FALSE),
  df.logradouro |> 
    fit.did(y = "sinistros_moto", idname = "logradouro", formula = ~ tipo_via + faixas + limite_velocidade + comprimento, clustervars = c("logradouro")) |> 
    preparar.grafico() |> 
    mutate(controle = TRUE)) |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros totais de moto")

ggsave("output/did/logradouro-sinistros-moto-total.pdf", width = 10, height = 7.5)


bind_rows(
  df.logradouro |> 
    fit.did(y = "sinistros_moto_km", idname = "logradouro", formula = ~ 1, clustervars = c("logradouro")) |> 
    preparar.grafico() |> 
    mutate(controle = FALSE),
  df.logradouro |> 
    fit.did(y = "sinistros_moto_km", idname = "logradouro", formula = ~ tipo_via + faixas + limite_velocidade + comprimento, clustervars = c("logradouro")) |> 
    preparar.grafico() |> 
    mutate(controle = TRUE)) |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto por km")

ggsave("output/did/logradouro-sinistros-moto-km.pdf", width = 10, height = 7.5)


# GOLDEN

bind_rows(
  df.logradouro |> 
    fit.did(y = "sinistros_moto_golden", idname = "logradouro", formula = ~ 1, clustervars = c("logradouro")) |> 
    preparar.grafico() |> 
    mutate(controle = FALSE),
  df.logradouro |> 
    fit.did(y = "sinistros_moto_golden", idname = "logradouro", formula = ~ tipo_via + faixas + limite_velocidade + comprimento, clustervars = c("logradouro")) |> 
    preparar.grafico() |> 
    mutate(controle = TRUE)) |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros totais de moto golden")

ggsave("output/did/logradouro-sinistros-moto-golden-total.pdf", width = 10, height = 7.5)


bind_rows(
  df.logradouro |> 
    fit.did(y = "sinistros_moto_km", idname = "logradouro", formula = ~ 1, clustervars = c("logradouro")) |> 
    preparar.grafico() |> 
    mutate(controle = FALSE),
  df.logradouro |> 
    fit.did(y = "sinistros_moto_km", idname = "logradouro", formula = ~ tipo_via + faixas + limite_velocidade + comprimento, clustervars = c("logradouro")) |> 
    preparar.grafico() |> 
    mutate(controle = TRUE)) |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto golden por km")

ggsave("output/did/logradouro-sinistros-moto-golden-km.pdf", width = 10, height = 7.5)


# DIFERENÇA

bind_rows(
  df.logradouro |> 
    fit.did(y = "sinistros_diff", idname = "logradouro", formula = ~ 1, clustervars = c("logradouro")) |> 
    preparar.grafico() |> 
    mutate(controle = FALSE),
  df.logradouro |> 
    fit.did(y = "sinistros_diff", idname = "logradouro", formula = ~ tipo_via + faixas + limite_velocidade + comprimento, clustervars = c("logradouro")) |> 
    preparar.grafico() |> 
    mutate(controle = TRUE)) |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Diferença entre sinistros com e sem moto")

ggsave("output/did/logradouro-sinistros-diff-total.pdf", width = 10, height = 7.5)


bind_rows(
  df.logradouro |> 
    fit.did(y = "sinistros_diff_km", idname = "logradouro", formula = ~ 1, clustervars = c("logradouro")) |> 
    preparar.grafico() |> 
    mutate(controle = FALSE),
  df.logradouro |> 
    fit.did(y = "sinistros_diff_km", idname = "logradouro", formula = ~ tipo_via + faixas + limite_velocidade + comprimento, clustervars = c("logradouro")) |> 
    preparar.grafico() |> 
    mutate(controle = TRUE)) |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Diferença entre sinistros com e sem moto por km")

ggsave("output/did/logradouro-sinistros-diff-km.pdf", width = 10, height = 7.5)


bind_rows(
  df.logradouro |> 
    fit.did(y = "sinistros_diff_golden", idname = "logradouro", formula = ~ 1, clustervars = c("logradouro")) |> 
    preparar.grafico() |> 
    mutate(controle = FALSE),
  df.logradouro |> 
    fit.did(y = "sinistros_diff_golden", idname = "logradouro", formula = ~ tipo_via + faixas + limite_velocidade + comprimento, clustervars = c("logradouro")) |> 
    preparar.grafico() |> 
    mutate(controle = TRUE)) |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Diferença entre sinistros com e sem moto (golden)")

ggsave("output/did/logradouro-sinistros-diff-golden-total.pdf", width = 10, height = 7.5)


bind_rows(
  df.logradouro |> 
    fit.did(y = "sinistros_diff_golden_km", idname = "logradouro", formula = ~ 1, clustervars = c("logradouro")) |> 
    preparar.grafico() |> 
    mutate(controle = FALSE),
  df.logradouro |> 
    fit.did(y = "sinistros_diff_golden_km", idname = "logradouro", formula = ~ tipo_via + faixas + limite_velocidade + comprimento, clustervars = c("logradouro")) |> 
    preparar.grafico() |> 
    mutate(controle = TRUE)) |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Diferença entre sinistros com e sem moto por km (golden)")

ggsave("output/did/logradouro-sinistros-diff-golden-km.pdf", width = 10, height = 7.5)

















# logradouros.sinistros <- read_csv("dados_tratados/infosiga_logradouros.csv")
# logradouros.OSM <- read_csv("dados_tratados/osm_logradouros.csv")
# logradouros.faixa <- readxl::read_excel("dados_tratados/faixa_azul_vias.xlsx")
# 
# 
# data <- logradouros.sinistros |> 
#   distinct(ano, mes) |> 
#   arrange(ano, mes) |> 
#   mutate(periodo = row_number())
# 
# df <- logradouros.sinistros |> 
#   semi_join(logradouros.OSM |> 
#               filter(tipo_via %in% c("trunk", "primary", "secondary"))) |> 
#   left_join(logradouros.OSM) |> 
#   left_join(data) |> 
#   left_join(logradouros.faixa |> 
#               left_join(data) |> 
#               select(logradouro, data_tratamento = periodo)) |>
#   filter(!logradouro == "AVENIDA SANTOS DUMONT") |> 
#   mutate(data_tratamento = replace_na(data_tratamento, 0),
#          logradouro = logradouro |> as.factor() |> as.numeric(),
#          logtamanho = log(tamanho))
# 
# 
# # todos sinistros, sem controle ----
# did <- att_gt(yname = "sinistros",
#               tname = "periodo",
#               idname = "logradouro",
#               gname = "data_tratamento",
#               data = df,
#               xformla = ~ 1,
#               pl = TRUE, cores = 10)
# did |> 
#   aggte() |> 
#   summary()
# 
# did |> 
#   aggte(type = "dynamic", na.rm = T) |> 
#   ggdid() +
#   scale_x_continuous("Meses até o tratamento", limits = c(-30, 30), breaks = (0:6-3)*10) +
#   theme_minimal() +
#   labs(subtitle = "Todos sinistros, sem variáveis de controle")
# 
# ggsave("output/did/event-study.pdf", width = 7, height = 5)
# 
# did |> 
#   ggdid() +
#   theme_minimal() +
#   scale_x_continuous("Meses da análise", breaks = 0:8*10) +
#   labs(subtitle = "Todos sinistros, sem variáveis de controle")
# 
# ggsave("output/did/grupos.pdf", width = 8, height = 15)
# 
# # sinistros moto, sem controle ----
# did.moto <- att_gt(yname = "sinistros_moto",
#               tname = "periodo",
#               idname = "logradouro",
#               gname = "data_tratamento",
#               data = df,
#               xformla = ~ 1,
#               pl = TRUE, cores = 10)
# 
# did.moto |> 
#   aggte(type = "dynamic", na.rm = T) |> 
#   ggdid() +
#   scale_x_continuous("Meses até o tratamento", limits = c(-30, 30), breaks = (0:6-3)*10) +
#   theme_minimal() +
#   labs(subtitle = "Somente sinistros com motocicletas; sem variáveis de controle")
# 
# ggsave("output/did/event-study-moto.pdf", width = 7, height = 5)
# 
# did.moto |> 
#   ggdid() +
#   theme_minimal() +
#   scale_x_continuous("Meses da análise", breaks = 0:8*10) +
#   labs(subtitle = "Somente sinistros com motocicletas; sem variáveis de controle")
# 
# ggsave("output/did/grupos-moto.pdf", width = 8, height = 15)
# 
# # sinistros moto, com controle ----
# 
# did.controle <- att_gt(yname = "sinistros_moto",
#                        tname = "periodo",
#                        idname = "logradouro",
#                        gname = "data_tratamento",
#                        data = df,
#                        xformla = ~ faixas + limite_velocidade + tipo_via + logtamanho)
# 
# did.controle |> 
#   aggte(type = "dynamic", na.rm = T) |> 
#   ggdid() +
#   scale_x_continuous("Meses até o tratamento", limits = c(-30, 30), breaks = (0:6-3)*10) +
#   theme_minimal() +
#   labs(subtitle = "Somente sinistros com motocicletas, com variáveis de controle")
# 
# ggsave("output/did/event-study-moto-controle.pdf", width = 7, height = 5)
# 
# did.controle |> 
#   ggdid() +
#   theme_minimal() +
#   scale_x_continuous("Meses da análise", breaks = 0:8*10) +
#   labs(subtitle = "Com variáveis de controle; somente sinistros com motocicletas")
# 
# ggsave("output/did/grupos-moto-controle.pdf", width = 8, height = 15)
# 
# # diff sinistros, sem controle ----
# 
# did.diff <- att_gt(yname = "diff",
#                        tname = "periodo",
#                        idname = "logradouro",
#                        gname = "data_tratamento",
#                        data = df |> mutate(diff = sinistros - sinistros_moto),
#                        xformla = ~ 1)
# 
# did.diff |> 
#   aggte(type = "dynamic") |> 
#   ggdid() +
#   scale_x_continuous("Meses até o tratamento", limits = c(-30, 30), breaks = (0:6-3)*10) +
#   theme_minimal() +
#   labs(subtitle = "Diferença entre sinistros sem e com motocicletas, sem variáveis de controle")
# 
# ggsave("output/did/event-study-diff.pdf", width = 7, height = 5)
# 
# did.diff |> 
#   ggdid() +
#   theme_minimal() +
#   scale_x_continuous("Meses da análise", breaks = 0:8*10) +
#   labs(subtitle = "Diferença entre sinistros sem e com motocicletas, sem variáveis de controle")
# 
# ggsave("output/did/grupos-diff.pdf", width = 8, height = 15)
