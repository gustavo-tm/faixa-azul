library(tidyverse)
library(sf)
library(did)

match <- read_csv("banco_dados/match_geografico.csv", col_types = list(id_osm = "c"))

trechos <- st_read("banco_dados/trechos.gpkg") |> 
  st_drop_geometry() |> 
  as_tibble() |> 
  select(id_osm, logradouro, tipo_via, faixas, limite_velocidade, mao_unica, superficie, comprimento) |> 
  filter(tipo_via %in% c("trunk", "primary", "secondary"))

faixa_azul <- read_csv("banco_dados/faixa_azul.csv", col_types = list(id_osm = "c"))

sinistros <- read_csv("banco_dados/sinistros.csv")

df.trecho <- match |> 
  left_join(sinistros) |> 
  filter(year(data) >= 2019, tipo != "NOTIFICACAO") |> # Antes de 2019 há apenas sinistros com óbito
  semi_join(trechos, by = join_by(id_osm)) |> 
  group_by(data = make_date(year = year(data), month = month(data)), id_osm) |> 
  summarize(sinistros = n(), 
            sinistros_moto = sum(motocicletas > 0),
            .groups = "drop") |> 
  complete(data, id_osm, fill = list(sinistros = 0, sinistros_moto = 0)) |> # Painel balanceado
  left_join(trechos |> 
              left_join(faixa_azul |> distinct())) |> 
  
  #trasformacao da data em valor numerico (na ordem)
  mutate(mes = data) |> 
  pivot_longer(c(mes, data_implementacao)) |> 
  mutate(value = value |> reorder(value) |> factor() |> as.numeric()) |> 
  pivot_wider(names_from = name, values_from = value) |> 
  mutate(data_implementacao = replace_na(data_implementacao, 0),
         id_osm = as.numeric(id_osm))


preparar.grafico <- function(df, y = "sinistros", clustervars = c("id_osm", "logradouro"), formula = ~ tipo_via + faixas + limite_velocidade, idname = "id_osm"){
  att_gt(yname = y,
         tname = "mes",
         idname = idname,
         gname = "data_implementacao",
         clustervars = clustervars,
         data = df,
         xformla = formula) |>
    aggte(type = "dynamic", na.rm = T) |> 
    (\(result) tibble(egt = result$egt, 
                      att = result$att.egt, 
                      se = result$se.egt, 
                      crit_val = result$crit.val.egt))(result = _) |> 
    mutate(ci_low = att - crit_val * se,
           ci_high = att + crit_val * se)
  # left_join(df.trecho |> 
  #             distinct(data, mes) |> 
  #             rename(egt = mes))
}

plotar.grafico <- function(df, titulo = NULL){
  df |> 
    ggplot(aes(x = factor(egt), colour = controle, y = att)) +
    geom_pointrange(aes(ymin = ci_low, ymax = ci_high), position = position_dodge(width = .5), fatten = 1) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = factor(0), alpha = .05, lwd = 5) +
    theme_minimal() +
    labs(x = "Meses ao tratamento", y = "Efeito do tratamento",
         title = titulo) 
}


# TOTAL

bind_rows(
  df.trecho |> 
    preparar.grafico(formula = ~ 1) |> 
    mutate(controle = FALSE),
  df.trecho |> 
    preparar.grafico(formula = ~ tipo_via + faixas + limite_velocidade) |> 
    mutate(controle = TRUE)) |> 
  filter(abs(egt) <= 12) |> 
  plotar.grafico(titulo = "Sinistros totais")

ggsave("output/did/sinistros.pdf", width = 8, height = 6)


bind_rows(
  df.trecho |> 
    mutate(sinistros_km = sinistros * 1000 / comprimento) |> 
    preparar.grafico(formula = ~ 1, y = "sinistros_km") |> 
    mutate(controle = FALSE),
  df.trecho |> 
    mutate(sinistros_km = sinistros * 1000 / comprimento) |>
    preparar.grafico(formula = ~ tipo_via + faixas + limite_velocidade, y = "sinistros_km") |> 
    mutate(controle = TRUE)) |> 
  filter(abs(egt) <= 12) |> 
  plotar.grafico(titulo = "Sinistros por km")


ggsave("output/did/sinistros-km.pdf", width = 8, height = 6)


# APENAS MOTOS

bind_rows(
  df.trecho |> 
    preparar.grafico(formula = ~ 1, y = "sinistros_moto") |> 
    mutate(controle = FALSE),
  df.trecho |> 
    preparar.grafico(formula = ~ tipo_via + faixas + limite_velocidade, y = "sinistros_moto") |> 
    mutate(controle = TRUE)) |> 
  filter(abs(egt) <= 12) |> 
  plotar.grafico(titulo = "Sinistros totais de moto")


ggsave("output/did/sinistros-moto.pdf", width = 8, height = 6)


bind_rows(
  df.trecho |> 
    mutate(sinistros_moto_km = sinistros_moto * 1000 / comprimento) |> 
    preparar.grafico(formula = ~ 1, y = "sinistros_moto_km") |> 
    mutate(controle = FALSE),
  df.trecho |> 
    mutate(sinistros_moto_km = sinistros_moto * 1000 / comprimento) |>
    preparar.grafico(formula = ~ tipo_via + faixas + limite_velocidade, y = "sinistros_moto_km") |> 
    mutate(controle = TRUE)) |> 
  filter(abs(egt) <= 12) |> 
  plotar.grafico(titulo = "Sinistros de moto por km")


ggsave("output/did/sinistros-moto-km.pdf", width = 8, height = 6)

# DIFERENÇA

bind_rows(
  df.trecho |> 
    mutate(diff = (sinistros - sinistros_moto) - sinistros_moto) |> #sinistro sem moto - sinistro com moto
    preparar.grafico(formula = ~ 1, y = "diff") |> 
    mutate(controle = FALSE),
  df.trecho |> 
    mutate(diff = (sinistros - sinistros_moto) - sinistros_moto) |> #sinistro sem moto - sinistro com moto
    preparar.grafico(formula = ~ tipo_via + faixas + limite_velocidade + comprimento, y = "diff") |> 
    mutate(controle = TRUE)) |> 
  filter(abs(egt) <= 12) |> 
  plotar.grafico(titulo = "Diferença entre sinistros sem e com moto")


ggsave("output/did/sinistros-diff.pdf", width = 8, height = 6)


bind_rows(
  df.trecho |> 
    mutate(diff = (sinistros - sinistros_moto) - sinistros_moto,
           diff_km = diff * 1000 / comprimento) |> #sinistro sem moto - sinistro com moto
    preparar.grafico(formula = ~ 1, y = "diff_km") |> 
    mutate(controle = FALSE),
  df.trecho |> 
    mutate(diff = (sinistros - sinistros_moto) - sinistros_moto,
           diff_km = diff * 1000 / comprimento) |> #sinistro sem moto - sinistro com moto
    preparar.grafico(formula = ~ tipo_via + faixas + limite_velocidade + comprimento, y = "diff_km") |> 
    mutate(controle = TRUE)) |> 
  filter(abs(egt) <= 12) |> 
  plotar.grafico(titulo = "Diferença entre sinistros sem e com moto por km")


ggsave("output/did/sinistros-diff-km.pdf", width = 8, height = 6)









logradouros <- trechos|>
  left_join(faixa_azul) |>
  mutate(logradouro = logradouro |>
           stringi::stri_trans_general("latin-ascii") |>
           str_to_upper() |>
           str_replace_all("[[:punct:]]", "")) |>
  group_by(logradouro) |>
  summarize(
    across(
      c(faixas, limite_velocidade),
      ~ .x |> as.numeric() |> mean(na.rm = TRUE) |> round(2)),
    across(
      c(mao_unica, superficie, tipo_via),
      ~ fct_infreq(.x) |> levels() |> first()),
    comprimento = sum(comprimento) |> as.numeric(),
    data_implementacao = first(data_implementacao))


df.logradouro <- sinistros |>
  filter(year(data) >= 2018) |>
  group_by(data = make_date(year = year(data), month = month(data)), logradouro) |>
  summarize(sinistros = n(), 
            sinistros_moto = sum(motocicletas > 0),
            .groups = "drop") |> 
  complete(data, logradouro, fill = list(sinistros = 0, sinistros_moto = 0)) |>
  right_join(logradouros) |>

  #trasformacao da data em valor numerico (na ordem)
  mutate(mes = data) |>
  pivot_longer(c(mes, data_implementacao)) |>
  mutate(value = value |> reorder(value) |> factor() |> as.numeric()) |>
  pivot_wider(names_from = name, values_from = value) |>
  mutate(data_implementacao = replace_na(data_implementacao, 0),
         logradouro = logradouro |> as.factor() |> as.numeric())




bind_rows(
  df.logradouro |> 
    preparar.grafico(formula = ~ 1, clustervars = c("logradouro"), idname = "logradouro") |> 
    mutate(controle = FALSE),
  df.logradouro |> 
    preparar.grafico(formula = ~ tipo_via + faixas + limite_velocidade + comprimento, clustervars = c("logradouro"), idname = "logradouro") |> 
    mutate(controle = TRUE)) |> 
  filter(abs(egt) <= 12) |> 
  plotar.grafico(titulo = "Sinistros totais")

ggsave("output/did/logradouro-sinistros.pdf", width = 8, height = 6)


bind_rows(
  df.logradouro |> 
    mutate(sinistros_km = sinistros * 1000 / comprimento) |> 
    preparar.grafico(formula = ~ 1, y = "sinistros_km", clustervars = c("logradouro"), idname = "logradouro") |> 
    mutate(controle = FALSE),
  df.logradouro |> 
    mutate(sinistros_km = sinistros * 1000 / comprimento) |>
    preparar.grafico(formula = ~ tipo_via + faixas + limite_velocidade + comprimento, y = "sinistros_km", clustervars = c("logradouro"), idname = "logradouro") |> 
    mutate(controle = TRUE)) |> 
  filter(abs(egt) <= 12) |> 
  plotar.grafico(titulo = "Sinistros por km")


ggsave("output/did/logradouro-sinistros-km.pdf", width = 8, height = 6)


# APENAS MOTOS

bind_rows(
  df.logradouro |> 
    preparar.grafico(formula = ~ 1, y = "sinistros_moto", clustervars = c("logradouro"), idname = "logradouro") |> 
    mutate(controle = FALSE),
  df.logradouro |> 
    preparar.grafico(formula = ~ tipo_via + faixas + limite_velocidade + comprimento, y = "sinistros_moto", clustervars = c("logradouro"), idname = "logradouro") |> 
    mutate(controle = TRUE)) |> 
  filter(abs(egt) <= 12) |> 
  plotar.grafico(titulo = "Sinistros totais de moto")


ggsave("output/did/logradouro-sinistros-moto.pdf", width = 8, height = 6)


bind_rows(
  df.logradouro |> 
    mutate(sinistros_moto_km = sinistros_moto * 1000 / comprimento) |> 
    preparar.grafico(formula = ~ 1, y = "sinistros_moto_km", clustervars = c("logradouro"), idname = "logradouro") |> 
    mutate(controle = FALSE),
  df.logradouro |> 
    mutate(sinistros_moto_km = sinistros_moto * 1000 / comprimento) |>
    preparar.grafico(formula = ~ tipo_via + faixas + limite_velocidade + comprimento, y = "sinistros_moto_km", clustervars = c("logradouro"), idname = "logradouro") |> 
    mutate(controle = TRUE)) |> 
  filter(abs(egt) <= 12) |> 
  plotar.grafico(titulo = "Sinistros de moto por km")


ggsave("output/did/logradouro-sinistros-moto-km.pdf", width = 8, height = 6)

# DIFERENÇA

bind_rows(
  df.logradouro |> 
    mutate(diff = (sinistros - sinistros_moto) - sinistros_moto) |> #sinistro sem moto - sinistro com moto
    preparar.grafico(formula = ~ 1, y = "diff", clustervars = c("logradouro"), idname = "logradouro") |> 
    mutate(controle = FALSE),
  df.logradouro |> 
    mutate(diff = (sinistros - sinistros_moto) - sinistros_moto) |> #sinistro sem moto - sinistro com moto
    preparar.grafico(formula = ~ tipo_via + faixas + limite_velocidade + comprimento, y = "diff", clustervars = c("logradouro"), idname = "logradouro") |> 
    mutate(controle = TRUE)) |> 
  filter(abs(egt) <= 12) |> 
  plotar.grafico(titulo = "Diferença entre sinistros sem e com moto")


ggsave("output/did/logradouro-sinistros-diff.pdf", width = 8, height = 6)


bind_rows(
  df.logradouro |> 
    mutate(diff = (sinistros - sinistros_moto) - sinistros_moto,
           diff_km = diff * 1000 / comprimento) |> #sinistro sem moto - sinistro com moto
    preparar.grafico(formula = ~ 1, y = "diff_km", clustervars = c("logradouro"), idname = "logradouro") |> 
    mutate(controle = FALSE),
  df.logradouro |> 
    mutate(diff = (sinistros - sinistros_moto) - sinistros_moto,
           diff_km = diff * 1000 / comprimento) |> #sinistro sem moto - sinistro com moto
    preparar.grafico(formula = ~ tipo_via + faixas + limite_velocidade + comprimento, y = "diff_km", clustervars = c("logradouro"), idname = "logradouro") |> 
    mutate(controle = TRUE)) |> 
  filter(abs(egt) <= 12) |> 
  plotar.grafico(titulo = "Diferença entre sinistros sem e com moto por km")


ggsave("output/did/logradouro-sinistros-diff-km.pdf", width = 8, height = 6)

















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
