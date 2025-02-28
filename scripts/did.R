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

trechos_complemento <-  read_csv("banco_dados/trechos_complemento.csv", col_types = list(id_osm = "c")) |> 
  mutate(intersec = replace_na(intersec, 0),
         amenidades = replace_na(amenidades, 0))

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
    labs(x = "Meses", y = "Efeito do tratamento",
         title = titulo, colour = "Controles") 
}

horizon <- 12


### POR TRECHO - MENSAL ----

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
  left_join(trechos_complemento) |> 
  mutate(intersec_km = intersec * 1000 / comprimento,
         amenidades_km = amenidades * 1000 / comprimento,
         intersec_f = intersec |> as_factor(),
         amenidades_f = amenidades |> as_factor()) |> 
  
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


# TODOS 

fit <- bind_rows(
  df.trecho |> 
    fit.did(y = "sinistros", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.trecho |> 
    fit.did(y = "sinistros", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor()))

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Todos os sinistros - total")
ggsave("output/did/trecho/todos-total.pdf", width = 10, height = 7.5)
ggsave("output/did/trecho/todos-total.png", width = 10, height = 7.5, dpi = 600)

fit |> 
  filter(abs(egt) <= 18) |> 
  plotar.grafico(titulo = "Todos os sinistros - total")
ggsave("output/did/trecho/todos-total-18.pdf", width = 10, height = 7.5)
ggsave("output/did/trecho/todos-total-18.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.trecho |> 
    fit.did(y = "sinistros_km", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.trecho |> 
    fit.did(y = "sinistros_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor()))

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Todos os sinistros - por km")
ggsave("output/did/trecho/todos-km.pdf", width = 10, height = 7.5)
ggsave("output/did/trecho/todos-km.png", width = 10, height = 7.5, dpi = 600)

fit |> 
  filter(abs(egt) <= 18) |> 
  plotar.grafico(titulo = "Todos os sinistros - por km")
ggsave("output/did/trecho/todos-km-18.pdf", width = 10, height = 7.5)
ggsave("output/did/trecho/todos-km-18.png", width = 10, height = 7.5, dpi = 600)


# MOTOS

fit <- bind_rows(
  df.trecho |> 
    fit.did(y = "sinistros_moto", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.trecho |> 
    fit.did(y = "sinistros_moto", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto - total")
ggsave("output/did/trecho/moto-total.pdf", width = 10, height = 7.5)
ggsave("output/did/trecho/moto-total.png", width = 10, height = 7.5, dpi = 600)

fit |> 
  filter(abs(egt) <= 18) |> 
  plotar.grafico(titulo = "Sinistros de moto - total")
ggsave("output/did/trecho/moto-total-18.pdf", width = 10, height = 7.5)
ggsave("output/did/trecho/moto-total-18.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.trecho |> 
    fit.did(y = "sinistros_moto_km", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.trecho |> 
    fit.did(y = "sinistros_moto_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto - por km")
ggsave("output/did/trecho/moto-km.pdf", width = 10, height = 7.5)
ggsave("output/did/trecho/moto-km.png", width = 10, height = 7.5, dpi = 600)

fit |> 
  filter(abs(egt) <= 18) |> 
  plotar.grafico(titulo = "Sinistros de moto - por km")
ggsave("output/did/trecho/moto-km-18.pdf", width = 10, height = 7.5)
ggsave("output/did/trecho/moto-km-18.png", width = 10, height = 7.5, dpi = 600)


# GOLDEN

fit <- bind_rows(
  df.trecho |> 
    fit.did(y = "sinistros_moto_golden", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.trecho |> 
    fit.did(y = "sinistros_moto_golden", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto golden - total")
ggsave("output/did/trecho/golden-total.pdf", width = 10, height = 7.5)
ggsave("output/did/trecho/golden-total.png", width = 10, height = 7.5, dpi = 600)

fit |> 
  filter(abs(egt) <= 18) |> 
  plotar.grafico(titulo = "Sinistros de moto golden - total")
ggsave("output/did/trecho/golden-total-18.pdf", width = 10, height = 7.5)
ggsave("output/did/trecho/golden-total-18.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.trecho |> 
    fit.did(y = "sinistros_moto_golden_km", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.trecho |> 
    fit.did(y = "sinistros_moto_golden_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto golden - por km")
ggsave("output/did/trecho/golden-km.pdf", width = 10, height = 7.5)
ggsave("output/did/trecho/golden-km.png", width = 10, height = 7.5, dpi = 600)

fit |> 
  filter(abs(egt) <= 18) |> 
  plotar.grafico(titulo = "Sinistros de moto golden - por km")
ggsave("output/did/trecho/golden-km-18.pdf", width = 10, height = 7.5)
ggsave("output/did/trecho/golden-km-18.png", width = 10, height = 7.5, dpi = 600)


# DIFERENÇA

fit <- bind_rows(
  df.trecho |> 
    fit.did(y = "sinistros_diff_golden", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.trecho |> 
    fit.did(y = "sinistros_diff_golden", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Diferença entre sinistros com e sem moto - total")
ggsave("output/did/trecho/diff-total.pdf", width = 10, height = 7.5)
ggsave("output/did/trecho/diff-total.png", width = 10, height = 7.5, dpi = 600)

fit |> 
  filter(abs(egt) <= 18) |> 
  plotar.grafico(titulo = "Diferença entre sinistros com e sem moto - total")
ggsave("output/did/trecho/diff-total-18.pdf", width = 10, height = 7.5)
ggsave("output/did/trecho/diff-total-18.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.trecho |> 
    fit.did(y = "sinistros_diff_golden_km", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.trecho |> 
    fit.did(y = "sinistros_diff_golden_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Diferença entre sinistros com e sem moto - por km")
ggsave("output/did/trecho/diff-km.pdf", width = 10, height = 7.5)
ggsave("output/did/trecho/diff-km.png", width = 10, height = 7.5, dpi = 600)

fit |> 
  filter(abs(egt) <= 18) |> 
  plotar.grafico(titulo = "Diferença entre sinistros com e sem moto - por km")
ggsave("output/did/trecho/diff-km-18.pdf", width = 10, height = 7.5)
ggsave("output/did/trecho/diff-km-18.png", width = 10, height = 7.5, dpi = 600)


### POR LOGRADOURO - MENSAL ----

logradouros <- trechos |>
  left_join(faixa_azul) |>
  left_join(trechos_complemento) |> 
  group_by(logradouro_limpo, data_implementacao) |>
  summarize(
    across(
      c(faixas, limite_velocidade),
      ~ .x |> as.numeric() |> mean(na.rm = TRUE)),
    across(
      c(mao_unica, superficie, tipo_via),
      ~ fct_infreq(.x) |> levels() |> first()),
    comprimento = sum(comprimento) |> as.numeric(),
    intersec = sum(intersec) |> as.numeric(),
    amenidades = sum(amenidades) |> as.numeric(),
    radar_proximo = mean(radar_proximo) |> ceiling() |> as.logical(),
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
  mutate(intersec_km = intersec * 1000 / comprimento,
         amenidades_km = amenidades * 1000 / comprimento,
         intersec_f = intersec |> as_factor(),
         amenidades_f = amenidades |> as_factor()) |> 

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


# TODOS

fit <- bind_rows(
  df.logradouro |> 
    fit.did(y = "sinistros", idname = "logradouro", clustervars = c("logradouro"), formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.logradouro |> 
    fit.did(y = "sinistros", idname = "logradouro", clustervars = c("logradouro"), 
            formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor()))

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Todos os sinistros - total")
ggsave("output/did/logradouro/todos-total.pdf", width = 10, height = 7.5)
ggsave("output/did/logradouro/todos-total.png", width = 10, height = 7.5, dpi = 600)

fit |> 
  filter(abs(egt) <= 18) |> 
  plotar.grafico(titulo = "Todos os sinistros - total")
ggsave("output/did/trecho/todos-total-18.pdf", width = 10, height = 7.5)
ggsave("output/did/trecho/todos-total-18.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.logradouro |> 
    fit.did(y = "sinistros_km", idname = "logradouro", clustervars = c("logradouro"), formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.logradouro |> 
    fit.did(y = "sinistros_km", idname = "logradouro", clustervars = c("logradouro"), 
            formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor()))

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Todos os sinistros - por km")
ggsave("output/did/logradouro/todos-km.pdf", width = 10, height = 7.5)
ggsave("output/did/logradouro/todos-km.png", width = 10, height = 7.5, dpi = 600)

fit |> 
  filter(abs(egt) <= 18) |> 
  plotar.grafico(titulo = "Todos os sinistros - por km")
ggsave("output/did/logradouro/todos-km-18.pdf", width = 10, height = 7.5)
ggsave("output/did/logradouro/todos-km-18.png", width = 10, height = 7.5, dpi = 600)


# MOTOS

fit <- bind_rows(
  df.logradouro |> 
    fit.did(y = "sinistros_moto", idname = "logradouro", clustervars = c("logradouro"), formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.logradouro |> 
    fit.did(y = "sinistros_moto", idname = "logradouro", clustervars = c("logradouro"), 
            formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto - total")
ggsave("output/did/logradouro/moto-total.pdf", width = 10, height = 7.5)
ggsave("output/did/logradouro/moto-total.png", width = 10, height = 7.5, dpi = 600)

fit |> 
  filter(abs(egt) <= 18) |> 
  plotar.grafico(titulo = "Sinistros de moto - total")
ggsave("output/did/logradouro/moto-total-18.pdf", width = 10, height = 7.5)
ggsave("output/did/logradouro/moto-total-18.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.logradouro |> 
    fit.did(y = "sinistros_moto_km", idname = "logradouro", clustervars = c("logradouro"), formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.logradouro |> 
    fit.did(y = "sinistros_moto_km", idname = "logradouro", clustervars = c("logradouro"), 
            formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto - por km")
ggsave("output/did/logradouro/moto-km.pdf", width = 10, height = 7.5)
ggsave("output/did/logradouro/moto-km.png", width = 10, height = 7.5, dpi = 600)

fit |> 
  filter(abs(egt) <= 18) |> 
  plotar.grafico(titulo = "Sinistros de moto - por km")
ggsave("output/did/logradouro/moto-km-18.pdf", width = 10, height = 7.5)
ggsave("output/did/logradouro/moto-km-18.png", width = 10, height = 7.5, dpi = 600)


# GOLDEN

fit <- bind_rows(
  df.logradouro |> 
    fit.did(y = "sinistros_moto_golden", idname = "logradouro", clustervars = c("logradouro"), formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.logradouro |> 
    fit.did(y = "sinistros_moto_golden", idname = "logradouro", clustervars = c("logradouro"), 
            formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto golden - total")
ggsave("output/did/logradouro/golden-total.pdf", width = 10, height = 7.5)
ggsave("output/did/logradouro/golden-total.png", width = 10, height = 7.5, dpi = 600)

fit |> 
  filter(abs(egt) <= 18) |> 
  plotar.grafico(titulo = "Sinistros de moto golden - total")
ggsave("output/did/logradouro/golden-total-18.pdf", width = 10, height = 7.5)
ggsave("output/did/logradouro/golden-total-18.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.logradouro |> 
    fit.did(y = "sinistros_moto_golden_km", idname = "logradouro", clustervars = c("logradouro"), formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.logradouro |> 
    fit.did(y = "sinistros_moto_golden_km", idname = "logradouro", clustervars = c("logradouro"), 
            formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto golden - por km")
ggsave("output/did/logradouro/golden-km.pdf", width = 10, height = 7.5)
ggsave("output/did/logradouro/golden-km.png", width = 10, height = 7.5, dpi = 600)

fit |> 
  filter(abs(egt) <= 18) |> 
  plotar.grafico(titulo = "Sinistros de moto golden - por km")
ggsave("output/did/logradouro/golden-km-18.pdf", width = 10, height = 7.5)
ggsave("output/did/logradouro/golden-km-18.png", width = 10, height = 7.5, dpi = 600)


# DIFERENÇA

fit <- bind_rows(
  df.logradouro |> 
    fit.did(y = "sinistros_diff_golden", idname = "logradouro", clustervars = c("logradouro"), formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.logradouro |> 
    fit.did(y = "sinistros_diff_golden", idname = "logradouro", clustervars = c("logradouro"), 
            formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Diferença entre sinistros com e sem moto - total")
ggsave("output/did/logradouro/diff-total.pdf", width = 10, height = 7.5)
ggsave("output/did/logradouro/diff-total.png", width = 10, height = 7.5, dpi = 600)

fit |> 
  filter(abs(egt) <= 18) |> 
  plotar.grafico(titulo = "Diferença entre sinistros com e sem moto - total")
ggsave("output/did/logradouro/diff-total-18.pdf", width = 10, height = 7.5)
ggsave("output/did/logradouro/diff-total-18.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.logradouro |> 
    fit.did(y = "sinistros_diff_golden_km", idname = "logradouro", clustervars = c("logradouro"), formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.logradouro |> 
    fit.did(y = "sinistros_diff_golden_km", idname = "logradouro", clustervars = c("logradouro"), 
            formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Diferença entre sinistros com e sem moto - por km")
ggsave("output/did/logradouro/diff-km.pdf", width = 10, height = 7.5)
ggsave("output/did/logradouro/diff-km.png", width = 10, height = 7.5, dpi = 600)

fit |> 
  filter(abs(egt) <= 18) |> 
  plotar.grafico(titulo = "Diferença entre sinistros com e sem moto - por km")
ggsave("output/did/logradouro/diff-km-18.pdf", width = 10, height = 7.5)
ggsave("output/did/logradouro/diff-km-18.png", width = 10, height = 7.5, dpi = 600)


### POR TRECHO - BIMESTRAL ----

plotar.grafico <- function(df, titulo = NULL){
  df |> 
    ggplot(aes(x = factor(egt), colour = controle, y = att)) +
    geom_pointrange(aes(ymin = ci_low, ymax = ci_high), position = position_dodge(width = .5), fatten = 1) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = factor(0), alpha = .05, lwd = 3) +
    theme_minimal() +
    labs(x = "Bimestres", y = "Efeito do tratamento",
         title = titulo, colour = "Controles") 
}

horizon <- 8

df.trecho <- sinistros |> 
  left_join(match) |> 
  semi_join(trechos, by = join_by(id_osm)) |> 
  group_by(data = make_date(year = year(data), month = ((month(data) + 1) %/% 2) * 2), id_osm) |> 
  summarize(sinistros = n(), 
            sinistros_moto = sum(motocicletas > 0),
            sinistros_moto_golden = sum(motocicletas > 0 & golden_match == TRUE),
            .groups = "drop") |> 
  complete(data, id_osm, fill = list(sinistros = 0, sinistros_moto = 0, sinistros_moto_golden = 0)) |> # Painel balanceado
  left_join(trechos |> 
              left_join(faixa_azul |> distinct())) |> 
  left_join(trechos_complemento) |> 
  mutate(intersec_km = intersec * 1000 / comprimento,
         amenidades_km = amenidades * 1000 / comprimento,
         intersec_f = intersec |> as_factor(),
         amenidades_f = amenidades |> as_factor()) |> 
  
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


# TODOS 

fit <- bind_rows(
  df.trecho |> 
    fit.did(y = "sinistros", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.trecho |> 
    fit.did(y = "sinistros", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor()))

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Todos os sinistros - total")
ggsave("output/did/trecho/bimestral/todos-total.pdf", width = 10, height = 7.5)
ggsave("output/did/trecho/bimestral/todos-total.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.trecho |> 
    fit.did(y = "sinistros_km", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.trecho |> 
    fit.did(y = "sinistros_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor()))

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Todos os sinistros - por km")
ggsave("output/did/trecho/bimestral/todos-km.pdf", width = 10, height = 7.5)
ggsave("output/did/trecho/bimestral/todos-km.png", width = 10, height = 7.5, dpi = 600)


# MOTOS

fit <- bind_rows(
  df.trecho |> 
    fit.did(y = "sinistros_moto", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.trecho |> 
    fit.did(y = "sinistros_moto", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto - total")
ggsave("output/did/trecho/bimestral/moto-total.pdf", width = 10, height = 7.5)
ggsave("output/did/trecho/bimestral/moto-total.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.trecho |> 
    fit.did(y = "sinistros_moto_km", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.trecho |> 
    fit.did(y = "sinistros_moto_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto - por km")
ggsave("output/did/trecho/bimestral/moto-km.pdf", width = 10, height = 7.5)
ggsave("output/did/trecho/bimestral/moto-km.png", width = 10, height = 7.5, dpi = 600)


# GOLDEN

fit <- bind_rows(
  df.trecho |> 
    fit.did(y = "sinistros_moto_golden", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.trecho |> 
    fit.did(y = "sinistros_moto_golden", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto golden - total")
ggsave("output/did/trecho/bimestral/golden-total.pdf", width = 10, height = 7.5)
ggsave("output/did/trecho/bimestral/golden-total.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.trecho |> 
    fit.did(y = "sinistros_moto_golden_km", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.trecho |> 
    fit.did(y = "sinistros_moto_golden_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto golden - por km")
ggsave("output/did/trecho/bimestral/golden-km.pdf", width = 10, height = 7.5)
ggsave("output/did/trecho/bimestral/golden-km.png", width = 10, height = 7.5, dpi = 600)


# DIFERENÇA

fit <- bind_rows(
  df.trecho |> 
    fit.did(y = "sinistros_diff_golden", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.trecho |> 
    fit.did(y = "sinistros_diff_golden", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Diferença entre sinistros com e sem moto - total")
ggsave("output/did/trecho/bimestral/diff-total.pdf", width = 10, height = 7.5)
ggsave("output/did/trecho/bimestral/diff-total.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.trecho |> 
    fit.did(y = "sinistros_diff_golden_km", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.trecho |> 
    fit.did(y = "sinistros_diff_golden_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Diferença entre sinistros com e sem moto - por km")
ggsave("output/did/trecho/bimestral/diff-km.pdf", width = 10, height = 7.5)
ggsave("output/did/trecho/bimestral/diff-km.png", width = 10, height = 7.5, dpi = 600)


### POR LOGRADOURO - BIMESTRAL ----

plotar.grafico <- function(df, titulo = NULL){
  df |> 
    ggplot(aes(x = factor(egt), colour = controle, y = att)) +
    geom_pointrange(aes(ymin = ci_low, ymax = ci_high), position = position_dodge(width = .5), fatten = 1) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = factor(0), alpha = .05, lwd = 3) +
    theme_minimal() +
    labs(x = "Bimestres", y = "Efeito do tratamento",
         title = titulo, colour = "Controles") 
}

horizon <- 8

logradouros <- trechos |>
  left_join(faixa_azul) |>
  left_join(trechos_complemento) |> 
  group_by(logradouro_limpo, data_implementacao) |>
  summarize(
    across(
      c(faixas, limite_velocidade),
      ~ .x |> as.numeric() |> mean(na.rm = TRUE)),
    across(
      c(mao_unica, superficie, tipo_via),
      ~ fct_infreq(.x) |> levels() |> first()),
    comprimento = sum(comprimento) |> as.numeric(),
    intersec = sum(intersec) |> as.numeric(),
    amenidades = sum(amenidades) |> as.numeric(),
    radar_proximo = mean(radar_proximo) |> ceiling() |> as.logical(),
    trechos = n()) |> 
  group_by(logradouro_limpo) |> 
  mutate(logradouro = case_when(n() > 1 ~ str_c(logradouro_limpo, " (", row_number(), ")"),
                                TRUE ~ logradouro_limpo))

df.logradouro <- sinistros |> 
  left_join(match) |> 
  semi_join(trechos, by = join_by(id_osm)) |>
  
  group_by(data = make_date(year = year(data), month = ((month(data) + 1) %/% 2) * 2), logradouro) |> 
  summarize(sinistros = n(), 
            sinistros_moto = sum(motocicletas > 0),
            sinistros_moto_golden = sum(motocicletas > 0 & golden_match == TRUE),
            .groups = "drop") |> 
  complete(data, logradouro, fill = list(sinistros = 0, sinistros_moto = 0, sinistros_moto_golden = 0)) |> # Painel balanceado
  left_join(logradouros) |>
  mutate(intersec_km = intersec * 1000 / comprimento,
         amenidades_km = amenidades * 1000 / comprimento,
         intersec_f = intersec |> as_factor(),
         amenidades_f = amenidades |> as_factor()) |> 
  
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


# TODOS

fit <- bind_rows(
  df.logradouro |> 
    fit.did(y = "sinistros", idname = "logradouro", clustervars = c("logradouro"), formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.logradouro |> 
    fit.did(y = "sinistros", idname = "logradouro", clustervars = c("logradouro"), 
            formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor()))

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Todos os sinistros - total")
ggsave("output/did/logradouro/bimestral/todos-total.pdf", width = 10, height = 7.5)
ggsave("output/did/logradouro/bimestral/todos-total.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.logradouro |> 
    fit.did(y = "sinistros_km", idname = "logradouro", clustervars = c("logradouro"), formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.logradouro |> 
    fit.did(y = "sinistros_km", idname = "logradouro", clustervars = c("logradouro"), 
            formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor()))

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Todos os sinistros - por km")
ggsave("output/did/logradouro/bimestral/todos-km.pdf", width = 10, height = 7.5)
ggsave("output/did/logradouro/bimestral/todos-km.png", width = 10, height = 7.5, dpi = 600)


# MOTOS

fit <- bind_rows(
  df.logradouro |> 
    fit.did(y = "sinistros_moto", idname = "logradouro", clustervars = c("logradouro"), formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.logradouro |> 
    fit.did(y = "sinistros_moto", idname = "logradouro", clustervars = c("logradouro"), 
            formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto - total")
ggsave("output/did/logradouro/bimestral/moto-total.pdf", width = 10, height = 7.5)
ggsave("output/did/logradouro/bimestral/moto-total.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.logradouro |> 
    fit.did(y = "sinistros_moto_km", idname = "logradouro", clustervars = c("logradouro"), formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.logradouro |> 
    fit.did(y = "sinistros_moto_km", idname = "logradouro", clustervars = c("logradouro"), 
            formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto - por km")
ggsave("output/did/logradouro/bimestral/moto-km.pdf", width = 10, height = 7.5)
ggsave("output/did/logradouro/bimestral/moto-km.png", width = 10, height = 7.5, dpi = 600)


# GOLDEN

fit <- bind_rows(
  df.logradouro |> 
    fit.did(y = "sinistros_moto_golden", idname = "logradouro", clustervars = c("logradouro"), formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.logradouro |> 
    fit.did(y = "sinistros_moto_golden", idname = "logradouro", clustervars = c("logradouro"), 
            formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto golden - total")
ggsave("output/did/logradouro/bimestral/golden-total.pdf", width = 10, height = 7.5)
ggsave("output/did/logradouro/bimestral/golden-total.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.logradouro |> 
    fit.did(y = "sinistros_moto_golden_km", idname = "logradouro", clustervars = c("logradouro"), formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.logradouro |> 
    fit.did(y = "sinistros_moto_golden_km", idname = "logradouro", clustervars = c("logradouro"), 
            formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto golden - por km")
ggsave("output/did/logradouro/bimestral/golden-km.pdf", width = 10, height = 7.5)
ggsave("output/did/logradouro/bimestral/golden-km.png", width = 10, height = 7.5, dpi = 600)


# DIFERENÇA

fit <- bind_rows(
  df.logradouro |> 
    fit.did(y = "sinistros_diff_golden", idname = "logradouro", clustervars = c("logradouro"), formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.logradouro |> 
    fit.did(y = "sinistros_diff_golden", idname = "logradouro", clustervars = c("logradouro"), 
            formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Diferença entre sinistros com e sem moto - total")
ggsave("output/did/logradouro/bimestral/diff-total.pdf", width = 10, height = 7.5)
ggsave("output/did/logradouro/bimestral/diff-total.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.logradouro |> 
    fit.did(y = "sinistros_diff_golden_km", idname = "logradouro", clustervars = c("logradouro"), formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.logradouro |> 
    fit.did(y = "sinistros_diff_golden_km", idname = "logradouro", clustervars = c("logradouro"), 
            formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Diferença entre sinistros com e sem moto - por km")
ggsave("output/did/logradouro/bimestral/diff-km.pdf", width = 10, height = 7.5)
ggsave("output/did/logradouro/bimestral/diff-km.png", width = 10, height = 7.5, dpi = 600)

