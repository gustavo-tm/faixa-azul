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

ifood <- read_csv("sala_segura/20-12-24/dado_disponivel.csv")



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
         title = titulo) 
}

horizon <- 12


############ TESTES NOVOS CONTROLES OSM ----

fit1 <- bind_rows(
  df.trecho |> 
    fit.did(y = "sinistros_moto_golden_km", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não"),
  df.trecho |> 
    fit.did(y = "sinistros_moto_golden_km", formula = ~ tipo_via + faixas + limite_velocidade) |> 
    preparar.grafico() |> 
    mutate(controle = "OSM base")) 
fit1 |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros totais")

fit2 <- bind_rows(
  df.trecho |> 
    fit.did(y = "sinistros_moto_golden_km", formula = ~ intersec_f + radar_proximo + amenidades_f) |> 
    preparar.grafico() |> 
    mutate(controle = "OSM complemento"),
  df.trecho |> 
    fit.did(y = "sinistros_moto_golden_km", formula = ~ tipo_via + faixas + limite_velocidade + intersec_f + radar_proximo + amenidades_f) |> 
    preparar.grafico() |> 
    mutate(controle = "OSM tudo")) 
fit2 |> 
  filter(abs(egt) <= horizon, controle != "OSM interseções") |> 
  plotar.grafico(titulo = "Sinistros totais")

fit3 <- bind_rows(
  df.trecho |> 
    fit.did(y = "sinistros_moto_golden_km", formula = ~ intersec_km + radar_proximo + amenidades_km) |> 
    preparar.grafico() |> 
    mutate(controle = "OSM complemento (km)"),
  df.trecho |> 
    fit.did(y = "sinistros_moto_golden_km", formula = ~ tipo_via + faixas + limite_velocidade + intersec_km + radar_proximo + amenidades_km) |> 
    preparar.grafico() |> 
    mutate(controle = "OSM tudo (km)")) 
fit3 |> 
  filter(abs(egt) <= horizon, controle != "OSM interseções (km)") |> 
  plotar.grafico(titulo = "Sinistros totais")

bind_rows(fit1, fit2, fit3) |> 
  filter(abs(egt) <= horizon, controle %in% c("Não", "OSM base", "OSM tudo", "OSM tudo (km)")) |> 
  mutate(controle = as_factor(controle)) |> 
  plotar.grafico(titulo = "Sinistros golden por km")
ggsave("output/did_het/golden_km_controles.png", width = 10, height = 7.5)


############ FILTRO IFOOD ----

df.reg <- df.trecho |> 
  semi_join(ifood)

fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_moto_golden_km", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = FALSE),
  df.reg |> 
    fit.did(y = "sinistros_moto_golden_km", formula = ~ tipo_via + faixas + limite_velocidade + intersec_km + radar_proximo + amenidades_km) |> 
    preparar.grafico() |> 
    mutate(controle = TRUE)) 
fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros golden por km (iFood)")
ggsave("output/did_het/golden_km_ifood.png", width = 10, height = 7.5)


############ BANDEIRANTES + 23 DE MAIO + JACU PESSEGO vs RESTO ----

avenidas <- c("BANDEIRANTES", "BANDEIRANTES SUL", "JACU PESSEGO", "VINTE E TRES MAIO")


### Apenas ----
df.reg <- df.trecho |> 
  filter(logradouro_limpo %in% avenidas | data_implementacao == 0)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Todos os sinistros - total")
ggsave("output/did_het/trecho/Band + 23 + Jacu/apenas-todos-total.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Band + 23 + Jacu/apenas-todos-total.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_km", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Todos os sinistros - por km")
ggsave("output/did_het/trecho/Band + 23 + Jacu/apenas-todos-km.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Band + 23 + Jacu/apenas-todos-km.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_moto_golden", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_moto_golden", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto golden - total")
ggsave("output/did_het/trecho/Band + 23 + Jacu/apenas-golden-total.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Band + 23 + Jacu/apenas-golden-total.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_moto_golden_km", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_moto_golden_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto golden - por km")
ggsave("output/did_het/trecho/Band + 23 + Jacu/apenas-golden-km.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Band + 23 + Jacu/apenas-golden-km.png", width = 10, height = 7.5, dpi = 600)


### Exceto ----

df.reg <- df.trecho |> 
  filter(!(logradouro_limpo %in% avenidas))


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Todos os sinistros - total")
ggsave("output/did_het/trecho/Band + 23 + Jacu/exceto-todos-total.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Band + 23 + Jacu/exceto-todos-total.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_km", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Todos os sinistros - por km")
ggsave("output/did_het/trecho/Band + 23 + Jacu/exceto-todos-km.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Band + 23 + Jacu/exceto-todos-km.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_moto_golden", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_moto_golden", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto golden - total")
ggsave("output/did_het/trecho/Band + 23 + Jacu/exceto-golden-total.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Band + 23 + Jacu/exceto-golden-total.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_moto_golden_km", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_moto_golden_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto golden - por km")
ggsave("output/did_het/trecho/Band + 23 + Jacu/exceto-golden-km.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Band + 23 + Jacu/exceto-golden-km.png", width = 10, height = 7.5, dpi = 600)


### Sem Jacu Pessego ----

df.reg <- df.trecho |> 
  filter(logradouro_limpo != "JACU PESSEGO")


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_km", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Todos os sinistros - por km")
ggsave("output/did_het/trecho/Band + 23 + Jacu/exceto-jacu-todos-km.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Band + 23 + Jacu/exceto-jacu-todos-km.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_moto_golden_km", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_moto_golden_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto golden - por km")
ggsave("output/did_het/trecho/Band + 23 + Jacu/exceto-jacu-golden-km.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Band + 23 + Jacu/exceto-jacu-golden-km.png", width = 10, height = 7.5, dpi = 600)


############ AVENIDAS POR MESES TRATADOS - 12 ----
max_mes = interval(df.trecho$data |> min() |> as_date(), df.trecho$data |> max() |> as_date()) %/% months(1) + 1
avenidas <- df.trecho |> 
  filter(data_implementacao < max_mes - 12,
         data_implementacao != 0) |> 
  distinct(logradouro_limpo) |> 
  pull(logradouro_limpo)


### Mais que ----

df.reg <- df.trecho |> 
  filter(logradouro_limpo %in% avenidas | data_implementacao == 0)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Todos os sinistros - total")
ggsave("output/did_het/trecho/Meses tratados/12-apenas-todos-total.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Meses tratados/12-apenas-todos-total.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_km", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Todos os sinistros - por km")
ggsave("output/did_het/trecho/Meses tratados/12-apenas-todos-km.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Meses tratados/12-apenas-todos-km.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_moto_golden", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_moto_golden", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto golden - total")
ggsave("output/did_het/trecho/Meses tratados/12-apenas-golden-total.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Meses tratados/12-apenas-golden-total.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_moto_golden_km", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_moto_golden_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto golden - por km")
ggsave("output/did_het/trecho/Meses tratados/12-apenas-golden-km.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Meses tratados/12-apenas-golden-km.png", width = 10, height = 7.5, dpi = 600)


### Ate ----

df.reg <- df.trecho |> 
  filter(!(logradouro_limpo %in% avenidas))


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Todos os sinistros - total")
ggsave("output/did_het/trecho/Meses tratados/12-exceto-todos-total.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Meses tratados/12-exceto-todos-total.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_km", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Todos os sinistros - por km")
ggsave("output/did_het/trecho/Meses tratados/12-exceto-todos-km.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Meses tratados/12-exceto-todos-km.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_moto_golden", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_moto_golden", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto golden - total")
ggsave("output/did_het/trecho/Meses tratados/12-exceto-golden-total.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Meses tratados/12-exceto-golden-total.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_moto_golden_km", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_moto_golden_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto golden - por km")
ggsave("output/did_het/trecho/Meses tratados/12-exceto-golden-km.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Meses tratados/12-exceto-golden-km.png", width = 10, height = 7.5, dpi = 600)


############ AVENIDAS POR MESES TRATADOS - 15 ----
max_mes = interval(df.trecho$data |> min() |> as_date(), df.trecho$data |> max() |> as_date()) %/% months(1) + 1
avenidas <- df.trecho |> 
  filter(data_implementacao < max_mes - 15,
         data_implementacao != 0) |> 
  distinct(logradouro_limpo) |> 
  pull(logradouro_limpo)


### Mais que ----

df.reg <- df.trecho |> 
  filter(logradouro_limpo %in% avenidas | data_implementacao == 0)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Todos os sinistros - total")
ggsave("output/did_het/trecho/Meses tratados/15-apenas-todos-total.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Meses tratados/15-apenas-todos-total.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_km", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Todos os sinistros - por km")
ggsave("output/did_het/trecho/Meses tratados/15-apenas-todos-km.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Meses tratados/15-apenas-todos-km.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_moto_golden", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_moto_golden", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto golden - total")
ggsave("output/did_het/trecho/Meses tratados/15-apenas-golden-total.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Meses tratados/15-apenas-golden-total.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_moto_golden_km", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_moto_golden_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto golden - por km")
ggsave("output/did_het/trecho/Meses tratados/15-apenas-golden-km.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Meses tratados/15-apenas-golden-km.png", width = 10, height = 7.5, dpi = 600)


### Ate ----

df.reg <- df.trecho |> 
  filter(!(logradouro_limpo %in% avenidas))


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Todos os sinistros - total")
ggsave("output/did_het/trecho/Meses tratados/15-exceto-todos-total.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Meses tratados/15-exceto-todos-total.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_km", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Todos os sinistros - por km")
ggsave("output/did_het/trecho/Meses tratados/15-exceto-todos-km.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Meses tratados/15-exceto-todos-km.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_moto_golden", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_moto_golden", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto golden - total")
ggsave("output/did_het/trecho/Meses tratados/15-exceto-golden-total.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Meses tratados/15-exceto-golden-total.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_moto_golden_km", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_moto_golden_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto golden - por km")
ggsave("output/did_het/trecho/Meses tratados/15-exceto-golden-km.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Meses tratados/15-exceto-golden-km.png", width = 10, height = 7.5, dpi = 600)


############ AVENIDAS POR NÚMERO DE FAIXAS - 2 ----

### Ate ----

df.reg <- df.trecho |> 
  mutate(faixas = as.integer(faixas)) |> 
  filter(faixas <= 2)

df.reg |> filter(data == "2024-12-01") |> count(data_implementacao)

fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Todos os sinistros - total")
ggsave("output/did_het/trecho/Numero de faixas/2-ate-todos-total.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Numero de faixas/2-ate-todos-total.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_km", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Todos os sinistros - por km")
ggsave("output/did_het/trecho/Numero de faixas/2-ate-todos-km.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Numero de faixas/2-ate-todos-km.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_moto_golden", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_moto_golden", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto golden - total")
ggsave("output/did_het/trecho/Numero de faixas/2-ate-golden-total.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Numero de faixas/2-ate-golden-total.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_moto_golden_km", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_moto_golden_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto golden - por km")
ggsave("output/did_het/trecho/Numero de faixas/2-ate-golden-km.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Numero de faixas/2-ate-golden-km.png", width = 10, height = 7.5, dpi = 600)


### Mais que ----

df.reg <- df.trecho |> 
  mutate(faixas = as.integer(faixas)) |> 
  filter(faixas > 2)

df.reg |> filter(data == "2024-12-01") |> count(data_implementacao)

fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Todos os sinistros - total")
ggsave("output/did_het/trecho/Numero de faixas/2-maisQue-todos-total.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Numero de faixas/2-maisQue-todos-total.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_km", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Todos os sinistros - por km")
ggsave("output/did_het/trecho/Numero de faixas/2-maisQue-todos-km.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Numero de faixas/2-maisQue-todos-km.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_moto_golden", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_moto_golden", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto golden - total")
ggsave("output/did_het/trecho/Numero de faixas/2-maisQue-golden-total.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Numero de faixas/2-maisQue-golden-total.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_moto_golden_km", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_moto_golden_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto golden - por km")
ggsave("output/did_het/trecho/Numero de faixas/2-maisQue-golden-km.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Numero de faixas/2-maisQue-golden-km.png", width = 10, height = 7.5, dpi = 600)


############ AVENIDAS POR NÚMERO DE FAIXAS - 3 ----

### Ate ----

df.reg <- df.trecho |> 
  mutate(faixas = as.integer(faixas)) |> 
  filter(faixas <= 3)

df.reg |> filter(data == "2024-12-01") |> count(data_implementacao)

fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Todos os sinistros - total")
ggsave("output/did_het/trecho/Numero de faixas/3-ate-todos-total.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Numero de faixas/3-ate-todos-total.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_km", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Todos os sinistros - por km")
ggsave("output/did_het/trecho/Numero de faixas/3-ate-todos-km.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Numero de faixas/3-ate-todos-km.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_moto_golden", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_moto_golden", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto golden - total")
ggsave("output/did_het/trecho/Numero de faixas/3-ate-golden-total.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Numero de faixas/3-ate-golden-total.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_moto_golden_km", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_moto_golden_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto golden - por km")
ggsave("output/did_het/trecho/Numero de faixas/3-ate-golden-km.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Numero de faixas/3-ate-golden-km.png", width = 10, height = 7.5, dpi = 600)


### Mais que ----

df.reg <- df.trecho |> 
  mutate(faixas = as.integer(faixas)) |> 
  filter(faixas > 3)

df.reg |> filter(data == "2024-12-01") |> count(data_implementacao)

fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Todos os sinistros - total")
ggsave("output/did_het/trecho/Numero de faixas/3-maisQue-todos-total.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Numero de faixas/3-maisQue-todos-total.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_km", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Todos os sinistros - por km")
ggsave("output/did_het/trecho/Numero de faixas/3-maisQue-todos-km.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Numero de faixas/3-maisQue-todos-km.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_moto_golden", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_moto_golden", formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto golden - total")
ggsave("output/did_het/trecho/Numero de faixas/3-maisQue-golden-total.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Numero de faixas/3-maisQue-golden-total.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_moto_golden_km", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_moto_golden_km", formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto golden - por km")
ggsave("output/did_het/trecho/Numero de faixas/3-maisQue-golden-km.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Numero de faixas/3-maisQue-golden-km.png", width = 10, height = 7.5, dpi = 600)


############ TIPO DE VIA ----

### Primary ----

df.reg <- df.trecho |> 
  filter(tipo_via == "primary")

df.reg |> filter(data == "2024-12-01") |> count(data_implementacao)

fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros", formula = ~ faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Todos os sinistros - total")
ggsave("output/did_het/trecho/Tipo de via/primary-todos-total.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Tipo de via/primary-todos-total.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_km", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_km", formula = ~ faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Todos os sinistros - por km")
ggsave("output/did_het/trecho/Tipo de via/primary-todos-km.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Tipo de via/primary-todos-km.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_moto_golden", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_moto_golden", formula = ~ faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto golden - total")
ggsave("output/did_het/trecho/Tipo de via/primary-golden-total.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Tipo de via/primary-golden-total.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_moto_golden_km", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_moto_golden_km", formula = ~ faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto golden - por km")
ggsave("output/did_het/trecho/Tipo de via/primary-golden-km.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Tipo de via/primary-golden-km.png", width = 10, height = 7.5, dpi = 600)


### Secondary ----

# df.reg <- df.trecho |> 
#   filter(tipo_via == "secondary")
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
# ggsave("output/did_het/trecho/Tipo de via/secondary-todos-total.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Tipo de via/secondary-todos-total.png", width = 10, height = 7.5, dpi = 600)
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
# ggsave("output/did_het/trecho/Tipo de via/secondary-todos-km.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Tipo de via/secondary-todos-km.png", width = 10, height = 7.5, dpi = 600)
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
# ggsave("output/did_het/trecho/Tipo de via/secondary-golden-total.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Tipo de via/secondary-golden-total.png", width = 10, height = 7.5, dpi = 600)
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
# ggsave("output/did_het/trecho/Tipo de via/secondary-golden-km.pdf", width = 10, height = 7.5)
# ggsave("output/did_het/trecho/Tipo de via/secondary-golden-km.png", width = 10, height = 7.5, dpi = 600)


### Trunk ----

df.reg <- df.trecho |> 
  filter(tipo_via == "trunk")

df.reg |> filter(data == "2024-12-01") |> count(data_implementacao)

fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros", formula = ~ faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Todos os sinistros - total")
ggsave("output/did_het/trecho/Tipo de via/trunk-todos-total.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Tipo de via/trunk-todos-total.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_km", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_km", formula = ~ faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Todos os sinistros - por km")
ggsave("output/did_het/trecho/Tipo de via/trunk-todos-km.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Tipo de via/trunk-todos-km.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_moto_golden", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_moto_golden", formula = ~ faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto golden - total")
ggsave("output/did_het/trecho/Tipo de via/trunk-golden-total.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Tipo de via/trunk-golden-total.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_moto_golden_km", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_moto_golden_km", formula = ~ faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto golden - por km")
ggsave("output/did_het/trecho/Tipo de via/trunk-golden-km.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Tipo de via/trunk-golden-km.png", width = 10, height = 7.5, dpi = 600)


############ VELOCIDADE MAXIMA - 40km/h ----

### Ate ----

df.reg <- df.trecho |> 
  mutate(limite_velocidade = as.integer(limite_velocidade)) |> 
  filter(limite_velocidade <= 40)

df.reg |> filter(data == "2024-12-01") |> count(data_implementacao)

fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros", formula = ~ faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Todos os sinistros - total")
ggsave("output/did_het/trecho/Velocidade maxima/40-ate-todos-total.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Velocidade maxima/40-ate-todos-total.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_km", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_km", formula = ~ faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Todos os sinistros - por km")
ggsave("output/did_het/trecho/Velocidade maxima/40-ate-todos-km.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Velocidade maxima/40-ate-todos-km.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_moto_golden", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_moto_golden", formula = ~ faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto golden - total")
ggsave("output/did_het/trecho/Velocidade maxima/40-ate-golden-total.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Velocidade maxima/40-ate-golden-total.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_moto_golden_km", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_moto_golden_km", formula = ~ faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto golden - por km")
ggsave("output/did_het/trecho/Velocidade maxima/40-ate-golden-km.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Velocidade maxima/40-ate-golden-km.png", width = 10, height = 7.5, dpi = 600)


### Mais que ----

df.reg <- df.trecho |> 
  mutate(limite_velocidade = as.integer(limite_velocidade)) |> 
  filter(limite_velocidade > 40)

df.reg |> filter(data == "2024-12-01") |> count(data_implementacao)

fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros", formula = ~ faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Todos os sinistros - total")
ggsave("output/did_het/trecho/Velocidade maxima/40-maisQue-todos-total.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Velocidade maxima/40-maisQue-todos-total.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_km", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_km", formula = ~ faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Todos os sinistros - por km")
ggsave("output/did_het/trecho/Velocidade maxima/40-maisQue-todos-km.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Velocidade maxima/40-maisQue-todos-km.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_moto_golden", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_moto_golden", formula = ~ faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto golden - total")
ggsave("output/did_het/trecho/Velocidade maxima/40-maisQue-golden-total.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Velocidade maxima/40-maisQue-golden-total.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_moto_golden_km", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_moto_golden_km", formula = ~ faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto golden - por km")
ggsave("output/did_het/trecho/Velocidade maxima/40-maisQue-golden-km.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Velocidade maxima/40-maisQue-golden-km.png", width = 10, height = 7.5, dpi = 600)


############ VELOCIDADE MAXIMA - 50km/h ----

### Ate ----

df.reg <- df.trecho |> 
  mutate(limite_velocidade = as.integer(limite_velocidade)) |> 
  filter(limite_velocidade <= 50)

df.reg |> filter(data == "2024-12-01") |> count(data_implementacao)

fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros", formula = ~ faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Todos os sinistros - total")
ggsave("output/did_het/trecho/Velocidade maxima/50-ate-todos-total.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Velocidade maxima/50-ate-todos-total.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_km", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_km", formula = ~ faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Todos os sinistros - por km")
ggsave("output/did_het/trecho/Velocidade maxima/50-ate-todos-km.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Velocidade maxima/50-ate-todos-km.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_moto_golden", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_moto_golden", formula = ~ faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto golden - total")
ggsave("output/did_het/trecho/Velocidade maxima/50-ate-golden-total.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Velocidade maxima/50-ate-golden-total.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_moto_golden_km", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_moto_golden_km", formula = ~ faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto golden - por km")
ggsave("output/did_het/trecho/Velocidade maxima/50-ate-golden-km.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Velocidade maxima/50-ate-golden-km.png", width = 10, height = 7.5, dpi = 600)


### Mais que ----

df.reg <- df.trecho |> 
  mutate(limite_velocidade = as.integer(limite_velocidade)) |> 
  filter(limite_velocidade > 50)

df.reg |> filter(data == "2024-12-01") |> count(data_implementacao)

fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros", formula = ~ faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Todos os sinistros - total")
ggsave("output/did_het/trecho/Velocidade maxima/50-maisQue-todos-total.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Velocidade maxima/50-maisQue-todos-total.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_km", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_km", formula = ~ faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Todos os sinistros - por km")
ggsave("output/did_het/trecho/Velocidade maxima/50-maisQue-todos-km.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Velocidade maxima/50-maisQue-todos-km.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_moto_golden", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_moto_golden", formula = ~ faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto golden - total")
ggsave("output/did_het/trecho/Velocidade maxima/50-maisQue-golden-total.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Velocidade maxima/50-maisQue-golden-total.png", width = 10, height = 7.5, dpi = 600)


fit <- bind_rows(
  df.reg |> 
    fit.did(y = "sinistros_moto_golden_km", formula = ~ 1) |> 
    preparar.grafico() |> 
    mutate(controle = "Não" |> as_factor()),
  df.reg |> 
    fit.did(y = "sinistros_moto_golden_km", formula = ~ faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km) |> 
    preparar.grafico() |> 
    mutate(controle = "Sim" |> as_factor())) 

fit |> 
  filter(abs(egt) <= horizon) |> 
  plotar.grafico(titulo = "Sinistros de moto golden - por km")
ggsave("output/did_het/trecho/Velocidade maxima/50-maisQue-golden-km.pdf", width = 10, height = 7.5)
ggsave("output/did_het/trecho/Velocidade maxima/50-maisQue-golden-km.png", width = 10, height = 7.5, dpi = 600)

