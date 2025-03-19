library(tidyverse)
library(sf)
library(did)
library(scales)

rm(list = ls())
setwd("T:/1285 - Faixa Azul")


###### INPUT ----
match <- read_csv("Entrada/dados/match.csv", col_types = list(id_osm = "c")) |> 
  mutate(golden_match = 
           similaridade > .75 & 
           distancia_geografica < 100 &
           match_titulo == TRUE &
           match_tipo == TRUE)

trechos <- st_read("Entrada/dados/trechos.gpkg") |> 
  st_drop_geometry() |> 
  as_tibble() |> 
  filter(tipo_via %in% c("trunk", "primary", "secondary")) |> 
  left_join(read_csv("Entrada/dados/osm-token.csv", col_types = list(id_osm = "c")) |> 
              select(id_osm, logradouro_limpo) |> 
              mutate(len = str_length(logradouro_limpo)) |> 
              group_by(id_osm) |> 
              arrange(-len) |> 
              summarize(logradouro_limpo = nth(logradouro_limpo, 1), .groups = "drop")) |> 
  select(id_osm, logradouro, logradouro_limpo, tipo_via, faixas, limite_velocidade, mao_unica, superficie, comprimento)

trechos_complemento <- read.csv("Entrada/dados/trechos_complemento.csv") |> 
  mutate(id_osm = as.character(id_osm),
         amenidades = replace_na(amenidades, 0))

faixa_azul <- read_csv("Entrada/dados/faixa_azul.csv", col_types = list(id_osm = "c")) |> 
  distinct() |> 
  filter(data_implementacao >= "2023-06-01" & data_implementacao <= "2024-08-01")

sinistros <- read_csv("Entrada/dados/sinistros.csv") |> 
  filter(year(data) >= 2019, tipo != "NOTIFICACAO") |> # Antes de 2019 há apenas sinistros com óbito
  select(id_sinistro, data, quantidade_envolvidos, motocicletas)

ifood <- read_csv("T:/iFood - Sala Segura/iFood_Velocidade_Media.csv") %>% 
  mutate(data = as_date(month_partition, format = "%Y-%m")) %>% 
  select(id_osm = osm_id, data, rotas = routes, velocidade_media = average_velocity)


###### MANIPULACAO ----

flag_sem10 <- FALSE
flag_preenchimento <- FALSE
flag_vmedia <- FALSE
flag_comprimento <- FALSE
flag_apenas_tratados <- FALSE

value_comprimento = 300
value_comprimento/15*3.6





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

tratados <- trechos |> 
  left_join(faixa_azul) |> 
  filter(!is.na(data_implementacao)) |> 
  distinct(logradouro) |> 
  pull(logradouro)
tratados_limpo <- trechos |> 
  left_join(faixa_azul) |> 
  filter(!is.na(data_implementacao)) |> 
  distinct(logradouro_limpo) |> 
  pull(logradouro_limpo)

###### COMPATIBILIDADE ENTRE AS BASES DO INFOSIGA E DO IFOOD ----

compat <- df.trecho |> 
  distinct(id_osm) |> 
  mutate(osm = TRUE) |> 
  full_join(ifood |> 
              distinct(id_osm) |> 
              mutate(ifood = TRUE)) |> 
  mutate(across(c(osm, ifood), ~ replace_na(.x, FALSE)),
         flag = case_when(
           osm & ifood  ~ "both",
           osm & !ifood ~ "osm",
           !osm & ifood ~ "ifood"
         ) |> as_factor()) |> 
  select(id_osm, flag)
compat |> 
  write_csv(file = "Saida/output/compat.csv")


ifood.completa <- ifood |> 
  mutate(id_osm = id_osm |> as.character()) |>
  mutate(n_meses = 1,
         existe = TRUE) |> 
  complete(id_osm, 
           nesting(data),
           fill = list(rotas = 0, velocidade_media = NA, n_meses = NA, existe = FALSE)) |> 
  group_by(id_osm) |> 
  mutate(vel_mediana = median(velocidade_media, na.rm = TRUE),
         n_meses = sum(n_meses, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(velocidade_media = if_else(existe, velocidade_media, vel_mediana)) |> 
  select(-vel_mediana) %>% 
  left_join(trechos %>% select(id_osm, comprimento)) %>% 
  mutate(velocidade_ms = 3.6 * velocidade_media,
         prob_pin = comprimento / (velocidade_ms * 15),
         rotas_ajuste = ifelse(prob_pin < 1, rotas/(prob_pin), rotas),
         rotas_ajuste_km = rotas_ajuste * 1000 / comprimento) 

ifood.incompleta <- ifood.completa |> 
  filter(n_meses == 15)


df.ifood <- ifood.completa |> 
  left_join(faixa_azul) |> 
  inner_join(trechos) |> 
  left_join(trechos_complemento) |> 
  mutate(mes = data) |> 
  pivot_longer(c(mes, data_implementacao)) |> 
  mutate(value = value |> reorder(value) |> factor() |> as.numeric()) |> 
  pivot_wider(names_from = name, values_from = value) |> 
  mutate(data_implementacao = replace_na(data_implementacao, 0),
         amenidades_km = amenidades * 1000 / comprimento,
         intersec_km = intersec * 1000 / comprimento,
         id_osm = as.numeric(id_osm),
         rotas_km = rotas * 1000 / comprimento)

#1000m/3600s = 1km/h
#1m/s = (3600/1000) * km/h

# df.ifood <- df.ifood %>% 
#   mutate(velocidade_ms = 3.6 * velocidade_media,
#          prob_pin = comprimento / (velocidade_ms * 15),
#          rotas_ajuste = ifelse(prob_pin < 1, rotas/(prob_pin), rotas)) 

df.ifood %>% 
  ggplot() +
  geom_histogram(aes(x = prob_pin)) +
  scale_x_continuous("Probabilidade estimada", limits = c(0, 1), labels = scales::percent) +
  labs(y = "Frequência", title = "Viés da coluna rotas", 
       subtitle = "Estimando a probabilidade de contabilizar a rota, dado que ela foi feita") +
  theme_minimal()

ggsave("Saida/03-12-25/frequencia_vies.pdf", width = 6, height = 5)



# Analise numero de rotas e velocidade media ----
df.aux <- df.ifood |> 
  filter(data_implementacao != 0) |> 
  mutate(logradouro = as_factor(logradouro) |> fct_reorder(data_implementacao)) |> 
  group_by(logradouro, data) |> 
  summarise(velocidade_media = weighted.mean(velocidade_media, comprimento),
            rotas = sum(rotas),
            rotas_ajuste = sum(rotas_ajuste),
            comprimento = sum(comprimento)) |> 
  group_by(logradouro) |> 
  summarise(velocidade_media = mean(velocidade_media),
            rotas = mean(rotas),
            rotas_ajuste = mean(rotas_ajuste),
            comprimento = mean(comprimento))
df.aux |> 
  ggplot() +
  geom_col(aes(rotas, logradouro), width = 0.5, fill = "#333F48FF") +
  labs(x = "Média de rotas", y = "",
       title = "Média de rotas por via com Faixa Azul") +
  theme_minimal()
ggsave("Saida/output/media_rotas_por_via.pdf", width = 10, height = 6)
df.aux |> 
  ggplot() +
  geom_col(aes(velocidade_media, logradouro), width = 0.5, fill = "#333F48FF") +
  labs(x = "Velocidade média", y = "",
       title = "Velocidade média por via com Faixa Azul") +
  scale_x_continuous(labels = label_number(scale_cut = cut_si("km/h"))) +
  theme_minimal()
ggsave("Saida/output/velocidade_media_por_via.pdf", width = 10, height = 6)

df.aux %>% 
  pivot_longer(rotas:rotas_ajuste) %>% 
  ggplot() +
  geom_col(aes(x= value, y= logradouro, fill = name), position = "dodge") +
  labs(x = "Média de rotas", y = "",
       title = "Média de rotas por via com Faixa Azul") +
  theme_minimal()
ggsave("Saida/03-12-25/ajuste_rotas.pdf", width = 10, height = 6)


df.aux %>% 
  pivot_longer(rotas:rotas_ajuste) %>% 
  ggplot() +
  geom_col(aes(x= value/comprimento, y= logradouro, fill = name), position = "dodge") +
  labs(x = "Média de rotas", y = "",
       title = "Média de rotas por metro de via com Faixa Azul") +
  theme_minimal()
ggsave("Saida/03-12-25/ajuste_rotas_metro.pdf", width = 10, height = 6)




f_vmedia_24 <- df.ifood |> 
  group_by(logradouro_limpo, data) |> 
  summarise(velocidade_media = weighted.mean(velocidade_media, comprimento),
            rotas = sum(rotas)) |> 
  group_by(logradouro_limpo) |> 
  summarise(velocidade_media = mean(velocidade_media),
            rotas = mean(rotas)) |> 
  filter(velocidade_media >= 24, !is.na(logradouro_limpo)) |> 
  pull(logradouro_limpo)


df.aux <- df.ifood |>
  filter(logradouro_limpo %in% f_vmedia_24) |> 
  mutate(tratado = if_else(data_implementacao != 0, "Sim", "Não") |> fct_rev()) |> 
  group_by(data, tratado) |> 
  summarise(velocidade_media = weighted.mean(velocidade_media, comprimento),
            rotas = mean(rotas))
df.aux |> 
  ggplot() +
  geom_line(aes(data, rotas, linetype = tratado)) +
  labs(x = "Mês", y = "Média de rotas", linetype = "Adotou Faixa Azul",
       title = "Média de rotas do trecho por mês") +
  theme_minimal()
ggsave("Saida/output/media_rotas_trecho_periodo.pdf", width = 10, height = 6)
df.aux |> 
  ggplot() +
  geom_line(aes(data, velocidade_media, linetype = tratado)) +
  labs(x = "Mês", y = "Velocidade média", linetype = "Adotou Faixa Azul",
       title = "Velocidade média do trecho por mês") +
  theme_minimal()
ggsave("Saida/output/velocidade_media_trecho_periodo.pdf", width = 10, height = 6)



# Analise preenchimento da base ----

df.ifood |> 
  group_by(data) |> 
  summarise(existe = mean(existe)) |> 
  ggplot() +
  geom_col(aes(data, existe), fill = "#333F48FF") +
  scale_y_continuous(labels = label_percent()) +
  labs(x = "Mês", y = "Razão",
       title = "Base do iFood - completude por mês (%)") +
  theme_minimal()
ggsave("Saida/output/preenchimento_mensal.pdf", width = 10, height = 6)


df.aux <- trechos |> 
  left_join(faixa_azul) |> 
  left_join(ifood.completa |> 
              distinct(id_osm) |> 
              mutate(existe = TRUE)) |> 
  mutate(existe = replace_na(existe, FALSE)) 

df.aux |>
  # filter(!is.na(data_implementacao)) |>
  filter(logradouro %in% tratados) |>
  mutate(logradouro = as_factor(logradouro) |> fct_rev()) |>
  group_by(logradouro) |> 
  summarize(existe = mean(existe)) |> 
  ggplot() +
  geom_col(aes(x = 1, y = logradouro), width = 0.4, alpha = 0.3) +
  geom_col(aes(x = existe, y = logradouro), width = 0.5, fill = "#333F48FF") +
  scale_x_continuous(labels = label_percent()) +
  labs(x = "Razão", y = "",
       title = "Trechos presentes na base do iFood - vias tratadas") +
  theme_minimal()
# ggsave("Saida/output/preenchimento_trechos_tratados.pdf", width = 10, height = 6)
ggsave("Saida/output/preenchimento_logradouros_tratados.pdf", width = 10, height = 6)

f_preenchimento_50 <- df.aux |>
  group_by(logradouro_limpo) |> 
  summarize(existe = mean(existe)) |> 
  filter(existe >= 0.5) |> 
  pull(logradouro_limpo)


# Join das bases ----
df.trecho.inner <- sinistros |> 
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
  inner_join(ifood.completa) |> 
  mutate(mes = data) |> 
  pivot_longer(c(mes, data_implementacao)) |> 
  mutate(value = value |> reorder(value) |> factor() |> as.numeric()) |> 
  pivot_wider(names_from = name, values_from = value) |> 
  mutate(data_implementacao = replace_na(data_implementacao, 0),
         id_osm = as.numeric(id_osm)) |> 
  mutate(amenidades_km = amenidades * 1000 / comprimento,
         intersec_km = intersec * 1000 / comprimento) |> 
  mutate(sinistros_km             = sinistros * 1000 / comprimento,
         sinistros_moto_km        = sinistros_moto * 1000 / comprimento,
         sinistros_moto_golden_km = sinistros_moto_golden * 1000 / comprimento) |> 
  mutate(sinistros_diff           = sinistros_moto - sinistros,
         sinistros_diff_km        = sinistros_diff * 1000 / comprimento,
         sinistros_diff_golden    = sinistros_moto_golden - sinistros,
         sinistros_diff_golden_km = sinistros_diff_golden * 1000 / comprimento) |> 
  filter(!flag_sem10 | data_implementacao != 10,
         !flag_preenchimento | logradouro_limpo %in% f_preenchimento_50,
         !flag_vmedia | logradouro_limpo %in% f_vmedia_24,
         !flag_comprimento | comprimento >= value_comprimento,
         !flag_apenas_tratados | logradouro_limpo %in% tratados_limpo)

df.ifood <- ifood.completa |> 
  left_join(faixa_azul) |> 
  inner_join(trechos) |> 
  left_join(trechos_complemento) |> 
  mutate(mes = data) |> 
  pivot_longer(c(mes, data_implementacao)) |> 
  mutate(value = value |> reorder(value) |> factor() |> as.numeric()) |> 
  pivot_wider(names_from = name, values_from = value) |> 
  mutate(data_implementacao = replace_na(data_implementacao, 0),
         amenidades_km = amenidades * 1000 / comprimento,
         intersec_km = intersec * 1000 / comprimento,
         id_osm = as.numeric(id_osm),
         rotas_km = rotas * 1000 / comprimento) |> 
  filter(!flag_sem10 | data_implementacao != 10,
         !flag_preenchimento | logradouro_limpo %in% f_preenchimento_50,
         !flag_vmedia | logradouro_limpo %in% f_vmedia_24,
         !flag_comprimento | comprimento >= value_comprimento,
         !flag_apenas_tratados | logradouro_limpo %in% tratados_limpo)


df.trecho.inner |>
  mutate(tratado = data_implementacao != 0 & data >= as_date("2023-06-01") + months(data_implementacao-1)) |> 
  group_by(data) |> 
  summarise(tratados = sum(tratado)) |>
  ggplot() +
  geom_col(aes(data, tratados), fill = "#333F48FF") +
  labs(x = "Mês", y = "Número de trechos tratados",
       title = "Trechos tratados por período") +
       # title = "Trechos (mais que 200m) tratados por período") +
       # title = "Trechos (mais que 300m) tratados por período") +
  theme_minimal()
ggsave("Saida/output/trechos_tratados_por_periodo.pdf", width = 10, height = 6)
# ggsave("Saida/output/trechos200_tratados_por_periodo.pdf", width = 10, height = 6)
# ggsave("Saida/output/trechos300_tratados_por_periodo.pdf", width = 10, height = 6)


###### ANALISE CAUSAL ----

fit.did <- function(df, y = "sinistros", clustervars = c("id_osm", "logradouro_limpo"), 
                    formula = ~ tipo_via + faixas + limite_velocidade, idname = "id_osm"){
  att_gt(yname = y,
         tname = "mes",
         idname = idname,
         gname = "data_implementacao",
         clustervars = clustervars,
         data = df,
         xformla = formula) 
}

preparar.grafico <- function(fit){
  fit |>
    aggte(type = "dynamic", na.rm = T) |> 
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


#### SINISTROS, com controles iFood ----
## Completa ----
df.reg <- df.trecho.inner


y <- "sinistros_km"
fit    <- df.reg |> fit.did(y = y, formula = ~ 1)
fit.c1 <- df.reg |> fit.did(y = y, formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km)
fit.c2 <- df.reg |> fit.did(y = y, formula = ~ rotas + velocidade_media)
fit.c3 <- df.reg |> fit.did(y = y, formula = ~ tipo_via + faixas + limite_velocidade + rotas + velocidade_media + radar_proximo + amenidades_km + intersec_km)

bind_rows(
   fit |> 
    preparar.grafico() |> 
    mutate(controle = "Não"),
  fit.c1 |> 
    preparar.grafico() |> 
    mutate(controle = "OSM"),
  fit.c2 |> 
    preparar.grafico() |> 
    mutate(controle = "iFood"),
  fit.c3 |> 
    preparar.grafico() |> 
    mutate(controle = "OSM + iFood")) |> 
  mutate(controle = as_factor(controle)) |>
  plotar.grafico(titulo = "Todos os sinistros - por km (amostra iFood, completa)")
ggsave("Saida/output/did/sinistros-completa-todos-km.pdf", width = 10, height = 7.5)


y <- "sinistros_moto_golden_km"
fit    <- df.reg |> fit.did(y = y, formula = ~ 1)
fit.c1 <- df.reg |> fit.did(y = y, formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km)
fit.c2 <- df.reg |> fit.did(y = y, formula = ~ rotas + velocidade_media)
fit.c3 <- df.reg |> fit.did(y = y, formula = ~ tipo_via + faixas + limite_velocidade + rotas + velocidade_media + radar_proximo + amenidades_km + intersec_km)

bind_rows(
  fit |> 
    preparar.grafico() |> 
    mutate(controle = "Não"),
  fit.c1 |> 
    preparar.grafico() |> 
    mutate(controle = "OSM"),
  fit.c2 |> 
    preparar.grafico() |> 
    mutate(controle = "iFood"),
  fit.c3 |> 
    preparar.grafico() |> 
    mutate(controle = "OSM + iFood")) |> 
  mutate(controle = as_factor(controle)) |>
  plotar.grafico(titulo = "Sinistros golden - por km (amostra iFood, completa)")
ggsave("Saida/output/did/sinistros-completa-golden-km.pdf", width = 10, height = 7.5)


## Incompleta ----
df.reg <- df.trecho.inner |> 
  semi_join(ifood.incompleta |> 
              mutate(id_osm = as.numeric(id_osm)))


y <- "sinistros_km"
fit    <- df.reg |> fit.did(y = y, formula = ~ 1)
fit.c1 <- df.reg |> fit.did(y = y, formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km)
fit.c2 <- df.reg |> fit.did(y = y, formula = ~ rotas + velocidade_media)
fit.c3 <- df.reg |> fit.did(y = y, formula = ~ tipo_via + faixas + limite_velocidade + rotas + velocidade_media + radar_proximo + amenidades_km + intersec_km)

bind_rows(
  fit |> 
    preparar.grafico() |> 
    mutate(controle = "Não"),
  fit.c1 |> 
    preparar.grafico() |> 
    mutate(controle = "OSM"),
  fit.c2 |> 
    preparar.grafico() |> 
    mutate(controle = "iFood"),
  fit.c3 |> 
    preparar.grafico() |> 
    mutate(controle = "OSM + iFood")) |> 
  mutate(controle = as_factor(controle)) |>
  plotar.grafico(titulo = "Todos os sinistros - por km (amostra iFood, incompleta)")
ggsave("Saida/output/did/sinistros-incompleta-todos-km.pdf", width = 10, height = 7.5)


y <- "sinistros_moto_golden_km"
fit    <- df.reg |> fit.did(y = y, formula = ~ 1)
fit.c1 <- df.reg |> fit.did(y = y, formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km)
fit.c2 <- df.reg |> fit.did(y = y, formula = ~ rotas + velocidade_media)
fit.c3 <- df.reg |> fit.did(y = y, formula = ~ tipo_via + faixas + limite_velocidade + rotas + velocidade_media + radar_proximo + amenidades_km + intersec_km)

bind_rows(
  fit |> 
    preparar.grafico() |> 
    mutate(controle = "Não"),
  fit.c1 |> 
    preparar.grafico() |> 
    mutate(controle = "OSM"),
  fit.c2 |> 
    preparar.grafico() |> 
    mutate(controle = "iFood"),
  fit.c3 |> 
    preparar.grafico() |> 
    mutate(controle = "OSM + iFood")) |> 
  mutate(controle = as_factor(controle)) |>
  plotar.grafico(titulo = "Sinistros golden - por km (amostra iFood, incompleta)")
ggsave("Saida/output/did/sinistros-incompleta-golden-km.pdf", width = 10, height = 7.5)


#### VARIAVEIS iFOOD ----
## Completa ----
df.reg <- df.ifood


y <- "rotas"
fit   <- df.reg |> fit.did(y = y, formula = ~ 1)
fit.c <- df.reg |> fit.did(y = y, formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km)

bind_rows(
  fit |> 
    preparar.grafico() |> 
    mutate(controle = "Não"),
  fit.c |> 
    preparar.grafico() |> 
    mutate(controle = "OSM")) |> 
  mutate(controle = as_factor(controle)) |>
  plotar.grafico(titulo = "Número de rotas - total (amostra iFood, completa)")
ggsave("Saida/output/did/rotas-total-completa.pdf", width = 10, height = 7.5)

y <- "rotas_ajuste"
fit   <- df.reg |> fit.did(y = y, formula = ~ 1)
fit.c <- df.reg |> fit.did(y = y, formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km)

bind_rows(
  fit |> 
    preparar.grafico() |> 
    mutate(controle = "Não"),
  fit.c |> 
    preparar.grafico() |> 
    mutate(controle = "OSM")) |> 
  plotar.grafico(titulo = "Número de rotas ajustado - total (amostra iFood, completa)")
ggsave("Saida/03-12-25/rotas-ajustado-completa.pdf", width = 10, height = 7.5)

y <- "rotas_km"
fit   <- df.reg |> fit.did(y = y, formula = ~ 1)
fit.c <- df.reg |> fit.did(y = y, formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km)

bind_rows(
  fit |> 
    preparar.grafico() |> 
    mutate(controle = "Não"),
  fit.c |> 
    preparar.grafico() |> 
    mutate(controle = "OSM")) |> 
  plotar.grafico(titulo = "Número de rotas - por km (amostra iFood, completa)")
ggsave("Saida/output/did/rotas-km-completa.pdf", width = 10, height = 7.5)



y <- "rotas_ajuste_km"
fit   <- df.reg |> fit.did(y = y, formula = ~ 1)
fit.c <- df.reg |> fit.did(y = y, formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km)

bind_rows(
  fit |> 
    preparar.grafico() |> 
    mutate(controle = "Não"),
  fit.c |> 
    preparar.grafico() |> 
    mutate(controle = "OSM")) |> 
  plotar.grafico(titulo = "Número de rotas ajustado - por km (amostra iFood, completa)")
ggsave("Saida/03-12-25/rotas-ajustado-km-completa.pdf", width = 10, height = 7.5)


y <- "velocidade_media"
fit   <- df.reg |> fit.did(y = y, formula = ~ 1)
fit.c <- df.reg |> fit.did(y = y, formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km)

bind_rows(
  fit |> 
    preparar.grafico() |> 
    mutate(controle = "Não"),
  fit.c |> 
    preparar.grafico() |> 
    mutate(controle = "OSM")) |> 
  mutate(controle = as_factor(controle)) |>
  plotar.grafico(titulo = "Velocidade média (amostra iFood, completa)")
ggsave("Saida/output/did/velocidade-completa.pdf", width = 10, height = 7.5)


## Incompleta ----
df.reg <- df.ifood |> 
  semi_join(ifood.incompleta |> 
              mutate(id_osm = as.numeric(id_osm)))


y <- "rotas"
fit   <- df.reg |> fit.did(y = y, formula = ~ 1)
fit.c <- df.reg |> fit.did(y = y, formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km)

bind_rows(
  fit |> 
    preparar.grafico() |> 
    mutate(controle = "Não"),
  fit.c |> 
    preparar.grafico() |> 
    mutate(controle = "OSM")) |> 
  mutate(controle = as_factor(controle)) |>
  plotar.grafico(titulo = "Número de rotas - total (amostra iFood, incompleta)")
ggsave("Saida/output/did/rotas-total-incompleta.pdf", width = 10, height = 7.5)

y <- "rotas_ajuste"
fit   <- df.reg |> fit.did(y = y, formula = ~ 1)
fit.c <- df.reg |> fit.did(y = y, formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km)

bind_rows(
  fit |> 
    preparar.grafico() |> 
    mutate(controle = "Não"),
  fit.c |> 
    preparar.grafico() |> 
    mutate(controle = "OSM")) |> 
  mutate(controle = as_factor(controle)) |>
  plotar.grafico(titulo = "Número de rotas ajustado - total (amostra iFood, incompleta)")
ggsave("Saida/03-12-25/rotas-ajuste-incompleta.pdf", width = 10, height = 7.5)


y <- "rotas_km"
fit   <- df.reg |> fit.did(y = y, formula = ~ 1)
fit.c <- df.reg |> fit.did(y = y, formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km)

bind_rows(
  fit |> 
    preparar.grafico() |> 
    mutate(controle = "Não"),
  fit.c |> 
    preparar.grafico() |> 
    mutate(controle = "OSM")) |> 
  mutate(controle = as_factor(controle)) |>
  plotar.grafico(titulo = "Número de rotas - por km (amostra iFood, incompleta)")
ggsave("Saida/output/did/rotas-km-incompleta.pdf", width = 10, height = 7.5)

y <- "rotas_ajuste_km"
fit   <- df.reg |> fit.did(y = y, formula = ~ 1)
fit.c <- df.reg |> fit.did(y = y, formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km)

bind_rows(
  fit |> 
    preparar.grafico() |> 
    mutate(controle = "Não"),
  fit.c |> 
    preparar.grafico() |> 
    mutate(controle = "OSM")) |> 
  mutate(controle = as_factor(controle)) |>
  plotar.grafico(titulo = "Número de rotas ajustado - por km (amostra iFood, incompleta)")
ggsave("Saida/03-12-25/rotas-ajuste_km-incompleta.pdf", width = 10, height = 7.5)


y <- "velocidade_media"
fit   <- df.reg |> fit.did(y = y, formula = ~ 1)
fit.c <- df.reg |> fit.did(y = y, formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km)

bind_rows(
  fit |> 
    preparar.grafico() |> 
    mutate(controle = "Não"),
  fit.c |> 
    preparar.grafico() |> 
    mutate(controle = "OSM")) |> 
  mutate(controle = as_factor(controle)) |>
  plotar.grafico(titulo = "Velocidade média (amostra iFood, incompleta)")
ggsave("Saida/output/did/velocidade-incompleta.pdf", width = 10, height = 7.5)


#### VARIAVEIS iFOOD - Trechos com Sinistro ----
## Completa ----
df.reg <- df.trecho.inner |> 
  mutate(rotas_km = rotas * 1000 / comprimento)


y <- "rotas"
fit   <- df.reg |> fit.did(y = y, formula = ~ 1)
fit.c <- df.reg |> fit.did(y = y, formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km)

bind_rows(
  fit |> 
    preparar.grafico() |> 
    mutate(controle = "Não"),
  fit.c |> 
    preparar.grafico() |> 
    mutate(controle = "OSM")) |> 
  mutate(controle = as_factor(controle)) |>
  plotar.grafico(titulo = "Número de rotas - total (amostra iFood, completa)")
ggsave("Saida/output/did/Filtro sinistros/rotas-total-completa.pdf", width = 10, height = 7.5)


y <- "rotas_km"
fit   <- df.reg |> fit.did(y = y, formula = ~ 1)
fit.c <- df.reg |> fit.did(y = y, formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km)

bind_rows(
  fit |> 
    preparar.grafico() |> 
    mutate(controle = "Não"),
  fit.c |> 
    preparar.grafico() |> 
    mutate(controle = "OSM")) |> 
  mutate(controle = as_factor(controle)) |>
  plotar.grafico(titulo = "Número de rotas - por km (amostra iFood, completa)")
ggsave("Saida/output/did/Filtro sinistros/rotas-km-completa.pdf", width = 10, height = 7.5)


y <- "velocidade_media"
fit   <- df.reg |> fit.did(y = y, formula = ~ 1)
fit.c <- df.reg |> fit.did(y = y, formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km)

bind_rows(
  fit |> 
    preparar.grafico() |> 
    mutate(controle = "Não"),
  fit.c |> 
    preparar.grafico() |> 
    mutate(controle = "OSM")) |> 
  mutate(controle = as_factor(controle)) |>
  plotar.grafico(titulo = "Velocidade média (amostra iFood, completa)")
ggsave("Saida/output/did/Filtro sinistros/velocidade-completa.pdf", width = 10, height = 7.5)


## Incompleta ----
df.reg <- df.trecho.inner |> 
  mutate(rotas_km = rotas * 1000 / comprimento) |> 
  semi_join(ifood.incompleta |> 
              mutate(id_osm = as.numeric(id_osm)))


y <- "rotas"
fit   <- df.reg |> fit.did(y = y, formula = ~ 1)
fit.c <- df.reg |> fit.did(y = y, formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km)

bind_rows(
  fit |> 
    preparar.grafico() |> 
    mutate(controle = "Não"),
  fit.c |> 
    preparar.grafico() |> 
    mutate(controle = "OSM")) |> 
  mutate(controle = as_factor(controle)) |>
  plotar.grafico(titulo = "Número de rotas - total (amostra iFood, incompleta)")
ggsave("Saida/output/did/Filtro sinistros/rotas-total-incompleta.pdf", width = 10, height = 7.5)


y <- "rotas_km"
fit   <- df.reg |> fit.did(y = y, formula = ~ 1)
fit.c <- df.reg |> fit.did(y = y, formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km)

bind_rows(
  fit |> 
    preparar.grafico() |> 
    mutate(controle = "Não"),
  fit.c |> 
    preparar.grafico() |> 
    mutate(controle = "OSM")) |> 
  mutate(controle = as_factor(controle)) |>
  plotar.grafico(titulo = "Número de rotas - por km (amostra iFood, incompleta)")
ggsave("Saida/output/did/Filtro sinistros/rotas-km-incompleta.pdf", width = 10, height = 7.5)


y <- "velocidade_media"
fit   <- df.reg |> fit.did(y = y, formula = ~ 1)
fit.c <- df.reg |> fit.did(y = y, formula = ~ tipo_via + faixas + limite_velocidade + radar_proximo + amenidades_km + intersec_km)

bind_rows(
  fit |> 
    preparar.grafico() |> 
    mutate(controle = "Não"),
  fit.c |> 
    preparar.grafico() |> 
    mutate(controle = "OSM")) |> 
  mutate(controle = as_factor(controle)) |>
  plotar.grafico(titulo = "Velocidade média (amostra iFood, incompleta)")
ggsave("Saida/output/did/Filtro sinistros/velocidade-incompleta.pdf", width = 10, height = 7.5)


###### LOGRADOURO ----

logradouros <- trechos |>
  left_join(faixa_azul) |> 
  left_join(trechos_complemento) |> 
  filter(!is.na(logradouro_limpo)) |> 
  group_by(logradouro_limpo, data_implementacao) |>
  summarize(
    across(
      c(faixas, limite_velocidade),
      ~ .x |> as.numeric() |> mean(na.rm = TRUE)),
    across(
      c(mao_unica, superficie, tipo_via),
      ~ fct_infreq(.x) |> levels() |> first()),
    comprimento = sum(comprimento) |> as.numeric(),
    trechos = n(),
    amenidades = sum(amenidades),
    intersec = sum(intersec),
    radar_proximo = any(radar_proximo)) |>
  group_by(logradouro_limpo) |>
  mutate(logradouro = case_when(n() > 1 ~ str_c(logradouro_limpo, " (", row_number(), ")"),
                                TRUE ~ logradouro_limpo),
         amenidades_km = amenidades * 1000 / comprimento,
         intersec_km = intersec * 1000 / comprimento)

df.ifood.logradouro <- ifood.completa |> 
  left_join(faixa_azul) |> 
  inner_join(trechos) |> 
  filter(!is.na(logradouro_limpo)) |> 
  group_by(data, logradouro_limpo) |>
  summarize(rotas = sum(rotas),
            velocidade_media = weighted.mean(velocidade_media, comprimento),
            .groups = "drop") |>
  left_join(logradouros) |> 

  #trasformacao da data em valor numerico (na ordem)
  mutate(mes = data) |>
  pivot_longer(c(mes, data_implementacao)) |>
  mutate(value = value |> reorder(value) |> factor() |> as.numeric()) |>
  pivot_wider(names_from = name, values_from = value) |>
  mutate(data_implementacao = replace_na(data_implementacao, 0),
         logradouro = logradouro |> as.factor() |> as.numeric()) |>
  mutate(rotas_km = rotas * 1000 / comprimento) |> 
  filter(!flag_comprimento | comprimento >= value_comprimento)

## Completa ----
df.reg <- df.ifood.logradouro


y <- "rotas"
fit   <- df.reg |> fit.did(y = y, formula = ~ 1,
                           idname = "logradouro", clustervars = "logradouro")
fit.c <- df.reg |> fit.did(y = y, formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km,
                           idname = "logradouro", clustervars = "logradouro")

bind_rows(
  fit |> 
    preparar.grafico() |> 
    mutate(controle = "Não"),
  fit.c |> 
    preparar.grafico() |> 
    mutate(controle = "OSM")) |> 
  mutate(controle = as_factor(controle)) |>
  plotar.grafico(titulo = "Número de rotas - total (amostra iFood, completa, por logradouro)")
ggsave("Saida/output/did/Logradouro/rotas-total-completa.pdf", width = 10, height = 7.5)


y <- "rotas_km"
fit   <- df.reg |> fit.did(y = y, formula = ~ 1,
                           idname = "logradouro", clustervars = "logradouro")
fit.c <- df.reg |> fit.did(y = y, formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km,
                           idname = "logradouro", clustervars = "logradouro")

bind_rows(
  fit |> 
    preparar.grafico() |> 
    mutate(controle = "Não"),
  fit.c |> 
    preparar.grafico() |> 
    mutate(controle = "OSM")) |> 
  mutate(controle = as_factor(controle)) |>
  plotar.grafico(titulo = "Número de rotas - por km (amostra iFood, completa, por logradouro)")
ggsave("Saida/output/did/Logradouro/rotas-km-completa.pdf", width = 10, height = 7.5)


y <- "velocidade_media"
fit   <- df.reg |> fit.did(y = y, formula = ~ 1,
                           idname = "logradouro", clustervars = "logradouro")
fit.c <- df.reg |> fit.did(y = y, formula = ~ tipo_via + faixas + limite_velocidade + comprimento + radar_proximo + amenidades_km + intersec_km,
                           idname = "logradouro", clustervars = "logradouro")

bind_rows(
  fit |> 
    preparar.grafico() |> 
    mutate(controle = "Não"),
  fit.c |> 
    preparar.grafico() |> 
    mutate(controle = "OSM")) |> 
  mutate(controle = as_factor(controle)) |>
  plotar.grafico(titulo = "Velocidade média (amostra iFood, completa, por logradouro)")
ggsave("Saida/output/did/Logradouro/velocidade-completa.pdf", width = 10, height = 7.5)

