library(tidyverse)
library(sf)
library(units)
library(paletteer)


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
  select(id_sinistro, data, quantidade_envolvidos, motocicletas, tipo)

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


### Trechos/logradouros tratados por período
faixa_azul |> 
  group_by(data_implementacao) |> 
  summarize(n_trechos = n()) |> 
  left_join(logradouros |> 
              filter(!is.na(data_implementacao)) |> 
              group_by(data_implementacao) |> 
              summarize(n_logradouros = n())) |>
  # group_by(data_implementacao) |> 
  mutate(n_trechos = cumsum(n_trechos)) |> 
  ggplot() +
  geom_line(aes(data_implementacao, n_trechos), width = 0.5) +
  geom_text(aes(data_implementacao, n_trechos, label = n_trechos), vjust = -0.5, hjust = 1) +
  # ylim(0, 350) +
  labs(title = "Trechos implementados por período") +
  theme_bw()
ggsave("output/did/trechos_implementados.png", width = 10, height = 7.5)



# Óbitos totais vs Óbitos em avenidas com faixa-azul


sinistros <- read_csv("banco_dados/sinistros.csv") |> 
  filter(year(data) >= 2019, tipo != "NOTIFICACAO") |> # Antes de 2019 há apenas sinistros com óbito
  select(id_sinistro, data, quantidade_envolvidos, motocicletas, tipo)


df <- sinistros |> 
  left_join(match) |> 
  semi_join(trechos, by = join_by(id_osm)) |>
  select(id_sinistro, id_osm, data, quantidade_envolvidos, motocicletas, tipo, golden_match) |> 
  # mutate(data = make_date(year = year(data), month = month(data))) |> 
  left_join(trechos |> 
              left_join(faixa_azul |> distinct())) |> 
  mutate(ano = year(data) |> as_factor()) |> 
  # filter(ano == 2023 | ano == 2024) |> 
  select(id_sinistro, ano, quantidade_envolvidos, motocicletas, tipo, golden_match, data_implementacao)


sinistros |> 
  filter(motocicletas != 0) |> 
  mutate(ano = year(data) |> as_factor()) |> 
  group_by(ano) |> 
  summarise(total = sum(quantidade_envolvidos, na.rm = T)) |> 
  ggplot() +
  geom_col(aes(ano, total), width = 0.5) +
  geom_text(aes(ano, total, label = total), vjust = -0.5) +
  ylim(0, 650) +
  labs(title = "Óbitos em sinistros com moto") +
  theme_bw()
ggsave("output/comparacao_panfleto/obitos_moto.png", width = 10, height = 7.5)

df |> 
  filter(motocicletas != 0) |> 
  group_by(ano) |> 
  summarise(total = sum(quantidade_envolvidos, na.rm = T)) |> 
  ggplot() +
  geom_col(aes(ano, total), width = 0.5) +
  geom_text(aes(ano, total, label = total), vjust = -0.5) +
  ylim(0, 350) +
  labs(title = "Óbitos em sinistros com moto - depois do match") +
  theme_bw()
ggsave("output/comparacao_panfleto/obitos_moto_match.png", width = 10, height = 7.5)

df |> 
  filter(tipo == "SINISTRO FATAL", motocicletas != 0, !is.na(data_implementacao)) |> 
  group_by(ano) |> 
  summarise(total = sum(quantidade_envolvidos)) |> 
  ggplot() +
  geom_col(aes(ano, total), width = 0.5) +
  geom_text(aes(ano, total, label = total), vjust = -0.5) +
  ylim(0, 25) +
  labs(title = "Óbitos em sinistros com moto em vias com Faixa Azul - depois do match") +
  theme_bw()
ggsave("output/comparacao_panfleto/obitos_moto_match_faixa-azul.png", width = 10, height = 7.5)




df.trecho |> 
  filter(motocicletas != 0) |> 
  mutate(ano = year(data)) |> 
  group_by(ano) |> 
  summarise(total = sum(quantidade_envolvidos, na.rm = T))



