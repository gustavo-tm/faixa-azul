library(tidyverse)
library(sf)
library(units)
library(mapview)
library(ggsankey)
library(paletteer)

trechos <- st_read("banco_dados/trechos.gpkg") |> 
  mutate(id_osm = as.numeric(id_osm))
sinistros <- read_csv("banco_dados/sinistros.csv")
faixa_azul <- read_csv("banco_dados/faixa_azul.csv")
match <- read_csv("banco_dados/match.csv")
osm.token <- read_csv("dados_tratados/osm-token.csv") |> 
  mutate(len = str_length(logradouro_limpo)) |> 
  group_by(id_osm) |> 
  arrange(-len) |> 
  filter(row_number() == 1) |> 
  ungroup() |> 
  arrange(id_osm)

# OBITOS E FAIXA AZUL ----

osm.token |> 
  right_join(osm.token |> 
              left_join(faixa_azul) |> 
              filter(!data_implementacao |> is.na()) |> 
              distinct(logradouro, data_implementacao)) |> 
  select(id_osm, logradouro_completo = logradouro, data_implementacao) |> 
  left_join(match) |> 
  filter(similaridade > .75 | (match_tipo == TRUE & match_titulo == TRUE)) |> 
  left_join(sinistros, by = join_by(id_sinistro)) |> 
  filter(tipo == "SINISTRO FATAL") |> 
  mutate(motocicleta = motocicletas > 0,
         logradouro_completo = factor(logradouro_completo) |> fct_rev()) |>
  (\(df) ggplot() +
     geom_rect(aes(xmin = as.Date(-Inf), 
                   xmax = as.Date(data_implementacao), 
                   ymin = as.numeric(logradouro_completo) - .15, 
                   ymax = as.numeric(logradouro_completo) + .15),
               data = df |> distinct(logradouro_completo, data_implementacao),
               fill = "grey85") +
     geom_rect(aes(xmin = as.Date(data_implementacao), 
                   xmax = as.Date(Inf), 
                   ymin = as.numeric(logradouro_completo) - .27, 
                   ymax = as.numeric(logradouro_completo) + .27),
               data = df |> distinct(logradouro_completo, data_implementacao),
               fill = "#333F48FF", alpha = .9) +
     geom_point(aes(x = as.Date(data), y = logradouro_completo, fill = motocicleta, shape = motocicleta), 
                alpha = .9, stroke = .15, colour = "white", size = 3,
                data = df) +
     scale_fill_manual("Veículo da vítima", values = c("TRUE" = "#BA0C2FFF", "FALSE" = "#C6AA76FF"), labels = c("TRUE" = "Motocicleta", "FALSE" = "Outros")) +
     scale_shape_manual("Veículo da vítima", values = c("TRUE" = 21, "FALSE" = 22), labels = c("TRUE" = "Motocicleta", "FALSE" = "Outros")) +
     scale_x_date(limits = c(make_date(year = 2021, month = 1), max(df$data))) +
     labs(y = NULL, x = NULL) +
     theme_minimal())(df = _)
ggsave("output/obitos.pdf", width = 11, height = 7)
ggsave("output/obitos.png", width = 11, height = 7, dpi = 600)


osm.token |> 
  right_join(osm.token |> 
               left_join(faixa_azul) |> 
               filter(!data_implementacao |> is.na()) |> 
               distinct(logradouro, data_implementacao)) |> 
  select(id_osm, logradouro_completo = logradouro, data_implementacao) |> 
  left_join(match) |> 
  filter(similaridade > .75 | (match_tipo == TRUE & match_titulo == TRUE)) |> 
  left_join(sinistros, by = join_by(id_sinistro)) |> 
  filter(tipo == "SINISTRO FATAL") |> 
  mutate(motocicleta = motocicletas > 0,
         logradouro_completo = factor(logradouro_completo) |> fct_reorder(desc(data_implementacao))) |> 
  (\(df) ggplot() +
     geom_rect(aes(xmin = as.Date(-Inf), 
                   xmax = as.Date(data_implementacao), 
                   ymin = as.numeric(logradouro_completo) - .15, 
                   ymax = as.numeric(logradouro_completo) + .15),
               data = df |> distinct(logradouro_completo, data_implementacao),
               fill = "grey85") +
     geom_rect(aes(xmin = as.Date(data_implementacao), 
                   xmax = as.Date(Inf), 
                   ymin = as.numeric(logradouro_completo) - .27, 
                   ymax = as.numeric(logradouro_completo) + .27),
               data = df |> distinct(logradouro_completo, data_implementacao),
               fill = "#333F48FF", alpha = .9) +
     geom_point(aes(x = as.Date(data), y = logradouro_completo, fill = motocicleta, shape = motocicleta), 
                alpha = .9, stroke = .15, colour = "white", size = 3,
                data = df) +
     scale_fill_manual("Veículo da vítima", values = c("TRUE" = "#BA0C2FFF", "FALSE" = "#C6AA76FF"), labels = c("TRUE" = "Motocicleta", "FALSE" = "Outros")) +
     scale_shape_manual("Veículo da vítima", values = c("TRUE" = 21, "FALSE" = 22), labels = c("TRUE" = "Motocicleta", "FALSE" = "Outros")) +
     scale_x_date(limits = c(make_date(year = 2021, month = 1), max(df$data))) +
     labs(y = NULL, x = NULL) +
     theme_minimal())(df = _)

ggsave("output/obitos_ordenado.pdf", width = 11, height = 7)
ggsave("output/obitos_ordenado.png", width = 11, height = 6, dpi = 400)

# PANFLETO CET ----

sinistros |> 
  filter(tipo == "SINISTRO FATAL", year(data) > 2015) |> 
  left_join(match, by = join_by(id_sinistro)) |> 
  left_join(faixa_azul |> distinct()) |> 
  mutate(match_sucesso = is.na(id_osm) == FALSE,
         faixa_azul = is.na(data_implementacao) == FALSE) |> 
  group_by(ano = year(data), match_sucesso, faixa_azul) |> 
  summarize(mortes = sum(quantidade_envolvidos)) |> 
  mutate(categoria = case_when(faixa_azul == TRUE ~ "Faixa Azul",
                               match_sucesso == TRUE ~ "Outras Vias",
                               match_sucesso == FALSE ~ "Via não encontrada",
                               .default = "ERRO") |> 
           factor(levels = c("Faixa Azul", "Outras Vias", "Via não encontrada") |> rev())) |> 
  group_by(ano) |> 
  mutate(total = ifelse(row_number() == 1, sum(mortes), NA)) |> 
  ggplot(aes(x = factor(ano))) +
  geom_col(aes(y = mortes, fill = categoria)) +
  geom_text(aes(y = total, label = total), vjust = -0.5) +
  scale_fill_manual(values = c(rgb(.2,.5,.8), rgb(.2,.5,.5), rgb(.05,.1,.3))) + 
  theme_minimal() +
  labs(x = NULL, fill = "Local do sinistro", y = "Óbitos",
       title = "Óbitos totais por tipo de via")
ggsave("output/comparacao_panfleto/obitos_tempo.pdf", width = 8, height = 5)


sinistros |> 
  filter(tipo == "SINISTRO FATAL", year(data) > 2015, motocicletas != 0) |> 
  left_join(match, by = join_by(id_sinistro)) |> 
  left_join(faixa_azul |> distinct()) |> 
  mutate(match_sucesso = is.na(id_osm) == FALSE,
         faixa_azul = is.na(data_implementacao) == FALSE) |> 
  group_by(ano = year(data), match_sucesso, faixa_azul) |> 
  summarize(mortes = sum(quantidade_envolvidos)) |> 
  mutate(categoria = case_when(faixa_azul == TRUE ~ "Faixa Azul",
                               match_sucesso == TRUE ~ "Outras Vias",
                               match_sucesso == FALSE ~ "Via não encontrada",
                               .default = "ERRO") |> 
           factor(levels = c("Faixa Azul", "Outras Vias", "Via não encontrada") |> rev())) |> 
  group_by(ano) |> 
  mutate(total = ifelse(row_number() == 1, sum(mortes), NA)) |> 
  ggplot(aes(x = factor(ano))) +
  geom_col(aes(y = mortes, fill = categoria)) +
  geom_text(aes(y = total, label = total), vjust = -0.5) +
  scale_fill_manual(values = c(rgb(.2,.5,.8), rgb(.2,.5,.5), rgb(.05,.1,.3))) + 
  theme_minimal() +
  labs(x = NULL, fill = "Local do sinistro", y = "Óbitos",
       title = "Óbitos em sinistros com moto por tipo de via")
ggsave("output/comparacao_panfleto/obitos_tempo_moto.pdf", width = 8, height = 5)


sinistros |> 
  filter(tipo == "SINISTRO FATAL", year(data) > 2015) |> 
  left_join(match, by = join_by(id_sinistro)) |> 
  group_by(ano = year(data)) |> 
  summarize(mortes = sum(quantidade_envolvidos)) |> 
  ggplot(aes(x = factor(ano))) +
  geom_col(aes(y = mortes), fill = rgb(.05,.1,.3)) +
  geom_text(aes(y = mortes, label = mortes), vjust = -0.5) +
  theme_minimal() +
  labs(x = NULL, y = "Óbitos",
       title = "Óbitos totais - todas as vias")
ggsave("output/comparacao_panfleto/obitos_tempo_trecho_todos.pdf", width = 8, height = 5)
ggsave("output/comparacao_panfleto/obitos_tempo_trecho_todos.png", width = 8, height = 7, dpi = 400)


sinistros |> 
  filter(tipo == "SINISTRO FATAL", year(data) > 2015, motocicletas != 0) |> 
  left_join(match, by = join_by(id_sinistro)) |> 
  group_by(ano = year(data)) |> 
  summarize(mortes = sum(quantidade_envolvidos)) |> 
  ggplot(aes(x = factor(ano))) +
  geom_col(aes(y = mortes), fill = rgb(.05,.1,.3)) +
  geom_text(aes(y = mortes, label = mortes), vjust = -0.5) +
  theme_minimal() +
  labs(x = NULL, y = "Óbitos",
       title = "Óbitos em sinistros com moto - todas as vias")
ggsave("output/comparacao_panfleto/obitos_tempo_trecho_todos_moto.pdf", width = 8, height = 5)
ggsave("output/comparacao_panfleto/obitos_tempo_trecho_todos_moto.png", width = 8, height = 7, dpi = 400)


sinistros |> 
  filter(tipo == "SINISTRO FATAL", year(data) > 2015) |> 
  left_join(match, by = join_by(id_sinistro)) |> 
  semi_join(faixa_azul |> select(id_osm)) |> 
  group_by(ano = year(data)) |> 
  summarize(mortes = sum(quantidade_envolvidos)) |> 
  ggplot(aes(x = factor(ano))) +
  geom_col(aes(y = mortes), fill = rgb(.05,.1,.3)) +
  geom_text(aes(y = mortes, label = mortes), vjust = -0.5) +
  theme_minimal() +
  labs(x = NULL, y = "Óbitos",
       title = "Óbitos totais - trechos com Faixa Azul")
ggsave("output/comparacao_panfleto/obitos_tempo_trecho_faixa_azul.pdf", width = 8, height = 5)
ggsave("output/comparacao_panfleto/obitos_tempo_trecho_faixa_azul.png", width = 8, height = 7, dpi = 400)


sinistros |> 
  filter(tipo == "SINISTRO FATAL", year(data) > 2015, motocicletas != 0) |> 
  left_join(match, by = join_by(id_sinistro)) |> 
  semi_join(faixa_azul |> select(id_osm)) |> 
  group_by(ano = year(data)) |> 
  summarize(mortes = sum(quantidade_envolvidos)) |> 
  ggplot(aes(x = factor(ano))) +
  geom_col(aes(y = mortes), fill = rgb(.05,.1,.3)) +
  geom_text(aes(y = mortes, label = mortes), vjust = -0.5) +
  theme_minimal() +
  labs(x = NULL, y = "Óbitos",
       title = "Óbitos em sinistros com moto - trechos que receberam Faixa Azul")
ggsave("output/comparacao_panfleto/obitos_tempo_trecho_faixa_azul_moto.pdf", width = 8, height = 5)
ggsave("output/comparacao_panfleto/obitos_tempo_trecho_faixa_azul_moto.png", width = 8, height = 7, dpi = 400)


sinistros |> 
  filter(tipo == "SINISTRO FATAL", year(data) > 2015) |> 
  left_join(match, by = join_by(id_sinistro)) |>
  left_join(faixa_azul |> distinct()) |> 
  group_by(logradouro.y) |>
  mutate(tratado = any(!is.na(data_implementacao))) |> 
  filter(tratado) |> 
  ungroup() |> 
  group_by(ano = year(data)) |> 
  summarize(obitos = sum(quantidade_envolvidos)) |> 
  ggplot(aes(x = factor(ano))) +
  geom_col(aes(y = obitos), fill = rgb(.05,.1,.3)) +
  geom_text(aes(y = obitos, label = obitos), vjust = -0.5) +
  labs(x = NULL, y = "Óbitos",
       title = "Óbitos totais - vias que receberam Faixa Azul") +
  ylim(c(0, 80)) +
  theme_minimal()
ggsave("output/comparacao_panfleto/obitos_tempo_via_faixa_azul.pdf", width = 8, height = 5)
ggsave("output/comparacao_panfleto/obitos_tempo_via_faixa_azul.png", width = 8, height = 7, dpi = 400)


sinistros |> 
  filter(tipo == "SINISTRO FATAL", year(data) > 2015, motocicletas != 0) |> 
  left_join(match, by = join_by(id_sinistro)) |>
  left_join(faixa_azul |> distinct()) |> 
  group_by(logradouro.y) |>
  mutate(tratado = any(!is.na(data_implementacao))) |> 
  filter(tratado) |> 
  ungroup() |> 
  group_by(ano = year(data)) |> 
  summarize(obitos = sum(quantidade_envolvidos)) |> 
  ggplot(aes(x = factor(ano))) +
  geom_col(aes(y = obitos), fill = rgb(.05,.1,.3)) +
  geom_text(aes(y = obitos, label = obitos), vjust = -0.5) +
  labs(x = NULL, y = "Óbitos",
       title = "Óbitos em sinistros com moto - vias que receberam Faixa Azul") +
  ylim(c(0, 50)) +
  theme_minimal()
ggsave("output/comparacao_panfleto/obitos_tempo_via_faixa_azul_moto.pdf", width = 8, height = 5)
ggsave("output/comparacao_panfleto/obitos_tempo_via_faixa_azul_moto.png", width = 8, height = 7, dpi = 400)


# TAMANHO DA FROTA ----

frota <- read_csv("dados_tratados/frota_sp.csv")

left_join(
  sinistros |> 
    filter(tipo == "SINISTRO FATAL", year(data) > 2015, motocicletas != 0) |> 
    group_by(ano = year(data)) |> 
    summarize(mortes = sum(quantidade_envolvidos)),
  frota |> 
    filter(mes == 12, tipo_veiculo %in% c("motocicleta", "motoneta")) |> 
    group_by(ano) |> 
    summarize(motocicletas = sum(quantidade))) |> 
  pivot_longer(mortes:motocicletas) |> 
  group_by(name) |> 
  mutate(num_indice = (value / first(value)) * 100) |> 
  ggplot(aes(x = factor(ano), y = num_indice)) +
  geom_line(aes(group = name, colour = name)) +
  geom_point(colour = "grey50") +
  theme_minimal() +
  # scale_y_continuous(labels = scales::percent) +
  scale_colour_manual(NULL, values = c(rgb(.3, .05, .1), rgb(.9, .8, .5)), 
                      labels = c("Óbitos em sinistros fatais que envolveram motocicleta", "Frota total de motocicletas e motonetas")) +
  theme(legend.position = "top",) +
  labs(y = "Variação (2016 = 100)", x = NULL) +
  guides(colour=guide_legend(nrow=2,byrow=TRUE))

ggsave("output/comparacao_panfleto/frota_vs_mortes.pdf", width = 6, height = 5)


# SINISTROS EM CADA HORA DO DIA ----

read_csv("banco_dados/sinistros.csv") |>
  filter(year(data) > 2018, year(data) <= 2023) |> 
  mutate(mes = fct_collapse(month(data) |> factor(),
                            "Jan-Mar" = 1:3,
                            "Abr-Jun" = 4:6,
                            "Jul-Set" = 7:9,
                            "Out-Dez" = 10:12,
                            other_level = "teste")) |> 
  group_by(hora = hour(data), dia = day(data), mes) |> 
  summarize(sinistros = n()) |> 
  ggplot(aes(x = dia, y = hora, fill = sinistros)) +
  geom_tile() +
  facet_grid(cols = vars(mes)) +
  theme_minimal() +
  scale_fill_viridis_c() +
  scale_y_continuous("Horário", breaks = 0:11*2) +
  scale_x_continuous("Dia do mês", breaks = NULL)

ggsave("output/horarios-sinistros.pdf", width = 10, height =4)

# TAMANHO VIAS ----

osm.token |> 
  right_join(osm.token |> 
               left_join(faixa_azul) |> 
               filter(!data_implementacao |> is.na()) |> 
               distinct(logradouro, data_implementacao)) |> 
  select(id_osm, logradouro_completo = logradouro, data_implementacao) |> 
  left_join(faixa_azul |> 
              mutate(faixa_azul = "faixa_azul") |> 
              select(id_osm, faixa_azul)) |> 
  left_join(trechos |> select(id_osm, geometry = geom)) |> 
  mutate(faixa_azul = replace_na(faixa_azul, "sem_faixa"),
         tamanho = st_length(geometry)) |> 
  st_drop_geometry() |> 
  group_by(logradouro_completo, faixa_azul) |> 
  summarize(tamanho = tamanho |> sum()) |> 
  group_by(logradouro_completo) |> 
  mutate(order = sum(tamanho), 
         faixa_azul = factor(faixa_azul, 
                             levels = c("sem_faixa", "faixa_azul"),
                             labels = c("Trecho comum", "Trecho de faixa azul"))) |> 
  ggplot(aes(x = tamanho, y = reorder(logradouro_completo, order), fill = faixa_azul)) +
  geom_col(colour = "black", lwd = .1) +
  scale_x_units("Tamanho da via", unit = "km") +
  labs(y = NULL) +
  scale_fill_manual("", values = c("Trecho comum" = "#A6A6A6", "Trecho de faixa azul" = "#4472C4")) +
  theme_minimal() +
  theme(legend.position = "inside", legend.position.inside = c(.7,.2))
ggsave("output/tamanho-trechos.pdf", width = 7, height = 6)
ggsave("output/tamanho-trechos.png", width = 9, height = 5, dpi = 400)


# RADIAL ANO MES ----

sinistros |> 
  filter(tipo != "NOTIFICACAO", year(data) > 2018) |> 
  group_by(ano = year(data), mes = month(data)) |> 
  summarize(n = n(), .groups = "drop") |>
  (\(.) bind_rows(., . |> filter(mes == 12) |> mutate(mes = 0, ano = ano + 1)))() |>
  ggplot(aes(x = mes, y = n, colour = factor(ano))) +
  geom_line(lwd = 1.5, alpha = .75) +
  coord_radial(expand = FALSE, inner.radius = 0.1, r_axis_inside = TRUE) +
  scale_y_continuous(limits = c(1000, 3000)) +
  scale_x_continuous(breaks = 1:12, labels = 1:12) +
  theme_minimal() 
  
ggsave("output/sinistros-meses.pdf", width = 8, height = 8)


# TRECHOS/LOGRADOUROS TRATADOS POR PERIODO ----

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
  mutate(id_osm = as.numeric(id_osm)) |> 
  select(id_osm, logradouro, logradouro_limpo, tipo_via, faixas, limite_velocidade, mao_unica, superficie, comprimento)

faixa_azul |> 
  distinct() |> 
  group_by(data_implementacao) |> 
  summarize(trechos = n()) |> 
  complete(data_implementacao = seq(min(data_implementacao), max(data_implementacao), by = "1 month"), fill = list(trechos = 0)) |> 
  mutate(trechos_total = cumsum(trechos),
         trechos_label = if_else(trechos == 0, "", as.character(trechos_total))) |> 
  ggplot() +
  geom_col(aes(data_implementacao, trechos_total), fill = rgb(.05,.1,.3)) +
  geom_text(aes(data_implementacao, trechos_total, label = trechos_label), vjust = -0.5, hjust = 0.8) +
  labs(x = NULL, y = "Número de trechos",
       title = "Trechos implementados por período") +
  theme_minimal()
ggsave("output/trechos_implementados.pdf", width = 10, height = 7.5)
ggsave("output/trechos_implementados.png", width = 7, height = 6, dpi = 400)

trechos |>
  left_join(faixa_azul) |> 
  group_by(logradouro_limpo, data_implementacao) |>
  summarize(trechos = n()) |> 
  filter(!is.na(data_implementacao)) |> 
  group_by(data_implementacao) |> 
  summarize(logradouros = n()) |> 
  complete(data_implementacao = seq(min(data_implementacao), max(data_implementacao), by = "1 month"), fill = list(logradouros = 0)) |> 
  mutate(logradouros_total = cumsum(logradouros),
         logradouros_label = if_else(logradouros == 0, "", as.character(logradouros_total))) |> 
  ggplot() +
  geom_col(aes(data_implementacao, logradouros_total), fill = rgb(.05,.1,.3)) +
  geom_text(aes(data_implementacao, logradouros_total, label = logradouros_label), vjust = -0.5, hjust = 0.8) +
  labs(x = NULL, y = "Número de vias",
       title = "Vias tratadas por período") +
  theme_minimal()
ggsave("output/logradouros_implementados.pdf", width = 10, height = 7.5)
ggsave("output/logradouros_implementados.png", width = 7, height = 6, dpi = 400)
  

# QUALIDADE DO MATCH ----

tabela <- sinistros |> 
  filter(tipo != "NOTIFICACAO", year(data) > 2018) |> 
  select(id_sinistro, data, logradouro, numero, latitude, longitude, tipo) |> 
  mutate(numero_zero = numero == 0,
         across(c(logradouro:longitude), ~ is.na(.x) | .x == "NAO DISPONIVEL"),
         apresenta_lognum = logradouro == FALSE & numero == FALSE,
         apresenta_latlong = latitude == FALSE & longitude == FALSE,
         completude = case_when(apresenta_latlong & apresenta_lognum & !numero_zero ~ "Completo",
                                apresenta_latlong & apresenta_lognum & numero_zero ~ "Completo, mas número zero",
                                apresenta_latlong & !apresenta_lognum ~ "Apenas latitude e longitude",
                                !apresenta_latlong & apresenta_lognum ~ "Apenas logradouro e número",
                                !apresenta_latlong & !apresenta_lognum & logradouro == FALSE ~ "Apenas logradouro",
                                !apresenta_latlong & !apresenta_lognum & numero == FALSE & numero_zero ~ "Apenas número zero",
                                !apresenta_latlong & !apresenta_lognum & numero == FALSE & !numero_zero ~ "Apenas número",
                                .default = "Nenhuma informação") |> 
           factor(levels = c("Completo", "Completo, mas número zero", "Apenas logradouro e número", "Apenas número zero") |> rev())) |>
  select(id_sinistro, data, completude, tipo) |> 
  left_join(match) |> 
  mutate(qualidade_match = case_when(similaridade == 1 & distancia_geografica < 100 & match_tipo & match_titulo ~ "Perfeito",
                                     similaridade > .9 & distancia_geografica < 200 & match_tipo & match_titulo ~ "Excelente",
                                     similaridade > .8 & distancia_geografica < 300 & (match_tipo | match_titulo) ~ "Bom",
                                     is.na(id_osm) ~ "Não encontrou match",
                                     .default = "Ruim") |> 
           factor(levels = c("Perfeito", "Excelente", "Bom", "Ruim", "Não encontrou match") |> rev()))

tabela |> 
  group_by(completude) |> 
  summarize(n = n()) |> 
  mutate(percent = n / sum(n)) |> 
  ggplot(aes(y = completude)) +
  geom_col(aes(x = n)) +
  geom_text(aes(x = n, label = percent |> round(3) |> scales::percent()), nudge_x = 10000) +
  scale_x_continuous(labels = scales::number) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Quantidade de sinistros", y = "Qualidade da informação na base")

ggsave("output/qualidade_georref_infosiga.pdf", width = 7, height = 4)

tabela |> 
  group_by(qualidade_match) |> 
  summarize(n = n()) |> 
  mutate(percent = n / sum(n)) |> 
  ggplot(aes(y = qualidade_match)) +
  geom_col(aes(x = n)) +
  geom_text(aes(x = n, label = percent |> round(3) |> scales::percent()), nudge_x = 10000) +
  scale_x_continuous(labels = scales::number) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Quantidade de sinistros", y = "Qualidade do match")

ggsave("output/qualidade_match.pdf", width = 7, height = 4)

rm(tabela)
