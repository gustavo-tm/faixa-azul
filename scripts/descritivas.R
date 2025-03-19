library(tidyverse)
library(sf)
library(units)
library(mapview)
# library(ggsankey)
library(paletteer)

# trechos <- tar_read(dado_trechos)
# logradouros <- tar_read(dado_logradouros)
# logradouros_id <- tar_read(dado_id_logradouros)
# sinistros <- tar_read(dado_sinistros)
# faixa_azul <- tar_read(dado_faixa_azul)
# match <- tar_read(dado_match)
# osm.token <- tar_read(dado_token_osm) |> 
#   mutate(len = str_length(logradouro_limpo)) |> 
#   group_by(id_osm) |> 
#   arrange(-len) |> 
#   filter(row_number() == 1) |> 
#   ungroup() |> 
#   arrange(id_osm)



plot_datas_FA <- function(logradouros, logradouros_id, match, sinistros){
  
  max_data <- max(sinistros$data)
  
  gg <- logradouros |> 
    filter(!is.na(data_implementacao)) |> 
    group_by(nome) |> 
    filter(row_number(data_implementacao) == 1) |> 
    ungroup() |> 
    mutate(nome = factor(nome) |> fct_reorder(desc(data_implementacao))) |> 
    ggplot(aes(y = nome)) +
    geom_rect(aes(xmin = as.Date(-Inf), 
                  xmax = data_implementacao, 
                  ymin = as.numeric(nome) - .15, 
                  ymax = as.numeric(nome) + .15),
              fill = "grey90") +
    geom_rect(aes(xmin = data_implementacao, 
                  xmax = as.Date(Inf), 
                  ymin = as.numeric(nome) - .25, 
                  ymax = as.numeric(nome) + .25),
              fill = "#333F48FF", alpha = .75) +
    scale_x_date(limits = c(make_date(year = 2021, month = 6), max_data)) +
    labs(y = NULL, x = NULL) +
    theme_minimal()
  ggsave("output/plot_datas_FA.pdf", gg, width = 11, height = 7)
  
  ggg <- gg +
    geom_point(aes(x = data, fill = motocicleta, shape = motocicleta),
               position = position_jitter(width = 0, height = .1),
               alpha = .9, stroke = .15, colour = "white", size = 2.5,
               data = logradouros_id |> 
                 semi_join(logradouros |> 
                             filter(!is.na(data_implementacao)) |> 
                             select(id_logradouro)) |> 
                 unnest(trechos) |> 
                 rename(id_osm = trechos, nome = logradouro) |> 
                 inner_join(match |> select(id_sinistro, id_osm)) |> 
                 left_join(sinistros |> 
                             select(id_sinistro, data, tipo, motocicletas)) |> 
                 mutate(motocicleta = replace_na(motocicletas, 0) > 0) |> 
                 arrange(motocicleta) |> 
                 filter(tipo == "SINISTRO FATAL")) +
    # geom_jitter() +
    scale_fill_manual("Veículo da vítima", values = c("TRUE" = "#BA0C2FFF", "FALSE" = "#C6AA76FF"), labels = c("TRUE" = "Motocicleta", "FALSE" = "Outros")) +
    scale_shape_manual("Veículo da vítima", values = c("TRUE" = 21, "FALSE" = 22), labels = c("TRUE" = "Motocicleta", "FALSE" = "Outros"))
  
  ggsave("output/plot_datas_FA_obitos.pdf", ggg, width = 11, height = 7)
  
}

# TAMANHO VIAS ----

plot_tamanho_FA <- function(){
  gg <- logradouros_id |> 
    semi_join(logradouros |> 
                filter(!is.na(data_implementacao)) |> 
                select(logradouro = nome)) |> 
    unnest(trechos) |> 
    rename(id_osm = trechos) |> 
    left_join(faixa_azul |> select(id_osm) |> mutate(FA = TRUE)) |> 
    left_join(trechos |> select(id_osm, comprimento)) |> 
    mutate(FA = replace_na(FA, FALSE)) |> 
    group_by(logradouro, FA) |>
    summarize(comprimento = sum(comprimento) / 1000) |> 
    mutate(ordem = sum(comprimento)) |> 
    ggplot(aes(x = comprimento, y = reorder(logradouro,ordem))) +
    geom_col(aes(fill = FA), colour = "black", lwd = .1) +
    scale_fill_manual("", values = c("FALSE" = "#A6A6A6", "TRUE" = "#4472C4"),
                      labels = c("TRUE" = "Com faixa azul", "FALSE" = "Sem faixa azul")) +
    labs(x = "Comprimento da via (km)", y = NULL) +
    theme_minimal()
  ggsave("output/plot_tamanho_FA.pdf", gg, width = 7, height = 6)
  
} 

logradouros_id |> 
  semi_join(logradouros |> 
              filter(!is.na(data_implementacao)) |> 
              select(logradouro = nome)) |> 
  unnest(trechos) |> 
  rename(id_osm = trechos) |> 
  filter(logradouro == " CAXINGUI ") |> 
  left_join(faixa_azul |> select(id_osm) |> mutate(FA = TRUE)) |> 
  left_join(trechos |> select(id_osm, comprimento)) |> 
  mutate(FA = replace_na(FA, FALSE)) |> 
  group_by(logradouro, FA) |>
  summarize(comprimento = sum(comprimento) / 1000) |> 
  mutate(ordem = sum(comprimento)) |> 
  ggplot(aes(x = comprimento, y = reorder(logradouro,ordem))) +
  geom_col(aes(fill = FA), colour = "black", lwd = .1) +
  scale_fill_manual("", values = c("FALSE" = "#A6A6A6", "TRUE" = "#4472C4"),
                    labels = c("TRUE" = "Com faixa azul", "FALSE" = "Sem faixa azul")) +
  labs(x = "Comprimento da via (km)", y = NULL) +
  theme_minimal()

logradouros_id |> 
  semi_join(logradouros |> 
              filter(!is.na(data_implementacao)) |> 
              select(logradouro = nome)) |> 
  unnest(trechos) |> 
  rename(id_osm = trechos) |> 
  filter(logradouro == " CAXINGUI ") |> 
  select(id_osm) |> 
  left_join(trechos) |> 
  View()
  

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

# MAPA SINISTROS ----

distrito <- st_read("dados_tratados/distrito/SIRGAS_SHP_distrito.shp") |> 
  st_set_crs("epsg:31983") |> 
  summarize(geometry = st_union(geometry) |> st_simplify(dTolerance = 100))

gg <- sinistros |> 
  filter(tipo != "NOTIFICACAO", !is.na(longitude), !is.na(latitude)) |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = "EPSG:4326") |> 
  st_transform(crs = "epsg:31983") |>
  st_intersection(distrito) |>
  st_coordinates() |> 
  ggplot() +
  geom_sf(data = distrito,
          aes(geometry = geometry), colour = NA, fill = "grey98") +
  geom_sf(data = trechos |> 
            filter(!tipo_via %in% c("service", "unclassified")) |> 
            st_transform("epsg:31983") |> 
            st_intersection(distrito) |> 
            filter(tipo_via %in% c("trunk", "primary", "secondary")),
          aes(geometry = st_simplify(geom, dTolerance = 10)), colour = "#3c3744", lwd = .3, alpha = .7) +
  geom_hex(aes(x = X, y = Y), alpha = .7, bins = 40) +
  geom_sf(data = distrito,
          aes(geometry = geometry), colour = "grey25", fill = NA, alpha = .7) +
  scale_fill_gradient("Número de sinistros", low = "grey98", high = "darkred") +
  theme_void()
  
ggsave("output/mapa_sinistros.pdf", gg, width = 10, height = 15)


# COMPLETUDE DADOS IFOOD ----

dado_disponivel <- read_csv("sala_segura/20-12-24/dado_disponivel.csv", col_types = c("id_osm" = "c"))

tabela <- trechos |> 
  as_tibble() |> 
  filter(tipo_via %in% c("trunk", "primary", "secondary")) |> 
  select(id_osm, comprimento) |> 
  left_join(faixa_azul) |> 
  mutate(faixa_azul = !is.na(data_implementacao)) |> 
  select(-data_implementacao)

tabela |> 
  left_join(dado_disponivel |> 
              group_by(id_osm) |> 
              summarize(meses_disponiveis = sum(dado))) |> 
  mutate(meses_disponiveis = replace_na(meses_disponiveis, 0)) |> 
  group_by(meses_disponiveis) |> 
  summarize(n = n()) |> 
  arrange(-meses_disponiveis) |> 
  mutate(percentual = ((n) / sum(n)) |> round(3) |> scales::percent()) |> 
  ggplot(aes(y = factor(meses_disponiveis), x = n)) +
  geom_col() +
  geom_text(aes(label = percentual), nudge_x = 1000) +
  theme_minimal() +
  labs(x = "Número de trechos", y = "Número de meses em que há dados disponíveis")

ggsave("output/ifood/completude_ifood.pdf", width = 5, height = 5)


tabela |> 
  left_join(dado_disponivel |> 
              group_by(id_osm) |> 
              summarize(meses_disponiveis = sum(dado))) |> 
  mutate(meses_disponiveis = replace_na(meses_disponiveis, 0)) |> 
  group_by(meses_disponiveis) |> 
  summarize(n = sum(comprimento)) |> 
  arrange(-meses_disponiveis) |> 
  mutate(percentual = ((n) / sum(n)) |> round(3) |> scales::percent()) |> 
  ggplot(aes(y = factor(meses_disponiveis), x = n)) +
  geom_col() +
  geom_text(aes(label = percentual), nudge_x = 150000) +
  theme_minimal() +
  scale_x_continuous(labels = scales::number) +
  labs(x = "Comprimento agregado de trechos (metros)", y = "Número de meses em que há dados disponíveis")

ggsave("output/ifood/completude_ifood_comprimento.pdf", width = 5, height = 5)


tabela |> 
  left_join(dado_disponivel |> 
              group_by(id_osm) |> 
              summarize(meses_disponiveis = sum(dado))) |> 
  mutate(meses_disponiveis = replace_na(meses_disponiveis, 0)) |> 
  filter(faixa_azul == TRUE) |> 
  group_by(meses_disponiveis) |> 
  summarize(n = n()) |> 
  arrange(-meses_disponiveis) |> 
  mutate(percentual = ((n) / sum(n)) |> round(3) |> scales::percent()) |> 
  ggplot(aes(y = factor(meses_disponiveis), x = n)) +
  geom_col(fill = "darkblue") +
  geom_text(aes(label = percentual), nudge_x = 25) +
  theme_minimal() +
  labs(x = "Número de trechos de faixa azul", y = "Número de meses em que há dados disponíveis")

ggsave("output/ifood/completude_ifood_faixa_azul.pdf", width = 5, height = 5)

tabela |> 
  left_join(dado_disponivel |> 
              group_by(id_osm) |> 
              summarize(meses_disponiveis = sum(dado))) |> 
  mutate(meses_disponiveis = replace_na(meses_disponiveis, 0)) |> 
  filter(faixa_azul == TRUE) |> 
  group_by(meses_disponiveis) |> 
  summarize(n = sum(comprimento)) |> 
  arrange(-meses_disponiveis) |> 
  mutate(percentual = ((n) / sum(n)) |> round(3) |> scales::percent()) |> 
  ggplot(aes(y = factor(meses_disponiveis), x = n)) +
  geom_col(fill = "darkblue") +
  geom_text(aes(label = percentual), nudge_x = 7000) +
  theme_minimal() +
  scale_x_continuous(labels = scales::number) +
  labs(x = "Comprimento agregado de trechos de faixa azul (metros)", y = "Número de meses em que há dados disponíveis")

ggsave("output/ifood/completude_ifood_faixa_azul_comprimento.pdf", width = 5, height = 5)
