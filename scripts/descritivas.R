library(tidyverse)
library(sf)
library(mapview)
# library(ggsankey)
library(paletteer)
library(gganimate)

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




plot_obitos_ano <- function(sinistros){
  gg <- sinistros |> 
    filter(tipo == "SINISTRO FATAL", year(data) %in% 2015:2024) |> 
    group_by(data = make_date(year = year(data)), moto = replace_na(tp_veiculo_motocicleta, 0) > 0) |> 
    summarize(obitos = sum(gravidade_fatal)) |> 
    group_by(data) |> 
    mutate(moto = replace_na(moto, FALSE),
           y = ifelse(moto == TRUE, obitos, sum(obitos)),
           label = ifelse(moto == TRUE, scales::percent(obitos / sum(obitos)), sum(obitos))) |> 
    ungroup() |> 
    ggplot(aes(x = data)) +
    geom_col(aes(y = obitos, fill = moto), colour = "white") +
    geom_text(aes(y = y, label = label), nudge_y = -30, colour = "white") +
    scale_fill_manual("Veículo", 
                      values = c(adjustcolor("darkblue", blue.f = 1.1, alpha.f = .9), 
                                 adjustcolor("darkblue", blue.f = .7, alpha.f = .9)), 
                      labels = c("Outros", "Motocicletas")) +
    scale_x_date(NULL, date_breaks = "years", date_labels = "%Y") +
    labs(y = "Total de óbitos em decorrência de sinistros fatais") +
    theme_minimal()
  
  ggsave("output/plot_obitos_ano.pdf", gg, width = 10, height = 6)
}




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
                             select(id_sinistro, data, tipo, tp_veiculo_motocicleta)) |> 
                 mutate(motocicleta = replace_na(tp_veiculo_motocicleta, 0) > 0) |> 
                 arrange(motocicleta) |> 
                 filter(tipo == "SINISTRO FATAL")) +
    # geom_jitter() +
    scale_fill_manual("Veículo da vítima", values = c("TRUE" = "#BA0C2FFF", "FALSE" = "#C6AA76FF"), labels = c("TRUE" = "Motocicleta", "FALSE" = "Outros")) +
    scale_shape_manual("Veículo da vítima", values = c("TRUE" = 21, "FALSE" = 22), labels = c("TRUE" = "Motocicleta", "FALSE" = "Outros"))
  
  ggsave("output/plot_datas_FA_obitos.pdf", ggg, width = 11, height = 7)
  
}


# TRECHOS/LOGRADOUROS TRATADOS POR PERIODO
plot_trechos_vias_periodo <- function(faixa_azul, logradouros) {
  df <- faixa_azul |> 
    group_by(data_implementacao) |> 
    summarize(trechos = n()) |> 
    left_join(logradouros |> 
                filter(!is.na(data_implementacao)) |> 
                group_by(data_implementacao) |> 
                summarize(logradouros = n())) |> 
    filter(year(data_implementacao) <= 2024) |> 
    complete(data_implementacao = seq(min(data_implementacao), max(data_implementacao), by = "1 month"), 
             fill = list(trechos = 0, logradouros = 0)) |> 
    mutate(trechos_total = cumsum(trechos),
           trechos_label = if_else(trechos == 0, "", as.character(trechos_total))) |> 
    mutate(logradouros_total = cumsum(logradouros),
           logradouros_label = if_else(logradouros == 0, "", as.character(logradouros_total)))
  
  gg <- df |> 
    ggplot() +
    geom_col(aes(data_implementacao, trechos_total), fill = rgb(.05,.1,.3)) +
    geom_text(aes(data_implementacao, trechos_total, label = trechos_label), vjust = -0.5, hjust = 0.8) +
    labs(x = NULL, y = "Número de trechos",
         title = "Trechos implementados por período") +
    theme_minimal(base_size = 14)
  ggsave("output/did/trechos_implementados.png", gg, width = 8, height = 6, dpi = 300)
  
  gg <- df |> 
    ggplot() +
    geom_col(aes(data_implementacao, logradouros_total), fill = rgb(.05,.1,.3)) +
    geom_text(aes(data_implementacao, logradouros_total, label = logradouros_label), vjust = -0.5, hjust = 0.8) +
    labs(x = NULL, y = "Número de vias",
         title = "Vias implementadas por período") +
    theme_minimal(base_size = 14)
  ggsave("output/did/logradouros_implementados.png", gg, width = 8, height = 6, dpi = 300)
}



# TAMANHO VIAS ----

plot_tamanho_FA <- function(logradouros_id, logradouros, faixa_azul, trechos){
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
  ggsave("output/plot_tamanho_FA.pdf", gg, width = 10, height = 6)
  
} 




# PANFLETO CET ----

plot_obitos_tempo <- function(sinistros, match, faixa_azul, logradouros, id_logradouros){
  gg <- sinistros |> 
    filter(tipo == "SINISTRO FATAL", year(data) > 2015) |> 
    left_join(match, by = join_by(id_sinistro)) |> 
    left_join(faixa_azul |> distinct()) |> 
    mutate(match_sucesso = is.na(id_osm) == FALSE,
           faixa_azul = is.na(data_implementacao) == FALSE) |> 
    group_by(ano = year(data), match_sucesso, faixa_azul) |> 
    summarize(mortes = sum(gravidade_fatal)) |> 
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
    scale_fill_viridis_d(option = "mako", direction = -1, end = .6, begin = .2) +
    theme_minimal() +
    labs(x = NULL, fill = "Local do sinistro", y = "Óbitos",
         title = "Óbitos totais - por tipo de via")
  ggsave("output/plot_obitos_tempo_total.pdf", gg, width = 8, height = 5)
  
  gg <- sinistros |> 
    filter(tipo == "SINISTRO FATAL", year(data) > 2015, !is.na(tp_veiculo_motocicleta)) |> 
    left_join(match, by = join_by(id_sinistro)) |> 
    left_join(faixa_azul |> distinct()) |> 
    mutate(match_sucesso = is.na(id_osm) == FALSE,
           faixa_azul = is.na(data_implementacao) == FALSE) |> 
    group_by(ano = year(data), match_sucesso, faixa_azul) |> 
    summarize(mortes = sum(gravidade_fatal)) |> 
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
    scale_fill_viridis_d(option = "mako", direction = -1, end = .6, begin = .2) +
    theme_minimal() +
    labs(x = NULL, fill = "Local do sinistro", y = "Óbitos",
         title = "Óbitos em sinistros com moto - por tipo de via")
  ggsave("output/plot_obitos_tempo_moto.pdf", gg, width = 8, height = 5)
  
  gg <- sinistros |> 
    filter(tipo == "SINISTRO FATAL", year(data) > 2015) |> 
    left_join(match, by = join_by(id_sinistro)) |> 
    semi_join(faixa_azul |> select(id_osm)) |> 
    group_by(ano = year(data)) |> 
    summarize(mortes = sum(gravidade_fatal)) |> 
    ggplot(aes(x = factor(ano))) +
    geom_col(aes(y = mortes), fill = rgb(.05,.1,.3)) +
    geom_text(aes(y = mortes, label = mortes), vjust = -0.5) +
    theme_minimal() +
    labs(x = NULL, y = "Óbitos",
         title = "Óbitos totais - trechos que receberam Faixa Azul")
  ggsave("output/plot_obitos_tempo_FA.pdf", gg, width = 8, height = 5)
  
  
  gg <- sinistros |> 
    filter(tipo == "SINISTRO FATAL", year(data) > 2015, !is.na(tp_veiculo_motocicleta)) |> 
    left_join(match, by = join_by(id_sinistro)) |> 
    semi_join(faixa_azul |> select(id_osm)) |> 
    group_by(ano = year(data)) |> 
    summarize(mortes = sum(gravidade_fatal)) |> 
    ggplot(aes(x = factor(ano))) +
    geom_col(aes(y = mortes), fill = rgb(.05,.1,.3)) +
    geom_text(aes(y = mortes, label = mortes), vjust = -0.5) +
    theme_minimal() +
    labs(x = NULL, y = "Óbitos",
         title = "Óbitos em sinistros com moto - trechos que receberam Faixa Azul")
  ggsave("output/plot_obitos_tempo_FA_moto.pdf", gg, width = 8, height = 5)
  
  gg <- sinistros |> 
    filter(tipo == "SINISTRO FATAL", year(data) > 2015) |> 
    left_join(match, by = join_by(id_sinistro)) |>
    semi_join(logradouros |> 
                filter(!is.na(data_implementacao)) |> 
                left_join(id_logradouros |> rename(id_osm = trechos)) |> 
                unnest(id_osm) |> 
                select(id_osm)) |> 
    group_by(ano = year(data)) |> 
    summarize(obitos = sum(gravidade_fatal)) |> 
    ggplot(aes(x = factor(ano))) +
    geom_col(aes(y = obitos), fill = rgb(.05,.1,.3)) +
    geom_text(aes(y = obitos, label = obitos), vjust = -0.5) +
    labs(x = NULL, y = "Óbitos",
         title = "Óbitos totais - vias que receberam Faixa Azul") +
    theme_minimal()
  ggsave("output/plot_obitos_tempo_FA_logradouro.pdf", gg, width = 8, height = 5)
  
  gg <- sinistros |> 
    filter(tipo == "SINISTRO FATAL", year(data) > 2015, !is.na(tp_veiculo_motocicleta)) |> 
    left_join(match, by = join_by(id_sinistro)) |>
    semi_join(logradouros |> 
                filter(!is.na(data_implementacao)) |> 
                left_join(id_logradouros |> rename(id_osm = trechos)) |> 
                unnest(id_osm) |> 
                select(id_osm)) |> 
    group_by(ano = year(data)) |> 
    summarize(obitos = sum(gravidade_fatal)) |> 
    ggplot(aes(x = factor(ano))) +
    geom_col(aes(y = obitos), fill = rgb(.05,.1,.3)) +
    geom_text(aes(y = obitos, label = obitos), vjust = -0.5) +
    labs(x = NULL, y = "Óbitos",
         title = "Óbitos em sinistros com moto - vias que receberam Faixa Azul") +
    theme_minimal()
  ggsave("output/plot_obitos_tempo_FA_logradouro_moto.pdf", gg, width = 8, height = 5)
  
  
}


# 
# 
# 
# 
# sinistros |> 
#   filter(tipo == "SINISTRO FATAL", year(data) > 2015) |> 
#   left_join(match, by = join_by(id_sinistro)) |> 
#   group_by(ano = year(data)) |> 
#   summarize(mortes = sum(gravidade_fatal)) |> 
#   ggplot(aes(x = factor(ano))) +
#   geom_col(aes(y = mortes), fill = rgb(.05,.1,.3)) +
#   geom_text(aes(y = mortes, label = mortes), vjust = -0.5) +
#   theme_minimal() +
#   labs(x = NULL, y = "Óbitos",
#        title = "Óbitos totais - todas as vias")
# ggsave("output/comparacao_panfleto/obitos_tempo_trecho_todos.pdf", width = 8, height = 5)
# ggsave("output/comparacao_panfleto/obitos_tempo_trecho_todos.png", width = 8, height = 7, dpi = 400)

# 
# sinistros |> 
#   filter(tipo == "SINISTRO FATAL", year(data) > 2015, motocicletas != 0) |> 
#   left_join(match, by = join_by(id_sinistro)) |> 
#   group_by(ano = year(data)) |> 
#   summarize(mortes = sum(gravidade_fatal)) |> 
#   ggplot(aes(x = factor(ano))) +
#   geom_col(aes(y = mortes), fill = rgb(.05,.1,.3)) +
#   geom_text(aes(y = mortes, label = mortes), vjust = -0.5) +
#   theme_minimal() +
#   labs(x = NULL, y = "Óbitos",
#        title = "Óbitos em sinistros com moto - todas as vias")
# ggsave("output/comparacao_panfleto/obitos_tempo_trecho_todos_moto.pdf", width = 8, height = 5)
# ggsave("output/comparacao_panfleto/obitos_tempo_trecho_todos_moto.png", width = 8, height = 7, dpi = 400)



# TAMANHO DA FROTA ----
# 
# frota <- read_csv("dados_tratados/frota_sp.csv")
# 
# left_join(
#   sinistros |>
#     filter(tipo == "SINISTRO FATAL", year(data) > 2015, motocicletas != 0) |>
#     group_by(ano = year(data)) |>
#     summarize(mortes = sum(quantidade_envolvidos)),
#   frota |>
#     filter(mes == 12, tipo_veiculo %in% c("motocicleta", "motoneta")) |>
#     group_by(ano) |>
#     summarize(motocicletas = sum(quantidade))) |>
#   pivot_longer(mortes:motocicletas) |>
#   group_by(name) |>
#   mutate(num_indice = (value / first(value)) * 100) |>
#   ggplot(aes(x = factor(ano), y = num_indice)) +
#   geom_line(aes(group = name, colour = name)) +
#   geom_point(colour = "grey50") +
#   theme_minimal() +
#   # scale_y_continuous(labels = scales::percent) +
#   scale_colour_manual(NULL, values = c(rgb(.3, .05, .1), rgb(.9, .8, .5)),
#                       labels = c("Óbitos em sinistros fatais que envolveram motocicleta", "Frota total de motocicletas e motonetas")) +
#   theme(legend.position = "top",) +
#   labs(y = "Variação (2016 = 100)", x = NULL) +
#   guides(colour=guide_legend(nrow=2,byrow=TRUE))
# 
# ggsave("output/comparacao_panfleto/frota_vs_mortes.pdf", width = 6, height = 5)
# 
# 
# SINISTROS EM CADA HORA DO DIA ----

plot_hora_sinistro <- function(sinistros){
  gg <- sinistros |>
    filter(year(data) > 2018, year(data) <= 2023) |>
    mutate(mes = fct_collapse(month(data) |> factor(),
                              "Jan-Mar" = 1:3,
                              "Abr-Jun" = 4:6,
                              "Jul-Set" = 7:9,
                              "Out-Dez" = 10:12,
                              other_level = "teste")) |>
    group_by(hora, dia = day(data), mes) |>
    summarize(sinistros = n()) |>
    ggplot(aes(x = dia, y = hora, fill = sinistros)) +
    geom_tile() +
    facet_grid(cols = vars(mes)) +
    theme_minimal() +
    scale_fill_viridis_c() +
    scale_y_continuous("Horário", breaks = 0:11*2) +
    scale_x_continuous("Dia do mês", breaks = NULL)
  
  ggsave("output/plot_horarios_sinistros.pdf", gg, width = 10, height =4)
}



plot_datas_trechos <- function(faixa_azul, trechos, token_osm){
  gg <- faixa_azul |>
    left_join(trechos |> st_drop_geometry() |> select(id_osm, comprimento)) |>
    group_by(data_implementacao) |>
    summarize(trechos = n(),
              comprimento = sum(comprimento)) |>
    complete(data_implementacao = seq(min(data_implementacao), max(data_implementacao), by = "1 month"), fill = list(trechos = 0, comprimento = 0)) |>
    mutate(trechos_total = cumsum(trechos),
           trechos_label = if_else(trechos == 0, "", as.character(trechos_total)),
           comprimento_total = cumsum(comprimento)) |>
    ggplot(aes(x= data_implementacao)) +
    geom_col(aes(y = trechos_total), fill = rgb(.05,.1,.3)) +
    geom_text(aes(y = trechos_total, label = trechos_label), vjust = -0.5, hjust = 0.8) +
    labs(x = NULL, y = "Número de trechos",
         title = "Trechos implementados por período") +
    theme_minimal()
  ggsave("output/plot_datas_ntrechos.pdf", gg, width = 10, height = 7)
  
  
  gg <- faixa_azul |>
    left_join(token_osm) |> 
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
  
  ggsave("output/plot_datas_nlogradouros.pdf", gg, width = 10, height = 7)

  
  gg <- faixa_azul |>
    left_join(trechos |> st_drop_geometry() |> select(id_osm, comprimento)) |>
    left_join(token_osm |> 
                arrange(alias) |> 
                group_by(id_osm) |> 
                filter(row_number() == 1) |> 
                select(id_osm, logradouro = logradouro_limpo)) |>
    mutate(logradouro = logradouro |> 
             factor() |> 
             fct_reorder(desc(data_implementacao))) |>
    group_by(data_implementacao, logradouro) |>
    summarize(trechos = sum(comprimento) / 1000) |> ungroup() |>
    complete(data_implementacao = seq(min(data_implementacao), max(data_implementacao), by = "1 month"), logradouro, fill = list(trechos = 0)) |>
    group_by(logradouro) |>
    mutate(trechos_total = cumsum(trechos),
           trechos_label = if_else(trechos == 0, "", as.character(trechos_total))) |>
    ggplot(aes(x= data_implementacao)) +
    geom_col(aes(y = trechos_total, fill = logradouro), colour = "white", lwd = .1) +
    labs(x = NULL, y = "Soma dos quilômetros implementados",
         title = NULL) +
    theme_minimal() +
    # scale_fill_viridis_d(direction = -1) +
    scale_fill_manual("", values = paletteer::paletteer_d("ggsci::default_igv")) +
    theme(legend.position = "bottom",
          legend.text=element_text(size=8))
  
  ggsave("output/plot_datas_comprimento_separado.pdf", gg, width = 12, height = 8)
  

  
}



# # QUALIDADE DO MATCH ----
# 


plot_qualidade_match <- function(sinistros, match){
  
  
  tabela <- sinistros |>
    filter(tipo != "NOTIFICACAO", year(data) > 2018) |>
    select(id_sinistro, data, logradouro, numero, latitude, longitude, tipo) |>
    mutate(numero_zero = as.numeric(numero) == 0) |>
    select(id_sinistro, data, numero_zero) |>
    left_join(match) |>
    mutate(qualidade_match = case_when(similaridade == 1 & distancia_geografica < 50 & match_tipo & match_titulo & numero_zero == FALSE ~ "Perfeito",
                                       similaridade > .85 & distancia_geografica < 150 & (match_tipo | match_titulo) & numero_zero == FALSE ~ "Excelente",
                                       similaridade > .7 & distancia_geografica < 250 ~ "Aceitável",
                                       is.na(id_osm) ~ "Não encontrou match",
                                       .default = "Ruim") |>
             factor(levels = c("Perfeito", "Excelente", "Aceitável", "Ruim", "Não encontrou match") |> rev()))
  
  gg <- tabela |>
    group_by(qualidade_match) |>
    summarize(n = n()) |>
    mutate(percent = n / sum(n)) |>
    ggplot(aes(y = qualidade_match)) +
    geom_col(aes(x = n)) +
    geom_text(aes(x = n, label = percent |> round(3) |> scales::percent()), nudge_x = 7500) +
    scale_x_continuous(labels = scales::number, expand = expansion(mult = c(0.05, 0.075))) +
    theme_minimal() +
    theme(legend.position = "none",
          plot.caption = element_text(hjust = 0),
          plot.margin = margin(10,30,10,10)) +
    labs(x = "Quantidade de sinistros", y = "Qualidade do match", 
         # caption = paste(c("DEFINIÇÕES", 
         #                   "Perfeito: nome exato (similaridade de 100%), distância < 50m, mesmo tipo e título, número não é zero",
         #                   "Excelente: similaridade no nome > 85%, distância < 150m, tipo ou título iguais, número não é zero",
         #                   "Aceitável: similaridade no nome > 70%, distância < 250m",
         #                   "Ruim: restante"), 
         #                 collapse = "\n")
    )
  
  
  ggsave("output/plot_qualidade_match.pdf", gg, width = 6, height = 6)
}






# MAPA SINISTROS ----
plot_mapas <- function(sinistros, trechos, faixa_azul){
  distrito <- st_read("dados_tratados/distrito/SIRGAS_SHP_distrito.shp") |>
    st_set_crs("epsg:31983") |>
    summarize(geometry = st_union(geometry) |> st_simplify(dTolerance = 100))
  
  trechos.mapa <- trechos |> 
    filter(!tipo_via %in% c("service", "unclassified")) |> 
    st_transform("epsg:31983") |> 
    st_intersection(distrito)
  
  gg <- sinistros |>
    filter(tipo != "NOTIFICACAO", !is.na(longitude), !is.na(latitude)) |>
    st_as_sf(coords = c("longitude", "latitude"), crs = "EPSG:4326") |>
    st_transform(crs = "epsg:31983") |>
    st_intersection(distrito) |>
    st_coordinates() |>
    ggplot() +
    geom_sf(data = distrito,
            aes(geometry = geometry), colour = NA, fill = "grey98") +
    geom_sf(data = trechos.mapa |>
              filter(tipo_via %in% c("trunk", "primary", "secondary")),
            aes(geometry = geometry), colour = "#3c3744", lwd = .3, alpha = .7) +
    geom_hex(aes(x = X, y = Y), alpha = .7, bins = 40) +
    geom_sf(data = distrito,
            aes(geometry = geometry), colour = "grey25", fill = NA, alpha = .7) +
    scale_fill_gradient("Número de sinistros", low = "grey98", high = "darkred") +
    theme_void()
  
  ggsave("output/mapa_sinistros.pdf", gg, width = 10, height = 15)
  
  gg <- ggplot() +
    geom_sf(data = distrito,
            aes(geometry = geometry), colour = "grey25", fill = "grey98") +
    geom_sf(data = trechos.mapa |> 
              filter(!tipo_via %in% c("trunk", "primary", "secondary")),
            aes(geometry = st_simplify(geometry, dTolerance = 10)), colour = "grey50", lwd = .15, alpha = .8) +
    geom_sf(data = trechos.mapa |> 
              filter(tipo_via %in% c("trunk", "primary", "secondary")),
            aes(geometry = st_simplify(geometry, dTolerance = 10)), colour = "#3c3744", lwd = .3, alpha = .8) +
    geom_sf(data = trechos.mapa |> 
              semi_join(faixa_azul),
            aes(geometry = geometry), colour = "#090c9b", lwd = 3, alpha = .1) +
    geom_sf(data = trechos.mapa |> 
              semi_join(faixa_azul),
            aes(geometry = geometry), colour = "#090c9b", lwd = 1, alpha = .1) +
    geom_sf(data = trechos.mapa |> 
              semi_join(faixa_azul),
            aes(geometry = geometry), colour = "#090c9b", lwd = .5, alpha = .8) +
    theme_void()
  
  ggsave("output/mapa_faixa_azul.pdf", gg, width = 10, height = 15)
  
  df <- faixa_azul |> 
    left_join(trechos |> st_transform("epsg:31983")) |> 
    group_by(data_implementacao = data_implementacao) |> 
    summarize(geometry = st_union(geometry)) |> 
    (\(df) df |>
       select(data_implementacao) |> 
       mutate(grupo = rep(list(1:length(data_implementacao)), length(data_implementacao))) |> 
       unnest(grupo) |> 
       left_join(df |> 
                   mutate(grupo = 1:length(data_implementacao)) |> 
                   select(-data_implementacao)))() |> 
    filter(grupo <= data_implementacao |> factor() |> as.numeric())
  
  gg <- ggplot(mapping = aes(geometry = geometry)) +
    geom_sf(data = distrito, colour = "grey25", fill = "grey98") +
    geom_sf(data = df, aes(colour = factor(grupo)), lwd = 1, alpha = .8) +
    transition_time(data_implementacao) +
    scale_colour_viridis_d() +
    labs(title = "{frame_time}") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, size = 80),
          legend.position = "none")
  
  anim_save("output/mapa_faixa_azul.gif", gg, 
            renderer = gifski_renderer(loop = T), 
            width = 2250, height = 3000, 
            duration = 10, end_pause = 20)
}






