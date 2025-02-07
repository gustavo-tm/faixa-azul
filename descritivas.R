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
         logradouro_completo = factor(logradouro_completo) |> fct_reorder(desc(data_implementacao))) |> View()
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










