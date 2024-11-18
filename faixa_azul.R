library(tidyverse)
library(sf)
library(units)
library(mapview)
library(ggsankey)

# LINHA DO TEMPO ----

df <- read_csv("dados_tratados/sinistros_logradouros.csv")
faixa_azul <- readxl::read_excel("dados_tratados/vias_faixa_azul.xlsx")

fix.alias <- function(df){
  df |> 
    mutate(logradouro = case_when(logradouro == "AVENIDA VINTE E TRES DE MAIO" ~ "AVENIDA 23 DE MAIO",
                                  logradouro == "RUA MIGUEL YUNES" ~ "AVENIDA MIGUEL YUNES",
                                  logradouro == "TUNEL AYRTON SENNA" ~ "ACESSO TUNEL AYRTON SENNA",
                                  TRUE ~ logradouro))
}


obitos <- read_csv("dados_tratados/sinistros.csv") |> 
  filter(tipo == "SINISTRO FATAL") |> 
  fix.alias()


df |>
  right_join(faixa_azul |> select(logradouro, id_logradouro)) |> 
  group_by(id_logradouro, mes, ano) |>
  summarize(acidentes = sum(sinistros),
            logradouro = first(logradouro)) |>
  left_join(faixa_azul |>
              mutate(data_faixa_azul = make_date(year = ano, month = mes)) |>
              select(logradouro, data_faixa_azul, id_trecho)) |>
  mutate(data = make_date(year = ano, month = mes),
         faixa_azul =  data > data_faixa_azul) |> 
  ggplot(aes(x = data, y = reorder(logradouro, id_trecho),
             lwd = faixa_azul,
             alpha = faixa_azul)) +
  geom_line() +
  scale_x_date(limits = c(make_date(year = 2021, month = 1), make_date(year = 2024, month = 9))) +
  scale_linewidth_manual(values = c("TRUE" = 2, "FALSE" = 1.5), labels = c("TRUE" = "Implementado", "FALSE" = "Não Há")) +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = .20), labels = c("TRUE" = "Implementado", "FALSE" = "Não Há")) +
  guides(linewidth = guide_legend("Faixa Azul"),
         alpha = guide_legend("Faixa Azul")) +
  labs(title = "Evolução da implementação das faixas azuis em São Paulo", x = "", y = "") +
  theme_minimal() +
  theme(plot.title.position = "plot",
        plot.title = element_text(hjust = 0.5))

ggsave("output/evolucao_faixas.pdf", width = 8, height = 5)

df |> 
  right_join(faixa_azul |> select(logradouro, id_logradouro)) |> 
  group_by(id_logradouro, mes, ano) |> 
  summarize(sinistros = sum(sinistros),
            logradouro = first(logradouro)) |> 
  left_join(faixa_azul |> 
              mutate(data_faixa_azul = make_date(year = ano, month = mes)) |> 
              select(logradouro, data_faixa_azul, id_trecho)) |> 
  mutate(data = make_date(year = ano, month = mes),
         faixa_azul =  data > data_faixa_azul) |>
  ggplot(aes(x = data)) +
  geom_line(aes(lwd = faixa_azul, y = reorder(logradouro, sinistros), alpha = faixa_azul)) +
  geom_point(data = obitos |>
               semi_join(faixa_azul) |> 
               mutate(motocicleta = as.logical(motocicleta)) |> 
               select(data, logradouro, motocicleta),
             aes(y = logradouro, fill = factor(motocicleta), shape = factor(motocicleta)), size = 2, alpha = .8, stroke = .1, colour = "white") +
  scale_x_date(limits = c(make_date(year = 2021, month = 1), make_date(year = 2024, month = 9))) +
  scale_linewidth_manual(values = c("TRUE" = 2, "FALSE" = 1.5), labels = c("TRUE" = "Pós faixa azul", "FALSE" = "Pré faixa azul")) +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = .20), labels = c("TRUE" = "Pós faixa azul", "FALSE" = "Pré faixa azul")) +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "purple"), labels = c("TRUE" = "Motocicleta", "FALSE" = "Outros")) +
  scale_shape_manual(values = c("TRUE" = 21, "FALSE" = 22), labels = c("TRUE" = "Motocicleta", "FALSE" = "Outros")) +
  guides(linewidth = guide_legend(""),
         alpha = guide_legend(""),
         fill = guide_legend("Veículo da vítima fatal"),
         shape = guide_legend("Veículo da vítima fatal")) +
  labs(title = "Evolução da implementação das faixas azuis em São Paulo", x = "", y = "") +
  theme_minimal() +
  theme(plot.title.position = "plot",
        plot.title = element_text(hjust = 0.5))

ggsave("output/obitos.pdf", width = 10, height = 6)

# MAPA ----

distrito <- read_sf("dados_tratados/distrito/SIRGAS_SHP_distrito.shp") |> 
  st_set_crs("epsg:31983") |> 
  filter(!ds_nome %in% c("PARELHEIROS", "MARSILAC", "RIO PEQUENO", "JAGUARE"))

logradouros <- st_read("dados_tratados/logradouros_osm.gpkg") |> 
  st_transform("epsg:31983") |> 
  mutate(logradouro = logradouro |> 
           stringi::stri_trans_general("latin-ascii") |> 
           str_to_upper() |> 
           str_replace_all("[[:punct:]]", "")) |> 
  fix.alias()


faixa_azul <- readxl::read_excel("dados_tratados/vias_faixa_azul.xlsx") |> 
  select(logradouro = logradouro_osm, ano, mes) |> 
  mutate(data = make_date(year = ano, month = mes)) |> 
  select(-ano, -mes)

mapa.todas <- logradouros |> 
  right_join(faixa_azul) |>
  filter(st_intersects(geom, distrito |> st_union()) |> as.logical()) |>
  mutate(data = as.factor(data)) |> 
  mapview(zcol = "data")

mapa.selecao <- logradouros |> 
  right_join(faixa_azul) |>
  filter(st_intersects(geom, distrito |> st_union()) |> as.logical()) |> 
  semi_join(read_csv("dados_tratados/faixa_azul_selecao.csv") |> mutate(id_osm = as.character(id_osm))) |> 
  mapview(color = "red")

(mapa.todas + mapa.selecao) |> mapshot(url = "output/mapas/logradouros_faixa_azul.html")
# 
# logradouros |>
#   semi_join(faixa_azul) |> 
#   left_join(read_csv("dados_tratados/faixa_azul_selecao.csv") |> 
#               mutate(faixa_azul = "faixa_azul", id_osm = as.character(id_osm))) |> 
#   mutate(tamanho = st_length(geom)) |>
#   st_drop_geometry() |> 
#   group_by(logradouro, faixa_azul) |> 
#   summarize(tamanho = tamanho |> as.numeric() |> sum()) |> 
#   ungroup() |> 
#   make_long(logradouro, faixa_azul, value = tamanho) |> 
#   ggplot(aes(x = x, 
#              next_x = next_x, 
#              node = factor(node), 
#              next_node = factor(next_node),
#              fill = node,
#              value = value,
#              label = node)) +
#   geom_sankey(flow.alpha = 1, na.rm = T, space = 0, colour = "black", lwd = .1) +
#   # geom_sankey_label(size = 3.5, color = "black", fill = "white", alpha = .9) +
#   theme_sankey() +
#   theme(legend.position = "none")
# 
# 
# pivot_wider(names_from = faixa_azul, values_from = tamanho, values_fill = 0)
#   
logradouros |>
  semi_join(faixa_azul) |> 
  left_join(read_csv("dados_tratados/faixa_azul_selecao.csv") |> 
              mutate(faixa_azul = "faixa_azul", id_osm = as.character(id_osm))) |> 
  mutate(tamanho = st_length(geom), faixa_azul = replace_na(faixa_azul, "sem_faixa")) |>
  st_drop_geometry() |> 
  group_by(logradouro, faixa_azul) |> 
  summarize(tamanho = tamanho |> sum()) |> 
  group_by(logradouro) |> 
  mutate(order = sum(tamanho), 
         faixa_azul = factor(faixa_azul, 
                             levels = c("sem_faixa", "faixa_azul"),
                             labels = c("Trecho comum", "Trecho de faixa azul"))) |> 
  ggplot(aes(x = tamanho, y = reorder(logradouro, order), fill = faixa_azul)) +
  geom_col(colour = "black", lwd = .1) +
  scale_x_units("Tamanho da via", unit = "km") +
  labs(y = NULL) +
  scale_fill_manual("", values = c("Trecho comum" = "#A6A6A6", "Trecho de faixa azul" = "#4472C4")) +
  theme_minimal() +
  theme(legend.position = "inside", legend.position.inside = c(.7,.2))

ggsave("output/tamanho-trechos.pdf", width = 7, height = 6)


data.frame(x = "G", y = c("A", "B"), z = 1:10) |> 
  ggplot(aes(x = z, y = x, fill = y, group = z)) +
  geom_col(position = "stack")














