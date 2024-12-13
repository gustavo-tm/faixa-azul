library(tidyverse)
library(gt)

sinistro.fatais <- data.table::fread("dados_brutos/sinistros_fatais.csv", encoding = "Latin-1") |>
  filter(Município == "SAO PAULO") |> 
  select(ano = "Ano do Sinistro",
         mes = "Mês do Sinistro",
         dia = "Dia do Sinistro",
         hora = "Hora do Sinistro",
         latitude, longitude,
         logradouro = Logradouro,
         numero = "Númeral / KM",
         motocicleta = "Motocicleta envolvida",
         quantidade = "Quantidade de vítimas fatais") |>
  mutate(tipo = "SINISTRO FATAL",
         numero = numero |> str_replace(",", "."))

sinistros.naofatais <- bind_rows(
  data.table::fread("dados_brutos/sinistros_nao_fatais_2019-2020.csv", encoding = "Latin-1"),
  data.table::fread("dados_brutos/sinistros_nao_fatais_2021-2024.csv", encoding = "Latin-1")) |> 
  as_tibble() |> 
  filter(Município == "SAO PAULO") |> 
  select(ano = "Ano do Sinistro",
         mes = "Mês do Sinistro",
         dia = "Dia do Sinistro",
         hora = "Hora do Sinistro",
         latitude, longitude,
         logradouro = Logradouro,
         numero = "Numero/KM",
         motocicleta = "Motocicleta envolvida",
         tipo = "Tipo de registro") |> 
  mutate(hora = str_sub(hora, 1, 2) |> as.numeric())


df <- bind_rows(sinistro.fatais, sinistro.naofatais) |>
  mutate(across(c(longitude, latitude), ~ str_replace(.x, ",", ".")),
         data = lubridate::make_datetime(year = ano, month = mes, day = dia, hour = as.numeric(hora)),
         id_sinistro = row_number()) |> 
  select(id_sinistro, data, logradouro, numero, latitude, longitude, tipo, quantidade_envolvidos = quantidade, motocicletas = motocicleta)

df |> write_csv("banco_dados/sinistros.csv")

df |> 
  as_tibble() |> 
  group_by(ano = year(data), tipo) |> 
  summarize(n = n(), .groups = "drop") |> 
  filter(ano >= 2019) |> 
  pivot_wider(names_from = tipo, values_from = n, id_cols = ano) |> 
  gt() |> 
  cols_move(columns = "NOTIFICACAO",
            after = "SINISTRO NAO FATAL") |> 
  cols_label(ano = "Ano",
             "SINISTRO FATAL" = "Sinistros fatais",
             "NOTIFICACAO" = "Notificações",
             "SINISTRO NAO FATAL" = "Sinistros não fatais") |> 
  fmt_number(columns = 2:4, decimals = 0, sep_mark = ".") |> 
  as_latex() |> cat()
 
