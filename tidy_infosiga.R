library(tidyverse)

df <- bind_rows(
  data.table::fread("dados_brutos/sinistros_fatais.csv", encoding = "Latin-1") |>
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
           numero = numero |> str_replace(",", ".")),
  data.table::fread("dados_brutos/sinistros_nao_fatais.csv", encoding = "Latin-1") |> 
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
    mutate(hora = str_sub(hora, 1, 2) |> as.numeric())) |>
  mutate(across(c(longitude, latitude), ~ str_replace(.x, ",", ".")),
         data = lubridate::make_datetime(year = ano, month = mes, day = dia, hour = as.numeric(hora)),
         logradouro = logradouro |> 
           stringi::stri_trans_general("latin-ascii") |> 
           str_to_upper() |> 
           str_replace_all("[[:punct:]]", ""),
         id_acidente = row_number()) |> 
  select(id_acidente, data, logradouro, latitude, longitude, tipo, quantidade_envolvidos = quantidade, indicacao_motocicleta = motocicleta)

df |> write_csv("banco_dados/sinistros.csv")

# 
# df |> write_csv("dados_tratados/infosiga_sinistros.csv")
# 
# df |> 
#   group_by(ano = year(data), mes = month(data), logradouro) |> 
#   summarize(sinistros = n(),
#             sinistros_moto = sum(ifelse(motocicleta > 0, 1, 0)),
#             .groups = "drop") |> 
#   filter(ano > 2018) |> 
#   complete(ano, mes, logradouro, 
#            fill = list(sinistros = 0, sinistros_moto = 0)) |> 
#   arrange(logradouro, desc(ano), desc(mes)) |> 
#   write_csv("dados_tratados/infosiga_logradouros.csv")
# 
