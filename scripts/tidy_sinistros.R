library(tidyverse)
library(gt)

tidy_sinistros <- function(){
  # sinistro.fatais <- data.table::fread("dados_brutos/sinistros_fatais.csv", encoding = "Latin-1") |>
  #   filter(Município == "SAO PAULO") |> 
  #   select(ano = "Ano do Sinistro",
  #          mes = "Mês do Sinistro",
  #          dia = "Dia do Sinistro",
  #          hora = "Hora do Sinistro",
  #          latitude, longitude,
  #          logradouro = Logradouro,
  #          numero = "Númeral / KM",
  #          motocicleta = "Motocicleta envolvida",
  #          quantidade = "Quantidade de vítimas fatais") |>
  #   mutate(tipo = "SINISTRO FATAL",
  #          numero = numero |> str_replace(",", "."))
  
  sinistros <- bind_rows(
    data.table::fread("dados_brutos/sinistros_2015-2021.csv", encoding = "Latin-1"),
    data.table::fread("dados_brutos/sinistros_2022-2025.csv", encoding = "Latin-1")) |> 
    as_tibble() |> 
    filter(municipio == "SAO PAULO") |> 
    select(id_infosiga = id_sinistro,
           ano = ano_sinistro,
           mes = mes_sinistro,
           dia = dia_sinistro,
           hora = hora_sinistro,
           latitude, longitude,
           logradouro, numero = numero_logradouro,
           motocicletas = tp_veiculo_motocicleta, 
           gravidade_ileso, gravidade_leve, gravidade_grave, gravidade_fatal, gravidade_nao_disponivel,
           tipo_acidente = tipo_acidente_primario,
           tipo = tipo_registro) |> 
    mutate(hora = str_sub(hora, 1, 2) |> as.numeric())
  
  df <- sinistros |>
    mutate(across(c(longitude, latitude), ~ as.numeric(str_replace(.x, ",", "."))),
           data = lubridate::make_date(year = ano, month = mes, day = dia),
           id_sinistro = row_number()) |> 
    select(id_sinistro, id_infosiga, data, hora, logradouro, numero, latitude, longitude, tipo, tipo_acidente,
           motocicletas, contains("gravidade"))
  
  return(df)
  
  # df |> write_csv("banco_dados/sinistros.csv")
  
  # df |> 
  #   as_tibble() |> 
  #   group_by(ano = year(data), tipo) |> 
  #   summarize(n = n(), .groups = "drop") |> 
  #   filter(ano >= 2019) |> 
  #   pivot_wider(names_from = tipo, values_from = n, id_cols = ano) |> 
  #   gt() |> 
  #   cols_move(columns = "NOTIFICACAO",
  #             after = "SINISTRO NAO FATAL") |> 
  #   cols_label(ano = "Ano",
  #              "SINISTRO FATAL" = "Sinistros fatais",
  #              "NOTIFICACAO" = "Notificações",
  #              "SINISTRO NAO FATAL" = "Sinistros não fatais") |> 
  #   fmt_number(columns = 2:4, decimals = 0, sep_mark = ".") |> 
  #   as_latex() |> cat()
}

