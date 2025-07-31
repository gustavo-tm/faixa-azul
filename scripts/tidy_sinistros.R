library(tidyverse)
library(gt)

tidy_sinistros <- function(){
    sinistros <- bind_rows(
    data.table::fread("dados_brutos/sinistros_2015-2021.csv", encoding = "Latin-1"),
    data.table::fread("dados_brutos/sinistros_2022-2025.csv", encoding = "Latin-1")) |> 
    as_tibble() |> 
    filter(municipio == "SAO PAULO") |> 
    filter(!if_all(c(tipo_via, administracao, conservacao, jurisdicao), ~ . %in% c("NAO DISPONIVEL", ""))) |>
    select(id_infosiga = id_sinistro,
           ano = ano_sinistro,
           mes = mes_sinistro,
           dia = dia_sinistro,
           hora = hora_sinistro,
           latitude, longitude,
           logradouro, numero = numero_logradouro,
           contains("gravidade"),
           contains("tp_veiculo"),
           tipo_acidente = tipo_acidente_primario,
           tipo = tipo_registro) |> 
    mutate(hora = str_sub(hora, 1, 2) |> as.numeric())
  
  sinistros <- sinistros |>
    mutate(across(c(longitude, latitude), ~ as.numeric(str_replace(.x, ",", "."))),
           data = lubridate::make_date(year = ano, month = mes, day = dia),
           id_sinistro = row_number()) |> 
    select(id_sinistro, id_infosiga, data, hora, logradouro, numero, latitude, longitude, tipo, tipo_acidente,
           contains("tp_veiculo"), contains("gravidade"))
  
  return(sinistros)
}

tidy_vitimas <- function(){
  bind_rows(data.table::fread("dados_brutos/pessoas_2022-2025.csv", encoding = "Latin-1"),
            data.table::fread("dados_brutos/pessoas_2015-2021.csv", encoding = "Latin-1")) |> 
    as_tibble() |> 
    select(id_infosiga = id_sinistro, 4:12)
}