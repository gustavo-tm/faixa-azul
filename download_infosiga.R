library(tidyverse)

a |>
  as_tibble() |> 
  mutate(teste = numero |> str_replace(",", ".") |> as.numeric()) |> 
  filter(teste |> is.na())

df <- bind_rows(
  data.table::fread("dados_brutos/sinistros_fatais.csv", encoding = "Latin-1") |>
    filter(Município == "SAO PAULO") |> 
    select(data = "Data do Sinistro",
           latitude, longitude,
           logradouro = Logradouro,
           numero = "Númeral / KM",
           motocicleta = "Motocicleta envolvida") |>
    mutate(tipo = "SINISTRO FATAL",
           numero = numero |> str_replace(",", ".")),
  data.table::fread("dados_brutos/sinistros_nao_fatais.csv", encoding = "Latin-1") |> 
    as_tibble() |> 
    filter(Município == "SAO PAULO") |> 
    select(data = "Data do Sinistro",
           latitude, longitude,
           logradouro = Logradouro,
           numero = "Numero/KM",
           motocicleta = "Motocicleta envolvida",
           tipo = "Tipo de registro")) |> 
  mutate(across(c(longitude, latitude), ~ str_replace(.x, ",", ".")),
         data = lubridate::as_date(data, format = "%d/%m/%Y"),
         logradouro = logradouro |> 
           stringi::stri_trans_general("latin-ascii") |> 
           str_to_upper() |> 
           str_replace_all("[[:punct:]]", ""))

df |> write_csv("dados_tratados/sinistros.csv")

df |> 
  group_by(ano = year(data), mes = month(data), logradouro) |> 
  summarize(sinistros = n(),
            sinistros_moto = sum(ifelse(motocicleta > 0, 1, 0))) |> 
  right_join(expand_grid(data = seq.Date(from = as.Date("2019-01-01"), 
                                         to = df$data |> max() |> as.Date(), 
                                         by = "month"),
                         logradouro = df |> distinct(logradouro) |> pull(logradouro)) |> 
               mutate(mes = month(data),
                      ano = year(data)) |> 
               select(-data)) |> 
  mutate(across(everything(), ~ replace_na(.x, 0))) |> 
  arrange(logradouro, desc(ano), desc(mes)) |> 
  write_csv("dados_tratados/sinistros_logradouros.csv")


