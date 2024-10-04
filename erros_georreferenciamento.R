library(tidyverse)
library(osmdata)
mapview::mapview()
acidentes <- read_csv("dados_tratados/acidentes.csv")

acidentes |> 
  group_by(ano = year(data)) |> 
  summarize(percent_na = mean(is.na(latitude)))

acidentes |> 
  group_by(ano = year(data), 
           numero = ifelse(is.na(numero), "Sem Número", "Com número")) |> 
  summarize(percent_na = mean(is.na(latitude))) |> 
  pivot_wider(id_cols = ano, names_from = numero, values_from = percent_na)

acidentes |> 
  group_by(ano = year(data), 
           feridos = ifelse(feridos_graves > 0 | feridos_leves > 0, "Com feridos", "Apenas Ilesos")) |> 
  summarize(percent_na = mean(is.na(latitude))) |> 
  pivot_wider(id_cols = ano, names_from = feridos, values_from = percent_na)

acidentes |> 
  group_by(ano = year(data), 
           tp_via = ifelse(startsWith(logradouro, "SP") |  startsWith(logradouro, "BR"), "Estrada", "Via")) |> 
  summarize(percent_na = mean(is.na(latitude))) |> 
  pivot_wider(id_cols = ano, names_from = tp_via, values_from = percent_na)

acidentes |> 
  group_by(feridos = ifelse(feridos_graves > 0 | feridos_leves > 0, "Com feridos", "Apenas Ilesos"), 
           numero = ifelse(is.na(numero), "Sem Número", "Com número"),
           tp_via = ifelse(startsWith(logradouro, "SP") |  startsWith(logradouro, "BR"), "Estrada", "Via")) |> 
  summarize(percent_na = mean(is.na(latitude)))
  # pivot_wider(id_cols = c(numero, tp_via), names_from = feridos, values_from = percent_na)

