library(tidyverse)

# Download ----

download.dados <- function(){
  dir.create("dados_brutos/")
  download.file("http://painelderesultados.infosiga.sp.gov.br/bases/acidentes_naofatais.csv",
                "dados_brutos/acidentes_naofatais.csv")
  download.file("http://painelderesultados.infosiga.sp.gov.br/bases/obitos_publico.csv",
                "dados_brutos/acidentes_fatais.csv")
}

if (!file.exists("dados_brutos")){download.dados()}

# Tratamento ----

arrumar.dados <- function(){
  dir.create("dados_tratados/")
  
  df <- data.table::fread("dados_brutos/acidentes_naofatais.csv", encoding = "Latin-1") |>
    rename(dia = "Dia do Acidente",
           nm_municipio = "Município",
           ano_mes = "Ano/Mês do Acidente",
           horario = "Hora do Acidente") |> 
    filter(nm_municipio == "SAO PAULO") |>  
    mutate(data = make_datetime(year = as.numeric(str_sub(ano_mes, 1, 4)), 
                                month = as.numeric(str_sub(ano_mes, 6, 8)), 
                                day = as.numeric(dia), 
                                hour = as.numeric(str_sub(horario, 1, 2)),
                                min = as.numeric(str_sub(horario, 4, 5))))
  
  df |> 
    select(ID, 
           data,
           logradouro = "Logradouro",
           latitude = "LAT_(GEO)",
           longitude = "LONG_(GEO)",
           feridos_graves = "Pessoas Envolvidas - Grave",
           feridos_leves = "Pessoas Envolvidas - Leve") |> 
    write_csv("dados_tratados/acidentes.csv")
  
  df |> 
    distinct(ano = year(data), mes = month(data)) |>
    mutate(join = 1) |> 
    left_join(df |> 
                distinct(logradouro = Logradouro) |> 
                mutate(join = 1)) |> 
    select(-join) |> 
    left_join(df |> 
                group_by(ano = year(data), mes = month(data), logradouro = Logradouro) |> 
                summarize(acidentes = n())) |> 
    mutate(acidentes = acidentes |> replace_na(0)) |> 
    write_csv("dados_tratados/logradouros.csv")
}

if (!file.exists("dados_tratados")){arrumar.dados()}




  
