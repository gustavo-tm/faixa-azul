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
  
  data.table::fread("dados_brutos/acidentes_fatais.csv", encoding = "Latin-1") |> 
    filter(Município == "SAO PAULO") |> 
    select(data = "Data do Acidente",
           logradouro = "Logradouro",
           veiculo = "Tipo do veículo da vítima",
           latitude = "LAT_(GEO)",
           longitude = "LONG_(GEO)") |> 
    mutate(across(c(longitude, latitude), ~ str_replace(.x, ",", "."))) |> 
    write_csv("dados_tratados/obitos.csv")
  
  
  df <- data.table::fread("dados_brutos/acidentes_naofatais.csv", encoding = "Latin-1") |>
    rename(dia = "Dia do Acidente",
           nm_municipio = "Município",
           ano_mes = "Ano/Mês do Acidente",
           horario = "Hora do Acidente",
           data = "Data do Acidente") |> 
    filter(nm_municipio == "SAO PAULO") |>  
    mutate(data = make_datetime(year = year(data), 
                                month = month(data), 
                                day = as.numeric(dia), 
                                hour = as.numeric(str_sub(horario, 1, 2)),
                                min = as.numeric(str_sub(horario, 4, 5))))
  
  df |> 
    select(ID, 
           data,
           logradouro = "Logradouro",
           numero = "Numero/KM",
           latitude = "LAT_(GEO)",
           longitude = "LONG_(GEO)",
           feridos_graves = "Pessoas Envolvidas - Grave",
           feridos_leves = "Pessoas Envolvidas - Leve") |> 
    mutate(numero = ifelse(numero == "NAO DISPONIVEL", NA, numero),
           across(c(longitude, latitude), ~ str_replace(.x, ",", "."))) |> 
    write_csv("dados_tratados/acidentes.csv")
  
  df |> 
    rename(motocicletas = "Veículos Envolvidos - Motocicleta",
           feridos_grave = "Pessoas Envolvidas - Grave",
           feridos_leve = "Pessoas Envolvidas - Leve") |> 
    group_by(ano = year(data), mes = month(data), logradouro = Logradouro) |> 
    summarize(acidentes = n(),
              acidentes_feridos = sum(ifelse(feridos_grave > 0 | feridos_leve > 0, 1, 0)),
              acidentes_moto = sum(ifelse(motocicletas > 0, 1, 0)),
              acidentes_moto_feridos = sum(ifelse((feridos_grave > 0 | feridos_leve > 0) & motocicletas > 0, 1, 0))) |> 
    right_join(expand_grid(data = seq.Date(from = df$data |> min() |> as.Date(), 
                                           to = df$data |> max() |> as.Date(), 
                                           by = "month"),
                           logradouro = df |> distinct(Logradouro) |> pull(Logradouro)) |> 
                 mutate(mes = month(data),
                        ano = year(data)) |> 
                 select(-data)) |> 
    mutate(across(everything(), ~ replace_na(.x, 0))) |> 
    arrange(logradouro, desc(ano), desc(mes)) |> 
    write_csv("dados_tratados/logradouros.csv")
}

if (!file.exists("dados_tratados")){arrumar.dados()}





