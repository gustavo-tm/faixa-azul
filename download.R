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
  data.table::fread("dados_brutos/acidentes_naofatais.csv", encoding = "Latin-1") |>
    rename(dia = "Dia do Acidente",
           nm_municipio = "Município",
           ano_mes = "Ano/Mês do Acidente",
           horario = "Hora do Acidente") |> 
    filter(nm_municipio == "SAO PAULO") |>  
    mutate(data = make_datetime(year = as.numeric(str_sub(ano_mes, 1, 4)), 
                                month = as.numeric(str_sub(ano_mes, 6, 8)), 
                                day = as.numeric(dia), 
                                hour = as.numeric(str_sub(horario, 1, 2)),
                                min = as.numeric(str_sub(horario, 4, 5)))) |> 
    select(ID, 
           data,
           logradouro = "Logradouro",
           latitude = "LAT_(GEO)",
           longitude = "LONG_(GEO)",
           feridos_graves = "Pessoas Envolvidas - Grave",
           feridos_leves = "Pessoas Envolvidas - Leve") |> 
    write_csv("dados_tratados/acidentes.csv")
}

if (!file.exists("dados_tratados")){arrumar.dados()}


