tidy_sinistros <- function(){
  
  tmp_dir <- tempdir()
  unzip("dados_brutos/sinistros.zip", files = c("sinistros_2015-2021.csv", "sinistros_2022-2025.csv"), exdir = tmp_dir)
  
  sinistros <- bind_rows(
    data.table::fread(file.path(tmp_dir, "sinistros_2015-2021.csv"), encoding = "Latin-1"),
    data.table::fread(file.path(tmp_dir, "sinistros_2022-2025.csv"), encoding = "Latin-1")) |> 
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
  
  file.remove(file.path(tmp_dir, c("sinistros_2015-2021.csv", "sinistros_2022-2025.csv")))
  
  return(sinistros)
}

tidy_vitimas <- function(){
  bind_rows(data.table::fread(unzip("dados_brutos/pessoas.zip", "pessoas_2022-2025.csv"),encoding = "Latin-1"),
            data.table::fread(unzip("dados_brutos/pessoas.zip", "pessoas_2015-2021.csv"),encoding = "Latin-1")) |> 
    as_tibble() |> 
    select(id_infosiga = id_sinistro, 4:12)
}

calcular_mortes <- function(vitimas){
  vitimas |> 
    filter(gravidade_lesao == "FATAL") |> 
    mutate(veiculo = fct_collapse(str_to_upper(tipo_veiculo_vitima),
                                  motocicleta = "MOTOCICLETA",
                                  pedestre_bike = c("PEDESTRE",  "BICICLETA"),
                                  other_level = "outros")) |> 
    group_by(id_infosiga, veiculo) |> 
    summarize(mortes = n()) |> 
    pivot_wider(id_cols = id_infosiga, names_from = veiculo, values_from = mortes, 
                values_fill = 0, names_prefix = "mortes_")
}