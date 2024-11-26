library(tidyverse)
library(basedosdados)

# RENAEST ----

localidades <- read_delim("dados_brutos/renaest/localidades.csv", delim = ";")
acidentes <- read_delim("dados_brutos/renaest/acidentes.csv", delim = ";")
vitimas <- read_delim("dados_brutos/renaest/vitimas.csv", delim = ";")
veiculos <- read_delim("dados_brutos/renaest/veiculos.csv", delim = ";")

acidentes |> 
  filter(codigo_ibge == "3550308", qtde_obitos > 0) |> 
  mutate(na = is.na(latitude_acidente)) |> 
  filter(na, ) |>
  summarize(nas = sum(na), total = n())


veiculos |> 
  semi_join(acidentes |> 
              filter(codigo_ibge == "3550308", qtde_obitos > 0)) |> 
  distinct(num_acidente)

vitimas |> 
  semi_join(acidentes |> 
              filter(codigo_ibge == "3550308", qtde_obitos > 0)) |> 
  distinct(num_acidente)



localidades
localidades.frota <- frota |>
  filter(ano == 2023, mes == 12) |> 
  filter(tipo_veiculo %in% c("motocicleta", "ciclomotor", "motoneta")) |> 
  group_by(ano_referencia = ano, mes_referencia = mes, codigo_ibge = id_municipio) |> 
  summarize(motocicletas = sum(quantidade)) |> 
  mutate(ano_referencia = ano_referencia |> as.numeric(),
         mes_referencia = mes_referencia |> as.character(),
         codigo_ibge = codigo_ibge |> as.numeric()) |> 
  left_join(localidades)
  
localidades.frota |> 
  mutate(motocicleta_percap = motocicletas / qtde_habitantes) |> 
  arrange(-motocicleta_percap) |> 
  mutate(rn = row_number()) |> 
  arrange(-motocicletas) |> 
  View()

localidades |> 
  filter(ano_referencia == 2023, mes_referencia == 12) |> 
  mutate(codigo_ibge = codigo_ibge |> as.character()) |> 
  select(codigo_ibge, municipio, uf, frota_circulante) |> 
  left_join(acidentes |> 
              group_by(codigo_ibge) |> 
              summarize(obitos = sum(qtde_obitos))) |> 
  mutate(obitos = replace_na(obitos, 0)) |>
  mutate(label = ifelse(codigo_ibge == "3550308", uf, "")) |> 
  filter(obitos > 0) |> 
  ggplot(aes(x = frota_circulante, y = obitos)) +
  geom_point(aes(colour = label)) +
  # geom_smooth(method = "lm", se = F) +
  scale_y_log10() +
  scale_x_log10()
  
geometrias <- geobr::read_municipality(code_muni = "all") 

gg <- localidades |> 
  filter(ano_referencia == 2023, mes_referencia == 12) |> 
  mutate(codigo_ibge = codigo_ibge |> as.character()) |> 
  select(codigo_ibge, municipio, uf, frota_circulante, qtde_habitantes) |> 
  left_join(acidentes |> 
              filter(ano_acidente == 2023) |> 
              group_by(codigo_ibge) |> 
              summarize(obitos = sum(qtde_obitos))) |> 
  mutate(obitos = replace_na(obitos, 0),
         obitos_percap = cut(10000 * obitos / qtde_habitantes, breaks = c(0, .5, 1, 2, 4, 10, Inf))) |> 
  left_join(geometrias |> 
              mutate(code_muni = code_muni |> as.character()) |> 
              select(codigo_ibge = code_muni, geom)) |> 
  ggplot() +
  geom_sf(aes(geometry = geom, fill = obitos_percap), colour = NA) +
  scale_fill_viridis_d(na.value = "#F2F2F2") +
  theme_void()

ggsave("output/obitos-percap-brasil.pdf", gg, width = 20, height = 20)

# Frota ----

readRenviron(".env")
billing.id <- Sys.getenv("BILLING_ID")
basedosdados::set_billing_id(billing.id)

query <- "
SELECT
    dados.ano as ano,
    dados.mes as mes,
    dados.sigla_uf as sigla_uf,
    dados.id_municipio AS id_municipio,
    diretorio_id_municipio.nome AS id_municipio_nome,
    dados.tipo_veiculo as tipo_veiculo,
    dados.quantidade as quantidade
FROM `basedosdados.br_denatran_frota.municipio_tipo` AS dados
LEFT JOIN (SELECT DISTINCT id_municipio,nome  FROM `basedosdados.br_bd_diretorios_brasil.municipio`) AS diretorio_id_municipio
    ON dados.id_municipio = diretorio_id_municipio.id_municipio
"

frota <- read_sql(query, billing_project_id = get_billing_id())

frota |> 
  write_csv("dados_brutos/frota.csv")

frota |> 
  filter(id_municipio == "3550308") |> 
  select(ano, mes, tipo_veiculo, frota = quantidade) |> 
  write_csv("dados_tratados/frota.csv")

