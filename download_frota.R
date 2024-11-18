library(tidyverse)
library(basedosdados)

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
  filter(id_municipio == "3550308") |> 
  select(ano, mes, tipo_veiculo, frota = quantidade) |> 
  write_csv("dados_tratados/frota.csv")
