library(targets)
library(visNetwork)
# library(tarchetypes) # Load other packages as needed.

workers <- 4

# para garantir que osmdata vai funcionar
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

# PARA DEBUGAR
# tar_meta(fields = error, complete_only = TRUE) |> View()
# tar_progress()
# tar_visnetwork()
# tar_delete()

tar_option_set(
  packages = c("tidyverse", "sf", "osmdata", "fuzzyjoin", "stringdist", "did", "gt", "circlize", "igraph", "gganimate"), 
  error = "trim",
  # format = "qs", # Optionally set the default storage format. qs is fast.

  # controller = crew::crew_controller_local(workers = workers)
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source("scripts/tidy_sinistros.R")
tar_source("scripts/tidy_trechos.R")
tar_source("scripts/trechos_complemento.R")
tar_source("scripts/tidy_faixa_azul.R")
tar_source("scripts/match.R")
tar_source("scripts/descritivas.R")
tar_source("scripts/did.R")
tar_source("scripts/did_het.R")


list(
  # 1. SINISTROS ----
  tar_target(
    name = dado_sinistros,
    command = tidy_sinistros()),
  
  # 2. TRECHOS ----
  tar_target(
    name = dado_osm,
    command = download_osm()),
  tar_target(
    name = dado_trechos,
    command = tidy_trechos(dado_osm)),
  tar_target(
    name = dado_radar,
    command = calcular_radares(dado_trechos)),
  tar_target(
    name = dado_amenidades,
    command = calcular_amenidades(dado_trechos)),
  tar_target(
    name = dado_interseccao,
    command = calcular_interseccao(dado_trechos, dado_token_osm)),
  tar_target(
    name = dado_trechos_complemento,
    command = tidy_complemento_trecho(dado_trechos, dado_radar, dado_interseccao, dado_amenidades)),
  
  # 3. FAIXA AZUL ----
  tar_target(
    name = dado_faixa_azul,
    command = tidy_faixa_azul(dado_trechos)),
  tar_target(
    name = dado_vizinhos_150,
    command = tidy_vizinhos(dado_trechos, dado_id_logradouros, dado_faixa_azul, 150)),
  tar_target(
    name = dado_vizinhos_500,
    command = tidy_vizinhos(dado_trechos, dado_id_logradouros, dado_faixa_azul, 500)),
  
  # 4. LOGRADOUROS ----
  tar_target(
    name = dado_id_logradouros,
    command = agrupar_logradouros(dado_trechos, dado_token_osm)),
  tar_target(
    name = dado_logradouros,
    command = tidy_logradouros(dado_id_logradouros, dado_trechos, dado_trechos_complemento, dado_faixa_azul)),
  
  
  # 5. MATCH ---- 
  tar_target(
    name = dado_token_infosiga,
    command = tokenizar_infosiga(dado_sinistros)),
  tar_target(
    name = dado_token_osm,
    command = tokenizar_osm(dado_trechos)),
  
  tar_target(
    name = dado_sinistros_chunks,
    command = match_dados_split(dado_sinistros, n = workers)),

  tar_target(
    name = dado_match_chunks,
    command = match_dados(dado_sinistros_chunks, 
                          sinistros_token = dado_token_infosiga, 
                          trechos = dado_trechos, 
                          trechos_token = dado_token_osm),
    pattern = NULL,
    iteration = "group"),
  
  tar_target(
    name = dado_match,
    command = bind_rows(dado_match_chunks)),
  
  # 6. DESCRITIVAS ----
  tar_target(
    name = descritiva_datas_FA,
    command = plot_datas_FA(dado_logradouros, dado_id_logradouros, dado_match, dado_sinistros)),
  tar_target(
    name = descritiva_datas_trechos,
    command = plot_datas_trechos(dado_faixa_azul, dado_trechos, dado_token_osm)),
  tar_target(
    name = descritiva_obitos_tempo,
    command = plot_obitos_tempo(dado_sinistros, dado_match, dado_faixa_azul, dado_logradouros, dado_id_logradouros)),
  tar_target(
    name = descritiva_tamanho_FA,
    command = plot_tamanho_FA(dado_id_logradouros, dado_logradouros, dado_faixa_azul, dado_trechos)),
  tar_target(
    name = descritiva_hora_sinistro,
    command = plot_hora_sinistro(dado_sinistros)),
  tar_target(
    name = descritiva_qualidade_match,
    command = plot_qualidade_match(dado_sinistros, dado_match)),
  tar_target(
    name = descritiva_mapas,
    command = plot_mapas(dado_sinistros, dado_trechos, dado_faixa_azul)),
  tar_target(
    name = descritiva_obitos_ano,
    command = plot_obitos_ano(dado_sinistros)),
  tar_target(
    name = descritiva_proporcao_grupos,
    command = plot_proporcao_grupos(dado_trechos, dado_faixa_azul)),
  tar_target(
    name = descritiva_tratados_periodo,
    command = plot_trechos_vias_periodo(dado_faixa_azul, dado_logradouros)),
  tar_target(
    name = descritiva_comprimento_trechos,
    command = plot_comprimento_trechos(dado_trechos)),
  tar_target(
    name = descritiva_sinistros_comprimento,
    command = plot_sinistros_comprimento(dado_did_trecho_golden)),
  
  
  
  # 7. DID ----
  # 7.1 agrega
  tar_target(
    name = dado_did_trecho_mes,
    command = dado_trecho_mes(dado_sinistros, dado_match, dado_trechos)),

  tar_target(
    name = dado_did_logradouro_mes,
    command = dado_logradouro_mes(dado_sinistros, dado_match, dado_id_logradouros)),

  # 7.2 prepara
  # 7.2.1. trecho
  tar_target(
    name = dado_did_trecho,
    command = prepara_trecho_did(dado_did_trecho_mes, dado_trechos, dado_trechos_complemento, dado_faixa_azul,
                                 filtrar_por = NULL)),
  tar_target(
    name = dado_did_trecho_golden,
    command = prepara_trecho_did(dado_did_trecho_mes, dado_trechos, dado_trechos_complemento, dado_faixa_azul,
                                 filtrar_por = golden_match)),
  # 7.2.2. logradouro
  tar_target(
    name = dado_did_logradouro,
    command = prepara_logradouro_did(dado_did_logradouro_mes, dado_logradouros,
                                     filtrar_por = NULL)),
  tar_target(
    name = dado_did_logradouro_golden,
    command = prepara_logradouro_did(dado_did_logradouro_mes, dado_logradouros,
                                     filtrar_por = golden_match)),
  # 7.2.3. cohort
  tar_target(
    name = dado_cohort_trecho,
    command = definir_cohort(dado_did_trecho,
                             faixa_azul = dado_faixa_azul,
                             trechos = dado_trechos,
                             logradouros = dado_logradouros,
                             por_logradouro = FALSE)),
  tar_target(
    name = dado_cohort_logradouro,
    command = definir_cohort(dado_did_logradouro,
                             logradouros = dado_logradouros,
                             por_logradouro = TRUE)),

  # 7.3    roda por trecho ----
  
  tar_target(
    name = did_todos_total,
    command = fit_did(
      df = dado_did_trecho,
      cohorts = dado_cohort_trecho,
      yname = "sinistros",
      titulo = "Todos os sinistros",
      filename = "trecho/todos",
      por_km = FALSE)),
  tar_target(
    name = did_todos_km,
    command = fit_did(
      df = dado_did_trecho,
      cohorts = dado_cohort_trecho,
      yname = "sinistros",
      titulo = "Todos os sinistros",
      filename = "trecho/todos",
      por_km = TRUE)),
  
  tar_target(
    name = did_moto_total,
    command = fit_did(
      df = dado_did_trecho,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_veiculo_motocicleta",
      titulo = "Sinistros de moto",
      filename = "trecho/moto",
      por_km = FALSE)),
  tar_target(
    name = did_moto_km,
    command = fit_did(
      df = dado_did_trecho,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_veiculo_motocicleta",
      titulo = "Sinistros de moto",
      filename = "trecho/moto",
      por_km = TRUE)),
  
  
  tar_target(
    name = did_todos_golden_total,
    command = fit_did(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros",
      titulo = "Todos os sinistros, golden",
      filename = "trecho/todos-golden",
      por_km = FALSE)),
  tar_target(
    name = did_todos_golden_km,
    command = fit_did(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros",
      titulo = "Todos os sinistros, golden",
      filename = "trecho/todos-golden",
      por_km = TRUE)),
  
  tar_target(
    name = did_moto_golden_total,
    command = fit_did(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_veiculo_motocicleta",
      titulo = "Sinistros de moto, golden",
      filename = "trecho/moto-golden",
      por_km = FALSE)),
  tar_target(
    name = did_moto_golden_km,
    command = fit_did(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_veiculo_motocicleta",
      titulo = "Sinistros de moto, golden",
      filename = "trecho/moto-golden",
      por_km = TRUE)),
  
  # 7.3.01 Gravidade ----
  tar_target(
    name = did_leve_todos_golden,
    command = fit_did_all(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_gravidade_leve",
      titulo = "Todos os sinistros, leve, golden",
      filename = "trecho/het/gravidade/leve-todos-golden")),
  tar_target(
    name = did_leve_moto_golden,
    command = fit_did_all(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_gravidade_leve_moto",
      titulo = "Sinistros de moto, leve, golden",
      filename = "trecho/het/gravidade/leve-moto-golden")),
  
  tar_target(
    name = did_grave_todos_golden,
    command = fit_did_all(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_gravidade_grave",
      titulo = "Todos os sinistros, grave, golden",
      filename = "trecho/het/gravidade/grave-todos-golden")),
  tar_target(
    name = did_grave_moto_golden,
    command = fit_did_all(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_gravidade_grave_moto",
      titulo = "Sinistros de moto, grave, golden",
      filename = "trecho/het/gravidade/grave-moto-golden")),
  
  tar_target(
    name = did_fatal_todos_golden,
    command = fit_did_all(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_gravidade_fatal",
      titulo = "Todos os sinistros, fatal, golden",
      filename = "trecho/het/gravidade/fatal-todos-golden")),
  tar_target(
    name = did_fatal_moto_golden,
    command = fit_did_all(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_gravidade_fatal_moto",
      titulo = "Sinistros de moto, fatal, golden",
      filename = "trecho/het/gravidade/fatal-moto-golden")),
  
  # 7.3.02 Tipo de acidente ----
  tar_target(
    name = did_choque_todos_golden,
    command = fit_did_all(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "acidente_choque",
      titulo = "Sinistros com choques, golden",
      filename = "trecho/het/acidente/choque-todos-golden")),
  tar_target(
    name = did_choque_apenas_moto_golden,
    command = fit_did_all(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_choque_apenas_moto",
      titulo = "Sinistros com choques, apenas motos, golden",
      filename = "trecho/het/acidente/choque-apenas-moto-golden")),
  tar_target(
    name = did_choque_moto_carro_golden,
    command = fit_did_all(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_choque_moto_carro",
      titulo = "Sinistros com choques, motos e carros, golden",
      filename = "trecho/het/acidente/choque-moto-carro-golden")),
  tar_target(
    name = did_choque_moto_outros_golden,
    command = fit_did_all(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_choque_moto_outros",
      titulo = "Sinistros com choques, motos e outros, golden",
      filename = "trecho/het/acidente/choque-moto-outros-golden")),
  
  tar_target(
    name = did_colisao_todos_golden,
    command = fit_did_all(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "acidente_colisao",
      titulo = "Sinistros com colisões, golden",
      filename = "trecho/het/acidente/colisao-todos-golden")),
  tar_target(
    name = did_colisao_apenas_moto_golden,
    command = fit_did_all(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_colisao_apenas_moto",
      titulo = "Sinistros com colisões, apenas motos, golden",
      filename = "trecho/het/acidente/colisao-apenas-moto-golden")),
  tar_target(
    name = did_colisao_moto_carro_golden,
    command = fit_did_all(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_colisao_moto_carro",
      titulo = "Sinistros com colisões, motos e carros, golden",
      filename = "trecho/het/acidente/colisao-moto-carro-golden")),
  tar_target(
    name = did_colisao_moto_outros_golden,
    command = fit_did_all(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_colisao_moto_outros",
      titulo = "Sinistros com colisões, motos e outros, golden",
      filename = "trecho/het/acidente/colisao-moto-outros-golden")),
  
  tar_target(
    name = did_atropelamento_todos_golden,
    command = fit_did_all(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "acidente_atropelamento",
      titulo = "Sinistros com atropelamentos, golden",
      filename = "trecho/het/acidente/atropelamento-todos-golden")),
  
  tar_target(
    name = did_outros_todos_golden,
    command = fit_did_all(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "acidente_outros",
      titulo = "Sinistros com outros tipos de acidente, golden",
      filename = "trecho/het/acidente/outros-todos-golden")),
  
  # 7.3.03 Tempo tratado ----
  tar_target(
    name = did_12ate_todos_golden,
    command = did_het_tempo_tratado(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros",
      titulo = "Todos os sinistros, até 12m",
      filename = "trecho/het/tempo_tratado/12ate-todos-golden",
      meses = 12,
      mais_que = FALSE)),
  tar_target(
    name = did_12mais_todos_golden,
    command = did_het_tempo_tratado(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros",
      titulo = "Todos os sinistros, mais que 12m",
      filename = "trecho/het/tempo_tratado/12mais-todos-golden",
      meses = 12,
      mais_que = TRUE)),
  
  tar_target(
    name = did_15ate_todos_golden,
    command = did_het_tempo_tratado(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros",
      titulo = "Todos os sinistros, até 15m",
      filename = "trecho/het/tempo_tratado/15ate-todos-golden",
      meses = 15,
      mais_que = FALSE)),
  tar_target(
    name = did_15mais_todos_golden,
    command = did_het_tempo_tratado(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros",
      titulo = "Todos os sinistros, mais que 15m",
      filename = "trecho/het/tempo_tratado/15mais-todos-golden",
      meses = 15,
      mais_que = TRUE)),
  
  
  tar_target(
    name = did_12ate_moto_golden,
    command = did_het_tempo_tratado(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_veiculo_motocicleta",
      titulo = "Sinistros de moto, até 12m",
      filename = "trecho/het/tempo_tratado/12ate-moto-golden",
      meses = 12,
      mais_que = FALSE)),
  tar_target(
    name = did_12mais_moto_golden,
    command = did_het_tempo_tratado(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_veiculo_motocicleta",
      titulo = "Sinistros de moto, mais que 12m",
      filename = "trecho/het/tempo_tratado/12mais-moto-golden",
      meses = 12,
      mais_que = TRUE)),
  
  tar_target(
    name = did_15ate_moto_golden,
    command = did_het_tempo_tratado(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_veiculo_motocicleta",
      titulo = "Sinistros de moto, até 15m",
      filename = "trecho/het/tempo_tratado/15ate-moto-golden",
      meses = 15,
      mais_que = FALSE)),
  tar_target(
    name = did_15mais_moto_golden,
    command = did_het_tempo_tratado(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_veiculo_motocicleta",
      titulo = "Sinistros de moto, mais que 15m",
      filename = "trecho/het/tempo_tratado/15mais-moto-golden",
      meses = 15,
      mais_que = TRUE)),
  
  # 7.3.04 Numero de faixas ----
  tar_target(
    name = did_faixas_2ate_todos_golden,
    command = did_het_num_faixas(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros",
      titulo = "Todos os sinistros, até 2 faixas",
      filename = "trecho/het/num_faixas/2ate-todos-golden",
      num_faixas = 2,
      mais_que = FALSE)),
  tar_target(
    name = did_faixas_2mais_todos_golden,
    command = did_het_num_faixas(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros",
      titulo = "Todos os sinistros, mais que 2 faixas",
      filename = "trecho/het/num_faixas/2mais-todos-golden",
      num_faixas = 2,
      mais_que = TRUE)),
  
  tar_target(
    name = did_faixas_3ate_todos_golden,
    command = did_het_num_faixas(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros",
      titulo = "Todos os sinistros, até 3 faixas",
      filename = "trecho/het/num_faixas/3ate-todos-golden",
      num_faixas = 3,
      mais_que = FALSE)),
  tar_target(
    name = did_faixas_3mais_todos_golden,
    command = did_het_num_faixas(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros",
      titulo = "Todos os sinistros, mais que 3 faixas",
      filename = "trecho/het/num_faixas/3mais-todos-golden",
      num_faixas = 3,
      mais_que = TRUE)),
  
  
  tar_target(
    name = did_faixas_2ate_moto_golden,
    command = did_het_num_faixas(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_veiculo_motocicleta",
      titulo = "Sinistros de moto, até 2 faixas",
      filename = "trecho/het/num_faixas/2ate-moto-golden",
      num_faixas = 2,
      mais_que = FALSE)),
  tar_target(
    name = did_faixas_2mais_moto_golden,
    command = did_het_num_faixas(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_veiculo_motocicleta",
      titulo = "Sinistros de moto, mais que 2 faixas",
      filename = "trecho/het/num_faixas/2mais-moto-golden",
      num_faixas = 2,
      mais_que = TRUE)),
  
  tar_target(
    name = did_faixas_3ate_moto_golden,
    command = did_het_num_faixas(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_veiculo_motocicleta",
      titulo = "Sinistros de moto, até 3 faixas",
      filename = "trecho/het/num_faixas/3ate-moto-golden",
      num_faixas = 3,
      mais_que = FALSE)),
  tar_target(
    name = did_faixas_3mais_moto_golden,
    command = did_het_num_faixas(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_veiculo_motocicleta",
      titulo = "Sinistros de moto, mais que 3 faixas",
      filename = "trecho/het/num_faixas/3mais-moto-golden",
      num_faixas = 3,
      mais_que = TRUE)),
  
  # 7.3.05 Tipo de via ----
  tar_target(
    name = did_tipovia_primary_todos_golden,
    command = did_het_tipo_via(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros",
      titulo = "Todos os sinistros, primárias",
      filename = "trecho/het/tipo_via/primary-todos-golden",
      tipo_via = "primary")),
  tar_target(
    name = did_tipovia_primary_moto_golden,
    command = did_het_tipo_via(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_veiculo_motocicleta",
      titulo = "Sinistros de moto, primárias",
      filename = "trecho/het/tipo_via/primary-moto-golden",
      tipo_via = "primary")),
  
  tar_target(
    name = did_tipovia_secondary_todos_golden,
    command = did_het_tipo_via(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros",
      titulo = "Todos os sinistros, secundárias",
      filename = "trecho/het/tipo_via/secondary-todos-golden",
      tipo_via = "secondary")),
  tar_target(
    name = did_tipovia_secondary_moto_golden,
    command = did_het_tipo_via(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_veiculo_motocicleta",
      titulo = "Sinistros de moto, secundárias",
      filename = "trecho/het/tipo_via/secondary-moto-golden",
      tipo_via = "secondary")),
  
  tar_target(
    name = did_tipovia_trunk_todos_golden,
    command = did_het_tipo_via(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros",
      titulo = "Todos os sinistros, troncais",
      filename = "trecho/het/tipo_via/trunk-todos-golden",
      tipo_via = "trunk")),
  tar_target(
    name = did_tipovia_trunk_moto_golden,
    command = did_het_tipo_via(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_veiculo_motocicleta",
      titulo = "Sinistros de moto, troncais",
      filename = "trecho/het/tipo_via/trunk-moto-golden",
      tipo_via = "trunk")),
  
  # 7.3.06 Velocidade maxima ----
  tar_target(
    name = did_velmax_40ate_todos_golden,
    command = did_het_vel_maxima(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros",
      titulo = "Todos os sinistros, até 40km/h",
      filename = "trecho/het/vel_maxima/40ate-todos-golden",
      vel_maxima = 40,
      mais_que = FALSE)),
  tar_target(
    name = did_velmax_40mais_todos_golden,
    command = did_het_vel_maxima(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros",
      titulo = "Todos os sinistros, mais que 40km/h",
      filename = "trecho/het/vel_maxima/40mais-todos-golden",
      vel_maxima = 40,
      mais_que = TRUE)),
  
  tar_target(
    name = did_velmax_50ate_todos_golden,
    command = did_het_vel_maxima(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros",
      titulo = "Todos os sinistros, até 50km/h",
      filename = "trecho/het/vel_maxima/50ate-todos-golden",
      vel_maxima = 50,
      mais_que = FALSE)),
  tar_target(
    name = did_velmax_50mais_todos_golden,
    command = did_het_vel_maxima(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros",
      titulo = "Todos os sinistros, mais que 50km/h",
      filename = "trecho/het/vel_maxima/50mais-todos-golden",
      vel_maxima = 50,
      mais_que = TRUE)),
  
  
  tar_target(
    name = did_velmax_40ate_moto_golden,
    command = did_het_vel_maxima(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_veiculo_motocicleta",
      titulo = "Sinistros de moto, até 40km/h",
      filename = "trecho/het/vel_maxima/40ate-moto-golden",
      vel_maxima = 40,
      mais_que = FALSE)),
  tar_target(
    name = did_velmax_40mais_moto_golden,
    command = did_het_vel_maxima(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_veiculo_motocicleta",
      titulo = "Sinistros de moto, mais que 40km/h",
      filename = "trecho/het/vel_maxima/40mais-moto-golden",
      vel_maxima = 40,
      mais_que = TRUE)),
  
  tar_target(
    name = did_velmax_50ate_moto_golden,
    command = did_het_vel_maxima(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_veiculo_motocicleta",
      titulo = "Sinistros de moto, até 50km/h",
      filename = "trecho/het/vel_maxima/50ate-moto-golden",
      vel_maxima = 50,
      mais_que = FALSE)),
  tar_target(
    name = did_velmax_50mais_moto_golden,
    command = did_het_vel_maxima(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_veiculo_motocicleta",
      titulo = "Sinistros de moto, mais que 50km/h",
      filename = "trecho/het/vel_maxima/50mais-moto-golden",
      vel_maxima = 50,
      mais_que = TRUE)),
  
  # 7.3.07 Quantidade de envolvidos ----
  tar_target(
    name = did_qtd_todos_golden,
    command = fit_did_all(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "qtd_envolvidos",
      titulo = "Quantidade de envolvidos, golden",
      filename = "trecho/het/qtd_envolvidos/qtd-todos-golden")),
  tar_target(
    name = did_qtd_veiculo_moto_golden,
    command = fit_did_all(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "veiculo_motocicleta",
      titulo = "Quantidade de motocicletas envolvidas, golden",
      filename = "trecho/het/qtd_envolvidos/qtd-veiculo-moto-golden")),
  tar_target(
    name = did_qtd_veiculo_auto_golden,
    command = fit_did_all(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "veiculo_automovel",
      titulo = "Quantidade de automóveis envolvidos, golden",
      filename = "trecho/het/qtd_envolvidos/qtd-veiculo-auto-golden")),
  
  tar_target(
    name = did_qtd_gravidade_leve_golden,
    command = fit_did_all(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "gravidade_leve",
      titulo = "Quantidade de envolvidos, leve, golden",
      filename = "trecho/het/qtd_envolvidos/qtd-gravidade-leve-golden")),
  tar_target(
    name = did_qtd_gravidade_grave_golden,
    command = fit_did_all(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "gravidade_grave",
      titulo = "Quantidade de envolvidos, grave, golden",
      filename = "trecho/het/qtd_envolvidos/qtd-gravidade-grave-golden")),
  tar_target(
    name = did_qtd_gravidade_fatal_golden,
    command = fit_did_all(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "gravidade_fatal",
      titulo = "Quantidade de envolvidos, fatal, golden",
      filename = "trecho/het/qtd_envolvidos/qtd-gravidade-fatal-golden")),
  
  # 7.3.08 Sem 23 de maio e sem grupo 63 ----
  tar_target(
    name = did_filtro_avs_todos_golden,
    command = did_filtro_avenidas(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros",
      titulo = "Todos os sinistros, golden",
      filename = "trecho/filtro_avenidas/todos-golden",
      sem_av23maio = TRUE, 
      sem_grupo63 = TRUE)),
  tar_target(
    name = did_filtro_avs_moto_golden,
    command = did_filtro_avenidas(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_veiculo_motocicleta",
      titulo = "Sinistros de moto, golden",
      filename = "trecho/filtro_avenidas/moto-golden",
      sem_av23maio = TRUE, 
      sem_grupo63 = TRUE)),
  
  # 7.3.09 Tipo de veículo ----
  # tar_target(
  #   name = did_tp_veiculo_moto_golden,
  #   command = fit_did_all(
  #     df = dado_did_trecho_golden,
  #     cohorts = dado_cohort_trecho,
  #     yname = "veiculo_motocicleta",
  #     titulo = "Motocicletas envolvidas, golden",
  #     filename = "trecho/het/tipo_veiculo/qtd-moto-golden")),
  # tar_target(
  #   name = did_tp_veiculo_auto_golden,
  #   command = fit_did_all(
  #     df = dado_did_trecho_golden,
  #     cohorts = dado_cohort_trecho,
  #     yname = "veiculo_automovel",
  #     titulo = "Automoveis envolvidos, golden",
  #     filename = "trecho/het/tipo_veiculo/qtd-auto-golden")),
  
  # 7.3.10 Horário ----
  tar_target(
    name = did_hora_05_10_todos_golden,
    command = fit_did_all(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_hora_05_10",
      titulo = "Todos os sinsitros, 05h-10h, golden",
      filename = "trecho/het/horario/05_10-todos-golden")),
  tar_target(
    name = did_hora_05_10_moto_golden,
    command = fit_did_all(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_hora_05_10_moto",
      titulo = "Sinistros de moto, 05h-10h, golden",
      filename = "trecho/het/horario/05_10-moto-golden")),
  
  tar_target(
    name = did_hora_11_16_todos_golden,
    command = fit_did_all(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_hora_11_16",
      titulo = "Todos os sinsitros, 11h-16h, golden",
      filename = "trecho/het/horario/11_16-todos-golden")),
  tar_target(
    name = did_hora_11_16_moto_golden,
    command = fit_did_all(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_hora_11_16_moto",
      titulo = "Sinistros de moto, 11h-16h, golden",
      filename = "trecho/het/horario/11_16-moto-golden")),
  
  tar_target(
    name = did_hora_12_22_todos_golden,
    command = fit_did_all(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_hora_12_22",
      titulo = "Todos os sinsitros, 12h-22h, golden",
      filename = "trecho/het/horario/12_22-todos-golden")),
  tar_target(
    name = did_hora_12_22_moto_golden,
    command = fit_did_all(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_hora_12_22_moto",
      titulo = "Sinistros de moto, 12h-22h, golden",
      filename = "trecho/het/horario/12_22-moto-golden")),
  
  tar_target(
    name = did_hora_17_21_todos_golden,
    command = fit_did_all(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_hora_17_21",
      titulo = "Todos os sinsitros, 17h-21h, golden",
      filename = "trecho/het/horario/17_21-todos-golden")),
  tar_target(
    name = did_hora_17_21_moto_golden,
    command = fit_did_all(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_hora_17_21_moto",
      titulo = "Sinistros de moto, 17h-21h, golden",
      filename = "trecho/het/horario/17_21-moto-golden")),
  
  tar_target(
    name = did_hora_18_19_todos_golden,
    command = fit_did_all(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_hora_18_19",
      titulo = "Todos os sinsitros, 18h-19h, golden",
      filename = "trecho/het/horario/18_19-todos-golden")),
  tar_target(
    name = did_hora_18_19_moto_golden,
    command = fit_did_all(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_hora_18_19_moto",
      titulo = "Sinistros de moto, 18h-19h, golden",
      filename = "trecho/het/horario/18_19-moto-golden")),
  
  tar_target(
    name = did_hora_17_21_choque_golden,
    command = fit_did_all(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_hora_17_21_choque",
      titulo = "Choques, 17h-21h, golden",
      filename = "trecho/het/horario/17_21-choque-golden")),
  tar_target(
    name = did_hora_17_21_colisao_golden,
    command = fit_did_all(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_hora_17_21_colisao",
      titulo = "Colisões, 17h-21h, golden",
      filename = "trecho/het/horario/17_21-colisao-golden")),
  tar_target(
    name = did_hora_17_21_choque_colisao_golden,
    command = fit_did_all(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_hora_17_21_choque_colisao",
      titulo = "Choques ou colisões, 17h-21h, golden",
      filename = "trecho/het/horario/17_21-choque-colisao-golden")),
  tar_target(
    name = did_hora_17_21_atropelamento_golden,
    command = fit_did_all(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_hora_17_21_atropelamento",
      titulo = "Atropelamentos, 17h-21h, golden",
      filename = "trecho/het/horario/17_21-atropelamento-golden")),
  tar_target(
    name = did_hora_17_21_outros_golden,
    command = fit_did_all(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_hora_17_21_outros",
      titulo = "Outros, 17h-21h, golden",
      filename = "trecho/het/horario/17_21-outros-golden")),

  # 7.3.11 Sem vias secundárias ----
  tar_target(
    name = did_filtro_tp_via_todos_golden,
    command = did_het_tipo_via(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros",
      titulo = "Todos os sinistros, golden",
      filename = "trecho/filtro_tipo_via/todos-golden",
      tipo_via = c("primary", "trunk"))),
  tar_target(
    name = did_filtro_tp_via_moto_golden,
    command = did_het_tipo_via(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_veiculo_motocicleta",
      titulo = "Sinistros de moto, golden",
      filename = "trecho/filtro_tipo_via/moto-golden",
      tipo_via = c("primary", "trunk"))),
  
  # 7.3.12 Comprimento do trecho ----
  tar_target(
    name = did_comprimento_50_todos_golden,
    command = did_het_comprimento_trecho(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros",
      titulo = "Todos os sinistros, trechos +50m, golden",
      filename = "trecho/het/comprimento/50-todos-golden",
      comprimento_trecho = 50)),
  tar_target(
    name = did_comprimento_50_moto_golden,
    command = did_het_comprimento_trecho(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_veiculo_motocicleta",
      titulo = "Sinistros de moto, trechos +50m, golden",
      filename = "trecho/filtro_tipo_via/50-moto-golden",
      comprimento_trecho = 50)),
  
  tar_target(
    name = did_comprimento_100_todos_golden,
    command = did_het_comprimento_trecho(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros",
      titulo = "Todos os sinistros, trechos +100m, golden",
      filename = "trecho/het/comprimento/100-todos-golden",
      comprimento_trecho = 100)),
  tar_target(
    name = did_comprimento_100_moto_golden,
    command = did_het_comprimento_trecho(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_veiculo_motocicleta",
      titulo = "Sinistros de moto, trechos +100m, golden",
      filename = "trecho/filtro_tipo_via/100-moto-golden",
      comprimento_trecho = 100)),
  
  tar_target(
    name = did_comprimento_200_todos_golden,
    command = did_het_comprimento_trecho(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros",
      titulo = "Todos os sinistros, trechos +200m, golden",
      filename = "trecho/het/comprimento/200-todos-golden",
      comprimento_trecho = 200)),
  tar_target(
    name = did_comprimento_200_moto_golden,
    command = did_het_comprimento_trecho(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistros_veiculo_motocicleta",
      titulo = "Sinistros de moto, trechos +200m, golden",
      filename = "trecho/filtro_tipo_via/200-moto-golden",
      comprimento_trecho = 200)),
  #
  # 7.4    roda por logradouro ----
  tar_target(
    name = did_logradouro_todos,
    command = fit_did_all(
      df = dado_did_logradouro,
      cohorts = dado_cohort_logradouro,
      yname = "sinistros",
      titulo = "Todos os sinistros",
      filename = "logradouro/todos")),
  tar_target(
    name = did_logradouro_moto,
    command = fit_did_all(
      df = dado_did_logradouro,
      cohorts = dado_cohort_logradouro,
      yname = "sinistros_veiculo_motocicleta",
      titulo = "Sinistros de moto",
      filename = "logradouro/moto")),
  
  tar_target(
    name = did_logradouro_todos_golden,
    command = fit_did_all(
      df = dado_did_logradouro_golden,
      cohorts = dado_cohort_logradouro,
      yname = "sinistros",
      titulo = "Todos os sinistros, golden",
      filename = "logradouro/todos-golden")),
  tar_target(
    name = did_logradouro_moto_golden,
    command = fit_did_all(
      df = dado_did_logradouro_golden,
      cohorts = dado_cohort_logradouro,
      yname = "sinistros_veiculo_motocicleta",
      titulo = "Sinistros de moto, golden",
      filename = "logradouro/moto-golden")),
  
  # 7.4.01 Gravidade ----
  tar_target(
    name = did_logradouro_leve_todos_golden,
    command = fit_did_all(
      df = dado_did_logradouro_golden,
      cohorts = dado_cohort_logradouro,
      yname = "sinistros_gravidade_leve",
      titulo = "Todos os sinistros, leve, golden",
      filename = "logradouro/het/gravidade/leve-todos-golden")),
  tar_target(
    name = did_logradouro_leve_moto_golden,
    command = fit_did_all(
      df = dado_did_logradouro_golden,
      cohorts = dado_cohort_logradouro,
      yname = "sinistros_gravidade_leve_moto",
      titulo = "Sinistros de moto, leve, golden",
      filename = "logradouro/het/gravidade/leve-moto-golden")),
  
  tar_target(
    name = did_logradouro_grave_todos_golden,
    command = fit_did_all(
      df = dado_did_logradouro_golden,
      cohorts = dado_cohort_logradouro,
      yname = "sinistros_gravidade_grave",
      titulo = "Todos os sinistros, grave, golden",
      filename = "logradouro/het/gravidade/grave-todos-golden")),
  tar_target(
    name = did_logradouro_grave_moto_golden,
    command = fit_did_all(
      df = dado_did_logradouro_golden,
      cohorts = dado_cohort_logradouro,
      yname = "sinistros_gravidade_grave_moto",
      titulo = "Sinistros de moto, grave, golden",
      filename = "logradouro/het/gravidade/grave-moto-golden")),
  
  tar_target(
    name = did_logradouro_fatal_todos_golden,
    command = fit_did_all(
      df = dado_did_logradouro_golden,
      cohorts = dado_cohort_logradouro,
      yname = "sinistros_gravidade_fatal",
      titulo = "Todos os sinistros, fatal, golden",
      filename = "logradouro/het/gravidade/fatal-todos-golden")),
  tar_target(
    name = did_logradouro_fatal_moto_golden,
    command = fit_did_all(
      df = dado_did_logradouro_golden,
      cohorts = dado_cohort_logradouro,
      yname = "sinistros_gravidade_fatal_moto",
      titulo = "Sinistros de moto, fatal, golden",
      filename = "logradouro/het/gravidade/fatal-moto-golden")),
  
  # 7.4.02 Tipo de acidente ----
  tar_target(
    name = did_logradouro_choque_todos_golden,
    command = fit_did_all(
      df = dado_did_logradouro_golden,
      cohorts = dado_cohort_logradouro,
      yname = "acidente_choque",
      titulo = "Sinistros com choques, golden",
      filename = "logradouro/het/acidente/choque-todos-golden")),
  tar_target(
    name = did_logradouro_choque_apenas_moto_golden,
    command = fit_did_all(
      df = dado_did_logradouro_golden,
      cohorts = dado_cohort_logradouro,
      yname = "sinistros_choque_apenas_moto",
      titulo = "Sinistros com choques, apenas motos, golden",
      filename = "logradouro/het/acidente/choque-apenas-moto-golden")),
  tar_target(
    name = did_logradouro_choque_moto_carro_golden,
    command = fit_did_all(
      df = dado_did_logradouro_golden,
      cohorts = dado_cohort_logradouro,
      yname = "sinistros_choque_moto_carro",
      titulo = "Sinistros com choques, motos e carros, golden",
      filename = "logradouro/het/acidente/choque-moto-carro-golden")),
  
  tar_target(
    name = did_logradouro_colisao_todos_golden,
    command = fit_did_all(
      df = dado_did_logradouro_golden,
      cohorts = dado_cohort_logradouro,
      yname = "acidente_colisao",
      titulo = "Sinistros com colisões, golden",
      filename = "logradouro/het/acidente/colisao-todos-golden")),
  tar_target(
    name = did_logradouro_colisao_apenas_moto_golden,
    command = fit_did_all(
      df = dado_did_logradouro_golden,
      cohorts = dado_cohort_logradouro,
      yname = "sinistros_colisao_apenas_moto",
      titulo = "Sinistros com colisões, apenas motos, golden",
      filename = "logradouro/het/acidente/colisao-apenas-moto-golden")),
  tar_target(
    name = did_logradouro_colisao_moto_carro_golden,
    command = fit_did_all(
      df = dado_did_logradouro_golden,
      cohorts = dado_cohort_logradouro,
      yname = "sinistros_colisao_moto_carro",
      titulo = "Sinistros com colisões, motos e carros, golden",
      filename = "logradouro/het/acidente/colisao-moto-carro-golden")),
  
  
  
  # 7.5    Efeitos spillover ----
  
  tar_target(
    name = dado_did_trecho_mes_vias,
    command = dado_trecho_mes(dado_sinistros, dado_match, dado_trechos, 
                              tipo_vias = c("trunk", "trunk_link", "motorway", "motorway_link",
                                            "primary", "primary_link", "secondary", "tertiary", "residencial"))),
  
  tar_target(
    name = dado_did_trecho_golden_vizinhos_150,
    command = prepara_trecho_did(dado_did_trecho_mes, dado_trechos, dado_trechos_complemento, 
                                 faixa_azul = dado_vizinhos_150, 
                                 vizinhos = TRUE,
                                 filtrar_por = golden_match)),
  tar_target(
    name = dado_did_trecho_golden_vizinhos_150_vias,
    command = prepara_trecho_did(dado_did_trecho_mes_vias, dado_trechos, dado_trechos_complemento, 
                                 faixa_azul = dado_vizinhos_150, 
                                 vizinhos = TRUE,
                                 filtrar_por = golden_match)),
  tar_target(
    name = dado_did_trecho_golden_vizinhos_500,
    command = prepara_trecho_did(dado_did_trecho_mes, dado_trechos, dado_trechos_complemento, 
                                 faixa_azul = dado_vizinhos_500, 
                                 vizinhos = TRUE,
                                 filtrar_por = golden_match)),
  
  tar_target(
    name = dado_cohort_trecho_vizinhos_150,
    command = definir_cohort_vizinho(
      dado_did_trecho_golden_vizinhos_150,
      faixa_azul = dado_vizinhos_150,
      trechos = dado_trechos,
      logradouros = dado_logradouros,
      por_logradouro = FALSE)),
  tar_target(
    name = dado_cohort_trecho_vizinhos_150_vias,
    command = definir_cohort_vizinho(
      dado_did_trecho_golden_vizinhos_150_vias,
      faixa_azul = dado_vizinhos_150,
      trechos = dado_trechos,
      logradouros = dado_logradouros,
      por_logradouro = FALSE)),
  tar_target(
    name = dado_cohort_trecho_vizinhos_500,
    command = definir_cohort_vizinho(
      dado_did_trecho_golden_vizinhos_500,
      faixa_azul = dado_vizinhos_500,
      trechos = dado_trechos,
      logradouros = dado_logradouros,
      por_logradouro = FALSE)),
  
  
  tar_target(
    name = did_vizinhos_150_todos_golden,
    command = fit_did_all(
      df = dado_did_trecho_golden_vizinhos_150,
      cohorts = dado_cohort_trecho_vizinhos_150,
      yname = "sinistros",
      titulo = "Todos os sinistros, efeito spillover, golden",
      filename = "trecho/vizinhos/150-todos-golden")),
  tar_target(
    name = did_vizinhos_150_moto_golden,
    command = fit_did_all(
      df = dado_did_trecho_golden_vizinhos_150,
      cohorts = dado_cohort_trecho_vizinhos_150,
      yname = "sinistros_veiculo_motocicleta",
      titulo = "Sinistros de moto, efeito spillover, golden",
      filename = "trecho/vizinhos/150-moto-golden")),
  
  tar_target(
    name = did_vizinhos_150_vias_todos_golden,
    command = fit_did_all(
      df = dado_did_trecho_golden_vizinhos_150_vias,
      cohorts = dado_cohort_trecho_vizinhos_150_vias,
      yname = "sinistros",
      titulo = "Todos os sinistros, efeito spillover (todas as vias), golden",
      filename = "trecho/vizinhos/150_vias-todos-golden")),
  tar_target(
    name = did_vizinhos_150_vias_moto_golden,
    command = fit_did_all(
      df = dado_did_trecho_golden_vizinhos_150_vias,
      cohorts = dado_cohort_trecho_vizinhos_150_vias,
      yname = "sinistros_veiculo_motocicleta",
      titulo = "Sinistros de moto, efeito spillover (todas as vias), golden",
      filename = "trecho/vizinhos/150_vias-moto-golden")),
  
  tar_target(
    name = did_vizinhos_500_todos_golden,
    command = fit_did_all(
      df = dado_did_trecho_golden_vizinhos_500,
      cohorts = dado_cohort_trecho_vizinhos_500,
      yname = "sinistros",
      titulo = "Todos os sinistros, efeito spillover, golden",
      filename = "trecho/vizinhos/500-todos-golden")),
  tar_target(
    name = did_vizinhos_500_moto_golden,
    command = fit_did_all(
      df = dado_did_trecho_golden_vizinhos_500,
      cohorts = dado_cohort_trecho_vizinhos_500,
      yname = "sinistros_veiculo_motocicleta",
      titulo = "Sinistros de moto, efeito spillover, golden",
      filename = "trecho/vizinhos/500-moto-golden"))
  
)
