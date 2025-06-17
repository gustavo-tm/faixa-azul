library(targets)
library(visNetwork)
# library(tarchetypes) # Load other packages as needed.

workers <- 4

# para garantir que osmdata vai funcionar
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

# PARA DEBUGAR
# tar_meta(fields = error, complete_only = TRUE) |> tail()
# tar_progress()
# tar_visnetwork()
# tar_delete()

tar_option_set(
  # circlize, webshot2, renv, targets, visNetwork, 
  packages = c("tidyverse", "sf", "osmdata", "fuzzyjoin", "stringdist", "did", "gt", "igraph", "gganimate",
               "tidygraph", "ggraph", "qs2", "MatchIt"), 
  error = "trim",
  format = "qs", # Optionally set the default storage format. qs is fast.

  # controller = crew::crew_controller_local(workers = workers)
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source("scripts/tidy_sinistros.R")
tar_source("scripts/tidy_trechos.R")
tar_source("scripts/trechos_complemento.R")
tar_source("scripts/tidy_faixa_azul.R")
tar_source("scripts/match.R")
tar_source("scripts/descritivas.R")
tar_source("scripts/prepara_did.R")
tar_source("scripts/did.R")


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



  # 3. Agregação de trechos ----
  # 3.1. Logradouro ----
  tar_target(
    name = dado_id_logradouros,
    command = agrupar_logradouros(dado_trechos, dado_token_osm)),
  tar_target(
    name = dado_logradouros,
    command = tidy_logradouros(dado_id_logradouros, dado_trechos, dado_trechos_complemento, dado_faixa_azul)),

  # 3.2. Trechos ----
  tar_target(
    name = dado_id_agregados,
    command = agregar_trechos(dado_trechos, dado_faixa_azul, metros = 500)),
  tar_target(
    name = dado_agregados,
    command = tidy_agregados(dado_id_agregados, dado_trechos, dado_trechos_complemento, dado_faixa_azul)),
  
  tar_target(
    name = dado_id_agregados_1000,
    command = agregar_trechos(dado_trechos, dado_faixa_azul, metros = 1000)),
  tar_target(
    name = dado_agregados_1000,
    command = tidy_agregados(dado_id_agregados_1000, dado_trechos, dado_trechos_complemento, dado_faixa_azul)),

  # 4. FAIXA AZUL ----
  tar_target(
    name = dado_faixa_azul,
    command = tidy_faixa_azul(dado_trechos)),
  tar_target(
    name = dado_vizinhos_150,
    command = tidy_vizinhos(dado_trechos, dado_id_logradouros, dado_faixa_azul, 150)),
  tar_target(
    name = dado_vizinhos_500,
    command = tidy_vizinhos(dado_trechos, dado_id_logradouros, dado_faixa_azul, 500)),


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
    name = dado_match_bind,
    command = bind_rows(dado_match_chunks)),
  tar_target(
    name = dado_match,
    command = golden_match(dado_match_bind)),

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
    name = descritiva_agregados,
    command = plot_agregacao_trechos(dado_trechos, dado_id_logradouros, dado_agregados)),
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
  tar_target(
    name = descritiva_staggered_descritivo,
    command = plot_staggered_descritivo(dado_sinistros, dado_match, dado_faixa_azul)),
  
  


  # 7. DID ----
  # 7.1 agrega
  tar_target(
    name = dado_did_trecho_mes,
    command = dado_trecho_mes(dado_sinistros, dado_match, dado_trechos)),
  
  
  tar_target(
    name = dado_did_agregado_mes,
    command = dado_agregado_mes(dado_sinistros, dado_match, dado_id_agregados, dado_agregados)),
  
  tar_target(
    name = dado_did_agregado_mes_completo,
    command = dado_agregado_mes(dado_sinistros, dado_match, dado_id_agregados, dado_agregados,
                                tipo_join = "completo")),
  tar_target(
    name = dado_did_agregado_mes_completo_1000,
    command = dado_agregado_mes(dado_sinistros, dado_match, dado_id_agregados_1000, dado_agregados_1000,
                                tipo_join = "completo")),
  
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
  tar_target(
    name = dado_did_trecho_bimestral,
    command = consolida_trecho_did(dado_did_trecho_golden, meses = 2)),
  
  # 7.2.2. trecho agregado
  tar_target(
    name = dado_did_agregado,
    command = prepara_agregado_did(dado_did_agregado_mes, dado_agregados, filtrar_por = NULL)),
  tar_target(
    name = dado_did_agregado_golden,
    command = prepara_agregado_did(dado_did_agregado_mes, dado_agregados, filtrar_por = golden_match)),
  
  tar_target(
    name = dado_did_agregado_completo,
    command = prepara_agregado_did(dado_did_agregado_mes_completo, dado_agregados, filtrar_por = golden_match)),
  
  tar_target(
    name = dado_did_agregado_bimestral,
    command = consolida_trecho_did(dado_did_agregado_completo, meses = 2)),
  
  
  tar_target(
    name = dado_did_agregado_completo_1000,
    command = prepara_agregado_did(dado_did_agregado_mes_completo_1000, dado_agregados_1000, filtrar_por = golden_match)),
  
  tar_target(
    name = dado_did_agregado_bimestral_1000,
    command = consolida_trecho_did(dado_did_agregado_completo_1000, meses = 2)),
  
  
  ### Balanceamento
  tar_target(
    name = dado_psm_agregados_simples,
    command = psm_agregados(dado_agregados, dado_id_agregados, dado_sinistros, dado_match, file = "psm simples",
                            inclui_medias = FALSE)),
  tar_target(
    name = dado_psm_agregados,
    command = psm_agregados(dado_agregados, dado_id_agregados, dado_sinistros, dado_match, file = "psm completo",
                            inclui_medias = TRUE)),
  tar_target(
    name = dado_psm_agregados_min,
    command = psm_agregados(dado_agregados, dado_id_agregados, dado_sinistros, dado_match, file = "psm completo min",
                            inclui_medias = TRUE, filtro_min = 0.05)),
  tar_target(
    name = dado_psm_agregados_1000,
    command = psm_agregados(dado_agregados, dado_id_agregados_1000, dado_sinistros, dado_match, file = "psm 1000",
                            inclui_medias = TRUE)),
  
  tar_target(
    name = dado_did_agregado_psm_simples,
    command = prepara_psm_agregados_did(dado_did_agregado_completo, dado_psm_agregados_simples,
                                        file = "df_did_simples")),
  tar_target(
    name = dado_did_agregado_psm,
    command = prepara_psm_agregados_did(dado_did_agregado_completo, dado_psm_agregados,
                                        file = "df_did")),
  tar_target(
    name = dado_did_agregado_psm_min,
    command = prepara_psm_agregados_did(dado_did_agregado_completo, dado_psm_agregados_min,
                                        file = "df_did_min")),
  tar_target(
    name = dado_did_agregado_psm_1000,
    command = prepara_psm_agregados_did(dado_did_agregado_completo, dado_psm_agregados_1000,
                                        file = "df_did_1000")),
  
  tar_target(
    name = dado_did_agregado_psm_bimestral,
    command = consolida_trecho_did(dado_did_agregado_psm, meses = 2)),
  tar_target(
    name = dado_did_agregado_psm_bimestral_1000,
    command = consolida_trecho_did(dado_did_agregado_psm_1000, meses = 2)),
  
  # 7.2.3. logradouro
  tar_target(
    name = dado_did_logradouro,
    command = prepara_logradouro_did(dado_did_logradouro_mes, dado_logradouros,
                                     filtrar_por = NULL)),
  tar_target(
    name = dado_did_logradouro_golden,
    command = prepara_logradouro_did(dado_did_logradouro_mes, dado_logradouros,
                                     filtrar_por = golden_match)),
  tar_target(
    name = dado_did_logradouro_bimestral,
    command = consolida_trecho_did(dado_did_logradouro_golden, meses = 2)),
  
  # 7.2.4. cohort
  tar_target(
    name = dado_cohort_trecho,
    command = definir_cohort(dado_did_trecho,
                             faixa_azul = dado_faixa_azul,
                             trechos = dado_trechos,
                             logradouros = dado_logradouros)),
  tar_target(
    name = dado_cohort_agregado,
    command = definir_cohort(dado_did_agregado,
                             agregados = dado_agregados,
                             logradouros = dado_logradouros,
                             por_agregado = TRUE)),
  tar_target(
    name = dado_cohort_logradouro,
    command = definir_cohort(dado_did_logradouro,
                             logradouros = dado_logradouros,
                             por_logradouro = TRUE)),
  
  
  # 7.3    roda por trecho agregado ----
  ## Resultados principais ----
  
  # Todos
  tar_target(
    name = did_agregado_todos,
    command = fit_csdid(
      df = dado_did_agregado,
      por_km = FALSE,
      yname = "sinistros")),
  tar_target(
    name = did_agregado_todos_w,
    command = fit_csdid(
      df = dado_did_agregado,
      por_km = FALSE,
      weightsname = "comprimento",
      yname = "sinistros")),
  tar_target(
    name = did_agregado_todos_km,
    command = fit_csdid(
      df = dado_did_agregado,
      por_km = TRUE,
      yname = "sinistros")),
  tar_target(
    name = did_agregado_todos_km_w,
    command = fit_csdid(
      df = dado_did_agregado,
      por_km = TRUE,
      weightsname = "comprimento",
      yname = "sinistros")),
  
  # Moto
  tar_target(
    name = did_agregado_moto,
    command = fit_csdid(
      df = dado_did_agregado,
      por_km = FALSE,
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_agregado_moto_w,
    command = fit_csdid(
      df = dado_did_agregado,
      por_km = FALSE,
      weightsname = "comprimento",
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_agregado_moto_km,
    command = fit_csdid(
      df = dado_did_agregado,
      por_km = TRUE,
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_agregado_moto_km_w,
    command = fit_csdid(
      df = dado_did_agregado,
      por_km = TRUE,
      weightsname = "comprimento",
      yname = "sinistros_veiculo_motocicleta")),
  
  # Todos golden
  tar_target(
    name = did_agregado_golden_todos,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      por_km = FALSE,
      yname = "sinistros")),
  tar_target(
    name = did_agregado_golden_todos_w,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      por_km = FALSE,
      weightsname = "comprimento",
      yname = "sinistros")),
  tar_target(
    name = did_agregado_golden_todos_w2,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      por_km = FALSE,
      weightsname = "comprimento_sqrt",
      yname = "sinistros")),
  tar_target(
    name = did_agregado_golden_todos_w3,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      por_km = FALSE,
      weightsname = "comprimento_cbrt",
      yname = "sinistros")),
  tar_target(
    name = did_agregado_golden_todos_km,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      por_km = TRUE,
      yname = "sinistros")),
  tar_target(
    name = did_agregado_golden_todos_km_w,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      por_km = TRUE,
      weightsname = "comprimento",
      yname = "sinistros")),
  tar_target(
    name = did_agregado_golden_todos_km_w2,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      por_km = TRUE,
      weightsname = "comprimento_sqrt",
      yname = "sinistros")),
  tar_target(
    name = did_agregado_golden_todos_km_w3,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      por_km = TRUE,
      weightsname = "comprimento_cbrt",
      yname = "sinistros")),
  
  # Moto golden
  tar_target(
    name = did_agregado_golden_moto,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      por_km = FALSE,
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_agregado_golden_moto_w,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      por_km = FALSE,
      weightsname = "comprimento",
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_agregado_golden_moto_w2,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      por_km = FALSE,
      weightsname = "comprimento_sqrt",
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_agregado_golden_moto_w3,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      por_km = FALSE,
      weightsname = "comprimento_cbrt",
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_agregado_golden_moto_km,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      por_km = TRUE,
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_agregado_golden_moto_km_w,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      por_km = TRUE,
      weightsname = "comprimento",
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_agregado_golden_moto_km_w2,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      por_km = TRUE,
      weightsname = "comprimento_sqrt",
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_agregado_golden_moto_km_w3,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      por_km = TRUE,
      weightsname = "comprimento_cbrt",
      yname = "sinistros_veiculo_motocicleta")),
  
  # Respostas
  
  tar_target(
    name = res_agregado_todos,
    command = res_csdid(
      fit = did_agregado_todos,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros",
      filename = "agregado/todos",
      ylim = 1)),
  tar_target(
    name = res_agregado_todos_w,
    command = res_csdid(
      fit = did_agregado_todos_w,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros",
      filename = "agregado/todos",
      ylim = 2)),
  tar_target(
    name = res_agregado_todos_km,
    command = res_csdid(
      fit = did_agregado_todos_km,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros",
      filename = "agregado/todos",
      ylim = 2)),
  tar_target(
    name = res_agregado_todos_km_w,
    command = res_csdid(
      fit = did_agregado_todos_km_w,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros",
      filename = "agregado/todos",
      ylim = 2)),
  
  tar_target(
    name = res_agregado_moto,
    command = res_csdid(
      fit = did_agregado_moto,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto",
      filename = "agregado/moto",
      ylim = 1)),
  tar_target(
    name = res_agregado_moto_w,
    command = res_csdid(
      fit = did_agregado_moto_w,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto",
      filename = "agregado/moto",
      ylim = 2)),
  tar_target(
    name = res_agregado_moto_km,
    command = res_csdid(
      fit = did_agregado_moto_km,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto",
      filename = "agregado/moto",
      ylim = 2)),
  tar_target(
    name = res_agregado_moto_km_w,
    command = res_csdid(
      fit = did_agregado_moto_km_w,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto",
      filename = "agregado/moto",
      ylim = 2)),
  
  tar_target(
    name = res_agregado_golden_todos,
    command = res_csdid(
      fit = did_agregado_golden_todos,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros, golden",
      filename = "agregado/golden-todos",
      ylim = 1)),
  tar_target(
    name = res_agregado_golden_todos_w,
    command = res_csdid(
      fit = did_agregado_golden_todos_w,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros, golden",
      filename = "agregado/golden-todos",
      ylim = 1)),
  tar_target(
    name = res_agregado_golden_todos_w2,
    command = res_csdid(
      fit = did_agregado_golden_todos_w2,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros, golden",
      filename = "agregado/golden-todos",
      ylim = 1)),
  tar_target(
    name = res_agregado_golden_todos_w3,
    command = res_csdid(
      fit = did_agregado_golden_todos_w3,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros, golden",
      filename = "agregado/golden-todos",
      ylim = 1)),
  tar_target(
    name = res_agregado_golden_todos_km,
    command = res_csdid(
      fit = did_agregado_golden_todos_km,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros, golden",
      filename = "agregado/golden-todos",
      ylim = 2)),
  tar_target(
    name = res_agregado_golden_todos_km_w,
    command = res_csdid(
      fit = did_agregado_golden_todos_km_w,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros, golden",
      filename = "agregado/golden-todos",
      ylim = 2)),
  tar_target(
    name = res_agregado_golden_todos_km_w2,
    command = res_csdid(
      fit = did_agregado_golden_todos_km_w2,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros, golden",
      filename = "agregado/golden-todos",
      ylim = 2)),
  tar_target(
    name = res_agregado_golden_todos_km_w3,
    command = res_csdid(
      fit = did_agregado_golden_todos_km_w3,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros, golden",
      filename = "agregado/golden-todos",
      ylim = 2)),
  
  tar_target(
    name = res_agregado_golden_moto,
    command = res_csdid(
      fit = did_agregado_golden_moto,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto, golden",
      filename = "agregado/golden-moto",
      ylim = 1)),
  tar_target(
    name = res_agregado_golden_moto_w,
    command = res_csdid(
      fit = did_agregado_golden_moto_w,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto, golden",
      filename = "agregado/golden-moto",
      ylim = 1)),
  tar_target(
    name = res_agregado_golden_moto_w2,
    command = res_csdid(
      fit = did_agregado_golden_moto_w2,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto, golden",
      filename = "agregado/golden-moto",
      ylim = 1)),
  tar_target(
    name = res_agregado_golden_moto_w3,
    command = res_csdid(
      fit = did_agregado_golden_moto_w3,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto, golden",
      filename = "agregado/golden-moto",
      ylim = 1)),
  tar_target(
    name = res_agregado_golden_moto_km,
    command = res_csdid(
      fit = did_agregado_golden_moto_km,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto, golden",
      filename = "agregado/golden-moto",
      ylim = 2)),
  tar_target(
    name = res_agregado_golden_moto_km_w,
    command = res_csdid(
      fit = did_agregado_golden_moto_km_w,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto, golden",
      filename = "agregado/golden-moto",
      ylim = 2)),
  tar_target(
    name = res_agregado_golden_moto_km_w2,
    command = res_csdid(
      fit = did_agregado_golden_moto_km_w2,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto, golden",
      filename = "agregado/golden-moto",
      ylim = 2)),
  tar_target(
    name = res_agregado_golden_moto_km_w3,
    command = res_csdid(
      fit = did_agregado_golden_moto_km_w3,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto, golden",
      filename = "agregado/golden-moto",
      ylim = 2)),
  
  
  ## Resultados principais em log ----
  tar_target(
    name = did_agregado_log_todos,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      por_km = FALSE,
      yname = "sinistros",
      log_delta = 1)),
  tar_target(
    name = did_agregado_log_todos_km,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      por_km = TRUE,
      yname = "sinistros",
      log_delta = 0.1)),
  tar_target(
    name = did_agregado_log_moto,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      por_km = FALSE,
      yname = "sinistros_veiculo_motocicleta",
      log_delta = 1)),
  tar_target(
    name = did_agregado_log_moto_km,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      por_km = TRUE,
      yname = "sinistros_veiculo_motocicleta",
      log_delta = 0.1)),
  
  
  tar_target(
    name = res_agregado_log_todos,
    command = res_csdid(
      fit = did_agregado_log_todos,
      cohort = dado_cohort_agregado,
      titulo = "Log todos os sinistros",
      filename = "agregado/log/todos",
      ylim = 1)),
  tar_target(
    name = res_agregado_log_todos_km,
    command = res_csdid(
      fit = did_agregado_log_todos_km,
      cohort = dado_cohort_agregado,
      titulo = "Log todos os sinistros",
      filename = "agregado/log/todos",
      ylim = 2)),
  tar_target(
    name = res_agregado_log_moto,
    command = res_csdid(
      fit = did_agregado_log_moto,
      cohort = dado_cohort_agregado,
      titulo = "Log sinistros de moto",
      filename = "agregado/log/moto",
      ylim = 1)),
  tar_target(
    name = res_agregado_log_moto_km,
    command = res_csdid(
      fit = did_agregado_log_moto_km,
      cohort = dado_cohort_agregado,
      titulo = "Log sinistros de moto",
      filename = "agregado/log/moto",
      ylim = 2)),
  
  
  ## Efeitos heterogeneos ----
  #### Horario ----
  tar_target(
    name = did_agregado_hora_05_10_todos,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      yname = "sinistros_hora_05_10")),
  tar_target(
    name = did_agregado_hora_11_16_todos,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      yname = "sinistros_hora_11_16")),
  tar_target(
    name = did_agregado_hora_12_22_todos,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      yname = "sinistros_hora_12_22")),
  tar_target(
    name = did_agregado_hora_17_20_todos,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      yname = "sinistros_hora_17_20")),
  tar_target(
    name = did_agregado_hora_18_19_todos,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      yname = "sinistros_hora_18_19")),
  
  tar_target(
    name = did_agregado_hora_05_10_moto,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      yname = "sinistros_hora_05_10_moto")),
  tar_target(
    name = did_agregado_hora_11_16_moto,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      yname = "sinistros_hora_11_16_moto")),
  tar_target(
    name = did_agregado_hora_12_22_moto,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      yname = "sinistros_hora_12_22_moto")),
  tar_target(
    name = did_agregado_hora_17_20_moto,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      yname = "sinistros_hora_17_20_moto")),
  tar_target(
    name = did_agregado_hora_18_19_moto,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      yname = "sinistros_hora_18_19_moto")),
  
  tar_target(
    name = did_agregado_hora_17_20_choque_colisao,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      yname = "sinistros_hora_17_20_choque_colisao")),
  tar_target(
    name = did_agregado_hora_17_20_atropelamento,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      yname = "sinistros_hora_17_20_atropelamento")),
  tar_target(
    name = did_agregado_hora_17_20_outros,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      yname = "sinistros_hora_17_20_outros")),
  
  
  # Respostas
  tar_target(
    name = res_agregado_hora_05_10_todos,
    command = res_csdid(
      fit = did_agregado_hora_05_10_todos,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros, 05h-10h",
      filename = "agregado/het/horario/05_10-todos",
      ylim = 1.5)),
  tar_target(
    name = res_agregado_hora_11_16_todos,
    command = res_csdid(
      fit = did_agregado_hora_11_16_todos,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros, 11h-16h",
      filename = "agregado/het/horario/11_16-todos",
      ylim = 1.5)),
  tar_target(
    name = res_agregado_hora_12_22_todos,
    command = res_csdid(
      fit = did_agregado_hora_12_22_todos,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros, 12h-22h",
      filename = "agregado/het/horario/12_22-todos",
      ylim = 1.5)),
  tar_target(
    name = res_agregado_hora_17_20_todos,
    command = res_csdid(
      fit = did_agregado_hora_17_20_todos,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros, 17h-20h",
      filename = "agregado/het/horario/17_20-todos",
      ylim = 1.5)),
  tar_target(
    name = res_agregado_hora_18_19_todos,
    command = res_csdid(
      fit = did_agregado_hora_18_19_todos,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros, 18h-19h",
      filename = "agregado/het/horario/18_19-todos",
      ylim = 1.5)),
  
  tar_target(
    name = res_agregado_hora_05_10_moto,
    command = res_csdid(
      fit = did_agregado_hora_05_10_moto,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto, 05h-10h",
      filename = "agregado/het/horario/05_10-moto",
      ylim = 1.5)),
  tar_target(
    name = res_agregado_hora_11_16_moto,
    command = res_csdid(
      fit = did_agregado_hora_11_16_moto,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto, 11h-16h",
      filename = "agregado/het/horario/11_16-moto",
      ylim = 1.5)),
  tar_target(
    name = res_agregado_hora_12_22_moto,
    command = res_csdid(
      fit = did_agregado_hora_12_22_moto,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto, 12h-22h",
      filename = "agregado/het/horario/12_22-moto",
      ylim = 1.5)),
  tar_target(
    name = res_agregado_hora_17_20_moto,
    command = res_csdid(
      fit = did_agregado_hora_17_20_moto,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto, 17h-20h",
      filename = "agregado/het/horario/17_20-moto",
      ylim = 1.5)),
  tar_target(
    name = res_agregado_hora_18_19_moto,
    command = res_csdid(
      fit = did_agregado_hora_18_19_moto,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto, 18h-19h",
      filename = "agregado/het/horario/18_19-moto",
      ylim = 1.5)),
  
  tar_target(
    name = res_agregado_hora_17_20_choque_colisao,
    command = res_csdid(
      fit = did_agregado_hora_17_20_choque_colisao,
      cohort = dado_cohort_agregado,
      titulo = "Choques e colisões, 17h-20h",
      filename = "agregado/het/horario/tipo_acidente/17_20-choque-colisao",
      ylim = 1.5)),
  tar_target(
    name = res_agregado_hora_17_20_atropelamento,
    command = res_csdid(
      fit = did_agregado_hora_17_20_atropelamento,
      cohort = dado_cohort_agregado,
      titulo = "Atropelamentos, 17h-20h",
      filename = "agregado/het/horario/tipo_acidente/17_20-atropelamento",
      ylim = 1.5)),
  tar_target(
    name = res_agregado_hora_17_20_outros,
    command = res_csdid(
      fit = did_agregado_hora_17_20_outros,
      cohort = dado_cohort_agregado,
      titulo = "Outros sinistros, 17h-20h",
      filename = "agregado/het/horario/tipo_acidente/17_20-outros",
      ylim = 1.5)),
  
  #### Gravidade ----
  tar_target(
    name = did_agregado_gravidade_fatal_todos,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      yname = "sinistros_gravidade_fatal")),
  tar_target(
    name = did_agregado_gravidade_grave_todos,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      yname = "sinistros_gravidade_grave")),
  tar_target(
    name = did_agregado_gravidade_leve_todos,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      yname = "sinistros_gravidade_leve")),
  
  tar_target(
    name = did_agregado_gravidade_fatal_moto,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      yname = "sinistros_gravidade_fatal_moto")),
  tar_target(
    name = did_agregado_gravidade_grave_moto,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      yname = "sinistros_gravidade_grave_moto")),
  tar_target(
    name = did_agregado_gravidade_leve_moto,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      yname = "sinistros_gravidade_leve_moto")),
  
  
  #### Tipo de acidente ----
  tar_target(
    name = did_agregado_acidente_choque_todos,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      yname = "acidente_choque")),
  tar_target(
    name = did_agregado_acidente_colisao_todos,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      yname = "acidente_colisao")),
  tar_target(
    name = did_agregado_acidente_atropelamento_todos,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      yname = "acidente_atropelamento")),
  tar_target(
    name = did_agregado_acidente_outros_todos,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      yname = "acidente_outros")),
  
  tar_target(
    name = did_agregado_acidente_choque_apenas_moto,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      yname = "sinistros_choque_apenas_moto")),
  tar_target(
    name = did_agregado_acidente_colisao_apenas_moto,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      yname = "sinistros_choque_apenas_moto")),
  
  tar_target(
    name = did_agregado_acidente_choque_moto_carro,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      yname = "sinistros_choque_moto_carro")),
  tar_target(
    name = did_agregado_acidente_colisao_moto_carro,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      yname = "sinistros_choque_moto_carro")),
  
  tar_target(
    name = did_agregado_acidente_choque_moto_outros,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      yname = "sinistros_choque_moto_outros")),
  tar_target(
    name = did_agregado_acidente_colisao_moto_outros,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      yname = "sinistros_choque_moto_outros")),
  
  
  #### Quantidade de envolvidos ----
  tar_target(
    name = did_agregado_qtd_envolvidos_todos,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      yname = "qtd_envolvidos")),
  
  tar_target(
    name = did_agregado_qtd_fatal_todos,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      yname = "gravidade_fatal")),
  tar_target(
    name = did_agregado_qtd_grave_todos,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      yname = "gravidade_grave")),
  tar_target(
    name = did_agregado_qtd_leve_todos,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      yname = "gravidade_leve")),
  
  
  # Veiculos
  tar_target(
    name = did_agregado_qtd_motocicleta_todos,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      yname = "veiculo_motocicleta")),
  tar_target(
    name = did_agregado_qtd_automovel_todos,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      yname = "veiculo_automovel")),
  
  
  #### Comprimento do trecho ----
  tar_target(
    name = did_agregado_comprimento_50_2000_todos,
    command = het_csdid_comprimento(
      df = dado_did_agregado_golden,
      comprimento_min = 50,
      comprimento_max = 2000,
      yname = "sinistros")),
  tar_target(
    name = did_agregado_comprimento_100_1500_todos,
    command = het_csdid_comprimento(
      df = dado_did_agregado_golden,
      comprimento_min = 50,
      comprimento_max = 2000,
      yname = "sinistros")),
  
  tar_target(
    name = did_agregado_comprimento_50_2000_moto,
    command = het_csdid_comprimento(
      df = dado_did_agregado_golden,
      comprimento_min = 100,
      comprimento_max = 1500,
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_agregado_comprimento_100_1500_moto,
    command = het_csdid_comprimento(
      df = dado_did_agregado_golden,
      comprimento_min = 100,
      comprimento_max = 1500,
      yname = "sinistros_veiculo_motocicleta")),
  
  
  #### Grupos (tempo de tratamento) ----
  tar_target(
    name = did_agregado_grupos_37_46_todos,
    command = het_csdid_grupos(
      df = dado_did_agregado_golden,
      grupos = 37:46,
      yname = "sinistros")),
  tar_target(
    name = did_agregado_grupos_58_60_todos,
    command = het_csdid_grupos(
      df = dado_did_agregado_golden,
      grupos = 58:60,
      yname = "sinistros")),
  tar_target(
    name = did_agregado_grupos_63_73_todos,
    command = het_csdid_grupos(
      df = dado_did_agregado_golden,
      grupos = 63:73,
      yname = "sinistros")),
  
  tar_target(
    name = did_agregado_grupos_37_46_moto,
    command = het_csdid_grupos(
      df = dado_did_agregado_golden,
      grupos = 37:46,
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_agregado_grupos_58_60_moto,
    command = het_csdid_grupos(
      df = dado_did_agregado_golden,
      grupos = 58:60,
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_agregado_grupos_63_73_moto,
    command = het_csdid_grupos(
      df = dado_did_agregado_golden,
      grupos = 63:73,
      yname = "sinistros_veiculo_motocicleta")),
  
  #### Número de faixas ----
  tar_target(
    name = did_agregado_num_faixas_2ate_todos,
    command = het_csdid_num_faixas(
      df = dado_did_agregado_golden,
      num_faixas = 2,
      ate = TRUE,
      yname = "sinistros")),
  tar_target(
    name = did_agregado_num_faixas_2maisque_todos,
    command = het_csdid_num_faixas(
      df = dado_did_agregado_golden,
      num_faixas = 2,
      ate = FALSE,
      yname = "sinistros")),
  tar_target(
    name = did_agregado_num_faixas_3ate_todos,
    command = het_csdid_num_faixas(
      df = dado_did_agregado_golden,
      num_faixas = 3,
      ate = TRUE,
      yname = "sinistros")),
  tar_target(
    name = did_agregado_num_faixas_3maisque_todos,
    command = het_csdid_num_faixas(
      df = dado_did_agregado_golden,
      num_faixas = 3,
      ate = FALSE,
      yname = "sinistros")),
  
  tar_target(
    name = did_agregado_num_faixas_2ate_moto,
    command = het_csdid_num_faixas(
      df = dado_did_agregado_golden,
      num_faixas = 2,
      ate = TRUE,
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_agregado_num_faixas_2maisque_moto,
    command = het_csdid_num_faixas(
      df = dado_did_agregado_golden,
      num_faixas = 2,
      ate = FALSE,
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_agregado_num_faixas_3ate_moto,
    command = het_csdid_num_faixas(
      df = dado_did_agregado_golden,
      num_faixas = 3,
      ate = TRUE,
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_agregado_num_faixas_3maisque_moto,
    command = het_csdid_num_faixas(
      df = dado_did_agregado_golden,
      num_faixas = 3,
      ate = FALSE,
      yname = "sinistros_veiculo_motocicleta")),
  
  #### Tipo de via ----
  tar_target(
    name = did_agregado_tipo_vias_todos,
    command = het_csdid_tipo_vias(
      df = dado_did_agregado_golden,
      tipo_vias = c("primary", "trunk"),
      yname = "sinistros")),
  tar_target(
    name = did_agregado_tipo_vias_primary_todos,
    command = het_csdid_tipo_vias(
      df = dado_did_agregado_golden,
      tipo_vias = c("primary"),
      yname = "sinistros")),
  tar_target(
    name = did_agregado_tipo_vias_trunk_todos,
    command = het_csdid_tipo_vias(
      df = dado_did_agregado_golden,
      tipo_vias = c("trunk"),
      yname = "sinistros")),
  
  tar_target(
    name = did_agregado_tipo_vias_moto,
    command = het_csdid_tipo_vias(
      df = dado_did_agregado_golden,
      tipo_vias = c("primary", "trunk"),
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_agregado_tipo_vias_primary_moto,
    command = het_csdid_tipo_vias(
      df = dado_did_agregado_golden,
      tipo_vias = c("primary"),
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_agregado_tipo_vias_trunk_moto,
    command = het_csdid_tipo_vias(
      df = dado_did_agregado_golden,
      tipo_vias = c("trunk"),
      yname = "sinistros_veiculo_motocicleta")),
  
  
  #### Velocidade máxima ----
  tar_target(
    name = did_agregado_vel_maxima_40ate_todos,
    command = het_csdid_vel_maxima(
      df = dado_did_agregado_golden,
      vel_maxima = 40,
      ate = TRUE,
      control_group = "notyettreated",
      yname = "sinistros")),
  tar_target(
    name = did_agregado_vel_maxima_40maisque_todos,
    command = het_csdid_vel_maxima(
      df = dado_did_agregado_golden,
      vel_maxima = 40,
      ate = FALSE,
      yname = "sinistros")),
  tar_target(
    name = did_agregado_vel_maxima_50ate_todos,
    command = het_csdid_vel_maxima(
      df = dado_did_agregado_golden,
      vel_maxima = 50,
      ate = TRUE,
      yname = "sinistros")),
  tar_target(
    name = did_agregado_vel_maxima_50maisque_todos,
    command = het_csdid_vel_maxima(
      df = dado_did_agregado_golden,
      vel_maxima = 50,
      ate = FALSE,
      yname = "sinistros")),
  
  tar_target(
    name = did_agregado_vel_maxima_40ate_moto,
    command = het_csdid_vel_maxima(
      df = dado_did_agregado_golden,
      vel_maxima = 40,
      ate = TRUE,
      control_group = "notyettreated",
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_agregado_vel_maxima_40maisque_moto,
    command = het_csdid_vel_maxima(
      df = dado_did_agregado_golden,
      vel_maxima = 40,
      ate = FALSE,
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_agregado_vel_maxima_50ate_moto,
    command = het_csdid_vel_maxima(
      df = dado_did_agregado_golden,
      vel_maxima = 50,
      ate = TRUE,
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_agregado_vel_maxima_50maisque_moto,
    command = het_csdid_vel_maxima(
      df = dado_did_agregado_golden,
      vel_maxima = 50,
      ate = FALSE,
      yname = "sinistros_veiculo_motocicleta")),
  
  
  # 7.3.1  com balanceamento proprio ----
  ## Resultados principais ----
  # Todos
  tar_target(
    name = did_agregado_psm_todos,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      por_km = FALSE,
      yname = "sinistros")),
  tar_target(
    name = did_agregado_psm_todos_w,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      por_km = FALSE,
      weightsname = "comprimento",
      yname = "sinistros")),
  tar_target(
    name = did_agregado_psm_todos_w2,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      por_km = FALSE,
      weightsname = "comprimento_sqrt",
      yname = "sinistros")),
  tar_target(
    name = did_agregado_psm_todos_w3,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      por_km = FALSE,
      weightsname = "comprimento_cbrt",
      yname = "sinistros")),
  
  tar_target(
    name = did_agregado_psm_todos_km,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      por_km = TRUE,
      yname = "sinistros")),
  tar_target(
    name = did_agregado_psm_todos_km_w,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      por_km = TRUE,
      weightsname = "comprimento",
      yname = "sinistros")),
  tar_target(
    name = did_agregado_psm_todos_km_w2,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      por_km = TRUE,
      weightsname = "comprimento_sqrt",
      yname = "sinistros")),
  tar_target(
    name = did_agregado_psm_todos_km_w3,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      por_km = TRUE,
      weightsname = "comprimento_cbrt",
      yname = "sinistros")),
  
  # Moto
  tar_target(
    name = did_agregado_psm_moto,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      por_km = FALSE,
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_agregado_psm_moto_w,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      por_km = FALSE,
      weightsname = "comprimento",
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_agregado_psm_moto_w2,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      por_km = FALSE,
      weightsname = "comprimento_sqrt",
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_agregado_psm_moto_w3,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      por_km = FALSE,
      weightsname = "comprimento_cbrt",
      yname = "sinistros_veiculo_motocicleta")),
  
  tar_target(
    name = did_agregado_psm_moto_km,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      por_km = TRUE,
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_agregado_psm_moto_km_w,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      por_km = TRUE,
      weightsname = "comprimento",
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_agregado_psm_moto_km_w2,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      por_km = TRUE,
      weightsname = "comprimento_sqrt",
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_agregado_psm_moto_km_w3,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      por_km = TRUE,
      weightsname = "comprimento_cbrt",
      yname = "sinistros_veiculo_motocicleta")),
  
  
  # Respostas 
  tar_target(
    name = res_agregado_psm_todos,
    command = res_csdid(
      fit = did_agregado_psm_todos,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros",
      filename = "agregado-psm/todos",
      ylim = 1)),
  tar_target(
    name = res_agregado_psm_todos_w,
    command = res_csdid(
      fit = did_agregado_psm_todos_w,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros",
      filename = "agregado-psm/todos",
      ylim = 1)),
  tar_target(
    name = res_agregado_psm_todos_w2,
    command = res_csdid(
      fit = did_agregado_psm_todos_w2,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros",
      filename = "agregado-psm/todos",
      ylim = 1)),
  tar_target(
    name = res_agregado_psm_todos_w3,
    command = res_csdid(
      fit = did_agregado_psm_todos_w3,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros",
      filename = "agregado-psm/todos",
      ylim = 1)),
  
  tar_target(
    name = res_agregado_psm_todos_km,
    command = res_csdid(
      fit = did_agregado_psm_todos_km,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros",
      filename = "agregado-psm/todos",
      ylim = 1.5)),
  tar_target(
    name = res_agregado_psm_todos_km_w,
    command = res_csdid(
      fit = did_agregado_psm_todos_km_w,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros",
      filename = "agregado-psm/todos",
      ylim = 1.5)),
  tar_target(
    name = res_agregado_psm_todos_km_w2,
    command = res_csdid(
      fit = did_agregado_psm_todos_km_w,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros",
      filename = "agregado-psm/todos",
      ylim = 1.5)),
  tar_target(
    name = res_agregado_psm_todos_km_w3,
    command = res_csdid(
      fit = did_agregado_psm_todos_km_w,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros",
      filename = "agregado-psm/todos",
      ylim = 1.5)),
  
  tar_target(
    name = res_agregado_psm_moto,
    command = res_csdid(
      fit = did_agregado_psm_moto,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto",
      filename = "agregado-psm/moto",
      ylim = 1)),
  tar_target(
    name = res_agregado_psm_moto_w,
    command = res_csdid(
      fit = did_agregado_psm_moto_w,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto",
      filename = "agregado-psm/moto",
      ylim = 1)),
  tar_target(
    name = res_agregado_psm_moto_w2,
    command = res_csdid(
      fit = did_agregado_psm_moto_w2,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto",
      filename = "agregado-psm/moto",
      ylim = 1)),
  tar_target(
    name = res_agregado_psm_moto_w3,
    command = res_csdid(
      fit = did_agregado_psm_moto_w3,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto",
      filename = "agregado-psm/moto",
      ylim = 1)),
  
  tar_target(
    name = res_agregado_psm_moto_km,
    command = res_csdid(
      fit = did_agregado_psm_moto_km,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto",
      filename = "agregado-psm/moto",
      ylim = 1.5)),
  tar_target(
    name = res_agregado_psm_moto_km_w,
    command = res_csdid(
      fit = did_agregado_psm_moto_km_w,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto",
      filename = "agregado-psm/moto",
      ylim = 1.5)),
  tar_target(
    name = res_agregado_psm_moto_km_w2,
    command = res_csdid(
      fit = did_agregado_psm_moto_km_w2,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto",
      filename = "agregado-psm/moto",
      ylim = 1.5)),
  tar_target(
    name = res_agregado_psm_moto_km_w3,
    command = res_csdid(
      fit = did_agregado_psm_moto_km_w3,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto",
      filename = "agregado-psm/moto",
      ylim = 1.5)),
  
  ## Resultados principais em log ----
  tar_target(
    name = did_agregado_psm_log_todos,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      por_km = FALSE,
      yname = "sinistros",
      log_delta = 1)),
  tar_target(
    name = did_agregado_psm_log_todos_km,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      por_km = TRUE,
      yname = "sinistros",
      log_delta = 0.1)),
  tar_target(
    name = did_agregado_psm_log_moto,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      por_km = FALSE,
      yname = "sinistros_veiculo_motocicleta",
      log_delta = 1)),
  tar_target(
    name = did_agregado_psm_log_moto_km,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      por_km = TRUE,
      yname = "sinistros_veiculo_motocicleta",
      log_delta = 0.1)),
  
  
  tar_target(
    name = res_agregado_psm_log_todos,
    command = res_csdid(
      fit = did_agregado_psm_log_todos,
      cohort = dado_cohort_agregado,
      titulo = "Log todos os sinistros",
      filename = "agregado-psm/log/todos",
      ylim = 0.5)),
  tar_target(
    name = res_agregado_psm_log_todos_km,
    command = res_csdid(
      fit = did_agregado_psm_log_todos_km,
      cohort = dado_cohort_agregado,
      titulo = "Log todos os sinistros",
      filename = "agregado-psm/log/todos",
      ylim = 1.5)),
  tar_target(
    name = res_agregado_psm_log_moto,
    command = res_csdid(
      fit = did_agregado_psm_log_moto,
      cohort = dado_cohort_agregado,
      titulo = "Log sinistros de moto",
      filename = "agregado-psm/log/moto",
      ylim = 0.5)),
  tar_target(
    name = res_agregado_psm_log_moto_km,
    command = res_csdid(
      fit = did_agregado_psm_log_moto_km,
      cohort = dado_cohort_agregado,
      titulo = "Log sinistros de moto",
      filename = "agregado-psm/log/moto",
      ylim = 1.5)),
  
  ## Com cutoff no balanceamento
  tar_target(
    name = did_agregado_psm_min_todos,
    command = fit_csdid(
      df = dado_did_agregado_psm_min,
      por_km = FALSE,
      yname = "sinistros",
      log_delta = 1)),
  tar_target(
    name = did_agregado_psm_min_todos_km,
    command = fit_csdid(
      df = dado_did_agregado_psm_min,
      por_km = TRUE,
      yname = "sinistros",
      log_delta = 0.1)),
  tar_target(
    name = did_agregado_psm_min_moto,
    command = fit_csdid(
      df = dado_did_agregado_psm_min,
      por_km = FALSE,
      yname = "sinistros_veiculo_motocicleta",
      log_delta = 1)),
  tar_target(
    name = did_agregado_psm_min_moto_km,
    command = fit_csdid(
      df = dado_did_agregado_psm_min,
      por_km = TRUE,
      yname = "sinistros_veiculo_motocicleta",
      log_delta = 0.1)),
  
  
  tar_target(
    name = res_agregado_psm_min_todos,
    command = res_csdid(
      fit = did_agregado_psm_min_todos,
      cohort = dado_cohort_agregado,
      titulo = "(min) Todos os sinistros",
      filename = "agregado-psm/min/todos",
      ylim = 0.5)),
  tar_target(
    name = res_agregado_psm_min_todos_km,
    command = res_csdid(
      fit = did_agregado_psm_min_todos_km,
      cohort = dado_cohort_agregado,
      titulo = "(min) Todos os sinistros",
      filename = "agregado-psm/min/todos",
      ylim = 1.5)),
  tar_target(
    name = res_agregado_psm_min_moto,
    command = res_csdid(
      fit = did_agregado_psm_min_moto,
      cohort = dado_cohort_agregado,
      titulo = "(min) Sinistros de moto",
      filename = "agregado-psm/min/moto",
      ylim = 0.5)),
  tar_target(
    name = res_agregado_psm_min_moto_km,
    command = res_csdid(
      fit = did_agregado_psm_min_moto_km,
      cohort = dado_cohort_agregado,
      titulo = "(min) Sinistros de moto",
      filename = "agregado-psm/min/moto",
      ylim = 1.5)),
  
  ## Efeitos heterogêneos ----
  #### Horario ----
  tar_target(
    name = did_agregado_psm_hora_07_10_todos,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      yname = "sinistros_hora_05_10")),
  tar_target(
    name = did_agregado_psm_hora_11_16_todos,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      yname = "sinistros_hora_11_16")),
  tar_target(
    name = did_agregado_psm_hora_12_22_todos,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      yname = "sinistros_hora_12_22")),
  tar_target(
    name = did_agregado_psm_hora_17_20_todos,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      yname = "sinistros_hora_17_20")),
  tar_target(
    name = did_agregado_psm_hora_18_19_todos,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      yname = "sinistros_hora_18_19")),
  
  tar_target(
    name = did_agregado_psm_hora_07_10_moto,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      yname = "sinistros_hora_05_10_moto")),
  tar_target(
    name = did_agregado_psm_hora_11_16_moto,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      yname = "sinistros_hora_11_16_moto")),
  tar_target(
    name = did_agregado_psm_hora_12_22_moto,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      yname = "sinistros_hora_12_22_moto")),
  tar_target(
    name = did_agregado_psm_hora_17_20_moto,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      yname = "sinistros_hora_17_20_moto")),
  tar_target(
    name = did_agregado_psm_hora_18_19_moto,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      yname = "sinistros_hora_18_19_moto")),
  
  tar_target(
    name = did_agregado_psm_hora_17_20_choque_colisao,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      yname = "sinistros_hora_17_20_choque_colisao")),
  tar_target(
    name = did_agregado_psm_hora_17_20_atropelamento,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      yname = "sinistros_hora_17_20_atropelamento")),
  tar_target(
    name = did_agregado_psm_hora_17_20_outros,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      yname = "sinistros_hora_17_20_outros")),
  
  
  
  tar_target(
    name = res_agregado_psm_hora_07_10_todos,
    command = res_csdid(
      fit = did_agregado_psm_hora_07_10_todos,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros, 07h-10h",
      filename = "agregado-psm/het/horario/07_10-todos",
      ylim = 1.5)),
  tar_target(
    name = res_agregado_psm_hora_11_16_todos,
    command = res_csdid(
      fit = did_agregado_psm_hora_11_16_todos,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros, 11h-16h",
      filename = "agregado-psm/het/horario/11_16-todos",
      ylim = 1.5)),
  tar_target(
    name = res_agregado_psm_hora_12_22_todos,
    command = res_csdid(
      fit = did_agregado_psm_hora_12_22_todos,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros, 12h-22h",
      filename = "agregado-psm/het/horario/12_22-todos",
      ylim = 1.5)),
  tar_target(
    name = res_agregado_psm_hora_17_20_todos,
    command = res_csdid(
      fit = did_agregado_psm_hora_17_20_todos,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros, 17h-20h",
      filename = "agregado-psm/het/horario/17_20-todos",
      ylim = 1.5)),
  tar_target(
    name = res_agregado_psm_hora_18_19_todos,
    command = res_csdid(
      fit = did_agregado_psm_hora_18_19_todos,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros, 18h-19h",
      filename = "agregado-psm/het/horario/18_19-todos",
      ylim = 1.5)),
  
  tar_target(
    name = res_agregado_psm_hora_07_10_moto,
    command = res_csdid(
      fit = did_agregado_psm_hora_07_10_moto,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto, 07h-10h",
      filename = "agregado-psm/het/horario/07_10-moto",
      ylim = 1.5)),
  tar_target(
    name = res_agregado_psm_hora_11_16_moto,
    command = res_csdid(
      fit = did_agregado_psm_hora_11_16_moto,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto, 11h-16h",
      filename = "agregado-psm/het/horario/11_16-moto",
      ylim = 1.5)),
  tar_target(
    name = res_agregado_psm_hora_12_22_moto,
    command = res_csdid(
      fit = did_agregado_psm_hora_12_22_moto,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto, 12h-22h",
      filename = "agregado-psm/het/horario/12_22-moto",
      ylim = 1.5)),
  tar_target(
    name = res_agregado_psm_hora_17_20_moto,
    command = res_csdid(
      fit = did_agregado_psm_hora_17_20_moto,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto, 17h-20h",
      filename = "agregado-psm/het/horario/17_20-moto",
      ylim = 1.5)),
  tar_target(
    name = res_agregado_psm_hora_18_19_moto,
    command = res_csdid(
      fit = did_agregado_psm_hora_18_19_moto,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto, 18h-19h",
      filename = "agregado-psm/het/horario/18_19-moto",
      ylim = 1.5)),
  
  tar_target(
    name = res_agregado_psm_hora_17_20_choque_colisao,
    command = res_csdid(
      fit = did_agregado_psm_hora_17_20_choque_colisao,
      cohort = dado_cohort_agregado,
      titulo = "Choques e colisões, 17h-20h",
      filename = "agregado-psm/het/horario/tipo_acidente/17_20-choque-colisao",
      ylim = 1.5)),
  tar_target(
    name = res_agregado_psm_hora_17_20_atropelamento,
    command = res_csdid(
      fit = did_agregado_psm_hora_17_20_atropelamento,
      cohort = dado_cohort_agregado,
      titulo = "Atropelamentos, 17h-20h",
      filename = "agregado-psm/het/horario/tipo_acidente/17_20-atropelamento",
      ylim = 1.5)),
  tar_target(
    name = res_agregado_psm_hora_17_20_outros,
    command = res_csdid(
      fit = did_agregado_psm_hora_17_20_outros,
      cohort = dado_cohort_agregado,
      titulo = "Outros sinistros, 17h-20h",
      filename = "agregado-psm/het/horario/tipo_acidente/17_20-outros",
      ylim = 1.5)),
  
  
  # EM LOG
  tar_target(
    name = did_agregado_psm_hora_log_07_10_todos,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      log_delta = 0.1,
      yname = "sinistros_hora_05_10")),
  tar_target(
    name = did_agregado_psm_hora_log_17_20_todos,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      log_delta = 0.1,
      yname = "sinistros_hora_17_20")),
  tar_target(
    name = did_agregado_psm_hora_log_07_10_moto,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      log_delta = 0.1,
      yname = "sinistros_hora_05_10_moto")),
  tar_target(
    name = did_agregado_psm_hora_log_17_20_moto,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      log_delta = 0.1,
      yname = "sinistros_hora_17_20_moto")),


  tar_target(
    name = res_agregado_psm_hora_log_07_10_todos,
    command = res_csdid(
      fit = did_agregado_psm_hora_log_07_10_todos,
      cohort = dado_cohort_agregado,
      titulo = "Log todos os sinistros, 07h-10h",
      filename = "agregado-psm/het/horario/log/07_10-todos",
      ylim = 1.5)),
  tar_target(
    name = res_agregado_psm_hora_log_17_20_todos,
    command = res_csdid(
      fit = did_agregado_psm_hora_log_17_20_todos,
      cohort = dado_cohort_agregado,
      titulo = "Log todos os sinistros, 17h-20h",
      filename = "agregado-psm/het/horario/log/17_20-todos",
      ylim = 1.5)),
  tar_target(
    name = res_agregado_psm_hora_log_07_10_moto,
    command = res_csdid(
      fit = did_agregado_psm_hora_log_07_10_moto,
      cohort = dado_cohort_agregado,
      titulo = "Log sinistros de moto, 07h-10h",
      filename = "agregado-psm/het/horario/log/07_10-moto",
      ylim = 1.5)),
  tar_target(
    name = res_agregado_psm_hora_log_17_20_moto,
    command = res_csdid(
      fit = did_agregado_psm_hora_log_17_20_moto,
      cohort = dado_cohort_agregado,
      titulo = "Log sinistros de moto, 17h-20h",
      filename = "agregado-psm/het/horario/log/17_20-moto",
      ylim = 1.5)),
  
  
  
  # TOTAL
  tar_target(
    name = did_agregado_psm_hora_07_10_todos_total,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      por_km = FALSE,
      yname = "sinistros_hora_05_10")),
  tar_target(
    name = did_agregado_psm_hora_17_20_todos_total,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      por_km = FALSE,
      yname = "sinistros_hora_17_20")),
  tar_target(
    name = did_agregado_psm_hora_07_10_moto_total,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      por_km = FALSE,
      yname = "sinistros_hora_05_10_moto")),
  tar_target(
    name = did_agregado_psm_hora_17_20_moto_total,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      por_km = FALSE,
      yname = "sinistros_hora_17_20_moto")),
  
  
  tar_target(
    name = res_agregado_psm_hora_07_10_todos_total,
    command = res_csdid(
      fit = did_agregado_psm_hora_07_10_todos_total,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros, 07h-10h",
      filename = "agregado-psm/het/horario/07_10-todos",
      ylim = 0.5)),
  tar_target(
    name = res_agregado_psm_hora_17_20_todos_total,
    command = res_csdid(
      fit = did_agregado_psm_hora_17_20_todos_total,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros, 17h-20h",
      filename = "agregado-psm/het/horario/17_20-todos",
      ylim = 0.5)),
  tar_target(
    name = res_agregado_psm_hora_07_10_moto_total,
    command = res_csdid(
      fit = did_agregado_psm_hora_07_10_moto_total,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto, 07h-10h",
      filename = "agregado-psm/het/horario/07_10-moto",
      ylim = 0.5)),
  tar_target(
    name = res_agregado_psm_hora_17_20_moto_total,
    command = res_csdid(
      fit = did_agregado_psm_hora_17_20_moto_total,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto, 17h-20h",
      filename = "agregado-psm/het/horario/17_20-moto",
      ylim = 0.5)),
  
  
  # TOTAL EM LOG
  tar_target(
    name = did_agregado_psm_hora_log_07_10_todos_total,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      por_km = FALSE,
      log_delta = 1,
      yname = "sinistros_hora_05_10")),
  tar_target(
    name = did_agregado_psm_hora_log_17_20_todos_total,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      por_km = FALSE,
      log_delta = 1,
      yname = "sinistros_hora_17_20")),
  tar_target(
    name = did_agregado_psm_hora_log_07_10_moto_total,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      por_km = FALSE,
      log_delta = 1,
      yname = "sinistros_hora_05_10_moto")),
  tar_target(
    name = did_agregado_psm_hora_log_17_20_moto_total,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      por_km = FALSE,
      log_delta = 1,
      yname = "sinistros_hora_17_20_moto")),
  
  
  tar_target(
    name = res_agregado_psm_hora_log_07_10_todos_total,
    command = res_csdid(
      fit = did_agregado_psm_hora_log_07_10_todos_total,
      cohort = dado_cohort_agregado,
      titulo = "Log todos os sinistros, 07h-10h",
      filename = "agregado-psm/het/horario/log/07_10-todos",
      ylim = 0.3)),
  tar_target(
    name = res_agregado_psm_hora_log_17_20_todos_total,
    command = res_csdid(
      fit = did_agregado_psm_hora_log_17_20_todos_total,
      cohort = dado_cohort_agregado,
      titulo = "Log todos os sinistros, 17h-20h",
      filename = "agregado-psm/het/horario/log/17_20-todos",
      ylim = 0.3)),
  tar_target(
    name = res_agregado_psm_hora_log_07_10_moto_total,
    command = res_csdid(
      fit = did_agregado_psm_hora_log_07_10_moto_total,
      cohort = dado_cohort_agregado,
      titulo = "Log sinistros de moto, 07h-10h",
      filename = "agregado-psm/het/horario/log/07_10-moto",
      ylim = 0.3)),
  tar_target(
    name = res_agregado_psm_hora_log_17_20_moto_total,
    command = res_csdid(
      fit = did_agregado_psm_hora_log_17_20_moto_total,
      cohort = dado_cohort_agregado,
      titulo = "Log sinistros de moto, 17h-20h",
      filename = "agregado-psm/het/horario/log/17_20-moto",
      ylim = 0.3)),
  
  #### Gravidade ----
  tar_target(
    name = did_agregado_psm_gravidade_fatal_todos,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      yname = "sinistros_gravidade_fatal")),
  tar_target(
    name = did_agregado_psm_gravidade_grave_todos,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      yname = "sinistros_gravidade_grave")),
  tar_target(
    name = did_agregado_psm_gravidade_leve_todos,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      yname = "sinistros_gravidade_leve")),
  
  tar_target(
    name = did_agregado_psm_gravidade_fatal_moto,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      yname = "sinistros_gravidade_fatal_moto")),
  tar_target(
    name = did_agregado_psm_gravidade_grave_moto,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      yname = "sinistros_gravidade_grave_moto")),
  tar_target(
    name = did_agregado_psm_gravidade_leve_moto,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      yname = "sinistros_gravidade_leve_moto")),
  
  
  tar_target(
    name = res_agregado_psm_gravidade_fatal_todos,
    command = res_csdid(
      fit = did_agregado_psm_gravidade_fatal_todos,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros, com óbitos",
      filename = "agregado-psm/het/gravidade/fatal-todos",
      ylim = 0.5)),
  tar_target(
    name = res_agregado_psm_gravidade_grave_todos,
    command = res_csdid(
      fit = did_agregado_psm_gravidade_grave_todos,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros, com vitmas graves",
      filename = "agregado-psm/het/gravidade/grave-todos",
      ylim = 0.5)),
  tar_target(
    name = res_agregado_psm_gravidade_leve_todos,
    command = res_csdid(
      fit = did_agregado_psm_gravidade_leve_todos,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros, com vitmas leves",
      filename = "agregado-psm/het/gravidade/leve-todos",
      ylim = 1)),
  
  tar_target(
    name = res_agregado_psm_gravidade_fatal_moto,
    command = res_csdid(
      fit = did_agregado_psm_gravidade_fatal_moto,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto, com óbitos",
      filename = "agregado-psm/het/gravidade/fatal-moto",
      ylim = 0.5)),
  tar_target(
    name = res_agregado_psm_gravidade_grave_moto,
    command = res_csdid(
      fit = did_agregado_psm_gravidade_grave_moto,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto, com vitmas graves",
      filename = "agregado-psm/het/gravidade/grave-moto",
      ylim = 0.5)),
  tar_target(
    name = res_agregado_psm_gravidade_leve_moto,
    command = res_csdid(
      fit = did_agregado_psm_gravidade_leve_moto,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto, com vitmas leves",
      filename = "agregado-psm/het/gravidade/leve-moto",
      ylim = 1)),
  
  
  # EM LOG
  tar_target(
    name = did_agregado_psm_gravidade_log_fatal_todos,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      log_delta = 0.1,
      yname = "sinistros_gravidade_fatal")),
  tar_target(
    name = did_agregado_psm_gravidade_log_grave_todos,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      log_delta = 0.1,
      yname = "sinistros_gravidade_grave")),
  tar_target(
    name = did_agregado_psm_gravidade_log_leve_todos,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      log_delta = 0.1,
      yname = "sinistros_gravidade_leve")),
  
  tar_target(
    name = did_agregado_psm_gravidade_log_fatal_moto,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      log_delta = 0.1,
      yname = "sinistros_gravidade_fatal_moto")),
  tar_target(
    name = did_agregado_psm_gravidade_log_grave_moto,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      log_delta = 0.1,
      yname = "sinistros_gravidade_grave_moto")),
  tar_target(
    name = did_agregado_psm_gravidade_log_leve_moto,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      log_delta = 0.1,
      yname = "sinistros_gravidade_leve_moto")),
  
  
  tar_target(
    name = res_agregado_psm_gravidade_log_fatal_todos,
    command = res_csdid(
      fit = did_agregado_psm_gravidade_log_fatal_todos,
      cohort = dado_cohort_agregado,
      titulo = "Log todos os sinistros, com óbitos",
      filename = "agregado-psm/het/gravidade/log/fatal-todos",
      ylim = 0.4)),
  tar_target(
    name = res_agregado_psm_gravidade_log_grave_todos,
    command = res_csdid(
      fit = did_agregado_psm_gravidade_log_grave_todos,
      cohort = dado_cohort_agregado,
      titulo = "Log todos os sinistros, com vitmas graves",
      filename = "agregado-psm/het/gravidade/log/grave-todos",
      ylim = 0.4)),
  tar_target(
    name = res_agregado_psm_gravidade_log_leve_todos,
    command = res_csdid(
      fit = did_agregado_psm_gravidade_log_leve_todos,
      cohort = dado_cohort_agregado,
      titulo = "Log todos os sinistros, com vitmas leves",
      filename = "agregado-psm/het/gravidade/log/leve-moto",
      ylim = 1)),
  
  tar_target(
    name = res_agregado_psm_gravidade_log_fatal_moto,
    command = res_csdid(
      fit = did_agregado_psm_gravidade_log_fatal_moto,
      cohort = dado_cohort_agregado,
      titulo = "Log sinistros de moto, com óbitos",
      filename = "agregado-psm/het/gravidade/log/fatal-moto",
      ylim = 0.4)),
  tar_target(
    name = res_agregado_psm_gravidade_log_grave_moto,
    command = res_csdid(
      fit = did_agregado_psm_gravidade_log_grave_moto,
      cohort = dado_cohort_agregado,
      titulo = "Log sinistros de moto, com vitmas graves",
      filename = "agregado-psm/het/gravidade/log/grave-moto",
      ylim = 0.4)),
  tar_target(
    name = res_agregado_psm_gravidade_log_leve_moto,
    command = res_csdid(
      fit = did_agregado_psm_gravidade_log_leve_moto,
      cohort = dado_cohort_agregado,
      titulo = "Log sinistros de moto, com vitmas leves",
      filename = "agregado-psm/het/gravidade/log/leve-moto",
      ylim = 1)),
  
  
  # TOTAL
  tar_target(
    name = did_agregado_psm_gravidade_fatal_todos_total,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      por_km = FALSE,
      yname = "sinistros_gravidade_fatal")),
  tar_target(
    name = did_agregado_psm_gravidade_grave_todos_total,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      por_km = FALSE,
      yname = "sinistros_gravidade_grave")),
  tar_target(
    name = did_agregado_psm_gravidade_leve_todos_total,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      por_km = FALSE,
      yname = "sinistros_gravidade_leve")),
  
  tar_target(
    name = did_agregado_psm_gravidade_fatal_moto_total,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      por_km = FALSE,
      yname = "sinistros_gravidade_fatal_moto")),
  tar_target(
    name = did_agregado_psm_gravidade_grave_moto_total,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      por_km = FALSE,
      yname = "sinistros_gravidade_grave_moto")),
  tar_target(
    name = did_agregado_psm_gravidade_leve_moto_total,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      por_km = FALSE,
      yname = "sinistros_gravidade_leve_moto")),
  
  
  tar_target(
    name = res_agregado_psm_gravidade_fatal_todos_total,
    command = res_csdid(
      fit = did_agregado_psm_gravidade_fatal_todos_total,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros, com óbitos",
      filename = "agregado-psm/het/gravidade/fatal-todos",
      ylim = 0.25)),
  tar_target(
    name = res_agregado_psm_gravidade_grave_todos_total,
    command = res_csdid(
      fit = did_agregado_psm_gravidade_grave_todos_total,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros, com vitmas graves",
      filename = "agregado-psm/het/gravidade/grave-todos",
      ylim = 0.25)),
  tar_target(
    name = res_agregado_psm_gravidade_leve_todos_total,
    command = res_csdid(
      fit = did_agregado_psm_gravidade_leve_todos_total,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros, com vitmas leves",
      filename = "agregado-psm/het/gravidade/leve-todos",
      ylim = 0.75)),
  
  tar_target(
    name = res_agregado_psm_gravidade_fatal_moto_total,
    command = res_csdid(
      fit = did_agregado_psm_gravidade_fatal_moto_total,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto, com óbitos",
      filename = "agregado-psm/het/gravidade/fatal-moto",
      ylim = 0.25)),
  tar_target(
    name = res_agregado_psm_gravidade_grave_moto_total,
    command = res_csdid(
      fit = did_agregado_psm_gravidade_grave_moto_total,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto, com vitmas graves",
      filename = "agregado-psm/het/gravidade/grave-moto",
      ylim = 0.25)),
  tar_target(
    name = res_agregado_psm_gravidade_leve_moto_total,
    command = res_csdid(
      fit = did_agregado_psm_gravidade_leve_moto_total,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto, com vitmas leves",
      filename = "agregado-psm/het/gravidade/leve-moto",
      ylim = 0.75)),
  
  
  # TOTAL EM LOG
  tar_target(
    name = did_agregado_psm_gravidade_log_fatal_todos_total,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      por_km = FALSE,
      log_delta = 0.1,
      yname = "sinistros_gravidade_fatal")),
  tar_target(
    name = did_agregado_psm_gravidade_log_grave_todos_total,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      por_km = FALSE,
      log_delta = 0.1,
      yname = "sinistros_gravidade_grave")),
  tar_target(
    name = did_agregado_psm_gravidade_log_leve_todos_total,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      por_km = FALSE,
      log_delta = 0.1,
      yname = "sinistros_gravidade_leve")),
  
  tar_target(
    name = did_agregado_psm_gravidade_log_fatal_moto_total,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      por_km = FALSE,
      log_delta = 0.1,
      yname = "sinistros_gravidade_fatal_moto")),
  tar_target(
    name = did_agregado_psm_gravidade_log_grave_moto_total,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      por_km = FALSE,
      log_delta = 0.1,
      yname = "sinistros_gravidade_grave_moto")),
  tar_target(
    name = did_agregado_psm_gravidade_log_leve_moto_total,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      por_km = FALSE,
      log_delta = 0.1,
      yname = "sinistros_gravidade_leve_moto")),
  
  
  tar_target(
    name = res_agregado_psm_gravidade_log_fatal_todos_total,
    command = res_csdid(
      fit = did_agregado_psm_gravidade_log_fatal_todos_total,
      cohort = dado_cohort_agregado,
      titulo = "Log todos os sinistros, com óbitos",
      filename = "agregado-psm/het/gravidade/log/fatal-todos",
      ylim = 0.4)),
  tar_target(
    name = res_agregado_psm_gravidade_log_grave_todos_total,
    command = res_csdid(
      fit = did_agregado_psm_gravidade_log_grave_todos_total,
      cohort = dado_cohort_agregado,
      titulo = "Log todos os sinistros, com vitmas graves",
      filename = "agregado-psm/het/gravidade/log/grave-todos",
      ylim = 0.4)),
  tar_target(
    name = res_agregado_psm_gravidade_log_leve_todos_total,
    command = res_csdid(
      fit = did_agregado_psm_gravidade_log_leve_todos_total,
      cohort = dado_cohort_agregado,
      titulo = "Log todos os sinistros, com vitmas leves",
      filename = "agregado-psm/het/gravidade/log/leve-moto",
      ylim = 1)),
  
  tar_target(
    name = res_agregado_psm_gravidade_log_fatal_moto_total,
    command = res_csdid(
      fit = did_agregado_psm_gravidade_log_fatal_moto_total,
      cohort = dado_cohort_agregado,
      titulo = "Log sinistros de moto, com óbitos",
      filename = "agregado-psm/het/gravidade/log/fatal-moto",
      ylim = 0.4)),
  tar_target(
    name = res_agregado_psm_gravidade_log_grave_moto_total,
    command = res_csdid(
      fit = did_agregado_psm_gravidade_log_grave_moto_total,
      cohort = dado_cohort_agregado,
      titulo = "Log sinistros de moto, com vitmas graves",
      filename = "agregado-psm/het/gravidade/log/grave-moto",
      ylim = 0.4)),
  tar_target(
    name = res_agregado_psm_gravidade_log_leve_moto_total,
    command = res_csdid(
      fit = did_agregado_psm_gravidade_log_leve_moto_total,
      cohort = dado_cohort_agregado,
      titulo = "Log sinistros de moto, com vitmas leves",
      filename = "agregado-psm/het/gravidade/log/leve-moto",
      ylim = 1)),
  
  
  #### Tipo de acidente ----
  tar_target(
    name = did_agregado_psm_acidente_choque_todos,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      yname = "acidente_choque")),
  tar_target(
    name = did_agregado_psm_acidente_colisao_todos,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      yname = "acidente_colisao")),
  tar_target(
    name = did_agregado_psm_acidente_atropelamento_todos,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      yname = "acidente_atropelamento")),
  tar_target(
    name = did_agregado_psm_acidente_outros_todos,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      yname = "acidente_outros")),
  
  tar_target(
    name = did_agregado_psm_acidente_choque_apenas_moto,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      yname = "sinistros_choque_apenas_moto")),
  tar_target(
    name = did_agregado_psm_acidente_colisao_apenas_moto,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      yname = "sinistros_choque_apenas_moto")),
  
  tar_target(
    name = did_agregado_psm_acidente_choque_moto_carro,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      yname = "sinistros_choque_moto_carro")),
  tar_target(
    name = did_agregado_psm_acidente_colisao_moto_carro,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      yname = "sinistros_choque_moto_carro")),
  
  tar_target(
    name = did_agregado_psm_acidente_choque_moto_outros,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      yname = "sinistros_choque_moto_outros")),
  tar_target(
    name = did_agregado_psm_acidente_colisao_moto_outros,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      yname = "sinistros_choque_moto_outros")),
  
  
  tar_target(
    name = res_agregado_psm_acidente_choque_todos,
    command = res_csdid(
      fit = did_agregado_psm_acidente_choque_todos,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros, choques",
      filename = "agregado-psm/het/tipo-acidente/choque-todos",
      ylim = 1.5)),
  tar_target(
    name = res_agregado_psm_acidente_colisao_todos,
    command = res_csdid(
      fit = did_agregado_psm_acidente_colisao_todos,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros, colisões",
      filename = "agregado-psm/het/tipo-acidente/colisao-todos",
      ylim = 1.5)),
  tar_target(
    name = res_agregado_psm_acidente_atropelamento_todos,
    command = res_csdid(
      fit = did_agregado_psm_acidente_atropelamento_todos,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros, atropelamentos",
      filename = "agregado-psm/het/tipo-acidente/atropelmento-todos",
      ylim = 1.5)),
  tar_target(
    name = res_agregado_psm_acidente_outros_todos,
    command = res_csdid(
      fit = did_agregado_psm_acidente_outros_todos,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros, outros",
      filename = "agregado-psm/het/tipo-acidente/outros-todos",
      ylim = 1.5)),
  
  tar_target(
    name = res_agregado_psm_acidente_choque_apenas_moto,
    command = res_csdid(
      fit = did_agregado_psm_acidente_choque_apenas_moto,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros com moto somente, choques",
      filename = "agregado-psm/het/tipo-acidente/choque-apenas-moto",
      ylim = 1.5)),
  tar_target(
    name = res_agregado_psm_acidente_colisao_apenas_moto,
    command = res_csdid(
      fit = did_agregado_psm_acidente_colisao_apenas_moto,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros com moto somente, colisões",
      filename = "agregado-psm/het/tipo-acidente/colisao-apenas-moto",
      ylim = 1.5)),
  
  tar_target(
    name = res_agregado_psm_acidente_choque_moto_carro,
    command = res_csdid(
      fit = did_agregado_psm_acidente_choque_moto_carro,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros com moto e carro, choques",
      filename = "agregado-psm/het/tipo-acidente/choque-moto-carro",
      ylim = 1.5)),
  tar_target(
    name = res_agregado_psm_acidente_colisao_moto_carro,
    command = res_csdid(
      fit = did_agregado_psm_acidente_colisao_moto_carro,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros com moto e carro, colisões",
      filename = "agregado-psm/het/tipo-acidente/colisao-moto-carro",
      ylim = 1.5)),
  
  tar_target(
    name = res_agregado_psm_acidente_choque_moto_outros,
    command = res_csdid(
      fit = did_agregado_psm_acidente_choque_moto_outros,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros com moto, choques",
      filename = "agregado-psm/het/tipo-acidente/choque-moto-outros",
      ylim = 1.5)),
  tar_target(
    name = res_agregado_psm_acidente_colisao_moto_outros,
    command = res_csdid(
      fit = did_agregado_psm_acidente_colisao_moto_outros,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros com moto, colisões",
      filename = "agregado-psm/het/tipo-acidente/colisao-moto-outros",
      ylim = 1.5)),
  
  
  #### Quantidade de envolvidos ----
  tar_target(
    name = did_agregado_psm_qtd_envolvidos_todos,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      yname = "qtd_envolvidos")),
  
  tar_target(
    name = did_agregado_psm_qtd_fatal_todos,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      yname = "gravidade_fatal")),
  tar_target(
    name = did_agregado_psm_qtd_grave_todos,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      yname = "gravidade_grave")),
  tar_target(
    name = did_agregado_psm_qtd_leve_todos,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      yname = "gravidade_leve")),
  
  
  tar_target(
    name = res_agregado_psm_qtd_envolvidos_todos,
    command = res_csdid(
      fit = did_agregado_psm_qtd_envolvidos_todos,
      cohort = dado_cohort_agregado,
      titulo = "Quantidade de indivíduos envolvidos",
      filename = "agregado-psm/het/qtd-envolvidos/todos",
      ylim = 4)),
  
  tar_target(
    name = res_agregado_psm_qtd_fatal_todos,
    command = res_csdid(
      fit = did_agregado_psm_qtd_fatal_todos,
      cohort = dado_cohort_agregado,
      titulo = "Quantidade de óbitos",
      filename = "agregado-psm/het/qtd-envolvidos/fatal",
      ylim = 0.6)),
  tar_target(
    name = res_agregado_psm_qtd_grave_todos,
    command = res_csdid(
      fit = did_agregado_psm_qtd_grave_todos,
      cohort = dado_cohort_agregado,
      titulo = "Quantidade de vitimas graves",
      filename = "agregado-psm/het/qtd-envolvidos/grave",
      ylim = 0.6)),
  tar_target(
    name = res_agregado_psm_qtd_leve_todos,
    command = res_csdid(
      fit = did_agregado_psm_qtd_leve_todos,
      cohort = dado_cohort_agregado,
      titulo = "Quantidade de vitimas leves",
      filename = "agregado-psm/het/qtd-envolvidos/leve",
      ylim = 1.5)),
  
  
  # Veiculos
  tar_target(
    name = did_agregado_psm_qtd_motocicleta_todos,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      yname = "veiculo_motocicleta")),
  tar_target(
    name = did_agregado_psm_qtd_automovel_todos,
    command = fit_csdid(
      df = dado_did_agregado_psm,
      yname = "veiculo_automovel")),
  
  
  tar_target(
    name = res_agregado_psm_qtd_motocicleta_todos,
    command = res_csdid(
      fit = did_agregado_psm_qtd_motocicleta_todos,
      cohort = dado_cohort_agregado,
      titulo = "Quantidade de motocicletas",
      filename = "agregado-psm/het/qtd-envolvidos/veiculo-moto",
      ylim = 1)),
  tar_target(
    name = res_agregado_psm_qtd_automovel_todos,
    command = res_csdid(
      fit = did_agregado_psm_qtd_automovel_todos,
      cohort = dado_cohort_agregado,
      titulo = "Quantidade de automoveis",
      filename = "agregado-psm/het/qtd-envolvidos/veiculo-carro",
      ylim = 1)),
  
  
  #### Comprimento do trecho ----
  tar_target(
    name = did_agregado_psm_comprimento_50_2000_todos,
    command = het_csdid_comprimento(
      df = dado_did_agregado_psm,
      comprimento_min = 50,
      comprimento_max = 2000,
      yname = "sinistros")),
  tar_target(
    name = did_agregado_psm_comprimento_100_1500_todos,
    command = het_csdid_comprimento(
      df = dado_did_agregado_psm,
      comprimento_min = 50,
      comprimento_max = 2000,
      yname = "sinistros")),
  
  tar_target(
    name = did_agregado_psm_comprimento_50_2000_moto,
    command = het_csdid_comprimento(
      df = dado_did_agregado_psm,
      comprimento_min = 100,
      comprimento_max = 1500,
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_agregado_psm_comprimento_100_1500_moto,
    command = het_csdid_comprimento(
      df = dado_did_agregado_psm,
      comprimento_min = 100,
      comprimento_max = 1500,
      yname = "sinistros_veiculo_motocicleta")),
  
  
  tar_target(
    name = res_agregado_psm_comprimento_50_2000_todos,
    command = res_csdid(
      fit = did_agregado_psm_comprimento_50_2000_todos,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros, 50m a 2000m",
      filename = "agregado-psm/het/comprimento/50_2000-todos",
      ylim = 1.5)),
  tar_target(
    name = res_agregado_psm_comprimento_100_1500_todos,
    command = res_csdid(
      fit = did_agregado_psm_comprimento_100_1500_todos,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros, 100m a 1500m",
      filename = "agregado-psm/het/comprimento/100_1500-todos",
      ylim = 1.5)),
  
  tar_target(
    name = res_agregado_psm_comprimento_50_2000_moto,
    command = res_csdid(
      fit = did_agregado_psm_comprimento_50_2000_moto,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto, 50m a 2000m",
      filename = "agregado-psm/het/comprimento/50_2000-moto",
      ylim = 1.5)),
  tar_target(
    name = res_agregado_psm_comprimento_100_1500_moto,
    command = res_csdid(
      fit = did_agregado_psm_comprimento_100_1500_moto,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto, 100m a 1500m",
      filename = "agregado-psm/het/comprimento/100_1500-moto",
      ylim = 1.5)),
  
  
  #### Grupos (tempo de tratamento) ----
  tar_target(
    name = did_agregado_psm_grupos_37_46_todos,
    command = het_csdid_grupos(
      df = dado_did_agregado_psm,
      grupos = 37:46,
      yname = "sinistros")),
  tar_target(
    name = did_agregado_psm_grupos_58_60_todos,
    command = het_csdid_grupos(
      df = dado_did_agregado_psm,
      grupos = 58:60,
      yname = "sinistros")),
  tar_target(
    name = did_agregado_psm_grupos_63_73_todos,
    command = het_csdid_grupos(
      df = dado_did_agregado_psm,
      grupos = 63:73,
      yname = "sinistros")),
  
  tar_target(
    name = did_agregado_psm_grupos_37_46_moto,
    command = het_csdid_grupos(
      df = dado_did_agregado_psm,
      grupos = 37:46,
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_agregado_psm_grupos_58_60_moto,
    command = het_csdid_grupos(
      df = dado_did_agregado_psm,
      grupos = 58:60,
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_agregado_psm_grupos_63_73_moto,
    command = het_csdid_grupos(
      df = dado_did_agregado_psm,
      grupos = 63:73,
      yname = "sinistros_veiculo_motocicleta")),
  
  
  tar_target(
    name = res_agregado_psm_grupos_37_46_todos,
    command = res_csdid(
      fit = did_agregado_psm_grupos_37_46_todos,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros, grupos 37 a 46",
      filename = "agregado-psm/het/grupo/37_46-todos",
      ylim = 3)),
  tar_target(
    name = res_agregado_psm_grupos_58_60_todos,
    command = res_csdid(
      fit = did_agregado_psm_grupos_58_60_todos,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros, grupos 58 a 60",
      filename = "agregado-psm/het/grupo/58_60-todos",
      ylim = 3)),
  tar_target(
    name = res_agregado_psm_grupos_63_73_todos,
    command = res_csdid(
      fit = did_agregado_psm_grupos_63_73_todos,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros, grupos 63 a 73",
      filename = "agregado-psm/het/grupo/63_73-todos",
      ylim = 3)),
  
  tar_target(
    name = res_agregado_psm_grupos_37_46_moto,
    command = res_csdid(
      fit = did_agregado_psm_grupos_37_46_moto,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto, grupos 37 a 46",
      filename = "agregado-psm/het/grupo/37_46-moto",
      ylim = 3)),
  tar_target(
    name = res_agregado_psm_grupos_58_60_moto,
    command = res_csdid(
      fit = did_agregado_psm_grupos_58_60_moto,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto, grupos 58 a 60",
      filename = "agregado-psm/het/grupo/58_60-moto",
      ylim = 3)),
  tar_target(
    name = res_agregado_psm_grupos_63_73_moto,
    command = res_csdid(
      fit = did_agregado_psm_grupos_63_73_moto,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto, grupos 63 a 73",
      filename = "agregado-psm/het/grupo/63_73-moto",
      ylim = 3)),
  
  #### Número de faixas ----
  tar_target(
    name = did_agregado_psm_num_faixas_2ate_todos,
    command = het_csdid_num_faixas(
      df = dado_did_agregado_psm,
      num_faixas = 2,
      ate = TRUE,
      yname = "sinistros")),
  tar_target(
    name = did_agregado_psm_num_faixas_2maisque_todos,
    command = het_csdid_num_faixas(
      df = dado_did_agregado_psm,
      num_faixas = 2,
      ate = FALSE,
      yname = "sinistros")),
  tar_target(
    name = did_agregado_psm_num_faixas_3ate_todos,
    command = het_csdid_num_faixas(
      df = dado_did_agregado_psm,
      num_faixas = 3,
      ate = TRUE,
      yname = "sinistros")),
  tar_target(
    name = did_agregado_psm_num_faixas_3maisque_todos,
    command = het_csdid_num_faixas(
      df = dado_did_agregado_psm,
      num_faixas = 3,
      ate = FALSE,
      yname = "sinistros")),
  
  tar_target(
    name = did_agregado_psm_num_faixas_2ate_moto,
    command = het_csdid_num_faixas(
      df = dado_did_agregado_psm,
      num_faixas = 2,
      ate = TRUE,
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_agregado_psm_num_faixas_2maisque_moto,
    command = het_csdid_num_faixas(
      df = dado_did_agregado_psm,
      num_faixas = 2,
      ate = FALSE,
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_agregado_psm_num_faixas_3ate_moto,
    command = het_csdid_num_faixas(
      df = dado_did_agregado_psm,
      num_faixas = 3,
      ate = TRUE,
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_agregado_psm_num_faixas_3maisque_moto,
    command = het_csdid_num_faixas(
      df = dado_did_agregado_psm,
      num_faixas = 3,
      ate = FALSE,
      yname = "sinistros_veiculo_motocicleta")),
  
  ## RESPOSTAS COLOCAR
  
  #### Tipo de via ----
  tar_target(
    name = did_agregado_psm_tipo_vias_todos,
    command = het_csdid_tipo_vias(
      df = dado_did_agregado_psm,
      tipo_vias = c("primary", "trunk"),
      yname = "sinistros")),
  tar_target(
    name = did_agregado_psm_tipo_vias_primary_todos,
    command = het_csdid_tipo_vias(
      df = dado_did_agregado_psm,
      tipo_vias = c("primary"),
      yname = "sinistros")),
  tar_target(
    name = did_agregado_psm_tipo_vias_trunk_todos,
    command = het_csdid_tipo_vias(
      df = dado_did_agregado_psm,
      tipo_vias = c("trunk"),
      yname = "sinistros")),
  
  tar_target(
    name = did_agregado_psm_tipo_vias_moto,
    command = het_csdid_tipo_vias(
      df = dado_did_agregado_psm,
      tipo_vias = c("primary", "trunk"),
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_agregado_psm_tipo_vias_primary_moto,
    command = het_csdid_tipo_vias(
      df = dado_did_agregado_psm,
      tipo_vias = c("primary"),
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_agregado_psm_tipo_vias_trunk_moto,
    command = het_csdid_tipo_vias(
      df = dado_did_agregado_psm,
      tipo_vias = c("trunk"),
      yname = "sinistros_veiculo_motocicleta")),
  
  
  tar_target(
    name = res_agregado_psm_tipo_vias_todos,
    command = res_csdid(
      fit = did_agregado_psm_tipo_vias_todos,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros, filtro tipo de vias",
      filename = "agregado-psm/het/tipo-via/filtro-todos",
      ylim = 2)),
  tar_target(
    name = res_agregado_psm_tipo_vias_primary_todos,
    command = res_csdid(
      fit = did_agregado_psm_tipo_vias_primary_todos,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros, vias primárias",
      filename = "agregado-psm/het/tipo-via/primary-todos",
      ylim = 2)),
  tar_target(
    name = res_agregado_psm_tipo_vias_trunk_todos,
    command = res_csdid(
      fit = did_agregado_psm_tipo_vias_trunk_todos,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros, vias troncais",
      filename = "agregado-psm/het/tipo-via/trunk-todos",
      ylim = 2)), 
  
  tar_target(
    name = res_agregado_psm_tipo_vias_moto,
    command = res_csdid(
      fit = did_agregado_psm_tipo_vias_moto,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto, filtro tipo de vias",
      filename = "agregado-psm/het/tipo-via/filtro-moto",
      ylim = 2)),
  tar_target(
    name = res_agregado_psm_tipo_vias_primary_moto,
    command = res_csdid(
      fit = did_agregado_psm_tipo_vias_primary_moto,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto, vias primárias",
      filename = "agregado-psm/het/tipo-via/primary-moto",
      ylim = 2)),
  tar_target(
    name = res_agregado_psm_tipo_vias_trunk_moto,
    command = res_csdid(
      fit = did_agregado_psm_tipo_vias_trunk_moto,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto, vias troncais",
      filename = "agregado-psm/het/tipo-via/trunk-moto",
      ylim = 2)),
  
  
  #### Velocidade máxima ----
  tar_target(
    name = did_agregado_psm_vel_maxima_40ate_todos,
    command = het_csdid_vel_maxima(
      df = dado_did_agregado_psm,
      vel_maxima = 40,
      ate = TRUE,
      control_group = "notyettreated",
      yname = "sinistros")),
  tar_target(
    name = did_agregado_psm_vel_maxima_40maisque_todos,
    command = het_csdid_vel_maxima(
      df = dado_did_agregado_psm,
      vel_maxima = 40,
      ate = FALSE,
      yname = "sinistros")),
  tar_target(
    name = did_agregado_psm_vel_maxima_50ate_todos,
    command = het_csdid_vel_maxima(
      df = dado_did_agregado_psm,
      vel_maxima = 50,
      ate = TRUE,
      yname = "sinistros")),
  tar_target(
    name = did_agregado_psm_vel_maxima_50maisque_todos,
    command = het_csdid_vel_maxima(
      df = dado_did_agregado_psm,
      vel_maxima = 50,
      ate = FALSE,
      yname = "sinistros")),
  
  tar_target(
    name = did_agregado_psm_vel_maxima_40ate_moto,
    command = het_csdid_vel_maxima(
      df = dado_did_agregado_psm,
      vel_maxima = 40,
      ate = TRUE,
      control_group = "notyettreated",
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_agregado_psm_vel_maxima_40maisque_moto,
    command = het_csdid_vel_maxima(
      df = dado_did_agregado_psm,
      vel_maxima = 40,
      ate = FALSE,
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_agregado_psm_vel_maxima_50ate_moto,
    command = het_csdid_vel_maxima(
      df = dado_did_agregado_psm,
      vel_maxima = 50,
      ate = TRUE,
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_agregado_psm_vel_maxima_50maisque_moto,
    command = het_csdid_vel_maxima(
      df = dado_did_agregado_psm,
      vel_maxima = 50,
      ate = FALSE,
      yname = "sinistros_veiculo_motocicleta")),
  
  #### Taxas ----
  tar_target(
    name = did_agregado_psm_taxa_fatalidade_todos,
    command = fit_csdid(
      df = dado_did_agregado_psm_min,
      por_km = FALSE,
      yname = "taxa_fatalidade")),
  tar_target(
    name = did_agregado_psm_taxa_feridos_todos,
    command = fit_csdid(
      df = dado_did_agregado_psm_min,
      por_km = FALSE,
      yname = "taxa_feridos")),
  tar_target(
    name = did_agregado_psm_taxa_fatalidade_moto,
    command = fit_csdid(
      df = dado_did_agregado_psm_min,
      por_km = FALSE,
      yname = "taxa_fatalidade_moto")),
  tar_target(
    name = did_agregado_psm_taxa_feridos_moto,
    command = fit_csdid(
      df = dado_did_agregado_psm_min,
      por_km = FALSE,
      yname = "taxa_feridos_moto")),
  
  tar_target(
    name = did_agregado_psm_taxa_fatalidade_todos_w2,
    command = fit_csdid(
      df = dado_did_agregado_psm_min,
      por_km = FALSE,
      weightsname = "comprimento_sqrt",
      yname = "taxa_fatalidade")),
  tar_target(
    name = did_agregado_psm_taxa_feridos_todos_w2,
    command = fit_csdid(
      df = dado_did_agregado_psm_min,
      por_km = FALSE,
      weightsname = "comprimento_sqrt",
      yname = "taxa_feridos")),
  tar_target(
    name = did_agregado_psm_taxa_fatalidade_moto_w2,
    command = fit_csdid(
      df = dado_did_agregado_psm_min,
      por_km = FALSE,
      weightsname = "comprimento_sqrt",
      yname = "taxa_fatalidade_moto")),
  tar_target(
    name = did_agregado_psm_taxa_feridos_moto_w2,
    command = fit_csdid(
      df = dado_did_agregado_psm_min,
      por_km = FALSE,
      weightsname = "comprimento_sqrt",
      yname = "taxa_feridos_moto")),
  
  
  tar_target(
    name = res_agregado_psm_taxa_fatalidade_todos,
    command = res_csdid(
      fit = did_agregado_psm_taxa_fatalidade_todos,
      cohort = dado_cohort_agregado,
      titulo = "Taxa de fatalidade, todos os sinistros",
      filename = "agregado-psm/het/taxa/fatalidade-todos",
      ylim = 0.1)),
  tar_target(
    name = res_agregado_psm_taxa_feridos_todos,
    command = res_csdid(
      fit = did_agregado_psm_taxa_feridos_todos,
      cohort = dado_cohort_agregado,
      titulo = "Taxa de feridos, todos os sinistros",
      filename = "agregado-psm/het/taxa/feridos-todos",
      ylim = 0.3)),
  tar_target(
    name = res_agregado_psm_taxa_fatalidade_moto,
    command = res_csdid(
      fit = did_agregado_psm_taxa_fatalidade_moto,
      cohort = dado_cohort_agregado,
      titulo = "Taxa de fatalidade, sinistros de moto",
      filename = "agregado-psm/het/taxa/fatalidade-moto",
      ylim = 0.1)),
  tar_target(
    name = res_agregado_psm_taxa_feridos_moto,
    command = res_csdid(
      fit = did_agregado_psm_taxa_feridos_moto,
      cohort = dado_cohort_agregado,
      titulo = "Taxa de feridos, sinistros de moto",
      filename = "agregado-psm/het/taxa/feridos-moto",
      ylim = 0.3)),
  
  tar_target(
    name = res_agregado_psm_taxa_fatalidade_todos_w2,
    command = res_csdid(
      fit = did_agregado_psm_taxa_fatalidade_todos_w2,
      cohort = dado_cohort_agregado,
      titulo = "Taxa de fatalidade, todos os sinistros",
      filename = "agregado-psm/het/taxa/fatalidade-todos",
      ylim = 0.1)),
  tar_target(
    name = res_agregado_psm_taxa_feridos_todos_w2,
    command = res_csdid(
      fit = did_agregado_psm_taxa_feridos_todos_w2,
      cohort = dado_cohort_agregado,
      titulo = "Taxa de feridos, todos os sinistros",
      filename = "agregado-psm/het/taxa/feridos-todos",
      ylim = 0.3)),
  tar_target(
    name = res_agregado_psm_taxa_fatalidade_moto_w2,
    command = res_csdid(
      fit = did_agregado_psm_taxa_fatalidade_moto_w2,
      cohort = dado_cohort_agregado,
      titulo = "Taxa de fatalidade, sinistros de moto",
      filename = "agregado-psm/het/taxa/fatalidade-moto",
      ylim = 0.1)),
  tar_target(
    name = res_agregado_psm_taxa_feridos_moto_w2,
    command = res_csdid(
      fit = did_agregado_psm_taxa_feridos_moto_w2,
      cohort = dado_cohort_agregado,
      titulo = "Taxa de feridos, sinistros de moto",
      filename = "agregado-psm/het/taxa/feridos-moto",
      ylim = 0.3)),
  
  # 7.3.2  com base completa ----
  ## Resultados principais ----
  
  # Todos
  tar_target(
    name = did_agregado_completo_todos,
    command = fit_csdid(
      df = dado_did_agregado_completo,
      por_km = FALSE,
      yname = "sinistros")),
  tar_target(
    name = did_agregado_completo_todos_w,
    command = fit_csdid(
      df = dado_did_agregado_completo,
      por_km = FALSE,
      weightsname = "comprimento",
      yname = "sinistros")),
  tar_target(
    name = did_agregado_completo_todos_w2,
    command = fit_csdid(
      df = dado_did_agregado_completo,
      por_km = FALSE,
      weightsname = "comprimento_sqrt",
      yname = "sinistros")),
  tar_target(
    name = did_agregado_completo_todos_w3,
    command = fit_csdid(
      df = dado_did_agregado_completo,
      por_km = FALSE,
      weightsname = "comprimento_cbrt",
      yname = "sinistros")),
  tar_target(
    name = did_agregado_completo_todos_km,
    command = fit_csdid(
      df = dado_did_agregado_completo,
      por_km = TRUE,
      yname = "sinistros")),
  tar_target(
    name = did_agregado_completo_todos_km_w,
    command = fit_csdid(
      df = dado_did_agregado_completo,
      por_km = TRUE,
      weightsname = "comprimento",
      yname = "sinistros")),
  tar_target(
    name = did_agregado_completo_todos_km_w2,
    command = fit_csdid(
      df = dado_did_agregado_completo,
      por_km = TRUE,
      weightsname = "comprimento_sqrt",
      yname = "sinistros")),
  tar_target(
    name = did_agregado_completo_todos_km_w3,
    command = fit_csdid(
      df = dado_did_agregado_completo,
      por_km = TRUE,
      weightsname = "comprimento_cbrt",
      yname = "sinistros")),
  
  # Moto
  tar_target(
    name = did_agregado_completo_moto,
    command = fit_csdid(
      df = dado_did_agregado_completo,
      por_km = FALSE,
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_agregado_completo_moto_w,
    command = fit_csdid(
      df = dado_did_agregado_completo,
      por_km = FALSE,
      weightsname = "comprimento",
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_agregado_completo_moto_w2,
    command = fit_csdid(
      df = dado_did_agregado_completo,
      por_km = FALSE,
      weightsname = "comprimento_sqrt",
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_agregado_completo_moto_w3,
    command = fit_csdid(
      df = dado_did_agregado_completo,
      por_km = FALSE,
      weightsname = "comprimento_cbrt",
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_agregado_completo_moto_km,
    command = fit_csdid(
      df = dado_did_agregado_completo,
      por_km = TRUE,
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_agregado_completo_moto_km_w,
    command = fit_csdid(
      df = dado_did_agregado_completo,
      por_km = TRUE,
      weightsname = "comprimento",
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_agregado_completo_moto_km_w2,
    command = fit_csdid(
      df = dado_did_agregado_completo,
      por_km = TRUE,
      weightsname = "comprimento_sqrt",
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_agregado_completo_moto_km_w3,
    command = fit_csdid(
      df = dado_did_agregado_completo,
      por_km = TRUE,
      weightsname = "comprimento_cbrt",
      yname = "sinistros_veiculo_motocicleta")),
  
  
  # Respostas
  tar_target(
    name = res_agregado_completo_todos,
    command = res_csdid(
      fit = did_agregado_completo_todos,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros",
      filename = "agregado-completo/todos",
      ylim = 1)),
  tar_target(
    name = res_agregado_completo_todos_w,
    command = res_csdid(
      fit = did_agregado_completo_todos_w,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros",
      filename = "agregado-completo/todos",
      ylim = 1)),
  tar_target(
    name = res_agregado_completo_todos_w2,
    command = res_csdid(
      fit = did_agregado_completo_todos_w2,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros",
      filename = "agregado-completo/todos",
      ylim = 1)),
  tar_target(
    name = res_agregado_completo_todos_w3,
    command = res_csdid(
      fit = did_agregado_completo_todos_w3,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros",
      filename = "agregado-completo/todos",
      ylim = 1)),
  tar_target(
    name = res_agregado_completo_todos_km,
    command = res_csdid(
      fit = did_agregado_completo_todos_km,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros",
      filename = "agregado-completo/todos",
      ylim = 2)),
  tar_target(
    name = res_agregado_completo_todos_km_w,
    command = res_csdid(
      fit = did_agregado_completo_todos_km_w,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros",
      filename = "agregado-completo/todos",
      ylim = 2)),
  tar_target(
    name = res_agregado_completo_todos_km_w2,
    command = res_csdid(
      fit = did_agregado_completo_todos_km_w2,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros",
      filename = "agregado-completo/todos",
      ylim = 2)),
  tar_target(
    name = res_agregado_completo_todos_km_w3,
    command = res_csdid(
      fit = did_agregado_completo_todos_km_w3,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros",
      filename = "agregado-completo/todos",
      ylim = 2)),
  
  tar_target(
    name = res_agregado_completo_moto,
    command = res_csdid(
      fit = did_agregado_completo_moto,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto",
      filename = "agregado-completo/moto",
      ylim = 1)),
  tar_target(
    name = res_agregado_completo_moto_w,
    command = res_csdid(
      fit = did_agregado_completo_moto_w,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto",
      filename = "agregado-completo/moto",
      ylim = 1)),
  tar_target(
    name = res_agregado_completo_moto_w2,
    command = res_csdid(
      fit = did_agregado_completo_moto_w2,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto",
      filename = "agregado-completo/moto",
      ylim = 1)),
  tar_target(
    name = res_agregado_completo_moto_w3,
    command = res_csdid(
      fit = did_agregado_completo_moto_w3,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto",
      filename = "agregado-completo/moto",
      ylim = 1)),
  tar_target(
    name = res_agregado_completo_moto_km,
    command = res_csdid(
      fit = did_agregado_completo_moto_km,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto",
      filename = "agregado-completo/moto",
      ylim = 2)),
  tar_target(
    name = res_agregado_completo_moto_km_w,
    command = res_csdid(
      fit = did_agregado_completo_moto_km_w,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto",
      filename = "agregado-completo/moto",
      ylim = 2)),
  tar_target(
    name = res_agregado_completo_moto_km_w2,
    command = res_csdid(
      fit = did_agregado_completo_moto_km_w2,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto",
      filename = "agregado-completo/moto",
      ylim = 2)),
  tar_target(
    name = res_agregado_completo_moto_km_w3,
    command = res_csdid(
      fit = did_agregado_completo_moto_km_w3,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de moto",
      filename = "agregado-completo/moto",
      ylim = 2)),
  
  
  ## Resultados principais em log ----
  tar_target(
    name = did_agregado_completo_log_todos,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      por_km = FALSE,
      yname = "sinistros",
      log_delta = 1)),
  tar_target(
    name = did_agregado_completo_log_todos_km,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      por_km = TRUE,
      yname = "sinistros",
      log_delta = 0.1)),
  tar_target(
    name = did_agregado_completo_log_moto,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      por_km = FALSE,
      yname = "sinistros_veiculo_motocicleta",
      log_delta = 1)),
  tar_target(
    name = did_agregado_completo_log_moto_km,
    command = fit_csdid(
      df = dado_did_agregado_golden,
      por_km = TRUE,
      yname = "sinistros_veiculo_motocicleta",
      log_delta = 0.1)),
  
  
  tar_target(
    name = res_agregado_completo_log_todos,
    command = res_csdid(
      fit = did_agregado_completo_log_todos,
      cohort = dado_cohort_agregado,
      titulo = "Log todos os sinistros",
      filename = "agregado-completo/log/todos",
      ylim = 1)),
  tar_target(
    name = res_agregado_completo_log_todos_km,
    command = res_csdid(
      fit = did_agregado_completo_log_todos_km,
      cohort = dado_cohort_agregado,
      titulo = "Log todos os sinistros",
      filename = "agregado-completo/log/todos",
      ylim = 2)),
  tar_target(
    name = res_agregado_completo_log_moto,
    command = res_csdid(
      fit = did_agregado_completo_log_moto,
      cohort = dado_cohort_agregado,
      titulo = "Log sinistros de moto",
      filename = "agregado-completo/log/moto",
      ylim = 1)),
  tar_target(
    name = res_agregado_completo_log_moto_km,
    command = res_csdid(
      fit = did_agregado_completo_log_moto_km,
      cohort = dado_cohort_agregado,
      titulo = "Log sinistros de moto",
      filename = "agregado-completo/log/moto",
      ylim = 2)),
  
  
  # 7.4    roda por trecho agregado - por bimestre ----
  ## Resultados principais ----
  # Todos
  tar_target(
    name = did_bimestre_agregado_todos,
    command = fit_csdid(
      df = dado_did_agregado_bimestral,
      por_km = FALSE,
      yname = "sinistros")),
  tar_target(
    name = did_bimestre_agregado_todos_w,
    command = fit_csdid(
      df = dado_did_agregado,
      por_km = FALSE,
      weightsname = "comprimento",
      yname = "sinistros")),
  tar_target(
    name = did_bimestre_agregado_todos_w2,
    command = fit_csdid(
      df = dado_did_agregado,
      por_km = FALSE,
      weightsname = "comprimento_sqrt",
      yname = "sinistros")),
  tar_target(
    name = did_bimestre_agregado_todos_w3,
    command = fit_csdid(
      df = dado_did_agregado,
      por_km = FALSE,
      weightsname = "comprimento_cbrt",
      yname = "sinistros")),
  
  tar_target(
    name = did_bimestre_agregado_todos_km,
    command = fit_csdid(
      df = dado_did_agregado_bimestral,
      por_km = TRUE,
      yname = "sinistros")),
  tar_target(
    name = did_bimestre_agregado_todos_km_w,
    command = fit_csdid(
      df = dado_did_agregado,
      por_km = TRUE,
      weightsname = "comprimento",
      yname = "sinistros")),
  tar_target(
    name = did_bimestre_agregado_todos_km_w2,
    command = fit_csdid(
      df = dado_did_agregado,
      por_km = TRUE,
      weightsname = "comprimento_sqrt",
      yname = "sinistros")),
  tar_target(
    name = did_bimestre_agregado_todos_km_w3,
    command = fit_csdid(
      df = dado_did_agregado,
      por_km = TRUE,
      weightsname = "comprimento_cbrt",
      yname = "sinistros")),
  
  # Moto
  tar_target(
    name = did_bimestre_agregado_moto,
    command = fit_csdid(
      df = dado_did_agregado_bimestral,
      por_km = FALSE,
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_bimestre_agregado_moto_w,
    command = fit_csdid(
      df = dado_did_agregado,
      por_km = FALSE,
      weightsname = "comprimento",
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_bimestre_agregado_moto_w2,
    command = fit_csdid(
      df = dado_did_agregado,
      por_km = FALSE,
      weightsname = "comprimento_sqrt",
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_bimestre_agregado_moto_w3,
    command = fit_csdid(
      df = dado_did_agregado,
      por_km = FALSE,
      weightsname = "comprimento_cbrt",
      yname = "sinistros_veiculo_motocicleta")),
  
  tar_target(
    name = did_bimestre_agregado_moto_km,
    command = fit_csdid(
      df = dado_did_agregado_bimestral,
      por_km = TRUE,
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_bimestre_agregado_moto_km_w,
    command = fit_csdid(
      df = dado_did_agregado,
      por_km = TRUE,
      weightsname = "comprimento",
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_bimestre_agregado_moto_km_w2,
    command = fit_csdid(
      df = dado_did_agregado,
      por_km = TRUE,
      weightsname = "comprimento_sqrt",
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_bimestre_agregado_moto_km_w3,
    command = fit_csdid(
      df = dado_did_agregado,
      por_km = TRUE,
      weightsname = "comprimento_cbrt",
      yname = "sinistros_veiculo_motocicleta")),
  
  
  # Respostas
  tar_target(
    name = res_bimestre_agregado_todos,
    command = res_csdid(
      fit = did_bimestre_agregado_todos,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros",
      filename = "bimestral/agregado/todos",
      ylim = 1,
      xlim = 6)),
  tar_target(
    name = res_bimestre_agregado_todos_w,
    command = res_csdid(
      fit = did_bimestre_agregado_todos_w,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros",
      filename = "bimestral/agregado/todos",
      ylim = 1,
      xlim = 6)),
  tar_target(
    name = res_bimestre_agregado_todos_w2,
    command = res_csdid(
      fit = did_bimestre_agregado_todos_w2,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros",
      filename = "bimestral/agregado/todos",
      ylim = 1,
      xlim = 6)),
  tar_target(
    name = res_bimestre_agregado_todos_w3,
    command = res_csdid(
      fit = did_bimestre_agregado_todos_w3,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros",
      filename = "bimestral/agregado/todos",
      ylim = 1,
      xlim = 6)),
  
  tar_target(
    name = res_bimestre_agregado_todos_km,
    command = res_csdid(
      fit = did_bimestre_agregado_todos_km,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros",
      filename = "bimestral/agregado/todos",
      ylim = 2,
      xlim = 6)),
  tar_target(
    name = res_bimestre_agregado_todos_km_w,
    command = res_csdid(
      fit = did_bimestre_agregado_todos_km_w,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros",
      filename = "bimestral/agregado/todos",
      ylim = 2,
      xlim = 6)),
  tar_target(
    name = res_bimestre_agregado_todos_km_w2,
    command = res_csdid(
      fit = did_bimestre_agregado_todos_km_w2,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros",
      filename = "bimestral/agregado/todos",
      ylim = 2,
      xlim = 6)),
  tar_target(
    name = res_bimestre_agregado_todos_km_w3,
    command = res_csdid(
      fit = did_bimestre_agregado_todos_km_w3,
      cohort = dado_cohort_agregado,
      titulo = "Todos os sinistros",
      filename = "bimestral/agregado/todos",
      ylim = 2,
      xlim = 6)),
  
  tar_target(
    name = res_bimestre_agregado_moto,
    command = res_csdid(
      fit = did_bimestre_agregado_moto,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de motocicleta",
      filename = "bimestral/agregado/moto",
      ylim = 1,
      xlim = 6)),
  tar_target(
    name = res_bimestre_agregado_moto_w,
    command = res_csdid(
      fit = did_bimestre_agregado_moto_w,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de motocicleta",
      filename = "bimestral/agregado/moto",
      ylim = 1,
      xlim = 6)),
  tar_target(
    name = res_bimestre_agregado_moto_w2,
    command = res_csdid(
      fit = did_bimestre_agregado_moto_w,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de motocicleta",
      filename = "bimestral/agregado/moto",
      ylim = 1,
      xlim = 6)),
  tar_target(
    name = res_bimestre_agregado_moto_w3,
    command = res_csdid(
      fit = did_bimestre_agregado_moto_w,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de motocicleta",
      filename = "bimestral/agregado/moto",
      ylim = 1,
      xlim = 6)),
  
  tar_target(
    name = res_bimestre_agregado_moto_km,
    command = res_csdid(
      fit = did_bimestre_agregado_moto_km,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de motocicleta",
      filename = "bimestral/agregado/moto",
      ylim = 2,
      xlim = 6)),
  tar_target(
    name = res_bimestre_agregado_moto_km_w,
    command = res_csdid(
      fit = did_bimestre_agregado_moto_km_w,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de motocicleta",
      filename = "bimestral/agregado/moto",
      ylim = 2,
      xlim = 6)),
  tar_target(
    name = res_bimestre_agregado_moto_km_w2,
    command = res_csdid(
      fit = did_bimestre_agregado_moto_km_w2,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de motocicleta",
      filename = "bimestral/agregado/moto",
      ylim = 2,
      xlim = 6)),
  tar_target(
    name = res_bimestre_agregado_moto_km_w3,
    command = res_csdid(
      fit = did_bimestre_agregado_moto_km_w3,
      cohort = dado_cohort_agregado,
      titulo = "Sinistros de motocicleta",
      filename = "bimestral/agregado/moto",
      ylim = 2,
      xlim = 6)),
  
  
  ## Resultados principais em log ----
  tar_target(
    name = did_bimestre_agregado_log_todos,
    command = fit_csdid(
      df = dado_did_agregado_bimestral,
      por_km = FALSE,
      yname = "sinistros",
      log_delta = 1)),
  tar_target(
    name = did_bimestre_agregado_log_todos_km,
    command = fit_csdid(
      df = dado_did_agregado_bimestral,
      por_km = TRUE,
      yname = "sinistros",
      log_delta = 0.1)),
  tar_target(
    name = did_bimestre_agregado_log_moto,
    command = fit_csdid(
      df = dado_did_agregado_bimestral,
      por_km = FALSE,
      yname = "sinistros_veiculo_motocicleta",
      log_delta = 1)),
  tar_target(
    name = did_bimestre_agregado_log_moto_km,
    command = fit_csdid(
      df = dado_did_agregado_bimestral,
      por_km = TRUE,
      yname = "sinistros_veiculo_motocicleta",
      log_delta = 0.1)),
  
  
  tar_target(
    name = res_bimestre_agregado_log_todos,
    command = res_csdid(
      fit = did_bimestre_agregado_log_todos,
      cohort = dado_cohort_agregado,
      titulo = "Log todos os sinistros",
      filename = "bimestral/agregado/log/todos",
      ylim = 2)),
  tar_target(
    name = res_bimestre_agregado_log_todos_km,
    command = res_csdid(
      fit = did_bimestre_agregado_log_todos_km,
      cohort = dado_cohort_agregado,
      titulo = "Log todos os sinistros",
      filename = "bimestral/agregado/log/todos",
      ylim = 2)),
  tar_target(
    name = res_bimestre_agregado_log_moto,
    command = res_csdid(
      fit = did_bimestre_agregado_log_moto,
      cohort = dado_cohort_agregado,
      titulo = "Log sinistros de moto",
      filename = "bimestral/agregado/log/moto",
      ylim = 2)),
  tar_target(
    name = res_bimestre_agregado_log_moto_km,
    command = res_csdid(
      fit = did_bimestre_agregado_log_moto_km,
      cohort = dado_cohort_agregado,
      titulo = "Log sinistros de moto",
      filename = "bimestral/agregado/log/moto",
      ylim = 2)),
  
  
  # tar_target(
  #   name = res_agregado_log_todos,
  #   command = res_csdid(
  #     fit = did_agregado_log_todos,
  #     cohort = dado_cohort_agregado,
  #     titulo = "Log todos os sinistros",
  #     filename = "agregado/log/todos",
  #     ylim = 1)),
  # tar_target(
  #   name = res_agregado_log_todos_km,
  #   command = res_csdid(
  #     fit = did_agregado_log_todos_km,
  #     cohort = dado_cohort_agregado,
  #     titulo = "Log todos os sinistros",
  #     filename = "agregado/log/todos",
  #     ylim = 2)),
  # tar_target(
  #   name = res_agregado_log_moto,
  #   command = res_csdid(
  #     fit = did_agregado_log_moto,
  #     cohort = dado_cohort_agregado,
  #     titulo = "Log sinistros de moto",
  #     filename = "agregado/log/moto",
  #     ylim = 1)),
  # tar_target(
  #   name = res_agregado_log_moto_km,
  #   command = res_csdid(
  #     fit = did_agregado_log_moto_km,
  #     cohort = dado_cohort_agregado,
  #     titulo = "Log sinistros de moto",
  #     filename = "agregado/log/moto",
  #     ylim = 2)),
  
  
  # 7.4.1  com balanceamento proprio ----
  ## Resultados principais ----
  # Todos
  tar_target(
    name = did_bimestre_agregado_psm_todos,
    command = fit_csdid(
      df = dado_did_agregado_psm_bimestral,
      por_km = FALSE,
      yname = "sinistros")),
  tar_target(
    name = did_bimestre_agregado_psm_todos_w,
    command = fit_csdid(
      df = dado_did_agregado_psm_bimestral,
      por_km = FALSE,
      weightsname = "comprimento",
      yname = "sinistros")),
  tar_target(
    name = did_bimestre_agregado_psm_todos_w2,
    command = fit_csdid(
      df = dado_did_agregado_psm_bimestral,
      por_km = FALSE,
      weightsname = "comprimento_sqrt",
      yname = "sinistros")),
  tar_target(
    name = did_bimestre_agregado_psm_todos_w3,
    command = fit_csdid(
      df = dado_did_agregado_psm_bimestral,
      por_km = FALSE,
      weightsname = "comprimento_cbrt",
      yname = "sinistros")),
  
  tar_target(
    name = did_bimestre_agregado_psm_todos_km,
    command = fit_csdid(
      df = dado_did_agregado_psm_bimestral,
      por_km = TRUE,
      yname = "sinistros")),
  tar_target(
    name = did_bimestre_agregado_psm_todos_km_w,
    command = fit_csdid(
      df = dado_did_agregado_psm_bimestral,
      por_km = TRUE,
      weightsname = "comprimento",
      yname = "sinistros")),
  tar_target(
    name = did_bimestre_agregado_psm_todos_km_w2,
    command = fit_csdid(
      df = dado_did_agregado_psm_bimestral,
      por_km = TRUE,
      weightsname = "comprimento_sqrt",
      yname = "sinistros")),
  tar_target(
    name = did_bimestre_agregado_psm_todos_km_w3,
    command = fit_csdid(
      df = dado_did_agregado_psm_bimestral,
      por_km = TRUE,
      weightsname = "comprimento_cbrt",
      yname = "sinistros")),
  
  # Moto
  tar_target(
    name = did_bimestre_agregado_psm_moto,
    command = fit_csdid(
      df = dado_did_agregado_psm_bimestral,
      por_km = FALSE,
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_bimestre_agregado_psm_moto_w,
    command = fit_csdid(
      df = dado_did_agregado_psm_bimestral,
      por_km = FALSE,
      weightsname = "comprimento",
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_bimestre_agregado_psm_moto_w2,
    command = fit_csdid(
      df = dado_did_agregado_psm_bimestral,
      por_km = FALSE,
      weightsname = "comprimento_sqrt",
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_bimestre_agregado_psm_moto_w3,
    command = fit_csdid(
      df = dado_did_agregado_psm_bimestral,
      por_km = FALSE,
      weightsname = "comprimento_cbrt",
      yname = "sinistros_veiculo_motocicleta")),
  
  tar_target(
    name = did_bimestre_agregado_psm_moto_km,
    command = fit_csdid(
      df = dado_did_agregado_psm_bimestral,
      por_km = TRUE,
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_bimestre_agregado_psm_moto_km_w,
    command = fit_csdid(
      df = dado_did_agregado_psm_bimestral,
      por_km = TRUE,
      weightsname = "comprimento",
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_bimestre_agregado_psm_moto_km_w2,
    command = fit_csdid(
      df = dado_did_agregado_psm_bimestral,
      por_km = TRUE,
      weightsname = "comprimento_sqrt",
      yname = "sinistros_veiculo_motocicleta")),
  tar_target(
    name = did_bimestre_agregado_psm_moto_km_w3,
    command = fit_csdid(
      df = dado_did_agregado_psm_bimestral,
      por_km = TRUE,
      weightsname = "comprimento_cbrt",
      yname = "sinistros_veiculo_motocicleta")),
  
  
  # Respostas 
  tar_target(
    name = res_bimestre_agregado_psm_todos,
    command = res_csdid(
      fit = did_bimestre_agregado_psm_todos,
      cohort = dado_cohort_agregado,
      titulo = "Pareamento psm, todos os sinistros",
      filename = "bimestral/agregado-psm/todos",
      ylim = 1)),
  tar_target(
    name = res_bimestre_agregado_psm_todos_w,
    command = res_csdid(
      fit = did_bimestre_agregado_psm_todos_w,
      cohort = dado_cohort_agregado,
      titulo = "Pareamento psm, todos os sinistros",
      filename = "bimestral/agregado-psm/todos",
      ylim = 1)),
  tar_target(
    name = res_bimestre_agregado_psm_todos_w2,
    command = res_csdid(
      fit = did_bimestre_agregado_psm_todos_w2,
      cohort = dado_cohort_agregado,
      titulo = "Pareamento psm, todos os sinistros",
      filename = "bimestral/agregado-psm/todos",
      ylim = 1)),
  tar_target(
    name = res_bimestre_agregado_psm_todos_w3,
    command = res_csdid(
      fit = did_bimestre_agregado_psm_todos_w3,
      cohort = dado_cohort_agregado,
      titulo = "Pareamento psm, todos os sinistros",
      filename = "bimestral/agregado-psm/todos",
      ylim = 1)),
  
  tar_target(
    name = res_bimestre_agregado_psm_todos_km,
    command = res_csdid(
      fit = did_bimestre_agregado_psm_todos_km,
      cohort = dado_cohort_agregado,
      titulo = "Pareamento psm, todos os sinistros",
      filename = "bimestral/agregado-psm/todos",
      ylim = 2)),
  tar_target(
    name = res_bimestre_agregado_psm_todos_km_w,
    command = res_csdid(
      fit = did_bimestre_agregado_psm_todos_km_w,
      cohort = dado_cohort_agregado,
      titulo = "Pareamento psm, todos os sinistros",
      filename = "bimestral/agregado-psm/todos",
      ylim = 2)),
  tar_target(
    name = res_bimestre_agregado_psm_todos_km_w2,
    command = res_csdid(
      fit = did_bimestre_agregado_psm_todos_km_w,
      cohort = dado_cohort_agregado,
      titulo = "Pareamento psm, todos os sinistros",
      filename = "bimestral/agregado-psm/todos",
      ylim = 2)),
  tar_target(
    name = res_bimestre_agregado_psm_todos_km_w3,
    command = res_csdid(
      fit = did_bimestre_agregado_psm_todos_km_w,
      cohort = dado_cohort_agregado,
      titulo = "Pareamento psm, todos os sinistros",
      filename = "bimestral/agregado-psm/todos",
      ylim = 2)),
  
  tar_target(
    name = res_bimestre_agregado_psm_moto,
    command = res_csdid(
      fit = did_bimestre_agregado_psm_moto,
      cohort = dado_cohort_agregado,
      titulo = "Pareamento psm, sinistros de moto",
      filename = "bimestral/agregado-psm/moto",
      ylim = 1)),
  tar_target(
    name = res_bimestre_agregado_psm_moto_w,
    command = res_csdid(
      fit = did_bimestre_agregado_psm_moto_w,
      cohort = dado_cohort_agregado,
      titulo = "Pareamento psm, sinistros de moto",
      filename = "bimestral/agregado-psm/moto",
      ylim = 1)),
  tar_target(
    name = res_bimestre_agregado_psm_moto_w2,
    command = res_csdid(
      fit = did_bimestre_agregado_psm_moto_w2,
      cohort = dado_cohort_agregado,
      titulo = "Pareamento psm, sinistros de moto",
      filename = "bimestral/agregado-psm/moto",
      ylim = 1)),
  tar_target(
    name = res_bimestre_agregado_psm_moto_w3,
    command = res_csdid(
      fit = did_bimestre_agregado_psm_moto_w3,
      cohort = dado_cohort_agregado,
      titulo = "Pareamento psm, sinistros de moto",
      filename = "bimestral/agregado-psm/moto",
      ylim = 1)),
  
  tar_target(
    name = res_bimestre_agregado_psm_moto_km,
    command = res_csdid(
      fit = did_bimestre_agregado_psm_moto_km,
      cohort = dado_cohort_agregado,
      titulo = "Pareamento psm, sinistros de moto",
      filename = "bimestral/agregado-psm/moto",
      ylim = 2)),
  tar_target(
    name = res_bimestre_agregado_psm_moto_km_w,
    command = res_csdid(
      fit = did_bimestre_agregado_psm_moto_km_w,
      cohort = dado_cohort_agregado,
      titulo = "Pareamento psm, sinistros de moto",
      filename = "bimestral/agregado-psm/moto",
      ylim = 2)),
  tar_target(
    name = res_bimestre_agregado_psm_moto_km_w2,
    command = res_csdid(
      fit = did_bimestre_agregado_psm_moto_km_w2,
      cohort = dado_cohort_agregado,
      titulo = "Pareamento psm, sinistros de moto",
      filename = "bimestral/agregado-psm/moto",
      ylim = 2)),
  tar_target(
    name = res_bimestre_agregado_psm_moto_km_w3,
    command = res_csdid(
      fit = did_bimestre_agregado_psm_moto_km_w3,
      cohort = dado_cohort_agregado,
      titulo = "Pareamento psm, sinistros de moto",
      filename = "bimestral/agregado-psm/moto",
      ylim = 2)),
  
  ## Resultados principais em log ----
  tar_target(
    name = did_bimestre_agregado_psm_log_todos,
    command = fit_csdid(
      df = dado_did_agregado_psm_bimestral,
      por_km = FALSE,
      yname = "sinistros",
      log_delta = 1)),
  tar_target(
    name = did_bimestre_agregado_psm_log_todos_km,
    command = fit_csdid(
      df = dado_did_agregado_psm_bimestral,
      por_km = TRUE,
      yname = "sinistros",
      log_delta = 0.1)),
  tar_target(
    name = did_bimestre_agregado_psm_log_moto,
    command = fit_csdid(
      df = dado_did_agregado_psm_bimestral,
      por_km = FALSE,
      yname = "sinistros_veiculo_motocicleta",
      log_delta = 1)),
  tar_target(
    name = did_bimestre_agregado_psm_log_moto_km,
    command = fit_csdid(
      df = dado_did_agregado_psm_bimestral,
      por_km = TRUE,
      yname = "sinistros_veiculo_motocicleta",
      log_delta = 0.1)),
  
  
  tar_target(
    name = res_bimestre_agregado_psm_log_todos,
    command = res_csdid(
      fit = did_bimestre_agregado_psm_log_todos,
      cohort = dado_cohort_agregado,
      titulo = "Log todos os sinistros",
      filename = "bimestral/agregado-psm/log/todos",
      ylim = 2)),
  tar_target(
    name = res_bimestre_agregado_psm_log_todos_km,
    command = res_csdid(
      fit = did_bimestre_agregado_psm_log_todos_km,
      cohort = dado_cohort_agregado,
      titulo = "Log todos os sinistros",
      filename = "bimestral/agregado-psm/log/todos",
      ylim = 2)),
  tar_target(
    name = res_bimestre_agregado_psm_log_moto,
    command = res_csdid(
      fit = did_bimestre_agregado_psm_log_moto,
      cohort = dado_cohort_agregado,
      titulo = "Log sinistros de moto",
      filename = "bimestral/agregado-psm/log/moto",
      ylim = 2)),
  tar_target(
    name = res_bimestre_agregado_psm_log_moto_km,
    command = res_csdid(
      fit = did_bimestre_agregado_psm_log_moto_km,
      cohort = dado_cohort_agregado,
      titulo = "Log sinistros de moto",
      filename = "bimestral/agregado-psm/log/moto",
      ylim = 2)),
  
  ## ----

  # 7.6    Efeitos spillover ----
  
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
