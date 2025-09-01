library(targets)

# ALTERAR O NÚMERO DE WORKERS PARA TORNAR A PIPELINE PARALELIZADA
# RECOMENDA-SE DE 2 A 4 WORKERS
workers <- 1

# para garantir que osmdata vai funcionar
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

# PARA DEBUGAR
# tar_meta(fields = error, complete_only = TRUE) |> tail()
# tar_progress()
# tar_visnetwork()
# tar_delete()

tar_option_set(
  # circlize, webshot2, renv, targets, visNetwork, 
  packages = c("tidyverse", "sf", "osmdata", "fuzzyjoin", "stringdist", "did", "gt", "kableExtra", "igraph", "gganimate", "gifski",
               "tidygraph", "ggraph", "qs2", "MatchIt", "patchwork", "ggnewscale", "ggspatial", "memoise", "visNetwork"), 
  error = "trim",
  format = "qs", # Optionally set the default storage format. qs is fast.

  controller = crew::crew_controller_local(workers = workers)
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source("scripts/tidy_sinistros.R")
tar_source("scripts/tidy_trechos.R")
tar_source("scripts/trechos_complemento.R")
tar_source("scripts/tidy_faixa_azul.R")
tar_source("scripts/match.R")
tar_source("scripts/descritivas.R")
tar_source("scripts/did.R")


list(
  # 1. SINISTROS ----
  tar_target(
    name = dado_sinistros,
    command = tidy_sinistros()),
  tar_target(
    name = dado_vitimas,
    command = tidy_vitimas()),
  tar_target(
    name = dado_mortes,
    command = calcular_mortes(dado_vitimas)),

  # 2. TRECHOS ----
  tar_target(
    name = dado_osm,
    command = download_osm()),
  tar_target(
    name = dado_trechos_bruto,
    command = tidy_trechos_bruto(dado_osm)),
  tar_target(
    name = dado_radar,
    command = calcular_radares(dado_trechos_bruto)),
  tar_target(
    name = dado_amenidades,
    command = calcular_amenidades(dado_trechos_bruto)),
  tar_target(
    name = dado_interseccao,
    command = calcular_interseccao(dado_trechos_bruto, dado_token_osm)),
  tar_target(
    name = dado_trechos_complemento,
    command = tidy_complemento_trecho(dado_trechos_bruto, dado_radar, dado_interseccao, dado_amenidades)),
  
  tar_target(
    name = dado_trechos,
    command = tidy_trechos(dado_trechos_bruto, dado_trechos_complemento, dado_faixa_azul)),


  # 3. Agregação de trechos ----
  # 3.1. Logradouro ----
  tar_target(
    name = dado_id_logradouros,
    command = agrupar_logradouros(dado_trechos_bruto, dado_token_osm)),
  tar_target(
    name = dado_logradouros,
    command = tidy_logradouros(dado_id_logradouros, dado_trechos_bruto, dado_trechos_complemento, dado_faixa_azul)),

  # 3.2. Trechos ----
  tar_target(
    name = dado_id_agregados,
    command = agregar_trechos(dado_trechos_bruto, dado_faixa_azul, metros = 500)),
  tar_target(
    name = dado_agregados,
    command = tidy_agregados(dado_id_agregados, dado_trechos_bruto, dado_trechos_complemento, dado_faixa_azul)),

  # 4. FAIXA AZUL ----
  tar_target(
    name = dado_faixa_azul,
    command = tidy_faixa_azul(dado_trechos_bruto)),


  # 5. MATCH ----
  tar_target(
    name = dado_token_infosiga,
    command = tokenizar_infosiga(dado_sinistros)),
  tar_target(
    name = dado_token_osm,
    command = tokenizar_osm(dado_trechos_bruto)),

  tar_target(
    name = dado_sinistros_chunks,
    command = match_dados_split(dado_sinistros, n = workers)),

  tar_target(
    name = dado_match_chunks,
    command = match_dados(dado_sinistros_chunks,
                          sinistros_token = dado_token_infosiga,
                          trechos = dado_trechos_bruto,
                          trechos_token = dado_token_osm),
    pattern = NULL,
    iteration = "group"),

  tar_target(
    name = dado_match_bind,
    command = bind_rows(dado_match_chunks)),
  tar_target(
    name = dado_match,
    command = match_ids(dado_match_bind, dado_trechos_bruto, dado_id_agregados, dado_id_logradouros)),

# 6. DESCRITIVAS ----
tar_target(
  name = descritiva_datas_FA,
  command = plot_datas_FA(dado_logradouros, dado_id_logradouros, dado_match, dado_sinistros)),
tar_target(
  name = descritiva_datas_trechos,
  command = plot_datas_trechos(dado_faixa_azul, dado_trechos_bruto, dado_token_osm)),
tar_target(
  name = descritiva_tamanho_FA,
  command = plot_tamanho_FA(dado_id_logradouros, dado_logradouros, dado_faixa_azul, dado_trechos_bruto)),
tar_target(
  name = descritiva_hora_sinistro,
  command = plot_hora_sinistro(dado_sinistros)),
tar_target(
  name = descritiva_qualidade_match,
  command = plot_qualidade_match(dado_sinistros, dado_match_bind)),
tar_target(
  name = descritiva_agregados,
  command = plot_agregacao_trechos(dado_trechos_bruto, dado_id_logradouros, dado_agregados)),
tar_target(
  name = descritiva_mapas,
  command = plot_mapas(dado_sinistros, dado_trechos_bruto, dado_faixa_azul)),
tar_target(
  name = descritiva_obitos_ano,
  command = plot_obitos_ano(dado_sinistros, dado_vitimas)),
tar_target(
  name = descritiva_obitos_ano_FA,
  command = plot_obitos_tempo(dado_sinistros, dado_vitimas, dado_match, dado_agregados)),
tar_target(
  name = descritiva_prepos,
  command = plot_antes_depois(dado_sinistros, dado_vitimas, dado_agregados, dado_match)),
  
  # 7. DID ----
  
  # Tabela cérebro com as configurações para os DIDs
  tar_target(did_tabela_file, "dados_tratados/did_tabela.csv", format = "file"),
  tar_target(did_tabela, did_tabela_file |> read_csv() |> limpar_tabela_did()),
  
  # 7.1. Base de segmentos ----
  tar_target(did_tabela_segmentos, 
             did_tabela |> select(segmento_nivel, 
                                  filtro_segmentos,
                                  rodarPSM,
                                  PSM_corte_minimo)),
  
  # Criação das tabelas de segmento por nível
  tar_target(name = did_segmento_combinado, 
             dplyr::bind_rows(dado_trechos |> dplyr::mutate(segmento = "trechos"), 
                       dado_agregados |> dplyr::mutate(segmento = "agregados"), 
                       dado_logradouros |> dplyr::mutate(segmento = "logradouros") |> select(-nome))),
  tar_target(name = did_segmento_nivel,
             command = segmento_nivel(
               segmentos = did_segmento_combinado, 
               nivel = did_tabela_segmentos$segmento_nivel),
             pattern = map(did_tabela_segmentos)),
  
  # Realizando filtros/efeitos heterogeneos em segmentos
  tar_target(name = did_segmento_filtrado,
             command = segmento_filtro(
               segmentos = did_segmento_nivel, 
               filtro = did_tabela_segmentos$filtro_segmentos),
             pattern = map(did_segmento_nivel, did_tabela_segmentos)),
  
  # Roda PSM, quando necessário
  tar_target(name = did_segmento_PSM,
             command = segmento_psm(
               segmentos = did_segmento_filtrado, 
               sinistros = dado_sinistros, 
               match = dado_match, 
               rodarPSM = did_tabela_segmentos$rodarPSM, 
               min_score_cut = did_tabela_segmentos$PSM_corte_minimo),
             pattern = map(did_segmento_filtrado, did_tabela_segmentos)),
  
  # 7.2. Base de sinistros ----
  tar_target(did_tabela_sinistros, 
             did_tabela |> select(filtro_sinistros, 
                                  apenas_moto)),
  
  # Realizando filtros/efeitos heterogeneos em sinistros
  tar_target(name = did_sinistro_filtrado,
             command = sinistro_filtro(
               sinistros = dado_sinistros, 
               vitimas = dado_mortes,
               filtro = did_tabela_sinistros$filtro_sinistros,
               apenas_moto = did_tabela_sinistros$apenas_moto),
             pattern = map(did_tabela_sinistros)),
  
  # 7.3. Base agregada ----
  tar_target(did_tabela_agrega, 
             did_tabela |> select(intervalo_meses,
                                  filtrar_golden,
                                  PSM_corte_minimo)),
  tar_target(did_periodos_datetime, 
             criar_periodos_datetime()),
  tar_target(name = did_df,
             command = agrega_tempo(
               segmentos_filtrado = did_segmento_PSM,
               sinistros_filtrado = did_sinistro_filtrado,
               match = dado_match,
               tabela_periodos_datetime = did_periodos_datetime,
               intervalo_meses = did_tabela_agrega$intervalo_meses,
               filtrar_golden = did_tabela_agrega$filtrar_golden),
             pattern = map(did_segmento_PSM, did_sinistro_filtrado, did_tabela_agrega)),
  
  # 7.4. Fit ----
  tar_target(did_tabela_fit, 
             did_tabela |> select(por_km, 
                                  log_delta,
                                  variavel_y,
                                  grupo_controle,
                                  pesos,
                                  remover_controle)),
  # Fit
  tar_target(name = did_fit,
             command = fit_did(
               df = did_df,
               por_km = did_tabela_fit$por_km, 
               log_delta = did_tabela_fit$log_delta,
               yname = did_tabela_fit$variavel_y,
               control_group = did_tabela_fit$grupo_controle,
               weightsname = did_tabela_fit$pesos,
               remover_formula = did_tabela_fit$remover_controle),
             pattern = map(did_df, did_tabela_fit),
             iteration = "list"),
  
  # 7.5. Plot e resultados----
  tar_target(did_tabela_plot, 
             did_tabela |> select(expand_grid,
                                  file,
                                  intervalo_meses,
                                  title)),
  # Tabelona de resultados
  tar_target(name = did_summary_tabelinha,
             command = summary_tabelinha_did(
               did = did_fit,
               nome = did_tabela_plot$file,
               intervalo_meses =  did_tabela_plot$intervalo_meses),
             pattern = map(did_fit, did_tabela_plot),
               iteration = "list"),

  tar_target(name = did_summary_tabela,
             command = bind_rows(did_summary_tabelinha)),
  
  tar_target(name = did_summary_tabela_save,
             command = save_tabela_agregada(did_summary_tabela)),

  # Plot
  tar_target(name = did_plot,
             command = plot_did(
               did = did_fit,
               intervalo_meses =  did_tabela_plot$intervalo_meses,
               tabela_summary = did_summary_tabelinha,
               expand_grid = did_tabela_plot$expand_grid,
               file = did_tabela_plot$file,
               title = did_tabela_plot$title),
             pattern = map(did_fit, did_tabela_plot, did_summary_tabelinha),
             iteration = "list")
  



  
  
)
