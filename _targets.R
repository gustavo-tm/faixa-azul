library(targets)
library(visNetwork)
library(tarchetypes) 

workers <- 2

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

  controller = crew::crew_controller_local(workers = workers)
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source("scripts/tidy_sinistros.R")
tar_source("scripts/tidy_trechos.R")
tar_source("scripts/trechos_complemento.R")
tar_source("scripts/tidy_faixa_azul.R")
tar_source("scripts/match.R")
# tar_source("scripts/descritivas.R")
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

  # # 6. DESCRITIVAS ----
  # tar_target(
  #   name = descritiva_datas_FA,
  #   command = plot_datas_FA(dado_logradouros, dado_id_logradouros, dado_match, dado_sinistros)),
  # tar_target(
  #   name = descritiva_datas_trechos,
  #   command = plot_datas_trechos(dado_faixa_azul, dado_trechos_bruto, dado_token_osm)),
  # tar_target(
  #   name = descritiva_obitos_tempo,
  #   command = plot_obitos_tempo(dado_sinistros, dado_match, dado_faixa_azul, dado_logradouros, dado_id_logradouros)),
  # tar_target(
  #   name = descritiva_tamanho_FA,
  #   command = plot_tamanho_FA(dado_id_logradouros, dado_logradouros, dado_faixa_azul, dado_trechos_bruto)),
  # tar_target(
  #   name = descritiva_hora_sinistro,
  #   command = plot_hora_sinistro(dado_sinistros)),
  # tar_target(
  #   name = descritiva_qualidade_match,
  #   command = plot_qualidade_match(dado_sinistros, dado_match)),
  # tar_target(
  #   name = descritiva_agregados,
  #   command = plot_agregacao_trechos(dado_trechos_bruto, dado_id_logradouros, dado_agregados)),
  # tar_target(
  #   name = descritiva_mapas,
  #   command = plot_mapas(dado_sinistros, dado_trechos_bruto, dado_faixa_azul)),
  # tar_target(
  #   name = descritiva_obitos_ano,
  #   command = plot_obitos_ano(dado_sinistros)),
  # tar_target(
  #   name = descritiva_proporcao_grupos,
  #   command = plot_proporcao_grupos(dado_trechos_bruto, dado_faixa_azul)),
  # tar_target(
  #   name = descritiva_tratados_periodo,
  #   command = plot_trechos_vias_periodo(dado_faixa_azul, dado_logradouros)),
  # tar_target(
  #   name = descritiva_comprimento_trechos,
  #   command = plot_comprimento_trechos(dado_trechos_bruto)),
  # tar_target(
  #   name = descritiva_sinistros_comprimento,
  #   command = plot_sinistros_comprimento(dado_did_trecho_golden)),
  # tar_target(
  #   name = descritiva_staggered_descritivo,
  #   command = plot_staggered_descritivo(dado_sinistros, dado_match, dado_faixa_azul)),
  
  
  # 7. DID ----
  
  # Tabela cérebro com as configurações para os DIDs
  tar_target(did_tabela_file, "dados_tratados/did_tabela.csv", format = "file"),
  tar_target(did_tabela, did_tabela_file |> read_csv() |> limpar_tabela_did()),
  
  # 7.1. Base de segmentos ----
  # Criação das tabelas de segmento por nível
  tar_target(name = did_segmento_combinado, 
             bind_rows(dado_trechos |> mutate(segmento = "trechos"), 
                       dado_agregados |> mutate(segmento = "agregados"), 
                       dado_logradouros |> mutate(segmento = "logradouros"))),
  tar_target(name = did_segmento_nivel,
             command = segmento_nivel(
               segmentos = did_segmento_combinado, 
               nivel = did_tabela$segmento_nivel),
             pattern = map(did_tabela)),
  
  # Realizando filtros/efeitos heterogeneos em segmentos
  tar_target(name = did_segmento_filtrado,
             command = segmento_filtro(
               segmentos = did_segmento_nivel, 
               filtro = did_tabela$filtro_segmentos),
             pattern = map(did_segmento_nivel, did_tabela)),
  
  # Roda PSM, quando necessário
  tar_target(name = did_segmento_PSM,
             command = segmento_psm(
               segmentos = did_segmento_filtrado, 
               sinistros = dado_sinistros, 
               match = dado_match, 
               rodarPSM = did_tabela$rodarPSM, 
               min_score_cut = did_tabela$PSM_corte_minimo),
             pattern = map(did_segmento_filtrado, did_tabela)),
  
  # 7.2. Base de sinistros ----
  # Realizando filtros/efeitos heterogeneos em sinistros
  tar_target(name = did_sinistro_filtrado,
             command = sinistro_filtro(
               sinistros = dado_sinistros, 
               filtro = did_tabela$filtro_sinistros),
             pattern = map(did_tabela)),
  
  # 7.2. Base agregada ----
  tar_target(name = did_df,
             command = agrega_tempo(
               segmentos_filtrado = did_segmento_PSM,
               sinistros_filtrado = did_sinistro_filtrado,
               match = dado_match,
               intervalo_meses = did_tabela$intervalo_meses,
               filtrar_golden = did_tabela$filtrar_golden),
             pattern = map(did_segmento_PSM, did_sinistro_filtrado, did_tabela)),
  
  # 7.3. DID
  # Fit
  tar_target(name = did_fit,
             command = fit_did(
               df = did_df),
             pattern = map(did_df, did_tabela),
             iteration = "list"),
  # Plot
  tar_target(name = did_plot,
             command = plot_did(
               did = did_fit),
             pattern = map(did_fit),
             iteration = "list")
  
  
)
