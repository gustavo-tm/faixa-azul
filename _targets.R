library(targets)
library(visNetwork)
# library(tarchetypes) # Load other packages as needed.

workers <- 8

# para garantir que osmdata vai funcionar
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

# PARA DEBUGAR
# tar_meta(fields = error, complete_only = TRUE) |> View()
# tar_progress()
# tar_visnetwork()
# tar_delete()

tar_option_set(
  packages = c("tidyverse", "sf", "osmdata", "fuzzyjoin", "stringdist", "did", "gt", "circlize", "igraph"), 
  error = "trim",
  # format = "qs", # Optionally set the default storage format. qs is fast.

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
    name = descritiva_obitos_tempo,
    command = plot_obitos_tempo(dado_sinistros, dado_match, dado_faixa_azul, dado_logradouros, dado_id_logradouros)),
  tar_target(
    name = descritiva_tamanho_FA,
    command = plot_tamanho_FA(dado_id_logradouros, dado_logradouros, dado_faixa_azul, dado_trechos)),
  tar_target(
    name = descritiva_hora_sinistro,
    command = plot_hora_sinistro(dado_sinistros)),
  
  
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
    name = dado_did_trecho_todos,
    command = prepara_trecho_did(dado_did_trecho_mes, dado_trechos, dado_trechos_complemento, dado_faixa_azul,
                                 filtrar_por = NULL)),
  tar_target(
    name = dado_did_trecho_moto,
    command = prepara_trecho_did(dado_did_trecho_mes, dado_trechos, dado_trechos_complemento, dado_faixa_azul,
                                 filtrar_por = motocicleta_envolvida)),
  tar_target(
    name = dado_did_trecho_todos_golden,
    command = prepara_trecho_did(dado_did_trecho_mes, dado_trechos, dado_trechos_complemento, dado_faixa_azul,
                                 filtrar_por = golden_match)),
  tar_target(
    name = dado_did_trecho_moto_golden,
    command = prepara_trecho_did(dado_did_trecho_mes, dado_trechos, dado_trechos_complemento, dado_faixa_azul,
                                 filtrar_por = c(motocicleta_envolvida, golden_match))),
  # 7.2.2. logradouro
  tar_target(
    name = dado_did_logradouro_todos,
    command = prepara_logradouro_did(dado_did_logradouro_mes, dado_logradouros,
                                 filtrar_por = NULL)),
  tar_target(
    name = dado_did_logradouro_moto,
    command = prepara_logradouro_did(dado_did_logradouro_mes, dado_logradouros,
                                 filtrar_por = motocicleta_envolvida)),
  tar_target(
    name = dado_did_logradouro_todos_golden,
    command = prepara_logradouro_did(dado_did_logradouro_mes, dado_logradouros,
                                 filtrar_por = golden_match)),
  tar_target(
    name = dado_did_logradouro_moto_golden,
    command = prepara_logradouro_did(dado_did_logradouro_mes, dado_logradouros,
                                 filtrar_por = c(motocicleta_envolvida, golden_match))),
  # 7.2.3. cohort
  tar_target(
    name = dado_cohort_trecho,
    command = definir_cohort(dado_did_trecho_todos,
                             faixa_azul = dado_faixa_azul,
                             trechos = dado_trechos,
                             logradouros = dado_logradouros,
                             por_logradouro = FALSE)),
  tar_target(
    name = dado_cohort_logradouro,
    command = definir_cohort(dado_did_logradouro_todos,
                             logradouros = dado_logradouros,
                             por_logradouro = TRUE)),

  # 7.3. roda
  
  tar_target(
    name = did_todos_total,
    command = fit_did(
      df = dado_did_trecho_todos,
      cohorts = dado_cohort_trecho,
      titulo = "Todos os sinistros",
      filename = "trecho/todos",
      por_km = FALSE)),
  
  tar_target(
    name = did_todos_km,
    command = fit_did(
      df = dado_did_trecho_todos,
      cohorts = dado_cohort_trecho,
      titulo = "Todos os sinistros",
      filename = "trecho/todos",
      por_km = TRUE)),
  
  tar_target(
    name = did_moto_total,
    command = fit_did(
      df = dado_did_trecho_moto,
      cohorts = dado_cohort_trecho,
      titulo = "Sinistros de moto",
      filename = "trecho/moto",
      por_km = FALSE)),
  
  tar_target(
    name = did_moto_km,
    command = fit_did(
      df = dado_did_trecho_moto,
      cohorts = dado_cohort_trecho,
      titulo = "Sinistros de moto",
      filename = "trecho/moto",
      por_km = TRUE)),
  
  tar_target(
    name = did_todos_golden_total,
    command = fit_did(
      df = dado_did_trecho_todos_golden,
      cohorts = dado_cohort_trecho,
      titulo = "Todos os sinistros, golden",
      filename = "trecho/todos-golden",
      por_km = FALSE)),
  
  tar_target(
    name = did_todos_golden_km,
    command = fit_did(
      df = dado_did_trecho_todos_golden,
      cohorts = dado_cohort_trecho,
      titulo = "Todos os sinistros, golden",
      filename = "trecho/todos-golden",
      por_km = TRUE)),
  
  tar_target(
    name = did_moto_golden_total,
    command = fit_did(
      df = dado_did_trecho_moto_golden,
      cohorts = dado_cohort_trecho,
      titulo = "Sinistros de moto, golden",
      filename = "trecho/moto-golden",
      por_km = FALSE)),
  
  tar_target(
    name = did_moto_golden_km,
    command = fit_did(
      df = dado_did_trecho_moto_golden,
      cohorts = dado_cohort_trecho,
      titulo = "Sinistros de moto, golden",
      filename = "trecho/moto-golden",
      por_km = TRUE)),
  
  
  # 7.3.1 efeitos heterogeneos
  tar_target(
    name = did_2201_todos_golden,
    command = did_by_group(
      df = dado_did_trecho_todos_golden,
      cohorts = dado_cohort_trecho,
      group = 37,
      titulo = "Todos os sinistros, golden, 2022-01",
      filename = "trecho/Grupo/202201-todos-golden")),
  
  tar_target(
    name = did_2201_moto_golden,
    command = did_by_group(
      df = dado_did_trecho_moto_golden,
      cohorts = dado_cohort_trecho,
      group = 37,
      titulo = "Sinistros de moto, golden, 2022-01",
      filename = "trecho/Grupo/202201-moto-golden")),
  
  tar_target(
    name = did_2210_todos_golden,
    command = did_by_group(
      df = dado_did_trecho_todos_golden,
      cohorts = dado_cohort_trecho,
      group = 37,
      titulo = "Todos os sinistros, golden, 2022-10",
      filename = "trecho/Grupo/202210-todos-golden")),

  tar_target(
    name = did_2210_moto_golden,
    command = did_by_group(
      df = dado_did_trecho_moto_golden,
      cohorts = dado_cohort_trecho,
      group = 37,
      titulo = "Sinistros de moto, golden, 2022-10",
      filename = "trecho/Grupo/202210-moto-golden")),
  
  
  tar_target(
    name = did_gravidade_leve_todos_golden,
    command = did_yname(
      df = dado_did_trecho_todos_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistro_leve",
      titulo = "Todos os sinistros, golden, sinistros leves",
      filename = "trecho/Gravidade/leve-todos-golden")),

  tar_target(
    name = did_gravidade_leve_moto_golden,
    command = did_yname(
      df = dado_did_trecho_moto_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistro_leve",
      titulo = "Sinistros de moto, golden, sinistros leves",
      filename = "trecho/Gravidade/leve-moto-golden")),

  tar_target(
    name = did_gravidade_grave_todos_golden,
    command = did_yname(
      df = dado_did_trecho_todos_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistro_grave",
      titulo = "Todos os sinistros, golden, sinistros graves",
      filename = "trecho/Gravidade/grave-todos-golden")),

  tar_target(
    name = did_gravidade_grave_moto_golden,
    command = did_yname(
      df = dado_did_trecho_moto_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistro_grave",
      titulo = "Sinistros de moto, golden, sinistros graves",
      filename = "trecho/Gravidade/grave-moto-golden")),

  tar_target(
    name = did_gravidade_fatal_todos_golden,
    command = did_yname(
      df = dado_did_trecho_todos_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistro_fatal",
      titulo = "Todos os sinistros, golden, sinistros fatais",
      filename = "trecho/Gravidade/fatal-todos-golden")),

  tar_target(
    name = did_gravidade_fatal_moto_golden,
    command = did_yname(
      df = dado_did_trecho_moto_golden,
      cohorts = dado_cohort_trecho,
      yname = "sinistro_fatal",
      titulo = "Sinistros de moto, golden, sinistros fatais",
      filename = "trecho/Gravidade/fatal-moto-golden")),


  tar_target(
    name = did_acidente_colisao_todos_golden,
    command = did_yname(
      df = dado_did_trecho_todos_golden,
      cohorts = dado_cohort_trecho,
      yname = "acidente_colisao",
      titulo = "Todos os sinistros, golden, apenas colisao",
      filename = "trecho/Acidente/colisao-todos-golden")),

  tar_target(
    name = did_acidente_colisao_moto_golden,
    command = did_yname(
      df = dado_did_trecho_moto_golden,
      cohorts = dado_cohort_trecho,
      yname = "acidente_colisao",
      titulo = "Sinistros de moto, golden, apenas colisao",
      filename = "trecho/Acidente/colisao-moto-golden")),

  tar_target(
    name = did_acidente_choque_todos_golden,
    command = did_yname(
      df = dado_did_trecho_todos_golden,
      cohorts = dado_cohort_trecho,
      yname = "acidente_choque",
      titulo = "Todos os sinistros, golden, apenas choque",
      filename = "trecho/Acidente/choque-todos-golden")),

  tar_target(
    name = did_acidente_choque_moto_golden,
    command = did_yname(
      df = dado_did_trecho_moto_golden,
      cohorts = dado_cohort_trecho,
      yname = "acidente_choque",
      titulo = "Sinistros de moto, golden, apenas choque",
      filename = "trecho/Acidente/choque-moto-golden")),

  tar_target(
    name = did_acidente_atropelamento_todos_golden,
    command = did_yname(
      df = dado_did_trecho_todos_golden,
      cohorts = dado_cohort_trecho,
      yname = "acidente_atropelamento",
      titulo = "Todos os sinistros, golden, apenas atropelamento",
      filename = "trecho/Acidente/atropelamento-todos-golden")),

  tar_target(
    name = did_acidente_atropelamento_moto_golden,
    command = did_yname(
      df = dado_did_trecho_moto_golden,
      cohorts = dado_cohort_trecho,
      yname = "acidente_atropelamento",
      titulo = "Sinistros de moto, golden, apenas atropelamento",
      filename = "trecho/Acidente/atropelamento-moto-golden")),

  tar_target(
    name = did_acidente_outros_todos_golden,
    command = did_yname(
      df = dado_did_trecho_todos_golden,
      cohorts = dado_cohort_trecho,
      yname = "acidente_outros",
      titulo = "Todos os sinistros, golden, apenas outros",
      filename = "trecho/Acidente/outros-todos-golden")),

  tar_target(
    name = did_acidente_outros_moto_golden,
    command = did_yname(
      df = dado_did_trecho_moto_golden,
      cohorts = dado_cohort_trecho,
      yname = "acidente_outros",
      titulo = "Sinistros de moto, golden, apenas outros",
      filename = "trecho/Acidente/outros-moto-golden"))

  # tar_target(
  #   name = did_gravidade_leve_moto_golden_km,
  #   command = did_gravidade_leve(
  #     df = dado_did_trecho_moto_golden,
  #     cohorts = dado_cohort_trecho,
  #     titulo = "Sinistros de moto golden por trecho",
  #     filename = "trecho/Gravidade/leve-golden",
  #     por_km = TRUE)),
  #
  # tar_target(
  #   name = did_gravidade_grave_moto_golden_km,
  #   command = did_gravidade_grave(
  #     df = dado_did_trecho_moto_golden,
  #     cohorts = dado_cohort_trecho,
  #     titulo = "Sinistros de moto golden por trecho",
  #     filename = "trecho/Gravidade/grave-golden",
  #     por_km = TRUE)),
  #
  # tar_target(
  #   name = did_gravidade_fatal_moto_golden_km,
  #   command = did_gravidade_fatal(
  #     df = dado_did_trecho_moto_golden,
  #     cohorts = dado_cohort_trecho,
  #     titulo = "Sinistros de moto golden por trecho",
  #     filename = "trecho/Gravidade/fatal-golden",
  #     por_km = TRUE))

  # # EXEMPLO! Para cada regressão pode ser utilizada a estrutura a seguir
  # # É possível fazer de forma mais inteligente talvez se usar dynamic branching, mas não entrei a fundo nisso:
  # # https://books.ropensci.org/targets/dynamic.html
  # tar_target(
  # 
  #   #nome que vai aparecer no tar_visnetword() e salvar na pasta _target/
  #   #é puramente estético, talvez seja bom inventar algum sistema mais organizado
  #   name = analise_did1,
  # 
  #   #o command rodar_did() ta no script did.R
  #   command = rodar_did(
  # 
  #     #df é a base de dados utilizada no did.
  #     #Tem uma para cada categoria: dado_did_A_B
  #     #opções A = trecho, logradouro
  #     #opções B = total, moto, golden, moto_golden
  #     #você pode fazer um pipe aqui mesmo e modificar a base, filtrar ou algo do tipo mas se for mudar muito acho melhor criar outra base
  #     df = dado_did_trecho_moto_golden,
  # 
  #     #base de dados apoio para trazer informações sobre cohorts, como data e número de unidades tratadas
  #     cohorts = dado_cohort_trecho,
  # 
  #     #título da tabela e do gráfico, assim como o nome do arquivo que será salvo em output/did/*
  #     titulo = "Sinistros por trecho envolvendo motociclistas com match padrão ouro",
  # 
  #     #parâmetro para decidir se a regressão é rodada por km ou em número absoluto
  #     #quando por km, calcula tanto os sinistros por km como as outras variáveis de controle
  #     #automaticamente atualiza título e nome do arquivo
  #     por_km = TRUE)),
  # 
  # tar_target(
  #   name = analise_did2,
  #   command = rodar_did(
  #     df = dado_did_trecho_golden,
  #     cohorts = dado_cohort_trecho,
  #     titulo = "Sinistros por trecho com match padrão ouro",
  #     por_km = FALSE)),
  # 
  # tar_target(
  #   name = analise_did3,
  #   command = rodar_did(
  #     df = dado_did_logradouro_golden,
  #     cohorts = dado_cohort_logradouro,
  #     titulo = "Sinistros por logradouro com match padrão ouro",
  #     por_km = TRUE))
  


)





