# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(visNetwork)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("tidyverse", "sf", "osmdata", "fuzzyjoin", "stringdist", "did", "gt", "circlize", "igraph"), 
  error = "trim",
  # format = "qs", # Optionally set the default storage format. qs is fast.
  #
  # Pipelines that take a long time to run may benefit from
  # optional distributed computing. To use this capability
  # in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller that scales up to a maximum of two workers
  # which run as local R processes. Each worker launches when there is work
  # to do and exits if 60 seconds pass with no tasks to run.
  #
    controller = crew::crew_controller_local(workers = 6)
  #
  # Alternatively, if you want workers to run on a high-performance computing
  # cluster, select a controller from the {crew.cluster} package.
  # For the cloud, see plugin packages like {crew.aws.batch}.
  # The following example is a controller for Sun Grid Engine (SGE).
  # 
  #   controller = crew.cluster::crew_controller_sge(
  #     # Number of workers that the pipeline can scale up to:
  #     workers = 10,
  #     # It is recommended to set an idle time so workers can shut themselves
  #     # down if they are not running tasks.
  #     seconds_idle = 120,
  #     # Many clusters install R as an environment module, and you can load it
  #     # with the script_lines argument. To select a specific verison of R,
  #     # you may need to include a version string, e.g. "module load R/4.3.2".
  #     # Check with your system administrator if you are unsure.
  #     script_lines = "module load R"
  #   )
  #
  # Set other options as needed.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source("scripts/tidy_sinistros.R")
tar_source("scripts/tidy_trechos.R")
tar_source("scripts/trechos_complemento.R")
tar_source("scripts/tidy_faixa_azul.R")
tar_source("scripts/match.R")
tar_source("scripts/did.R")
# tar_source("other_functions.R") # Source other scripts as needed.

assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

# Replace the target list below with your own:
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
    name = dado_match,
    command = match_dados(dado_sinistros, dado_token_infosiga, dado_trechos, dado_token_osm)),
  
  # 6. DID ----
  # 6.1 agrega
  
  tar_target(
    name = dado_did_trecho_mes,
    command = dado_trecho_mes(dado_sinistros, dado_match, dado_trechos)),
  
  tar_target(
    name = dado_did_logradouro_mes,
    command = dado_logradouro_mes(dado_sinistros, dado_match, dado_id_logradouros)),
  
  # 6.2 prepara
  # 6.2.1. trecho
  tar_target(
    name = dado_did_trecho_total,
    command = prepara_trecho_did(dado_did_trecho_mes, dado_trechos, dado_trechos_complemento, dado_faixa_azul, 
                                 filtrar_por = NULL)),
  tar_target(
    name = dado_did_trecho_moto,
    command = prepara_trecho_did(dado_did_trecho_mes, dado_trechos, dado_trechos_complemento, dado_faixa_azul, 
                                 filtrar_por = motocicleta_envolvida)),
  tar_target(
    name = dado_did_trecho_golden,
    command = prepara_trecho_did(dado_did_trecho_mes, dado_trechos, dado_trechos_complemento, dado_faixa_azul, 
                                 filtrar_por = golden_match)),
  tar_target(
    name = dado_did_trecho_moto_golden,
    command = prepara_trecho_did(dado_did_trecho_mes, dado_trechos, dado_trechos_complemento, dado_faixa_azul,
                                 filtrar_por = c(motocicleta_envolvida, golden_match))),
  # 6.2.2. logradouro
  
  tar_target(
    name = dado_did_logradouro_total,
    command = prepara_logradouro_did(dado_did_logradouro_mes, dado_logradouros,
                                 filtrar_por = NULL)),
  tar_target(
    name = dado_did_logradouro_moto,
    command = prepara_logradouro_did(dado_did_logradouro_mes, dado_logradouros,
                                 filtrar_por = motocicleta_envolvida)),
  tar_target(
    name = dado_did_logradouro_golden,
    command = prepara_logradouro_did(dado_did_logradouro_mes, dado_logradouros,
                                 filtrar_por = golden_match)),
  tar_target(
    name = dado_did_logradouro_moto_golden,
    command = prepara_logradouro_did(dado_did_logradouro_mes, dado_logradouros,
                                 filtrar_por = c(motocicleta_envolvida, golden_match))),
  # 6.2.3. cohort
  tar_target(
    name = dado_cohort_trecho,
    command = definir_cohort(dado_did_trecho_total, 
                             faixa_azul = dado_faixa_azul, 
                             trechos = dado_trechos,
                             logradouros = dado_logradouros,
                             por_logradouro = FALSE)),
  
  tar_target(
    name = dado_cohort_logradouro,
    command = definir_cohort(dado_did_logradouro_total, 
                             logradouros = dado_logradouros,
                             por_logradouro = TRUE)),
  
  # 6.3. roda
  
  # EXEMPLO! Para cada regressão pode ser utilizada a estrutura a seguir
  # É possível fazer de forma mais inteligente talvez se usar dynamic branching, mas não entrei a fundo nisso:
  # https://books.ropensci.org/targets/dynamic.html
  tar_target(
    
    #nome que vai aparecer no tar_visnetword() e salvar na pasta _target/
    #é puramente estético, talvez seja bom inventar algum sistema mais organizado
    name = analise_did1,
    
    #o command rodar_did() ta no script did.R
    command = rodar_did(
      
      #df é a base de dados utilizada no did. 
      #Tem uma para cada categoria: dado_did_A_B
      #opções A = trecho, logradouro
      #opções B = total, moto, golden, moto_golden
      #você pode fazer um pipe aqui mesmo e modificar a base, filtrar ou algo do tipo mas se for mudar muito acho melhor criar outra base
      df = dado_did_trecho_moto_golden,
      
      #base de dados apoio para trazer informações sobre cohorts, como data e número de unidades tratadas
      cohorts = dado_cohort_trecho,
      
      #título da tabela e do gráfico, assim como o nome do arquivo que será salvo em output/did/*
      titulo = "Sinistros por trecho envolvendo motociclistas com match padrão ouro",
      
      #parâmetro para decidir se a regressão é rodada por km ou em número absoluto
      #quando por km, calcula tanto os sinistros por km como as outras variáveis de controle
      #automaticamente atualiza título e nome do arquivo
      por_km = TRUE)),

  tar_target(
    name = analise_did2,
    command = rodar_did(
      df = dado_did_trecho_golden,
      cohorts = dado_cohort_trecho,
      titulo = "Sinistros por trecho com match padrão ouro",
      por_km = FALSE)),
  
  tar_target(
    name = analise_did3,
    command = rodar_did(
      df = dado_did_logradouro_golden,
      cohorts = dado_cohort_logradouro,
      titulo = "Sinistros por logradouro com match padrão ouro",
      por_km = TRUE))
  


)





