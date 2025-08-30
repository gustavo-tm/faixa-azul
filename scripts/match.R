# PREPARAÇÃO DADOS ----

limpar <- function(coluna){
  coluna |> 
    # Removing Unwanted Characters and Tokens
    replace_na(" ") |> 
    stringi::stri_trans_general("latin-ascii") |> 
    str_to_upper() |> 
    str_replace_all("[[:punct:]]", " ") |> 
    str_squish() |> 
    (\(col) str_glue(" {col} "))(col = _) |> 
    as.character()
} 
padronizar <- function(coluna, titulo, preposicao, tipo, numero){
  coluna |> 
    limpar() |> 
    # Standardisation
    str_replace_all(setNames(rep(" ", length(preposicao$preposicao)), limpar(preposicao$preposicao))) |> #remover preposicao
    str_replace_all("(BR|SP)\\s(\\d+)", "\\1\\2") |> #correção estradas SP 015 -> SP015
    str_remove(" KM .*") |>  #correção quando o KM está escrito no nome (Marginal km 15)
    str_remove(" X .*") |>  #correção quando é uma intersecção e se coloca o nome da outra via (sto amaro x quatá)
    str_replace_all(setNames(tipo$tipo, tipo$tipo_abrev)) |> #correcao tipo via (av -> avenida)
    str_replace_all(setNames(titulo$titulo_ext, titulo$titulo)) |> #correcao titulo (BRIG -> BRIGADEIRO)
    str_replace_all(setNames(numero$numero_ext, numero$numero)) |> #correcao numero (23 de maio -> vinte e tres de maio)
    str_remove_all(" \\d+ ") |> #remover números maiores de 300 (certamente não fazem parte do nome)
    limpar()
}
tokenizar <- function(df, id){
  id <- sym(id)
  
  preposicao <- read_csv("dados_tratados/logradouros/preposicao.csv") |> 
    mutate(across(everything(), ~ limpar(.x)))
  numero <- read_csv("dados_tratados/logradouros/numero.csv") |> 
    mutate(across(everything(), ~ limpar(.x)))
  tipo <- read_csv("dados_tratados/logradouros/tipo.csv") |> 
    mutate(across(everything(), ~ limpar(.x)))
  titulo <- read_csv("dados_tratados/logradouros/titulo.csv") |> 
    mutate(across(everything(), ~ limpar(.x)))
  
  df |> 
    #Encontrar tokens e criar uma string de logradouro sem os tokens
    mutate(logradouro_limpo = logradouro |> 
             padronizar(titulo, preposicao, tipo, numero) |> 
             str_replace_all(string = _, setNames(rep(" ", length(titulo$titulo_ext)), titulo$titulo_ext)) |> 
             str_replace_all(string = _, setNames(rep(" ", length(tipo$tipo)), tipo$tipo)) |> 
             limpar()) |> 
    fuzzyjoin::regex_left_join(tipo |> select(tipo), by = join_by(logradouro == tipo)) |> 
    fuzzyjoin::regex_left_join(titulo |> select(titulo = titulo_ext), by = join_by(logradouro == titulo)) |> 
    mutate(across(c(logradouro_limpo, tipo, titulo), ~ replace_na(.x, ""))) |> 
    
    #Corrigir quando aparecem 2 tokens no mesmo campo
    #"ACESSO AVENIDA" apareceria em duas linhas diferentes, mas deveria ser um token só
    group_by(!!id, logradouro, alias) |> 
    summarize(logradouro_limpo = nth(logradouro_limpo, 1),
              tipo = tipo |> str_split(" ") |> unlist() |> unique() |> paste(collapse = " ") |> limpar(),
              titulo = titulo |> str_split(" ") |> unlist() |> unique() |> paste(collapse = " ") |> limpar())
}




tokenizar_infosiga <- function(sinistros){

  infosiga.token <- sinistros |> 
    filter(tipo != "NOTIFICACAO", logradouro != "NAO DISPONIVEL") |> 
    select(id_sinistro, logradouro) |> 
    mutate(alias = "logradouro") |> 
    tokenizar(id = "id_sinistro")
  
  return(infosiga.token)
}


tokenizar_osm <- function(trechos){
  osm.token <- trechos |>  
    st_drop_geometry() |> 
    select(id_osm, starts_with("logradouro")) |>
    mutate(across(starts_with("logradouro"), ~ str_replace_all(.x, ",", ";"))) |> 
    
    #Capturar todos os alias (Marginal = SP015), o mesmo ID OSM pode ter vários nomes
    separate_wider_delim(logradouro_ref, delim = ";", too_few = "align_start", names_sep = "_") |>
    separate_wider_delim(logradouro_alt1, delim = ";", too_few = "align_start", names_sep = "_") |>
    pivot_longer(starts_with("logradouro")) |>
    drop_na() |> 
    rename(logradouro = value, alias = name) |>
    tokenizar(id = "id_osm")
  
  return(osm.token)
}


match_dados_split <- function(sinistros, n){
  sinistros |> 
    filter(tipo != "NOTIFICACAO", logradouro != "NAO DISPONIVEL") |> 
    #Resto da divisão por n vai trazer n grupos de tamanhos iguais 
    mutate(grupo = row_number() %% n) |> 
    group_by(grupo) |> 
    targets::tar_group()
}


# 
# osm.token |> write_csv("dados_tratados/osm-token.csv")
# infosiga.token |> write_csv("dados_tratados/infosiga-token.csv")

match_dados <- function(sinistros, sinistros_token, trechos, trechos_token){
  
  sinistros <- sinistros |> 
    as_tibble() |> 
    filter(tipo != "NOTIFICACAO", logradouro != "NAO DISPONIVEL")
  
  # INDEXING: juntando todos os candidatos no raio de 300 metros
  join <- sinistros |> 
    filter(!is.na(longitude), !is.na(latitude)) |> 
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> 
    st_transform("epsg:31983") |> 
    mutate(geometry = st_buffer(geometry, 300)) |>
    select(id_sinistro, geometry) |> 
    st_join(trechos |> select(id_osm, geometry) |> st_transform("epsg:31983"))
  
  # Match pelo nome mais próximo
  match_nome <- join |> 
    st_drop_geometry() |> 
    left_join(sinistros_token, by = join_by(id_sinistro)) |> 
    left_join(trechos_token, by = join_by(id_osm)) |> 
    mutate(similaridade = stringdist::stringsim(logradouro_limpo.x, logradouro_limpo.y),
           match_tipo = tipo.x == tipo.y,
           match_titulo = titulo.x == titulo.y) |> 
    drop_na() |> 
    group_by(id_sinistro) |> 
    filter(similaridade == max(similaridade)) |> 
    mutate(distancia_nome = stringdist::stringdist(logradouro_limpo.x, logradouro_limpo.y))
  
  # Seleção do trecho mais próximo geograficamente quando há empate de proximidade de nome
  match_grafico <- match_nome |> 
    select(id_sinistro, id_osm) |> 
    left_join(sinistros |> 
                filter(!is.na(longitude), !is.na(latitude)) |>
                st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
                st_transform("epsg:31983") |>
                select(id_sinistro, geometria_ponto = geometry)) |> 
    left_join(trechos |> 
                st_transform("epsg:31983") |>
                select(id_osm, geometria_trecho = geometry)) |> 
    filter(!st_is_empty(geometria_ponto), !st_is_empty(geometria_trecho)) |>
    
    # Encontrar o vizinho mais próximo e depois calcular a distância é significativamente mais rápido do que calcular todas as distâncias e depois pegar a menor, 
    # mas essa parte demora bastante para rodar mesmo (são 8 milhões de comparações)
    group_by(id_sinistro) |> 
    filter(row_number() == st_nearest_feature(nth(geometria_ponto, 1), geometria_trecho)) |> 
    mutate(distancia = st_distance(geometria_ponto, geometria_trecho, by_element = TRUE) |> as.numeric()) |> 
    st_drop_geometry() |> 
    ungroup()
  

  match <- match_grafico |> 
    select(id_sinistro, id_osm, distancia_geografica = distancia) |> 
    left_join(match_nome) |> 
    select(id_sinistro, id_osm, logradouro = logradouro_limpo.y, similaridade, distancia_geografica, distancia_nome, match_tipo, match_titulo) |> 
    
    # Verificação de haver apenas um match por sinistro
    group_by(id_sinistro) |> 
    filter(match_tipo + match_titulo == max(match_tipo + match_titulo), #Selecionar o que tem maior match de título e tipo
           match_titulo == max(match_titulo), #Se um tiver match e título, e o outro match no tipo, manter apenas match no título
           row_number() == 1) #Garantir que sobra apenas uma linha no matter what
  
  match <- match |> 
    left_join(sinistros |> 
                mutate(numero_zero = as.numeric(numero) == 0) |> 
                select(id_sinistro, numero_zero))
  
  
  return(match)
  
  # match |> 
  #   select(id_sinistro, id_osm, logradouro = logradouro_limpo.y, similaridade, distancia_geografica, distancia_nome, match_tipo, match_titulo) |> 
  #   write_csv("banco_dados/match.csv")
}


match_ids <- function(match_bind, trechos, id_agregados, id_logradouros) {
  match <- match_bind |> 
    mutate(numero_zero = replace_na(numero_zero, TRUE)) |> 
    mutate(golden_match =
             similaridade > .85 &
             distancia_geografica < 150 &
             (match_titulo == TRUE | match_tipo == TRUE) & 
             numero_zero == FALSE) |> 
    select(id_sinistro, id_osm, golden_match)
  
  match |> 
    semi_join(trechos |> 
                st_drop_geometry() |> 
                filter(tipo_via %in% c("primary", "secondary", "trunk"))) |> 
    left_join(id_agregados |> 
                unnest(id_osm) |> 
                select(contains("id"))) |> 
    left_join(id_logradouros |> 
                unnest(id_osm = trechos) |> 
                select(contains("id"))) |> 
    select(id_sinistro, id_osm, id_trecho_agregado, id_logradouro, golden_match) |> 
    ungroup()
}



