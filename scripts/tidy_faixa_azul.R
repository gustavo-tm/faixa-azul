tidy_faixa_azul <- function(trechos){

  faixa_azul <- read_csv("dados_brutos/faixa_azul.csv", col_types = list(id_osm = "c"))
  
  return(faixa_azul)
}

