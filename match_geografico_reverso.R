library(tidyverse)
library(sf)


match_geografico <- read_csv("banco_dados/match_geografico.csv")
sinistros <- read_csv("banco_dados/sinistros.csv")

match_geografico |> 
  filter(as.numeric(distancia_geografica) > 100) |>
  select(id_sinistro) |> 
  left_join(sinistros)
