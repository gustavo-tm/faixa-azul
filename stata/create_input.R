library(targets)
library(tidyverse)

tar_read(did_df, branches = 20) |> 
  write.csv(file = "stata/input/1-moto-padrao.csv")
tar_read(did_df, branches = 20) |> 
  mutate(across(c(sinistros:mortes_pedestre_bike, intersec, amenidades), 
                ~ .x * 1000 / comprimento)) |> 
  write.csv(file = "stata/input/1-moto-padrao-km.csv")
tar_read(did_df, branches = 34) |> 
  write.csv(file = "stata/input/1-moto-padrao-bi.csv")
tar_read(did_df, branches = 34) |> 
  mutate(across(c(sinistros:mortes_pedestre_bike, intersec, amenidades), 
                ~ .x * 1000 / comprimento)) |> 
  write.csv(file = "stata/input/1-moto-padrao-bi-km.csv")

tar_read(did_df, branches = 21) |> 
  write.csv(file = "stata/input/2-moto-pico.csv")
tar_read(did_df, branches = 21) |> 
  mutate(across(c(sinistros:mortes_pedestre_bike, intersec, amenidades), 
                ~ .x * 1000 / comprimento)) |> 
  write.csv(file = "stata/input/2-moto-pico-km.csv")
tar_read(did_df, branches = 153) |> 
  write.csv(file = "stata/input/2-moto-pico-bi.csv")
tar_read(did_df, branches = 153) |> 
  mutate(across(c(sinistros:mortes_pedestre_bike, intersec, amenidades), 
                ~ .x * 1000 / comprimento)) |> 
  write.csv(file = "stata/input/2-moto-pico-bi-km.csv")

tar_read(did_df, branches = 23) |> 
  write.csv(file = "stata/input/3-moto-atrop.csv")
tar_read(did_df, branches = 23) |> 
  mutate(across(c(sinistros:mortes_pedestre_bike, intersec, amenidades), 
                ~ .x * 1000 / comprimento)) |> 
  write.csv(file = "stata/input/3-moto-atrop-km.csv")
tar_read(did_df, branches = 155) |> 
  write.csv(file = "stata/input/3-moto-atrop-bi.csv")
tar_read(did_df, branches = 155) |> 
  mutate(across(c(sinistros:mortes_pedestre_bike, intersec, amenidades), 
                ~ .x * 1000 / comprimento)) |> 
  write.csv(file = "stata/input/3-moto-atrop-bi-km.csv")

tar_read(did_df, branches = 27) |> 
  write.csv(file = "stata/input/4-moto-inter.csv")
tar_read(did_df, branches = 27) |> 
  mutate(across(c(sinistros:mortes_pedestre_bike, intersec, amenidades), 
                ~ .x * 1000 / comprimento)) |> 
  write.csv(file = "stata/input/4-moto-inter-km.csv")
tar_read(did_df, branches = 157) |> 
  write.csv(file = "stata/input/4-moto-inter-bi.csv")
tar_read(did_df, branches = 157) |> 
  mutate(across(c(sinistros:mortes_pedestre_bike, intersec, amenidades), 
                ~ .x * 1000 / comprimento)) |> 
  write.csv(file = "stata/input/4-moto-inter-bi-km.csv")




tar_read(did_df, branches = 1) |> 
  write.csv(file = "stata/input/5-total-padrao.csv")
tar_read(did_df, branches = 1) |> 
  mutate(across(c(sinistros:mortes_pedestre_bike, intersec, amenidades), 
                ~ .x * 1000 / comprimento)) |> 
  write.csv(file = "stata/input/5-total-padrao-km.csv")
tar_read(did_df, branches = 15) |> 
  write.csv(file = "stata/input/5-total-padrao-bi.csv")
tar_read(did_df, branches = 15) |> 
  mutate(across(c(sinistros:mortes_pedestre_bike, intersec, amenidades), 
                ~ .x * 1000 / comprimento)) |> 
  write.csv(file = "stata/input/5-total-padrao-bi-km.csv")

tar_read(did_df, branches = 2) |> 
  write.csv(file = "stata/input/6-total-pico.csv")
tar_read(did_df, branches = 2) |> 
  mutate(across(c(sinistros:mortes_pedestre_bike, intersec, amenidades), 
                ~ .x * 1000 / comprimento)) |> 
  write.csv(file = "stata/input/6-total-pico-km.csv")
tar_read(did_df, branches = 159) |> 
  write.csv(file = "stata/input/6-total-pico-bi.csv")
tar_read(did_df, branches = 159) |> 
  mutate(across(c(sinistros:mortes_pedestre_bike, intersec, amenidades), 
                ~ .x * 1000 / comprimento)) |> 
  write.csv(file = "stata/input/6-total-pico-bi-km.csv")

tar_read(did_df, branches = 4) |> 
  write.csv(file = "stata/input/7-total-atropelamento.csv")
tar_read(did_df, branches = 4) |> 
  mutate(across(c(sinistros:mortes_pedestre_bike, intersec, amenidades), 
                ~ .x * 1000 / comprimento)) |> 
  write.csv(file = "stata/input/7-total-atropelamento-km.csv")
tar_read(did_df, branches = 161) |> 
  write.csv(file = "stata/input/7-total-atropelamento-bi.csv")
tar_read(did_df, branches = 161) |> 
  mutate(across(c(sinistros:mortes_pedestre_bike, intersec, amenidades), 
                ~ .x * 1000 / comprimento)) |> 
  write.csv(file = "stata/input/7-total-atropelamento-bi-km.csv")

tar_read(did_df, branches = 8) |> 
  write.csv(file = "stata/input/8-total-intersec.csv")
tar_read(did_df, branches = 8) |> 
  mutate(across(c(sinistros:mortes_pedestre_bike, intersec, amenidades), 
                ~ .x * 1000 / comprimento)) |> 
  write.csv(file = "stata/input/8-total-intersec-km.csv")
tar_read(did_df, branches = 163) |> 
  write.csv(file = "stata/input/8-total-intersec-bi.csv")
tar_read(did_df, branches = 163) |> 
  mutate(across(c(sinistros:mortes_pedestre_bike, intersec, amenidades), 
                ~ .x * 1000 / comprimento)) |> 
  write.csv(file = "stata/input/8-total-intersec-bi-km.csv")
