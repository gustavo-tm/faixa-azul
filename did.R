library(tidyverse)
library(did)

logradouros.sinistros <- read_csv("dados_tratados/infosiga_logradouros.csv")
logradouros.OSM <- read_csv("dados_tratados/osm_logradouros.csv")
logradouros.faixa <- readxl::read_excel("dados_tratados/faixa_azul_vias.xlsx")


data <- logradouros.sinistros |> 
  distinct(ano, mes) |> 
  arrange(ano, mes) |> 
  mutate(periodo = row_number())

df <- logradouros.sinistros |> 
  semi_join(logradouros.OSM |> 
              filter(tipo_via %in% c("trunk", "primary", "secondary"))) |> 
  left_join(logradouros.OSM) |> 
  left_join(data) |> 
  left_join(logradouros.faixa |> 
              left_join(data) |> 
              select(logradouro, data_tratamento = periodo)) |>
  filter(!logradouro == "AVENIDA SANTOS DUMONT") |> 
  mutate(data_tratamento = replace_na(data_tratamento, 0),
         logradouro = logradouro |> as.factor() |> as.numeric(),
         logtamanho = log(tamanho))


# todos sinistros, sem controle ----
did <- att_gt(yname = "sinistros",
              tname = "periodo",
              idname = "logradouro",
              gname = "data_tratamento",
              data = df,
              xformla = ~ 1,
              pl = TRUE, cores = 10)
did |> 
  aggte() |> 
  summary()

did |> 
  aggte(type = "dynamic", na.rm = T) |> 
  ggdid() +
  scale_x_continuous("Meses até o tratamento", limits = c(-30, 30), breaks = (0:6-3)*10) +
  theme_minimal() +
  labs(subtitle = "Todos sinistros, sem variáveis de controle")

ggsave("output/did/event-study.pdf", width = 7, height = 5)

did |> 
  ggdid() +
  theme_minimal() +
  scale_x_continuous("Meses da análise", breaks = 0:8*10) +
  labs(subtitle = "Todos sinistros, sem variáveis de controle")

ggsave("output/did/grupos.pdf", width = 8, height = 15)

# sinistros moto, sem controle ----
did.moto <- att_gt(yname = "sinistros_moto",
              tname = "periodo",
              idname = "logradouro",
              gname = "data_tratamento",
              data = df,
              xformla = ~ 1,
              pl = TRUE, cores = 10)

did.moto |> 
  aggte(type = "dynamic", na.rm = T) |> 
  ggdid() +
  scale_x_continuous("Meses até o tratamento", limits = c(-30, 30), breaks = (0:6-3)*10) +
  theme_minimal() +
  labs(subtitle = "Somente sinistros com motocicletas; sem variáveis de controle")

ggsave("output/did/event-study-moto.pdf", width = 7, height = 5)

did.moto |> 
  ggdid() +
  theme_minimal() +
  scale_x_continuous("Meses da análise", breaks = 0:8*10) +
  labs(subtitle = "Somente sinistros com motocicletas; sem variáveis de controle")

ggsave("output/did/grupos-moto.pdf", width = 8, height = 15)

# sinistros moto, com controle ----

did.controle <- att_gt(yname = "sinistros_moto",
                       tname = "periodo",
                       idname = "logradouro",
                       gname = "data_tratamento",
                       data = df,
                       xformla = ~ faixas + limite_velocidade + tipo_via + logtamanho)

did.controle |> 
  aggte(type = "dynamic", na.rm = T) |> 
  ggdid() +
  scale_x_continuous("Meses até o tratamento", limits = c(-30, 30), breaks = (0:6-3)*10) +
  theme_minimal() +
  labs(subtitle = "Somente sinistros com motocicletas, com variáveis de controle")

ggsave("output/did/event-study-moto-controle.pdf", width = 7, height = 5)

did.controle |> 
  ggdid() +
  theme_minimal() +
  scale_x_continuous("Meses da análise", breaks = 0:8*10) +
  labs(subtitle = "Com variáveis de controle; somente sinistros com motocicletas")

ggsave("output/did/grupos-moto-controle.pdf", width = 8, height = 15)

# diff sinistros, sem controle ----

did.diff <- att_gt(yname = "diff",
                       tname = "periodo",
                       idname = "logradouro",
                       gname = "data_tratamento",
                       data = df |> mutate(diff = sinistros - sinistros_moto),
                       xformla = ~ 1)

did.diff |> 
  aggte(type = "dynamic") |> 
  ggdid() +
  scale_x_continuous("Meses até o tratamento", limits = c(-30, 30), breaks = (0:6-3)*10) +
  theme_minimal() +
  labs(subtitle = "Diferença entre sinistros sem e com motocicletas, sem variáveis de controle")

ggsave("output/did/event-study-diff.pdf", width = 7, height = 5)

did.diff |> 
  ggdid() +
  theme_minimal() +
  scale_x_continuous("Meses da análise", breaks = 0:8*10) +
  labs(subtitle = "Diferença entre sinistros sem e com motocicletas, sem variáveis de controle")

ggsave("output/did/grupos-diff.pdf", width = 8, height = 15)
