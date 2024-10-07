library(tidyverse)


df <- read_csv("dados_tratados/logradouros.csv")
faixa_azul <- readxl::read_excel("dados_tratados/vias_faixa_azul.xlsx")

obitos <- read_csv("dados_tratados/obitos.csv") |> 
  mutate(logradouro = case_when(logradouro == "AVENIDA VINTE E TRES DE MAIO" ~ "AVENIDA 23 DE MAIO",
                                logradouro == "RUA MIGUEL YUNES" ~ "AVENIDA MIGUEL YUNES",
                                logradouro == "TUNEL AYRTON SENNA" ~ "ACESSO TUNEL AYRTON SENNA",
                                TRUE ~ logradouro))


df |> 
  right_join(faixa_azul |> select(logradouro, id_logradouro)) |> 
  group_by(id_logradouro, mes, ano) |> 
  summarize(acidentes = sum(acidentes),
            logradouro = first(logradouro)) |> 
  left_join(faixa_azul |> 
              mutate(data_faixa_azul = make_date(year = ano, month = mes)) |> 
              select(logradouro, data_faixa_azul, id_trecho)) |> 
  mutate(data = make_date(year = ano, month = mes),
         faixa_azul =  data > data_faixa_azul) |>
  ggplot(aes(x = data, y = reorder(logradouro, id_trecho), 
             lwd = faixa_azul,
             alpha = faixa_azul)) +
  geom_line() +
  scale_x_date(limits = c(make_date(year = 2021, month = 1), make_date(year = 2024, month = 6))) +
  scale_linewidth_manual(values = c("TRUE" = 2, "FALSE" = 1.5), labels = c("TRUE" = "Implementado", "FALSE" = "Não Há")) +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = .20), labels = c("TRUE" = "Implementado", "FALSE" = "Não Há")) +
  guides(linewidth = guide_legend("Faixa Azul"),
         alpha = guide_legend("Faixa Azul")) +
  labs(title = "Evolução da implementação das faixas azuis em São Paulo", x = "", y = "") +
  theme_minimal() +
  theme(plot.title.position = "plot",
        plot.title = element_text(hjust = 0.5))

ggsave("output/evolucao_faixas.pdf", width = 8, height = 5)

df |> 
  right_join(faixa_azul |> select(logradouro, id_logradouro)) |> 
  group_by(id_logradouro, mes, ano) |> 
  summarize(acidentes = sum(acidentes),
            logradouro = first(logradouro)) |> 
  left_join(faixa_azul |> 
              mutate(data_faixa_azul = make_date(year = ano, month = mes)) |> 
              select(logradouro, data_faixa_azul, id_trecho)) |> 
  mutate(data = make_date(year = ano, month = mes),
         faixa_azul =  data > data_faixa_azul) |>
  ggplot(aes(x = data)) +
  geom_line(aes(lwd = faixa_azul, y = reorder(logradouro, id_trecho), alpha = faixa_azul)) +
  geom_point(data = obitos |>
               semi_join(faixa_azul) |>
               mutate(veiculo = fct_collapse(veiculo,
                                             "Motocicleta" = "MOTOCICLETA",
                                             "Automóvel" = "AUTOMOVEL",
                                             "Ônibus ou caminhão" = c("ONIBUS", "CAMINHAO"),
                                             "Pedestre ou bicicleta" = c("PEDESTRE", "BICICLETA"),
                                             "Não disponível" = "NAO DISPONIVEL")) |> 
               arrange(veiculo) |> 
               select(data, logradouro, veiculo),
             aes(y = logradouro, fill = veiculo, shape = veiculo), size = 1.2, alpha = .8, stroke = .1, colour = "white") +
  scale_x_date(limits = c(make_date(year = 2021, month = 1), make_date(year = 2024, month = 6))) +
  scale_linewidth_manual(values = c("TRUE" = 2, "FALSE" = 1.5), labels = c("TRUE" = "Pós faixa azul", "FALSE" = "Pré faixa azul")) +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = .20), labels = c("TRUE" = "Pós faixa azul", "FALSE" = "Pré faixa azul")) +
  scale_fill_manual(values = c("Motocicleta" = "red",
                                 "Automóvel" = "purple",
                                 "Ônibus ou caminhão" = "purple",
                                 "Pedestre ou bicicleta" = "purple",
                                 "Não disponível" = "darkred")) +
  scale_shape_manual(values = c("Motocicleta" = 21,
                                 "Automóvel" = 22,
                                 "Ônibus ou caminhão" = 23,
                                 "Pedestre ou bicicleta" = 24,
                                 "Não disponível" = 25)) +
  guides(linewidth = guide_legend(""),
         alpha = guide_legend(""),
         fill = guide_legend("Veículo da vítima fatal"),
         shape = guide_legend("Veículo da vítima fatal")) +
  labs(title = "Evolução da implementação das faixas azuis em São Paulo", x = "", y = "") +
  theme_minimal() +
  theme(plot.title.position = "plot",
        plot.title = element_text(hjust = 0.5))

ggsave("output/obitos.pdf", width = 8, height = 5)
