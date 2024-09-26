library(tidyverse)

df <- read_csv("dados_tratados/logradouros.csv")
faixa_azul <- readxl::read_excel("dados_tratados/vias_faixa_azul.xlsx")


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
