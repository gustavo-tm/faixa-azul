library(tidyverse)

faixa_azul <- readxl::read_excel("dados_tratados/vias_faixa_azul.xlsx") |> 
  mutate(data = make_date(year = ano, month = mes)) |> 
  group_by(id_trecho, data, ano, mes) |> 
  mutate(logradouro_conjunto = str_flatten(logradouro, collapse = "; ")) |> 
  ungroup()

df <- read_csv("dados_tratados/sinistros_logradouros.csv") |> 
  # Calcular HP
  group_by(logradouro) |> 
  filter(sum(sinistros) > 10) |> # Filtro apenas avenidas com mais de 20 acidentes no total
  mutate(sinistros_hp = mFilter::hpfilter(sinistros, freq = 144)$trend[,1],
         sinistros_moto_hp = mFilter::hpfilter(sinistros_moto, freq = 144)$trend[,1]) |> 
  ungroup() |> 
  
  # Juntar trechos iguais
  left_join(faixa_azul |> select(logradouro, logradouro_conjunto, data_faixa_azul = data)) |> 
  mutate(logradouro = ifelse(is.na(logradouro_conjunto), logradouro, logradouro_conjunto)) |>
  group_by(logradouro, ano, mes, data_faixa_azul) |> 
  summarize(across(c(sinistros, sinistros_moto, sinistros_hp, sinistros_moto_hp), ~ sum(.x))) |> 
  group_by(logradouro) |> 
  mutate(faixa_azul = ifelse(is.na(data_faixa_azul), FALSE, make_date(year = ano, month = mes) > data_faixa_azul)) |> 
  ungroup()
  

## Plot linha do tempo de cada avenida ----
plot.acidentes <- function(logradouro_analise, data_faixa_azul){
  (df |> 
     filter(logradouro == logradouro_analise) |> 
     mutate(data = make_date(year = ano, month = mes))  |>  
     ggplot(aes(x = data)) +
     geom_line(aes(y = sinistros, linetype = "Sinistros")) + 
     geom_line(aes(y = sinistros_hp, linetype = "Sinistros (filtro HP)")) + 
     geom_vline(xintercept = data_faixa_azul, 
                colour = "blue", lwd = 4, alpha = .1) +
     scale_linetype_manual(values = c("Sinistros" = "solid", "Sinistros (filtro HP)" = "dashed")) + 
     labs(x = "Data", y="Quantidade de sinistros", linetype = "") +
     scale_x_date() +
     labs(title = logradouro_analise) +
     theme_classic()) |> 
    ggsave(paste("output/acidentes_tempo/", logradouro_analise, ".pdf", sep = ""),
           plot = _, width = 8, height = 5)
}

faixa_azul |> 
  distinct(logradouro_conjunto, data) |> 
  rowwise() |> 
  mutate(grafico = list(plot.acidentes(logradouro_conjunto, data)))

