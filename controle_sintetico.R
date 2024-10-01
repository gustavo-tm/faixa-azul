library(tidyverse)

# Dados ----
faixa_azul <- readxl::read_excel("dados_tratados/vias_faixa_azul.xlsx") |> 
  mutate(data = make_date(year = ano, month = mes, day = ifelse(dia |> is.na(), 1, dia)))

df <- read_csv("dados_tratados/logradouros.csv") |> 
  group_by(logradouro) |> 
  mutate(acidentes_total = sum(acidentes)) |> 
  filter(acidentes_total > 100) |> # Filtro apenas avenidas com mais de 20 acidentes no total
  select(-acidentes_total) |> 
  pivot_longer(4:7, values_to = "total") |> 
  group_by(logradouro, name) |> 
  mutate(hp = mFilter::hpfilter(total, freq = 144)$trend[,1],
         sqrt_hp = mFilter::hpfilter(sqrt(total), freq = 144)$trend[,1]) |> 
  ungroup() |> 
  pivot_wider(id_cols = c(ano, mes, logradouro),
              names_from = name,
              values_from = c(total, sqrt_hp, hp),
              names_glue = "{name}_{.value}") |> 
  mutate(data = make_date(year = ano, month = mes))

## Plot linha do tempo de cada avenida ----
plot.acidentes <- function(logradouro_analise, data_faixa_azul){
  (df |> 
     filter(logradouro == logradouro_analise) |> 
     mutate(data = make_date(year = ano, month = mes))  |>  
     ggplot(aes(x = data)) +
     geom_line(aes(y = acidentes_total, linetype = "Sinistros")) + 
     geom_line(aes(y = acidentes_hp, linetype = "Sinistros (filtro HP)")) + 
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
  rowwise() |> 
  mutate(grafico = list(plot.acidentes(logradouro, data)))

# Controle sintético ----

## Dataprep ----
id_data <- tibble(data = seq.Date(from = df$data |> min() |> as.Date(), 
                                      to = df$data |> max() |> as.Date(), 
                                      by = "month")) |> 
  mutate(ano = year(data),
         mes = month(data),
         id_data = as.factor(data) |> as.numeric())

id_logradouro <- df |> 
  distinct(logradouro) |> 
  mutate(id_logradouro = row_number())

controle.sintetico <- function(logradouro_tratamento, data_tratamento, nsplits = 5, variavel = "acidentes_moto_feridos_sqrt_hp"){
  
  logradouro_tratamento <- id_logradouro |> 
    filter(logradouro == logradouro_tratamento) |> 
    pull(id_logradouro) |> 
    as.numeric()
  
  dataprep <- df |> 
    left_join(id_data) |> 
    left_join(id_logradouro) |> 
    select(id_data, id_logradouro, logradouro, y = variavel) |> 
    as.data.frame()

  data_tratamento <- id_data |> 
    filter(ano == year(data_tratamento), mes == month(data_tratamento)) |> 
    pull(id_data) |> 
    as.numeric()
  
  anos <- data_tratamento %/% 12
  
  # if (anos > 0){
  #   special.predictors <- lapply(0:(anos-1), function(ano) {list("y", 1:12 + 12 * ano, "mean")}) |>
  #     append(list(list("y", (anos*12 + 1):(data_tratamento-1), "mean")))
  # }else{
  #   special.predictors <- list(list("y", 1:(data_tratamento-1), "mean"))
  # }
  
  special.predictors <- lapply(split(1:data_tratamento, cut(1:data_tratamento, nsplits)), function(seq){list("y", seq, "mean")})
  
  dataprep.out <- Synth::dataprep(
    foo = dataprep,
    predictors = c("y"),
    special.predictors = special.predictors,
    dependent = "y",
    predictors.op = "mean",
    unit.variable = "id_logradouro",
    unit.names.variable = "logradouro",
    time.variable = "id_data",
    treatment.identifier = logradouro_tratamento,
    controls.identifier = dataprep |> 
      distinct(id_logradouro) |> 
      pull(id_logradouro) |> 
      _[-logradouro_tratamento],
    time.optimize.ssr = 1:(data_tratamento - 1),
    time.predictors.prior = 1:(data_tratamento - 1),
    time.plot = 1:max(dataprep$id_data)
  )
  
  synth.out <- Synth::synth(data.prep.obj = dataprep.out)
  
  return(data.frame(sintetico = dataprep.out$Y0plot %*% synth.out$solution.w,
                    id_data = dataprep.out$tag$time.plot,
                    band = dataprep.out$Y1plot) %>% 
           as_tibble() |> 
           select(id_data, observado = paste("X", logradouro_tratamento, sep = ""), sintetico = w.weight) |> 
           mutate(tratado = id_data > data_tratamento) |> 
           left_join(id_data)) 
    
}



bandeirantes <- controle.sintetico("AVENIDA DOS BANDEIRANTES", 
                                   make_date(year = 2022, month = 10), 
                                   variavel = "acidentes_moto_feridos_hp")

plot.controle.sintetico <- function(dados_controle_sintetico, logradouro_analise, data_tratamento){
  gg <- dados_controle_sintetico |> 
    mutate(fill = case_when(tratado == FALSE ~ NA,
                            observado > sintetico ~ "red",
                            TRUE ~ "blue")) |>
    ggplot(aes(x = data)) +
    annotate("rect", xmin = data_tratamento, xmax = max(id_data$data), 
             ymin = -Inf, ymax = Inf, fill = "black", alpha =.05) +
    annotate("rect", xmin = data_tratamento, xmax = data_tratamento + months(1), 
             ymin = -Inf, ymax = Inf, fill = "black", alpha =.075) +
    ggbraid::geom_braid(aes(ymin = sintetico, ymax = observado, fill = fill), alpha = .3) +
    geom_line(aes(y = sintetico, linetype = "Sintético")) +
    geom_line(aes(y = observado, linetype = "Observado")) +
    # geom_vline(xintercept = data_tratamento, alpha = 1, linetype = "dotted") +
    scale_linetype_manual(values = c("Sintético" = "dashed", "Observado" = "solid")) + 
    scale_fill_manual(values = c("blue" = "blue", "red" = "red"), guide = "none") +
    labs(x = "Data", y = "Número de sinistros por mês", linetype = "", title = logradouro_analise) +
    ylim(c(0,NA)) +
    theme_classic() +
    theme(legend.position = c(0.25,0.25))
  
    ggsave(paste("output/controle_sintetico/", logradouro_analise, ".pdf", sep = ""),
           plot = gg, width = 8, height = 5)
}

# remotes::install_github("nsgrantham/ggbraid")

faixa_azul |> 
  rowwise() |> 
  mutate(data = make_date(year = ano, month = mes),
         df = list(controle.sintetico(logradouro, data)),
         plot = list(plot.controle.sintetico(df, logradouro, data)))


for (log in faixa_azul |> 
     filter(ano <= 2023) |> 
     group_by(id_logradouro) |> 
     filter(n() == 1) |> 
     pull(logradouro)){
  
  if (log %in% (df |> distinct(logradouro) |> pull(logradouro))){
    data  <- faixa_azul |> filter(logradouro == log) |> pull(data)
    dados <- controle.sintetico(log, data)
    plot  <- plot.controle.sintetico(dados, log, data)
  }
  
}

