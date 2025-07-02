library(tidyverse)
library(sf)
library(memoise)

# Tabela para tornar os datetimes numéricos (por limitação do pacote did)
tabela_periodos_datetime <- tibble(
  data = seq(make_date(year = 2015, month = 1),
             make_date(year = 2030, month = 12),
             by = "1 month")) |> 
  mutate(periodo = row_number())


limpar_tabela_did <- function(did_tabela){
  did_tabela |> 
    mutate(across(c(filtro_sinistros, filtro_segmentos), ~ .x |> as.character() |> replace_na("TRUE")),
           across(c(rodarPSM, filtrar_golden), ~ .x |> as.logical() |> replace_na(TRUE)),
           across(c(PSM_corte_minimo), ~ .x |> as.numeric() |> replace_na(0)),
           across(c(intervalo_meses), ~ .x |> as.numeric() |> replace_na(1)))
}


# Carrega uma base para cada especificação de nível de segmento 
segmento_nivel <- memoise(function(segmentos, nivel){
  segmentos |> 
    filter(segmento == nivel)
})

# Possibilita filtrar a base para fazer efeitos heterogêneos
# Exemplo: "tipo_via == 'primary'"
segmento_filtro <- memoise(function(segmentos, filtro){
  segmentos |> 
    filter(eval(parse(text = filtro))) 
})



# Roda Propensity Score Matching, caso seja necessário
segmento_psm <- memoise(function(segmentos, sinistros, match, rodarPSM = TRUE, min_score_cut = 0){
  
  if(rodarPSM == TRUE){
    # Cálculo sinistros e óbitos antes do tratamento por segmento, para entrar no logit
    medias_pre <- segmentos |>
      mutate(ID = row_number()) |>
      left_join(match) |>
      left_join(sinistros |>
                  filter(year(data) >= 2019,
                         year(data) <= 2021) |>
                  select(id_sinistro, gravidade_fatal)) |>
      filter(golden_match) |>
      mutate(sinistro_fatal = !is.na(gravidade_fatal) & gravidade_fatal > 0) |>
      group_by(ID) |>
      summarise(sinistros_fatais = sum(sinistro_fatal),
                sinistros_total = n()) |>
      right_join(segmentos |>
                   mutate(ID = row_number()) |>
                   select(ID, starts_with("id"))) |>
      select(-ID)

    # Preparação da base para o PSM - criação de factors e arredondamentos para o logit
    df <- segmentos |>
      left_join(medias_pre) |>
      filter(tipo_via %in% c("trunk", "primary", "secondary")) |>
      mutate(faixa_azul = as.integer(!is.na(data_implementacao)),
             across(c(faixas), ~ round(.x) |> as.character()),
             limite_velocidade = ((round(limite_velocidade / 10) * 10) |> as.character()),
             across(c(amenidades, intersec), ~ .x / comprimento),
             across(c(faixas, limite_velocidade, mao_unica), ~ ifelse(is.na(.x), "NA", .x)),
             across(c(sinistros_fatais, sinistros_total), ~ replace_na(.x, 0)))

    # Propensity score matching
    PSM <- df |>
      matchit(faixa_azul ~ trechos + comprimento + faixas + limite_velocidade + amenidades +
                intersec + tipo_via + radar_proximo + sinistros_fatais + sinistros_total,
              data = _,
              method = "nearest",
              distance = "glm", link = "logit")

    # Agrupando resultados do PSM em uma tabela
    resultado <- df |>
      select(starts_with("id"), faixa_azul) |>

      # Quando encontra um vizinho, resultado_match = 1, caso contrário 0 (deve ser removido)
      mutate(resultado_match = PSM$weights,
             propensity_score = PSM$distance) |>

      # Quando for especificado um propensity mínimo, remover os que não passam no critério
      mutate(resultado_match = if_else(propensity_score <= min_score_cut, 0, resultado_match))

    # Devolver a base do segmento filtrada
    segmentos_filtrado <- resultado |>
      filter(resultado_match == 1) |>
      select(starts_with("id")) |>
      left_join(segmentos)
    
    return(segmentos_filtrado)
  } else{return(segmentos)}
})

# Possibilita filtrar a base para fazer efeitos heterogêneos
# Exemplo: "tp_veiculo_motocicleta > 0" 
sinistro_filtro <- memoise(function(sinistros, filtro){
  sinistros |> 
    filter(eval(parse(text = filtro))) |> 
    select(id_sinistro, data, starts_with("gravidade"))
})

# Prepara a base para o did, agrega no nível período/segmento
# Intervalo meses: 1 mensal, 2 bimestral, 3 trimestral...
agrega_tempo <- memoise(function(segmentos_filtrado, sinistros_filtrado, match, 
                         intervalo_meses = 1, filtrar_golden = TRUE){
  
  segmentos <- segmentos_filtrado |>
    pivot_longer(starts_with("id")) |> 
    drop_na(value) |> 
    pivot_wider(id_cols = everything(), names_from = name, values_from = value) |> 
    mutate(ID = row_number())
  
  segmentos |>
    left_join(match) |> 
    left_join(sinistros_filtrado) |> 
    
    # Tornar datetimes mensais (desconsiderar o dia do mês) 
    mutate(data = make_date(year = year(data), month = month(data))) |> 
    left_join(tabela_periodos_datetime) |> 
    mutate(periodo = ((periodo - 1) %/% intervalo_meses + 1)) |> 
    
    
    # Filtrar golden match, e remover os que não tem match
    filter(if(filtrar_golden == TRUE){eval(golden_match == TRUE)}else{TRUE},
           !is.na(id_sinistro),
           year(data) >= 2019) |> 
    
    # Agregar para o DID
    group_by(ID, periodo) |> 
    summarize(sinistros = n(),
              envolvidos_fatal = sum(gravidade_fatal, na.rm = T),
              envolvidos_grave = sum(gravidade_grave, na.rm = T),
              envolvidos_leve = sum(gravidade_leve, na.rm = T),
              envolvidos_ileso = sum(gravidade_ileso, na.rm = T),
              envolvidos_na = sum(gravidade_nao_disponivel, na.rm = T)) |> 
    
    # Painel balanceado e retornar os trechos sempre zero sinistros
    ungroup() |> 
    right_join(segmentos |> select(ID)) |> 
    complete(ID, periodo) |> 
    filter(!is.na(periodo)) |> 
    mutate(across(everything(), ~ replace_na(.x, 0))) |> 
    
    # Recuperar os controles na base final
    left_join(
      segmentos |> 
        
        # Tornar datetimes mensais (desconsiderar o dia do mês) 
        mutate(data_implementacao = make_date(year = year(data_implementacao), 
                                              month = month(data_implementacao))) |> 
        left_join(tabela_periodos_datetime |> 
                    rename(coorte = periodo), 
                  by = join_by(data_implementacao == data)) |> 
        mutate(coorte = ((coorte - 1) %/% intervalo_meses + 1) |> replace_na(0)))
})

# Roda Callaway-Sant'Anna (did staggered)
fit_did <- function(
    # Dataframe
  df, 
  
  # Modificadores
  por_km = FALSE, log_delta = NULL,
  
  # Parâmetros do did
  yname = "sinistros", control_group = "nevertreated", 
  weightsname = NULL, remover_formula = NA
){
  
  
  # Modificar fórmula controles, caso necessário
  # Exemplo: "intersec, faixas"
  formula <- ~ comprimento + tipo_via + faixas + limite_velocidade + amenidades + intersec + radar_proximo
  if (!is.na(remover_formula)){
    formula <- update(formula, paste(" .~. -", paste(remover_formula |> str_split_1(","), collapse = "-")))
  }
  
  # Calcular variáveis de resposta e controle por km
  if(por_km){
    df <- df |> 
      mutate(across(c(matches("envolvidos"), sinistros, intersec, amenidades), 
                    ~ .x * 1000 / comprimento))
  }
  
  # Calcular variáveis de resposta em log
  # Como tem zeros, necessário somar constante (delta)
  if (!is.null(log_delta)) {
    df <- df |> 
      mutate(across(c(matches("envolvidos"), sinistros), 
                    ~ log(.x + log_delta)))
  }
  
  fit <- att_gt(
    yname = yname,
    tname = "periodo",
    idname = "ID",
    gname = "coorte",
    data = df,
    clustervars = c("ID"),
    control_group = control_group,
    xformla = formula,
    base_period = "universal",
    weightsname = weightsname)
  
  return(fit)
}

plot_did <- function(did){
  did |> 
    aggte(type = "dynamic", min_e = -12, max_e = 12) |> 
    ggdid()
}


# Gráfico
# Tabela
# Targets
# Excel



# 
# 
# agregado <- teste |> aggte(type = "dynamic", min_e = -12, max_e = 12) 
# 
# agregado |> 
#   ggdid() + xlim(c(-12, 12)) + ylim(c(-0.5, 0.5))
# 




