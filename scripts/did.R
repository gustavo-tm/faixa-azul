# Tabela para tornar os datetimes numéricos (por limitação do pacote did)
criar_periodos_datetime <- function(){
  tibble(
    data = seq(make_date(year = 2015, month = 1),
               make_date(year = 2030, month = 12),
               by = "1 month")) |> 
    mutate(periodo = row_number())
}


limpar_tabela_did <- function(did_tabela){
  did_tabela |> 
    mutate(across(c(filtro_sinistros, filtro_segmentos), ~ .x |> as.character() |> replace_na("TRUE")),
           across(c(rodarPSM, filtrar_golden), ~ .x |> as.logical() |> replace_na(TRUE)),
           across(c(apenas_moto, por_km), ~ .x |> as.logical() |> replace_na(FALSE)),
           across(c(PSM_corte_minimo), ~ .x |> as.numeric() |> replace_na(0)),
           across(c(intervalo_meses), ~ .x |> as.numeric() |> replace_na(1)),
           variavel_y = variavel_y |> as.character() |> replace_na("sinistros"),
           grupo_controle = grupo_controle |> as.character() |> replace_na("notyettreated"),
           expand_grid = expand_grid |> as.numeric() |> replace_na(.5))
}


# Carrega uma base para cada especificação de nível de segmento 
segmento_nivel <- memoise::memoise(function(segmentos, nivel){
  segmentos |> 
    filter(segmento == nivel)
})

# Possibilita filtrar a base para fazer efeitos heterogêneos
# Exemplo: "tipo_via == 'primary'"
segmento_filtro <- memoise::memoise(function(segmentos, filtro){
  segmentos |> 
    filter(eval(parse(text = filtro))) 
})



# Roda Propensity Score Matching, caso seja necessário
segmento_psm <- memoise::memoise(function(segmentos, sinistros, match, rodarPSM = TRUE, min_score_cut = 0){
  
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
sinistro_filtro <- memoise::memoise(function(sinistros, vitimas, filtro, apenas_moto = FALSE){
  
  sinistros <- sinistros |> 
    left_join(vitimas)
  
  if(apenas_moto == TRUE){sinistros <- sinistros |> filter(tp_veiculo_motocicleta > 0)}

  sinistros |> 
    filter(eval(parse(text = filtro))) |> 
    select(id_sinistro, data, starts_with("gravidade"), starts_with("mortes_"))
})

# Prepara a base para o did, agrega no nível período/segmento
# Intervalo meses: 1 mensal, 2 bimestral, 3 trimestral...
agrega_tempo <- memoise::memoise(function(segmentos_filtrado, sinistros_filtrado, match, tabela_periodos_datetime,
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
              envolvidos_na = sum(gravidade_nao_disponivel, na.rm = T),
              mortes_moto = sum(mortes_motocicleta, na.rm = T),
              mortes_pedestre_bike = sum(mortes_pedestre_bike, na.rm = T)) |> 
    
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
        mutate(coorte = ((coorte - 1) %/% intervalo_meses + 1) |> replace_na(0))) |> 
    select(-data_implementacao)
})

# Roda Callaway-Sant'Anna (did staggered)
fit_did <- function(
    # Dataframe
  df, 
  
  # Modificadores
  por_km = FALSE, log_delta = NA,
  
  # Parâmetros do did
  yname = "sinistros", control_group = "nevertreated", 
  weightsname = NA, remover_formula = NA
){
  
  df <- df |> 
    rename(y = !!yname)
  
  if (is.na(log_delta)){log_delta <- NULL}
  if (is.na(weightsname)){weightsname <- NULL}
  
  # Modificar fórmula controles, caso necessário
  # Exemplo: "intersec, faixas"
  formula <- ~ comprimento + tipo_via + faixas + limite_velocidade + amenidades + intersec + radar_proximo
  if (!is.na(remover_formula)){
    if(remover_formula == "tudo"){
      formula <- NULL
    }else{
      formula <- update(formula, paste(" .~. -", paste(remover_formula |> str_split_1(","), collapse = "-")))
    }
    
  }
  
  # Calcular variáveis de resposta e controle por km
  if(por_km){
    df <- df |> 
      mutate(across(c(y, intersec, amenidades), 
                    ~ .x * 1000 / comprimento))
  }
  
  # Calcular variáveis de resposta em log
  # Como tem zeros, necessário somar constante (delta)
  if (!is.null(log_delta)) {
    df <- df |> 
      mutate(y = log(y + log_delta))
  }
  
  fit <- att_gt(
    yname = "y",
    tname = "periodo",
    idname = "ID",
    gname = "coorte",
    data = df,
    clustervars = c("ID"),
    control_group = control_group,
    xformla = formula,
    base_period = "universal",
    weightsname = weightsname,
    biters = 5000)
  
  return(fit)
}

plot_did <- function(did, file, tabela_summary, title = NULL, expand_grid = .5, intervalo_meses = 1){
  
  if (is.na(title)){title <- NULL}
  
  # gráfico
  plot <- did |> 
    aggte(type = "dynamic", min_e = -12  / intervalo_meses, max_e = 12 / intervalo_meses, na.rm = TRUE) |> 
    ggdid() +
    scale_y_continuous(expand = expansion(mult = expand_grid)) +
    scale_x_continuous("Meses até data da implementação", breaks = c(0:9-4)*3) +
    scale_colour_manual(values = c("red", "blue"), labels = c("Pré faixa azul", "Pós faixa azul")) +
    labs(title = title) +
    theme_minimal() +
    theme(legend.position = "top")
  
  # tabelas
  
  tabela1 <- tabela_summary |> 
    select(-nome) |> 
    gt() |> 
    fmt_number(decimals = 2) |> 
    cols_align(align= "right") |> 
    cols_width(everything() ~ 600/4)
  
  # tabelinha descritivas da base
  tabela2 <- did$DIDparams$data |> 
    as_tibble() |> 
    summarize(Média = mean(y),
              Mediana = median(y),
              "Desvio Padrão" = sd(y),
              Máximo = max(y)) |> 
    gt() |> 
    fmt_number(decimals = 2) |> 
    cols_width(everything() ~ 600/4)
  
  figura <- (wrap_table(tabela1, space = "fixed") / plot / wrap_table(tabela2, space = "fixed"))
  
  ggsave(paste0("output/did/", file, ".pdf"), figura, 
        width = 7, height = 6, 
        bg = "white",
        create.dir = TRUE)
  
  return(plot)
}

summary_tabelinha_did <- function(did, nome, intervalo_meses = 1){
  out <- aggte(did, type = "simple", min_e = -12 / intervalo_meses, max_e = 12  / intervalo_meses, na.rm = TRUE)
  ATT <- out$overall.att
  se <- out$overall.se
  ci_low = ATT - se * 1.96
  ci_high = ATT + se * 1.96
  significance <- if(ci_low < 0 & ci_high > 0){""}else{"*"}
  
  # tabelinha resultados agregados
  tabelinha <- tibble(
    nome = nome, 
    ATT = format(round(ATT, 3), nsmall = 3), 
    SE = format(round(se, 3), nsmall = 3), 
    "IC (95%)" = paste0("[", format(round(ci_low, 3), nsmall = 3), ", ", format(round(ci_high, 3), nsmall = 3), "]"),
    Significante = if(ci_low < 0 & ci_high > 0){"Não"}else{"Sim"})
  return(tabelinha)
}

save_tabela_agregada <- function(tabela){
  tabela |> 
    mutate(id = row_number()) |> 
    (\(df) bind_rows(
      df, 
      df |> 
        filter(str_detect(nome, "padrao")) |> 
        mutate(id = id - .1,
               across(c(everything(), -id), ~ ""))))() |> 
    arrange(id) |> select(-id) |> 
    kable(format = "latex", booktabs = TRUE, longtable = TRUE, linesep = "", align = c("l", "r", "r", "c", "l"))|> 
    kable_styling(latex_options = c("repeat_header")) |> 
    write(file = "output/did/tabela_agregada.tex")
}

