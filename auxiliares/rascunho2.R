tar_read(dado_match_bind) |> left_join(tar_read(dado_match)) |> ungroup() |>  count(golden_match) |> mutate(p = n/sum(n))

match |> left_join(match_bind) |> ungroup() |> 
  filter(!golden_match) |> 
  count(numero_zero)|> mutate(p = n/sum(n))

match |> left_join(match_bind) |> ungroup() |> 
  filter(!golden_match) |> 
  ggplot() + 
  geom_histogram(aes(x = similaridade))

match |> left_join(match_bind) |> ungroup() |> 
  filter(!golden_match) |> 
  count(similaridade < .85) |> mutate(p = n/sum(n))

match |> left_join(match_bind) |> ungroup() |> 
  left_join(tar_read(dado_sinistros), by = "id_sinistro") |> 
  filter(!golden_match, similaridade < .85) |> select(starts_with("logradouro")) |> View() 

tar_read(dado_trechos_bruto) |> 
  st_drop_geometry() |> 
  select(id_osm, starts_with("logradouro")) |> 
  pivot_longer(starts_with("logradouro")) |> 
  drop_na() |>
  group_by(id_osm) |> 
  mutate(n = n()) |> 
  arrange(-n, id_osm) |> View()

tar_read(dado_trechos_bruto) |> 
  st_drop_geometry() |> 
  filter(tipo_via %in% c("primary", "secondary", "trunk")) |> 
  select(id_osm, starts_with("logradouro")) |> 
  pivot_longer(starts_with("logradouro")) |> 
  drop_na() |>
  group_by(id_osm) |> 
  summarize(n = n()) |> 
  count(n) |> mutate(p = nn/sum(nn))

tar_read(dado_trechos_bruto) |> 
  st_drop_geometry() |> 
  # filter(tipo_via %in% c("primary", "secondary", "trunk")) |> 
  summarize(quantile(comprimento, c(0.2)))

tar_read(dado_sinistros) |> 
  filter(tipo != "NOTIFICACAO") |> head(100) |> View()
  count(tp_veiculo_motocicleta > 0) |> 
  mutate(p = n/sum(n))

tar_read(dado_sinistros) |> 
  filter(year(data) == 2024) |> 
  select(id_sinistro, starts_with("tp_")) |> 
  pivot_longer(starts_with("tp_veiculo")) |> 
  filter(name != "tp_veiculo_nao_disponivel") |> 
  drop_na() |> 
  mutate(moto = name == "tp_veiculo_motocicleta") |> 
  group_by(id_sinistro) |> 
  summarize(moto = sum(moto)) |> 
  count(moto) |> 
  mutate(percentual = n/sum(n))

data.table::fread("dados_brutos/pessoas_2022-2025.csv", encoding = "Latin-1") |> 
  filter(gravidade_lesao == "FATAL") |> 
  group_by(ano_sinistro) |> 
  count(tipo_veiculo_vitima) |> 
  View()


df <- bind_rows(data.table::fread("dados_brutos/pessoas_2022-2025.csv", encoding = "Latin-1"),
          data.table::fread("dados_brutos/pessoas_2015-2021.csv", encoding = "Latin-1")) |> 
  as_tibble()

vitimas <- bind_rows(data.table::fread("dados_brutos/pessoas_2022-2025.csv", encoding = "Latin-1"),
                     data.table::fread("dados_brutos/pessoas_2015-2021.csv", encoding = "Latin-1")) |> 
  as_tibble() |> 
  select(id_infosiga = id_sinistro, 4:12)

sinistros <- tar_read(dado_sinistros)


vitimas |> 
  left_join(sinistros |> select(id_infosiga, data)) |> 
  filter(gravidade_lesao == "FATAL", year(data) >= 2015, year(data) <= 2024) |> 
  mutate(tipo_veiculo_vitima = str_to_upper(tipo_veiculo_vitima),
         veiculo = fct_collapse(tipo_veiculo_vitima,
                                other_level = "outros",
                                motocicleta = "MOTOCICLETA",
                                nao_disponivel = "NAO DISPONIVEL") |>
           factor(levels = c("outros", "nao_disponivel", "motocicleta"))) |> 
  group_by(data = make_date(year = year(data)), veiculo) |> 
  summarize(obitos = n()) |>
  mutate(y = case_when(veiculo == "motocicleta" ~ obitos, 
                       veiculo == "outros" ~ sum(obitos)),
         label = case_when(veiculo == "motocicleta" ~ scales::percent(obitos / sum(obitos)), 
                           veiculo == "outros" ~ sum(obitos) |> as.character())) |> 
  ggplot(aes(x = data)) +
  geom_col(aes(y = obitos, fill = veiculo), colour = "white") +
  geom_text(aes(y = y, label = label), nudge_y = -30, colour = "white") +
  scale_fill_manual("Modal de transporte\nda vítima", 
                    values = c(adjustcolor("darkblue", blue.f = 1.2, alpha.f = .9), 
                               "grey80", 
                               adjustcolor("darkblue", blue.f = .5, alpha.f = .9)), 
                    labels = c("Outros", "Não disponível", "Motocicleta")) +
  scale_x_date(NULL, date_breaks = "years", date_labels = "%Y") +
  labs(y = "Total de óbitos em decorrência de sinistros fatais") +
  theme_minimal()


gg <- sinistros |> 
  filter(tipo == "SINISTRO FATAL", year(data) %in% 2015:2024) |> 
  group_by(data = make_date(year = year(data)), moto = replace_na(tp_veiculo_motocicleta, 0) > 0) |> 
  summarize(obitos = sum(gravidade_fatal)) |> 
  group_by(data) |> 
  mutate(moto = replace_na(moto, FALSE),
         y = ifelse(moto == TRUE, obitos, sum(obitos)),
         label = ifelse(moto == TRUE, scales::percent(obitos / sum(obitos)), sum(obitos))) |> 
  ungroup() |> 
  ggplot(aes(x = data)) +
  geom_col(aes(y = obitos, fill = moto), colour = "white") +
  geom_text(aes(y = y, label = label), nudge_y = -30, colour = "white") +
  scale_fill_manual("Veículo", 
                    values = c(adjustcolor("darkblue", blue.f = 1.1, alpha.f = .9), 
                               adjustcolor("darkblue", blue.f = .7, alpha.f = .9)), 
                    labels = c("Outros", "Motocicletas")) +
  scale_x_date(NULL, date_breaks = "years", date_labels = "%Y") +
  labs(y = "Total de óbitos em decorrência de sinistros fatais") +
  theme_minimal()







sinistros |> 
  filter(tipo == "SINISTRO FATAL", year(data) %in% 2015:2024) |> 
  anti_join(vitimas) |> View()
  
  
  group_by(data = make_date(year = year(data)), moto = replace_na(tp_veiculo_motocicleta, 0) > 0) |> 
  summarize(obitos = sum(gravidade_fatal)) |> 
  group_by(data) |> 
  mutate(moto = replace_na(moto, FALSE),
         y = ifelse(moto == TRUE, obitos, sum(obitos)),
         label = ifelse(moto == TRUE, scales::percent(obitos / sum(obitos)), sum(obitos))) |> 
  ungroup() |> 
  ggplot(aes(x = data)) +
  geom_col(aes(y = obitos, fill = moto), colour = "white") +
  geom_text(aes(y = y, label = label), nudge_y = -30, colour = "white") +
  scale_fill_manual("Veículo", 
                    values = c(adjustcolor("darkblue", blue.f = 1.1, alpha.f = .9), 
                               adjustcolor("darkblue", blue.f = .7, alpha.f = .9)), 
                    labels = c("Outros", "Motocicletas")) +
  scale_x_date(NULL, date_breaks = "years", date_labels = "%Y") +
  labs(y = "Total de óbitos em decorrência de sinistros fatais") +
  theme_minimal()
  
  
  
sinistros |> 
  filter(if_all(c(tipo_via, administracao, conservacao, jurisdicao), ~ . %in% c("NAO DISPONIVEL", ""))) |> View()



sinistros |> 
  filter(tipo == "SINISTRO FATAL", year(data) > 2015) |> 
  left_join(match, by = join_by(id_sinistro)) |>
  semi_join(logradouros |> 
              filter(!is.na(data_implementacao)) |> 
              left_join(id_logradouros |> rename(id_osm = trechos)) |> 
              unnest(id_osm) |> 
              select(id_osm)) |> 
  group_by(ano = year(data)) |> 
  summarize(obitos = sum(gravidade_fatal)) |> 
  ggplot(aes(x = factor(ano))) +
  geom_col(aes(y = obitos), fill = rgb(.05,.1,.3)) +
  geom_text(aes(y = obitos, label = obitos), vjust = -0.5) +
  labs(x = NULL, y = "Óbitos",
       title = "Óbitos totais - vias que receberam Faixa Azul") +
  theme_minimal()

vitimas |>
  left_join(sinistros) |> 
  filter(gravidade_lesao == "FATAL", year(data) > 2015, year(data) < 2025) |> 
  left_join(match, by = join_by(id_sinistro)) |>
  semi_join(agregados |> 
              filter(!is.na(data_implementacao)) |> 
              left_join(id_agregados) |> 
              unnest(id_osm) |> 
              select(id_osm)) |> 
  group_by(ano = year(data), moto = tp_veiculo_motocicleta > 0) |> 
  summarize(obitos = sum(gravidade_fatal)) |> 
  ggplot(aes(x = factor(ano))) +
  geom_col(aes(y = obitos, fill = moto),  position = "dodge") +
  geom_text(aes(y = obitos, label = obitos), vjust = -0.5) +
  labs(x = NULL, y = "Óbitos",
       title = NULL) +
  theme_minimal()



vitimas |>
  left_join(sinistros) |> 
  filter(gravidade_lesao == "FATAL", year(data) > 2015, year(data) < 2025) |> 
  left_join(match, by = join_by(id_sinistro)) |> 
  left_join(agregados) |> 
  filter(!is.na(data_implementacao)) |> 
  mutate(tipo_veiculo_vitima = str_to_upper(tipo_veiculo_vitima),
         veiculo = fct_collapse(tipo_veiculo_vitima,
                                other_level = "outros",
                                motocicleta = "MOTOCICLETA",
                                nao_disponivel = "NAO DISPONIVEL") |>
           factor(levels = c("outros", "nao_disponivel", "motocicleta"))) |> 
  group_by(data = make_date(year = year(data)), veiculo) |> 
  summarize(obitos = n()) |> 
  mutate(y = case_when(veiculo == "motocicleta" ~ obitos, 
                       veiculo == "outros" ~ sum(obitos)),
         label = case_when(veiculo == "motocicleta" ~ scales::percent(obitos / sum(obitos)), 
                           veiculo == "outros" ~ sum(obitos) |> as.character())) |> 
  ggplot(aes(x = data)) +
  geom_col(aes(y = obitos, fill = veiculo), colour = "white") +
  geom_text(aes(y = y, label = label), nudge_y = -3, colour = "white") +
  scale_fill_manual("Modal de transporte\nda vítima", 
                    values = c(adjustcolor("darkblue", blue.f = 1.2, alpha.f = .9), 
                               "grey80", 
                               adjustcolor("darkblue", blue.f = .5, alpha.f = .9)), 
                    labels = c("Outros", "Não disponível", "Motocicleta")) +
  scale_x_date(NULL, date_breaks = "years", date_labels = "%Y") +
  labs(y = "Total de óbitos em decorrência de sinistros fatais") +
  theme_minimal()



# Tabela antes depois

sinistros |>
  left_join(vitimas |> 
              filter(gravidade_lesao == "FATAL", 
                     str_to_upper(tipo_veiculo_vitima) == "MOTOCICLETA") |> 
              group_by(id_infosiga) |> 
              summarize(obitos_moto = n())) |> 
  mutate(moto = tp_veiculo_motocicleta > 0 & !is.na(tp_veiculo_motocicleta),
         fatal = gravidade_fatal > 0 & !is.na(gravidade_fatal)) |>
  select(id_sinistro,  data, moto, fatal, obitos = gravidade_fatal, obitos_moto) |> 
  left_join(match) |> 
  left_join(agregados |> select(id_trecho_agregado, data_implementacao)) |> 
  filter(!is.na(data_implementacao), 
         data_implementacao <= make_date(year = 2024, month = 10, day = 30)) |> 
  mutate(data = make_date(year = year(data), month = month(data)),
         dist = time_length(data - data_implementacao, "months") |> round(),
         periodo = case_when(dist > 0 ~ "pos", dist < 0 ~ "pre")) |> 
  filter(abs(dist) <= 6, dist !=  0) |> 
  group_by(periodo) |> 
  summarize(sinistros_fatal = sum(fatal == TRUE),
            sinistro_fatal_moto = sum(fatal == TRUE & moto == TRUE),
            sinistros_nao_fatal = sum(fatal == FALSE),
            sinistros_nao_fatal_moto = sum(fatal == FALSE & moto == TRUE),
            obitos = sum(obitos, na.rm = TRUE),
            obitos_moto = sum(obitos_moto, na.rm = TRUE)
  )


df <- sinistros |>
  left_join(vitimas |> 
              filter(gravidade_lesao == "FATAL") |> 
              mutate(grupo = fct_collapse(str_to_upper(tipo_veiculo_vitima),
                                          motocicleta = "MOTOCICLETA",
                                          pedestre = "PEDESTRE",
                                          nao_disponivel = "NAO DISPONIVEL",
                                          other_level = "outros")) |> 
              group_by(id_infosiga, grupo) |> 
              summarize(obitos = n()) |> 
              ungroup() |> 
              pivot_wider(id_cols = id_infosiga, names_from = grupo, values_from = obitos, 
                          names_prefix = "obitos_", values_fill = 0)) |> 
  mutate(moto = tp_veiculo_motocicleta > 0 & !is.na(tp_veiculo_motocicleta),
         fatal = gravidade_fatal > 0 & !is.na(gravidade_fatal)) |>
  select(id_sinistro,  data, moto, fatal, obitos = gravidade_fatal, starts_with("obitos_")) |> 
  left_join(match) |> 
  left_join(agregados |> select(id_trecho_agregado, data_implementacao)) |> 
  filter(!is.na(data_implementacao), 
         data_implementacao <= make_date(year = 2024, month = 8)) |> 
  mutate(data = make_date(year = year(data), month = month(data)),
         dist = time_length(data - data_implementacao, "months") |> round(),
         periodo = case_when(dist > 0 ~ "pos", dist < 0 ~ "pre")) |> 
  filter(abs(dist) <= 3, dist !=  0)
  
df |> 
  group_by(periodo) |> 
  summarize(sinistros_fatal = sum(fatal == TRUE),
            sinistro_fatal_moto = sum(fatal == TRUE & moto == TRUE),
            sinistros_nao_fatal = sum(fatal == FALSE),
            sinistros_nao_fatal_moto = sum(fatal == FALSE & moto == TRUE),
            obitos = sum(obitos, na.rm = TRUE),
            obitos_moto = sum(obitos_motocicleta, na.rm = TRUE)
  )


df |> 
  group_by(periodo) |> 
  summarize(across(starts_with("obitos"), ~ sum(.x, na.rm = TRUE)))






vitimas |> 
  filter(gravidade_lesao == "FATAL") |> 
  mutate(grupo = fct_collapse(str_to_upper(tipo_veiculo_vitima),
                              motocicleta = "MOTOCICLETA",
                              pedestre = "PEDESTRE",
                              nao_disponivel = "NAO DISPONIVEL",
                              other_level = "outros")) |> 
  group_by(id_infosiga, grupo) |> 
  summarize(obitos = n()) |> 
  ungroup() |> 
  pivot_wider(id_cols = id_infosiga, names_from = grupo, values_from = obitos, 
              names_prefix = "obitos_", values_fill = 0)




sinistros |>
  left_join(vitimas |> 
              filter(gravidade_lesao == "FATAL", 
                     str_to_upper(tipo_veiculo_vitima) == "MOTOCICLETA") |> 
              group_by(id_infosiga) |> 
              summarize(obitos_moto = n())) |> 
  mutate(moto = tp_veiculo_motocicleta > 0 & !is.na(tp_veiculo_motocicleta),
         fatal = gravidade_fatal > 0 & !is.na(gravidade_fatal)) |>
  select(id_sinistro,  data, moto, fatal, obitos = gravidade_fatal, obitos_moto) |> 
  left_join(match) |> 
  left_join(agregados |> select(id_trecho_agregado, data_implementacao)) |> 
  filter(!is.na(data_implementacao), 
         data_implementacao <= make_date(year = 2024, month = 8, day = 30)) |> 
  mutate(data = make_date(year = year(data), month = month(data)),
         dist = time_length(data - data_implementacao, "months") |> round(),
         periodo = case_when(dist > 0 ~ "pos", dist < 0 ~ "pre")) |> 
  filter(abs(dist) <= 8, dist !=  0) |> 
  filter(!moto, fatal) |> 
  select(id_sinistro) |> 
  left_join(sinistros |> select(id_sinistro, id_infosiga)) |>
  left_join(vitimas) |> View()




sinistros |>
  left_join(vitimas |> 
              filter(gravidade_lesao == "FATAL", 
                     str_to_upper(tipo_veiculo_vitima) == "MOTOCICLETA") |> 
              group_by(id_infosiga) |> 
              summarize(obitos_moto = n())) |> 
  mutate(moto = tp_veiculo_motocicleta > 0 & !is.na(tp_veiculo_motocicleta),
         fatal = gravidade_fatal > 0 & !is.na(gravidade_fatal)) |>
  select(id_sinistro,  data, moto, fatal, obitos = gravidade_fatal, obitos_moto) |> 
  left_join(match) |> 
  left_join(agregados |> select(id_trecho_agregado, data_implementacao)) |> 
  filter(!is.na(data_implementacao)) |> 
  mutate(data = make_date(year = year(data), month = month(data)),
         dist = time_length(data - data_implementacao, "months") |> round(),
         periodo = case_when(dist > 0 ~ "pos", dist < 0 ~ "pre")) |>
  filter(abs(dist) <= 12) |> 
  group_by(data_implementacao, dist) |> 
  summarize(n = n()) |> 
  # mutate(dist = dist |> as.character() |> str_replace("-", "a")) |> 
  ungroup() |> 
  complete(data_implementacao, dist, fill = list(n = 0)) |> 
  mutate(n = ifelse(make_date(year = 2025, month = 5) - months(dist) < data_implementacao, NA, n)) |> 
  pivot_wider(id_cols =  data_implementacao, names_from = dist, values_from = n)


sinistros |>
  left_join(vitimas |> 
              filter(gravidade_lesao == "FATAL", 
                     str_to_upper(tipo_veiculo_vitima) == "MOTOCICLETA") |> 
              group_by(id_infosiga) |> 
              summarize(obitos_moto = n())) |> 
  mutate(moto = tp_veiculo_motocicleta > 0 & !is.na(tp_veiculo_motocicleta),
         fatal = gravidade_fatal > 0 & !is.na(gravidade_fatal)) |>
  select(id_sinistro,  data, moto, fatal, obitos = gravidade_fatal, obitos_moto) |> 
  left_join(match) |> 
  left_join(agregados |> select(id_trecho_agregado, data_implementacao)) |> 
  filter(!is.na(data_implementacao)) |> 
  mutate(data = make_date(year = year(data), month = month(data)),
         dist = time_length(data - data_implementacao, "months") |> round(),
         periodo = case_when(dist > 0 ~ "pos", dist < 0 ~ "pre")) |>
  filter(abs(dist) <= 12) |> 
  group_by(data_implementacao, dist) |> 
  summarize(n = n()) |> 
  # mutate(dist = dist |> as.character() |> str_replace("-", "a")) |> 
  ungroup() |> 
  complete(data_implementacao, dist, fill = list(n = 0)) |> 
  mutate(n = ifelse(make_date(year = 2025, month = 5) - months(dist) < data_implementacao, NA, n)) |> 
  (\(df) bind_rows(
    df |> filter(abs(dist) <= 3) |> mutate(p = 3),
    df |> filter(abs(dist) <= 6, data_implementacao <= make_date(year = 2024, month = 10)) |> mutate(p = 6),
    df |> filter(abs(dist) <= 12, data_implementacao <= make_date(year = 2024, month = 4)) |> mutate(p = 12)
  ))() |> 
  ggplot(aes(x = dist, y = n, fill = factor(data_implementacao))) +
  geom_vline(xintercept = 0, linetype = "dashed")+ 
  geom_area() +
  facet_wrap(~p, nrow = 3) + 
  theme_minimal() +
  labs(x = "Meses até o tratamento", y = "Sinistros", fill = "Coorte")





sinistros |>
  left_join(vitimas |> 
              filter(gravidade_lesao == "FATAL") |> 
              mutate(grupo = fct_collapse(str_to_upper(tipo_veiculo_vitima),
                                          motocicleta = "MOTOCICLETA",
                                          pedestre = "PEDESTRE",
                                          nao_disponivel = "NAO DISPONIVEL",
                                          other_level = "outros")) |> 
              group_by(id_infosiga, grupo) |> 
              summarize(obitos = n()) |> 
              ungroup() |> 
              pivot_wider(id_cols = id_infosiga, names_from = grupo, values_from = obitos, 
                          names_prefix = "obitos_", values_fill = 0)) |> 
  select(id_sinistro,  data, starts_with("obitos")) |> 
  left_join(match) |> 
  filter(golden_match) |> 
  left_join(agregados |> select(id_trecho_agregado, data_implementacao)) |> 
  filter(!is.na(data_implementacao)) |> 
  mutate(data = make_date(year = year(data), month = month(data)),
         dist = time_length(data - data_implementacao, "months") |> round(),
         periodo = case_when(dist > 0 ~ "pos", dist < 0 ~ "pre")) |>
  filter(abs(dist) <= 12) |> 
  group_by(data_implementacao, dist) |> 
  summarize(across(c(starts_with("obitos")), ~ sum(.x, na.rm = T))) |> 
  select(-obitos_nao_disponivel) |> 
  pivot_longer(starts_with("obito")) |> 
  # mutate(dist = dist |> as.character() |> str_replace("-", "a")) |> 
  ungroup() |> 
  complete(data_implementacao, dist, name, fill = list(value = 0)) |> 
  # mutate(obitos = ifelse(make_date(year = 2025, month = 5) - months(dist) < data_implementacao, NA, obitos)) |> 
  (\(df) bind_rows(
    # df |> 
    #   filter(abs(dist) <= 3) |> 
    #   mutate(p = "03 meses antes e depois (vias implementadas até 01/25)"),
    
    df |> 
      filter(abs(dist) <= 6, 
             data_implementacao <= make_date(year = 2024, month = 10)) |> 
      mutate(p = "06 meses antes e depois (vias implementadas até 10/24)"),
    
    df |> 
      filter(abs(dist) <= 12, 
             data_implementacao <= make_date(year = 2024, month = 4)) |> 
      mutate(p = "12 meses antes e depois (vias implementadas até 04/24)")
  ))() |> 
  group_by(p, dist, name) |> 
  summarize(value = sum(value)) |> 
  ggplot(aes(x = dist, y = value, fill = name)) +
  geom_vline(xintercept = 0, linetype = "dashed")+ 
  # geom_vline(xintercept = 3.5, alpha = .3, colour = "grey60", lwd = 2)+ 
  # geom_vline(xintercept = -3.5, alpha = .3, colour = "grey60", lwd = 2)+ 
  geom_vline(xintercept = 6.5, alpha = .3, colour = "grey60", lwd = 2)+ 
  geom_vline(xintercept = -6.5, alpha = .3, colour = "grey60", lwd = 2)+ 
  geom_vline(xintercept = 12.5, alpha = .3, colour = "grey60", lwd = 2)+ 
  geom_vline(xintercept = -12.5, alpha = .3, colour = "grey60", lwd = 2)+ 
  geom_col() +
  facet_wrap(~p, nrow = 3) + 
  scale_x_continuous(breaks = 0:24-12) +
  scale_fill_discrete("Veículo da vítima", labels = c("Motocicleta", "Outros", "Pedestre")) +
  labs(x = "Meses até o tratamento", y = "Óbitos") +
  theme_minimal() +
  theme(legend.position = "bottom")



sinistros |>
  mutate(moto = tp_veiculo_motocicleta > 0 & !is.na(tp_veiculo_motocicleta),
         fatal = gravidade_fatal > 0 & !is.na(gravidade_fatal)) |>
  select(id_sinistro,  data, moto) |> 
  left_join(match) |> 
  filter(golden_match) |> 
  left_join(agregados |> select(id_trecho_agregado, data_implementacao)) |> 
  filter(!is.na(data_implementacao)) |> 
  mutate(data = make_date(year = year(data), month = month(data)),
         dist = time_length(data - data_implementacao, "months") |> round(),
         periodo = case_when(dist > 0 ~ "pos", dist < 0 ~ "pre")) |>
  filter(abs(dist) <= 12) |> 
  group_by(data_implementacao, dist, moto) |> 
  summarize(n = n()) |> 
  ungroup() |> 
  complete(data_implementacao, dist, moto, fill = list(n = 0)) |> 
  mutate(n = ifelse(make_date(year = 2025, month = 5) - months(dist) < data_implementacao, NA, n)) |> 
  (\(df) bind_rows(
    # df |> 
    #   filter(abs(dist) <= 3) |> 
    #   mutate(p = "03 meses antes e depois (vias implementadas até 01/25)"),
    
    df |> 
      filter(abs(dist) <= 6, 
             data_implementacao <= make_date(year = 2024, month = 10)) |> 
      mutate(p = "06 meses antes e depois (vias implementadas até 10/24)"),
    
    df |> 
      filter(abs(dist) <= 12, 
             data_implementacao <= make_date(year = 2024, month = 4)) |> 
      mutate(p = "12 meses antes e depois (vias implementadas até 04/24)")
  ))() |> 
  group_by(p, dist, moto) |>
  summarize(n = sum(n)) |>
  ggplot(aes(x = dist, y = n, fill = moto)) +
  geom_vline(xintercept = 0, linetype = "dashed")+ 
  # geom_vline(xintercept = 3.5, alpha = .3, colour = "grey60", lwd = 2)+ 
  # geom_vline(xintercept = -3.5, alpha = .3, colour = "grey60", lwd = 2)+ 
  geom_vline(xintercept = 6.5, alpha = .3, colour = "grey60", lwd = 2)+ 
  geom_vline(xintercept = -6.5, alpha = .3, colour = "grey60", lwd = 2)+ 
  geom_vline(xintercept = 12.5, alpha = .3, colour = "grey60", lwd = 2)+ 
  geom_vline(xintercept = -12.5, alpha = .3, colour = "grey60", lwd = 2)+ 
  geom_col() +
  facet_wrap(~p, nrow = 3) + 
  scale_x_continuous(breaks = 0:24-12) +
  scale_fill_discrete("Veículos envolvidos", labels = c("Não envolveu motocicleta",  "Envolveu motocicleta")) +
  labs(x = "Meses até o tratamento", y = "Sinistros") +
  theme_minimal() +
  theme(legend.position = "bottom")


sinistros |>
  left_join(vitimas |> 
              filter(gravidade_lesao == "FATAL", 
                     str_to_upper(tipo_veiculo_vitima) == "MOTOCICLETA") |> 
              group_by(id_infosiga) |> 
              summarize(obitos_moto = n())) |> 
  mutate(moto = tp_veiculo_motocicleta > 0 & !is.na(tp_veiculo_motocicleta),
         fatal = gravidade_fatal > 0 & !is.na(gravidade_fatal)) |>
  select(id_sinistro,  data, moto, fatal, obitos = gravidade_fatal, obitos_moto) |> 
  left_join(match) |> 
  left_join(agregados |> select(id_trecho_agregado, data_implementacao)) |> 
  filter(!is.na(data_implementacao)) |> 
  mutate(data = make_date(year = year(data), month = month(data)),
         dist = time_length(data - data_implementacao, "months") |> round(),
         periodo = case_when(dist > 0 ~ "pos", dist < 0 ~ "pre")) |>
  filter(abs(dist) <= 12) |> 
  group_by(data_implementacao, dist) |> 
  summarize(n = n(),
            obitos = sum(obitos_moto, na.rm = T)) |> 
  # mutate(dist = dist |> as.character() |> str_replace("-", "a")) |> 
  ungroup() |> 
  complete(data_implementacao, dist, fill = list(obitos = 0)) |> 
  mutate(obitos = ifelse(make_date(year = 2025, month = 5) - months(dist) < data_implementacao, NA, obitos)) |> 
  (\(df) bind_rows(
    df |> 
      filter(abs(dist) <= 3) |> 
      mutate(p = "03 meses antes e depois (vias implementadas até 01/25)"),
    
    df |> 
      filter(abs(dist) <= 6, 
             data_implementacao <= make_date(year = 2024, month = 10)) |> 
      mutate(p = "06 meses antes e depois (vias implementadas até 10/24)"),
    
    df |> 
      filter(abs(dist) <= 12, 
             data_implementacao <= make_date(year = 2024, month = 4)) |> 
      mutate(p = "12 meses antes e depois (vias implementadas até 04/24)")
  ))() |> 
  ggplot(aes(x = dist, y = obitos, fill = factor(data_implementacao))) +
  geom_vline(xintercept = 0, linetype = "dashed")+ 
  geom_vline(xintercept = 3.5, alpha = .3, colour = "grey60", lwd = 2)+ 
  geom_vline(xintercept = -3.5, alpha = .3, colour = "grey60", lwd = 2)+ 
  geom_vline(xintercept = 6.5, alpha = .3, colour = "grey60", lwd = 2)+ 
  geom_vline(xintercept = -6.5, alpha = .3, colour = "grey60", lwd = 2)+ 
  geom_vline(xintercept = 12.5, alpha = .3, colour = "grey60", lwd = 2)+ 
  geom_vline(xintercept = -12.5, alpha = .3, colour = "grey60", lwd = 2)+ 
  geom_col(fill = "darkblue") +
  facet_wrap(~p, nrow = 3) + 
  theme_minimal() +
  scale_x_continuous(breaks = 0:24-12) +
  labs(x = "Meses até o tratamento", y = "Óbitos de motociclistas", fill = "Data de implementação \nda faixa azul")




sinistros |>
  left_join(vitimas |> 
              filter(gravidade_lesao == "FATAL", 
                     str_to_upper(tipo_veiculo_vitima) == "MOTOCICLETA") |> 
              group_by(id_infosiga) |> 
              summarize(obitos_moto = n())) |> 
  mutate(moto = tp_veiculo_motocicleta > 0 & !is.na(tp_veiculo_motocicleta),
         fatal = gravidade_fatal > 0 & !is.na(gravidade_fatal)) |>
  select(id_sinistro,  data, moto, fatal, obitos = gravidade_fatal, obitos_moto) |> 
  left_join(match) |> 
  left_join(agregados |> select(id_trecho_agregado, data_implementacao)) |> 
  filter(!is.na(data_implementacao)) |> 
  mutate(data = make_date(year = year(data), month = month(data)),
         dist = time_length(data - data_implementacao, "months") |> round(),
         periodo = case_when(dist > 0 ~ "pos", dist < 0 ~ "pre")) |>
  filter(abs(dist) <= 12) |> 
  group_by(data_implementacao, dist) |> 
  summarize(n = n(),
            obitos = sum(obitos_moto, na.rm = T)) |> 
  # mutate(dist = dist |> as.character() |> str_replace("-", "a")) |> 
  ungroup() |> 
  complete(data_implementacao, dist, fill = list(obitos = 0)) |> 
  mutate(obitos = ifelse(make_date(year = 2025, month = 5) - months(dist) < data_implementacao, NA, obitos),
         n = ifelse(make_date(year = 2025, month = 5) - months(dist) < data_implementacao, NA, n)) |> 
  (\(df) bind_rows(
    df |> 
      filter(abs(dist) <= 3) |> 
      mutate(p = "03 meses antes e depois (vias implementadas até 01/25)"),
    
    df |> 
      filter(abs(dist) <= 6, 
             data_implementacao <= make_date(year = 2024, month = 10)) |> 
      mutate(p = "06 meses antes e depois (vias implementadas até 10/24)"),
    
    df |> 
      filter(abs(dist) <= 12, 
             data_implementacao <= make_date(year = 2024, month = 4)) |> 
      mutate(p = "12 meses antes e depois (vias implementadas até 04/24)")
  ))() |> 
  (\(df) bind_rows(
    df |> mutate(y = obitos, var = "Óbitos"),
    df |> mutate(y = n, var = "Sinistros")
  ))() |> 
  ggplot(aes(x = dist, y = y, fill = factor(data_implementacao))) +
  geom_vline(xintercept = 0, linetype = "dashed")+ 
  geom_vline(xintercept = 3.5, alpha = .3, colour = "grey60", lwd = 2)+ 
  geom_vline(xintercept = -3.5, alpha = .3, colour = "grey60", lwd = 2)+ 
  geom_vline(xintercept = 6.5, alpha = .3, colour = "grey60", lwd = 2)+ 
  geom_vline(xintercept = -6.5, alpha = .3, colour = "grey60", lwd = 2)+ 
  geom_vline(xintercept = 12.5, alpha = .3, colour = "grey60", lwd = 2)+ 
  geom_vline(xintercept = -12.5, alpha = .3, colour = "grey60", lwd = 2)+ 
  geom_col() +
  # facet_grid(p~var, scale = "free") + 
  facet_grid(vars(p, var), scale = "free_y") +
  theme_minimal() +
  scale_x_continuous(breaks = 0:24-12) +
  labs(x = "Meses até o tratamento", y = "Óbitos de motociclistas", fill = "Data de implementação \nda faixa azul")







faixa_azul |> 
  filter(data_implementacao == make_date(year = 2023, month = 12)) |> 
  left_join(trechos_bruto) |> View()

vitimas |>
  left_join(sinistros) |> 
  filter(gravidade_lesao == "FATAL", year(data) > 2015, year(data) < 2025) |> 
  left_join(match, by = join_by(id_sinistro)) |> 
  left_join(agregados) |> 
  filter(!is.na(data_implementacao)) |> 
  mutate(tipo_veiculo_vitima = str_to_upper(tipo_veiculo_vitima),
         veiculo = fct_collapse(tipo_veiculo_vitima,
                                other_level = "outros",
                                motocicleta = "MOTOCICLETA",
                                nao_disponivel = "NAO DISPONIVEL") |>
           factor(levels = c("outros", "nao_disponivel", "motocicleta"))) |> 
  group_by(data = make_date(year = year(data), month = month(data)), veiculo) |> 
  summarize(obitos = n()) |> 
  mutate(y = case_when(veiculo == "motocicleta" ~ obitos, 
                       veiculo == "outros" ~ sum(obitos)),
         label = case_when(veiculo == "motocicleta" ~ scales::percent(obitos / sum(obitos)), 
                           veiculo == "outros" ~ sum(obitos) |> as.character())) |> 
  ggplot(aes(x = data)) +
  geom_area(aes(y = obitos, fill = veiculo), colour = "white") +
  geom_text(aes(y = y, label = label), nudge_y = -3, colour = "white") +
  scale_fill_manual("Modal de transporte\nda vítima", 
                    values = c(adjustcolor("darkblue", blue.f = 1.2, alpha.f = .9), 
                               "grey80", 
                               adjustcolor("darkblue", blue.f = .5, alpha.f = .9)), 
                    labels = c("Outros", "Não disponível", "Motocicleta")) +
  scale_x_date(NULL, date_breaks = "years", date_labels = "%Y") +
  labs(y = "Total de óbitos em decorrência de sinistros fatais") +
  theme_minimal()

did <- tar_read(did_fit, branches = 1)$did_fit_f6610c82c55a05d4

gg <- did |> 
  aggte(type = "dynamic", min_e = -12, max_e = 12, na.rm = TRUE) |> 
  ggdid() +
  scale_y_continuous(expand = expansion(mult = .5)) +
  scale_x_continuous("Meses até data da implementação", breaks = c(0:9-4)*3) +
  scale_colour_manual(values = c("red", "blue"), labels = c("Pré faixa azul", "Pós faixa azul")) +
  theme_minimal() +
  labs(title = NULL) +
  theme(legend.position = "top")

# did$DIDparams$data |> as_tibble() |> 
#   summarize("Mínimo",
#             "a b" = mean(sinistros),
#             median = median(sinistros)) |> 
#   gt()
# 
# 
tabela_cohort_csdid(fit$fit, fit$fit.c, cohort, titulo, filename)

out <- aggte(did, type = "simple", na.rm = TRUE)
ATT <- out$overall.att
ci_low = ATT - out$overall.se * 1.96
ci_high = ATT + out$overall.se * 1.96
se <- out$overall.se
ci_low = ATT - se * 1.96
ci_high = ATT + se * 1.96
significance <- if(ci_low < 0 & ci_high > 0){""}else{"*"}

# tabelinha resultados agregados
tabela1 <- tibble(ATT = ATT, 
       SE = se, 
       "IC (95%)" = paste0("[", round(ci_low, 2), ", ", round(ci_high, 2), "]"),
       Significante = if(ci_low < 0 & ci_high > 0){"Não"}else{"Sim"}) |> 
  gt() |> 
  fmt_number(decimals = 2) |> 
  cols_align(align= "right") |> 
  cols_width(everything() ~ 600/4)

# tabelinha descritivas da base
tabela2 <- did$DIDparams$data |> 
  as_tibble() |> 
  summarize(Mínimo = min(sinistros),
            Q1 = quantile(sinistros, .25),
            Mediana = median(sinistros),
            Média = mean(sinistros),
            Q3 = quantile(sinistros, .75),
            Max = max(sinistros)) |> 
  gt() |> 
  fmt_number(decimals = 2) |> 
  cols_width(everything() ~ 100)

tabela1 + tabela2
free(tabela1) / gg

figura <- (wrap_table(tabela1, space = "fixed") / gg / wrap_table(tabela2, space = "fixed"))

# / wrap_table(tabela2, space = "fixed")

ggsave("teste.pdf", figura, width = 7, height =  6)  



bind_rows(list(
  read_csv("auxiliares/did_tabela_template.csv") |> mutate(t = "total"),
  read_csv("auxiliares/did_tabela_template.csv") |> mutate(t = "moto"),
  read_csv("auxiliares/did_tabela_template.csv") |> mutate(t = "total-km"),
  read_csv("auxiliares/did_tabela_template.csv") |> mutate(t = "moto-km"),
  read_csv("auxiliares/did_tabela_template.csv") |> mutate(t = "total-log"),
  read_csv("auxiliares/did_tabela_template.csv") |> mutate(t = "moto-log"),
  read_csv("auxiliares/did_tabela_template.csv") |> mutate(t = "moto-km-log"),
  read_csv("auxiliares/did_tabela_template.csv") |> mutate(t = "total-km-log")
)) |> mutate(file = str_c(t, "/", file),
             across(everything(), ~ .x |> as.character() |> replace_na(""))) |> write_csv("tabela_did.csv")


tabela |> 
  gt()
  

tabela |> 
  gt() |>
  fmt_number(decimals = 2) |>
  gtsave("teste.tex")


tar_read(dado_agregados) |> 
  ggplot() +
  geom_histogram(aes(x = amenidades / comprimento), bins = 100)


tar_read(dado_agregados) |> filter(!is.na(data_implementacao)) |> mutate(x = amenidades / comprimento) |> summarize(median(x))
tar_read(dado_agregados) |> filter(!is.na(data_implementacao)) |> summarize(median(intersec))


tar_read(dado_vitimas) |> 
  filter(gravidade_lesao == "FATAL") |> 
  mutate(veiculo = fct_collapse(str_to_upper(tipo_veiculo_vitima),
                                motocicleta = "MOTOCICLETA",
                                pedestre_bike = c("PEDESTRE",  "BICICLETA"),
                                other_level = "outros")) |> 
  group_by(id_infosiga, veiculo) |> 
  summarize(mortes = n()) |> 
  pivot_wider(id_cols = id_infosiga, names_from = veiculo, values_from = mortes, 
              values_fill = 0, names_prefix = "mortes_")
  
  
  select(id_infosiga, gravidade_lesao, tipo_veiculo_vitima) 

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
      mutate(coorte = ((coorte - 1) %/% intervalo_meses + 1) |> replace_na(0))) |> 
  select(-data_implementacao)




osm <- dado_osm


# Interpolação 
logradouro <- osm |> 
  filter(!logradouro |> is.na()) |> head(1000) |> 
  st_drop_geometry() |> 
  select(!contains("geometry")) |> 
  group_by(logradouro) |> 
  
  #Pegar o valor que mais se repete naquele logradouro
  summarize(
    across(
      c(everything(), - comprimento),
      ~ fct_infreq(.x) |> levels() |> first())) |> 
  pivot_longer(c(everything(), - logradouro))


tar_read(did_summary_tabela) |> 
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

bind_rows(
  data.table::fread(unz("dados_brutos/sinistros.zip", "sinistros_2015-2021.csv"), encoding = "Latin-1"),
  data.table::fread(unz("dados_brutos/sinistros.zip", "sinistros_2022-2025.csv"), encoding = "Latin-1")) |> 
  as_tibble()




tmp_dir <- tempdir()
unzip("dados_brutos/sinistros.zip", files = c("sinistros_2015-2021.csv", "sinistros_2022-2025.csv"), exdir = tmp_dir)

df <- bind_rows(
  data.table::fread(file.path(tmp_dir, "sinistros_2015-2021.csv"), encoding = "Latin-1"),
  data.table::fread(file.path(tmp_dir, "sinistros_2022-2025.csv"), encoding = "Latin-1")
) %>%
  as_tibble()

file.remove(file.path(tmp_dir, c("sinistros_2015-2021.csv", "sinistros_2022-2025.csv")))




tar_read(dado_trechos_bruto) |> 
  filter(tipo_via %in% c("trunk", "primary", "secondary")) |> 
  summarize(sd(comprimento),
            mean(comprimento),
            median(comprimento))

tar_read(dado_agregados) |> 
  summarize(sd(comprimento),
            mean(comprimento))
  


tar_read(dado_logradouros) |> 
  summarize(sd(comprimento),
            mean(comprimento))




tar_read(dado_match) |> 
  group_by(golden_match) |> 
  summarize(n())

tar_read(dado_sinistros) |> 
  filter(tipo != "NOTIFICACAO") |> 
  anti_join(tar_read(dado_match)) |> View()

tar_read(dado_match) |> 
  arrange(id_sinistro)

tar_read(dado_match_bind)
tar_read(dado_match)





tar_read(dado_match_bind)

match <- tar_read(dado_match_bind) |> 
  mutate(numero_zero = replace_na(numero_zero, TRUE)) |> 
  mutate(golden_match =
           similaridade > .85 &
           distancia_geografica < 150 &
           (match_titulo == TRUE | match_tipo == TRUE) & 
           numero_zero == FALSE) 

match |> 
  semi_join(trechos |> 
              st_drop_geometry() |> 
              filter(tipo_via %in% c("primary", "secondary", "trunk"))) |> 
  left_join(id_agregados |> 
              unnest(id_osm) |> 
              select(contains("id"))) |> 
  left_join(id_logradouros |> 
              unnest(id_osm = trechos) |> 
              select(contains("id"))) |> 
  select(id_sinistro, id_osm, id_trecho_agregado, id_logradouro, golden_match) |> 
  ungroup()

tar_read(dado_sinistros) |> filter(tipo != "NOTIFICACAO", year(data) >= 2019)


match |> 
  mutate(na = is.na(id_osm)) |>
  group_by(na) |> 
  summarize(n())

match |> 
  left_join(tar_read(dado_sinistros) |> select(-logradouro)) |> 
  filter(tipo == "SINISTRO FATAL") |> 
  group_by(golden_match) |> 
  summarize(n = n()) |> 
  mutate(p = n / sum(n))

match |> 
  left_join(tar_read(dado_trechos)) |> 
  filter(tipo_via %in% c("primary", "secondary", "trunk")) |> 
  group_by(golden_match) |> 
  summarize(n = n()) |> 
  mutate(p = n / sum(n))

match |> 
  ungroup() |> 
  summarize(mean(numero_zero))


match |> 
  group_by(golden_match) |> 
  summarize(n = n()) |> 
  mutate(p = n / sum(n))

match |> 
  ggplot() +
  geom_histogram(aes(x = similaridade)) +
  coord_flip()

match |> 
  ggplot() +
  geom_histogram(aes(x = distancia_geografica)) +
  coord_flip()

match |> 
  group_by(match_titulo) |> 
  summarize(n = n()) |> 
  ggplot() +
  geom_col(aes(y = n, fill = match_titulo, x = 1))

