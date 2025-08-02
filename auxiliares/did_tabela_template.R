bind_rows(list(
  read_csv("auxiliares/did_tabela_template.csv") |> mutate(t = "total"),
  read_csv("auxiliares/did_tabela_template.csv") |> mutate(t = "moto", apenas_moto = TRUE),
  read_csv("auxiliares/did_tabela_template.csv") |> mutate(t = "total-km", por_km = TRUE, ),
  read_csv("auxiliares/did_tabela_template.csv") |> mutate(t = "moto-km", apenas_moto = TRUE, por_km = TRUE),
  read_csv("auxiliares/did_tabela_template.csv") |> mutate(t = "total-log", log_delta = 1),
  read_csv("auxiliares/did_tabela_template.csv") |> mutate(t = "moto-log", apenas_moto = TRUE, log_delta = 1),
  read_csv("auxiliares/did_tabela_template.csv") |> mutate(t = "moto-km-log", apenas_moto = TRUE, por_km = TRUE, log_delta = .1),
  read_csv("auxiliares/did_tabela_template.csv") |> mutate(t = "total-km-log", por_km = TRUE, log_delta = .1)
)) |> 
  mutate(file = str_c(t, "/", file),
         across(everything(), ~ .x |> as.character() |> replace_na("")),
         filtro_segmentos = case_when(por_km == "TRUE" & filtro_segmentos == "intersec >= 20" ~ "intersec / comprimento >= 0.03",
                                      por_km == "TRUE" & filtro_segmentos == "intersec < 20" ~ "intersec / comprimento < 0.03",
                                      por_km == "TRUE" & filtro_segmentos == "amenidades >4" ~ "amenidades / comprimento >= 0.01",
                                      por_km == "TRUE" & filtro_segmentos == "amenidades <= 4" ~ "amenidades / comprimento < 0.01",
                                      TRUE ~ filtro_segmentos)) |> 
  select(-t) |> 
  write_csv("auxiliares/did_tabela.csv")

