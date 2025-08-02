bind_rows(list(
  read_csv("auxiliares/did_tabela_template.csv") |> mutate(t = "total"),
  read_csv("auxiliares/did_tabela_template.csv") |> mutate(t = "moto", apenas_moto = TRUE),
  read_csv("auxiliares/did_tabela_template.csv") |> mutate(t = "total-km", por_km = TRUE),
  read_csv("auxiliares/did_tabela_template.csv") |> mutate(t = "moto-km", apenas_moto = TRUE, por_km = TRUE),
  read_csv("auxiliares/did_tabela_template.csv") |> mutate(t = "total-log", log_delta = 1),
  read_csv("auxiliares/did_tabela_template.csv") |> mutate(t = "moto-log", apenas_moto = TRUE, log_delta = 1),
  read_csv("auxiliares/did_tabela_template.csv") |> mutate(t = "moto-km-log", apenas_moto = TRUE, por_km = TRUE, log_delta = .1),
  read_csv("auxiliares/did_tabela_template.csv") |> mutate(t = "total-km-log", por_km = TRUE, log_delta = .1)
)) |> mutate(file = str_c(t, "/", file),
             across(everything(), ~ .x |> as.character() |> replace_na(""))) |> 
  select(-t) |> 
  write_csv("auxiliares/did_tabela.csv")

