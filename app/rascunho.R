logradouro_selecionado <- " FARIA LIMA "
geometria <- logradouro_selecionado |>
  obter_logradouro()

pontos = coletar_pontos_golden(logradouro_selecionado)
pontos 

