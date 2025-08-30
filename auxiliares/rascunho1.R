library(targets)

tar_make()

tar_visnetwork(label = c("time", "branches"))

tar_read(did_segmento_filtrado, branches = 2) 
tar_read(did_segmento_PSM, branches = 3)
tar_read(did_segmento_PSM, branches = 4)
tar_read(did_sinistros_filtrado)
tar_read(did_segmento_filtrado, branches = 3) |> View()

tar_read(did_df)
tar_read(did_segmento_PSM, branches = 7)
tar_read(did_sinistro_filtrado, branches = 1)
tar_read(did_df, branches = 1)


tar_meta(fields = error, complete_only = TRUE) |> tail() |> View()
tar_progress() |> View()
tar_visnetwork()
tar_delete()
tar_manifest() |> View()


tar_workspace(did_fit_81c6acd4fe8f08d7)
tar_traceback(did_fit_5443474165fa1954)
tar_meta(teste) |> View()
tar_meta(did_segmento_PSM) |> View()




tar_read(teste) |> View()
tar_read(did_segmento_PSM_75b498a68d45040c) |> View()


tar_read(did_tabela) |> 
  group_by(segmento_nivel, filtro_segmentos, rodarPSM, PSM_corte_minimo) |> 
  summarize(n = n())




