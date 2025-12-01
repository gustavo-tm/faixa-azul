library(readr)
library(did)
library(gt)
library(patchwork)


for (file_main in c("1-moto-padrao", "2-moto-pico", "3-moto-atrop", "4-moto-inter")) {
  for (ap in c("", "-km", "-bi", "-bi-km")) {
    for (w in c(0, 12)) {
      file <- paste0(file_main, ap)
      meses <- 12
      if (ap == "-bi" | ap == "-bi-km") { 
        meses <- 6
        if (w == 12) { w <- 6 }
      }
      
      did_simple <- read_csv(paste0("stata/output/imput/", w, "-", file, "-s.csv"),
                             show_col_types = FALSE) |>
        filter(variable == "imput")
      
      ATT <- did_simple$coefficient
      se <- did_simple$std_error
      ll = did_simple$lower_ci
      ul = did_simple$upper_ci
      significance <- if(ll < 0 & ul > 0){""}else{"*"}
      
      tabelinha <- tibble(
        nome = file,
        ATT = format(round(ATT, 3), nsmall = 3),
        SE = format(round(se, 3), nsmall = 3),
        "IC (95%)" = paste0("[", format(round(ll, 3), nsmall = 3), ", ", format(round(ul, 3), nsmall = 3), "]"),
        Significante = if(ll < 0 & ul > 0){"Não"}else{"Sim"})
      
      
      did_dynamic <- read_csv(paste0("stata/output/imput/", w, "-", file, "-d.csv"),
                              show_col_types = FALSE)
      
      event <- did_dynamic |>
        mutate(month = variable |>
                 str_replace("pre", "-") |>
                 str_remove("post") |>
                 as.numeric()) |>
        filter(abs(month) <= meses) |>
        select(month, att = coefficient, ll = lower_ci, ul = upper_ci) |>
        bind_rows(tibble(att = 0, month = -1)) |>
        mutate(post = month >= 0)
      
      p <- event |>
        ggplot(aes(x = month, y = att,
                   ymin = ll, ymax = ul)) +
        geom_point(aes(colour = post), size = 1.5) +
        geom_errorbar(aes(colour = post), width = 0.1) +
        geom_hline(aes(yintercept = 0), linetype = "dashed") +
        scale_y_continuous(expand = expansion(mult = 0.5)) +
        scale_x_continuous("Meses até data da implementação", breaks = c(0:9-4)*3) +
        scale_colour_manual(values = c("red", "blue"), labels = c("Pré faixa azul", "Pós faixa azul")) +
        labs(title = "", y = NULL, colour = NULL) +
        theme_minimal() +
        theme(legend.position = "top")
      
      tabela1 <- tabelinha |>
        select(-nome) |>
        gt() |>
        fmt_number(decimals = 2) |>
        cols_align(align= "right") |>
        cols_width(everything() ~ 600/4)
      
      did_df <- read_csv(paste0("stata/input/", file, ".csv"),
                         show_col_types = FALSE)
      
      tabela2 <- did_df |>
        as_tibble() |>
        mutate(y = sinistros) |>
        summarize(Média = mean(y),
                  Mediana = median(y),
                  "Desvio Padrão" = sd(y),
                  Máximo = max(y)) |>
        gt() |>
        fmt_number(decimals = 2) |>
        cols_width(everything() ~ 600/4)
      
      figura <- (wrap_table(tabela1, space = "fixed") / p / wrap_table(tabela2, space = "fixed"))
      
      ggsave(paste0("stata/output/imput/plots/", w, "-", file, ".pdf"), figura,
             width = 7, height = 6,
             bg = "white",
             create.dir = TRUE)
    }
  }
}


for (file_main in c("1-moto-padrao", "2-moto-pico", "3-moto-atrop", "4-moto-inter")) {
  for (ap in c("", "-km", "-bi", "-bi-km")) {
  
    file <- paste0(file_main, ap)
    if (!file.exists(paste0("stata/output/jwdid/", file, "-s.csv"))) {
      next
    }
    
    meses <- 12
    if (ap == "-bi" | ap == "-bi-km") { meses <- 6 }
    
    did_simple <- read_csv(paste0("stata/output/jwdid/", file, "-s.csv"),
                           show_col_types = FALSE)
    
    ATT <- did_simple$coefficient
    se <- did_simple$std_error
    ll = did_simple$lower_ci
    ul = did_simple$upper_ci
    significance <- if(ll < 0 & ul > 0){""}else{"*"}
    
    tabelinha <- tibble(
      nome = file,
      ATT = format(round(ATT, 3), nsmall = 3),
      SE = format(round(se, 3), nsmall = 3),
      "IC (95%)" = paste0("[", format(round(ll, 3), nsmall = 3), ", ", format(round(ul, 3), nsmall = 3), "]"),
      Significante = if(ll < 0 & ul > 0){"Não"}else{"Sim"})
    
    
    did_dynamic <- read_csv(paste0("stata/output/jwdid/", file, "-d.csv"),
                            show_col_types = FALSE)
    
    event <- did_dynamic |>
      mutate(month = -12:12) |>
      filter(abs(month) <= meses) |>
      select(month, att = coefficient, ll = lower_ci, ul = upper_ci) |>
      mutate(post = month >= 0)
    
    p <- event |>
      ggplot(aes(x = month, y = att,
                 ymin = ll, ymax = ul)) +
      geom_point(aes(colour = post), size = 1.5) +
      geom_errorbar(aes(colour = post), width = 0.1) +
      geom_hline(aes(yintercept = 0), linetype = "dashed") +
      scale_y_continuous(expand = expansion(mult = 0.5)) +
      scale_x_continuous("Meses até data da implementação", breaks = c(0:9-4)*3) +
      scale_colour_manual(values = c("red", "blue"), labels = c("Pré faixa azul", "Pós faixa azul")) +
      labs(title = "", y = NULL, colour = NULL) +
      theme_minimal() +
      theme(legend.position = "top")
    
    tabela1 <- tabelinha |>
      select(-nome) |>
      gt() |>
      fmt_number(decimals = 2) |>
      cols_align(align= "right") |>
      cols_width(everything() ~ 600/4)
    
    did_df <- read_csv(paste0("stata/input/", file_main, ".csv"),
                       show_col_types = FALSE)
    
    tabela2 <- did_df |>
      as_tibble() |>
      mutate(y = sinistros) |>
      summarize(Média = mean(y),
                Mediana = median(y),
                "Desvio Padrão" = sd(y),
                Máximo = max(y)) |>
      gt() |>
      fmt_number(decimals = 2) |>
      cols_width(everything() ~ 600/4)
    
    figura <- (wrap_table(tabela1, space = "fixed") / p / wrap_table(tabela2, space = "fixed"))
    
    ggsave(paste0("stata/output/jwdid/plots/", file, ".pdf"), figura,
           width = 7, height = 6,
           bg = "white",
           create.dir = TRUE)
  }
}

