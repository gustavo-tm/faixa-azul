library(shiny)
library(bslib)
library(shinyWidgets)

# shinyWidgets::shinyWidgetsGallery()

library(tidyverse)
library(leaflet)
library(sf)
library(targets)

source("app/funcoes.R")
load("app/dados.RData")

ui <- page_fluid(
  title = "Faixa Azul",
  card(
    card_header("Mapa"),
    checkboxInput(
      inputId = "funcao_pontos",
      label = "Selecionar apenas golden matches",
      value = TRUE
    ),
    sliderInput(
      inputId = "range_ano",
      label = "Escolha as datas dos sinitros",
      min = make_date(year = 2015),
      max = make_date(year = 2025),
      value = c(make_date(year = 2024), make_date(year = 2025))
    ),
    pickerInput(
      inputId = "logradouro_selecionado",
      label = "Selecione os logradouros", 
      selected = " FARIA LIMA ",
      multiple = TRUE,
      choices = logradouros_id |> pull(logradouro) |> unique(),
      options = pickerOptions(liveSearch = TRUE,
                              actionsBox = TRUE,
                              maxOptions = 10),
      width = "100%"
    ),
    leafletOutput("map", height = 1000)
  )
  
)

server <- function(input, output, session){
  
  
  sinistros_selecionados <- reactive({
    sinistros |> 
      filter(data >= as.Date(input$range_ano[1]),
             data <= as.Date(input$range_ano[2]))
  })
  
  geometria_logradouro <- reactive(obter_logradouro(input$logradouro_selecionado))
  pontos <- reactive({
    if(input$funcao_pontos){
      coletar_pontos_golden(input$logradouro_selecionado,
                            sinistros_selecionados())
    }else{
      coletar_pontos_entorno(geometria_logradouro(),
                             sinistros_selecionados())
    }
  })
  
  output$map <- renderLeaflet({
    plotar_logradouro(geometria = geometria_logradouro(), 
                      pontos = pontos())
  })
}

shinyApp(ui, server)

