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
    pickerInput(
      inputId = "logradouro_selecionado",
      label = "Selecione o logradouro", 
      selected = " FARIA LIMA ",
      multiple = TRUE,
      choices = logradouros_id |> pull(logradouro) |> unique(),
      options = pickerOptions(liveSearch = TRUE,
                              actionsBox = TRUE,
                              maxOptions = 10),
      width = "100%"
    ),
    leafletOutput("map")
  )
  
)

server <- function(input, output, session){
  
  logradouro_selecionado <- reactive(obter_logradouro(input$logradouro_selecionado))
  
  output$map <- renderLeaflet({
    logradouro_selecionado() |> 
      plotar_logradouro()
  })
}

shinyApp(ui, server)

