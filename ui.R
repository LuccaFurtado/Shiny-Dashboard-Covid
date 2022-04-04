---
title: "Covid_ui Notebook"
output: html_notebook
---
#library  
library(plotly)
library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(leaflet)
library(geobr)
library(spData) # For getting spatial data
library(sf)

# Define UI for  that draws a histogram ----
ui <- dashboardPage(
  dashboardHeader(title="Covid Painel"),
  dashboardSidebar(
  radioGroupButtons(
  inputId = "type",
  label = "Choose a graph :",
  choices = c(`<i class='fa fa-line-chart'></i>` = "line",
  `<i class="fa fa-map" aria-hidden="true"></i>` = "map",
  `<i class="fa fa-location-arrow" aria-hidden="true"></i>` = "city",
  `<i class="fa fa-medkit" aria-hidden="true"></i>` = "vaccine",
  `<i class="fa fa-globe" aria-hidden="true"></i>` = "world"),
  justified = TRUE
  ),
  conditionalPanel(condition="input.type=='line'",
  uiOutput("target_var"),
  selectInput("select", label = h3("Selecione"),
  choices = list("Novos Casos"= 'newCases' , "Óbitos"='newDeaths',
  "Casos totais"='totalCases',"Total de Óbitps"= 'deaths',
  'Vacinados'='vaccinated','Vacinados 2ª dose'="vaccinated_second",
  'Vacinados Dose Unica'='vaccinated_single',
  'Vacinado 3ª dose' = 'vaccinated_third'
  ),
  selected = 1),
dateRangeInput(
    inputId = "date",
    label = "Selecione a data",
    start = min(covid_states$date), # The initial start date. Either a Date object, or a string in yyyy-mm-dd format
    end = max(covid_states$date), # The initial end date. Either a Date object, or a string in yyyy-mm-dd format
    min = min(covid_states$date), # The minimum allowed date. Either a Date object, or a string in yyyy-mm-dd format.
    max = max(covid_states$date), # The maximum allowed date. Either a Date object, or a string in yyyy-mm-dd format.
    format = "yyyy/mm/dd",  # The format of the date to display in the browser. try "mm/dd/yy"
    separator = "-" # String to display between the start and end input boxes. try "to"
  ),
  textOutput("startdate"), # display the start date
  textOutput("enddate") # display the end date
  ),
conditionalPanel(condition="input.type=='map'",
                 dateInput(inputId = 'table_date', label = 'Selecione a data para tabela',
                           value = max(covid_states$date),
                           min = min(covid_states$date),
                           max = max(covid_states$date))),
conditionalPanel(condition="input.type=='world'",
                 dateInput(inputId = 'world_date', label = 'Selecione a data para o mapa',
                           value = max(jh$Date),
                           min = min(jh$Date),
                           max = max(jh$Date)))
),
dashboardBody(
conditionalPanel(condition="input.type=='world'",
                leafletOutput(outputId = 'world_map'),
                dataTableOutput('world_table')),
conditionalPanel(condition="input.type=='city'",
                 leafletOutput(outputId = 'cities_map'),
                 dataTableOutput('cities_table')),
conditionalPanel(condition="input.type=='vaccine'",
                 leafletOutput(outputId = 'vaccine_map'),
                 dataTableOutput('vaccine_table')),
conditionalPanel(condition="input.type=='line'",
                plotlyOutput("plot")),
conditionalPanel(condition="input.type=='map'",
                leafletOutput( outputId = "state_map"),
                dataTableOutput('table')),
))
