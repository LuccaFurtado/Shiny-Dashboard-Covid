library(plotly)
library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(leaflet)
library(geobr)
library(spData) 
library(sf)
library(shinyWidgets)
library(leaflet)


#getting data
link_covid_brasil_states <- 'https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv'
link_covid_brasil_cities <- 'https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities.csv'


#preparing Brasil states dataframe

states <- read_state(year=2020)
covid_states <- read_csv(link_covid_brasil_states)
covid_states$data <- format(as.Date(covid_states$date), "%d/%m/%Y")
covid_states <-covid_states %>% select(-c("country","epi_week","epi_week","city","recovered","suspects","tests","tests_per_100k_inhabitants","deathsMS","totalCasesMS" ))
covid_states$temp <- covid_states$totalCases_per_100k_inhabitants
new_cols <-c("date","Estado","Novos_Óbitos","Total_de_Óbitos","Novos_Casos","Total_de_casos","Óbitos_por_100_mil_habitantes","Total_de_casos_por_100_mil_habitantes","Óbitos_por_total_de_casos",
  "Vacinados","Vacinados_por_100_habitantes", "Vacinados_com_2_doses","Vacinados_com_a_segunda_dose_por_100_habitantes","Vacinados_com_dose_unica","Vacinados_com_dose_unica_por_100_habitantes",
  "Vacinados_com_a_terceira_dose", "Vacinados_com_a_terceira_dose_por_100  habitantes","Data","totalCases_per_100k_inhabitants")
colnames(covid_states) <- new_cols

#preparing cities dataframe----
covid_cities <- read_csv(link_covid_brasil_cities)
covid_cities$name <- as.character(str_split(covid_cities$city, '/', simplify = TRUE)[,1]) %>% str_to_title()
covid_cities <- covid_cities %>% select(-c('country',"ibgeID","_source" ,"last_info_date"))
cols_cities <- c("Estado",'Cidade','Óbitos','Total de casos','Mortos por 100 mil habitantes', 'Total de casos por 100 mil habitantes','Óbitos por total de casos',
               'date','Novos Casos', 'Novos Óbitos', 'Nome')
colnames(covid_cities) <- cols_cities


# -----
map_states <- leaflet::renderLeaflet({
  ama <- covid_states %>% filter(date==max(covid_states$date))
  new_data <- inner_join(states,ama, by=c("abbrev_state"="Estado")) %>%
    sf::st_transform('+proj=longlat +datum=WGS84')
  pal <- colorNumeric(palette = "Reds", domain = new_data$Total_de_casos_por_100_mil_habitantes)
  map_100k <- leaflet(new_data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(smoothFactor = 0.5,
                fillOpacity = 0.5,
                weight = 0.5,
                color = ~pal(Total_de_casos_por_100_mil_habitantes),
                opacity = 0.8,
                highlightOptions = highlightOptions(color = "gray",
                                                    weight = 2,
                                                    bringToFront = FALSE),
                popup = ~paste0(sep = " ",
                                "<b>Estado: </b>",abbrev_state, "<br>",
                                "<b>Data: </b>", date, "<br>",
                                "<b>Novos Casos confirmados: </b>", Novos_Casos, "<br>",
                                "<b>Novos Obitos : </b>", Novos_Óbitos, "<br>",
                                "<b>Total de casos : </b>", Total_de_casos, "<br>",
                                "<b>Total de Obitos : </b>", Total_de_Óbitos, "<br>",
                                "<b>Vacinados por 100 habitantes: </b>", Vacinados_por_100_habitantes, "<br>",
                                "<b>Casos por 100mil habitantes: </b>", Total_de_casos_por_100_mil_habitantes),
                label = ~abbrev_state) %>%
    addLegend("bottomright",
              pal = pal,
              values = ~Total_de_casos_por_100_mil_habitantes, "<b>Casos por 100mil habitantes: </b>" ,
              opacity = 0.8)
  
})
# Define UI for  that draws a histogram ----
ui <- dashboardPage(
  dashboardHeader(title="Covid Painel"),
  dashboardSidebar(
    radioGroupButtons(
      inputId = "type",
      label = "Selecione um modo:",
      choices = c(`<i class='fa fa-line-chart'></i>` = "line",
                  `<i class="fa fa-map" aria-hidden="true"></i>` = "map",
                  `<i class="fa fa-location-arrow" aria-hidden="true"></i>` = "city"),
      justified = TRUE
    ),
    conditionalPanel(condition="input.type=='line'",
                     uiOutput("target_var"),
                     selectInput("select", label = h3("Selecione"),
                                 choices = c("Novos_Óbitos","Total_de_Óbitos","Novos_Casos","Total_de_casos","Óbitos_por_100_mil_habitantes","Total_de_casos_por_100_mil_habitantes","Óbitos_por_total_de_casos",
                                            "Vacinados","Vacinados_por_100_habitantes", "Vacinados_com_2_doses","Vacinados_com_dose_unica","Vacinados_com_a_terceira_dose"),
                                 selected = 5),
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
                     textOutput("enddate"), # display the end date
                     selectInput("select1", label = h3("Selecione novamente"),
                                 choices = c("Novos_Óbitos","Total_de_Óbitos","Novos_Casos","Total_de_casos","Óbitos_por_100_mil_habitantes","Total_de_casos_por_100_mil_habitantes","Óbitos_por_total_de_casos",
                                             "Vacinados","Vacinados_por_100_habitantes", "Vacinados_com_2_doses","Vacinados_com_dose_unica","Vacinados_com_a_terceira_dose"),
                                 selected = 3)
    ),
    
    conditionalPanel(condition="input.type=='map'",
                     dateInput(inputId = 'table_date', label = 'Selecione a data para tabela',
                               value = max(covid_states$date),
                               min = min(covid_states$date),
                               max = max(covid_states$date)))
  ),
  dashboardBody(
    conditionalPanel(condition="input.type=='city'",
                     dataTableOutput('cities_table')),
    conditionalPanel(condition="input.type=='line'",
                     plotlyOutput("plot"),
                     plotlyOutput("plot1")),
    conditionalPanel(condition="input.type=='map'",
                     leafletOutput( outputId = "state_map"),
                     dataTableOutput('table')),
  ))

server <- function(input, output) {
  new_df<- reactive({
    covid_states %>% filter(Estado%in%as.character(input$v_target)) %>%
      filter(between(date ,as.Date(input$date[1]), as.Date(input$date[2])))
  })
  selected <- reactive({
    input$select
  })
  selected1 <- reactive({
    input$select1
  })
  t_data <- reactive({
    input$table_date
  })
  output$target_var <- renderUI({
    selectInput("v_target", "Estado", choices = unique(covid_states$Estado))
  })
  output$table <- renderDataTable({
    covid_states$data <- format(as.Date(covid_states$date), "%d/%m/%Y")
    covid_states %>% filter(date==as.Date(t_data())) %>% select(c("date","Estado","Novos_Óbitos","Total_de_Óbitos","Novos_Casos","Total_de_casos","Óbitos_por_100_mil_habitantes","Total_de_casos_por_100_mil_habitantes","Óbitos_por_total_de_casos",
                                                                    "Vacinados"))
  })
  
 
  output$cities_table <- renderDataTable({
    covid_cities 
  })
  
  
  output$plot <- renderPlotly({
    plot_ly(data = new_df(), x = ~date, y =~get(selected()), type='bar') %>% 
      layout(xaxis = list(title = 'Date'),
    yaxis = list(title = selected()))
  

    
  })
  output$plot1 <- renderPlotly({
    plot_ly(data = new_df(), x = ~date, y =~get(selected1()), type='bar',color = I('#ff7f0e')) %>% 
      layout(xaxis = list(title = 'Date'),
             yaxis = list(title = selected1()))
    
    
    
  })
  
  output$state_map <- map_states

  
  
  
}


# Create Shiny app ----
shinyApp(ui = ui, server = server)