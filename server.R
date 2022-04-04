---
title: "R Notebook"
output: html_notebook
---
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


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  new_df<- reactive({
    covid_states %>% filter(state%in%as.character(input$v_target)) %>%
      filter(between(date ,as.Date(input$date[1]), as.Date(input$date[2])))
  })
  selected <- reactive({
    input$select
  })
  t_data <- reactive({
    input$table_date
  })
  w_date <- reactive({
    input$world_date
  })
  output$target_var <- renderUI({
    selectInput("v_target", "Estado", choices = unique(covid_states$state))
  })
  output$table <- renderDataTable({
    covid_states$data <- format(as.Date(covid_states$date), "%d/%m/%Y")
    covid_states %>% filter(date==as.Date(t_data())) %>% select(c(data,state,newDeaths,
                                                        deaths,newCases, totalCases,vaccinated,vaccinated_second,vaccinated_third,
                                                        vaccinated_per_100_inhabitants, deaths_per_100k_inhabitants ))
  })
  output$cities_table <- renderDataTable({
    covid_cities 
  })
  output$cities_map <- leaflet::renderLeaflet({
    #prov_cities <- covid_cities %>% filter(date==as.Date(city_date()))
    city_map <- inner_join(cities,covid_cities, by = c('name_muni'='name'))
    
    pal <- colorNumeric(palette = "Reds", domain = city_map$totalCases_per_100k_inhabitants)
    map <-leaflet(city_map) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(smoothFactor = 0.5,
                  fillOpacity = 0.5,
                  weight = 0.5,
                  color = ~pal(totalCases_per_100k_inhabitants),
                  opacity = 0.8,
                  highlightOptions = highlightOptions(color = "gray",
                                                      weight = 2,
                                                      bringToFront = FALSE),
                  popup = ~paste0(sep = " ",
                                  "<b>Data: </b>", date, "<br>",
                                  "<b>Cidade: </b>",name_muni , "<br>",
                                  "<b>Casos confirmados: </b>", newCases, "<br>",
                                  "<b>Casos por 100k habitantes: </b>", totalCases_per_100k_inhabitants),
                  label = ~name_muni) %>%
      addLegend("bottomright",
                title = "Casos confirmados por<br>100k habitantes",
                pal = pal,
                values = ~totalCases_per_100k_inhabitants, 
                opacity = 0.8)
  })
    
  output$plot <- renderPlotly({
    plot_ly(data = new_df(), x = ~date, y =~get(selected()), type='bar')

  })
  output$world_map <- leaflet::renderLeaflet({
    countrys <- jh %>% filter(Date==as.Date(w_date()))
    covid_map <- inner_join(mapData,countrys,c('name_long'='Country'))
    pal <- colorNumeric(palette = "Reds", domain = covid_map$Daily.new.confirmed.cases.of.COVID.19.per.million.people)
    map <-leaflet(covid_map) %>% addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(smoothFactor = 0.5,
                  fillOpacity = 0.5,
                  weight = 0.5,
                  color =~pal(Daily.new.confirmed.cases.of.COVID.19) ,
                  opacity = 0.8,
                  highlightOptions = highlightOptions(color = "gray",
                                                      weight = 2,
                                                      bringToFront = FALSE),
                  popup = ~paste0(sep = " ",
                                  "<b>Estado: </b>", name_long, "<br>",
                                  "<b>Casos diários confirmados : </b>", Daily.new.confirmed.cases.of.COVID.19, "<br>",
                                  "<b>Casos por 1 MilhÃ£o de habitantes: </b>", Daily.new.confirmed.cases.of.COVID.19.per.million.people),
                  label = ~name_long) %>%
      addLegend("bottomright",
                title = "Casos diários por milhão de pessoas confirmados",
                pal = pal,
                values = ~Daily.new.confirmed.cases.of.COVID.19.per.million.people, 
                opacity = 0.8) 
  })
  output$world_table <- renderDataTable({
    jh %>% filter(Date==as.Date(w_date()))
  })
  
  output$state_map <- leaflet::renderLeaflet({
    # call reactive map
    ama <- covid_states %>% filter(date==as.Date(t_data()))
    new_data <- inner_join(states,ama, by=c("abbrev_state"='state')) %>%
      sf::st_transform('+proj=longlat +datum=WGS84')
    pal <- colorNumeric(palette = "Reds", domain = new_data$totalCases_per_100k_inhabitants)
    map_100k <- leaflet(new_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(smoothFactor = 0.5,
                  fillOpacity = 0.5,
                  weight = 0.5,
                  color = ~pal(totalCases_per_100k_inhabitants),
                  opacity = 0.8,
                  highlightOptions = highlightOptions(color = "gray",
                                                      weight = 2,
                                                      bringToFront = FALSE),
                  popup = ~paste0(sep = " ",
                                  "<b>Estado: </b>", abbrev_state, "<br>",
                                  "<b>Data: </b>", t_data(), "<br>",
                                  "<b>Novos Casos confirmados: </b>", newCases, "<br>",
                                  "<b>Novos Ã“bitos : </b>", newDeaths, "<br>",
                                  "<b>Total de cases : </b>", totalCases, "<br>",
                                  "<b>Total de Ã“bitos : </b>", deaths, "<br>",
                                  "<b>Vacinados por 100 mil habitantes: </b>", vaccinated_per_100_inhabitants, "<br>",
                                  "<b>Casos por 100k habitantes: </b>", totalCases_per_100k_inhabitants),
                  label = ~abbrev_state) %>%
      addLegend("bottomright",
                title = "Casos confirmados por<br>100k habitantes",
                pal = pal,
                values = ~totalCases_per_100k_inhabitants, 
                opacity = 0.8)

  })
  output$vaccine_table <- renderDataTable({
    vaccines_world %>% select(name_long,date,total_vaccinations,people_vaccinated,people_fully_vaccinated,daily_vaccinations,pcntg_vaccinated,
                              pcntg_fully_vaccinated,pcntg_boost)
  })
  output$vaccine_map <- leaflet::renderLeaflet({
    pal <- colorNumeric(palette = "Reds", domain = vaccines_world$pcntg_fully_vaccinated)
    leaflet(vaccines_world) %>% addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(smoothFactor = 0.5,
                  fillOpacity = 0.5,
                  weight = 0.5,
                  color =~pal(pcntg_fully_vaccinated) ,
                  opacity = 0.8,
                  highlightOptions = highlightOptions(color = "gray",
                                                      weight = 2,
                                                      bringToFront = FALSE),
                  popup = ~paste0(sep = " ",
                                  "<b>País: </b>", name_long, "<br>",
                                  "<b>Data: </b>", date, "<br>",
                                  "<b>Porcentagem de vacinados parcialmente: </b>", pcntg_vaccinated, "<b>%</b>", "<br>",
                                  "<b>Porcentagem de vacinados totalmente: </b>", pcntg_fully_vaccinated,"<b>%</b>" , "<br>",
                                  "<b>Porcentagem de vacinados com boost: </b>", pcntg_boost ,"<b>%</b>"),
                  label = ~name_long) %>%
      addLegend("bottomright",
                title = "Porcentagem de vacinados totalmentes",
                pal = pal,
                values = ~pcntg_fully_vaccinated, 
                opacity = 0.8)
    
    
  })
  
  
}


# Create Shiny app ----
shinyApp(ui = ui, server = server)
