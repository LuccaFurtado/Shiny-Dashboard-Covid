
title: "Covid Data preparation Notebook"
output:
html_notebook

#getting libraries
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

world_pop <- read.csv('C:/Users/Marco Furtado/Desktop/R projects/covid_shiny/world_population.csv.csv') %>% select(c(name,pop2021))

#getting data
link_world_covid_john_hopkins <-"https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/COVID-19%20-%20Johns%20Hopkins%20University.csv"
link_covid_brasil_states <- 'https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv'
link_covid_brasil_cities <- 'https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities.csv'
link_covid_vaccines_country <-'https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv'
country <- read_country(year=2019)
  
#preparing covid world data from john hopkins

jh <- read.csv(link_world_covid_john_hopkins)



#preparing world dataframe

  
  mapData <- world[c(2,11)]
  jh$Date <- as.Date (as.Date("2020-01-21") + jh$Year)
  jh$Country[jh$Country=="Cote d'Ivoire"] <- "Côte d'Ivoire"
  jh$Country[jh$Country=="Democratic Republic of Congo"] <- "Democratic Republic of the Congo"
  jh$Country[jh$Country=="Congo"] <- "Republic of the Congo"
  jh$Country[jh$Country=="Czechia"] <- "Czech Republic"
  mapData$name_long[mapData$name_long=="Russian Federation"] <- "Russia"
  mapData$name_long[mapData$name_long=="Macedonia"] <- "North Macedonia"
  mapData$name_long[mapData$name_long=="Republic of Korea"] <- "South Korea"
  mapData$name_long[mapData$name_long=="Lao PDR"] <- "Laos"


#preparing Brasil states dataframe
  
  states <- read_state(year=2020)
  covid_states <- read_csv(link_covid_brasil_states)
  covid_states$data <- format(as.Date(covid_states$date), "%d/%m/%Y")
  covid_states <-covid_states %>% select(-c("country","epi_week","epi_week","city","recovered","suspects","tests","tests_per_100k_inhabitants" ))

#preparing cities dataframe
  cities <- read_municipality(year = 2020)
  covid_cities <- read_csv(link_covid_brasil_cities)
  covid_cities$name <- as.character(str_split(covid_cities$city, '/', simplify = TRUE)[,1]) %>% str_to_title()
  
#preparing vaccination dataframe
  world_pop <- read.csv('C:/Users/Marco Furtado/Desktop/R projects/covid_shiny/world_population.csv.csv') %>% select(c(name,pop2021))
  world_pop$name[world_pop$name=='DR Congo'] <- "Democratic Republic of the Congo"
  world_pop$name[world_pop$name=='Gambia'] <- 'The Gambia'
  world_pop$name[world_pop$name=='Ivory Coast'] <- "Côte d'Ivoire"
  prov <- inner_join(mapData,world_pop, by=c('name_long'='name'))
  vaccines <- read.csv(link_covid_vaccines_country)
  vaccines$date <- as.Date(vaccines$date)
  vaccines$location[vaccines$location=="Democratic Republic of Congo"] <- "Democratic Republic of the Congo"
  vaccines$location [vaccines$location=="Congo"] <- "Republic of the Congo"
  vaccines$location [vaccines$location=="Cote d'Ivoire"] <- "Côte d'Ivoire"
  vaccines$location [vaccines$location=="Czechia"] <- "Czech Republic"
  vaccines <- vaccines %>%  group_by(location) %>% slice(which.max(date))
  vaccines_world <- inner_join(prov,vaccines, by=c('name_long'='location'))
  vaccines_world$date <- as.Date(vaccines_world$date)
  vaccines_world$pcntg_vaccinated <- round(vaccines_world$people_vaccinated/vaccines_world$pop2021 *(1/10),2)
  vaccines_world$pcntg_fully_vaccinated <-  round(vaccines_world$people_fully_vaccinated/vaccines_world$pop2021 *(1/10),2)
  vaccines_world$pcntg_boost <- round(vaccines_world$total_boosters/vaccines_world$pop2021 *(1/10),2)
  vaccines_world$pcntg_boost[is.na(vaccines_world$pcntg_boost)] <-0
  rm(world_pop)
  rm(prov)
 
  total <- covid_states %>% filter(state=='RN')
  ggplot(total, aes(x=date,y=totalCases)) + geom_bar(stat="identity", na.rm = TRUE)
  plot_ly(total, x = ~date, y = ~newCases, type = 'bar', name = 'SF Zoo')  
  