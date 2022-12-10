#Install and load the following packages


library(shiny)
library(DT)
library(readr)
library(leaflet)
library(tidyverse)
library(shinyWidgets)
library(plotly)
library(shinybusy)
library(rsconnect)
library(magrittr)
library(shinythemes)



census2 = #
  read_csv("2018_Central_Park_Squirrel_Census_-_Squirrel_Data.csv")%>% 
  janitor::clean_names() %>%
  #filter(!is.na(primary_fur_color))%>%
  mutate(date=as.character(date),
         long = as.numeric(x),
         lat = as.numeric(y),
         shift = as.character(shift),
         primary_fur_color=as.character(primary_fur_color),
         activities = case_when( running == "TRUE" ~ "running",
                                 chasing == "TRUE" ~ "chasing",
                                 climbing == "TRUE" ~ "climbing",
                                 eating == "TRUE" ~ "eating",
                                 foraging == "TRUE" ~ "foraging"),
         interaction = case_when( approaches == "TRUE" ~ "approaches",
                                  indifferent == "TRUE" ~ "indifferent",
                                  runs_from == "TRUE" ~ "runs_from"),
         sounds = case_when( kuks == "TRUE" ~ "kuks",
                             quaas == "TRUE" ~ "quaas",
                             moans == "TRUE" ~ "moans"))%>%
  mutate(primary_fur_color = case_when(primary_fur_color == "Gray" ~ "1"
                                       ,primary_fur_color == "Cinnamon" ~ "2",
                                       primary_fur_color == "Black" ~ "3",
                                       TRUE ~ "0"
  ))%>%
  mutate(shift = case_when(shift == "AM" ~ "1"
                           ,shift == "PM" ~ "2",
                           TRUE ~ "0"
  ))%>%
  mutate(age = case_when(age == "Adult" ~ "1"
                         ,age == "Juvenile" ~ "2",
                         TRUE ~ "0"
  ))%>%
  mutate(activities = case_when(activities == "running" ~ "1"
                                ,activities == "eating" ~ "2",
                                activities == "foraging" ~ "3",
                                activities== "climbing" ~ "4",
                                activities == "chasing" ~ "5",
                                TRUE ~ "0"
  ))%>%
  mutate(interaction = case_when(interaction == "indifferent" ~ "1"
                                 ,interaction == "runs_from" ~ "2",
                                 interaction == "approaches" ~ "3",
                                 TRUE ~ "0"
  ))%>%
  mutate(sounds = case_when(sounds == "kuks"~ "1",
                            sounds =="quaas" ~ "2",
                            sounds == "moans" ~ "3",
                            TRUE ~ "0"
  ))%>%
  dplyr::select(long, lat, unique_squirrel_id, shift, date, age, primary_fur_color, activities, interaction, sounds,lat_long,hectare )%>%
  na.omit()


ui <- fluidPage(
  shinythemes::themeSelector(),  
  sidebarLayout(
    sidebarPanel(numericInput(inputId = "primary_fur_color",
                              label = "Primary Fur Color:",
                              min = 0,
                              max = 3,
                              step = 1,
                              value = 1),
                 numericInput(inputId = "age",
                              label = "Age:",
                              min = 0,
                              max = 2,
                              step = 1,
                              value = 1),
                 numericInput(inputId = "shift",
                              label = "Shift:",
                              min = 0,
                              max = 2,
                              step  = 1,
                              value = 1),
                 numericInput(inputId = "reaction",
                              label = "Reaction:",
                              min = 0,
                              max = 3,
                              step  = 1,
                              value = 1),
                 numericInput(inputId = "activity",
                              label = "Activity:",
                              min = 0,
                              max = 5,
                              step  = 1,
                              value = 1),
                 numericInput(inputId = "sounds",
                              label = "Sounds:",
                              min = 0,
                              max = 3,
                              step  = 1,
                              value = 1)
    ),
    mainPanel(h3(textOutput("Power"), leafletOutput("map"),verbatimTextOutput("verb1"),verbatimTextOutput("verb2"),verbatimTextOutput("verb3"),verbatimTextOutput("verb4"),verbatimTextOutput("verb5"),verbatimTextOutput("verb6"),verbatimTextOutput("verb7"))
    ))
  
)





server <- function(input, output) {
  output$Power <- renderPrint({
    long1 <- 0.0005639*input$shift-73.97-0.0009412*input$age-0.0002267*input$activity+0.0004809*input$reaction+0.002142*input$sounds
    lat1 <- 40.781005-0.0013140*input$primary_fur_color+0.0007784*input$reaction+0.0029074*input$sounds
    results <- paste(long1,lat1)
    results 
  })
  output$map <- renderLeaflet({
    long2 <- 0.0005639*input$shift-73.97-0.0009412*input$age-0.0002267*input$activity+0.0004809*input$reaction+0.002142*input$sounds
    lat2 <- 40.781005-0.0013140*input$primary_fur_color+0.0007784*input$reaction+0.0029074*input$sounds
    leaflet(data = census2) %>%
      #setView(lng = -73.961344937861, lat = 40.7840823884086,zoom = 23) %>%
      addTiles()  %>%
      addCircleMarkers(
        ~long2,~lat2,
        color = '#ff0000',
        stroke = FALSE, fillOpacity = 0.5
      )%>%
      fitBounds(lng1 = min(census2$long), 
                lat1 = min(census2$lat), 
                lng2 = max(census2$long), 
                lat2 = max(census2$lat))
  })
  output$verb1 <- renderText({ 
    "   If primary fur color = ”Gray”, input = 1 ;
   If primary fur color = ”Cinnamon”, input = 2 ;
   If primary fur color = ”Black”, input = 3 ;" 
  })
  output$verb2 <- renderText({ 
    "   If age = ”Adult”, input = 1 ;
   If age = ”Juvenile”, input = 2 ;" 
  })
  output$verb3 <- renderText({ 
    "   If shift = ”AM”, input = 1 ;
   If shift = ”PM”, input = 2 ;" 
  })
  output$verb4 <- renderText({ 
    "   If reaction  = ”indifferent”, input = 1 ;
   If reaction = ”runs_from”, input = 2 ;
   If reaction = ”approaches”, input = 3 ;" 
  })
  output$verb5 <- renderText({ 
    "   If activity = ”running”, input = 1 ;
   If activity = ”eating”, input = 2 ;
   If activity = ”foraging”, input = 3 ;
   If activity = ”climbing”, input = 4 ; 
   If activity = ”chasing”, input = 5 ;" 
  })
  output$verb6 <- renderText({ 
    "   If sounds = ”kuks”, input = 1 ;
   If sounds = ”quaas”, input = 2 ;
   If sounds = ”moans”, input = 3 ;" 
  })
  output$verb7 <- renderText({ 
    "   If unknown, input = 0 ; "
  })
  
}  





shinyApp(ui = ui, server = server) 
