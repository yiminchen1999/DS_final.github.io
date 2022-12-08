#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(leaflet)
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(ggthemes)
library(lubridate)
library(report)
library(sf)
library(shiny.semantic)
library(plotly)
library(DT)

knitr::opts_chunk$set(
  echo = TRUE,
  warning = FALSE,
  message = FALSE, 
  fig.width = 8, 
  fig.height = 6,
  out.width = "90%"
)
theme_set(theme_minimal() + theme(legend.position = "bottom"))
options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d

theme_set(theme_minimal() + theme(legend.position = "bottom"))
# define function class_date: puts the date in format
class_date <- function(x){
  day <- substr(x, 3, 4)
  month <- substr(x, 1, 2)
  year <- substr(x, 5, 8)
  return <- paste(month, day, year, sep="/")
}


library(RColorBrewer)
census = #这是未数据处理过后的，即原始版
  read_csv("2018_Central_Park_Squirrel_Census_-_Squirrel_Data.csv")%>% 
  janitor::clean_names() %>%
  #filter(!is.na(primary_fur_color))%>%
  mutate(date=as.character(date),
         long = as.numeric(x),
         lat = as.numeric(y),
         shift = as.character(shift),
         primary_fur_color=as.character(primary_fur_color),
         activity = case_when( running == "TRUE" ~ "running",
                               chasing == "TRUE" ~ "chasing",
                               climbing == "TRUE" ~ "climbing",
                               eating == "TRUE" ~ "eating",
                               foraging == "TRUE" ~ "foraging"),
         reaction = case_when( approaches == "TRUE" ~ "approaches",
                               indifferent == "TRUE" ~ "indifferent",
                               runs_from == "TRUE" ~ "runs_from"),
         sounds = case_when( kuks == "TRUE" ~ "kuks",
                             quaas == "TRUE" ~ "quaas",
                             moans == "TRUE" ~ "moans"))%>%
  select(-x,-y,-running,-chasing,-climbing,-eating,-foraging,-above_ground_sighter_measurement,-color_notes,-other_activities,-specific_location ,-tail_flags ,-tail_twitches,-other_interactions,-kuks,-quaas,-moans,-approaches,-indifferent,-runs_from)



census1 = #这是数据处理过后的，即编号0，1，2，3的，原始版在下面
  read_csv("2018_Central_Park_Squirrel_Census_-_Squirrel_Data.csv")%>% 
  janitor::clean_names() %>%
  #filter(!is.na(primary_fur_color))%>%
  mutate(date=as.character(date),
         long = as.numeric(x),
         lat = as.numeric(y),
         shift = as.character(shift),
         primary_fur_color=as.character(primary_fur_color),
         activity = case_when( running == "TRUE" ~ "running",
                               chasing == "TRUE" ~ "chasing",
                               climbing == "TRUE" ~ "climbing",
                               eating == "TRUE" ~ "eating",
                               foraging == "TRUE" ~ "foraging"),
         reaction = case_when( approaches == "TRUE" ~ "approaches",
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
  mutate(activity = case_when(activity == "running" ~ "1"
                              ,activity == "eating" ~ "2",
                              activity == "foraging" ~ "3",
                              activity == "climbing" ~ "4",
                              activity == "chasing" ~ "5",
                              TRUE ~ "0"
  ))%>%
  mutate(reaction = case_when(reaction == "indifferent" ~ "1"
                              ,reaction == "runs_from" ~ "2",
                              reaction == "approaches" ~ "3",
                              TRUE ~ "0"
  ))%>%
  mutate(sounds = case_when(sounds == "kuks"~ "1",
                            sounds =="quaas" ~ "2",
                            sounds == "moans" ~ "3",
                            TRUE ~ "0"
  ))%>%
  select(-x,-y,-running,-chasing,-climbing,-eating,-foraging,-above_ground_sighter_measurement,-color_notes,-other_activities,-specific_location ,-tail_flags ,-tail_twitches,-other_interactions,-kuks,-quaas,-moans,-approaches,-indifferent,-runs_from)
#write.csv(census1,"~/Desktop/squirrel_tidy.csv", row.names = TRUE)

library(shiny.semantic)


ui <- semanticPage(
  segment(
    class = "basic",
    a(class="ui blue ribbon label", "Leaflet demo"),
    leafletOutput("map"),semantic_DTOutput("table")
    
  ))

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    map <- leaflet(data=census1) %>%
      addTiles() %>%  
      addMarkers(lng=~long, lat=~lat, popup= ~hectare,clusterOptions = markerClusterOptions())
    map
  })
}

shinyApp(ui = ui, server = server)

ui <- semanticPage(
  h2("Pretty tables in Shiny Semantic"),
  semantic_DTOutput("table")
)
server <- function(input, output, session) {
  output$table <- renderDataTable(census1)
  
  
}
shinyApp(ui, server)


ui <- semanticPage(
  segment(
    class = "basic",
    a(class="ui green ribbon label", "Plotly demo"),
    plotlyOutput("plot")
    
  )
)

server <- function(input, output, session) {
  output$plot <- renderPlotly({
    plot_ly(census1, x = ~lat,  color=~hectare) %>%
      add_lines(y = ~long) 
  })
}

shinyApp(ui = ui, server = server)



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
