#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
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
library(shiny)
library(shiny.semantic)
library(plotly)
library(DT)

# define function class_date: puts the date in format
class_date <- function(x){
  day <- substr(x, 3, 4)
  month <- substr(x, 1, 2)
  year <- substr(x, 5, 8)
  return <- paste(month, day, year, sep="/")
}

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


census %>%
  ggplot(aes(long, lat)) +
  geom_point()


library(tidyverse)
theme_set(theme_light())
nyc_squirrels =read_csv("2018_Central_Park_Squirrel_Census_-_Squirrel_Data.csv")

library(sf)
central_park_sf <- read_sf("CentralAndProspectParks/")
by_hectare <- nyc_squirrels %>%
  add_count(hectare) %>%
  mutate(above_ground = !is.na(location) & location == "Above Ground") %>%
  summarize_at(vars(long, lat, approaches:runs_from,  ends_with("ing"), above_ground), mean) 
 
by_hectare %>%
  filter(n >= 10) %>%
  ggplot() +
  geom_sf(data = central_park_sf) +  
  geom_point(aes(long, lat, size = n, color = runs_from)) +
  theme_void() +
  scale_color_gradient2(low = "blue", high = "red", mid = "pink",
                        midpoint = .3, labels = scales::percent) +
  labs(color = "% of squirrels run",
       size = "# of squirrels",
       title = "Squirrels in the northwest corner of Central Park are more likely to run away") +
  coord_sf(datum = NA)


central_park_sf %>%
  count(lanes, sort = TRUE)
ggplot(central_park_sf) +
  geom_sf() +
  geom_point(aes(long, lat, color = runs_from), data = by_hectare) +
  coord_sf(datum = NA)

central_park_sf %>%
  ggplot() +
  geom_sf(aes(color = bicycle)) +
  coord_sf(datum = NA)


library(shiny)

squirrel_variables <- by_hectare %>%
  select(-(hectare:lat)) %>%
  colnames()

names(squirrel_variables) <- squirrel_variables %>%
  str_replace_all("_", " ") %>%
  str_to_title()

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Central Park Squirrels"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("variable",
                  "Variable:",
                  choices = squirrel_variables),
      sliderInput("min_squirrels",
                  "Minimum squirrels:",
                  min = 1,
                  max = 30,
                  value = 10)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("park_plot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$park_plot <- renderPlot({
    # generate bins based on input$bins from ui.R
    var <- sym(input$variable)
    
    filtered <- by_hectare %>%
      filter(n >= input$min_squirrels)
    
    midpoint <- mean(filtered[[input$variable]])
    
    filtered %>%
      ggplot() +
      geom_sf(data = central_park_sf) +  
      geom_point(aes(long, lat, size = n, color = !!var)) +
      theme_void() +
      scale_color_gradient2(low = "blue", high = "red", mid = "pink",
                            midpoint = midpoint, labels = scales::percent) +
      labs(color = paste("%", input$variable),
           size = "# of squirrels") +
      coord_sf(datum = NA)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)