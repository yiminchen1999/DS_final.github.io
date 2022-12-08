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


# import libraries
library(shiny)

##########
# SERVER #
##########

#generic line initiating the SERVER 

server <- shinyServer(function(input, output) {
  
  #########################
  # Data load and cleanup #
  #########################
  
  #Import data
  
  #Clean data
  
  #############
  # Reactives #
  #############
  
  # define any reactive elements of the app
  
  #Close the server definition
})

##################
# User Interface #
##################

#generic line initiating the UI
ui <- shinyUI(fluidPage(
  
  #Add a title
  titlePanel("Add Title Here"),
  
  #This creates a layout with a left sidebar and main section
  sidebarLayout(
    
    #beginning of sidebar section
    #usually includes inputs
    sidebarPanel(),
    
    #beginning of main section
    mainPanel()
  )
  
  #Close the UI definition
))

##############
# Launch App #
##############

#generic line that launches the app
shinyApp(ui = ui, server = server)