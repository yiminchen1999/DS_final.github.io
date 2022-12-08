#Install and load the following packages

library(shiny)
library(DT)
library(readr)
library(leaflet)
library(tidyverse)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(shinybusy)
library(rsconnect)
library(magrittr)

#import the squirrels_fin.csv data file

squirrels <-read_csv("2018_Central_Park_Squirrel_Census_-_Squirrel_Data.csv") %>%janitor::clean_names()

squirrels <- squirrels %>%
  mutate(date=as.character(date),
         long = as.numeric(x),
         lat = as.numeric(y),
         shift = as.character(shift),
         primary_fur_color=as.character(primary_fur_color))%>%
  filter(primary_fur_color == "Gray"|primary_fur_color =="Black"|primary_fur_color =="Cinnamon")%>%
  mutate(interaction = ifelse(approaches==TRUE,"approaches",ifelse(indifferent == TRUE,"indifferent",ifelse(runs_from == TRUE, "runs away", NA)))) %>%
  filter(interaction == "approaches" | interaction == "indifferent" |interaction == "runs away" )%>%
  mutate(activities = ifelse(running==TRUE,"running",ifelse(chasing == TRUE,"chasing",ifelse(climbing == TRUE, "climbing", ifelse(eating == TRUE, "eating",ifelse(foraging == TRUE, "foraging",NA)))))) %>%
  filter(activities == "running" |activities == "chasing" |activities == "climbing" |activities == "eating" |activities == "foraging")%>%
  dplyr::select(long, lat, unique_squirrel_id, shift, date, age, primary_fur_color, activities, interaction, lat_long )%>%
  na.omit()





#attribute options
fur_color <- c("Gray", "Black", "Cinnamon")
agegrp <- c("Adult","Juvenile")
tod <- c("AM", "PM")
interact <- c("approaches", "indifferent", "runs away")
activ <- c("running", "chasing", "climbing", "eating", "foraging")

#color pallet
pal <- colorFactor(palette = c("blue", "black", "red"), levels = c("Gray", "Black", "Cinnamon"))

#Create Shiny Dahboard theme and add a side panel

ui <- dashboardPage(
  #set header color
  skin = "black",
  
  #set header text
  dashboardHeader(title = "Central Park Squirrel Data Analysis-shinyAPP"),
  #add tab items
  dashboardSidebar(    sidebarMenu(
    
    
    menuItem("Map", tabName = "home", icon = icon("map")),
    menuItem("Data analysis", tabName ="BD", icon = icon("fas fa-bookmark")),
    
    menuItem("Sources", tabName = "sources", icon = icon("paw"))
  ) #end Sidebar Menu
  ), #end Dashboard Sidebar
  
  #Develop the UI for each tab
  
  dashboardBody(
    tabItems(
      
      
      #develop leaflet tab
      tabItem(
        tabName = "home",
        fluidPage(
          #add busy circle for loading
          add_busy_spinner(spin = "fading-circle", color = "white"),
          
          titlePanel("Central Park Squirrel Tracker"),
          
          sidebarLayout(
            
            #add in user selection items in the sidebar
            sidebarPanel(
              
              pickerInput("color", "Fur Color", choices = fur_color, 
                          options = list(`actions-box` = TRUE),multiple = TRUE, selected = "Gray"),
              pickerInput("age", "Age", choices = agegrp, 
                          options = list(`actions-box` = TRUE), multiple = TRUE, selected = "Adult"),
              pickerInput("shift", "Time of Day", choices = tod,
                          options = list(`actions-box` = TRUE),multiple = TRUE, selected = "PM"),
              pickerInput("inter", "Interaction", choices = interact, 
                          options = list(`actions-box` = TRUE), multiple = TRUE, selected = "runs away"),
              pickerInput("act", "Activities", choices = activ,  
                          options = list(`actions-box` = TRUE), multiple = TRUE, selected = "eating")
              
              
            ), #end sidebarpanel
            
            # Show a plot of the generated distribution
            mainPanel(
              
              leafletOutput("map"), # potentially add heatmap option
              DTOutput('tbl')
              
              
              
            ) #end mainpanel
          ) # end sidebarLayout
        ) # end fluidPage
      ), #end Home tab
      
      #Add in behavior tab
      
      tabItem(
        tabName ="BD",
        fluidPage(
          #add busy spinner for loading
          add_busy_spinner(spin = "fading-circle", color = "white"),
          titlePanel("Plotting the Behavior of Squirrels"),
          #create layout for plots
          plotlyOutput('behav'),
          
          hr(),
          
          fluidRow(
            column(6, plotlyOutput('plot1')),
            column(6, plotOutput('plot2'))      
            
            
          )#end Fluidrow
        )#end fluid page
      ), #end behavior tab
      
      #add sources tab
      tabItem(
        tabName = "sources",
        fluidPage(
          titlePanel("Data and Source Information"),
          mainPanel( 
            fluidRow(h3("Data")),
            
            fluidRow(strong("Github Repo:"), "https://github.com/yiminchen1999/DS_final.github.io.git"),
            fluidRow(strong("Data Sources")),
            fluidRow("https://data.cityofnewyork.us/Environment/2018-Central-Park-Squirrel-Census-Squirrel-Data/vfnx-vebw"),
            
            
            
          ) # end mainPanel
        ) #end FluidPage
      ) #end Sources tab
    ) #end Tab Items
  ) #end Dashboard Body
)#end Dashboard

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  squir <- reactive ({
    validate(
      need(input$color != "", "Please select one or more fur colors")
    )
    validate(
      need(input$age != "", "Please select one or more age groups")
    )
    validate(
      need(input$shift != "", "Please select a time of day")
    )
    validate(
      need(input$inter != "", "Please select one or more interactions")
    )
    validate(
      need(input$act != "", "Please select one or more actions")
    )
    
    
    squirrels %>%
      filter(primary_fur_color %in% input$color, 
             age %in% input$age,
             shift %in% input$shift, 
             interaction %in% input$inter, 
             activities %in% input$act
      ) 
    
    
    
  })#end reactive
  
  squir2 <- reactive ({
    squir() %>%
      select(unique_squirrel_id,primary_fur_color, age, shift, interaction, activities)
    
    
  })  #end reactive
  
  
  popupsquir <- reactive({ paste0(
    "Age: ", input$age, "<br>",
    "Interaction: ", input$inter, "<br>",
    "Activity: ", input$act, "<br>"
  ) #end Paste0
    
  }) # End reactive
  
  output$map <- renderLeaflet({ 
    leaflet(data = squir()) %>%
      addTiles()  %>%
      addCircles(
        ~long, ~lat,
        color = ~pal(primary_fur_color),
        stroke = FALSE, fillOpacity = 3, popup = popupsquir() )
    
    
    
  }) #end renderLeaflet
  
  output$tbl <- renderDT(squir2(), colnames = c('Unique_squirrel_id','Fur Color', 'Age', 'Time of Day', 'Interaction', 'Activity'),
                         filter = 'top', 
                         options = list(pageLength = 10))
  # plot-data cleaning 
  squirrel_fin<- squirrels %>% 
    group_by(primary_fur_color, activities) %>% 
    mutate(count = n())
  # titles formatting
  titles_plot <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
                       legend.title = element_text(colour = "darkgray",  face = "bold.italic", family = "Helvetica"), 
                       legend.text = element_text(face = "italic", colour="black",family = "Helvetica"), 
                       axis.title = element_text(family = "Helvetica", size = (10), colour = "balck"),
                       axis.text = element_text(family = "Courier", colour = "darkgray", size = (10)))
  
  
  # With count converted to dbl for calculating percentage
  squirrel_final <- squirrel_fin %>%
    transform(count = as.numeric(count))
  
  sq<-squirrel_final %>% 
    count(primary_fur_color,interaction) %>% 
    mutate(perc = n / nrow(squirrel_final)) 
  
  output$behav <- renderPlotly({
    titles_plot <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
                         legend.title = element_text(colour = "black",  face = "bold.italic", family = "Helvetica"),
                         legend.text = element_text(face = "italic", colour="black",family = "Helvetica"), 
                         axis.title = element_text(family = "Helvetica", size = (10), colour = "black")
    )
    squirrel_final %>%
      ggplot(aes(x=primary_fur_color, y=count, group=shift, colour=interaction)) +
      geom_line()+
      geom_point()+facet_wrap(~shift)+theme_bw()+titles_plot+
      labs( title= "Interaction with Shift", y="Count of Squirrels", x = "Fur Color")
    
    
    
  })
  
  output$plot1<-renderPlotly({
    titles_plot <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
                         legend.title = element_text(colour = "black",  face = "bold.italic", family = "Helvetica"),
                         legend.text = element_text(face = "italic", colour="black",family = "Helvetica"), 
                         axis.title = element_text(family = "Helvetica", size = (10), colour = "black")
    )
    sq_plot<-ggplot(squirrel_final, aes(x=primary_fur_color, y=count, group=interaction, colour=activities)) +
      geom_line() +
      geom_point()+theme_bw()+titles_plot+
      labs( title= "Fur color and Activities", y="Count of Squirrels", x = "Fur Color")
    
  })
  output$plot2 <- renderPlot({
    
    titles_plot <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
                         legend.title = element_text(colour = "black",  face = "bold.italic", family = "Helvetica"), 
                         legend.text = element_text(face = "italic", colour="black",family = "Helvetica"), 
                         axis.title = element_text(family = "Helvetica", size = (10), colour = "black")
    )
    ggplot(sq, aes(x = interaction, y = perc,fill=primary_fur_color)) + 
      geom_bar(stat = "identity")+ 
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))+facet_wrap(~primary_fur_color)+
      scale_fill_manual(values=c("#000000", "#D2691E", "#808080"))+theme_bw()+titles_plot+
      labs( title= "Fur color and Interaction Facet", y="Percent (%)", x = "Interaction")
    
  }) #end renderPlot
  
} #end server


shinyApp(ui = ui, server = server)
