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
library(dplyr)
#import the squirrels_fin.csv data file

squirrels <-read_csv("~/Desktop/squirrels_fin1.csv")%>%janitor::clean_names() 


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
  skin = "purple",
  
  #set header text
  dashboardHeader(title = "Squirrels in NYC"),
  #add tab items
  dashboardSidebar(    sidebarMenu(
    
    
    menuItem("Details", tabName = "details", icon = icon("pagelines")),
    menuItem("Map", tabName = "home", icon = icon("map-marked-alt")),
    menuItem("Behavioral Data", tabName ="BD", icon = icon("fas fa-bookmark")),
    menuItem("Future Work", tabName = "images", icon = icon("twitter")),
    
    menuItem("Sources", tabName = "sources", icon = icon("paw"))
  ) #end Sidebar Menu
  ), #end Dashboard Sidebar
  
  #Develop the UI for each tab
  
  dashboardBody(
    tabItems(
      tabItem(
        
        #Create the details tab
        tabName = "details",
        fluidPage(
          
          titlePanel("Squirrel Agression in New York City"),
          #Insert descriptive text
          h3("Ban on Squirrel Feeding"),
          fluidRow(
            column(12, "In March of 2019, New York City brought forward a potential ban on feeding pigeons and squirrels. Bans on feeding other wild animals are already in place, but pigeons and squirrels were exceptions. If the bill becomes a law, individuals will be fined both for squirrel feeders and hand feeding the animals. The reason for the new ban is due to the increased aggression in squirrels, particularly the Eastern Gray Squirrels, resulting in an increased number of bites and scratches in recent years.  This aggression is due to a decreased fear of humans after being hand-fed.
Hand-feeding squirrels comes with a host of other problems for the squirrels. As squirrels are readily supplied human food, they do not receive the same nutritional benefit as squirrels in the wild. In addition, this decreased reliance on foraging means squirrels lose much of their natural survival instinct."),
            column(12, h3("Data Collection and and Analysis")),
            
            column(12, "With the current data, we are targeting the behavior of gray squirrels as they make up the majority of squirrels in New York City. In addition, gray squirrels cause other problems as well, becoming a leading cause of power outages across the US due to chewing on power lines. We are looking to see whether the behaviors of the grey squirrels are more aggressive than other squirrels found in the park and develop a baseline measure for the squirrel’s fear to humans to determine if the ban has any effect over time. 
"),
            column(12, h3("Additional Gray Squirrel Information")),
            column(12, "The Eastern Grey Squirrel in native to New York as well as the eastern portion of the US. However, the species has become a major threat to the environment, housing extreme populations in the Northeast and becoming an invasive species throughout Europe, South Africa, and Bermuda. The Eastern Grey Squirrel is even threatening the survival of the red squirrels in Europe due to outcompeting the native squirrel for food and spreading infectious diseases.")
          ) #end FluidRow
        ) #end fluidPage
      ), #end Details tab
      
      #develop future work tab
      
      tabItem(
        tabName = "images",
        fluidPage(
          
          titlePanel("Future Work"),
          
          h3("Sentiment Analysis"),
        
          #add in Twitter images
          fluidRow(
            
            
            column(6, tags$img(src = "graph2.GIF", width = "450px", height = "500px")),
            column(4, tags$img(src = "graph.GIF", width = "300px", height = "500px"))
          ),
          fluidRow(
            column(6, tags$img(src = "twitter2.GIF", width = "450px", height = "225px")),
            column(5, tags$img(src = "twitter.GIF", width = "450px", height = "225px"))
            
            
            
          )#end FluidRow
        )#end fluidPage
      ),#end Images tab
      
      
      #develop leaflet tab
      tabItem(
        tabName = "home",
        fluidPage(
          #add busy circle for loading
          add_busy_spinner(spin = "fading-circle", color = "white"),
          
          titlePanel("The Squirrels of Central Park"),
          
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
            fluidRow(strong("Tweet:"), "https://twitter.com/LasSchee/status/1190398712612315136",
            ),
            fluidRow(strong("Tidy Tuesday Data:"), "https://github.com/lassescheele/TidyTuesday/tree/master/20191029_NYCSquirrelCensus"
            ),
            fluidRow(strong("Github Repo:"), "https://github.com/MrinmayeeKulkarni/New-York-City-Squirrel-Aggression"),
            fluidRow(h3("Sources")),
            fluidRow("https://www.nationalgeographic.com/animals/2019/06/squirrel-census-new-york-city-central-park/"),
            fluidRow("  https://nypost.com/2019/03/22/animal-lovers-rip-proposed-ban-on-feeding-pigeons-and-squirrels/"),
            fluidRow("https://www.youtube.com/watch?v=pjDBZ8oV95o"),
            tags$img(src = "squirrel.jpg")
            
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
      select(primary_fur_color, age, shift, interaction, activities)
    
    
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
  
  output$tbl <- renderDT(squir2(), colnames = c('Fur Color', 'Age', 'Time of Day', 'Interaction', 'Activity'),
                         filter = 'bottom', 
                         options = list(pageLength = 5))
  # plot-data cleaning 
  squirrel_fin<- squirrels %>% 
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




