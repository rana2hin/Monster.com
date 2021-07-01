#Download and run these packages
#install.packages("shiny")
# library(shiny)
# library(tidyverse)
#library(shinyjs)
#setwd("~/Desktop/project")

# Define UI for app that draws a highway histogram 
ui <- fluidPage(

  headerPanel("Monster.com Jobs in USA"),
  
  # Sidebar layout with input and output definitions 
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      
      # Slider input for the number of bins
      tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #8900CD}")),
      
      textInput("PlotTitle", "Please Select Plot Title", placeholder = "Enter Your Plot Title Here"),
      # Select input for plot colors
      selectInput("PlotColor", "Please Select Plot Color",
                  choices = c("Red"= "Reds","Blue"="Blues", "Green"= "Greens", "Orange"="Oranges", "Purple"= "Purples"),
                  selected = "Purples"),
      hr(style = "border-top: 3px dashed #8900CD;"),
      sliderInput("bars", "How many Categories Do you want to show?", min = 3, max = 9, value = 9),
      radioButtons("sorts", "Sort Job Categories:", choices = list("No Sorting"=1, "Ascending"=2, "Descending"=3))
    ),
    
    # Main panel for displaying the histogram
    mainPanel( plotOutput(outputId = "distPlot3"),plotOutput(outputId = "distPlot2"),plotOutput(outputId = "distPlot1"),plotOutput(outputId = "distPlot")
      
    )
  )

)
  


