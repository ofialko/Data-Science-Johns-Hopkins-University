

library(shiny)
library(leaflet)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Interesting facts about New Zealand, 17 DEC 2016"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
        radioButtons("camps", "Select city:",
                     c("Auckland" = "auck",
                       "Wellington" = "welli",
                       "Nelson" = "queen",
                       "Dunedin" = 'dune'
                       )
    )),
    
    # Show a plot of the generated distribution
    mainPanel(
       leafletOutput("my_map")
    )
  )
))
