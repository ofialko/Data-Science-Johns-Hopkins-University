
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Tabs"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
        textInput('box1','Enter tab 1 text',value = 'Tab 1 !'),
        textInput('box2','Enter tab 2 text',value = 'Tab 2 !'),
        textInput('box3','Enter tab 3 text',value = 'Tab 3 !')
    
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       tabsetPanel(type='tabs',
                   tabPanel('Tab 1',br(),textOutput('out1')),
                   tabPanel('Tab 2',br(),textOutput('out2')),
                   tabPanel('Tab 3',br(),textOutput('out3'))
                   )
    )
  )
))
