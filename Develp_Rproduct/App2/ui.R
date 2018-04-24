library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Predict horse power from MPG"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
    sliderInput("sliderMPG","What is the MPG of the car?",
                   min = 10,max = 25,value = 20),
    checkboxInput('showmodel1','Show/Hide Model 1',value=TRUE),
    checkboxInput('showmodel2','Show/Hide Model 2',value=TRUE),
    submitButton('Submit')
    ),
    
    mainPanel(
       plotOutput("plot1"),
       h3('Predicted horse power from model 1:'),
       textOutput('pred1'),
       h3('Predicted horse power from model 2:'),
       textOutput('pred2')
    )
  )
))
