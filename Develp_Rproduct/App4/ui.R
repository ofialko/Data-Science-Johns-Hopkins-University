library(shiny)

shinyUI(fluidPage(
  titlePanel("Visualize many models !"),
  sidebarLayout(
    sidebarPanel(
        h3('Slope'),
        textOutput('slopeOut'),
        h3('Intercept'),
        textOutput('intOut')
    ),
    mainPanel(
       plotOutput("plot1",brush = brushOpts(id='brush1'))
    )
  )
))
