library(shiny)

shinyUI(
  navbarPage("Next word predictor",theme = shinytheme('cosmo'),
             tabPanel('The App',
                fluidRow(
                column(6,textInput("text", 
                                    label = h3("Start typing:"),
                                    value = ),
                h4("The predicted next words:"),
                textOutput("pred_word")))),
             tabPanel('About',
                fluidRow(
                  column(2),
                  column(8,includeMarkdown("about.md")),
                  column(2)
                  
                )
             )
  )
)

