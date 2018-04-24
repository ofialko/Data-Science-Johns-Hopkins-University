library(shiny)
# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("The Central Limit Theorem (CLT)"),

  sidebarLayout(
    sidebarPanel(
       radioButtons("dist", "Choose a distribution type:",
                     c("Normal" = "norm",
                       "Uniform" = "unif",
                       "Exponential" = "exp")),
       numericInput("sam", "Sample size:", 100, min = 1, max = 1000),
       numericInput("obs", "# of observations:", 1000, min = 1, max = 1000)
       #submitButton("Submit")
    ),

    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot"),
       fluidRow(
           shiny::column(7, offset = 3,
                         sliderInput("bins",
                                     "Number of bins:",
                                     min = 1,
                                     max = 30,
                                     value = 10)

           )
       )
    )
)))
