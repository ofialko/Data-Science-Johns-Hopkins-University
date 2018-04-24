#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("Plot Random numbers"),
  sidebarLayout(
    sidebarPanel(
        numericInput('numeric','Number of random numbers to be plotted ?',
                     value = 1000, min = 1,max = 1000,step = 1),
        sliderInput('sliderX',label = 'Pick minimum and maximun X value',
                    -100,100,value = c(-50,50)),
        sliderInput('sliderY',label = 'Pick minimum and maximun Y value',
                    -100,100,value = c(-50,50)),
        checkboxInput('show_xlab','Show/Hide X Axis Label',value = TRUE),
        checkboxInput('show_ylab','Show/Hide Y Axis Label',value = TRUE),
        checkboxInput('show_title','Show/Hide X Title',value = TRUE)
    )
    ,
    
    # Show a plot of the generated distribution
    mainPanel(
       h3('Graph of random points'),
       plotOutput('plot1')
    )
  )
))
