#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
      output$plot1 = renderPlot({
          set.seed(1234)
          n <- input$numeric
          minx <- input$sliderX[1]
          maxx <- input$sliderX[2]
          miny <- input$sliderY[1]
          maxy <- input$sliderY[2]
          dataX <- runif(n,minx,maxy)
          dataY <- runif(n,miny,maxy)
          xlab <- ifelse(input$show_xlab,'X axis','')
          ylab <- ifelse(input$show_ylab,'Y axis','')
          title <- ifelse(input$show_title,'Title','')
          plot(dataX,dataY,xlab = xlab,ylab = ylab, main = title,
               xlim = c(-100,100),ylim = c(-100,100))
          })
})
