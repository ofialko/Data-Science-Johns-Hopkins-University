#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

   output$distPlot <- renderPlot({
        dist <- switch(input$dist,
                       norm = rnorm,
                       unif = runif,
                       exp = rexp,
                       rnorm)
        samples     <- input$sam
        observations <- input$obs
        bins        <- input$bins

        num <- max(c(1e6,observations*samples))
        data0 <- dist(num)
        data  <- matrix(data0,nrow = observations,ncol = samples)
        means <- apply(data, 1, mean)
        mu <- mean(data0); sde <- sqrt(var(data0)/samples)
        x1 <- mu-4*sde
        x2 <- mu+4*sde
        step <- (x2-x1)/100
        x <- seq(from=x1,to=x2,by=step)
        y <- exp(-(x-mu)^2/sde^2/2)
        y <- y/sum(y)/step
        par(mfrow=c(1,2))
        hist(means,breaks = bins,freq = FALSE,
             xlim = c(x1,x2),
             ylim=c(0,max(y)),xlab = '',main='Distribution of means')
        lines(x,y,col='red',lwd=2)
        legend(x='topright',legend = 'CLT',col='red',lty=1,lwd=2)
        hist(data0,breaks = bins,main = 'Original distribution',xlab='',freq=FALSE)


    })

})
