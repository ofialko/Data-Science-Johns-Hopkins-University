#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)

shinyServer(function(input, output) {
    coord <- reactive({ 
    coords <- switch(input$camps,
                   auck = data.frame(lng=174.768, lat=-36.852),
                   welli = data.frame(lat= c(-41.2865),lng=c(174.7762)),
                   queen = data.frame(lat=-42.0594420,lng=172.658172),
                   dune = data.frame(lat=-45.849173,lng=170.534036)
                   )
    coords
    })
    pop <- reactive({
        pops<-switch(input$camps,
                   auck = 'R was born here !',
                   welli = 'Wellington is the southernmost capital in the world',
                   queen = 'Blue Lake has the clearest water in the world',
                   dune = 'Baldwin street is the steepest street in the world')
    
       pops
       })
    output$my_map <- renderLeaflet({

        coord() %>%
        leaflet() %>% 
            addTiles()      %>%
            addMarkers(popup = pop())
    }) 

})
