library(shiny)
source('process_text.R')     

shinyServer(function(input, output) {
  
  next_word <- reactive({
                if (input$text == '') return(NULL)
                next_word <- predict_next(input$text)})

  output$pred_word <- renderText({
    if (is.null(next_word())) return(NULL)
    next_word()})
})
