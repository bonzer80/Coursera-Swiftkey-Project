suppressPackageStartupMessages(c(
        library(shinythemes),
        library(shiny),
        library(tm),
        library(stringr),
        library(markdown),
        library(stylo)))

source("./predict.R")

shinyServer(function(input, output) {
        
        prediction <- reactive({
                text <- input$text
                textInput <- cleaner(text)
                wordCount <- length(textInput)
                prediction <- predictor(wordCount,textInput)})
        
        output$predictedWord <- renderPrint(prediction())
        output$enteredString <- renderText({ input$text }, quoted = FALSE)
})