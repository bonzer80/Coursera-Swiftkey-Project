library(shiny)
library(shinythemes)

shinyUI(fluidPage(
        theme = shinytheme("cosmo"),
        
        titlePanel(h1("N-gram based text predictor", align="center"),
                   windowTitle = "coursera capstone project"),
        h4("(Coursera Data Science Capstone Project)", align="center"),
        
        hr(),
        
        fluidRow(
                
                column(6, offset=3,
                       
                       tabsetPanel(type = "tabs",
                                   tabPanel("Next Word Prediction",
                                                                                       
                                            fluidRow(
                                                    
                                                    column(3),
                                                    column(6,
                                                           tags$div(textInput("text", 
                                                                              label = h3("Type your text here:"),
                                                                              value = ),
                                                                     br(),
                                                                    tags$hr(),
                                                                    h4("The predicted next word is.."),
                                                                    tags$span(style="color:darkred",
                                                                              tags$strong(tags$h3(textOutput("predictedWord")))),
                                                                    br(),
                                                                    tags$hr(),
                                                                    h4("You entered:"),
                                                                    tags$em(tags$h4(textOutput("enteredString"))),
                                                                    align="center")
                                                    ),
                                                    column(3)
                                            )
                                   ),
                                   tabPanel("About",
                                            fluidRow(
                                                    column(2,
                                                           p("")),
                                                     column(8,
                                                            includeMarkdown("./about/Capstone.Rmd"))),
                                                    column(2,
                                                           p(""))
                                            )
                                   )
                       )
                )
        )

                       )