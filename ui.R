library(shiny)
library(shinyjs)

shinyUI(
    fluidPage(
        useShinyjs(),
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),
            tags$link(rel = "icon", href="mathIcon.ico"),
            tags$link(href="https://fonts.googleapis.com/css2?family=Roboto&display=swap", rel="stylesheet")
        ),
        shinyjs::hidden(titlePanel("Générateur d'exercices")),
        uiOutput("uiPart")
    )
)
