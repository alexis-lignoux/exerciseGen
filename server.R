library(shiny)
library(shinyjs)
library(shinyWidgets)
library(rmarkdown)
library(knitr)

options(stringsAsFactors = FALSE)

rm(list = ls())
cat('\014')

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    database                <- reactiveValues()
    database$exEquation     <- "Equation"
    database$correctionDone <- FALSE
    
    source("scripts/solvingfunctions.R", local = TRUE, encoding = "UTF-8")
    
    output$uiPart <- renderUI({
        
        tags$div(
            id = "fullPage",
            tags$div(
                id = "background"
            ),
            tags$div(
                id = "pageHeader",
                fluidRow(
                    column(
                        10,
                        tags$div(
                            style = "margin-left: 15px; margin-right: 15px; margin-top: 10px;",
                            "Tableaux de variation sur des polynômes de degré 2 ou 3"
                        )
                    ),
                    column(
                        2,
                        tags$div(
                            style = "text-align: right",
                            img(src = "logo2.svg", height = 65, width = 100)
                        )
                    )
                )
                
            ),
            tags$div(
                id = "pageBody",
                fluidRow(
                    column(
                        3,
                        offset = 0,
                        tags$div(
                            id = "leftOfPage",
                            tags$div(
                                radioGroupButtons(
                                    inputId = "pageSelection",
                                    choices = c("Génération aléatoire" = "page1",
                                                "Équation spécifique" = "page2"),
                                    selected = "page1",
                                    direction = "vertical",
                                    status = "customized",
                                    size = "lg",
                                    width = "100%"
                                )
                            )
                        )
                    ),
                    column(
                        7,
                        offset = 0,
                        tags$div(
                            id = "centerOfPage",
                            tags$div(
                                fluidRow(
                                    column(
                                        9,
                                        tags$div(
                                            id = "consigne",
                                            style = "font-style: italic; font-size: 18px;",
                                            "Cliquez sur le bouton \"Générer\" pour créér un exercice :"
                                        )
                                    ),
                                    column(
                                        3,
                                        tags$div(
                                            style = "text-align: right",
                                            actionButton(
                                                inputId = "generateEx",
                                                label = "Générer",
                                                icon("sync-alt")
                                            ),
                                            shinyjs::hidden(
                                                actionButton(
                                                    inputId = "resetParams",
                                                    label = "Réinitialiser",
                                                    icon("sync-alt")
                                                )
                                            )
                                        )
                                    )
                                ),
                                shinyjs::hidden(
                                    tags$div(
                                        id = "DLButtons",
                                        tags$div(id = "firstTab", style = "font-size: 30px;", uiOutput("ex1")),
                                        shinyjs::hidden(
                                            tags$div(
                                                id = "secondTab",
                                                tags$div(style = "font-size: 30px;", uiOutput("ex2")),
                                                tags$div(
                                                    id = "parameters",
                                                    fluidRow(
                                                        column(3, sliderInput(inputId = "A", label = "a",  min = -20, max = 20, step = 1, value = 0)),
                                                        column(3, sliderInput(inputId = "B", label = "b",  min = -20, max = 20, step = 1, value = 0)),
                                                        column(3, sliderInput(inputId = "C", label = "c",  min = -20, max = 20, step = 1, value = 0)),
                                                        column(3, sliderInput(inputId = "D", label = "d",  min = -20, max = 20, step = 1, value = 0))
                                                    )
                                                )
                                            )
                                        ),
                                        tags$div(
                                            style = "height: 30px;",
                                            shinyjs::hidden(
                                                tags$div(
                                                    id = "errorMsg",
                                                    style = "color: red; font-style: italic; text-align: center; font-size: 14px; margin-",
                                                    "La fonction doit être un polynôme de degré 2 ou 3."
                                                )
                                            )
                                        ),
                                        fluidRow(
                                            align = "right",
                                            column(
                                                6,
                                                offset = 6,
                                                actionButton(
                                                    inputId = "generateCorrection",
                                                    label = "Corriger l'exercice",
                                                    icon = icon("check")
                                                ),
                                                shinyjs::hidden(
                                                    actionLink(
                                                        inputId = "displayCorrection",
                                                        label = "Masquer la correction"
                                                    )
                                                )
                                            )
                                        ),
                                        shinyjs::hidden(
                                            tags$div(
                                                id = "pdfDisplay",
                                                uiOutput("iframe")
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    ),
                    column(
                        2,
                        offset = 0,
                        tags$div(
                            id = "rightOfPage"
                            
                        )
                    )
                )
            )
        )
    })
    
    output$ex1 <- renderUI({
        withMathJax(paste0('$$', database$exEquation, '$$'))
    })
    
    output$ex2 <- renderUI({
        
        if (input$A != 0){
            equation <- paste0(
                "f(x)=", ifelse(input$A < 0, "-", ""), ifelse(abs(input$A) != 1, abs(input$A), ""), "x^3",
                ifelse(input$B != 0, paste0(ifelse(sign(input$B) == -1, "-", "+"), ifelse(abs(input$B) != 1, abs(input$B), ""), "x^2"), ""),
                ifelse(input$C != 0, paste0(ifelse(sign(input$C) == -1, "-", "+"), ifelse(abs(input$C) != 1, abs(input$C), ""), "x"), ""),
                ifelse(input$D != 0, paste0(ifelse(sign(input$D) == -1, "-", "+"), abs(input$D)), "")
            )
        } else if (input$B != 0){
            equation <- paste0(
                "f(x)=", ifelse(input$B < 0, "-", ""), ifelse(abs(input$B) != 1, abs(input$B), ""), "x^2",
                ifelse(input$C != 0, paste0(ifelse(sign(input$C) == -1, "-", "+"), ifelse(abs(input$C) != 1, abs(input$C), ""), "x"), ""),
                ifelse(input$D != 0, paste0(ifelse(sign(input$D) == -1, "-", "+"), abs(input$D)), "")
            )
        } else if (input$C != 0){
            equation <- paste0(
                "f(x)=", ifelse(input$C < 0, "-", ""), ifelse(abs(input$C) != 1, abs(input$C), ""), "x",
                ifelse(input$D != 0, paste0(ifelse(sign(input$D) == -1, "-", "+"), abs(input$D)), "")
            )
        } else {
            equation <- paste0(
                "f(x)=",
                input$D
            )
        }
        
        withMathJax(paste0("$$", equation, "$$"))
    })
    
    observeEvent(input$pageSelection,{
        shinyjs::hide(id = "pdfDisplay")
        if (input$pageSelection == "page1"){
            shinyjs::show(id = "firstTab")
            shinyjs::hide(id = "secondTab")
            shinyjs::show(id = "generateEx")
            shinyjs::hide(id = "resetParams")
            shinyjs::html(id = "consigne", "Cliquez sur le bouton \"Générer\" pour créér un exercice :")
            shinyjs::hide(id = "DLButtons")
            shinyjs::hide(id = "generateCorrection")
            shinyjs::hide(id = "displayCorrection")
            shinyjs::hide(id = "parameters")
            updateSliderInput(session = session, inputId = "A", value = 0)
            updateSliderInput(session = session, inputId = "B", value = 0)
            updateSliderInput(session = session, inputId = "C", value = 0)
            updateSliderInput(session = session, inputId = "D", value = 0)
        } else {
            shinyjs::hide(id = "firstTab")
            shinyjs::show(id = "secondTab")
            shinyjs::hide(id = "generateEx")
            shinyjs::show(id = "resetParams")
            shinyjs::html(id = "consigne", "Déterminer le polynôme que vous souhaitez étudier :")
            shinyjs::show(id = "DLButtons")
            shinyjs::show(id = "generateCorrection")
            shinyjs::enable(id = "generateCorrection")
            shinyjs::hide(id = "displayCorrection")
            shinyjs::show(id = "parameters")
        }
        database$correctionDone   <- FALSE
        database$correctionShown  <- FALSE
    })
    
    observeEvent(input$generateEx,{
        if (file.exists("www/pdfCorrection.pdf")){
            try(file.remove("www/pdfCorrection.pdf"), silent = TRUE)
        }
        shinyjs::enable("generateCorrection")
        shinyjs::html(id = "consigne", "Déterminer le tableau de variation de la fonction suivante :")
        shinyjs::hide(id = "pdfDisplay")
        database$correctionDone   <- FALSE
        database$correctionShown  <- FALSE
        database$polyDegre <- ifelse(runif(n = 1, min = 0, max = 1) > 0.75, 2, 3)
        
        if (database$polyDegre == 3){
            a <- b <- c <- d <- 0 
            while (a == 0 || b == 0 && c == 0) {
                a = round(runif(n = 1, min = -10, max = 10))
                b = round(runif(n = 1, min = -10, max = 10))
                c = round(runif(n = 1, min = -10, max = 10))
                d = round(runif(n = 1, min = -10, max = 10))
            }
            database$coefficients <- list("a" = a,
                                          "b" = b,
                                          "c" = c,
                                          "d" = d)
            database$exEquation <- paste0(
                "f(x)=", ifelse(a < 0, "-", ""), ifelse(abs(a) != 1, abs(a), ""), "x^3",
                ifelse(b != 0, paste0(ifelse(sign(b) == -1, "-", "+"), ifelse(abs(b) != 1, abs(b), ""), "x^2"), ""),
                ifelse(c != 0, paste0(ifelse(sign(c) == -1, "-", "+"), ifelse(abs(c) != 1, abs(c), ""), "x"), ""),
                ifelse(d != 0, paste0(ifelse(sign(d) == -1, "-", "+"), abs(d)), "")
            )
        } else {
            a <- b <- c <- 0
            while (a == 0) {
                a = round(runif(n = 1, min = -10, max = 10))
                b = round(runif(n = 1, min = -10, max = 10))
                c = round(runif(n = 1, min = -10, max = 10))
            }
            database$coefficients <- list("a" = a,
                                          "b" = b,
                                          "c" = c)
            database$exEquation <- paste0(
                "f(x)=", ifelse(a < 0, "-", ""), ifelse(abs(a) != 1, abs(a), ""), "x^2",
                ifelse(b != 0, paste0(ifelse(sign(b) == -1, "-", "+"), ifelse(abs(b) != 1, abs(b), ""), "x"), ""),
                ifelse(c != 0, paste0(ifelse(sign(c) == -1, "-", "+"), abs(c)), "")
            )
        }
        
        
        shinyjs::show(id = "DLButtons")
        shinyjs::show(id = "generateCorrection")
        shinyjs::hide(id = "displayCorrection")
        
    })
    
    observeEvent(input$generateCorrection,{
        if (input$pageSelection == "page2"){
            if (input$A != 0 || input$B !=0){
                errorCoeffs <- FALSE
            } else {
                errorCoeffs <- TRUE
            }
            
            if (input$A != 0){
                database$coefficients <- list(
                    "a" = input$A,
                    "b" = input$B,
                    "c" = input$C,
                    "d" = input$D
                )
            } else {
                database$coefficients <- list(
                    "a" = input$B,
                    "b" = input$C,
                    "c" = input$D
                )
            }
        } else {
            errorCoeffs <- FALSE
        }
        
        if (errorCoeffs == FALSE){
            shinyjs::disable("generateCorrection")
            if (input$pageSelection == "page2"){
                shinyjs::hide(id = "parameters")
            }
            tryGenerate <- try(rmarkdown::render("templateDoc/pdfCorrection.Rmd", output_dir = "www", encoding = "UTF-8"))
            shinyjs::show(id = "pdfDisplay", anim = TRUE, animType = "slide", time = 0.2)
            database$correctionDone  <- TRUE
            database$correctionShown <- TRUE
            output$iframe <- renderUI({
                tags$iframe(style="height:650px; width:100%", src="pdfCorrection.pdf")
            })
            shinyjs::hide(id = "generateCorrection")
            shinyjs::show(id = "displayCorrection")
            shinyjs::html(id = "displayCorrection", HTML("Masquer la correction <i class='fa fa-chevron-up'></i>"))
        } else {
            shinyjs::show(id = "errorMsg", anim = TRUE, animType = "fade", time = 0.1)
            shinyjs::delay(ms = 3000, {
                shinyjs::hide(id = "errorMsg", anim = TRUE, animType = "fade", time = 0.25)
            })
        }
    })
    
    observeEvent(input$displayCorrection,{
        if (database$correctionShown == TRUE){
            shinyjs::hide(id = "pdfDisplay", anim = TRUE, animType = "slide", time = 0.2)
            shinyjs::html(id = "displayCorrection", HTML("Montrer la correction <i class='fa fa-chevron-down'></i>"))
            database$correctionShown <- FALSE
        } else {
            shinyjs::show(id = "pdfDisplay", anim = TRUE, animType = "slide", time = 0.2)
            shinyjs::html(id = "displayCorrection", HTML("Masquer la correction <i class='fa fa-chevron-up'></i>"))
            database$correctionShown <- TRUE
        }
    })
    
    observeEvent(input$resetParams,{
        updateSliderInput(session = session, inputId = "A", value = 0)
        updateSliderInput(session = session, inputId = "B", value = 0)
        updateSliderInput(session = session, inputId = "C", value = 0)
        updateSliderInput(session = session, inputId = "D", value = 0)
        shinyjs::hide(id = "pdfDisplay", anim = TRUE, animType = "slide", time = 0.2)
        database$correctionDone  <- FALSE
        database$correctionShown <- FALSE
        shinyjs::show(id = "generateCorrection")
        shinyjs::hide(id = "displayCorrection")
        shinyjs::enable("generateCorrection")
        if (input$pageSelection == "page2"){
            shinyjs::show(id = "parameters")
        }
    })
    
})
