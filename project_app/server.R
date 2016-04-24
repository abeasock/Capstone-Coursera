# Coursera - Capstone - Course Project

# server.R file for the shiny app

# This app was developed to predict the next word in a user's input

library(shiny)
library(stringr)

options(shiny.trace = TRUE)

shinyServer(function(input, output) {
  
  withProgress(message = 'Loading Data ...', value = NULL, {
    Sys.sleep(0.25)
    dat <- data.frame(x = numeric(0), y = numeric(0))
    withProgress(message = 'App Initializing', detail = "part 0", value = 0, {
      for (i in 1:10) {
        dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
        incProgress(0.1, detail = paste(":", i*10,"%"))
        if(i == 5){load("gram2.RData", .GlobalEnv)}
        if(i == 6){load("gram3.RData", .GlobalEnv)}
        if(i == 7){load("gram4.RData", .GlobalEnv)}
        if(i == 8){source("predict_model.R",echo=TRUE)}
        Sys.sleep(0.5)
      }
    })
    
    # Increment the top-level progress indicator
    incProgress(0.5)
  })
  
  output$predict <- renderText({
    
    output  <- predict(input$textinput)
    output2 <- paste(output[1],output[2],output[3],sep="\r\n")
    output2 <- str_replace_all(output2,'\r\nNA','')
    if (output2 != 'NA'){
      output2} 
    else {"Enter some words in the 'Text Input' to see next word predictions here"}
    })

  output$inputphrase <- renderText({input$textinput})
  
})