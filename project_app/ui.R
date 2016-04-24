# Coursera - Capstone - Course Project

# ui.R file for the shiny app

# This app was developed to predict the next word in a user's input

library(shiny)
library(markdown)

shinyUI(fluidPage(
  navbarPage("Word Prediction App", 
  
    tabPanel("App", 
      sidebarPanel(
        p("This app will attempt to predict the next word given the preceding words in a user's input."),
        p("Type in two or more words into input box and hit 'Submit'."),
  
        textInput("textinput", "Text Input:", value="Example: I’ll be on my"),
  
        submitButton('Submit'),
  
        hr(),
        h4("Important Info"),
        p("Please wait for the app to initialize at the first load. After", code("100% loading"), 
          " the 'initializing message' in the upper right corner will disappear and you may begin using the app once
            see words in the", code("Prediction Next Word"), "output box. This may take up to 30 seconds.",
            style="color:#0000FF"),
        p("An example set of words are in the input box and predictions will be displayed based on these by 
            default when the app is initialized"),
        h6("*Only supports the English language.", style="color:#FF0000"),
        hr(),
        
        h4("Purpose of App"),
        p("This app was built to meet the requirements for the Data Science Capstone Project to complete the", 
        a("Data Science Certification", href="https://www.coursera.org/course/dsscapstone"), 
            "by John Hopkins University through Coursera."),
        hr(),
        
        h4("Author"),
        p("For more information about Amber Beasock:"),
        a(img(src = "website.png", height = 30, width = 30),href="http://amberbeasock.com"),  
        a(img(src = "github.png", height = 30, width = 30),href="https://github.com/abeasock"),
        a(img(src = "linkedin.png", height = 26, width = 26),href="https://www.linkedin.com/in/abeasock"),
        br()
        ),
      
      mainPanel(
          h4('Predicted Next Word (Top 3 Possibilities):'),
          fluidRow(verbatimTextOutput("predict")),
    
          h4('You Entered:'),
          fluidRow(verbatimTextOutput("inputphrase")),
          br()),
      
          
          h4('Examples to Try:'),
          p("I have no"),
          p("How are you"),
          p("Wait a couple of"),
          p("He traveled around the")
    ),
    
    tabPanel("How To Use", includeHTML("how_to_use.html")),
      
    navbarMenu("Documentation",
      tabPanel("Summary", includeMarkdown("summary.md")),
      tabPanel("Final Report", includeHTML("final.html"))
      )
    )
  )
)


