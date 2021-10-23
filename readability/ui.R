library(shiny)
library(shinyWidgets)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    tags$head(tags$style("#nstep{color: black;
                                 font-size: 14px;
                                 font-weight: bold;
                                 }"
    )
    ),
    
    tags$head(tags$style("#maxlogL{color: black;
                                 font-size: 20px;
                                 }"
    )
    ),
    
    tags$head(tags$style("#nr{color: white;
                                 font-size: 1px;
                                 }"
    )
    ),
    
    
    
    setBackgroundColor(
      color = c("#ffffff", "#ffffff"),
      gradient = "linear",
      direction = "bottom"
    ),
    
    titlePanel(""),
  
    fluidRow(
      column(3,
             'Put here information about the app and Kaggle competition'),
      
      column(5,
             textAreaInput(
               'mytext',
               label ='Enter the text in the box',
               value = "",
               width = '800px',
               height= '400px',
               cols = 80,
               rows = 10,
               resize = 'none')
             ),
      
      column(4,
             
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             
             fluidRow(
              selectInput('model',
                           label='Select the prediction model',
                           choices = c('Ridge','Lasso')),
              br(),

              actionBttn("predict", 
                          label = "Predict the Readability Score",
                          icon=icon("play"), 
                          style="simple",
                          color="primary",
                          size = "sm"),
             ),
             
             br(),
             
             fluidRow(
              tableOutput("guess"), 
              )
            ),
    ),
    
    fluidRow(
      column(3,
             "Contact information",
             ),
      column(5,
             'What do these scores mean?'
             ),
      column(4,
             'General info about model building and features'
             )
    )
  
))
