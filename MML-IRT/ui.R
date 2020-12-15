library(shiny)
library(plotly)
library(shinyWidgets)


shinyUI(
  
navbarPage("Exploring Loglikelihood Surface for IRT Models",
             
  tabPanel("Unidimensional Item Response Models",
           
           column(2,
                  
                  fluidRow(
                    tags$h4('ENTER THE ITEM PARAMETERS:')
                  ),
                  
                  fluidRow(
                    
                    numericInput(inputId = "a", "Item Discrimination", 1, min = 0.2, max = 4),
                    numericInput(inputId = "b", "Item Difficulty", 0, min = -3, max = 3),
                    numericInput(inputId = "g", "Lower Bound", 0, min = 0, max = .3),
                    numericInput(inputId = "d", "Upper Bound", 1, min = 0.7, max = 1),
                    
                    HTML("<br>"),
                    
                    selectInput(inputId='xaxis','Choose the X-axis parameter',c('a','b','g','d'),selected = 'a'),
                    selectInput(inputId='yaxis','Choose the Y-axis parameter',c('a','b','g','d'),selected = 'b'),
                    
                    ),
                  
                  br(),
                  
                  fluidRow(
                    
                    actionBttn("draw", 
                               label = "Draw Loglikelihood Surface",
                               style="simple",
                               color="primary",
                               size = "md"),
                    
                    tags$h5('Calculations take 10-12 seconds...'),
                  ),
                  
                  hr(),
                  
                  fluidRow(
                    
                    HTML('<p><b>For questions and feedback:</b></p>'),
                    HTML('<p>Cengiz Zopluoglu <cengiz@uoregon.edu> </p>'),
                    HTML('<p>cengiz@uoregon.edu</p>'),
                    HTML("<br>"),
                    HTML('<a href="https://cengiz.me/"> Personal Website </a>'),
                    HTML("<br>"),
                    HTML("<br>"),
                    HTML('<a href="https://education.uoregon.edu/qrme"> Quantitative Research Methods Program </a>'), 
                    HTML("<br>"),
                    HTML("<br>"),
                    HTML('<a href="https://education.uoregon.edu/"> College of Education at UO </a>')
                    
                    
                  ),
                  
                  
           ),
           
           
           
           column(width=5,
                  plotlyOutput('plot2',width = '200%',height='100%'),
                  hr(),
                  verbatimTextOutput('info')
           ),
           
           
           
           column(width=5,
                  plotlyOutput('plot',width = '200%',height='100%'),
           )
  ),
  
             
  
  
  tabPanel("Compensatory Multidimensional Item Response Models"),
             
  
  
  tabPanel("Partially Compensatory Multidimensional Item Response Models")
  
  
)
)


