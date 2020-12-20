library(shiny)
library(plotly)
library(shinyWidgets)


shinyUI(
  
navbarPage("Exploring Loglikelihood Surface for IRT Models",
             
  tabPanel("Unidimensional Item Response Model",
           
           column(2,
                  
                  fluidRow(
                    tags$h4('ENTER THE ITEM PARAMETERS:')
                  ),
                  
                  fluidRow(
                    
                    numericInput(inputId = "a", "Item Discrimination (a)", 1, min = 0.2, max = 4),
                    numericInput(inputId = "b", "Item Difficulty (b)", 0, min = -3, max = 3),
                    numericInput(inputId = "g", "Lower Bound (g)", 0, min = 0, max = .3),
                    numericInput(inputId = "d", "Upper Bound (d)", 1, min = 0.7, max = 1),
                    
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
  
             
  
  
  tabPanel("Compensatory Two-Dimensional Item Response Model",
           
           column(2,
                  
                  fluidRow(
                    tags$h4('ENTER THE ITEM PARAMETERS:')
                  ),
                  
                  fluidRow(
                    
                    numericInput(inputId = "a1", "Item Discrimination - Dimension 1 (a1)", 1, min = 0.2, max = 4),
                    numericInput(inputId = "a2", "Item Discrimination - Dimension 2 (a2)", 1, min = 0.2, max = 4),
                    numericInput(inputId = "c", "Item Intercept (c)", 0, min = -3, max = 3),
                    numericInput(inputId = "g_c", "Lower Bound (g)", 0, min = 0, max = .3),
                    numericInput(inputId = "d_c", "Upper Bound (d)", 1, min = 0.7, max = 1),
                    numericInput(inputId = "rho", "Correlation between dimensions", 0, min = 0, max = 1),
                    
                    
                    HTML("<br>"),
                    
                    selectInput(inputId='xaxis_c','Choose the X-axis parameter',c('a1','a2','c','g','d'),selected = 'a1'),
                    selectInput(inputId='yaxis_c','Choose the Y-axis parameter',c('a1','a2','c','g','d'),selected = 'a2'),
                    
                  ),
                  
                  br(),
                  
                  fluidRow(
                    
                    actionBttn("draw2", 
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
                  plotlyOutput('plot4',width = '200%',height='100%'),
                  hr(),
                  verbatimTextOutput('info2')
           ),
           
           
           
           column(width=5,
                  plotlyOutput('plot3',width = '200%',height='100%'),
           )
           
  ),
             
  
  
  tabPanel("Partially Compensatory Two-Dimensional Item Response Models",
           
           column(2,
                  
                  fluidRow(
                    tags$h4('ENTER THE ITEM PARAMETERS:')
                  ),
                  
                  fluidRow(
                    
                    numericInput(inputId = "a1_pc", "Item Discrimination - Dimension 1 (a1)", 1, min = 0.2, max = 4),
                    numericInput(inputId = "a2_pc", "Item Discrimination - Dimension 2 (a2)", 1, min = 0.2, max = 4),
                    numericInput(inputId = "b1_pc", "Item Difficulty - Dimension 1 (b1)", 0, min = -3, max = 3),
                    numericInput(inputId = "b2_pc", "Item Difficulty - Dimension 2 (b2)", 0, min = -3, max = 3),
                    numericInput(inputId = "g_pc", "Lower Bound (g)", 0, min = 0, max = .3),
                    numericInput(inputId = "d_pc", "Upper Bound (d)", 1, min = 0.7, max = 1),
                    numericInput(inputId = "rho_pc", "Correlation between dimensions", 0, min = 0, max = 1),
                    
                    
                    HTML("<br>"),
                    
                    selectInput(inputId='xaxis_pc','Choose the X-axis parameter',c('a1','a2','b1','b2','g','d'),selected = 'a1'),
                    selectInput(inputId='yaxis_pc','Choose the Y-axis parameter',c('a1','a2','b1','b2','g','d'),selected = 'a2'),
                    
                  ),
                  
                  br(),
                  
                  fluidRow(
                    
                    actionBttn("draw3", 
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
                  plotlyOutput('plot6',width = '200%',height='100%'),
                  hr(),
                  verbatimTextOutput('info3')
           ),
           
           
           
           column(width=5,
                  plotlyOutput('plot5',width = '200%',height='100%'),
           )
           
  )
  
  
)
)


