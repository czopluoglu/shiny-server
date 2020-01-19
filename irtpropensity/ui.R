library(shiny)

shinyUI(
    fluidPage(

        titlePanel("2-parameter Normal Ogive Item Response Model"),
        
        plotOutput('plot'),
        
        hr(),
        
        fluidRow(
            
            column(6,
                   fluidRow(
                       column(6, 
                              sliderInput('lambda', 'Item Factor Loading (\\(\\lambda)\\)', 
                                          min=.05, max=.95, value=.5, 
                                          step=.01)
                              ),
                       column(6,
                              sliderInput('threshold', 'Item Threshold (\\(\\tau)\\)', 
                                          min=-4, max=4, value=0, 
                                          step=0.1)
                              )
                       ),
                   
                   hr(),
                  
                   fluidRow(
                              mainPanel(
                                  withMathJax(),
                                  #helpText("$$X_{ij}^{*}=\\lambda_jF_i+\\epsilon_{ij}$$"),
                                  uiOutput('restext'),
                                  uiOutput('a'),
                                  uiOutput('b'),
                                  #helpText("$$P(X_{ij}^* > \\tau|F,\\lambda)=P(X_{ij}=1|F,\\lambda,\\tau)=\\int_{-z}^{\\infty}\\frac{e^{-t^2}}{2}dt$$"),
                                  #helpText("$$z=a*(F-b)$$")
                              )
                       )
                   ),
            
            column(width = 6,
                   plotOutput('plot2'),
                   )
            )
        )
    )

