
setwd("B:/shinyserver/shiny-server/ridge")

#setwd("/srv/shiny-server/deceased_turkey/data/")

d <- read.csv('readability_feat_sub.csv')

require(shiny)
require(plotly)
require(tidyr)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Regularized Regression with Ridge Penalty"),
    
    HTML('<p>Calculations may take 10-15 seconds for the plot to show up after entering a new value.</p>'),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          
          HTML("<b>Ridge Regression</b>: Enter 0 for the Elastic Net mixing parameter and a positive lambda value for the penalty term."),
          HTML("<br>"),
          HTML("<br>"),
          HTML("<b>Lasso Regression</b>: Enter 1 for the Elastic Net mixing parameter and a positive lambda value for the penalty term."),
          HTML("<br>"),
          HTML("<br>"),
          HTML("<b>Elastic Net</b>: Enter a value between 0 and 1 for the Elastic Net mixing parameter and a positive lambda value for the penalty term."),
          HTML("<br>"),
          HTML("<br>"),

          numericInput("mix", 
                         label = "Elastic Net Mixing Parameter (Alpha)", 
                         value = '',
                         width='50%'),
          
          numericInput("lambda", 
                       label = "Lambda", 
                       value = '',
                       width='50%'),
            
            textOutput("ssr"),
            textOutput("b0"),
            textOutput("b1"),
            HTML("<br>"),
            HTML("<br>"),
            HTML("<br>"),
            HTML("<br>"),
            HTML("<br>"),
            HTML("<br>"),
            HTML('<p>Cengiz Zopluoglu <cengiz@uoregon.edu> </p>'),
            HTML('<p>cengiz@uoregon.edu</p>'),
            HTML("<br>"),
            HTML("<br>"),
            HTML('<a href="https://cengiz.me/"> Personal Website </a>'),
            HTML("<br>"),
            HTML("<br>"),
            HTML('<a href="https://education.uoregon.edu/qrme"> Learn about the Quantitative Research Methods Program </a>'), 
            HTML("<br>"),
            HTML("<br>"),
            HTML('<a href="https://education.uoregon.edu/"> Learn about the College of Education at UO </a>')
            
            
            ),

        # Show a plot of the generated distribution
        mainPanel(
          plotlyOutput('plot',width = '100%',height='100%')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  myplot <- eventReactive(c(input$lambda,input$mix),{

      grid    <- expand.grid(b0=seq(-10,10,.1),b1=seq(-5,5,.01))           
      grid$SSR <- NA
      
      B1    <- matrix(grid$b1,ncol=20,nrow=nrow(grid),byrow=FALSE)
      B0    <- matrix(grid$b0,ncol=20,nrow=nrow(grid),byrow=FALSE)
      X     <- matrix(d$mean.wl,ncol=20,nrow=nrow(grid),byrow=TRUE)
      Y_hat <- B0 + X*B1
      Y     <- matrix(d$target,ncol=20,nrow=nrow(grid),byrow=TRUE)
      
      P1     <- grid$b0^2 + grid$b1^2
      P2     <- abs(grid$b0) + abs(grid$b1)
      
      P      <- input$lambda*((1-input$mix)*P1 + input$mix*P2)
      
      grid$SSR <- rowSums((Y - Y_hat)^2) + P

      pl <- plot_ly(grid, x = ~b0, y = ~b1, z = ~SSR, 
              marker = list(color = ~SSR,
                            showscale = TRUE,
                            cmin=min(grid$SSR),
                            cmax=max(grid$SSR),cauto=F),
              width=600,height=600) %>% 
        add_markers()
      
      return(list(pl=pl,
                  SSR = grid[which.min(grid$SSR),]$SSR,
                  b0  = grid[which.min(grid$SSR),]$b0,
                  b1  = grid[which.min(grid$SSR),]$b1))
    })
  
  output$plot <- renderPlotly({
    myplot()$pl
  })
  
  output$ssr <- renderText({
    paste0("Minimum SSR = ",round(myplot()$SSR,2)) 
  })
  
  output$b0 <- renderText({
    paste0("Estimated B0 = ",round(myplot()$b0,2)) 
  })
  
  output$b1 <- renderText({
    paste0("Estimated B1 = ",round(myplot()$b1,2)) 
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
