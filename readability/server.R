library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  values <- reactiveValues(
    df_data = data.frame(matrix(NA,nrow=1,ncol=1))
  )
  
  guessm <- eventReactive(input$predict, {
    colnames(values$df_data) <- c("Score")
    values$df_data[1,1] = 0
    values$df_data
  })
  
  output$guess <- renderTable(digits=3,bordered=FALSE,striped=FALSE,{
    guessm()
  })

})
