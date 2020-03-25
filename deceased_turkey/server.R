#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    url <- a("@cengizzopluoglu", href="https://twitter.com/cengizzopluoglu")
    output$info <- renderUI({
        tagList("Twitter:", url)
    })
    
    url2 <- a("cengiz.me", href="https://cengiz.me/")
    output$info2 <- renderUI({
      tagList("Website:", url2)
    })
    
    url3 <- a("https://www.turkiye.gov.tr/", href="https://www.turkiye.gov.tr/")
    output$info3 <- renderUI({
      tagList("Source:", url3)
    })
    
    output$finalupdate <- renderText({
      # Read the final date from the data
      finaldate = paste0("Final Update: ", "Mar 22, 2020")
    })
    
    
    ##############################################################################
    
    values <- reactiveValues(df_data = data.frame(matrix(NA,nrow=11,ncol=2)),
                             df_data2 = data.frame(matrix(NA,nrow=11,ncol=2)),
                             df_data3 = data.frame(matrix(NA,nrow=11,ncol=2)),
                             df_data4 = data.frame(matrix(NA,nrow=11,ncol=2))
                             )
    
    ##############################################################################
    
    myplot <- eventReactive(req(isTruthy(input$submit)),{
       x = 1:10
       plot(x,x^2)
    })
    
    output$plot <- renderPlot({
      myplot()
    },height = 450, width = 700)
   
    tab1 <- eventReactive(req(isTruthy(input$submit)), {
      values$df_data
    })
    
    output$table1 <- renderTable(digits=5,bordered=TRUE,striped=TRUE,{
      tab1()
    })
    
    ################################################################################
    
    myplot2 <- eventReactive(req(isTruthy(input$submit)),{
      x = 1:10
      plot(x,x^2)
    })
    
    output$plot2 <- renderPlot({
      myplot2()
    },height = 450, width = 700)
    
    tab2 <- eventReactive(req(isTruthy(input$submit)), {
      values$df_data2
    })
    
    output$table2 <- renderTable(digits=5,bordered=TRUE,striped=TRUE,{
      tab2()
    })
    
    #################################################################################
    
    myplot3 <- eventReactive(req(isTruthy(input$submit2)),{
      x = 1:10
      plot(x,x^2)
    })
    
    output$plot3 <- renderPlot({
      myplot3()
    },height = 450, width = 700)
    
    tab3 <- eventReactive(req(isTruthy(input$submit2)), {
      values$df_data3
    })
    
    output$table3 <- renderTable(digits=1,bordered=TRUE,striped=TRUE,{
      tab3()
    })
    
    ###################################################################################

    myplot4 <- eventReactive(req(isTruthy(input$submit2)),{
      x = 1:10
      plot(x,x^2)
    })
    
    output$plot4 <- renderPlot({
      myplot4()
    },height = 450, width = 700)
    
    tab4 <- eventReactive(req(isTruthy(input$submit2)), {
      values$df_data4
    })
    
    output$table4 <- renderTable(digits=1,bordered=TRUE,striped=TRUE,{
      tab4()
    })
    
})
