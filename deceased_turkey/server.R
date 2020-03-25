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
        tagList("Cengiz Zopluoglu:", url)
    })
    
    myplot <- function(){
        x <- 1:10
        plot(x,x^2)  
    }
    
    output$plot <- eventReactive(input$submit,{
      myplot()
    })
    
    output$plot2 <- eventReactive(input$submit,{
        myplot()
    })
    

})
