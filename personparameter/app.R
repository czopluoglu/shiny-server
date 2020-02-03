
library(shiny)
library(shinyWidgets)

##########################################################################################

ui <- fluidPage(
    
    setBackgroundColor(
        color = c("#ffffff", "#ffffff"),
        gradient = "linear",
        direction = "bottom"
    ),
    
    titlePanel(""),
    
    fluidRow(
        column(4,
               
               sliderInput("n", label = h4("Select the Number of Items"), min = 5, max = 10, value = 10),
               
               actionBttn("GenItem", 
                          label = "Generate the Item Parameters",
                          icon=icon("play"), 
                          style="simple",
                          color="primary",
                          size = "sm"),
               
               checkboxInput("guessing", label = "Fix the guessing parameters to 0", value = TRUE),
               checkboxInput("slipping", label = "Fix the slipping parameters to 1", value = TRUE),
               
               mainPanel(tableOutput("ipar")),
               
               actionBttn("Genresp", 
                          label = "Enter the Response Vector",
                          icon=icon("play"), 
                          style="simple",
                          color="primary",
                          size = "sm"),
               
              conditionalPanel(
                
                condition = "input.Genresp==1",
                textInput('resp', 'Enter a response vector'),
                helpText("*The length of response vector should be same as the number of items you selected."),
                helpText("**The response vector should be comma separated."),
                helpText("*** The response vector should include only 0 (incorrect response) or 1 (correct response).")
                
                
              ) 
               
 
              
        ),
        
        column(8,
               mainPanel(
                   tabsetPanel(
                       tabPanel("Guesstimate",
                                
                                conditionalPanel(
                                  condition = "input.Genresp==1",
                                  tableOutput("resp2")),
                                
                                
                                conditionalPanel(
                                  condition = "input.Genresp==1",
                                  plotOutput('plot')
                                )
                                
                                ),
                       
                       tabPanel("Summary"), 
                       tabPanel("Table")
                       )
               )
        )
    )
)

#########################################################################

server <- function(input, output,session) {
  
  tabb <- reactive({
    
    tab     = data.frame(matrix(nrow=input$n,ncol=5))
    tab[,1] = 1:input$n
    colnames(tab) <- c("Item","a","b","c","d")
    
    tab[,2] = rlnorm(input$n,0,.5)
    tab[,3] = rnorm(input$n,0,1)
    
    if(input$guessing==FALSE){
      tab[,4] = runif(input$n,0,.25)
    } else {
      tab[,4]=0
    }
    
    if(input$slipping==FALSE){
      tab[,5] = runif(input$n,0.9,1)
    } else {
      tab[,5]=1
    }
    
    tab 
    
  })
  
  
  ####################################################

  tabb.table <- eventReactive(input$GenItem, {

    tabb()
    
   })
  
   
   output$ipar  <- renderTable({tabb.table()})

  ####################################################

  
  observe({
      x <- input$n
      updateTextInput(session, "resp", value = c(1,rep(0,x-1)))
  })
    
  respvec <- eventReactive(input$resp, {
    as.numeric(unlist(strsplit(input$resp,",")))
  })
  
  output$resp2  <- renderTable({
    
    responses = t(respvec())
    
    namess = "Item1"
    for(i in 2:input$n){
      namess = c(namess,paste0("Item",i))
    }
    
    colnames(responses) <- namess
    
    responses
    
    })
  
  ####################################################
  
  
  output$plot <- renderPlot({
  
    ipars <- tabb()
    responses <- as.numeric(unlist(strsplit(input$resp,",")))
    
    p <- function(theta,a,b,c,d){
      num = exp(a*(theta-b))
      c+((d-c)*(num/(1+num)))
    }
    
    t <- seq(-3,3,.01)
    
    loglik <- c()
    
    for(i in 1:length(t)){
      
      pvec = c()
      
      for(j in 1:input$n){
        pvec[j] = p(theta=t[i],a=ipars[j,2],b=ipars[j,3],c=ipars[j,4],d=ipars[j,5])  
      }
      
      loglik[i] =   sum(log((responses*pvec) + ((1-responses)*(1-pvec))))
      
    }
    
    plot(t,loglik,type="l",lty=2,col="gray")
    
  })
  
  ####################################################
}


# Run the application 
shinyApp(ui = ui, server = server)




















