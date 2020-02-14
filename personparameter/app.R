
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
               
               sliderInput("n", label = h4("Select the Number of Items"), min = 5, max = 15, value = 15),
               
              # actionBttn("GenItem", 
              #            label = "Generate the Item Parameters",
              #            icon=icon("play"), 
              #            style="simple",
              #            color="primary",
              #            size = "sm"),
              
              selectInput("model", label = h4("Select the Model and Generate Item Parameters"), 
                          choices = list("1PL" = 1, 
                                         "2PL" = 2, 
                                         "3PL" = 3,
                                         "4PL" = 4), 
                          selected = 2),
              
               #checkboxInput("rasch", label = "Fix the discrimination parameters to 1.7", value = FALSE),
               #checkboxInput("disc", label = "Fix the discrimination parameters to be same", value = FALSE),
               #checkboxInput("guessing", label = "Fix the guessing parameters to 0", value = FALSE),
               #checkboxInput("slipping", label = "Fix the slipping parameters to 1", value = FALSE),
               

              
               mainPanel(tableOutput("ipar")),
               
               actionBttn("Genresp", 
                            label = "Simulate a New Response Vector",
                            icon=icon("play"), 
                            style="simple",
                           color="primary",
                            size = "xs"),
              
              
             # conditionalPanel(
                
             #    condition = "input.Genresp==1",
             #    textInput('resp', 'You can manipulate the response vector manuall below'),
             #    helpText("*The length of response vector should be same as the number of items you selected."),
             #    helpText("**The response vector should be comma separated."),
             #    helpText("*** The response vector should include only 0 (incorrect response) or 1 (correct response).")
             #  ) 
             
             mainPanel(
                 textInput('resp', 'You can manipulate the response vector manuall below'),
                 helpText("*The length of response vector should be same as the number of items you selected."),
                 helpText("**The response vector should be comma separated."),
                 helpText("*** The response vector should include only 0 (incorrect response) or 1 (correct response).")
               ) 
              
        ),
        
        column(8,
               mainPanel(
                   tabsetPanel(
                     
                       tabPanel("Guess MLE", align='center',
                                
                                br(),
                                br(),
                                
                                mainPanel(
                                  tableOutput("resp2")
                                  ),
                                
                                mainPanel(
                                  plotOutput('plot')
                                  ),
                                
                                br(),
                                br(),
                                
                                fluidRow(
                                  
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),

                                  
                                  column(4,
                                         fluidRow(
                                                  numericInput("theta.est", 
                                                               label = "", 
                                                               value = 0,
                                                               width='50%')
                                           ),
                                         
                                         fluidRow(
                                                  actionBttn("guess", 
                                                             label = "Guess", 
                                                             style="simple",
                                                             color="primary",
                                                             size = "sm")
                                           )
                                         ),
                                  
                                  column(8,
                                         'Output',
                                           tableOutput("guess")
                                         
                                         
                                         )
                                  
                                )
                                
                                
                                )
                       ),
                       
                       tabPanel("Summary"), 
                       tabPanel("Table")
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
    
    if(input$model==4){
      tab[,2] = rlnorm(input$n,0,.5)
      tab[,3] = rnorm(input$n,0,1)
      tab[,4] = runif(input$n,0,.25)
      tab[,5] = runif(input$n,0.9,1)
    }
    
    if(input$model==3){
      tab[,2] = rlnorm(input$n,0,.5)
      tab[,3] = rnorm(input$n,0,1)
      tab[,4] = runif(input$n,0,.25)
      tab[,5] = 1
    }
    
    if(input$model==2){
      tab[,2] = rlnorm(input$n,0,.5)
      tab[,3] = rnorm(input$n,0,1)
      tab[,4] = 0
      tab[,5] = 1
    }
    
    if(input$model==1){
      tab[,2] = rlnorm(1,0,.5)
      tab[,3] = rnorm(input$n,0,1)
      tab[,4] = 0
      tab[,5] = 1
    }
    
    tab 
    
  })
  
  
  ####################################################

  tabb.table <- reactive({
    
    tabb()
    
   })
  
   
   output$ipar  <- renderTable({tabb.table()})

  ####################################################

   respdf <- reactive({
     resp = rbinom(input$n,1,.5)
     resp
   })
   
   observe({
     
     updateTextInput(session, "resp", value = respdf())
     
   })
   
  
  observeEvent(input$Genresp,{
    
      ipars <- tabb()
      
      th = runif(1,-2,2)
      
      p <- function(theta,a,b,c,d){
        num = exp(a*(theta-b))
        c+((d-c)*(num/(1+num)))
      }
      
      rvec = c()
      
      for(j in 1:input$n){
        a=ipars[j,2]
        b=ipars[j,3]
        c=ipars[j,4]
        d=ipars[j,5]
        
        rvec[j] = rbinom(1,1,p(theta=th,a=ipars[j,2],b=ipars[j,3],c=ipars[j,4],d=ipars[j,5]))
      }
    
      updateTextInput(session, "resp", value = rvec)
  })

  respvec <- eventReactive(input$resp, {
    matrix(as.numeric(unlist(strsplit(input$resp,","))),nrow=input$n,ncol=1)
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
  
  guessm <- eventReactive(input$guess, {
    if(input$theta.est==1){
      matrix(1,nrow=5,ncol=2)
    } else {
      matrix(NA,nrow=5,ncol=2)
    }
  })
  
  output$guess <- renderTable({
    guessmatrix = guessm()
    guessmatrix
    })
    
  
  ####################################################
  
  
  output$plot <- renderPlot({
  
    ipars <- tabb()
    responses2 <- as.numeric(unlist(strsplit(input$resp,",")))
    
    p <- function(theta,a,b,c,d){
      num = exp(a*(theta-b))
      c+((d-c)*(num/(1+num)))
    }
    
    t <- seq(-4,4,.01)
    
    loglik <- c()
    
    for(i in 1:length(t)){
      
      pvec = c()
      
      for(j in 1:input$n){
        pvec[j] = p(theta=t[i],a=ipars[j,2],b=ipars[j,3],c=ipars[j,4],d=ipars[j,5])  
      }
      
      loglik[i] =   sum(log((responses2*pvec) + ((1-responses2)*(1-pvec))))
      
    }
    
    plot(t,loglik,type="l",lty=2,col="gray",xlab="Theta",ylab="Log-likelihood",
         ylim=c(min(loglik)*1.1,max(loglik)*.9))
    
  })
  
  ####################################################
}


# Run the application 
shinyApp(ui = ui, server = server)




















