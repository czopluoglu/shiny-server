
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
               
              # actionBttn("GenItem", 
              #            label = "Generate the Item Parameters",
              #            icon=icon("play"), 
              #            style="simple",
              #            color="primary",
              #            size = "sm"),
              
              selectInput("model", label = h3("Select the Model and Generate Item Parameters"), 
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
                          label = "Generate the Response Vector",
                          icon=icon("play"), 
                          style="simple",
                          color="primary",
                          size = "sm"),
               
              conditionalPanel(
                
                condition = "input.Genresp==1",
                textInput('resp', 'You can manipulate the response vector manuall below'),
                helpText("*The length of response vector should be same as the number of items you selected."),
                helpText("**The response vector should be comma separated."),
                helpText("*** The response vector should include only 0 (incorrect response) or 1 (correct response).")
                
                
              ) 
              
        ),
        
        column(8,
               mainPanel(
                   tabsetPanel(
                     
                       tabPanel("Guesstimate", align='center',
                                
                                br(),
                                br(),
                                
                                conditionalPanel(
                                  condition = "input.Genresp==1",
                                  tableOutput("resp2")),
                                
                                conditionalPanel(
                                  condition = "input.Genresp==1",
                                  plotOutput('plot')
                                ),
                                
                                br(),
                                br(),
                                br(),
                                
                                fluidRow(
                                  column(4,
                                         conditionalPanel(
                                           h3("Guess Theta"),
                                           condition = "input.Genresp==1",
                                           numericInput("theta.est", 
                                                        label = "", 
                                                        value = NULL,
                                                        width='40%'),
                                            actionBttn("guess", 
                                                       label = "Run", 
                                                       style="simple",
                                                       color="primary",
                                                       size = "xs"),
                                           
                                         )
                                         ),
                                  
                                  column(8,
                                         "A"
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
   
  
  observe({
    
      x <- input$n
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
  
  
  output$plot <- renderPlot({
  
    ipars <- tabb()
    responses2 <- as.numeric(unlist(strsplit(input$resp,",")))
    
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
      
      loglik[i] =   sum(log((responses2*pvec) + ((1-responses2)*(1-pvec))))
      
    }
    
    plot(t,loglik,type="l",lty=2,col="gray",xlab="Theta",ylab="Log-likelihood")
    
  })
  
  ####################################################
}


# Run the application 
shinyApp(ui = ui, server = server)




















