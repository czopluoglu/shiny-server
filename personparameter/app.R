
library(shiny)
library(shinyWidgets)


##########################################################################################
loglikelihood <- function(ipar,resp,t){
  
  amat <- matrix(ipar[,2],nrow=length(t),ncol=nrow(ipar),byrow=TRUE)
  bmat <- matrix(ipar[,3],nrow=length(t),ncol=nrow(ipar),byrow=TRUE)
  cmat <- matrix(ipar[,4],nrow=length(t),ncol=nrow(ipar),byrow=TRUE)
  dmat <- matrix(ipar[,5],nrow=length(t),ncol=nrow(ipar),byrow=TRUE)
  tmat <- matrix(t,nrow=length(t),ncol=nrow(ipar),byrow=FALSE)
  num  = exp(amat*(tmat-bmat))
  pmat = cmat + (dmat-cmat)*(num/(1+num))
  r <- matrix(resp,nrow=length(t),ncol=nrow(ipar),byrow=TRUE)
  p = (pmat^r)*((1-pmat)^(1-r))
  rowSums(log(p))
}  

l1 <- function(ipar,resp,t){
  
  amat <- matrix(ipar[,2],nrow=length(t),ncol=nrow(ipar),byrow=TRUE)
  bmat <- matrix(ipar[,3],nrow=length(t),ncol=nrow(ipar),byrow=TRUE)
  cmat <- matrix(ipar[,4],nrow=length(t),ncol=nrow(ipar),byrow=TRUE)
  dmat <- matrix(ipar[,5],nrow=length(t),ncol=nrow(ipar),byrow=TRUE)
  tmat <- matrix(t,nrow=length(t),ncol=nrow(ipar),byrow=FALSE)
  x  = exp(amat*(bmat-tmat))
  r <- matrix(resp,nrow=length(t),ncol=nrow(ipar),byrow=TRUE)

  a=(-(amat*x*(dmat-cmat))*((r-cmat)*x+r-dmat))
  b=(x+1)*((cmat-1)*x+dmat-1)*(cmat*x+dmat)
  sum(a/b)
} 

# First derivative of 4PL with respect to theta
# Latex equation code
# -\dfrac{a\left(d-c\right)\mathrm{e}^{a\left(b-t\right)}\left(\left(r-c\right)\mathrm{e}^{a\left(b-t\right)}+r-d\right)}{\left(\mathrm{e}^{a\left(b-t\right)}+1\right)\left(\left(c-1\right)\mathrm{e}^{a\left(b-t\right)}+d-1\right)\left(c\mathrm{e}^{a\left(b-t\right)}+d\right)}

l2 <- function(ipar,resp,t){
  
  amat <- matrix(ipar[,2],nrow=length(t),ncol=nrow(ipar),byrow=TRUE)
  bmat <- matrix(ipar[,3],nrow=length(t),ncol=nrow(ipar),byrow=TRUE)
  cmat <- matrix(ipar[,4],nrow=length(t),ncol=nrow(ipar),byrow=TRUE)
  dmat <- matrix(ipar[,5],nrow=length(t),ncol=nrow(ipar),byrow=TRUE)
  tmat <- matrix(t,nrow=length(t),ncol=nrow(ipar),byrow=FALSE)
  x  = exp(amat*(bmat-tmat))
  r <- matrix(resp,nrow=length(t),ncol=nrow(ipar),byrow=TRUE)
  
  term1 = ((amat^2)*(dmat-cmat)*(r-cmat)*(x^2))/
            ((x+1)*((cmat-1)*x+dmat-1)*(cmat*x+dmat))
  
  term2 = ((amat^2)*(dmat-cmat)*((r-cmat)*x+r-dmat)*(x^2))/
            (((x+1)^2)*((cmat-1)*x+dmat-1)*(cmat*x+dmat))
  
  
  term3 = ((amat^2)*(cmat-1)*(dmat-cmat)*((r-cmat)*x+r-dmat)*(x^2))/
             ((x+1)*(((cmat-1)*x+dmat-1)^2)*(cmat*x+dmat))
  
  term4 = ((amat^2)*cmat*(dmat-cmat)*((r-cmat)*x+r-dmat)*(x^2))/
             ((x+1)*((cmat-1)*x+dmat-1)*((cmat*x+dmat)^2))
  
  term5 = ((amat^2)*(dmat-cmat)*x*((r-cmat)*x+r-dmat))/
             ((x+1)*((cmat-1)*x+dmat-1)*(cmat*x+dmat))
  
  sum(term1 - term2 - term3 - term4 + term5)
} 

# Second derivative of 4PL with respect to theta
# Latex equation code
# \dfrac{a^2\left(d-c\right)\left(r-c\right)\mathrm{e}^{2a\left(b-t\right)}}{\left(\mathrm{e}^{a\left(b-t\right)}+1\right)\left(\left(c-1\right)\mathrm{e}^{a\left(b-t\right)}+d-1\right)\left(c\mathrm{e}^{a\left(b-t\right)}+d\right)}-\dfrac{a^2\left(d-c\right)\left(\left(r-c\right)\mathrm{e}^{a\left(b-t\right)}+r-d\right)\mathrm{e}^{2a\left(b-t\right)}}{\left(\mathrm{e}^{a\left(b-t\right)}+1\right)^2\left(\left(c-1\right)\mathrm{e}^{a\left(b-t\right)}+d-1\right)\left(c\mathrm{e}^{a\left(b-t\right)}+d\right)}-\dfrac{a^2\left(c-1\right)\left(d-c\right)\left(\left(r-c\right)\mathrm{e}^{a\left(b-t\right)}+r-d\right)\mathrm{e}^{2a\left(b-t\right)}}{\left(\mathrm{e}^{a\left(b-t\right)}+1\right)\left(\left(c-1\right)\mathrm{e}^{a\left(b-t\right)}+d-1\right)^2\left(c\mathrm{e}^{a\left(b-t\right)}+d\right)}-\dfrac{a^2c\left(d-c\right)\left(\left(r-c\right)\mathrm{e}^{a\left(b-t\right)}+r-d\right)\mathrm{e}^{2a\left(b-t\right)}}{\left(\mathrm{e}^{a\left(b-t\right)}+1\right)\left(\left(c-1\right)\mathrm{e}^{a\left(b-t\right)}+d-1\right)\left(c\mathrm{e}^{a\left(b-t\right)}+d\right)^2}+\dfrac{a^2\left(d-c\right)\mathrm{e}^{a\left(b-t\right)}\left(\left(r-c\right)\mathrm{e}^{a\left(b-t\right)}+r-d\right)}{\left(\mathrm{e}^{a\left(b-t\right)}+1\right)\left(\left(c-1\right)\mathrm{e}^{a\left(b-t\right)}+d-1\right)\left(c\mathrm{e}^{a\left(b-t\right)}+d\right)}

#require(numDeriv)


#grad <- function(ipar,resp,t){
#  
#  temp = loglikelihood(ipar=ipar,resp=resp,t=t)
#  temp2 = loglikelihood(ipar=ipar,resp=resp,t=t+.001)
#  
#  (temp2-temp)/(.001)
#}

#hess <- function(ipar,resp,t){
#  
#  temp  = grad(ipar=ipar,resp=resp,t=t)
#  temp2 = grad(ipar=ipar,resp=resp,t=t+.001)
#  
#  (temp2-temp)/.001
#}


#th = 1
#numDeriv::grad(func=loglikelihood,x = th,ipar=ipar,resp=resp)
#grad(ipar=ipar,resp=resp,t=th)
#l1(ipar=ipar,resp=resp,t=th)

#hessian(func=loglikelihood,x = th,ipar=ipar,resp=resp)
#hess(ipar=ipar,resp=resp,t=th)
#l2(ipar=ipar,resp=resp,t=th)


##########################################################################################

ui <- fluidPage(
  
  tags$head(tags$style("#nstep{color: black;
                                 font-size: 14px;
                                 font-weight: bold;
                                 }"
  )
  ),
  
  tags$head(tags$style("#maxlogL{color: black;
                                 font-size: 20px;
                                 }"
  )
  ),
  
  
    
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
                                
                                fluidRow(
                                  column(12,align='center',
                                         mainPanel(
                                           tableOutput("resp2")
                                           )
                                  )
                                ),
                                
                                fluidRow(
                                  column(12,align='center',
                                         mainPanel(
                                           plotOutput('plot',width = "100%")
                                           )
                                  )
                                ),
                                
                                br(),
                                br(),
                                br(),
                                br(),
                                
                                fluidRow(
                                  
                                  br(),
                                
                                  textOutput("maxlogL"),
                                  
                                  br(),
                                  br(),
                                  br(),
                                  
                                  column(3,
                                         
                                         fluidRow(
                                                  numericInput("theta.est", 
                                                               label = "", 
                                                               value = NULL,
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
                                  
                                  column(3,
                                           tableOutput("guess")
                                         ),
                                  
                                  column(3,
                                         " "
                                  ),
                                  
                                  column(3,
                                         textOutput("nstep"),
                                         tableOutput("guess2")
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
  
  values <- reactiveValues(df_data = data.frame(matrix(NA,nrow=1,ncol=3)),
                           df_data2 = data.frame(matrix(NA,nrow=1,ncol=2)),
                           step = 0,
                           step2= 0
                           )
  
                    
  guessm <- eventReactive(input$guess, {
    if(values$step==0){
      colnames(values$df_data) <- c("Theta","Loglikelihood","Slope of Tangent Line")
    }
    
    values$step = values$step+1
    
    values$df_data[values$step,1]= input$theta.est
    
    ip <- tabb()
    r  <- as.numeric(unlist(strsplit(input$resp,",")))
    
    values$df_data[values$step,2]= loglikelihood(ipar=ip,resp=r,t=input$theta.est)
    
    values$df_data[values$step,3]= l1(ipar=ip,resp=r,t=input$theta.est)
    
    
    values$df_data
  })
  
  output$guess <- renderTable(digits=5,bordered=TRUE,striped=TRUE,{
   guessm()
  })
  
  ####################################################
  
  guessm2 <- eventReactive(input$guess, {
    if(values$step2==0){
      colnames(values$df_data2) <- c("Theta","Loglikelihood")
    }
    
    values$step2 = values$step2+1
    
    values$df_data2[values$step2,1]= input$theta.est
    
    ip <- tabb()
    r  <- as.numeric(unlist(strsplit(input$resp,",")))
    
    values$df_data2[values$step2,2]= loglikelihood(ipar=ip,resp=r,t=input$theta.est)
    
    values$df_data2
  })
  
  output$guess2 <- renderTable(digits=3,{
    guessm2()
  })
    
  
  ####################################################
  
  my.maxlogL <- eventReactive(input$guess,{
    ipars <- tabb()
    responses2 <- as.numeric(unlist(strsplit(input$resp,",")))
    
    t <- seq(-4,4,.00005)
    loglik <- loglikelihood(ipar=ipars,resp=responses2,t=t)
    
    maxL = round(max(loglik),5)
    
    paste0("The maximum value of loglikelihood function is ",maxL,
           ". The slope of tangent line at the maximum point is 0. 
           Find which theta value makes the loglikelihood maximum. 
           Guess as many times as you like. Let the slope of tangent line guide you for your next guess.")
  })
  
  output$maxlogL <- renderText({
    my.maxlogL()
    })
  
  
  output$nstep <- renderText({
    paste0("Number of Guesses = ", values$step) 
  })
  
  ####################################################
  
  myplot <- eventReactive(input$guess,{
     
    ip <- tabb()
    r <- as.numeric(unlist(strsplit(input$resp,",")))
    t <- seq(-4,4,.001)
    
    loglik <- loglikelihood(ipar=ip,resp=r,t=t)
    
    minn=min(loglik)
    maxx=max(loglik)
    rangex=maxx-minn
    
    plot(t,loglik,type="l",lty=2,xlab="Theta",ylab="Log-likelihood",
         ylim=c(min(loglik)-rangex/4,max(loglik)+rangex/3))
    
    slope = l1(ipar=ip,resp=r,t=input$theta.est)
    x0 = input$theta.est
    y0 = loglikelihood(ipar=ip,resp=r,t=input$theta.est)
    int = y0 - x0*slope
    
    points(x0,y0,pch=16,cex=1.4)
    
    segments(x0=input$theta.est,y0=min(loglik)-rangex/4,
             x1=input$theta.est,y1=y0,
             lty=2,col="gray")
    
    segments(x0=input$theta.est,y0=y0,
             x1=t[1],y1=y0,
             lty=2,col="gray")
    
    abline(coef=c(int,slope),lty=2,col="gray")
  })
  
  output$plot <- renderPlot({
    myplot()}
    ,height = 500, width = 800)
  
  ####################################################
}


# Run the application 
shinyApp(ui = ui, server = server)




















