library(shiny)
library(shinyWidgets)

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

shinyUI(

  fluidPage(
    
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
    
    tags$head(tags$style("#nr{color: white;
                                 font-size: 1px;
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
             
             fluidRow(
               column(12,
                      actionBttn("reset", 
                                 label = "RESET EVERYTHING",
                                 style="simple",
                                 color="primary",
                                 size = "md")
               )
             ),
             
             br(),
             
             sliderInput("n", label = h4("Select the Number of Items"), min = 5, max = 15, value = 15),
             
             # actionBttn("GenItem", 
             #            label = "Generate the Item Parameters",
             #            icon=icon("play"), 
             #            style="simple",
             #            color="primary",
             #            size = "sm"),
             
             fluidRow(
               column(12,
                      selectInput("model", label = h4("Select the Model"), 
                                  choices = list("1PL" = 1, 
                                                 "2PL" = 2, 
                                                 "3PL" = 3,
                                                 "4PL" = 4), 
                                  selected = 2),
               )
             ),
             
             #checkboxInput("rasch", label = "Fix the discrimination parameters to 1.7", value = FALSE),
             #checkboxInput("disc", label = "Fix the discrimination parameters to be same", value = FALSE),
             #checkboxInput("guessing", label = "Fix the guessing parameters to 0", value = FALSE),
             #checkboxInput("slipping", label = "Fix the slipping parameters to 1", value = FALSE),
             
             fluidRow(
               column(12,
                      mainPanel(tableOutput("ipar"))
               )
             ),
             
             
             fluidRow(
               column(12,
                      actionBttn("Genresp", 
                                 label = "Simulate a New Response Vector",
                                 icon=icon("play"), 
                                 style="simple",
                                 color="primary",
                                 size = "sm")
               )
             ),
             
             
             # conditionalPanel(
             
             #    condition = "input.Genresp==1",
             #    textInput('resp', 'You can manipulate the response vector manuall below'),
             #    helpText("*The length of response vector should be same as the number of items you selected."),
             #    helpText("**The response vector should be comma separated."),
             #    helpText("*** The response vector should include only 0 (incorrect response) or 1 (correct response).")
             #  ) 
             
             fluidRow(
               column(12,
                      mainPanel(
                        textInput('resp', 'You can manipulate the response vector manually below')
                      )
               )
             )
             
      ),
      
      column(8,
             mainPanel(
               tabsetPanel(
                 
                 tabPanel("Guess MLE", align='center',
                          
                          fluidRow(
                            column(12,align='center',
                                   mainPanel(
                                     tableOutput("resp2")
                                   )
                            )
                          ),
                          
                          actionBttn("start", 
                                     label = "Draw Loglikelihood Function", 
                                     style="simple",
                                     color="primary",
                                     size = "sm"),      
                          fluidRow(
                            column(12,align='center',
                                   mainPanel(
                                     plotOutput('plot',width = "100%")
                                   )
                            )
                          ),
                          
                          fluidRow(
                            
                            textOutput("maxlogL"),
                            
                            br(),
                            
                            column(3,
                                   
                                   fluidRow(
                                     conditionalPanel(
                                       
                                       condition = "input.start==1",
                                       
                                       numericInput("theta.est", 
                                                    label = "", 
                                                    value = 0,
                                                    width='50%')
                                     )
                                   ),
                                   
                                   fluidRow(
                                     
                                     conditionalPanel(
                                       
                                       condition = "input.start==1",
                                       
                                       actionBttn("guess", 
                                                  label = "Start Guessing", 
                                                  style="simple",
                                                  color="primary",
                                                  size = "sm"),
                                       
                                       br(),
                                       br(),
                                       
                                       textOutput("nstep")
                                     )
                                   )
                            ),
                            
                            column(3,
                                   tableOutput("guess")
                            ),
                            
                            column(3,
                                   " "
                            ),
                            
                            column(3,
                                   
                                   textOutput("nr"),
                                   
                                   conditionalPanel( 
                                     
                                     condition = "output.nr > 5",
                                     
                                     numericInput("start1", 
                                                  label = "Start Value", 
                                                  value = NULL,
                                                  width='50%'),
                                     
                                     actionBttn("runnr", 
                                                label = "Run Newton-Raphson", 
                                                style="simple",
                                                color="primary",
                                                size = "sm"),
                                     
                                     tableOutput("guess2")
                                   ),
                                   
                                   conditionalPanel(
                                     condition = "input.runnr == 1",
                                     textOutput("theta")
                                   )
                            )
                          ),
                          
                          br(),
                          br(),
                          br()
                          
                          
                 )
               ),
               
               tabPanel("Summary"), 
               tabPanel("Table")
             )
      )
    )
  )
  
  
)

