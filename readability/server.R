library(shiny)
################################################################################

setwd("B:/shinyserver/shiny-server/readability")

load("B:/shinyserver/models.RData")

require(quanteda)
require(quanteda.textstats)
require(udpipe)
require(reticulate)
require(text)

ud_eng <- udpipe_load_model('english-ewt-ud-2.5-191206.udpipe')

reticulate::import('torch')
reticulate::import('numpy')
reticulate::import('transformers')
reticulate::import('nltk')
reticulate::import('tokenizers')

require(caret)
require(glmnet)


################################################################################


################################################################################

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  values <- reactiveValues(
    df_data = data.frame(matrix(NA,nrow=1,ncol=1))
  )
  
  guessm <- eventReactive(input$predict, {
    colnames(values$df_data) <- c("Score")

    newinput <- generate_feats(my.model = ridge,
                               new.text = input$mytext)
    
    values$df_data[1,1] = predict(ridge,newinput$input)
    values$df_data
  })
  
  output$guess <- renderTable(digits=3,bordered=FALSE,striped=FALSE,{
    guessm()
  })

})
