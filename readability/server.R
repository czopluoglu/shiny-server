require(shiny)

require(quanteda)
require(quanteda.textstats)
require(udpipe)
require(reticulate)
require(text)

reticulate::import('torch')
reticulate::import('numpy')
reticulate::import('transformers')
reticulate::import('nltk')
reticulate::import('tokenizers')

require(caret)
require(glmnet)

################################################################################

setwd("B:/shinyserver/shiny-server/readability")
load("B:/shinyserver/models.RData")

################################################################################

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  #############################################################################
  values <- reactiveValues(
    df_data = data.frame(matrix(NA,nrow=1,ncol=1))
  )
  
  guessm <- eventReactive(input$predict, {
    colnames(values$df_data) <- c("Score")

    ud_eng <- udpipe_load_model('english-ewt-ud-2.5-191206.udpipe')
    
    newinput1 <- generate_feats(my.model = ridge,
                                new.text = input$mytext,
                                lang     = ud_eng)
    
    
    newinput2 <- generate_feats(my.model = lasso,
                                new.text = input$mytext,
                                lang     = ud_eng)
    
    if(input$model=='Ridge'){
      values$df_data[1,1] = predict(ridge,newinput1$input)
    }
    
    if(input$model=='Lasso'){
      values$df_data[1,1] = predict(lasso,newinput1$input)
    }
    
    values$df_data
    
  })
  
  output$guess <- renderTable(digits=3,bordered=FALSE,striped=FALSE,{
    guessm()
  })
  
  
})
