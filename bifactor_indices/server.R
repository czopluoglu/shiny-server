
#Debugging

#options(shiny.error = browser) 
#options(shiny.error = recover)


##############################################################

shinyServer(function(input, output) {

    url <- a("@cengizzopluoglu", href="https://twitter.com/cengizzopluoglu")
    output$info <- renderUI({
        tagList("Twitter:", url)
    })
    
    url2 <- a("cengiz.me", href="https://cengiz.me/")
    output$info2 <- renderUI({
      tagList("Website:", url2)
    })
    
    url3 <- a("Rodriguez, A., Reise, S. P., & Haviland, M. G. (2016). Applying bifactor statistical 
              indices in the evaluation of psychological measures. Journal of personality assessment, 
              98(3), 223-237.", 
              href="https://www.tandfonline.com/doi/10.1080/00223891.2015.1089249")
    
    output$info3 <- renderUI({
      tagList("Reference:", url3)
    })
    
    output$step1 <- renderText({
      "Step 1. Upload the input factor loading matrix by using the Input tab in the right panel."
    })
    
    output$step2 <- renderText({
      "Step 2. Check the accuracy of imported matrix and hit Calculate Indices Button"
    })
    
    
    output$step3 <- renderText({
      "Step 3. Go to the different tabs to see the results."
    })
    
    output$step <- renderText({
      "Input matrix must be a comma separated text file including the standardized factor loadings.
      The first column must include the factor loadings for the general factor, and the remaining
      columns must include the factor loadings for the specific factors."
    })
    
    
    output$table1 <- renderTable(digits=3,bordered=TRUE,striped=TRUE,{
      req(input$file1)
      df <- read.csv(input$file1$datapath,header = FALSE)
      
      cnames = "G"
      for(i in 1:(ncol(df)-1)){
        cnames=c(cnames,paste0("G",i))
      }
      
      colnames(df) = cnames
      
      rnames = "Item 1"
      for(i in 2:nrow(df)){
        rnames=c(rnames,paste0("Item ",i))
      }
      
      rownames(df) = rnames
      
      
      df
    },rownames = TRUE)
    

    
})
