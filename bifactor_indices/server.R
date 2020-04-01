
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
    
    
    url4 <- a("You can download a sample input file here.", href="https://cengiz.me/")
    output$info2 <- renderUI({
      tagList("Website:", url2)
    })
    
    
    output$step1 <- renderText({
      "Step 1. Upload the input factor loading matrix by using the Input tab in the right panel."
    })
    
    output$step2 <- renderText({
      "Step 2. Check the accuracy of imported matrix."
    })
    
    output$step3 <- renderText({
      "Step 3. Enter the starting item positions for each specific factor seprated by comma."
    })
    
    output$step4 <- renderText({
      "Step 4. Hit the Calculate Indices Button"
    })
    
    output$step5 <- renderText({
      "Step 5. Go to the different tabs to see the results."
    })
    
    output$text1 <- renderText({
      "Input matrix must be a comma separated text file containing the standardized factor loadings.
      The first column must contain the factor loadings for the general factor, and the remaining
      columns must contain the factor loadings for the specific factors."
    })
    
    
    output$text2 <- renderText({
      "Starting item positions must be a comma separated vector of numeric values.
      For instance, '1,8,14' indicates that there are three specific factors and Item 1 - Item 7
      loads on the first specific factor, Item 8 - Item 13 loads on the second specific factor,
      and Item 14 to the last item loads on the third specific factor."
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
