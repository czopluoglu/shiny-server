
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
      "Step 1. Enter the number of items and number of group factors to create the input matrix"
    })
    
    output$step2 <- renderText({
      "Step 2. Select the number of group factors"
    })
    
    
    output$step3 <- renderText({
      "Step 3. Create the input matrix"
    })
    
    output$step4 <- renderText({
      "Step 4. Enter the standardized factor loadings from your analysis in the input matrix"
    })
    
    output$step5 <- renderText({
      "Step 5. Hit Calculate to get a report of the utility indices"
    })
    
    
    

    
})
