
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
    
    
    url4 <- a("You can download a sample input file here.", 
              href="https://raw.githubusercontent.com/czopluoglu/shiny-server/master/bifactor_indices/sample.csv")
    output$info4 <- renderUI({
      tagList(url4)
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
      For instance, '1,13,22,31' indicates that there are four specific factors and 
      Item 1 - Item 12 loads on the first specific factor, 
      Item 13 - Item 21 loads on the second specific factor,
      Item 22 - Item 30 loads on the third specific factor,
      and Item 31 to the last item loads on the fourth specific factor."
    })
    
    output$text3 <- renderText({
      "Omega Hierarchical coefficient estimates the proportion of variance in total scores that can be attributed to a single general factor.
      Omega Hierarchical coefficient for specific factors estimates the reliability of a subscale score after controlling for the variance due to the
      general factor."
    })
    
    cite1 <- a("Reise, S. P., Bonifay, W. E., & Haviland, M. G. (2013). Scoring and modeling psychological measures in the presence of multidimensionality. 
                Journal of Personality Assessment, 95, 129–140.", 
              href="http://dx.doi.org/10.1080/00223891.2012.725437")
    
    output$cit1 <- renderUI({
      tagList(cite1)
    })
    
    
    output$text4 <- renderText({
      "Factor determinacy ranges from 0 to 1, and values closer to 1 indicates better determinacy.
      Values above .9 is typically recommended for factor scores to be useful."
    })

    output$text5 <- renderText({
      'H values provide the correlation between a factor and an optimally weighted item composite
       Hancock and Mueller (2001) have justified the need to meet a standard criterion of H=.70'
    })

    
    output$text6 <- renderText({
      'Explained Common Variance (ECV) is an index to measure the proportion of variance accounted by
      the general factor. A high ECV value may indicate a strong general factor and can be used to
      justify to fit a unidimensional model although there are specific factors and 
      data are multidimensional.'
    })
    
    
    output$text7 <- renderText({
      'Explained Common Variance at the item level (I-ECV) is reported below and can be used to
      construct a more unidimensional measure by selecting items with I-ECV greater than 0.80 or
      0.85.'
    })
        
    cite2 <- a("Grice, J. W. (2001). Computing and evaluating factor scores. Psychological Methods, 6, 430–450.", 
               href="http://dx.doi.org/10.1037/1082-989X.6.4.430")
    
    cite3 <- a("Gorsuch, R. L. (1983). Factor analysis (2nd ed.). Hillsdale, NJ: Erlbaum.", 
               href="https://books.google.com/books?id=LDecBQAAQBAJ")
    
    output$cit2 <- renderUI({
      tagList(cite2)
    })
    
    output$cit3 <- renderUI({
      tagList(cite3)
    })
    
    cite4 <- a("Hancock, G. R., & Mueller, R. O. (2001). Rethinking construct reliability
                within latent variable systems. In R. Cudeck, S. du Toit, & D. Sörbom
                (Eds.), Structural equation modeling: Present and future—A Festschrift
                in honor of Karl Jöreskog (pp. 195–216). Lincolnwood, IL: Scientific
               Software International.", 
               href="www.google.com")
    
    output$cit4 <- renderUI({
      tagList(cite4)
    })
    
    cite5 <- a("Reise, S. P., Moore, T. M., & Haviland, M. G. (2010). Bifactor models and
                rotations: Exploring the extent to which multidimensional data yield
                univocal scale scores. Journal of Personality Assessment, 92, 544–559.", 
               href="http://dx.doi.org/10.1080/00223891.2010.496477")
    
    output$cit5 <- renderUI({
      tagList(cite5)
    })
    
    cite6 <- a("Rodriguez, A., Reise, S. P., & Haviland, M. G. (2016). Applying bifactor statistical 
              indices in the evaluation of psychological measures. Journal of personality assessment, 
              98(3), 223-237.", 
              href="https://www.tandfonline.com/doi/10.1080/00223891.2015.1089249")
    
    output$cit6 <- renderUI({
      tagList(cite6)
    })
    
    ##########################################################
    
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
    
    ##########################################################
    
    tab1 <- eventReactive(req(isTruthy(input$calculate)), {
      
      df <- read.csv(input$file1$datapath,header = FALSE)
      
      p = as.numeric(strsplit(input$pos,",")[[1]])
      p = c(p,nrow(df)+1)
      
      load <- vector('list',length(p))
      s    <- c()
      
      for(i in 1:length(p)){
        load[[i]]=df[,i]
        s[i] = sum(load[[i]])^2
      }
      
      u = 1-rowSums(df^2)
      
      omega <- c()
      
      omega[1] = s[1]/(sum(s)+sum(u))
      
      for(i in 2:length(p)){
        
          a = sum(load[[i]][p[i-1]:(p[i]-1)])^2
          b = sum(load[[1]][p[i-1]:(p[i]-1)])^2
          d = sum(u[p[i-1]:(p[i]-1)])
          
          omega[i] =  a/(a+b+d)
      }
      
      om <- data.frame(matrix(NA,length(p),2))
      
      cnames = "G"
      for(i in 1:(ncol(df)-1)){
        cnames=c(cnames,paste0("G",i))
      }
      
      om[,1] <- cnames
      om[,2] <- omega
      
      colnames(om) <- c("Factor","Omega_H")
      
      om
    })
    
    output$omegaH <- renderTable(digits=3,bordered=TRUE,striped=TRUE,{
      tab1()
    })
    
    ##########################################################
    
    tab2 <- eventReactive(req(isTruthy(input$calculate)), {
      
      df <- read.csv(input$file1$datapath,header = FALSE)
      
      u = 1-rowSums(df^2)
      
      phi  = diag(5)
      L    = as.matrix(df)
      uniq = diag(u)
      
      S = L%*%phi%*%t(L) + uniq
      
      
      FD = sqrt(diag(phi%*%t(L)%*%solve(S)%*%L%*%phi))
      
      
      om <- data.frame(matrix(NA,length(p),2))
      
      cnames = "G"
      for(i in 1:(ncol(df)-1)){
        cnames=c(cnames,paste0("G",i))
      }
      
      om[,1] <- cnames
      om[,2] <- FD
      
      colnames(om) <- c("Factor","FD")
      
      om
    })
    
    output$
      FD<- renderTable(digits=3,bordered=TRUE,striped=TRUE,{
      tab2()
    })
    
    ######################################################################################
    
    tab3 <- eventReactive(req(isTruthy(input$calculate)), {
      
      df <- read.csv(input$file1$datapath,header = FALSE)
      
      p = as.numeric(strsplit(input$pos,",")[[1]])
      p = c(p,nrow(df)+1)
      
      load <- vector('list',length(p))
      
      for(i in 1:length(p)){
        load[[i]]=df[,i]
       }
      
      h <- c()
      
      h[1]= 1/(1+ 1/sum(load[[1]]^2/(1-load[[1]]^2)))
      
      for(i in 2:length(p)){
        
        h[i]=1/(1+ 1/sum((load[[i]][p[i-1]:(p[i]-1)]^2)/
                      (1-(load[[i]][p[i-1]:(p[i]-1)])^2)))
        
      }
      
      om <- data.frame(matrix(NA,length(p),2))
      
      cnames = "G"
      for(i in 1:(ncol(df)-1)){
        cnames=c(cnames,paste0("G",i))
      }
      
      om[,1] <- cnames
      om[,2] <- h
      
      colnames(om) <- c("Factor","H")
      
      om
    })
    
    output$H <- renderTable(digits=3,bordered=TRUE,striped=TRUE,{
      tab3()
    })


    ######################################################################################
    
    tab4 <- eventReactive(req(isTruthy(input$calculate)), {
      
      df <- read.csv(input$file1$datapath,header = FALSE)
      
      p = as.numeric(strsplit(input$pos,",")[[1]])
      p = c(p,nrow(df)+1)
      
      load <- vector('list',length(p))
      
      for(i in 1:length(p)){
        load[[i]]=df[,i]
      }
      
      h <- c()
      
      h[1]= sum(load[[1]]^2)
                
      for(i in 2:length(p)){
        
        h[i]= sum(load[[i]][p[i-1]:(p[i]-1)]^2)
      }
      
      ECV = h[1]/(sum(h))
      
      o = data.frame(matrix(ECV,1,1))
      colnames(o) = 'ECV'
      o
    })
    
    output$ECV <- renderTable(digits=3,bordered=TRUE,striped=TRUE,{
      tab4()
    })
    
    ######################################################################################
    
    tab5 <- eventReactive(req(isTruthy(input$calculate)), {
      
      df <- read.csv(input$file1$datapath,header = FALSE)
      
      o2 = data.frame(matrix(df[,1]^2/(df[,1]^2+df[,2]^2+df[,3]^2+df[,4]^2+df[,5]^2),
                 nrow=nrow(df),ncol=1))
      
      colnames(o2) = "I-ECV"
      o2
      
      rnames = "Item 1"
      for(i in 2:nrow(df)){
        rnames=c(rnames,paste0("Item ",i))
      }
      
      rownames(o2) <- rnames
      o2
    })
    
    output$IECV <- renderTable(digits=3,bordered=TRUE,striped=TRUE,rownames=TRUE,{
      tab5()
    })
    
    ##########################################################
    

    
})
