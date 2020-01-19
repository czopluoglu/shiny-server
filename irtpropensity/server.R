library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$plot <- renderPlot({

        lambda    = input$lambda
        threshold = input$threshold
        
        theta = c(-3,-2,-1,0,1,2,3)
        mean = theta*lambda
    
        err.sd = sqrt(1-lambda^2)
        
        plot(theta,mean,type="p",ylim=c(-4,4),xlim=c(-3.8,3),xaxt="n",
             cex=2,pch=19,
             xlab="Latent Trait Score",
             ylab="Latent Propensity Score")
        
        
        axis(side=1,at=c(-3,-2,-1,0,1,2,3))
        abline(h=threshold,col="blue",lwd=2)
        
        dens = dnorm(seq(-3,3,.01),0,err.sd)
        cols = c("blue4","darkgoldenrod1","firebrick3","violetred3",
                 "hotpink1","navajowhite","springgreen4")
        
        for(i in 1:7){
            
            x = theta[i] - dens
            y = mean[i]+seq(-3,3,.01)
            
            points(x,y,type="l",lty=2,col=cols[i])
            abline(v=theta[i],lty=2,col="gray")
            
            poly.range = (y>=threshold)
            
            polygon(c(x[poly.range],theta[i]),
                    c(y[poly.range],threshold),
                    density=35,angle=0,lty=1,col=cols[i])
           
        }
        
        abline(a=0,b=lambda,lwd=2)
        
    })
    
    output$plot2 <- renderPlot({
        
        
        lambda    = input$lambda
        threshold = input$threshold
        
        theta = c(-3,-2,-1,0,1,2,3)
        
        err.sd = sqrt(1-lambda^2)
        
        probs = 1-pnorm(-(lambda*theta-threshold)/err.sd,0,err.sd)
        
        plot(theta,probs,type="b",
             xlab="Latent Trait Score",
             ylab="Probability of Correct Response",
             ylim=c(0,1))
        
    })
    
    output$restext <- renderUI({
        
        res = sqrt(1-input$lambda^2)
        
        withMathJax(
            helpText(
                sprintf("Residual Standard Deviation: \\(\\psi=\\sqrt{1-\\lambda^2}\\) = %.02f",res)
            )
        )
    })
    
    output$a <- renderUI({
        
        res = sqrt(1-input$lambda^2)
        a = input$lambda/res
        
        withMathJax(
            helpText(
                sprintf("Item Discrimination: \\(a=\\frac{\\lambda}{\\psi}\\) = %.02f",a)
            )
        )
    })
    
    output$b <- renderUI({
        
        b = input$threshold/input$lambda
        
        withMathJax(
            helpText(
                sprintf("Item Difficulty: \\(b=\\frac{\\tau}{\\lambda}\\) = %.02f",b)
            )
        )
    })
    
    output$table <- renderTable({
        
        lambda    = input$lambda
        threshold = input$threshold
        
        theta = c(-3,-2,-1,0,1,2,3)
        
        err.sd = sqrt(1-lambda^2)
        
        probs = 1-pnorm(-(lambda*theta-threshold)/err.sd,0,err.sd)
        
        tab = data.frame(cbind(theta,probs))
        
        colnames(tab) <- c("F","Probability")
        
        tab
    })
    
    
    
    

})
