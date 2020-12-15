library(shiny)
library(plotly)
library(mvQuad)
library(lubridate)

#########################################################################
p <- function(t,a,b,g,d){
  g + (d-g)*(1/(1+exp(-a*(t-b))))
}

##########################################################################

shinyServer(function(input, output,session) {
  
  
  gen.th <- eventReactive(input$draw,{
  
    th <- rnorm(10000,0,1)
    
    th  
  })
  
  
  gen.y <- eventReactive(input$draw,{
    
    th <- gen.th()
    
    y  <- (runif(10000,0,1) < 
             p(t=th,
               a=input$a,
               b=input$b,
               g=input$g,
               d=input$d))*1
    
    y
  })

  
  
  #########################################################
  
  myplot <- eventReactive(input$draw,{
    
    th <- gen.th()
    y  <- gen.y()
    
    if((input$xaxis=='a' & input$yaxis=='b') | 
       (input$xaxis=='b' & input$yaxis=='a')){
      
        grid    <- expand.grid(a=seq(0,4,.1),b=seq(-3,3,.1))
        grid$LL <- NA
        th.m    <- matrix(th,nrow=nrow(grid),ncol=length(th),byrow=T)
        b.m     <- matrix(grid[,2],nrow=nrow(grid),ncol=length(th),byrow=F)
        a.m     <- matrix(grid[,1],nrow=nrow(grid),ncol=length(th),byrow=F)
        P.m     <- input$g + (input$d-input$g)*(1/(1+exp(-a.m*(th.m-b.m))))
        Y.m     <- matrix(y,nrow=nrow(grid),ncol=length(th),byrow=T)
        grid[,3] <- rowSums(log(P.m)*Y.m + log(1-P.m)*(1-Y.m))
        
        if(input$xaxis=='a' & input$yaxis=='b'){
          pl <-   plot_ly(grid, 
                    x = ~a, 
                    y = ~b, 
                    z = ~LL, 
                    marker = list(color = ~LL, 
                                  showscale = TRUE,
                                  cmin=-10000,
                                  cmax=0,cauto=F),
                    width=800,height=600) %>% 
              add_markers()
          return(pl)
        }
        
        if(input$xaxis=='b' & input$yaxis=='a'){
          pl <- plot_ly(grid, 
                  x = ~b, 
                  y = ~a, 
                  z = ~LL, 
                  marker = list(color = ~LL, 
                                showscale = TRUE,
                                cmin=-10000,
                                cmax=0,cauto=F),
                  width=800,height=600) %>% 
            add_markers()
          return(pl)
        }
    }
    
    
    if((input$xaxis=='a' & input$yaxis=='g') | 
       (input$xaxis=='g' & input$yaxis=='a')){
      
      grid    <- expand.grid(a=seq(0,4,.1),g=seq(0,.3,.005))
      grid$LL <- NA
      th.m    <- matrix(th,nrow=nrow(grid),ncol=length(th),byrow=T)
      g.m     <- matrix(grid[,2],nrow=nrow(grid),ncol=length(th),byrow=F)
      a.m     <- matrix(grid[,1],nrow=nrow(grid),ncol=length(th),byrow=F)
      P.m     <- g.m + (input$d-g.m)*(1/(1+exp(-a.m*(th.m-input$b))))
      Y.m     <- matrix(y,nrow=nrow(grid),ncol=length(th),byrow=T)
      grid[,3] <- rowSums(log(P.m)*Y.m + log(1-P.m)*(1-Y.m))
      
      if(input$xaxis=='a' & input$yaxis=='g'){
          pl <- plot_ly(grid, 
                  x = ~a, 
                  y = ~g, 
                  z = ~LL, 
                  marker = list(color = ~LL, 
                                showscale = TRUE,
                                cmin=-10000,
                                cmax=0,cauto=F),
                  width=800,height=600) %>% 
            add_markers()
          return(pl)
      }
      
      if(input$xaxis=='g' & input$yaxis=='a'){
        pl <- plot_ly(grid, 
                x = ~g, 
                y = ~a, 
                z = ~LL, 
                marker = list(color = ~LL, 
                              showscale = TRUE,
                              cmin=-10000,
                              cmax=0,cauto=F),
                width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
      
      
    }
    
    if((input$xaxis=='a' & input$yaxis=='d') | 
       (input$xaxis=='d' & input$yaxis=='a')){
      
      grid    <- expand.grid(a=seq(0,4,.1),d=seq(0.7,1,.005))
      grid$LL <- NA
      th.m    <- matrix(th,nrow=nrow(grid),ncol=length(th),byrow=T)
      d.m     <- matrix(grid[,2],nrow=nrow(grid),ncol=length(th),byrow=F)
      a.m     <- matrix(grid[,1],nrow=nrow(grid),ncol=length(th),byrow=F)
      P.m     <- input$g + (d.m-input$g)*(1/(1+exp(-a.m*(th.m-input$b))))
      Y.m     <- matrix(y,nrow=nrow(grid),ncol=length(th),byrow=T)
      grid[,3] <- rowSums(log(P.m)*Y.m + log(1-P.m)*(1-Y.m))
      
      if(input$xaxis=='a' & input$yaxis=='d'){
          pl <- plot_ly(grid, 
                  x = ~a, 
                  y = ~d, 
                  z = ~LL, 
                  marker = list(color = ~LL, 
                                showscale = TRUE,
                                cmin=-10000,
                                cmax=0,cauto=F),
                  width=800,height=600) %>% 
            add_markers()
          return(pl)
      }
      
      if(input$xaxis=='d' & input$yaxis=='a'){
        pl <- plot_ly(grid, 
                x = ~d, 
                y = ~a, 
                z = ~LL, 
                marker = list(color = ~LL, 
                              showscale = TRUE,
                              cmin=-10000,
                              cmax=0,cauto=F),
                width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
      
    }
    
    if((input$xaxis=='b' & input$yaxis=='g') | 
       (input$xaxis=='g' & input$yaxis=='b')){
      
      grid    <- expand.grid(b=seq(-3,3,.1),g=seq(0,.3,.005))
      grid$LL <- NA
      th.m    <- matrix(th,nrow=nrow(grid),ncol=length(th),byrow=T)
      g.m     <- matrix(grid[,2],nrow=nrow(grid),ncol=length(th),byrow=F)
      b.m     <- matrix(grid[,1],nrow=nrow(grid),ncol=length(th),byrow=F)
      P.m     <- g.m + (input$d-g.m)*(1/(1+exp(-input$a*(th.m-b.m))))
      Y.m     <- matrix(y,nrow=nrow(grid),ncol=length(th),byrow=T)
      grid[,3] <- rowSums(log(P.m)*Y.m + log(1-P.m)*(1-Y.m))
      
      if(input$xaxis=='b' & input$yaxis=='g'){
          pl <- plot_ly(grid, 
                  x = ~b, 
                  y = ~g, 
                  z = ~LL, 
                  marker = list(color = ~LL, 
                                showscale = TRUE,
                                cmin=-10000,
                                cmax=0,cauto=F),
                  width=800,height=600) %>% 
            add_markers()
          return(pl)
      }
      
      if(input$xaxis=='g' & input$yaxis=='b'){
        pl <- plot_ly(grid, 
                x = ~b, 
                y = ~g, 
                z = ~LL, 
                marker = list(color = ~LL, 
                              showscale = TRUE,
                              cmin=-10000,
                              cmax=0,cauto=F),
                width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
      
      
    }
    
    
    if((input$xaxis=='b' & input$yaxis=='d') | 
       (input$xaxis=='d' & input$yaxis=='b')){
      
      grid    <- expand.grid(b=seq(-3,3,.1),d=seq(0.7,1,.005))
      grid$LL <- NA
      th.m    <- matrix(th,nrow=nrow(grid),ncol=length(th),byrow=T)
      d.m     <- matrix(grid[,2],nrow=nrow(grid),ncol=length(th),byrow=F)
      b.m     <- matrix(grid[,1],nrow=nrow(grid),ncol=length(th),byrow=F)
      P.m     <- input$g + (d.m-input$g)*(1/(1+exp(-input$a*(th.m-b.m))))
      Y.m     <- matrix(y,nrow=nrow(grid),ncol=length(th),byrow=T)
      grid[,3] <- rowSums(log(P.m)*Y.m + log(1-P.m)*(1-Y.m))
      
      if(input$xaxis=='b' & input$yaxis=='d'){
        pl <- plot_ly(grid, 
                x = ~b, 
                y = ~d, 
                z = ~LL, 
                marker = list(color = ~LL, 
                              showscale = TRUE,
                              cmin=-10000,
                              cmax=0,cauto=F),
                width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
      
      if(input$xaxis=='d' & input$yaxis=='b'){
        pl <- plot_ly(grid, 
                x = ~d, 
                y = ~b, 
                z = ~LL, 
                marker = list(color = ~LL, 
                              showscale = TRUE,
                              cmin=-10000,
                              cmax=0,cauto=F),
                width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
      
    }
    
    
    if((input$xaxis=='g' & input$yaxis=='d') | 
       (input$xaxis=='d' & input$yaxis=='g')){
      
      grid    <- expand.grid(g=seq(0,.3,.005),d=seq(0.7,1,.005))
      grid$LL <- NA
      th.m    <- matrix(th,nrow=nrow(grid),ncol=length(th),byrow=T)
      d.m     <- matrix(grid[,2],nrow=nrow(grid),ncol=length(th),byrow=F)
      g.m     <- matrix(grid[,1],nrow=nrow(grid),ncol=length(th),byrow=F)
      P.m     <- g.m + (d.m-g.m)*(1/(1+exp(-input$a*(th.m-input$b))))
      Y.m     <- matrix(y,nrow=nrow(grid),ncol=length(th),byrow=T)
      grid[,3] <- rowSums(log(P.m)*Y.m + log(1-P.m)*(1-Y.m))
      
      if(input$xaxis=='g' & input$yaxis=='d'){
        pl <- plot_ly(grid, 
                x = ~g, 
                y = ~d, 
                z = ~LL, 
                marker = list(color = ~LL, 
                              showscale = TRUE,
                              cmin=-10000,
                              cmax=0,cauto=F),
                width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
      
      
      if(input$xaxis=='d' & input$yaxis=='g'){
        pl <- plot_ly(grid, 
                x = ~d, 
                y = ~g, 
                z = ~LL, 
                marker = list(color = ~LL, 
                              showscale = TRUE,
                              cmin=-10000,
                              cmax=0,cauto=F),
                width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
    }
    
  })
  
  myplot2 <- eventReactive(input$draw,{
    
    th <- gen.th()
    y  <- gen.y()
    
    if((input$xaxis=='a' & input$yaxis=='b') | 
       (input$xaxis=='b' & input$yaxis=='a')){

      grid    <- expand.grid(a=seq(0,4,.1),b=seq(-3,3,.1))
      grid$LL <- NA
      th.m    <- matrix(th,nrow=nrow(grid),ncol=length(th),byrow=T)
      b.m     <- matrix(grid[,2],nrow=nrow(grid),ncol=length(th),byrow=F)
      a.m     <- matrix(grid[,1],nrow=nrow(grid),ncol=length(th),byrow=F)
      P.m     <- input$g + (input$d-input$g)*(1/(1+exp(-a.m*(th.m-b.m))))
      Y.m     <- matrix(y,nrow=nrow(grid),ncol=length(th),byrow=T)
      grid[,3] <- rowSums(log(P.m)*Y.m + log(1-P.m)*(1-Y.m))

      if(input$xaxis=='a' & input$yaxis=='b'){
        
        pl <- plot_ly(grid, 
                x = ~a, 
                y = ~b, 
                z = ~LL, 
                type='contour', 
                showscale=TRUE,
                contours = list(
                  coloring = 'heatmap'
                ),
                zmin = -10000,
                zmax = 0,
                zauto=F,
                colors = 'Blues',
                width=800,height=600)
        return(pl)
      }
      
      if(input$xaxis=='b' & input$yaxis=='a'){
        
        pl <- plot_ly(grid, 
                x = ~b, 
                y = ~a, 
                z = ~LL, 
                type='contour', 
                showscale=TRUE,
                contours = list(
                  coloring = 'heatmap'
                ),
                zmin = -10000,
                zmax = 0,
                zauto=F,
                colors = 'Blues',
                width=800,height=600)
        return(pl)
      }
      
      
    }
    
    if((input$xaxis=='a' & input$yaxis=='g') | 
       (input$xaxis=='g' & input$yaxis=='a')){
      
      grid    <- expand.grid(a=seq(0,4,.1),g=seq(0,.3,.005))
      grid$LL <- NA
      th.m    <- matrix(th,nrow=nrow(grid),ncol=length(th),byrow=T)
      g.m     <- matrix(grid[,2],nrow=nrow(grid),ncol=length(th),byrow=F)
      a.m     <- matrix(grid[,1],nrow=nrow(grid),ncol=length(th),byrow=F)
      P.m     <- g.m + (input$d-g.m)*(1/(1+exp(-a.m*(th.m-input$b))))
      Y.m     <- matrix(y,nrow=nrow(grid),ncol=length(th),byrow=T)
      grid[,3] <- rowSums(log(P.m)*Y.m + log(1-P.m)*(1-Y.m))
      
      if(input$xaxis=='a' & input$yaxis=='g'){
        pl <- plot_ly(grid, 
                x = ~a, 
                y = ~g, 
                z = ~LL, 
                type='contour', 
                showscale=TRUE,
                contours = list(
                  coloring = 'heatmap'
                ),
                zmin = -10000,
                zmax = 0,
                zauto=F,
                colors = 'Blues',
                width=800,height=600)
        return(pl)
      }
      
      if(input$xaxis=='g' & input$yaxis=='a'){
        pl <- plot_ly(grid, 
                x = ~g, 
                y = ~a, 
                z = ~LL, 
                type='contour', 
                showscale=TRUE,
                contours = list(
                  coloring = 'heatmap'
                ),
                zmin = -10000,
                zmax = 0,
                zauto=F,
                colors = 'Blues',
                width=800,height=600)
        return(pl)
      }
      
      
    }
    
    if((input$xaxis=='a' & input$yaxis=='d') | 
       (input$xaxis=='d' & input$yaxis=='a')){
      
      grid    <- expand.grid(a=seq(0,4,.1),d=seq(0.7,1,.005))
      grid$LL <- NA
      th.m    <- matrix(th,nrow=nrow(grid),ncol=length(th),byrow=T)
      d.m     <- matrix(grid[,2],nrow=nrow(grid),ncol=length(th),byrow=F)
      a.m     <- matrix(grid[,1],nrow=nrow(grid),ncol=length(th),byrow=F)
      P.m     <- input$g + (d.m-input$g)*(1/(1+exp(-a.m*(th.m-input$b))))
      Y.m     <- matrix(y,nrow=nrow(grid),ncol=length(th),byrow=T)
      grid[,3] <- rowSums(log(P.m)*Y.m + log(1-P.m)*(1-Y.m))
      
      if(input$xaxis=='a' & input$yaxis=='d'){
        pl <- plot_ly(grid, 
                x = ~a, 
                y = ~d, 
                z = ~LL, 
                type='contour', 
                showscale=TRUE,
                contours = list(
                  coloring = 'heatmap'
                ),
                zmin = -10000,
                zmax = 0,
                zauto=F,
                colors = 'Blues',
                width=800,height=600)
        return(pl)
      }
      
      if(input$xaxis=='d' & input$yaxis=='a'){
        pl <- plot_ly(grid, 
                x = ~d, 
                y = ~a, 
                z = ~LL, 
                type='contour', 
                showscale=TRUE,
                contours = list(
                  coloring = 'heatmap'
                ),
                zmin = -10000,
                zmax = 0,
                zauto=F,
                colors = 'Blues',
                width=800,height=600)
        return(pl)
      }
      
      
    }
    
    if((input$xaxis=='b' & input$yaxis=='g') | 
       (input$xaxis=='g' & input$yaxis=='b')){
      
      grid    <- expand.grid(b=seq(-3,3,.1),g=seq(0,.3,.005))
      grid$LL <- NA
      th.m    <- matrix(th,nrow=nrow(grid),ncol=length(th),byrow=T)
      g.m     <- matrix(grid[,2],nrow=nrow(grid),ncol=length(th),byrow=F)
      b.m     <- matrix(grid[,1],nrow=nrow(grid),ncol=length(th),byrow=F)
      P.m     <- g.m + (input$d-g.m)*(1/(1+exp(-input$a*(th.m-b.m))))
      Y.m     <- matrix(y,nrow=nrow(grid),ncol=length(th),byrow=T)
      grid[,3] <- rowSums(log(P.m)*Y.m + log(1-P.m)*(1-Y.m))
      
      if(input$xaxis=='b' & input$yaxis=='g'){
        pl <- plot_ly(grid, 
                x = ~b, 
                y = ~g, 
                z = ~LL, 
                type='contour', 
                showscale=TRUE,
                contours = list(
                  coloring = 'heatmap'
                ),
                zmin = -10000,
                zmax = 0,
                zauto=F,
                colors = 'Blues',
                width=800,height=600)
        return(pl)
      }
      
      if(input$xaxis=='g' & input$yaxis=='b'){
        pl <- plot_ly(grid, 
                x = ~g, 
                y = ~b, 
                z = ~LL, 
                type='contour', 
                showscale=TRUE,
                contours = list(
                  coloring = 'heatmap'
                ),
                zmin = -10000,
                zmax = 0,
                zauto=F,
                colors = 'Blues',
                width=800,height=600)
        return(pl)
      }
      
      
    }
    
    
    if((input$xaxis=='b' & input$yaxis=='d') | 
       (input$xaxis=='d' & input$yaxis=='b')){
      
      grid    <- expand.grid(b=seq(-3,3,.1),d=seq(0.7,1,.005))
      grid$LL <- NA
      th.m    <- matrix(th,nrow=nrow(grid),ncol=length(th),byrow=T)
      d.m     <- matrix(grid[,2],nrow=nrow(grid),ncol=length(th),byrow=F)
      b.m     <- matrix(grid[,1],nrow=nrow(grid),ncol=length(th),byrow=F)
      P.m     <- input$g + (d.m-input$g)*(1/(1+exp(-input$a*(th.m-b.m))))
      Y.m     <- matrix(y,nrow=nrow(grid),ncol=length(th),byrow=T)
      grid[,3] <- rowSums(log(P.m)*Y.m + log(1-P.m)*(1-Y.m))
      
      if(input$xaxis=='b' & input$yaxis=='d'){
        pl <- plot_ly(grid, 
                x = ~b, 
                y = ~d, 
                z = ~LL, 
                type='contour', 
                showscale=TRUE,
                contours = list(
                  coloring = 'heatmap'
                ),
                zmin = -10000,
                zmax = 0,
                zauto=F,
                colors = 'Blues',
                width=800,height=600)
        return(pl)
      }
      
      if(input$xaxis=='d' & input$yaxis=='b'){
        pl <- plot_ly(grid, 
                x = ~d, 
                y = ~b, 
                z = ~LL, 
                type='contour', 
                showscale=TRUE,
                contours = list(
                  coloring = 'heatmap'
                ),
                zmin = -10000,
                zmax = 0,
                zauto=F,
                colors = 'Blues',
                width=800,height=600)
        return(pl)
      }
  
    }
    
    
    if((input$xaxis=='g' & input$yaxis=='d') | 
       (input$xaxis=='d' & input$yaxis=='g')){
      
      grid    <- expand.grid(g=seq(0,.3,.005),d=seq(0.7,1,.005))
      grid$LL <- NA
      th.m    <- matrix(th,nrow=nrow(grid),ncol=length(th),byrow=T)
      d.m     <- matrix(grid[,2],nrow=nrow(grid),ncol=length(th),byrow=F)
      g.m     <- matrix(grid[,1],nrow=nrow(grid),ncol=length(th),byrow=F)
      P.m     <- g.m + (d.m-g.m)*(1/(1+exp(-input$a*(th.m-input$b))))
      Y.m     <- matrix(y,nrow=nrow(grid),ncol=length(th),byrow=T)
      grid[,3] <- rowSums(log(P.m)*Y.m + log(1-P.m)*(1-Y.m))
      
      if(input$xaxis=='g' & input$yaxis=='d'){
        pl <- plot_ly(grid, 
                x = ~g, 
                y = ~d, 
                z = ~LL, 
                type='contour', 
                showscale=TRUE,
                contours = list(
                  coloring = 'heatmap'
                ),
                zmin = -10000,
                zmax = 0,
                zauto=F,
                colors = 'Blues',
                width=800,height=600)
        return(pl)
      }
      
      if(input$xaxis=='d' & input$yaxis=='g'){
        pl <- plot_ly(grid, 
                x = ~d, 
                y = ~g, 
                z = ~LL, 
                type='contour', 
                showscale=TRUE,
                contours = list(
                  coloring = 'heatmap'
                ),
                zmin = -10000,
                zmax = 0,
                zauto=F,
                colors = 'Blues',
                width=800,height=600)
        return(pl)
      }
      
      
    }
    
    
  })
  
  output$plot <- renderPlotly({
    myplot()
  })
  
  output$plot2 <- renderPlotly({
    myplot2()
  })
  
  text1 <- eventReactive(input$draw,{
    paste("For every draw, 10,000 responses are generated for a given set of 
item parameters and assuming that theta follow a standard normal distribution. 
Loglikelihood is computed for possible combinations of parameters on the X-axis 
and Y-axis using the known theta values. "," ",
"Grid points used for parameters: "," ",
"  - a --> from 0 to 4 by 0.1",
"  - b --> from -3 to 3 by 0.1",
"  - g --> from 0 to 0.3 by 0.005",
"  - d --> from 0.7 to 1.0 by .005", sep="\n")
    
  })
  
  output$info <- renderText({
     text1()
  })
  
})
















