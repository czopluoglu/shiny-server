library(shiny)
library(plotly)

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
        
        for(i in 1:nrow(grid)){
          P = input$g + (input$d-input$g)*(1/(1+exp(-grid[i,1]*(th-grid[i,2]))))
          grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
        }

        if(input$xaxis=='a' & input$yaxis=='b'){
          pl <-   plot_ly(grid, 
                    x = ~a, 
                    y = ~b, 
                    z = ~LL, 
                    marker = list(color = ~LL, 
                                  showscale = TRUE,
                                  cmin=max(grid[,3])*1.2,
                                  cmax=max(grid[,3])*0.9,cauto=F),
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
                                cmin=max(grid[,3])*1.2,
                                cmax=max(grid[,3])*0.9,cauto=F),
                  width=800,height=600) %>% 
            add_markers()
          return(pl)
        }
    }
    
    
    if((input$xaxis=='a' & input$yaxis=='g') | 
       (input$xaxis=='g' & input$yaxis=='a')){
      
      grid    <- expand.grid(a=seq(0,4,.1),g=seq(0,.3,.005))
      grid$LL <- NA

      for(i in 1:nrow(grid)){
        P = grid[i,2] + (input$d-grid[i,2])*(1/(1+exp(-grid[i,1]*(th-input$b))))
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis=='a' & input$yaxis=='g'){
          pl <- plot_ly(grid, 
                  x = ~a, 
                  y = ~g, 
                  z = ~LL, 
                  marker = list(color = ~LL, 
                                showscale = TRUE,
                                cmin=max(grid[,3])*1.2,
                                cmax=max(grid[,3])*0.9,cauto=F),
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
                              cmin=max(grid[,3])*1.2,
                              cmax=max(grid[,3])*0.9,cauto=F),
                width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
      
      
    }
    
    if((input$xaxis=='a' & input$yaxis=='d') | 
       (input$xaxis=='d' & input$yaxis=='a')){
      
      grid    <- expand.grid(a=seq(0,4,.1),d=seq(0.7,1,.005))
      grid$LL <- NA

      for(i in 1:nrow(grid)){
        P = input$g + (grid[i,2]-input$g)*(1/(1+exp(-grid[i,1]*(th-input$b))))
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis=='a' & input$yaxis=='d'){
          pl <- plot_ly(grid, 
                  x = ~a, 
                  y = ~d, 
                  z = ~LL, 
                  marker = list(color = ~LL, 
                                showscale = TRUE,
                                cmin=max(grid[,3])*1.2,
                                cmax=max(grid[,3])*0.9,cauto=F),
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
                              cmin=max(grid[,3])*1.2,
                              cmax=max(grid[,3])*0.9,cauto=F),
                width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
      
    }
    
    if((input$xaxis=='b' & input$yaxis=='g') | 
       (input$xaxis=='g' & input$yaxis=='b')){
      
      grid    <- expand.grid(b=seq(-3,3,.1),g=seq(0,.3,.005))
      grid$LL <- NA

      for(i in 1:nrow(grid)){
        P = grid[i,2]+ (input$d-grid[i,2])*(1/(1+exp(-input$a*(th-grid[i,1]))))
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis=='b' & input$yaxis=='g'){
          pl <- plot_ly(grid, 
                  x = ~b, 
                  y = ~g, 
                  z = ~LL, 
                  marker = list(color = ~LL, 
                                showscale = TRUE,
                                cmin=max(grid[,3])*1.2,
                                cmax=max(grid[,3])*0.9,cauto=F),
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
                              cmin=max(grid[,3])*1.2,
                              cmax=max(grid[,3])*0.9,cauto=F),
                width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
      
      
    }
    
    
    if((input$xaxis=='b' & input$yaxis=='d') | 
       (input$xaxis=='d' & input$yaxis=='b')){
      
      grid    <- expand.grid(b=seq(-3,3,.1),d=seq(0.7,1,.005))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        P = input$g + (grid[i,2]-input$g)*(1/(1+exp(-input$a*(th-grid[i,1]))))
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      
      if(input$xaxis=='b' & input$yaxis=='d'){
        pl <- plot_ly(grid, 
                x = ~b, 
                y = ~d, 
                z = ~LL, 
                marker = list(color = ~LL, 
                              showscale = TRUE,
                              cmin=max(grid[,3])*1.2,
                              cmax=max(grid[,3])*0.9,cauto=F),
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
                              cmin=max(grid[,3])*1.2,
                              cmax=max(grid[,3])*0.9,cauto=F),
                width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
      
    }
    
    
    if((input$xaxis=='g' & input$yaxis=='d') | 
       (input$xaxis=='d' & input$yaxis=='g')){
      
      grid    <- expand.grid(g=seq(0,.3,.005),d=seq(0.7,1,.005))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        P = grid[i,1]+ (grid[i,2]-grid[i,1])*(1/(1+exp(-input$a*(th-input$b))))
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis=='g' & input$yaxis=='d'){
        pl <- plot_ly(grid, 
                x = ~g, 
                y = ~d, 
                z = ~LL, 
                marker = list(color = ~LL, 
                              showscale = TRUE,
                              cmin=max(grid[,3])*1.2,
                              cmax=max(grid[,3])*0.9,cauto=F),
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
                              cmin=max(grid[,3])*1.2,
                              cmax=max(grid[,3])*0.9,cauto=F),
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
      
      for(i in 1:nrow(grid)){
        P = input$g + (input$d-input$g)*(1/(1+exp(-grid[i,1]*(th-grid[i,2]))))
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
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
                zmin=max(grid[,3])*1.2,
                zmax=max(grid[,3])*0.9,
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
                zmin=max(grid[,3])*1.2,
                zmax=max(grid[,3])*0.9,
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

      for(i in 1:nrow(grid)){
        P = grid[i,2] + (input$d-grid[i,2])*(1/(1+exp(-grid[i,1]*(th-input$b))))
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
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
                zmin=max(grid[,3])*1.2,
                zmax=max(grid[,3])*0.9,
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
                zmin=max(grid[,3])*1.2,
                zmax=max(grid[,3])*0.9,
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
      
      for(i in 1:nrow(grid)){
        P = input$g + (grid[i,2]-input$g)*(1/(1+exp(-grid[i,1]*(th-input$b))))
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
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
                zmin=max(grid[,3])*1.2,
                zmax=max(grid[,3])*0.9,
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
                zmin=max(grid[,3])*1.2,
                zmax=max(grid[,3])*0.9,
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

      for(i in 1:nrow(grid)){
        P = grid[i,2]+ (input$d-grid[i,2])*(1/(1+exp(-input$a*(th-grid[i,1]))))
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
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
                zmin=max(grid[,3])*1.2,
                zmax=max(grid[,3])*0.9,
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
                zmin=max(grid[,3])*1.2,
                zmax=max(grid[,3])*0.9,
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

      for(i in 1:nrow(grid)){
        P = input$g + (grid[i,2]-input$g)*(1/(1+exp(-input$a*(th-grid[i,1]))))
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
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
                zmin=max(grid[,3])*1.2,
                zmax=max(grid[,3])*0.9,
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
                zmin=max(grid[,3])*1.2,
                zmax=max(grid[,3])*0.9,
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

      for(i in 1:nrow(grid)){
        P = grid[i,1]+ (grid[i,2]-grid[i,1])*(1/(1+exp(-input$a*(th-input$b))))
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
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
                zmin=max(grid[,3])*1.2,
                zmax=max(grid[,3])*0.9,
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
                zmin=max(grid[,3])*1.2,
                zmax=max(grid[,3])*0.9,
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
  
  #############################################################################
  #############################################################################
  #############################################################################
  #############################################################################
  require(MASS)
  
  p2 <- function(t1,t2,a1,a2,b,g,d){
    g + (d-g)*(1/(1+exp(-(a1*t1+a2*t2+b))))
  }
  
  gen.th2 <- eventReactive(input$draw2,{
    
    S <- diag(2)
    S[1,2]=S[2,1]=input$rho
    th <- mvrnorm(10000,mu=c(0,0),Sigma=S)
    
    th  
  })
  
  
  gen.y2 <- eventReactive(input$draw2,{
    
    th <- gen.th2()
    
    y  <- (runif(10000,0,1) < 
             p2(t1=th[,1],
                t2=th[,2],
               a1=input$a1,
               a2=input$a2,
               b=input$c,
               g=input$g_c,
               d=input$d_c))*1
    
    y
  })
  
  
  
  #########################################################
  
  myplot3 <- eventReactive(input$draw2,{
    
    th <- gen.th2()
    y  <- gen.y2()
    
    if((input$xaxis_c=='a1' & input$yaxis_c=='a2') | 
       (input$xaxis_c=='a2' & input$yaxis_c=='a1')){
      
      grid    <- expand.grid(a1=seq(0,4,.1),a2=seq(0,4,.1))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        P = input$g_c + (input$d_c-input$g_c)*(1/(1+exp(-(grid[i,1]*th[,1]+grid[i,2]*th[,2]+input$c))))
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_c=='a1' & input$yaxis_c=='a2'){
        pl <-   plot_ly(grid, 
                        x = ~a1, 
                        y = ~a2, 
                        z = ~LL, 
                        marker = list(color = ~LL, 
                                      showscale = TRUE,
                                      cmin=max(grid[,3])*1.2,
                                      cmax=max(grid[,3])*0.9,cauto=F),
                        width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
      
      if(input$xaxis_c=='a2' & input$yaxis_c=='a1'){
        pl <- plot_ly(grid, 
                      x = ~a2, 
                      y = ~a1, 
                      z = ~LL, 
                      marker = list(color = ~LL, 
                                    showscale = TRUE,
                                    cmin=max(grid[,3])*1.2,
                                    cmax=max(grid[,3])*0.9,cauto=F),
                      width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
    }
    
    
    
    
    
    if((input$xaxis_c=='a1' & input$yaxis_c=='c') | 
       (input$xaxis_c=='c' & input$yaxis_c=='a1')){
      
      grid    <- expand.grid(a1=seq(0,4,.1),c=seq(-3,3,.1))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        P = input$g_c + (input$d_c-input$g_c)*(1/(1+exp(-(grid[i,1]*th[,1]+input$a2*th[,2]+grid[,2]))))
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_c=='a1' & input$yaxis_c=='c'){
        pl <-   plot_ly(grid, 
                        x = ~a1, 
                        y = ~c, 
                        z = ~LL, 
                        marker = list(color = ~LL, 
                                      showscale = TRUE,
                                      cmin=max(grid[,3])*1.2,
                                      cmax=max(grid[,3])*0.9,cauto=F),
                        width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
      
      if(input$xaxis_c=='c' & input$yaxis_c=='a1'){
        pl <- plot_ly(grid, 
                      x = ~c, 
                      y = ~a1, 
                      z = ~LL, 
                      marker = list(color = ~LL, 
                                    showscale = TRUE,
                                    cmin=max(grid[,3])*1.2,
                                    cmax=max(grid[,3])*0.9,cauto=F),
                      width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
    }
    
    
    
    
    if((input$xaxis_c=='a1' & input$yaxis_c=='g') | 
       (input$xaxis_c=='g' & input$yaxis_c=='a1')){
      
      grid    <- expand.grid(a1=seq(0,4,.1),g=seq(0,0.3,.005))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        P = grid[i,2] + (input$d_c-grid[i,2])*(1/(1+exp(-(grid[i,1]*th[,1]+input$a2*th[,2]+input$c))))
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_c=='a1' & input$yaxis_c=='g'){
        pl <-   plot_ly(grid, 
                        x = ~a1, 
                        y = ~g, 
                        z = ~LL, 
                        marker = list(color = ~LL, 
                                      showscale = TRUE,
                                      cmin=max(grid[,3])*1.2,
                                      cmax=max(grid[,3])*0.9,cauto=F),
                        width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
      
      if(input$xaxis_c=='g' & input$yaxis_c=='a1'){
        pl <- plot_ly(grid, 
                      x = ~g, 
                      y = ~a1, 
                      z = ~LL, 
                      marker = list(color = ~LL, 
                                    showscale = TRUE,
                                    cmin=max(grid[,3])*1.2,
                                    cmax=max(grid[,3])*0.9,cauto=F),
                      width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
    }
    
    
    
    
    
    if((input$xaxis_c=='a1' & input$yaxis_c=='d') | 
       (input$xaxis_c=='d' & input$yaxis_c=='a1')){
      
      grid    <- expand.grid(a1=seq(0,4,.1),d=seq(0.7,1,.005))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        P = input$g_c + (grid[i,2]-input$g_c)*(1/(1+exp(-(grid[i,1]*th[,1]+input$a2*th[,2]+input$c))))
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_c=='a1' & input$yaxis_c=='d'){
        pl <-   plot_ly(grid, 
                        x = ~a1, 
                        y = ~d, 
                        z = ~LL, 
                        marker = list(color = ~LL, 
                                      showscale = TRUE,
                                      cmin=max(grid[,3])*1.2,
                                      cmax=max(grid[,3])*0.9,cauto=F),
                        width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
      
      if(input$xaxis_c=='d' & input$yaxis_c=='a1'){
        pl <- plot_ly(grid, 
                      x = ~d, 
                      y = ~a1, 
                      z = ~LL, 
                      marker = list(color = ~LL, 
                                    showscale = TRUE,
                                    cmin=max(grid[,3])*1.2,
                                    cmax=max(grid[,3])*0.9,cauto=F),
                      width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
    }
    
    
    
    
    
    
    if((input$xaxis_c=='a2' & input$yaxis_c=='c') | 
       (input$xaxis_c=='c' & input$yaxis_c=='a2')){
      
      grid    <- expand.grid(a2=seq(0,4,.1),c=seq(-3,3,.1))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        P = input$g_c + (input$d_c-input$g_c)*(1/(1+exp(-(input$a1*th[,1]+grid[i,1]*th[,2]+grid[i,2]))))
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_c=='a2' & input$yaxis_c=='c'){
        pl <-   plot_ly(grid, 
                        x = ~a2, 
                        y = ~c, 
                        z = ~LL, 
                        marker = list(color = ~LL, 
                                      showscale = TRUE,
                                      cmin=max(grid[,3])*1.2,
                                      cmax=max(grid[,3])*0.9,cauto=F),
                        width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
      
      if(input$xaxis_c=='c' & input$yaxis_c=='a2'){
        pl <- plot_ly(grid, 
                      x = ~c, 
                      y = ~a2, 
                      z = ~LL, 
                      marker = list(color = ~LL, 
                                    showscale = TRUE,
                                    cmin=max(grid[,3])*1.2,
                                    cmax=max(grid[,3])*0.9,cauto=F),
                      width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
    }

    
    
    
    
    if((input$xaxis_c=='a2' & input$yaxis_c=='g') | 
       (input$xaxis_c=='g' & input$yaxis_c=='a2')){
      
      grid    <- expand.grid(a2=seq(0,4,.1),g=seq(0,0.3,.005))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        P = grid[i,2] + (input$d_c-grid[i,2])*(1/(1+exp(-(input$a1*th[,1]+grid[i,1]*th[,2]+input$c))))
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_c=='a2' & input$yaxis_c=='g'){
        pl <-   plot_ly(grid, 
                        x = ~a2, 
                        y = ~g, 
                        z = ~LL, 
                        marker = list(color = ~LL, 
                                      showscale = TRUE,
                                      cmin=max(grid[,3])*1.2,
                                      cmax=max(grid[,3])*0.9,cauto=F),
                        width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
      
      if(input$xaxis_c=='g' & input$yaxis_c=='a2'){
        pl <- plot_ly(grid, 
                      x = ~g, 
                      y = ~a2, 
                      z = ~LL, 
                      marker = list(color = ~LL, 
                                    showscale = TRUE,
                                    cmin=max(grid[,3])*1.2,
                                    cmax=max(grid[,3])*0.9,cauto=F),
                      width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
    }
    
    
    
    
    
    if((input$xaxis_c=='a2' & input$yaxis_c=='d') | 
       (input$xaxis_c=='d' & input$yaxis_c=='a2')){
      
      grid    <- expand.grid(a2=seq(0,4,.1),d=seq(0.7,1,.005))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        P = input$g_c + (grid[i,2]-input$g_c)*(1/(1+exp(-(input$a1*th[,1]+grid[i,1]*th[,2]+input$c))))
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_c=='a2' & input$yaxis_c=='d'){
        pl <-   plot_ly(grid, 
                        x = ~a2, 
                        y = ~d, 
                        z = ~LL, 
                        marker = list(color = ~LL, 
                                      showscale = TRUE,
                                      cmin=max(grid[,3])*1.2,
                                      cmax=max(grid[,3])*0.9,cauto=F),
                        width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
      
      if(input$xaxis_c=='d' & input$yaxis_c=='a2'){
        pl <- plot_ly(grid, 
                      x = ~d, 
                      y = ~a2, 
                      z = ~LL, 
                      marker = list(color = ~LL, 
                                    showscale = TRUE,
                                    cmin=max(grid[,3])*1.2,
                                    cmax=max(grid[,3])*0.9,cauto=F),
                      width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
    }
    
    
    
        
    
    if((input$xaxis_c=='c' & input$yaxis_c=='g') | 
       (input$xaxis_c=='g' & input$yaxis_c=='c')){
      
      grid    <- expand.grid(c=seq(-3,3,.1),g=seq(0,0.3,.005))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        P = grid[i,2] + (input$d_c-grid[i,2])*(1/(1+exp(-(input$a1*th[,1]+input$a2*th[,2]+grid[i,1]))))
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_c=='c' & input$yaxis_c=='g'){
        pl <-   plot_ly(grid, 
                        x = ~c, 
                        y = ~g, 
                        z = ~LL, 
                        marker = list(color = ~LL, 
                                      showscale = TRUE,
                                      cmin=max(grid[,3])*1.2,
                                      cmax=max(grid[,3])*0.9,cauto=F),
                        width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
      
      if(input$xaxis_c=='g' & input$yaxis_c=='c'){
        pl <- plot_ly(grid, 
                      x = ~g, 
                      y = ~c, 
                      z = ~LL, 
                      marker = list(color = ~LL, 
                                    showscale = TRUE,
                                    cmin=max(grid[,3])*1.2,
                                    cmax=max(grid[,3])*0.9,cauto=F),
                      width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
    }
    

    
    if((input$xaxis_c=='c' & input$yaxis_c=='d') | 
       (input$xaxis_c=='d' & input$yaxis_c=='c')){
      
      grid    <- expand.grid(c=seq(-3,3,.1),d=seq(0.7,1,.005))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        P = input$g_c + (grid[i,2]-input$g_c)*(1/(1+exp(-(input$a1*th[,1]+input$a2*th[,2]+grid[i,1]))))
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_c=='c' & input$yaxis_c=='d'){
        pl <-   plot_ly(grid, 
                        x = ~c, 
                        y = ~d, 
                        z = ~LL, 
                        marker = list(color = ~LL, 
                                      showscale = TRUE,
                                      cmin=max(grid[,3])*1.2,
                                      cmax=max(grid[,3])*0.9,cauto=F),
                        width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
      
      if(input$xaxis_c=='d' & input$yaxis_c=='c'){
        pl <- plot_ly(grid, 
                      x = ~d, 
                      y = ~c, 
                      z = ~LL, 
                      marker = list(color = ~LL, 
                                    showscale = TRUE,
                                    cmin=max(grid[,3])*1.2,
                                    cmax=max(grid[,3])*0.9,cauto=F),
                      width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
    }
    
    
    
    
    
    
    
    if((input$xaxis_c=='g' & input$yaxis_c=='d') | 
       (input$xaxis_c=='d' & input$yaxis_c=='g')){
      
      grid    <- expand.grid(g=seq(0,.3,.005),d=seq(0.7,1,.005))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        P = grid[i,1] + (grid[i,2]-grid[i,1])*(1/(1+exp(-(input$a1*th[,1]+input$a2*th[,2]+input$c))))
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_c=='g' & input$yaxis_c=='d'){
        pl <-   plot_ly(grid, 
                        x = ~g, 
                        y = ~d, 
                        z = ~LL, 
                        marker = list(color = ~LL, 
                                      showscale = TRUE,
                                      cmin=max(grid[,3])*1.2,
                                      cmax=max(grid[,3])*0.9,cauto=F),
                        width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
      
      if(input$xaxis_c=='d' & input$yaxis_c=='g'){
        pl <- plot_ly(grid, 
                      x = ~d, 
                      y = ~g, 
                      z = ~LL, 
                      marker = list(color = ~LL, 
                                    showscale = TRUE,
                                    cmin=max(grid[,3])*1.2,
                                    cmax=max(grid[,3])*0.9,cauto=F),
                      width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
    }
    
    
    
  })

  
  
  myplot4 <- eventReactive(input$draw2,{
    
    th <- gen.th2()
    y  <- gen.y2()
    
    if((input$xaxis_c=='a1' & input$yaxis_c=='a2') | 
       (input$xaxis_c=='a2' & input$yaxis_c=='a1')){
      
      grid    <- expand.grid(a1=seq(0,4,.1),a2=seq(0,4,.1))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        P = input$g_c + (input$d_c-input$g_c)*(1/(1+exp(-(grid[i,1]*th[,1]+grid[i,2]*th[,2]+input$c))))
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_c=='a1' & input$yaxis_c=='a2'){
        pl <- plot_ly(grid, 
                      x = ~a1, 
                      y = ~a2, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
      
      if(input$xaxis_c=='a2' & input$yaxis_c=='a1'){
        pl <- plot_ly(grid, 
                      x = ~a2, 
                      y = ~a1, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
    }
    
    
    
    
    
    if((input$xaxis_c=='a1' & input$yaxis_c=='c') | 
       (input$xaxis_c=='c' & input$yaxis_c=='a1')){
      
      grid    <- expand.grid(a1=seq(0,4,.1),c=seq(-3,3,.1))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        P = input$g_c + (input$d_c-input$g_c)*(1/(1+exp(-(grid[i,1]*th[,1]+input$a2*th[,2]+grid[,2]))))
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_c=='a1' & input$yaxis_c=='c'){
        pl <- plot_ly(grid, 
                      x = ~a1, 
                      y = ~c, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
      
      if(input$xaxis_c=='c' & input$yaxis_c=='a1'){
        pl <- plot_ly(grid, 
                      x = ~c, 
                      y = ~a2, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
    }
    
    
    
    
    if((input$xaxis_c=='a1' & input$yaxis_c=='g') | 
       (input$xaxis_c=='g' & input$yaxis_c=='a1')){
      
      grid    <- expand.grid(a1=seq(0,4,.1),g=seq(0,0.3,.005))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        P = grid[i,2] + (input$d_c-grid[i,2])*(1/(1+exp(-(grid[i,1]*th[,1]+input$a2*th[,2]+input$c))))
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_c=='a1' & input$yaxis_c=='g'){
        pl <- plot_ly(grid, 
                      x = ~a1, 
                      y = ~g, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
      
      if(input$xaxis_c=='g' & input$yaxis_c=='a1'){
        pl <- plot_ly(grid, 
                      x = ~g, 
                      y = ~a1, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
    }
    
    
    
    
    
    if((input$xaxis_c=='a1' & input$yaxis_c=='d') | 
       (input$xaxis_c=='d' & input$yaxis_c=='a1')){
      
      grid    <- expand.grid(a1=seq(0,4,.1),d=seq(0.7,1,.005))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        P = input$g_c + (grid[i,2]-input$g_c)*(1/(1+exp(-(grid[i,1]*th[,1]+input$a2*th[,2]+input$c))))
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_c=='a1' & input$yaxis_c=='d'){
        pl <- plot_ly(grid, 
                      x = ~a1, 
                      y = ~d, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
      
      if(input$xaxis_c=='d' & input$yaxis_c=='a1'){
        pl <- plot_ly(grid, 
                      x = ~d, 
                      y = ~a1, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
    }
    
    
    
    
    
    
    if((input$xaxis_c=='a2' & input$yaxis_c=='c') | 
       (input$xaxis_c=='c' & input$yaxis_c=='a2')){
      
      grid    <- expand.grid(a2=seq(0,4,.1),c=seq(-3,3,.1))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        P = input$g_c + (input$d_c-input$g_c)*(1/(1+exp(-(input$a1*th[,1]+grid[i,1]*th[,2]+grid[i,2]))))
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_c=='a2' & input$yaxis_c=='c'){
        pl <- plot_ly(grid, 
                      x = ~a2, 
                      y = ~c, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
      
      if(input$xaxis_c=='c' & input$yaxis_c=='a2'){
        pl <- plot_ly(grid, 
                      x = ~c, 
                      y = ~a2, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
    }
    
    
    
    
    
    if((input$xaxis_c=='a2' & input$yaxis_c=='g') | 
       (input$xaxis_c=='g' & input$yaxis_c=='a2')){
      
      grid    <- expand.grid(a2=seq(0,4,.1),g=seq(0,0.3,.005))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        P = grid[i,2] + (input$d_c-grid[i,2])*(1/(1+exp(-(input$a1*th[,1]+grid[i,1]*th[,2]+input$c))))
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_c=='a2' & input$yaxis_c=='g'){
        pl <- plot_ly(grid, 
                      x = ~a2, 
                      y = ~g, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
      
      if(input$xaxis_c=='g' & input$yaxis_c=='a2'){
        pl <- plot_ly(grid, 
                      x = ~g, 
                      y = ~a2, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
    }
    
    
    
    
    
    if((input$xaxis_c=='a2' & input$yaxis_c=='d') | 
       (input$xaxis_c=='d' & input$yaxis_c=='a2')){
      
      grid    <- expand.grid(a2=seq(0,4,.1),d=seq(0.7,1,.005))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        P = input$g_c + (grid[i,2]-input$g_c)*(1/(1+exp(-(input$a1*th[,1]+grid[i,1]*th[,2]+input$c))))
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_c=='a2' & input$yaxis_c=='d'){
        pl <- plot_ly(grid, 
                      x = ~a2, 
                      y = ~d, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
      
      if(input$xaxis_c=='d' & input$yaxis_c=='a2'){
        pl <- plot_ly(grid, 
                      x = ~d, 
                      y = ~a2, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
    }
    
    
    
    
    
    if((input$xaxis_c=='c' & input$yaxis_c=='g') | 
       (input$xaxis_c=='g' & input$yaxis_c=='c')){
      
      grid    <- expand.grid(c=seq(-3,3,.1),g=seq(0,0.3,.005))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        P = grid[i,2] + (input$d_c-grid[i,2])*(1/(1+exp(-(input$a1*th[,1]+input$a2*th[,2]+grid[i,1]))))
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_c=='c' & input$yaxis_c=='g'){
        pl <- plot_ly(grid, 
                      x = ~c, 
                      y = ~g, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
      
      if(input$xaxis_c=='g' & input$yaxis_c=='c'){
        pl <- plot_ly(grid, 
                      x = ~g, 
                      y = ~c, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
    }
    
    
    
    if((input$xaxis_c=='c' & input$yaxis_c=='d') | 
       (input$xaxis_c=='d' & input$yaxis_c=='c')){
      
      grid    <- expand.grid(c=seq(-3,3,.1),d=seq(0.7,1,.005))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        P = input$g_c + (grid[i,2]-input$g_c)*(1/(1+exp(-(input$a1*th[,1]+input$a2*th[,2]+grid[i,1]))))
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_c=='c' & input$yaxis_c=='d'){
        pl <- plot_ly(grid, 
                      x = ~c, 
                      y = ~d, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
      
      if(input$xaxis_c=='d' & input$yaxis_c=='c'){
        pl <- plot_ly(grid, 
                      x = ~d, 
                      y = ~c, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
    }
    
    
    
    
    
    
    
    if((input$xaxis_c=='g' & input$yaxis_c=='d') | 
       (input$xaxis_c=='d' & input$yaxis_c=='g')){
      
      grid    <- expand.grid(g=seq(0,.3,.005),d=seq(0.7,1,.005))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        P = grid[i,1] + (grid[i,2]-grid[i,1])*(1/(1+exp(-(input$a1*th[,1]+input$a2*th[,2]+input$c))))
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_c=='g' & input$yaxis_c=='d'){
        pl <- plot_ly(grid, 
                      x = ~g, 
                      y = ~d, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
      
      if(input$xaxis_c=='d' & input$yaxis_c=='g'){
        pl <- plot_ly(grid, 
                      x = ~d, 
                      y = ~g, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
    }
    
    
    
  })
  
  output$plot3 <- renderPlotly({
    myplot3()
  })
  
  output$plot4 <- renderPlotly({
    myplot4()
  })
  
  text2 <- eventReactive(input$draw2,{
    paste("For every draw, 10,000 responses are generated for a given set of 
item parameters and assuming that theta follow a multivariate normal distribution.
Loglikelihood is computed for possible combinations of parameters on the X-axis 
and Y-axis using the known theta values. "," ",
          "Grid points used for parameters: "," ",
          "  - a1 --> from 0 to 4 by 0.1",
          "  - a2 --> from 0 to 4 by 0.1",
          "  - c --> from -3 to 3 by 0.1",
          "  - g --> from 0 to 0.3 by 0.005",
          "  - d --> from 0.7 to 1.0 by .005", sep="\n")
    
  })
  output$info2 <- renderText({
    text2()
  })
  
  
  #############################################################################
  #############################################################################
  #############################################################################
  #############################################################################
  require(MASS)
  
  p3 <- function(t1,t2,a1,a2,b1,b2,g,d){
    
    k1 = 1/(1+exp(-a1*(t1-b1)))
    k2 = 1/(1+exp(-a2*(t2-b2)))
    g + (d-g)*(k1*k2)
  }
  
  gen.th3 <- eventReactive(input$draw3,{
    
    S <- diag(2)
    S[1,2]=S[2,1]=input$rho_pc
    th <- mvrnorm(10000,mu=c(0,0),Sigma=S)
    
    th  
  })
  
  
  gen.y3 <- eventReactive(input$draw3,{
    
    th <- gen.th3()
    
    y  <- (runif(10000,0,1) < 
             p3(t1=th[,1],
                t2=th[,2],
                a1=input$a1_pc,
                a2=input$a2_pc,
                b1=input$b1_pc,
                b2=input$b2_pc,
                g=input$g_pc,
                d=input$d_pc))*1
    
    y
  })
  
  
  
  #########################################################
  
  myplot5 <- eventReactive(input$draw3,{
    
    th <- gen.th3()
    y  <- gen.y3()
    
    if((input$xaxis_pc=='a1' & input$yaxis_pc=='a2') | 
       (input$xaxis_pc=='a2' & input$yaxis_pc=='a1')){
      
      grid    <- expand.grid(a1=seq(0,4,.1),a2=seq(0,4,.1))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        
        k1 = 1/(1+exp(-grid[i,1]*(th[,1]-input$b1_pc)))
        k2 = 1/(1+exp(-grid[i,2]*(th[,2]-input$b2_pc)))
        P = input$g_pc + (input$d_pc-input$g_pc)*(k1*k2)
        
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_pc=='a1' & input$yaxis_pc=='a2'){
        pl <-   plot_ly(grid, 
                        x = ~a1, 
                        y = ~a2, 
                        z = ~LL, 
                        marker = list(color = ~LL, 
                                      showscale = TRUE,
                                      cmin=max(grid[,3])*1.2,
                                      cmax=max(grid[,3])*0.9,cauto=F),
                        width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
      
      if(input$xaxis_pc=='a2' & input$yaxis_pc=='a1'){
        pl <- plot_ly(grid, 
                      x = ~a2, 
                      y = ~a1, 
                      z = ~LL, 
                      marker = list(color = ~LL, 
                                    showscale = TRUE,
                                    cmin=max(grid[,3])*1.2,
                                    cmax=max(grid[,3])*0.9,cauto=F),
                      width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
    }
    
    
    
    
    
    
    if((input$xaxis_pc=='a1' & input$yaxis_pc=='b1') | 
       (input$xaxis_pc=='b1' & input$yaxis_pc=='a1')){
      
      grid    <- expand.grid(a1=seq(0,4,.1),b1=seq(-3,3,.1))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        
        k1 = 1/(1+exp(-grid[i,1]*(th[,1]-grid[i,2])))
        k2 = 1/(1+exp(-input$a2_pc*(th[,2]-input$b2_pc)))
        P = input$g_pc + (input$d_pc-input$g_pc)*(k1*k2)
        
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_pc=='a1' & input$yaxis_pc=='b1'){
        pl <-   plot_ly(grid, 
                        x = ~a1, 
                        y = ~b1, 
                        z = ~LL, 
                        marker = list(color = ~LL, 
                                      showscale = TRUE,
                                      cmin=max(grid[,3])*1.2,
                                      cmax=max(grid[,3])*0.9,cauto=F),
                        width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
      
      if(input$xaxis_pc=='b1' & input$yaxis_pc=='a1'){
        pl <- plot_ly(grid, 
                      x = ~b1, 
                      y = ~a1, 
                      z = ~LL, 
                      marker = list(color = ~LL, 
                                    showscale = TRUE,
                                    cmin=max(grid[,3])*1.2,
                                    cmax=max(grid[,3])*0.9,cauto=F),
                      width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
    }
    

    
    if((input$xaxis_pc=='a1' & input$yaxis_pc=='b2') | 
       (input$xaxis_pc=='b2' & input$yaxis_pc=='a1')){
      
      grid    <- expand.grid(a1=seq(0,4,.1),b2=seq(-3,3,.1))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        
        k1 = 1/(1+exp(-grid[i,1]*(th[,1]-input$b1_pc)))
        k2 = 1/(1+exp(-input$a2_pc*(th[,2]-grid[i,2])))
        P = input$g_pc + (input$d_pc-input$g_pc)*(k1*k2)
        
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_pc=='a1' & input$yaxis_pc=='b2'){
        pl <-   plot_ly(grid, 
                        x = ~a1, 
                        y = ~b2, 
                        z = ~LL, 
                        marker = list(color = ~LL, 
                                      showscale = TRUE,
                                      cmin=max(grid[,3])*1.2,
                                      cmax=max(grid[,3])*0.9,cauto=F),
                        width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
      
      if(input$xaxis_pc=='b2' & input$yaxis_pc=='a1'){
        pl <- plot_ly(grid, 
                      x = ~b2, 
                      y = ~a1, 
                      z = ~LL, 
                      marker = list(color = ~LL, 
                                    showscale = TRUE,
                                    cmin=max(grid[,3])*1.2,
                                    cmax=max(grid[,3])*0.9,cauto=F),
                      width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
    }
    
    
    
    
    
    
    if((input$xaxis_pc=='a1' & input$yaxis_pc=='g') | 
       (input$xaxis_pc=='g' & input$yaxis_pc=='a1')){
      
      grid    <- expand.grid(a1=seq(0,4,.1),g=seq(0,.3,.005))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        
        k1 = 1/(1+exp(-grid[i,1]*(th[,1]-input$b1_pc)))
        k2 = 1/(1+exp(-input$a2_pc*(th[,2]-input$b2_pc)))
        P = grid[i,2] + (input$d_pc-grid[i,2])*(k1*k2)
        
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_pc=='a1' & input$yaxis_pc=='g'){
        pl <-   plot_ly(grid, 
                        x = ~a1, 
                        y = ~g, 
                        z = ~LL, 
                        marker = list(color = ~LL, 
                                      showscale = TRUE,
                                      cmin=max(grid[,3])*1.2,
                                      cmax=max(grid[,3])*0.9,cauto=F),
                        width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
      
      if(input$xaxis_pc=='g' & input$yaxis_pc=='a1'){
        pl <- plot_ly(grid, 
                      x = ~g, 
                      y = ~a1, 
                      z = ~LL, 
                      marker = list(color = ~LL, 
                                    showscale = TRUE,
                                    cmin=max(grid[,3])*1.2,
                                    cmax=max(grid[,3])*0.9,cauto=F),
                      width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
    }
    
    
    
    
    
    
    if((input$xaxis_pc=='a1' & input$yaxis_pc=='d') | 
       (input$xaxis_pc=='d' & input$yaxis_pc=='a1')){
      
      grid    <- expand.grid(a1=seq(0,4,.1),d=seq(0.7,1,.005))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        
        k1 = 1/(1+exp(-grid[i,1]*(th[,1]-input$b1_pc)))
        k2 = 1/(1+exp(-input$a2_pc*(th[,2]-input$b2_pc)))
        P = input$g_pc + (grid[i,2]-input$g_pc)*(k1*k2)
        
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_pc=='a1' & input$yaxis_pc=='d'){
        pl <-   plot_ly(grid, 
                        x = ~a1, 
                        y = ~d, 
                        z = ~LL, 
                        marker = list(color = ~LL, 
                                      showscale = TRUE,
                                      cmin=max(grid[,3])*1.2,
                                      cmax=max(grid[,3])*0.9,cauto=F),
                        width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
      
      if(input$xaxis_pc=='d' & input$yaxis_pc=='a1'){
        pl <- plot_ly(grid, 
                      x = ~d, 
                      y = ~a1, 
                      z = ~LL, 
                      marker = list(color = ~LL, 
                                    showscale = TRUE,
                                    cmin=max(grid[,3])*1.2,
                                    cmax=max(grid[,3])*0.9,cauto=F),
                      width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
    }
    
    
    
    
    
    
    if((input$xaxis_pc=='a2' & input$yaxis_pc=='b1') | 
       (input$xaxis_pc=='b1' & input$yaxis_pc=='a2')){
      
      grid    <- expand.grid(a2=seq(0,4,.1),b1=seq(-3,3,.1))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        
        k1 = 1/(1+exp(-input$a1_pc*(th[,1]-grid[i,2])))
        k2 = 1/(1+exp(-grid[i,1]*(th[,2]-input$b2_pc)))
        P = input$g_pc + (input$d_pc-input$g_pc)*(k1*k2)
        
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_pc=='a2' & input$yaxis_pc=='b1'){
        pl <-   plot_ly(grid, 
                        x = ~a2, 
                        y = ~b1, 
                        z = ~LL, 
                        marker = list(color = ~LL, 
                                      showscale = TRUE,
                                      cmin=max(grid[,3])*1.2,
                                      cmax=max(grid[,3])*0.9,cauto=F),
                        width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
      
      if(input$xaxis_pc=='b1' & input$yaxis_pc=='a2'){
        pl <- plot_ly(grid, 
                      x = ~b1, 
                      y = ~a2, 
                      z = ~LL, 
                      marker = list(color = ~LL, 
                                    showscale = TRUE,
                                    cmin=max(grid[,3])*1.2,
                                    cmax=max(grid[,3])*0.9,cauto=F),
                      width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
    }
    

    
    
    
    if((input$xaxis_pc=='a2' & input$yaxis_pc=='b2') | 
       (input$xaxis_pc=='b2' & input$yaxis_pc=='a2')){
      
      grid    <- expand.grid(a2=seq(0,4,.1),b2=seq(-3,3,.1))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        
        k1 = 1/(1+exp(-input$a1_pc*(th[,1]-input$b1_pc)))
        k2 = 1/(1+exp(-grid[i,1]*(th[,2]-grid[i,2])))
        P = input$g_pc + (input$d_pc-input$g_pc)*(k1*k2)
        
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_pc=='a2' & input$yaxis_pc=='b2'){
        pl <-   plot_ly(grid, 
                        x = ~a2, 
                        y = ~b2, 
                        z = ~LL, 
                        marker = list(color = ~LL, 
                                      showscale = TRUE,
                                      cmin=max(grid[,3])*1.2,
                                      cmax=max(grid[,3])*0.9,cauto=F),
                        width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
      
      if(input$xaxis_pc=='b2' & input$yaxis_pc=='a2'){
        pl <- plot_ly(grid, 
                      x = ~b2, 
                      y = ~a2, 
                      z = ~LL, 
                      marker = list(color = ~LL, 
                                    showscale = TRUE,
                                    cmin=max(grid[,3])*1.2,
                                    cmax=max(grid[,3])*0.9,cauto=F),
                      width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
    }
    

    
    
    if((input$xaxis_pc=='a2' & input$yaxis_pc=='g') | 
       (input$xaxis_pc=='g' & input$yaxis_pc=='a2')){
      
      grid    <- expand.grid(a2=seq(0,4,.1),g=seq(0,.3,.005))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        
        k1 = 1/(1+exp(-input$a1_pc*(th[,1]-input$b1_pc)))
        k2 = 1/(1+exp(-grid[i,1]*(th[,2]-input$b2_pc)))
        P = grid[i,2] + (input$d_pc-grid[i,2])*(k1*k2)
        
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_pc=='a2' & input$yaxis_pc=='g'){
        pl <-   plot_ly(grid, 
                        x = ~a2, 
                        y = ~g, 
                        z = ~LL, 
                        marker = list(color = ~LL, 
                                      showscale = TRUE,
                                      cmin=max(grid[,3])*1.2,
                                      cmax=max(grid[,3])*0.9,cauto=F),
                        width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
      
      if(input$xaxis_pc=='g' & input$yaxis_pc=='a2'){
        pl <- plot_ly(grid, 
                      x = ~g, 
                      y = ~a2, 
                      z = ~LL, 
                      marker = list(color = ~LL, 
                                    showscale = TRUE,
                                    cmin=max(grid[,3])*1.2,
                                    cmax=max(grid[,3])*0.9,cauto=F),
                      width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
    }
    
    
    
    
    
    
    
    if((input$xaxis_pc=='a2' & input$yaxis_pc=='d') | 
       (input$xaxis_pc=='d' & input$yaxis_pc=='a2')){
      
      grid    <- expand.grid(a2=seq(0,4,.1),d=seq(0.7,1,.005))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        
        k1 = 1/(1+exp(-input$a1_pc*(th[,1]-input$b1_pc)))
        k2 = 1/(1+exp(-grid[i,1]*(th[,2]-input$b2_pc)))
        P = input$g_pc + (grid[i,2]-input$g_pc)*(k1*k2)
        
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_pc=='a2' & input$yaxis_pc=='d'){
        pl <-   plot_ly(grid, 
                        x = ~a2, 
                        y = ~d, 
                        z = ~LL, 
                        marker = list(color = ~LL, 
                                      showscale = TRUE,
                                      cmin=max(grid[,3])*1.2,
                                      cmax=max(grid[,3])*0.9,cauto=F),
                        width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
      
      if(input$xaxis_pc=='d' & input$yaxis_pc=='a2'){
        pl <- plot_ly(grid, 
                      x = ~d, 
                      y = ~a2, 
                      z = ~LL, 
                      marker = list(color = ~LL, 
                                    showscale = TRUE,
                                    cmin=max(grid[,3])*1.2,
                                    cmax=max(grid[,3])*0.9,cauto=F),
                      width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
    }
    
    
    
    
    if((input$xaxis_pc=='b1' & input$yaxis_pc=='b2') | 
       (input$xaxis_pc=='b2' & input$yaxis_pc=='b1')){
      
      grid    <- expand.grid(b1=seq(-3,3,.1),b2=seq(-3,3,.1))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        
        k1 = 1/(1+exp(-input$a1_pc*(th[,1]-grid[i,1])))
        k2 = 1/(1+exp(-input$a2_pc*(th[,2]-grid[i,2])))
        P = input$g_pc + (input$d_pc-input$g_pc)*(k1*k2)
        
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_pc=='b1' & input$yaxis_pc=='b2'){
        pl <-   plot_ly(grid, 
                        x = ~b1, 
                        y = ~b2, 
                        z = ~LL, 
                        marker = list(color = ~LL, 
                                      showscale = TRUE,
                                      cmin=max(grid[,3])*1.2,
                                      cmax=max(grid[,3])*0.9,cauto=F),
                        width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
      
      if(input$xaxis_pc=='b2' & input$yaxis_pc=='b1'){
        pl <- plot_ly(grid, 
                      x = ~b2, 
                      y = ~b1, 
                      z = ~LL, 
                      marker = list(color = ~LL, 
                                    showscale = TRUE,
                                    cmin=max(grid[,3])*1.2,
                                    cmax=max(grid[,3])*0.9,cauto=F),
                      width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
    }
        
    
    
    
    
    if((input$xaxis_pc=='b1' & input$yaxis_pc=='g') | 
       (input$xaxis_pc=='g' & input$yaxis_pc=='b1')){
      
      grid    <- expand.grid(b1=seq(-3,3,.1),g=seq(0,.3,0.005))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        
        k1 = 1/(1+exp(-input$a1_pc*(th[,1]-grid[i,1])))
        k2 = 1/(1+exp(-input$a2_pc*(th[,2]-input$b2_pc)))
        P = grid[i,2] + (input$d_pc-grid[i,2])*(k1*k2)
        
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_pc=='b1' & input$yaxis_pc=='g'){
        pl <-   plot_ly(grid, 
                        x = ~b1, 
                        y = ~g, 
                        z = ~LL, 
                        marker = list(color = ~LL, 
                                      showscale = TRUE,
                                      cmin=max(grid[,3])*1.2,
                                      cmax=max(grid[,3])*0.9,cauto=F),
                        width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
      
      if(input$xaxis_pc=='g' & input$yaxis_pc=='b1'){
        pl <- plot_ly(grid, 
                      x = ~g, 
                      y = ~b1, 
                      z = ~LL, 
                      marker = list(color = ~LL, 
                                    showscale = TRUE,
                                    cmin=max(grid[,3])*1.2,
                                    cmax=max(grid[,3])*0.9,cauto=F),
                      width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
    }
    
    
    
    
    
    
    if((input$xaxis_pc=='b1' & input$yaxis_pc=='d') | 
       (input$xaxis_pc=='d' & input$yaxis_pc=='b1')){
      
      grid    <- expand.grid(b1=seq(-3,3,.1),d=seq(0.7,1,0.005))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        
        k1 = 1/(1+exp(-input$a1_pc*(th[,1]-grid[i,1])))
        k2 = 1/(1+exp(-input$a2_pc*(th[,2]-input$b2_pc)))
        P = input$g_pc + (grid[i,2]-input$g_pc)*(k1*k2)
        
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_pc=='b1' & input$yaxis_pc=='d'){
        pl <-   plot_ly(grid, 
                        x = ~b1, 
                        y = ~d, 
                        z = ~LL, 
                        marker = list(color = ~LL, 
                                      showscale = TRUE,
                                      cmin=max(grid[,3])*1.2,
                                      cmax=max(grid[,3])*0.9,cauto=F),
                        width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
      
      if(input$xaxis_pc=='d' & input$yaxis_pc=='b1'){
        pl <- plot_ly(grid, 
                      x = ~d, 
                      y = ~b1, 
                      z = ~LL, 
                      marker = list(color = ~LL, 
                                    showscale = TRUE,
                                    cmin=max(grid[,3])*1.2,
                                    cmax=max(grid[,3])*0.9,cauto=F),
                      width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
    }
    
    
    
    
    if((input$xaxis_pc=='b2' & input$yaxis_pc=='g') | 
       (input$xaxis_pc=='g' & input$yaxis_pc=='b2')){
      
      grid    <- expand.grid(b2=seq(-3,3,.1),g=seq(0,0.3,0.005))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        
        k1 = 1/(1+exp(-input$a1_pc*(th[,1]-input$b1_pc)))
        k2 = 1/(1+exp(-input$a2_pc*(th[,2]-grid[i,1])))
        P = grid[i,2] + (input$d_pc-grid[i,2])*(k1*k2)
        
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_pc=='b2' & input$yaxis_pc=='g'){
        pl <-   plot_ly(grid, 
                        x = ~b2, 
                        y = ~g, 
                        z = ~LL, 
                        marker = list(color = ~LL, 
                                      showscale = TRUE,
                                      cmin=max(grid[,3])*1.2,
                                      cmax=max(grid[,3])*0.9,cauto=F),
                        width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
      
      if(input$xaxis_pc=='g' & input$yaxis_pc=='b2'){
        pl <- plot_ly(grid, 
                      x = ~g, 
                      y = ~b2, 
                      z = ~LL, 
                      marker = list(color = ~LL, 
                                    showscale = TRUE,
                                    cmin=max(grid[,3])*1.2,
                                    cmax=max(grid[,3])*0.9,cauto=F),
                      width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
    }

    
    
    
    
    
    if((input$xaxis_pc=='b2' & input$yaxis_pc=='d') | 
       (input$xaxis_pc=='d' & input$yaxis_pc=='b2')){
      
      grid    <- expand.grid(b2=seq(-3,3,.1),d=seq(0.7,1,0.005))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        
        k1 = 1/(1+exp(-input$a1_pc*(th[,1]-input$b1_pc)))
        k2 = 1/(1+exp(-input$a2_pc*(th[,2]-grid[i,1])))
        P = input$g_pc + (grid[i,2]-input$g_pc)*(k1*k2)
        
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_pc=='b2' & input$yaxis_pc=='d'){
        pl <-   plot_ly(grid, 
                        x = ~b2, 
                        y = ~d, 
                        z = ~LL, 
                        marker = list(color = ~LL, 
                                      showscale = TRUE,
                                      cmin=max(grid[,3])*1.2,
                                      cmax=max(grid[,3])*0.9,cauto=F),
                        width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
      
      if(input$xaxis_pc=='d' & input$yaxis_pc=='b2'){
        pl <- plot_ly(grid, 
                      x = ~d, 
                      y = ~b2, 
                      z = ~LL, 
                      marker = list(color = ~LL, 
                                    showscale = TRUE,
                                    cmin=max(grid[,3])*1.2,
                                    cmax=max(grid[,3])*0.9,cauto=F),
                      width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
    }
    
    
    
    
    if((input$xaxis_pc=='g' & input$yaxis_pc=='d') | 
       (input$xaxis_pc=='d' & input$yaxis_pc=='g')){
      
      grid    <- expand.grid(g=seq(0,.3,.005),d=seq(0.7,1,0.005))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        
        k1 = 1/(1+exp(-input$a1_pc*(th[,1]-input$b1_pc)))
        k2 = 1/(1+exp(-input$a2_pc*(th[,2]-input$b2_pc)))
        P = grid[i,1] + (grid[i,2]-grid[i,1])*(k1*k2)
        
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_pc=='g' & input$yaxis_pc=='d'){
        pl <-   plot_ly(grid, 
                        x = ~g, 
                        y = ~d, 
                        z = ~LL, 
                        marker = list(color = ~LL, 
                                      showscale = TRUE,
                                      cmin=max(grid[,3])*1.2,
                                      cmax=max(grid[,3])*0.9,cauto=F),
                        width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
      
      if(input$xaxis_pc=='d' & input$yaxis_pc=='g'){
        pl <- plot_ly(grid, 
                      x = ~d, 
                      y = ~g, 
                      z = ~LL, 
                      marker = list(color = ~LL, 
                                    showscale = TRUE,
                                    cmin=max(grid[,3])*1.2,
                                    cmax=max(grid[,3])*0.9,cauto=F),
                      width=800,height=600) %>% 
          add_markers()
        return(pl)
      }
    }
    
        
    
  })
    
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  myplot6 <- eventReactive(input$draw3,{
    
    th <- gen.th3()
    y  <- gen.y3()
    
    if((input$xaxis_pc=='a1' & input$yaxis_pc=='a2') | 
       (input$xaxis_pc=='a2' & input$yaxis_pc=='a1')){
      
      grid    <- expand.grid(a1=seq(0,4,.1),a2=seq(0,4,.1))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        
        k1 = 1/(1+exp(-grid[i,1]*(th[,1]-input$b1_pc)))
        k2 = 1/(1+exp(-grid[i,2]*(th[,2]-input$b2_pc)))
        P = input$g_pc + (input$d_pc-input$g_pc)*(k1*k2)
        
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_pc=='a1' & input$yaxis_pc=='a2'){
        pl <- plot_ly(grid, 
                      x = ~a1, 
                      y = ~a2, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
      
      if(input$xaxis_pc=='a2' & input$yaxis_pc=='a1'){
        pl <- plot_ly(grid, 
                      x = ~a2, 
                      y = ~a1, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
    }
    
    
    
    
    
    
    if((input$xaxis_pc=='a1' & input$yaxis_pc=='b1') | 
       (input$xaxis_pc=='b1' & input$yaxis_pc=='a1')){
      
      grid    <- expand.grid(a1=seq(0,4,.1),b1=seq(-3,3,.1))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        
        k1 = 1/(1+exp(-grid[i,1]*(th[,1]-grid[i,2])))
        k2 = 1/(1+exp(-input$a2_pc*(th[,2]-input$b2_pc)))
        P = input$g_pc + (input$d_pc-input$g_pc)*(k1*k2)
        
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_pc=='a1' & input$yaxis_pc=='b1'){
        pl <- plot_ly(grid, 
                      x = ~a1, 
                      y = ~b1, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
      
      if(input$xaxis_pc=='b1' & input$yaxis_pc=='a1'){
        pl <- plot_ly(grid, 
                      x = ~b1, 
                      y = ~a1, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
    }
    
    
    
    if((input$xaxis_pc=='a1' & input$yaxis_pc=='b2') | 
       (input$xaxis_pc=='b2' & input$yaxis_pc=='a1')){
      
      grid    <- expand.grid(a1=seq(0,4,.1),b2=seq(-3,3,.1))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        
        k1 = 1/(1+exp(-grid[i,1]*(th[,1]-input$b1_pc)))
        k2 = 1/(1+exp(-input$a2_pc*(th[,2]-grid[i,2])))
        P = input$g_pc + (input$d_pc-input$g_pc)*(k1*k2)
        
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_pc=='a1' & input$yaxis_pc=='b2'){
        pl <- plot_ly(grid, 
                      x = ~a1, 
                      y = ~b2, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
      
      if(input$xaxis_pc=='b2' & input$yaxis_pc=='a1'){
        pl <- plot_ly(grid, 
                      x = ~b2, 
                      y = ~a1, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
    }
    
    
    
    
    
    
    if((input$xaxis_pc=='a1' & input$yaxis_pc=='g') | 
       (input$xaxis_pc=='g' & input$yaxis_pc=='a1')){
      
      grid    <- expand.grid(a1=seq(0,4,.1),g=seq(0,.3,.005))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        
        k1 = 1/(1+exp(-grid[i,1]*(th[,1]-input$b1_pc)))
        k2 = 1/(1+exp(-input$a2_pc*(th[,2]-input$b2_pc)))
        P = grid[i,2] + (input$d_pc-grid[i,2])*(k1*k2)
        
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_pc=='a1' & input$yaxis_pc=='g'){
        pl <- plot_ly(grid, 
                      x = ~a1, 
                      y = ~g, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
      
      if(input$xaxis_pc=='g' & input$yaxis_pc=='a1'){
        pl <- plot_ly(grid, 
                      x = ~g, 
                      y = ~a1, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
    }
    
    
    
    
    
    
    if((input$xaxis_pc=='a1' & input$yaxis_pc=='d') | 
       (input$xaxis_pc=='d' & input$yaxis_pc=='a1')){
      
      grid    <- expand.grid(a1=seq(0,4,.1),d=seq(0.7,1,.005))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        
        k1 = 1/(1+exp(-grid[i,1]*(th[,1]-input$b1_pc)))
        k2 = 1/(1+exp(-input$a2_pc*(th[,2]-input$b2_pc)))
        P = input$g_pc + (grid[i,2]-input$g_pc)*(k1*k2)
        
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_pc=='a1' & input$yaxis_pc=='d'){
        pl <- plot_ly(grid, 
                      x = ~a1, 
                      y = ~d, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
      
      if(input$xaxis_pc=='d' & input$yaxis_pc=='a1'){
        pl <- plot_ly(grid, 
                      x = ~d, 
                      y = ~a1, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
    }
    
    
    
    
    
    
    if((input$xaxis_pc=='a2' & input$yaxis_pc=='b1') | 
       (input$xaxis_pc=='b1' & input$yaxis_pc=='a2')){
      
      grid    <- expand.grid(a2=seq(0,4,.1),b1=seq(-3,3,.1))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        
        k1 = 1/(1+exp(-input$a1_pc*(th[,1]-grid[i,2])))
        k2 = 1/(1+exp(-grid[i,1]*(th[,2]-input$b2_pc)))
        P = input$g_pc + (input$d_pc-input$g_pc)*(k1*k2)
        
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_pc=='a2' & input$yaxis_pc=='b1'){
        pl <- plot_ly(grid, 
                      x = ~a2, 
                      y = ~b1, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
      
      if(input$xaxis_pc=='b1' & input$yaxis_pc=='a2'){
        pl <- plot_ly(grid, 
                      x = ~b1, 
                      y = ~a2, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
    }
    
    
    
    
    
    if((input$xaxis_pc=='a2' & input$yaxis_pc=='b2') | 
       (input$xaxis_pc=='b2' & input$yaxis_pc=='a2')){
      
      grid    <- expand.grid(a2=seq(0,4,.1),b2=seq(-3,3,.1))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        
        k1 = 1/(1+exp(-input$a1_pc*(th[,1]-input$b1_pc)))
        k2 = 1/(1+exp(-grid[i,1]*(th[,2]-grid[i,2])))
        P = input$g_pc + (input$d_pc-input$g_pc)*(k1*k2)
        
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_pc=='a2' & input$yaxis_pc=='b2'){
        pl <- plot_ly(grid, 
                      x = ~a2, 
                      y = ~b2, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
      
      if(input$xaxis_pc=='b2' & input$yaxis_pc=='a2'){
        pl <- plot_ly(grid, 
                      x = ~b2, 
                      y = ~a2, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
    }
    
    
    
    
    if((input$xaxis_pc=='a2' & input$yaxis_pc=='g') | 
       (input$xaxis_pc=='g' & input$yaxis_pc=='a2')){
      
      grid    <- expand.grid(a2=seq(0,4,.1),g=seq(0,.3,.005))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        
        k1 = 1/(1+exp(-input$a1_pc*(th[,1]-input$b1_pc)))
        k2 = 1/(1+exp(-grid[i,1]*(th[,2]-input$b2_pc)))
        P = grid[i,2] + (input$d_pc-grid[i,2])*(k1*k2)
        
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_pc=='a2' & input$yaxis_pc=='g'){
        pl <- plot_ly(grid, 
                      x = ~a2, 
                      y = ~g, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
      
      if(input$xaxis_pc=='g' & input$yaxis_pc=='a2'){
        pl <- plot_ly(grid, 
                      x = ~g, 
                      y = ~a2, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
    }
    
    
    
    
    
    
    
    if((input$xaxis_pc=='a2' & input$yaxis_pc=='d') | 
       (input$xaxis_pc=='d' & input$yaxis_pc=='a2')){
      
      grid    <- expand.grid(a2=seq(0,4,.1),d=seq(0.7,1,.005))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        
        k1 = 1/(1+exp(-input$a1_pc*(th[,1]-input$b1_pc)))
        k2 = 1/(1+exp(-grid[i,1]*(th[,2]-input$b2_pc)))
        P = input$g_pc + (grid[i,2]-input$g_pc)*(k1*k2)
        
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_pc=='a2' & input$yaxis_pc=='d'){
        pl <- plot_ly(grid, 
                      x = ~a2, 
                      y = ~d, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
      
      if(input$xaxis_pc=='d' & input$yaxis_pc=='a2'){
        pl <- plot_ly(grid, 
                      x = ~d, 
                      y = ~a2, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
    }
    
    
    
    
    if((input$xaxis_pc=='b1' & input$yaxis_pc=='b2') | 
       (input$xaxis_pc=='b2' & input$yaxis_pc=='b1')){
      
      grid    <- expand.grid(b1=seq(-3,3,.1),b2=seq(-3,3,.1))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        
        k1 = 1/(1+exp(-input$a1_pc*(th[,1]-grid[i,1])))
        k2 = 1/(1+exp(-input$a2_pc*(th[,2]-grid[i,2])))
        P = input$g_pc + (input$d_pc-input$g_pc)*(k1*k2)
        
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_pc=='b1' & input$yaxis_pc=='b2'){
        pl <- plot_ly(grid, 
                      x = ~b1, 
                      y = ~b2, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
      
      if(input$xaxis_pc=='b2' & input$yaxis_pc=='b1'){
        pl <- plot_ly(grid, 
                      x = ~b2, 
                      y = ~b1, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
    }
    
    
    
    
    
    if((input$xaxis_pc=='b1' & input$yaxis_pc=='g') | 
       (input$xaxis_pc=='g' & input$yaxis_pc=='b1')){
      
      grid    <- expand.grid(b1=seq(-3,3,.1),g=seq(0,.3,0.005))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        
        k1 = 1/(1+exp(-input$a1_pc*(th[,1]-grid[i,1])))
        k2 = 1/(1+exp(-input$a2_pc*(th[,2]-input$b2_pc)))
        P = grid[i,2] + (input$d_pc-grid[i,2])*(k1*k2)
        
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_pc=='b1' & input$yaxis_pc=='g'){
        pl <- plot_ly(grid, 
                      x = ~b1, 
                      y = ~g, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
      
      if(input$xaxis_pc=='g' & input$yaxis_pc=='b1'){
        pl <- plot_ly(grid, 
                      x = ~g, 
                      y = ~b1, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
    }
    
    
    
    
    
    
    if((input$xaxis_pc=='b1' & input$yaxis_pc=='d') | 
       (input$xaxis_pc=='d' & input$yaxis_pc=='b1')){
      
      grid    <- expand.grid(b1=seq(-3,3,.1),d=seq(0.7,1,0.005))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        
        k1 = 1/(1+exp(-input$a1_pc*(th[,1]-grid[i,1])))
        k2 = 1/(1+exp(-input$a2_pc*(th[,2]-input$b2_pc)))
        P = input$g_pc + (grid[i,2]-input$g_pc)*(k1*k2)
        
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_pc=='b1' & input$yaxis_pc=='d'){
        pl <- plot_ly(grid, 
                      x = ~b1, 
                      y = ~d, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
      
      if(input$xaxis_pc=='d' & input$yaxis_pc=='b1'){
        pl <- plot_ly(grid, 
                      x = ~d, 
                      y = ~b1, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
    }
    
    
    
    
    if((input$xaxis_pc=='b2' & input$yaxis_pc=='g') | 
       (input$xaxis_pc=='g' & input$yaxis_pc=='b2')){
      
      grid    <- expand.grid(b2=seq(-3,3,.1),g=seq(0,0.3,0.005))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        
        k1 = 1/(1+exp(-input$a1_pc*(th[,1]-input$b1_pc)))
        k2 = 1/(1+exp(-input$a2_pc*(th[,2]-grid[i,1])))
        P = grid[i,2] + (input$d_pc-grid[i,2])*(k1*k2)
        
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_pc=='b2' & input$yaxis_pc=='g'){
        pl <- plot_ly(grid, 
                      x = ~b2, 
                      y = ~g, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
      
      if(input$xaxis_pc=='g' & input$yaxis_pc=='b2'){
        pl <- plot_ly(grid, 
                      x = ~g, 
                      y = ~b2, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
    }
    
    
    
    
    
    
    if((input$xaxis_pc=='b2' & input$yaxis_pc=='d') | 
       (input$xaxis_pc=='d' & input$yaxis_pc=='b2')){
      
      grid    <- expand.grid(b2=seq(-3,3,.1),d=seq(0.7,1,0.005))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        
        k1 = 1/(1+exp(-input$a1_pc*(th[,1]-input$b1_pc)))
        k2 = 1/(1+exp(-input$a2_pc*(th[,2]-grid[i,1])))
        P = input$g_pc + (grid[i,2]-input$g_pc)*(k1*k2)
        
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_pc=='b2' & input$yaxis_pc=='d'){
        pl <- plot_ly(grid, 
                      x = ~b2, 
                      y = ~d, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
      
      if(input$xaxis_pc=='d' & input$yaxis_pc=='b2'){
        pl <- plot_ly(grid, 
                      x = ~d, 
                      y = ~b2, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
    }
    
    
    
    
    if((input$xaxis_pc=='g' & input$yaxis_pc=='d') | 
       (input$xaxis_pc=='d' & input$yaxis_pc=='g')){
      
      grid    <- expand.grid(g=seq(0,.3,.005),d=seq(0.7,1,0.005))
      grid$LL <- NA
      
      for(i in 1:nrow(grid)){
        
        k1 = 1/(1+exp(-input$a1_pc*(th[,1]-input$b1_pc)))
        k2 = 1/(1+exp(-input$a2_pc*(th[,2]-input$b2_pc)))
        P = grid[i,1] + (grid[i,2]-grid[i,1])*(k1*k2)
        
        grid[i,3] = sum(log(P*y + (1-P)*(1-y)))
      }
      
      if(input$xaxis_pc=='g' & input$yaxis_pc=='d'){
        pl <- plot_ly(grid, 
                      x = ~g, 
                      y = ~d, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
      
      if(input$xaxis_pc=='d' & input$yaxis_pc=='g'){
        pl <- plot_ly(grid, 
                      x = ~d, 
                      y = ~g, 
                      z = ~LL, 
                      type='contour', 
                      showscale=TRUE,
                      contours = list(
                        coloring = 'heatmap'
                      ),
                      zmin=max(grid[,3])*1.2,
                      zmax=max(grid[,3])*0.9,
                      zauto=F,
                      colors = 'Blues',
                      width=800,height=600)
        return(pl)
      }
    }
    
    
    
  })

  
  output$plot5 <- renderPlotly({
    myplot5()
  })
  
  output$plot6 <- renderPlotly({
    myplot6()
  })
  
  text3 <- eventReactive(input$draw3,{
    paste("For every draw, 10,000 responses are generated for a given set of 
item parameters and assuming that theta follow a multivariate normal distribution.
Loglikelihood is computed for possible combinations of parameters on the X-axis 
and Y-axis using the known theta values. "," ",
          "Grid points used for parameters: "," ",
          "  - a1 --> from 0 to 4 by 0.1",
          "  - a2 --> from 0 to 4 by 0.1",
          "  - b1 --> from -3 to 3 by 0.1",
          "  - b2 --> from -3 to 3 by 0.1",
          "  - g --> from 0 to 0.3 by 0.005",
          "  - d --> from 0.7 to 1.0 by .005", sep="\n")
    
  })
  output$info3 <- renderText({
    text3()
  })
  
  
})
















