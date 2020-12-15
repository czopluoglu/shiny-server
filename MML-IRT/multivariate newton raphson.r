require(numDeriv)

############################################################

# TWO ITEMS

p <- function(a,b,g,d,t){
  g + (d-g)*(1/(1+exp(-a*(t-b))))
}

input <- list(a1 = 2,b1=-1,g1=0,d1=1,
              a2 = 2.5,b2=1,g2=0,d2=1)

th <- rnorm(10000,0,1)
y1  <- (runif(10000,0,1) < 
         p(t=th,
           a=input$a1,
           b=input$b1,
           g=input$g1,
           d=input$d1))*1

y2  <- (runif(10000,0,1) < 
          p(t=th,
            a=input$a2,
            b=input$b2,
            g=input$g2,
            d=input$d2))*1



L <- function(st,y1,y2,t){
  
  P1 <- (1/(1+exp(-st[1]*(t-st[2]))))
  P2 <- (1/(1+exp(-st[3]*(t-st[4]))))
  
  sum(log((P1*y1 + (1-P1)*(1-y1))*
            (P2*y2 + (1-P2)*(1-y2))
          )
      )
}


st <- c(1.5,-.5,1.5,0.5)
iter = 1

while(iter<1000){
gr <- grad(L,x=st,y1=y1,y2=y2,t=th)
hs <- hessian(L,x=st,y1=y1,y2=y2,t=th)

st <- st - solve(hs)%*%as.matrix(gr)
cat(st,'\n')
iter = iter+1
}













