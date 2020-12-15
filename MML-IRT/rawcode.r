

p <- function(t,a,b,g,d){
  g + (d-g)*(1/(1+exp(-a*(t-b))))
}

th <- rnorm(100000,0,1)

y = (runif(100000,0,1)<p(t=th,a=2,b=1,g=0.2,d=1))*1

####################################################


MlogL <- function(resp,a,b,g,d){
  
  p <- function(th,a,b,g,d){
      u = g + (d-g)*(1/(1+exp(-a*(th[,1]-b))))
  }

  nw <- createNIGrid(dim=1,type="GHN",level=40)

  P = quadrature(p,grid = nw,a=grid[i,1],b=grid[i,2],g=g,d=d)

  length(which(resp==1))*log(P) + length(which(resp==0))*log(1-P)
}

grid <- expand.grid(a=seq(0.1,3,.1),b=seq(-3,3,.1))           
grid$LL <- NA

for(i in 1:nrow(grid)){
  grid[i,]$LL = MlogL(resp=y,a=grid[i,1],b=grid[i,2],g=0.3,d=1)
  print(i)
}        
           
plot_ly(grid, 
               x = ~a, 
               y = ~b, 
               z = ~LL, 
               color = ~LL) %>% add_markers()


plot_ly(grid, 
        x = ~a, 
        y = ~b, 
        z = ~LL, 
        type='contour',color=~LL) 
           
as.numeric(grid[which.max(grid$LL),])

           
           
           
           
           