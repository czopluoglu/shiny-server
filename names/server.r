
load("data/names.RData")
load("data/names2.RData")


shinyServer(function(input, output){

plotInput <- function(){
  
 if(input$state=="US"){    
    d.sub <- names[which(names[,2]==input$name1),c(2,4,5,6)]
    if(nrow(d.sub)>0){
     info <- aggregate(cbind(V3,prop) ~ Year,data=d.sub,FUN=sum)
     plot(info$prop ~ info$Year,type="l",ylab="Proportion",xlab="Year",
          col=input$color1,lwd=input$lwd,ylim=c(0,input$ylim))
    }
    
    d.sub2 <- names[which(names[,2]==input$name2),c(2,4,5,6)]
    
    if(nrow(d.sub2)>0){
     info2  <- aggregate(cbind(V3,prop) ~ Year,data=d.sub2,FUN=sum)
     points(info2$prop ~ info2$Year,type="l",col=input$color2,lwd=input$lwd)
    }
    
    d.sub3 <- names[which(names[,2]==input$name3),c(2,4,5,6)]
    
    if(nrow(d.sub3)>0){
      info3  <- aggregate(cbind(V3,prop) ~ Year,data=d.sub3,FUN=sum)
      points(info3$prop ~ info3$Year,type="l",col=input$color3,lwd=input$lwd)
    }
    
    if(nrow(d.sub2)>0 & nrow(d.sub3)==0) {
      legend("topright", legend=c(input$name1,input$name2), 
           col=c(input$color1,input$color2),lwd=input$lwd,cex=2)
    }
    
    if(nrow(d.sub2)>0 & nrow(d.sub3)>0) {
      legend("topright", legend=c(input$name1,input$name2,input$name3), 
             col=c(input$color1,input$color2,input$color3),lwd=input$lwd,cex=2)
    }
    
    
    mtext("Database: Social Security Administration, National Data On Given Names In the Population Of U.S. Births, Prepared by Cengiz Zopluoglu @cen_zop",
          side=1,cex=1,line=4.1)
 }
  
  
  if(input$state!="US"){    
    d.sub <- subset(d,subset=(V1==input$state & V4==input$name1),select=c(V3,prop))
    if(nrow(d.sub)>0){
      info <- aggregate(prop ~ V3,data=d.sub,FUN=sum)
      plot(info$prop ~ info$V3,type="l",ylab="Proportion",xlab="Year",
           col=input$color1,lwd=input$lwd,ylim=c(0,input$ylim))
    }
    
    d.sub2 <- subset(d,subset=(V1==input$state & V4==input$name2),select=c(V3,prop))
    if(nrow(d.sub2)>0){
      info <- aggregate(prop ~ V3,data=d.sub2,FUN=sum)
      points(info$prop ~ info$V3,type="l",
           col=input$color2,lwd=input$lwd,ylim=c(0,input$ylim))
    }
    
    d.sub3 <- subset(d,subset=(V1==input$state & V4==input$name3),select=c(V3,prop))
    if(nrow(d.sub3)>0){
      info <- aggregate(prop ~ V3,data=d.sub3,FUN=sum)
      points(info$prop ~ info$V3,type="l",
           col=input$color3,lwd=input$lwd,ylim=c(0,input$ylim))
    }
        
    
    if(nrow(d.sub2)>0 & nrow(d.sub3)==0) {
      legend("topright", legend=c(input$name1,input$name2), 
             col=c(input$color1,input$color2),lwd=input$lwd,cex=2)
    }
    
    if(nrow(d.sub2)>0 & nrow(d.sub3)>0) {
      legend("topright", legend=c(input$name1,input$name2,input$name3), 
             col=c(input$color1,input$color2,input$color3),lwd=input$lwd,cex=2)
    }
    
    mtext("Database: Social Security Administration, National Data On Given Names In the Population Of U.S. Births, Prepared by Cengiz Zopluoglu @cen_zop",
          side=1,cex=1,line=4.1)
  }
}    


output$plot <- renderPlot({plotInput()}, height=700)

output$down <- downloadHandler(
  filename ="plot.jpeg",
  content = function(file) {
    jpeg(file, width=1200,height=800)
    print(plotInput())
          dev.off()
  })

})