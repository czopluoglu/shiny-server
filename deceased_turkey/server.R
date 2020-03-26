
load("/data/data.RData")

require(ggplot2)
library(png)
library(gridExtra)
library(grid)
library(magick)

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
    
    url3 <- a("https://www.turkiye.gov.tr/", href="https://www.turkiye.gov.tr/")
    output$info3 <- renderUI({
      tagList("Source:", url3)
    })
    
    output$finalupdate <- renderText({
      # Read the final date from the data
      finaldate = paste0("Final Update: ", "Mar 22, 2020")
    })
    
    
    ##############################################################################
    
    values <- reactiveValues(df_data = data.frame(matrix(NA,nrow=11,ncol=2)),
                             df_data2 = data.frame(matrix(NA,nrow=11,ncol=2)),
                             df_data3 = data.frame(matrix(NA,nrow=11,ncol=2)),
                             df_data4 = data.frame(matrix(NA,nrow=11,ncol=2))
                             )
    
    ##############################################################################

    myplot <- eventReactive(req(isTruthy(input$submit)),{
      
      cities = c('bursa','denizli','diyarbakir','istanbul','kahramanmaras',
                 'kocaeli','konya','malatya','sakarya','tekirdag')
      
      d <- vector("list",10)
      
      if(input$bursa==1){
        d[[1]] = data[which(data$Sehir==cities[1]),]
      } else {
        d[[1]] = matrix(NA,nrow=1,ncol=1)
      }
      
      
      if(input$denizli==1){
        d[[2]] = data[which(data$Sehir==cities[2]),]
      } else {
        d[[2]] = matrix(NA,nrow=1,ncol=1)
      }
      
      
      if(input$diyarbakir==1){
        d[[3]] = data[which(data$Sehir==cities[3]),]
      } else {
        d[[3]] = matrix(NA,nrow=1,ncol=1)
      }
      
      
      if(input$istanbul==1){
        d[[4]] = data[which(data$Sehir==cities[4]),]
      } else {
        d[[4]] = matrix(NA,nrow=1,ncol=1)
      }
      
      
      if(input$kahramanmaras==1){
        d[[5]] = data[which(data$Sehir==cities[5]),]
      } else {
        d[[5]] = matrix(NA,nrow=1,ncol=1)
      }
      
      
      if(input$kocaeli==1){
        d[[6]] = data[which(data$Sehir==cities[6]),]
      } else {
        d[[6]] = matrix(NA,nrow=1,ncol=1)
      }
      
      
      if(input$konya==7){
        d[[7]] = data[which(data$Sehir==cities[7]),]
      } else {
        d[[7]] = matrix(NA,nrow=1,ncol=1)
      }
      
      
      if(input$malatya==8){
        d[[8]] = data[which(data$Sehir==cities[8]),]
      } else {
        d[[8]] = matrix(NA,nrow=1,ncol=1)
      }
      
      
      if(input$sakarya==9){
        d[[9]] = data[which(data$Sehir==cities[9]),]
      } else {
        d[[9]] = matrix(NA,nrow=1,ncol=1)
      }
      
      
      if(input$tekirdag==10){
        d[[10]] = data[which(data$Sehir==cities[10]),]
      } else {
        d[[10]] = matrix(NA,nrow=1,ncol=1)
      }
      
      
      dl <- c()
      for(i in 1:10){
        dl[i]=nrow(d[[i]])
      }
      
      s = which(dl!=1)
      
      subdata = d[[s[1]]]
      
      if(length(s)>1){
        for(i in 2:length(s)){
          subdata = rbind(subdata,d[[s[i]]])
        }
      }
      
      
      dates = c(paste0(substring(input$date,1,6),2010:2019),input$date)
      
      sub = subdata[subdata$OlumTar%in%dates,]
      
      plotd = data.frame(matrix(NA,nrow=11,ncol=2))
      
      plotd[,1]=2010:2020
      plotd[,2]= as.numeric(table(sub$OlumTar))
      colnames(plotd) <- c("Year","Total")
      
      values$df_data = plotd
      
      
      plotd2 = data.frame(matrix(NA,nrow=11,ncol=2))
      plotd2[,1]=2010:2020
      plotd2[,2]= as.numeric(table(sub[which(sub$Yasi>64),]$OlumTar))
      colnames(plotd2) <- c("Year","Total")
      values$df_data2 = plotd2

      t <- rasterGrob(png::readPNG('/data/logo.png'),
                      interpolate = TRUE)
      
      title.d <- format(as.Date(input$date,format="%d/%m/%Y"),"%b %d")
      
      p <- ggplot(plotd, aes(Year, Total)) + theme_bw()+
        geom_point(size=3,color=c(rep("black",10),"red"))+
        geom_smooth(method = "loess")+
        xlab("Year")+ylab("Total")+
        ggtitle(paste0("Total Number of Deceased Individuals in TURKEY on ",title.d," (2010-2020)"))  +
        scale_x_continuous(breaks=2010:2020)+
        scale_y_continuous(limits=c(0,max(plotd[,2]*1.5)))+
        annotation_custom(textGrob("Source: www.turkiye.gov.tr", gp=gpar(col="black")), 
                          xmin=2018, xmax=2020, 
                          ymin=10, ymax=10) +
        annotation_custom(t, xmin = 2010, xmax = 2013, ymin =0, ymax =max(plotd[,2]*.1))+
        theme(plot.title = element_text(lineheight=.8, face="bold"),
              plot.margin=margin(1,1,3,1))
      
      p
    })
    
    output$plot <- renderPlot({
      myplot()
    },height = 450, width = 700)
   
    tab1 <- eventReactive(req(isTruthy(input$submit)), {
      values$df_data
    })
    
    output$table1 <- renderTable(digits=0,bordered=TRUE,striped=TRUE,{
      tab1()
    })
    
    ################################################################################
    
    myplot2 <- eventReactive(req(isTruthy(input$submit)),{
      
      t <- rasterGrob(png::readPNG('/data/logo.png'),
                      interpolate = TRUE)
      
      title.d <- format(as.Date(input$date,format="%d/%m/%Y"),"%b %d")
      
      p <- ggplot(values$df_data2, aes(Year, Total)) + theme_bw()+
        geom_point(size=3,color=c(rep("black",10),"red"))+
        geom_smooth(method = "loess")+
        xlab("Year")+ylab("Total")+
        ggtitle(paste0("Total Number of Deceased Individuals (Age > 64) in TURKEY on ",title.d," (2010-2020)")) + 
        scale_x_continuous(breaks=2010:2020)+
        scale_y_continuous(limits=c(0,max(values$df_data2[,2]*1.5)))+
        annotation_custom(textGrob("Source: www.turkiye.gov.tr", gp=gpar(col="black")), 
                          xmin=2018, xmax=2020, 
                          ymin=10, ymax=10) +
        annotation_custom(t, xmin = 2010, xmax = 2013, ymin =0, ymax =max(values$df_data2[,2]*.1))+
        theme(plot.title = element_text(lineheight=.8, face="bold"),
               plot.margin=margin(1,1,3,1))
      
      p
    })
    

    output$plot2 <- renderPlot({
      myplot2()
    },height = 450, width = 700)
    
    tab2 <- eventReactive(req(isTruthy(input$submit)), {
      values$df_data2
    })
    
    output$table2 <- renderTable(digits=0,bordered=TRUE,striped=TRUE,{
      tab2()
    })
    
    #################################################################################
    
    myplot3 <- eventReactive(req(isTruthy(input$submit2)),{
      
      cities = c('bursa','denizli','diyarbakir','istanbul','kahramanmaras',
                 'kocaeli','konya','malatya','sakarya','tekirdag')
      
      d <- vector("list",10)
      
      if(input$bursa==1){
        d[[1]] = data[which(data$Sehir==cities[1]),]
      } else {
        d[[1]] = matrix(NA,nrow=1,ncol=1)
      }
      
      
      if(input$denizli==1){
        d[[2]] = data[which(data$Sehir==cities[2]),]
      } else {
        d[[2]] = matrix(NA,nrow=1,ncol=1)
      }
      
      
      if(input$diyarbakir==1){
        d[[3]] = data[which(data$Sehir==cities[3]),]
      } else {
        d[[3]] = matrix(NA,nrow=1,ncol=1)
      }
      
      
      if(input$istanbul==1){
        d[[4]] = data[which(data$Sehir==cities[4]),]
      } else {
        d[[4]] = matrix(NA,nrow=1,ncol=1)
      }
      
      
      if(input$kahramanmaras==1){
        d[[5]] = data[which(data$Sehir==cities[5]),]
      } else {
        d[[5]] = matrix(NA,nrow=1,ncol=1)
      }
      
      
      if(input$kocaeli==1){
        d[[6]] = data[which(data$Sehir==cities[6]),]
      } else {
        d[[6]] = matrix(NA,nrow=1,ncol=1)
      }
      
      
      if(input$konya==7){
        d[[7]] = data[which(data$Sehir==cities[7]),]
      } else {
        d[[7]] = matrix(NA,nrow=1,ncol=1)
      }
      
      
      if(input$malatya==8){
        d[[8]] = data[which(data$Sehir==cities[8]),]
      } else {
        d[[8]] = matrix(NA,nrow=1,ncol=1)
      }
      
      
      if(input$sakarya==9){
        d[[9]] = data[which(data$Sehir==cities[9]),]
      } else {
        d[[9]] = matrix(NA,nrow=1,ncol=1)
      }
      
      
      if(input$tekirdag==10){
        d[[10]] = data[which(data$Sehir==cities[10]),]
      } else {
        d[[10]] = matrix(NA,nrow=1,ncol=1)
      }
      
      
      dl <- c()
      for(i in 1:10){
        dl[i]=nrow(d[[i]])
      }
      
      s = which(dl!=1)
      
      subdata = d[[s[1]]]
      
      if(length(s)>1){
        for(i in 2:length(s)){
          subdata = rbind(subdata,d[[s[i]]])
        }
      }
      
      
      beg  <- as.Date(input$begin,format="%d/%m/%Y")
      beg2 <- as.Date(input$end,format="%d/%m/%Y")
      date <- seq.Date(beg,beg2, by='days')
      date <- format(date,format="%d/%m/%Y")
      
      dates = c(paste0(substring(as.character(date[1]),1,6),2010:2019),as.character(date[1]))
      for(i in 2:length(date)){
        dates = c(dates,
                  c(paste0(substring(as.character(date[i]),1,6),2010:2019),as.character(date[i])))
      }
      
      sub = subdata[subdata$OlumTar%in%dates,]
      
      sub$day  <- substring(sub$OlumTar,1,5)
      sub$year <- substring(sub$OlumTar,7,11)
      sub$count <- 1
      
      plotd <- aggregate(count ~ year,data=sub,FUN=sum)
      colnames(plotd) <- c("Year","Total")
      plotd$Year <- as.numeric(plotd$Year)
      
      values$df_data3 = plotd
      
      
      sub2 <- sub[which(sub$Yasi>64),]
      plotd2 <- aggregate(count ~ year,data=sub2,FUN=sum)
      colnames(plotd2) <- c("Year","Total")
      plotd2$Year <- as.numeric(plotd2$Year)
      values$df_data4 = plotd2
      
      t <- rasterGrob(png::readPNG('/data/logo.png'),
                      interpolate = TRUE)
      
      title.d <- paste0(format(as.Date(input$begin,format="%d/%m/%Y"),"%b %d")," and ",
                        format(as.Date(input$end,format="%d/%m/%Y"),"%b %d"))
      
      p <- ggplot(values$df_data3, aes(Year, Total)) + theme_bw()+
        geom_point(size=3,color=c(rep("black",10),"red"))+
        geom_smooth(method = "loess")+
        xlab("Year")+ylab("Total")+
        ggtitle(paste0("Total Number of Deceased Individuals in TURKEY between ",title.d," (2010-2020)"))  +
        scale_x_continuous(breaks=2010:2020)+
        scale_y_continuous(limits=c(0,max(values$df_data3[,2]*1.2)))+
        annotation_custom(textGrob("Source: www.turkiye.gov.tr", gp=gpar(col="black")), 
                          xmin=2018, xmax=2020, 
                          ymin=10, ymax=10) +
        annotation_custom(t, xmin = 2009.5, xmax = 2012, ymin =0, ymax =max(values$df_data3[,2]*.1))+
        theme(plot.title = element_text(lineheight=.8, face="bold"),
              plot.margin=margin(1,1,3,1))
      
      p
    })
    
    
    output$plot3 <- renderPlot({
      myplot3()
    },height = 450, width = 700)
    
    tab3 <- eventReactive(req(isTruthy(input$submit2)), {
      values$df_data3
    })
    
    output$table3 <- renderTable(digits=0,bordered=TRUE,striped=TRUE,{
      tab3()
    })
    
    ###################################################################################

    myplot4 <- eventReactive(req(isTruthy(input$submit2)),{
      
      t <- rasterGrob(png::readPNG('/data/logo.png'),
                      interpolate = TRUE)
      
      title.d <- paste0(format(as.Date(input$begin,format="%d/%m/%Y"),"%b %d")," and ",
                        format(as.Date(input$end,format="%d/%m/%Y"),"%b %d"))
      
      p <- ggplot(values$df_data4, aes(Year, Total)) + theme_bw()+
        geom_point(size=3,color=c(rep("black",10),"red"))+
        geom_smooth(method = "loess")+
        xlab("Year")+ylab("Total")+
        ggtitle(paste0("Total Number of Deceased Individuals (Age > 64) in TURKEY between ",title.d," (2010-2020)"))  +
        scale_x_continuous(breaks=2010:2020)+
        scale_y_continuous(limits=c(0,max(values$df_data4[,2]*1.2)))+
        annotation_custom(textGrob("Source: www.turkiye.gov.tr", gp=gpar(col="black")), 
                          xmin=2018, xmax=2020, 
                          ymin=10, ymax=10) +
        annotation_custom(t, xmin = 2009.5, xmax = 2012, ymin =0, ymax =max(values$df_data4[,2]*.1))+
        theme(plot.title = element_text(lineheight=.8, face="bold"),
              plot.margin=margin(1,1,3,1))
      
      p
    })
    
    output$plot4 <- renderPlot({
      myplot4()
    },height = 450, width = 700)
    
    tab4 <- eventReactive(req(isTruthy(input$submit2)), {
      values$df_data4
    })
    
    output$table4 <- renderTable(digits=0,bordered=TRUE,striped=TRUE,{
      tab4()
    })
    
})
