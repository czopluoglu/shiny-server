
#Debugging

#options(shiny.error = browser) 
#options(shiny.error = recover)

load('/srv/shiny-server/deceased_turkey/data/data.Rdata')

#load('C:/Users/Dr Zopluoglu/Desktop/shiny-server/deceased_turkey/data/data.Rdata')


require(ggplot2)
library(png)
library(gridExtra)
library(grid)

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
      finaldate = paste0("Last Update: ", "Mar 22, 2020")
    })
    
    
    ##############################################################################
    
    values <- reactiveValues(sub  = NULL,
                             sub2 = NULL
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
      
      
      if(input$konya==1){
        d[[7]] = data[which(data$Sehir==cities[7]),]
      } else {
        d[[7]] = matrix(NA,nrow=1,ncol=1)
      }
      
      
      if(input$malatya==1){
        d[[8]] = data[which(data$Sehir==cities[8]),]
      } else {
        d[[8]] = matrix(NA,nrow=1,ncol=1)
      }
      
      
      if(input$sakarya==1){
        d[[9]] = data[which(data$Sehir==cities[9]),]
      } else {
        d[[9]] = matrix(NA,nrow=1,ncol=1)
      }
      
      
      if(input$tekirdag==1){
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
      
      values$sub <- subdata
      
      
      M = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
      
      mm = which(M==input$month)
      
      
      date1  <- as.Date(paste0(input$day,"/",which(M==input$month),"/",2020),
                        format="%d/%m/%Y")
      
      date2  <- as.Date(paste0(input$day,"/",which(M==input$month),"/",2010),
                        format="%d/%m/%Y")
      
      date <- seq.Date(date2,date1, by='years')
      dates <- format(date,format="%d/%m/%Y")
      
      
      sub = subdata[subdata$OlumTar%in%dates,]

      sub$day  <- substring(sub$OlumTar,1,5)
      sub$year <- substring(sub$OlumTar,7,11)
      sub$count <- 1
      
      plotd <- aggregate(count ~ year,data=sub,FUN=sum)
      colnames(plotd) <- c("Year","Total")
      plotd$Year <- as.numeric(plotd$Year)
      
      #t <- rasterGrob(png::readPNG('C:/Users/Dr Zopluoglu/Desktop/shiny-server/deceased_turkey/data/logo.png'),
      #                interpolate = TRUE)
      
      
      t <- rasterGrob(png::readPNG('/srv/shiny-server/deceased_turkey/data/logo.png'),
                      interpolate = TRUE)
      
      
      title.d <- paste0(input$month," ",input$day)
      tit <- paste0("Total Number of Deceased Individuals on ",title.d," (2010-2020)")
      
      p <- ggplot(plotd, aes(Year, Total)) + theme_bw()+
        geom_point(size=3)+
        geom_smooth(method = "loess")+
        xlab("Year")+ylab("Total")+
        ggtitle(tit)  +
        scale_x_continuous(breaks=plotd[,1])+
        scale_y_continuous(limits=c(0,max(plotd[,2]*1.5)))+
        annotation_custom(textGrob("Source: www.turkiye.gov.tr", gp=gpar(col="black")), 
                          xmin=max(plotd[,1])-2, xmax=max(plotd[,1]), 
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
      
      
      M = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
      
      date1  <- as.Date(paste0(input$day,"/",which(M==input$month),"/",2020),
                        format="%d/%m/%Y")
      
      date2  <- as.Date(paste0(input$day,"/",which(M==input$month),"/",2010),
                        format="%d/%m/%Y")
      
      date <- seq.Date(date2,date1, by='years')
      dates <- format(date,format="%d/%m/%Y")
      
      sub = values$sub[values$sub$OlumTar%in%dates,]
      
      sub$day  <- substring(sub$OlumTar,1,5)
      sub$year <- substring(sub$OlumTar,7,11)
      sub$count <- 1
      
      plotd <- aggregate(count ~ year,data=sub,FUN=sum)
      colnames(plotd) <- c("Year","Total")
      plotd$Year <- as.numeric(plotd$Year)
      
      plotd
    })
    
    output$table1 <- renderTable(digits=0,bordered=TRUE,striped=TRUE,{
      tab1()
    })
    
    ################################################################################
    
    myplot2 <- eventReactive(req(isTruthy(input$submit)),{
      
      #t <- rasterGrob(png::readPNG('C:/Users/Dr Zopluoglu/Desktop/shiny-server/deceased_turkey/data/logo.png'),
      #                interpolate = TRUE)
      
      t <- rasterGrob(png::readPNG('/srv/shiny-server/deceased_turkey/data/logo.png'),
                      interpolate = TRUE)
      
      
      M = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
      
      date1  <- as.Date(paste0(input$day,"/",which(M==input$month),"/",2020),
                        format="%d/%m/%Y")
      
      date2  <- as.Date(paste0(input$day,"/",which(M==input$month),"/",2010),
                        format="%d/%m/%Y")
      
      date <- seq.Date(date2,date1, by='years')
      dates <- format(date,format="%d/%m/%Y")
      
      sub = values$sub[values$sub$OlumTar%in%dates,]
      
      sub$day  <- substring(sub$OlumTar,1,5)
      sub$year <- substring(sub$OlumTar,7,11)
      sub$count <- 1
      
      plotd2 <- aggregate(count ~ year,data=sub[which(sub$Yasi>64),],FUN=sum)
      colnames(plotd2) <- c("Year","Total")
      plotd2$Year <- as.numeric(plotd2$Year)
      
      title.d <- paste0(input$month," ",input$day)
      tit = paste0("Total Number of Deceased Individuals on ",title.d," (2010-2020)")
      
      p <- ggplot(plotd2, aes(Year, Total)) + theme_bw()+
        geom_point(size=3)+
        geom_smooth(method = "loess")+
        xlab("Year")+ylab("Total")+
        ggtitle(tit)+ 
        scale_x_continuous(breaks=plotd2[,1])+
        scale_y_continuous(limits=c(0,max(plotd2[,2]*1.5)))+
        annotation_custom(textGrob("Source: www.turkiye.gov.tr", gp=gpar(col="black")), 
                          xmin=max(plotd2[,1])-2, xmax=max(plotd2[,1]),
                          ymin=10, ymax=10) +
        annotation_custom(t, xmin = 2010, xmax = 2013, ymin =0, ymax =max(plotd2[,2]*.1))+
        theme(plot.title = element_text(lineheight=.8, face="bold"),
               plot.margin=margin(1,1,3,1))
      
      p
    })
    

    output$plot2 <- renderPlot({
      myplot2()
    },height = 450, width = 700)
    
    tab2 <- eventReactive(req(isTruthy(input$submit)), {
      
      M = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
      
      date1  <- as.Date(paste0(input$day,"/",which(M==input$month),"/",2020),
                        format="%d/%m/%Y")
      
      date2  <- as.Date(paste0(input$day,"/",which(M==input$month),"/",2010),
                        format="%d/%m/%Y")
      
      date <- seq.Date(date2,date1, by='years')
      dates <- format(date,format="%d/%m/%Y")
      
      sub = values$sub[values$sub$OlumTar%in%dates,]
      
      sub$day  <- substring(sub$OlumTar,1,5)
      sub$year <- substring(sub$OlumTar,7,11)
      sub$count <- 1
      
      plotd2 <- aggregate(count ~ year,data=sub[which(sub$Yasi>64),],FUN=sum)
      colnames(plotd2) <- c("Year","Total")
      plotd2$Year <- as.numeric(plotd2$Year)
      
      plotd2
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
      
      
      if(input$konya==1){
        d[[7]] = data[which(data$Sehir==cities[7]),]
      } else {
        d[[7]] = matrix(NA,nrow=1,ncol=1)
      }
      
      
      if(input$malatya==1){
        d[[8]] = data[which(data$Sehir==cities[8]),]
      } else {
        d[[8]] = matrix(NA,nrow=1,ncol=1)
      }
      
      
      if(input$sakarya==1){
        d[[9]] = data[which(data$Sehir==cities[9]),]
      } else {
        d[[9]] = matrix(NA,nrow=1,ncol=1)
      }
      
      
      if(input$tekirdag==1){
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
      
      
      values$sub2 <- subdata
      
      
      M = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
      
      beg  <- as.Date(paste0(input$day.beg,"/",which(M==input$month.beg),"/",2020),format="%d/%m/%Y")
      beg2 <- as.Date(paste0(input$day.end,"/",which(M==input$month.end),"/",2020),format="%d/%m/%Y")
      
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
      
      plotd3 <- aggregate(count ~ year,data=sub,FUN=sum)
      colnames(plotd3) <- c("Year","Total")
      plotd3$Year <- as.numeric(plotd3$Year)
      
       t <- rasterGrob(png::readPNG('/srv/shiny-server/deceased_turkey/data/logo.png'),
                      interpolate = TRUE)
                      
                      
      #t <- rasterGrob(png::readPNG('C:/Users/Dr Zopluoglu/Desktop/shiny-server/deceased_turkey/data/logo.png'),
      #                interpolate = TRUE)
      
      title.d <- paste0(input$month.beg," ",input$day.beg," and ",
                        input$month.end," ",input$day.end)
      
      p <- ggplot(plotd3, aes(Year, Total)) + theme_bw()+
        geom_point(size=3)+
        geom_smooth(method = "loess")+
        xlab("Year")+ylab("Total")+
        ggtitle(paste0("Total Number of Deceased Individuals between ",title.d," (2010-2020)"))  +
        scale_x_continuous(breaks=plotd3[,1])+
        scale_y_continuous(limits=c(0,max(plotd3[,2]*1.2)))+
        annotation_custom(textGrob("Source: www.turkiye.gov.tr", gp=gpar(col="black")), 
                          xmin=2018, xmax=2020, 
                          ymin=10, ymax=10) +
        annotation_custom(t, xmin = 2009.5, xmax = 2012, ymin =0, ymax =max(plotd3[,2]*.1))+
        theme(plot.title = element_text(lineheight=.8, face="bold"),
              plot.margin=margin(1,1,3,1))
      
      p
    })
    
    
    output$plot3 <- renderPlot({
      myplot3()
    },height = 450, width = 700)
    
    tab3 <- eventReactive(req(isTruthy(input$submit2)), {
      
      M = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
      
      beg  <- as.Date(paste0(input$day.beg,"/",which(M==input$month.beg),"/",2020),format="%d/%m/%Y")
      beg2 <- as.Date(paste0(input$day.end,"/",which(M==input$month.end),"/",2020),format="%d/%m/%Y")
      
      date <- seq.Date(beg,beg2, by='days')
      date <- format(date,format="%d/%m/%Y")
      
      dates = c(paste0(substring(as.character(date[1]),1,6),2010:2019),as.character(date[1]))
      for(i in 2:length(date)){
        dates = c(dates,
                  c(paste0(substring(as.character(date[i]),1,6),2010:2019),as.character(date[i])))
      }
      
      sub = values$sub2[values$sub2$OlumTar%in%dates,]
      
      sub$day  <- substring(sub$OlumTar,1,5)
      sub$year <- substring(sub$OlumTar,7,11)
      sub$count <- 1
      
      plotd3 <- aggregate(count ~ year,data=sub,FUN=sum)
      colnames(plotd3) <- c("Year","Total")
      plotd3$Year <- as.numeric(plotd3$Year)
      
      plotd3
    })
    
    output$table3 <- renderTable(digits=0,bordered=TRUE,striped=TRUE,{
      tab3()
    })
    
    ###################################################################################

    myplot4 <- eventReactive(req(isTruthy(input$submit2)),{
      
      M = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
      
      beg  <- as.Date(paste0(input$day.beg,"/",which(M==input$month.beg),"/",2020),format="%d/%m/%Y")
      beg2 <- as.Date(paste0(input$day.end,"/",which(M==input$month.end),"/",2020),format="%d/%m/%Y")
      
      date <- seq.Date(beg,beg2, by='days')
      date <- format(date,format="%d/%m/%Y")
      
      dates = c(paste0(substring(as.character(date[1]),1,6),2010:2019),as.character(date[1]))
      for(i in 2:length(date)){
        dates = c(dates,
                  c(paste0(substring(as.character(date[i]),1,6),2010:2019),as.character(date[i])))
      }
      
      
      sub = values$sub2[values$sub2$OlumTar%in%dates,]
      sub$day  <- substring(sub$OlumTar,1,5)
      sub$year <- substring(sub$OlumTar,7,11)
      sub$count <- 1
      
      sub2 <- sub[which(sub$Yasi>64),]
      
      plotd4 <- aggregate(count ~ year,data=sub2,FUN=sum)
      colnames(plotd4) <- c("Year","Total")
      plotd4$Year <- as.numeric(plotd4$Year)
      
      t <- rasterGrob(png::readPNG('/srv/shiny-server/deceased_turkey/data/logo.png'),
                       interpolate = TRUE)
     
     # t <- rasterGrob(png::readPNG('C:/Users/Dr Zopluoglu/Desktop/shiny-server/deceased_turkey/data/logo.png'),
     #                  interpolate = TRUE)
      
      title.d <- paste0(input$month.beg," ",input$day.beg," and ",
                        input$month.end," ",input$day.end)
      
      p <- ggplot(plotd4, aes(Year, Total)) + theme_bw()+
        geom_point(size=3)+
        geom_smooth(method = "loess")+
        xlab("Year")+ylab("Total")+
        ggtitle(paste0("Total Number of Deceased Individuals (Age > 64) between ",title.d," (2010-2020)"))  +
        scale_x_continuous(breaks=plotd4[,1])+
        scale_y_continuous(limits=c(0,max(plotd4[,2]*1.2)))+
        annotation_custom(textGrob("Source: www.turkiye.gov.tr", gp=gpar(col="black")), 
                          xmin=2018, xmax=2020, 
                          ymin=10, ymax=10) +
        annotation_custom(t, xmin = 2009.5, xmax = 2012, ymin =0, ymax =max(plotd4[,2]*.1))+
        theme(plot.title = element_text(lineheight=.8, face="bold"),
              plot.margin=margin(1,1,3,1))
      
      p
    })
    
    output$plot4 <- renderPlot({
      myplot4()
    },height = 450, width = 700)
    
    tab4 <- eventReactive(req(isTruthy(input$submit2)), {
      
      M = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
      
      beg  <- as.Date(paste0(input$day.beg,"/",which(M==input$month.beg),"/",2020),format="%d/%m/%Y")
      beg2 <- as.Date(paste0(input$day.end,"/",which(M==input$month.end),"/",2020),format="%d/%m/%Y")
      
      date <- seq.Date(beg,beg2, by='days')
      date <- format(date,format="%d/%m/%Y")
      
      dates = c(paste0(substring(as.character(date[1]),1,6),2010:2019),as.character(date[1]))
      for(i in 2:length(date)){
        dates = c(dates,
                  c(paste0(substring(as.character(date[i]),1,6),2010:2019),as.character(date[i])))
      }
      
      sub = values$sub2[values$sub2$OlumTar%in%dates,]
      sub$day  <- substring(sub$OlumTar,1,5)
      sub$year <- substring(sub$OlumTar,7,11)
      sub$count <- 1
      
      sub2 <- sub[which(sub$Yasi>64),]
      
      plotd4 <- aggregate(count ~ year,data=sub2,FUN=sum)
      colnames(plotd4) <- c("Year","Total")
      plotd4$Year <- as.numeric(plotd4$Year)
      
      plotd4
    })
    
    output$table4 <- renderTable(digits=0,bordered=TRUE,striped=TRUE,{
      tab4()
    })
    
})
