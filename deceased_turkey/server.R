
#Debugging

#options(shiny.error = browser) 
#options(shiny.error = recover)

#setwd("C:/Users/Dr Zopluoglu/Desktop/shiny-server/deceased_turkey/data")
setwd("F:/shiny-server/deceased_turkey/data/")
#setwd("/srv/shiny-server/deceased_turkey/data/")

load('data.Rdata')

data2 <- data

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
      finaldate = paste0("Last Update: ", format(Sys.Date(),"%B %d,%Y"))
    })
    
    ##############################################################################

    myplot <- eventReactive(req(isTruthy(input$submit)),{
      
      if(input$sehir!='all cities'){
        
        data <- data[which(data$Sehir==input$sehir),]
        
      }
      
      M = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
      
      date1  <- as.Date(paste0(input$day,"/",which(M==input$month),"/",2020),
                        format="%d/%m/%Y")
      
      date2  <- as.Date(paste0(input$day,"/",which(M==input$month),"/",2010),
                        format="%d/%m/%Y")
      
      date <- seq.Date(date2,date1, by='years')
      dates <- format(date,format="%d/%m/%Y")
      
      
      sub1 = data[data$OlumTar%in%dates,]

      sub1$day  <- substring(sub1$OlumTar,1,5)
      sub1$year <- substring(sub1$OlumTar,7,11)
      sub1$count <- 1
      
      plotd <- aggregate(count ~ year,data=sub1,FUN=sum)
      colnames(plotd) <- c("Year","Total")
      plotd$Year <- as.numeric(plotd$Year)
      
      #t <- rasterGrob(png::readPNG('C:/Users/Dr Zopluoglu/Desktop/shiny-server/deceased_turkey/data/logo.png'),interpolate = TRUE)
      #t <- rasterGrob(png::readPNG('logo.png'),interpolate = TRUE)
      
      
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
        #annotation_custom(t, xmin = 2010, xmax = 2013, ymin =0, ymax =max(plotd[,2]*.1))+
        theme(plot.title = element_text(lineheight=.8, face="bold"),
              plot.margin=margin(1,1,3,1))
      
      p
    })
    
    output$plot <- renderPlot({
      myplot()
    },height = 450, width = 700)
   
    tab1 <- eventReactive(req(isTruthy(input$submit)), {
      
      if(input$sehir!='all cities'){
        
        data <- data[which(data$Sehir==input$sehir),]
        
      }

      M = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
      
      date1  <- as.Date(paste0(input$day,"/",which(M==input$month),"/",2020),
                        format="%d/%m/%Y")
      
      date2  <- as.Date(paste0(input$day,"/",which(M==input$month),"/",2010),
                        format="%d/%m/%Y")
      
      date <- seq.Date(date2,date1, by='years')
      dates <- format(date,format="%d/%m/%Y")
      
      
      sub1 = data[data$OlumTar%in%dates,]
      
      sub1$day  <- substring(sub1$OlumTar,1,5)
      sub1$year <- substring(sub1$OlumTar,7,11)
      sub1$count <- 1
      
      plotd <- aggregate(count ~ year,data=sub1,FUN=sum)
      colnames(plotd) <- c("Year","Total")
      plotd$Year <- as.numeric(plotd$Year)

      plotd
    })
    
    output$table1 <- renderTable(digits=0,bordered=TRUE,striped=TRUE,{
      tab1()
    })
    
    ################################################################################
    
    myplot2 <- eventReactive(req(isTruthy(input$submit)),{
      
      if(input$sehir!='all cities'){
        
        data <- data[which(data$Sehir==input$sehir),]
        
      }
      
      #t <- rasterGrob(png::readPNG('C:/Users/Dr Zopluoglu/Desktop/shiny-server/deceased_turkey/data/logo.png'),interpolate = TRUE)
      #t <- rasterGrob(png::readPNG('logo.png'),interpolate = TRUE)
      
      
      M = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
      
      date1  <- as.Date(paste0(input$day,"/",which(M==input$month),"/",2020),
                        format="%d/%m/%Y")
      
      date2  <- as.Date(paste0(input$day,"/",which(M==input$month),"/",2010),
                        format="%d/%m/%Y")
      
      date <- seq.Date(date2,date1, by='years')
      dates <- format(date,format="%d/%m/%Y")
      
      sub2 = data[data$OlumTar%in%dates,]
      
      sub2$day  <- substring(sub2$OlumTar,1,5)
      sub2$year <- substring(sub2$OlumTar,7,11)
      sub2$count <- 1
      
      plotd2 <- aggregate(count ~ year,data=sub2[which(sub2$Yasi>64),],FUN=sum)
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
       # annotation_custom(t, xmin = 2010, xmax = 2013, ymin =0, ymax =max(plotd2[,2]*.1))+
        theme(plot.title = element_text(lineheight=.8, face="bold"),
               plot.margin=margin(1,1,3,1))
      
      p
    })
    

    output$plot2 <- renderPlot({
      myplot2()
    },height = 450, width = 700)
    
    tab2 <- eventReactive(req(isTruthy(input$submit)), {
      
      if(input$sehir!='all cities'){
        
        data <- data[which(data$Sehir==input$sehir),]
        
      }
      
      M = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
      
      date1  <- as.Date(paste0(input$day,"/",which(M==input$month),"/",2020),
                        format="%d/%m/%Y")
      
      date2  <- as.Date(paste0(input$day,"/",which(M==input$month),"/",2010),
                        format="%d/%m/%Y")
      
      date <- seq.Date(date2,date1, by='years')
      dates <- format(date,format="%d/%m/%Y")
      
      sub2 = data[data$OlumTar%in%dates,]
      
      sub2$day  <- substring(sub2$OlumTar,1,5)
      sub2$year <- substring(sub2$OlumTar,7,11)
      sub2$count <- 1
      
      plotd2 <- aggregate(count ~ year,data=sub2[which(sub2$Yasi>64),],FUN=sum)
      colnames(plotd2) <- c("Year","Total")
      plotd2$Year <- as.numeric(plotd2$Year)
      
      plotd2
    })
    
    output$table2 <- renderTable(digits=0,bordered=TRUE,striped=TRUE,{
      tab2()
    })
    
    #################################################################################
    
    myplot3 <- eventReactive(req(isTruthy(input$submit2)),{
      
      if(input$sehir2!='all cities'){
        
        data2 <- data2[which(data2$Sehir==input$sehir2),]
        
      }
      
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
      
      
      sub3 = data2[data2$OlumTar%in%dates,]
      
      sub3$day  <- substring(sub3$OlumTar,1,5)
      sub3$year <- substring(sub3$OlumTar,7,11)
      sub3$count <- 1
      
      plotd3 <- aggregate(count ~ year,data=sub3,FUN=sum)
      colnames(plotd3) <- c("Year","Total")
      plotd3$Year <- as.numeric(plotd3$Year)
      
      #t <- rasterGrob(png::readPNG('logo.png'),interpolate = TRUE)
      #t <- rasterGrob(png::readPNG('C:/Users/Dr Zopluoglu/Desktop/shiny-server/deceased_turkey/data/logo.png'),interpolate = TRUE)
      
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
                          xmin=max(plotd3[,1])-2, xmax=max(plotd3[,1]), 
                          ymin=10, ymax=10) +
        #annotation_custom(t, xmin = 2009.5, xmax = 2012, ymin =0, ymax =max(plotd3[,2]*.1))+
        theme(plot.title = element_text(lineheight=.8, face="bold"),
              plot.margin=margin(1,1,3,1))
      
      p
    })
    
    
    output$plot3 <- renderPlot({
      myplot3()
    },height = 450, width = 700)
    
    tab3 <- eventReactive(req(isTruthy(input$submit2)), {
      
      if(input$sehir2!='all cities'){
        
        data2 <- data2[which(data2$Sehir==input$sehir2),]
        
      }
      
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
      
      
      sub3 = data2[data2$OlumTar%in%dates,]
      
      sub3$day  <- substring(sub3$OlumTar,1,5)
      sub3$year <- substring(sub3$OlumTar,7,11)
      sub3$count <- 1
      
      plotd3 <- aggregate(count ~ year,data=sub3,FUN=sum)
      colnames(plotd3) <- c("Year","Total")
      plotd3$Year <- as.numeric(plotd3$Year)
      
      plotd3
    })
    
    output$table3 <- renderTable(digits=0,bordered=TRUE,striped=TRUE,{
      tab3()
    })
    
    ###################################################################################

    myplot4 <- eventReactive(req(isTruthy(input$submit2)),{
      
      if(input$sehir2!='all cities'){
        
        data2 <- data2[which(data2$Sehir==input$sehir2),]
        
      }
      
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
      
      
      sub4 = data2[data2$OlumTar%in%dates,]
      sub4$day  <- substring(sub4$OlumTar,1,5)
      sub4$year <- substring(sub4$OlumTar,7,11)
      sub4$count <- 1
      
      plotd4           <- aggregate(count ~ year,data=sub4[which(sub4$Yasi>64),],FUN=sum)
      colnames(plotd4) <- c("Year","Total")
      plotd4$Year      <- as.numeric(plotd4$Year)
      
      #t <- rasterGrob(png::readPNG('logo.png'),interpolate = TRUE)
      #t <- rasterGrob(png::readPNG('C:/Users/Dr Zopluoglu/Desktop/shiny-server/deceased_turkey/data/logo.png'),interpolate = TRUE)
      
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
                          xmin=max(plotd4[,1])-2, xmax=max(plotd4[,1]), 
                          ymin=10, ymax=10) +
       # annotation_custom(t, xmin = 2009.5, xmax = 2012, ymin =0, ymax =max(plotd4[,2]*.1))+
        theme(plot.title = element_text(lineheight=.8, face="bold"),
              plot.margin=margin(1,1,3,1))
      
      p
    })
    
    output$plot4 <- renderPlot({
      myplot4()
    },height = 450, width = 700)
    
    tab4 <- eventReactive(req(isTruthy(input$submit2)), {
      
      if(input$sehir2!='all cities'){
        
        data2 <- data2[which(data2$Sehir==input$sehir2),]
        
      }
      
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
      
      
      sub4 = data2[data2$OlumTar%in%dates,]
      sub4$day  <- substring(sub4$OlumTar,1,5)
      sub4$year <- substring(sub4$OlumTar,7,11)
      sub4$count <- 1
      
      plotd4 <- aggregate(count ~ year,data=sub4[which(sub4$Yasi>64),],FUN=sum)
      colnames(plotd4) <- c("Year","Total")
      plotd4$Year <- as.numeric(plotd4$Year)
      
      plotd4
    })
    
    output$table4 <- renderTable(digits=0,bordered=TRUE,striped=TRUE,{
      tab4()
    })
    
})
