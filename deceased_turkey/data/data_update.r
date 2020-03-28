
library(RSelenium)
library(rvest)
library(xml2)
require(rstudioapi)


termId <- rstudioapi::terminalExecute("docker run -d -p 4445:4444 selenium/standalone-chrome")
rstudioapi::terminalKill(termId)

load('data.Rdata')

#######################################

dates  <- as.Date(data$OlumTar,format="%d/%m/%Y")

date.range <- seq(from=max(dates)-45,to=max(dates)+1,by='days')



# docker ps 
# you should get a table of information if it is successful
####################################################################################



####################################################################################

remDr <- RSelenium::remoteDriver(remoteServerAddr = "localhost",
                                 port = 4445L,
                                 browserName = "chrome")
remDr$open()


website= 'https://www.turkiye.gov.tr'
cities = c('bursa','denizli','diyarbakir','istanbul','kahramanmaras',
           'kocaeli','konya','malatya','sakarya','tekirdag',
           'antalya','erzurum')

dates <- seq.Date(Sys.Date() - 3737,Sys.Date()-1, by='days')
dates <- dates[1:(length(dates)-1)]


for(i in 1:length(cities)){
  
  url  = paste0(website,"/",cities[i],'-buyuksehir-belediyesi-vefat-sorgulama')
  
  tables <- vector("list",length(dates))
  
  for(j in 1:length(dates)){
    
    tables[[j]]=list()
    rep = 0
    
    while(length(tables[[j]])==0 & rep<20){
      remDr$navigate(url)
      element<- remDr$findElement(using = 'css selector', "#tarih")
      button <- remDr$findElement(using = 'css selector', ".submitButton") 
      element$clearElement()
      
      element$sendKeysToElement(list(dates[j]))
      button$clickElement()
      
      #remDr$screenshot(display=TRUE)
      
      html <- xml2::read_html(remDr$getPageSource()[[1]])
      tables[[j]] <- html_table(html_nodes(html, "table"))
      
      rep = rep+1 
    }
    
    print(c(j,dates[j]))
    print(tables[[j]])
  }
  
  
  for(k in 1:length(tables)){
    
    if(length(tables[[k]])!=0){
      colnames(tables[[k]][[1]])[1:6] <- c("AdSoyad","BabaAdi","DogumTar","Yasi","Sebeb","Islem")
      tables[[k]][[1]]$OlumTar <- dates[k]
      tables[[k]][[1]]$Sehir   <- cities[i]
    }
  }
  
  tab <- tables[[1]][[1]]
  
  for(k in 2:length(tables)){
    
    if(length(tables[[k]])!=0){
      tab <- rbind(tab,tables[[k]][[1]])
    }
  }
  
  write.csv(tab,paste0(cities[i],".csv"),row.names=FALSE)
  
} 
