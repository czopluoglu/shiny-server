#setwd("C:/Users/Dr Zopluoglu/Desktop/shiny-server/deceased_turkey/data")
#setwd("F:/shiny-server/deceased_turkey/data/")

# 0 5 * * * /usr/lib/R/bin/Rscript simpleRScript.R

setwd("/srv/shiny-server/deceased_turkey/data/")

####################################################
library(RSelenium)
library(rvest)
library(xml2)
require(rstudioapi)

system('docker run -d -p 4445:4444 selenium/standalone-chrome')

#termId <- rstudioapi::terminalExecute("docker run -d -p 4445:4444 selenium/standalone-chrome")
#rstudioapi::terminalKill(termId)

load('data.Rdata')

	save.image(paste0("data_backup_",substring(as.character(file.info('data.Rdata')$mtime),1,10),".Rdata"))

##############################################################################

 last.date = as.Date(substring(as.character(file.info('data.Rdata')$mtime),1,10),"%Y-%m-%d")

dates <- seq(from=last.date-45,to=Sys.Date()-1,by='days')
dates <- format(dates,format="%d/%m/%Y")

####################################################################################

remDr <- RSelenium::remoteDriver(remoteServerAddr = "localhost",
                                 port = 4445L,
                                 browserName = "chrome")
remDr$open()


website= 'https://www.turkiye.gov.tr'
cities = c('bursa','denizli','diyarbakir','istanbul','kahramanmaras',
           'kocaeli','konya','malatya','sakarya','tekirdag','erzurum')



city <- vector('list')

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
  
  city[[i]] = tab

} 


replace <- city[[1]]

for(i in 2:length(city)){
  
  replace <- rbind(replace,city[[i]])
  
}

####################################################################################


temp = data[data$OlumTar%in%dates,]

comparison <- data.frame(matrix(nrow=length(dates),ncol=3))
colnames(comparison) <- c('date','old','new')

for(i in 1:length(dates)){
 
  comparison[i,]$date = dates[i]
  comparison[i,]$old =  nrow(temp[temp$OlumTar%in%dates[i],])
  comparison[i,]$new =  nrow(replace[replace$OlumTar%in%dates[i],])

}

write.csv(comparison,
          paste0("comparison_",substring(as.character(file.info('data.Rdata')$mtime),1,10),".csv"),
          row.names = FALSE)

####################################################################################

data2 = data[!data$OlumTar%in%dates,]
data2 <- rbind(data2,replace)

  sub1 = data[which(data$Sehir=='istanbul'),]
  sub2 = data2[which(data2$Sehir=='istanbul'),]

	if(nrow(sub2)>nrow(sub1)) {

		rm(list=setdiff(ls(), "data2")) 
		data = data2
		rm('data2')
		save.image("data.Rdata")

	}

####################################################################################