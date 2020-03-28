#setwd("C:/Users/Dr Zopluoglu/Desktop/shiny-server/deceased_turkey/data")
#setwd("F:/shiny-server/deceased_turkey/data/")

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

##############################################################################

dates  <- as.Date(data$OlumTar,format="%d/%m/%Y")

dates <- seq(from=max(dates)-45,to=Sys.Date()-1,by='days')
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


data2 = data[!data$OlumTar%in%dates,]

data2 <- rbind(data2,replace)

rm(list=setdiff(ls(), "data2")) 

data = data2
rm('data2')


save.image("data.Rdata")

####################################################################################

#install.packages("git2r")
#library(git2r)

# Insure you have navigated to a directory with a git repo.
#dir <- "mypath"
#setwd(dir)

# Configure git.
#git2r::config(user.name = "myusername",user.email = "myemail")

# Check git status.
#gitstatus()

# Add and commit changes. 
#gitadd()
#gitcommit()

# Push changes to github.
#gitpush()
