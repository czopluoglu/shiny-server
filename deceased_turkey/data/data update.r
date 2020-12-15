#setwd("F:/shiny-server/deceased_turkey/data/")

# 0 5 * * * /usr/lib/R/bin/Rscript simpleRScript.R

#setwd("/srv/shiny-server/deceased_turkey/data/")

#remDr$screenshot(display=TRUE)

####################################################

require(googleAuthR)
require(RoogleVision)
require(jsonlite)
require(RSelenium)
require(rvest)
require(xml2)
require(rstudioapi)
require(magick)

#######################################################

# Google API authentication

gar_auth_configure(path="/google.json")
gar_auth(email = "cen.zop@gmail.com", 
         scopes = "https://www.googleapis.com/auth/cloud-vision")

######################################################################

# Start docker container

system('docker run -d -p 4445:4444 selenium/standalone-chrome')

######################################################################

# Load the old existing data

load('data.Rdata')

# Back up the old existing data

save.image("data_backup.Rdata")

##############################################################################

# Create a date vector starting 31 days before the last back up 
# until today's date

last.date = as.Date(substring(as.character(file.info('data.Rdata')$mtime),1,10),"%Y-%m-%d")

dates <- seq(from=last.date-31,to=Sys.Date()-1,by='days')
dates <- format(dates,format="%d/%m/%Y")

####################################################################################

# Start remote chrome browser

remDr <- RSelenium::remoteDriver(remoteServerAddr = "localhost",
                                 port = 4445L,
                                 browserName = "chrome")
remDr$open()


# Objects to create the url for each city

  website= 'https://www.turkiye.gov.tr'
  cities = c('bursa','denizli','diyarbakir','istanbul','kahramanmaras',
           'kocaeli','konya','malatya','sakarya','tekirdag','erzurum')

  # A list object to save each city data
  
  city <- vector('list')

# Loop over each city url
  
for(i in 1:length(cities)){
  
  # City url
  
  url  = paste0(website,"/",cities[i],'-buyuksehir-belediyesi-vefat-sorgulama')
  
  # A list object to store the data for each date
  
  tables <- vector("list",length(dates))
  
  # Loop over the number of dates
  
  for(j in 1:length(dates)){
    
    tables[[j]]=list()
    rep = 0
    
    # External while loop, keeps running until Google Vision
    # gets the correct 5-character captcha label
    
    while(length(tables[[j]])==0 & rep<3){
    
      correct = FALSE
      
      # Internal while loop, keeps running until Google Vision
      # gets something with 5 character
    
      while(correct==FALSE){
      
        yes = 'FALSE'
    
        while(yes=='FALSE'){
      
          # As long as Google Vision doesn't return something
          # with 5-character, this refreshes the page for
          # a new captcha and tries again
          
          remDr$navigate(url)
        
          remDr$screenshot(file='captcha.png')
      
          temp = image_read('captcha.png')
          temp = image_crop(temp, "780x629+270+584")
          temp = image_crop(temp, "114x40+0+0")
          image_write(temp, 
                      path = "temp.png", 
                      format = "png")
        
          label = getGoogleVisionResponse('temp.png',feature = 'TEXT_DETECTION')$description[2]
        
          if(is.null(label)==FALSE){
              yes=ifelse(nchar(label)==5,TRUE,FALSE)
            }
          }
        
        # When it finally gets a 5-character label
        # it tries and checks the html page.
        # If html page returns an error message about
        # the captcha being wrong, it goes back and starts 
        # again.
      
        element<- remDr$findElement(using = 'css selector', "#captcha_name")
        element$sendKeysToElement(list(label))
      
        element<- remDr$findElement(using = 'css selector', "#tarih")
        element$clearElement()
      
        element$sendKeysToElement(list(dates[j]))
      
        button <- remDr$findElement(using = 'css selector', ".submitButton") 
        button$clickElement()
      
        html <- xml2::read_html(remDr$getPageSource()[[1]]) 
        correct = ifelse(length(html_nodes(html, ".fieldError"))==0,TRUE,FALSE)
      }
      
      # If html page does not return an error message,
      # external while loop ends,below code gets the table 
      # with statistics, store it in the table object and 
      # and move to the next date
      
        html <- xml2::read_html(remDr$getPageSource()[[1]])
        tables[[j]] <- html_table(html_nodes(html, "table"))
      
        rep = rep+1 
        
        print(c(j,dates[j]))
        print(tables[[j]])
      
        #remDr$screenshot(display=TRUE)
    }
  }
    
    # Double check the number of counts for each day within a city
  
     count <- c()
     for(uu in 1:length(dates)){
       count[uu]=ifelse(length(tables[[uu]])!=0,nrow(tables[[uu]][[1]]),0)
     }
     
     which(count==0)
  
  
  # For each date, it appends the city name and date of death
     # to the scraped tables
  
  for(k in 1:length(tables)){
    
    if(length(tables[[k]])!=0){
      colnames(tables[[k]][[1]])[1:6] <- c("AdSoyad","BabaAdi","DogumTar","Yasi","Sebeb","Islem")
      tables[[k]][[1]]$OlumTar <- dates[k]
      tables[[k]][[1]]$Sehir   <- cities[i]
    }
  }
  
  
  count=1
  
  while(length(tables[[count]])==0){
    count = count+1
  }
  
  # Combine all the dates with updates and make one single file for
  # each city, then store it in the city object
  
  tab <- tables[[count]][[1]]
  
  for(k in (count+1):length(tables)){
    
    if(length(tables[[k]])!=0){
      tab <- rbind(tab,tables[[k]][[1]])
    }
  }
  
  city[[i]] = tab
  
} 

  
# Combine the updates information for all cities
  # this will replace the information in the original
  # dataset

replace <- city[[1]]

for(i in 2:length(city)){
  
  replace <- rbind(replace,city[[i]])
  
}

####################################################################################


# For the dates included in the update,
# this writes a control file so we can check
# the numbers before the update and after the update
# if needed


city.check = c()

temp = data[data$OlumTar%in%dates,]

for(j in 1:length(cities)) {
  
  comparison <- data.frame(matrix(nrow=length(dates),ncol=3))
  colnames(comparison) <- c('date','old','new')
  
  for(i in 1:length(dates)){
    
    comparison[i,]$date = dates[i]
    comparison[i,]$old =  nrow(temp[temp$OlumTar%in%dates[i] & temp$Sehir==cities[j],])
    comparison[i,]$new =  nrow(replace[replace$OlumTar%in%dates[i] & replace$Sehir==cities[j],])
    
  }
  
  write.csv(comparison,
            paste0("comparison_",as.character(Sys.Date()),"_",cities[j],".csv"),
            row.names = FALSE)
  
  city.check[j] = ifelse(sum(comparison[,3])>sum(comparison[,2]),1,0)
  
}

####################################################################################

# Get rid of the old data for the dates involved in the update
# and replace it with the new data

data.new = data

for(j in 1:length(cities)) {
  if(city.check[j]==1){
    data2 = data.new[!data.new$OlumTar%in%dates & data.new$Sehir==cities[j],]
    replace2 = replace[which(replace$Sehir==cities[j]),]
    data2 <- rbind(data2,replace2)
    
    data.temp = data.new[!data.new$Sehir==cities[j],]
    data.new <- rbind(data.temp,data2)
  }
}


rm(list=setdiff(ls(), "data.new")) 
data = data.new
rm('data.new')
save.image("data.Rdata")

####################################################################################