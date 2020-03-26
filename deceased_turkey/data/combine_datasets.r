

a1 = read.csv("bursa.csv",header=TRUE)
a2 = read.csv("denizli.csv",header=TRUE)
a3 = read.csv("diyarbakir.csv",header=TRUE)
a4 = read.csv("istanbul.csv",header=TRUE)
a5 = read.csv("kahramanmaras.csv",header=TRUE)
a6 = read.csv("kocaeli.csv",header=TRUE)
a7 = read.csv("konya.csv",header=TRUE)
a8 = read.csv("malatya.csv",header=TRUE)
a9 = read.csv("malatya.csv",header=TRUE)
a10 = read.csv("malatya.csv",header=TRUE)

data <- rbind(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)

  names(table(data$Yasi))

  
  data[which(data$Yasi=="-"),]$Yasi=NA                       
  data[which(data$Yasi=="-1"),]$Yasi=NA
  data[which(data$Yasi=="-2"),]$Yasi=NA
  
  data$Yasi <- as.numeric(as.character(data$Yasi))
  data[which(data$Yasi>145),]$Yasi=NA
  
  
  data$OlumTar <- as.character(data$OlumTar)
  
  data$OlumTar <- as.Date(data$OlumTar,format="%Y-%m-%d")
  data$OlumTar <- format(data$OlumTar,format="%d/%m/%Y")
  
  
  data$OlumTar <- as.character(data$OlumTar)
  
  
  data$Sehir <- as.character(data$Sehir)
  
  
  rm("a1","a2","a3","a4","a5","a6","a7","a8","a9","a10")
  
  save.image("C:/Users/Dr Zopluoglu/Desktop/shiny-server/deceased_turkey/data/data.Rdata")
  

  