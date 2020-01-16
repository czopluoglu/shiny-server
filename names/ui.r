library(shiny)

fluidPage(
  
  titlePanel(h1("National Data On The Relative Frequency Of 
             Given Names In the Population Of U.S. Births",align="center")),
  
  sidebarPanel(
    
    h3("The information is based on the database at this",
      a("Website.", 
        href = "http://www.ssa.gov/OACT/babynames/limits.html")),
    
    
    selectInput("state","Select a State",
                choices = list("US (ALL STATES)"="US","AK"="AK","AL"="AL","AR"="AR","AZ"="AZ",
                               "CA"="CA","CO"="CO","CT"="CT","DC"="DC","DE"="DE",
                               "FL"="FL","GA"="GA","HI"="HI","IA"="IA","ID"="ID",
                               "IL"="IL","IN"="IN","KS"="KS","KY"="KY","LA"="LA",
                               "MA"="MA","MD"="MD","ME"="ME","MI"="MI","MN"="MN",
                               "MO"="MO","MS"="MS","MT"="MT","NC"="NC","ND"="ND",
                               "NE"="NE","NH"="NH","NJ"="NJ","NM"="NM","NV"="NV",
                               "NY"="NY","OH"="OH","OK"="OK","OR"="OR","PA"="PA",
                               "RI"="RI","SC"="SC","SD"="SD","TN"="TN","TX"="TX",
                               "UT"="UT","VA"="VA","VT"="VT","WA"="WA","WI"="WI",
                               "WV"="WV","WY"="WY"), selected = "US"),
    
    textInput("name1", label = "Name 1","Enter a name... Capitalize only the first letter"),
    selectInput("color1","Name 1 Color",
                choices = list("Black" = "black", "Red" = "red","Green" = "green",
                               "Blue"="blue","Cyan"="cyan","Magenta"="magenta",
                               "Yellow"="yellow","Gray"="gray"), 
                selected = "black"),
    
    textInput("name2", label = "Name 2","Enter a name... Capitalize only the first letter"),
    selectInput("color2","Name 2 Color",
                choices = list("Black" = "black", "Red" = "red","Green" = "green",
                               "Blue"="blue","Cyan"="cyan","Magenta"="magenta",
                               "Yellow"="yellow","Gray"="gray"), 
                selected = "black"),
    
    textInput("name3", label = "Name 3","Enter a name... Capitalize only the first letter"),
    selectInput("color3","Name 3 Color",
                choices = list("Black" = "black", "Red" = "red","Green" = "green",
                               "Blue"="blue","Cyan"="cyan","Magenta"="magenta",
                               "Yellow"="yellow","Gray"="gray"), 
                selected = "black"),
   
    sliderInput("lwd", label = "Line Width",min = 1, max = 4, value = 1.5),
    sliderInput("ylim", label = "Change Y-axis Maximum",min = 0, max = 0.2, value = .2),
    downloadButton("down","Download the plot")
  ),
  
  mainPanel(
    plotOutput("plot")
  )
)
