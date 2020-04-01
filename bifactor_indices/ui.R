# Deceased Statistics - TURKEY

library(shiny)
library(shinyWidgets)
library(shinyMatrix)

m <- matrix(NA,9,4)

colnames(m) <- c("G","S1","S2","S3")
rownames(m) <- c("Item 1","Item 2","Item 3","Item 4","Item 5","Item 6","Item 7","Item 8","Item 9")


###############################################################3

# Define UI for application that draws a histogram

shinyUI(fluidPage(
    
    tags$head(tags$style("#step1{color: black;
                                 font-size: 14px;
                                 font-weight: bold;
                                 }"
    )
    ),
    
    tags$head(tags$style("#step2{color: black;
                                 font-size: 12px;
                                 font-weight: bold;
                                 }"
    )
    ),
    
    tags$head(tags$style("#step3{color: black;
                                 font-size: 12px;
                                 font-weight: bold;
                                 }"
    )
    ),
    
    tags$head(tags$style("#step4{color: black;
                                 font-size: 12px;
                                 font-weight: bold;
                                 }"
    )
    ),
    
    tags$head(tags$style("#step5{color: black;
                                 font-size: 12px;
                                 font-weight: bold;
                                 }"
    )
    ),
    
    tags$head(tags$style("#step{color: black;
                                 font-size: 18px;
                                 }"
    )
    ),
    
    tags$head(
        tags$style(HTML('#calculate{background-color: #85c1e9 ;font-weight:bold;color:black}'))
    ),
    
    
    # tags$head(includeHTML(("/srv/shiny-server/deceased_turkey/google-analytics.html"))),
    
    titlePanel("Utility Indices for Bifactor Model"),

            sidebarPanel(id="sidebar",
              
               br(),
                
                tags$h5("This Shiny app calculates the indices useful for bifactor models.
                        These indices are discussed in detail by the Rodriguez et al. (2016)."),
                br(),
                         
                uiOutput("info3"),
                         
                hr(),
                
                h5("For questions: Cengiz Zopluoglu"),
                
                uiOutput("info"),
                
                uiOutput("info2"),
                         
                hr(),
               
               textOutput("step1")
                         
            ),
          
            mainPanel(
                tabsetPanel(
                    
                    tabPanel("Input", align='center',
                             
                             br(),
                             
                             textOutput("step"),
                             
                             hr(),
                             
                             fluidRow(
                                 br(),
                                column(6, 
                                       
                                       fileInput("file1", "Choose CSV File",
                                                 multiple = FALSE,
                                                 accept = c("text/csv",
                                                            "text/comma-separated-values,text/plain",
                                                            ".csv"))
                                ),
                                
                                column(2,
                                       br(),
                                       actionButton('calculate',
                                                    "Calculate Indices")    
                                ),
                                
                                column(4)
                                
                                ),
                                
                             
                             hr(),
                             
                             fluidRow(
                                 tableOutput("table1")
                                 )
                             
                             ),
                    
                    tabPanel("Panel 2", align='center')
                    
                    )
            )
        
))
