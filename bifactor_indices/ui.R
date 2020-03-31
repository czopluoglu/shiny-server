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
                                 font-size: 12px;
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
    
    tags$head(
        tags$style(HTML('#create{background-color: #85c1e9 ;font-weight:bold;color:black}'))
    ),
    
    tags$head(
        tags$style(HTML('#calculate{background-color: #85c1e9 ;font-weight:bold;color:black}'))
    ),
    
    
    # tags$head(includeHTML(("/srv/shiny-server/deceased_turkey/google-analytics.html"))),
    
    titlePanel("Utility Indices for Bifactor Model"),

    fluidRow(
        column(2,
               
            br(),
            tags$h5("This Shiny app calculates the indices useful for bifactor models.
                    These indices are discussed in detail by the Rodriguez et al. (2006)."),
            br(),
            
            uiOutput("info3"),
            
            hr(),
            h5("For questions: Cengiz Zopluoglu"),
            uiOutput("info"),
            uiOutput("info2"),
            
            hr(),
            textOutput("step1"),
            textInput('nitem',"",value=9,width="25%"),
            
            hr(),
            textOutput("step2"),
            selectInput('nsub',"",selected=3,c(2:10),width="25%"),
            
            hr(),
            textOutput("step3"),
            br(),
            actionButton("create", "Create the input matrix",width="60%"),
            
            hr(),
            textOutput("step4"),
            
            hr(),
            textOutput("step5"),
            actionButton("calculate", "Calculate",width="60%"),
            
            ),
        
        column(6,
               mainPanel(
                matrixInput("matrix1", 
                               value=m,
                               paste=TRUE,
                               copy=TRUE,
                               class = "numeric",
                            rows = list(names=TRUE,extended=TRUE),
                            cols = list(names=TRUE,extended=TRUE))
               )
               
               ),
        
        column(4),
        
        
    )
))
