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
                                 font-size: 16px;
                                 }"
    )
    ),
    
    tags$head(tags$style("#step2{color: black;
                                 font-size: 16px;
                                 }"
    )
    ),
    
    tags$head(tags$style("#step3{color: black;
                                 font-size: 16px;
                                 }"
    )
    ),
    
    tags$head(tags$style("#step4{color: black;
                                 font-size: 16px;
                                 }"
    )
    ),
    
    tags$head(tags$style("#step5{color: black;
                                 font-size: 16px;
                                 }"
    )
    ),
    
    tags$head(tags$style("#text1{color: black;
                                 font-size: 18px;
                                 }"
    )
    ),
    
    
    tags$head(tags$style("#text2{color: black;
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
                
                tags$h4("This Shiny app calculates some utility indices for bifactor models.
                        These indices are discussed in detail by the Rodriguez et al. (2016)."),
                br(),
                         
                uiOutput("info3"),
                         
                hr(),
                
                h5("For questions and reporting bugs: Cengiz Zopluoglu"),
                
                uiOutput("info"),
                
                uiOutput("info2"),
                         
                hr(),
               
                tags$h4("HOW TO USE?"),
               
               textOutput("step1"),
               br(),
               textOutput("step2"),
               br(),
               textOutput("step3"),
               br(),
               textOutput("step4"),
               br(),
               textOutput("step5"),
               
               
            ),
          
            mainPanel(
                tabsetPanel(
                    
                    tabPanel("Input", align='center',
                             
                             br(),
                             
                             textOutput("text1"),
                             
                             br(),
                             
                             textOutput("text2"),
                             
                             hr(),
                             
                             fluidRow(
                                 br(),
                                column(4, 
                                       
                                       fileInput("file1", "Choose CSV File",
                                                 multiple = FALSE,
                                                 accept = c("text/csv",
                                                            "text/comma-separated-values,text/plain",
                                                            ".csv"))
                                ),
                                
                                column(4,
                                       textInput('pos',"Enter starting item positons for specific factors",
                                                 width='100%', value="1,8,14")
                                       ),
                                
                                column(4)
                                
                                ),
                             
                             fluidRow(
                                 column(2),
                                 column(4,
                                        br(),
                                        actionButton('calculate',
                                                     "Calculate Indices")),
                                 column(2),
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
