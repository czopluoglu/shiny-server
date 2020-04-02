
library(shiny)

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
    
    tags$head(tags$style("#text3{color: black;
                                 font-size: 16px;
                                 }"
    )
    ),
    
    
    tags$head(tags$style("#text4{color: black;
                                 font-size: 16px;
                                 }"
    )
    ),
    
    tags$head(tags$style("#text5{color: black;
                                 font-size: 16px;
                                 }"
    )
    ),
    
    tags$head(tags$style("#info4{color: black;
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
                             uiOutput("info4"),
                             br(),
                             
                             textOutput("text2"),
                             
                             hr(),
                             
                             fluidRow(
                                 br(),
                                column (1),
                                column(3, 
                                       
                                       fileInput("file1", "Choose CSV File",
                                                 multiple = FALSE,
                                                 accept = c("text/csv",
                                                            "text/comma-separated-values,text/plain",
                                                            ".csv"))
                                ),
                                column(1),
                                column(4,
                                       textInput('pos',"Enter starting item positons for specific factors",
                                                 width='100%', value="1,13,22,31")
                                       ),
                                
                                column(3)
                                
                                ),
                             
                             fluidRow(
                                 column(2),
                                 column(4,
                                        br(),
                                        actionButton('calculate',
                                                     "Calculate Indices")),
                                 column(6),
                             ),
                             
                             hr(),
                             
                             fluidRow(
                                 column(8,
                                        tableOutput("table1")
                                 ),
                                 column(4)
                                 
                                 )
                             
                             ),
                    
                    tabPanel("Omega Hierarchical", align='center',
                             
                             br(),
                             tableOutput("omegaH"),
                             hr(),
                             textOutput("text3"),
                             hr(),
                             uiOutput("cit1")
                             ),
                    
                    tabPanel("Factor Determinacy", align='center',
                             
                             br(),
                             tableOutput("FD"),
                             hr(),
                             textOutput("text4"),
                             hr(),
                             uiOutput("cit2"),
                             br(),
                             uiOutput("cit3")
                             
                    ),
                    
                    tabPanel("Construct Reliability", align='center',
                             
                             br(),
                             tableOutput("H"),
                             hr(),
                             textOutput("text5"),
                             hr(),
                             uiOutput("cit4"),
                             
                    ),
                    
                    tabPanel("Explained Common Variance", align='center',
                             
                             br(),
                             tableOutput("ECV"),
                             hr(),
                             textOutput("text6"),
                             br(),
                             textOutput("text7"),
                             hr(),
                             uiOutput("cit5"),
                             br(),
                             uiOutput("cit6"),
                             hr(),
                             tableOutput("IECV")
                             
                             
                    )
                    
                    ),
                
            ),
    

        
))
