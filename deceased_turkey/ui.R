# Deceased Statistics - TURKEY
library(shiny)

library(shinyWidgets)

###############################################################3

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    titlePanel(""),

    fluidRow(
        column(2,
               
            br(),
            br(),
            tags$h3("Select Cities"),
               br(),
               checkboxInput('bursa', 'Bursa',value=TRUE),
               checkboxInput('denizli', 'Denizli',value=TRUE),
               checkboxInput('diyarbakir', 'Diyarbakir',value=TRUE),
               checkboxInput('istanbul', 'Istanbul',value=TRUE),
               checkboxInput('kahramanmaras', 'Kahramanmaras',value=TRUE),
               checkboxInput('kocaeli', 'Kocaeli',value=TRUE),
               checkboxInput('konya', 'Konya',value=TRUE),
               checkboxInput('malatya', 'Malatya',value=TRUE),
               checkboxInput('sakarya', 'Sakarya',value=TRUE),
               checkboxInput('tekirdag', 'Tekirdag',value=TRUE),
               
            hr(),
            
            uiOutput("info3"),
            
            h5("You can compare the number of deceased individuals for any given day
            in 2020 to the number of deceased individuals in the past 10 years
            on the same day."),
            br(),
            hr(),
            textOutput("finalupdate"),
            hr(),
            
            h5("Cengiz Zopluoglu"),
            uiOutput("info"),
            uiOutput("info2"),
            
               ),
        
        column(10,
               mainPanel(
                   tabsetPanel(
                       
                       tabPanel("Single Date", align='center',
                            
                                fluidRow(
                                    h5("Enter date in DD/MM/YYYY format"),
                                    column(4),
                                    column(4,
                                           textInput('date',label=NULL,value="22/03/2020")
                                    ),
                                    column(4)
                                    ),
                            
                                fluidRow(
                                    column(4),
                                    column(4,
                                       actionBttn("submit", 
                                                  label = "Submit",
                                                  icon=icon("play"), 
                                                  style="simple",
                                                  color="primary",
                                                  size = "sm")),
                                    column(4)
                                    ),
                                
                                fluidRow(
                                    column(10,
                                           mainPanel(
                                               plotOutput('plot',width = "100%",height="100%")
                                           )
                                           ),
                                    
                                    column(2,
                                           tableOutput("table1")
                                           ),
                                ),
                            
                                fluidRow(
                                    column(10,
                                           mainPanel(
                                               plotOutput('plot2',width = "100%",height="100%")
                                        )
                                    ),
                                
                                    column(2,
                                           tableOutput("table2")
                                    ),
                                )
                                
                       ),
                       
                       tabPanel("Date Range", align='center',
                                
                                fluidRow(
                                    h5("Enter date in DD/MM/YYYY format"),
                                    column(3),
                                    column(3,
                                           textInput('begin',label="Beginning Date",value="22/03/2020")
                                           ),
                                    column(2),
                                    column(3,
                                           textInput('end',label="End Date",value="22/03/2020")
                                    ),
                                    column(1)
                                    
                                ),
                                
                                fluidRow(
                                    column(3),
                                    column(3),
                                    column(2,
                                           actionBttn("submit2", 
                                                      label = "Submit",
                                                      icon=icon("play"), 
                                                      style="simple",
                                                      color="primary",
                                                      size = "sm")),
                                    column(3),
                                    column(1)
                                ),
                                
                                fluidRow(
                                    column(10,
                                           mainPanel(
                                               plotOutput('plot3',width = "100%",height="100%")
                                           )
                                    ),
                                    
                                    column(2,
                                           tableOutput("table3")
                                    ),
                                ),
                                
                                fluidRow(
                                    column(10,
                                           mainPanel(
                                               plotOutput('plot4',width = "100%",height="100%")
                                           )
                                    ),
                                    
                                    column(2,
                                           tableOutput("table4")
                                    ),
                                )
                       )
                   )
               )
        )
    )
                       
                       
    
        
               
    
))
