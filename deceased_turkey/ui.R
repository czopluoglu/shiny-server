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
            tags$h4("The numbers reported on this page are based on 
                    the following major cities the data is available on
                    www.turkey.gov.tr:"),
            h5("- Bursa"),
            h5("- Denizli"),
            h5("- Diyarbakir"),
            h5("- Istanbul"),
            h5("- Kahramanmaras"),
            h5("- Kocaeli"),
            h5("- Konya"),
            h5("- Malatya"),
            h5("- Sakarya"),
            h5("- Tekirdag"),
            br(),
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
                            
                                br(),
                                
                                fluidRow(
                                    column(2),
                                    column(4,
                                           selectInput("month", "Month",
                                                       c("Jan","Feb","Mar","Apr","May","Jun",
                                                         "Jul","Aug","Sep","Oct","Nov","Dec"))
                                    ),
                                    column(4,
                                           selectInput("day", "Day",c(1:31))
                                    ),
                                    column(2)
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
                                
                                hr(),
                                
                                fluidRow(
                                    column(10,
                                           mainPanel(
                                               plotOutput('plot',width = "100%",height="100%")
                                           ),
                                     
                                           ),
                                    
                                    column(2,
                                           tableOutput("table1")
                                           )
                                ),
                                
                                hr(),
                                
                                fluidRow(
                                    column(10,
                                           mainPanel(
                                               plotOutput('plot2',width = "100%",height="100%")
                                        )
                                    ),
                                
                                    column(2,
                                           tableOutput("table2")
                                    ),
                                ),
                                
                                
                       ),
                       
                       tabPanel("Date Range", align='center',
                                
                                fluidRow(
                                    
                                    column(1),
                                    column(2,
                                           selectInput("month.beg", "Beginning Month",
                                                       c("Jan","Feb","Mar","Apr","May","Jun",
                                                         "Jul","Aug","Sep","Oct","Nov","Dec"))
                                           ),
                                    column(2,
                                           selectInput("day.beg","Beginning Day",c(1:31))
                                           ),
                                    column(2),
                                    column(2,
                                           selectInput("month.end", "Ending Month",
                                                       c("Jan","Feb","Mar","Apr","May","Jun",
                                                         "Jul","Aug","Sep","Oct","Nov","Dec"))
                                           ),
                                    column(2,
                                           selectInput("day.end","Ending Day",c(1:31))
                                    ),
                                    
                                    column(1)
                                    
                                ),
                                
                                fluidRow(
                                    column(1),
                                    column(2),
                                    column(2),
                                    column(2,
                                           actionBttn("submit2", 
                                                      label = "Submit",
                                                      icon=icon("play"), 
                                                      style="simple",
                                                      color="primary",
                                                      size = "sm")),
                                    column(2),
                                    column(2),
                                    column(1)
                                ),
                                
                                hr(),
                                
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
                                
                                hr(),
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
