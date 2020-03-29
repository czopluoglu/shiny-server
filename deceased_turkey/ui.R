# Deceased Statistics - TURKEY
library(shiny)

library(shinyWidgets)

###############################################################3

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    
    tags$head(includeHTML(("/srv/shiny-server/deceased_turkey/google-analytics.html"))),
    
    titlePanel(""),

    fluidRow(
        column(2,
               
            br(),
            tags$h5("The numbers reported on this page include 
                    the following major cities in which the data are available on
                    www.turkey.gov.tr:"),
            h5("- Bursa"),
            h5("- Denizli"),
            h5("- Diyarbakir"),
            h5("- Erzurum"),
            h5("- Istanbul"),
            h5("- Kahramanmaras"),
            h5("- Kocaeli"),
            h5("- Konya"),
            h5("- Malatya"),
            h5("- Sakarya"),
            h5("- Tekirdag"),
            hr(),
            br(),
            uiOutput("info3"),
            
            h5("You can compare the number of deceased individuals for any given 
            day or date range in 2020 to the number of deceased individuals 
            in the past 10 years on the same day or same date range."),
            br(),
            
            h5("Due to the lag in reporting numbers from the original
               source, the numbers are automatically updated
               on a daily basis for the prior 45 days."),
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
                                    column(1),
                                    column(2,
                                           selectInput("month", "Month",
                                                       c("Jan","Feb","Mar","Apr","May","Jun",
                                                         "Jul","Aug","Sep","Oct","Nov","Dec"))
                                    ),
                                    column(2,
                                           selectInput("day", "Day",c(1:31))
                                    ),
                                    
                                    column(2,
                                           selectInput('year1','Starting Year',c(2010:2019))
                                           ),
                                    
                                    column(3,
                                           selectInput("sehir", "City",
                                                       c('all cities','bursa','denizli','diyarbakir','istanbul','kahramanmaras',
                                                           'kocaeli','konya','malatya','sakarya','tekirdag','erzurum'))
                                           )
                                    
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
                                    
                                    column(2,
                                           selectInput("month.beg", "Beginning Month",
                                                       c("Jan","Feb","Mar","Apr","May","Jun",
                                                         "Jul","Aug","Sep","Oct","Nov","Dec"))
                                           ),
                                    column(2,
                                           selectInput("day.beg","Beginning Day",c(1:31))
                                           ),
                                    column(2,
                                           selectInput("year2","Starting Year",c(2010:2019))
                                    ),
                                    column(2,
                                           selectInput("sehir2", "City",
                                                       c('all cities','bursa','denizli','diyarbakir','istanbul','kahramanmaras',
                                                         'kocaeli','konya','malatya','sakarya','tekirdag','erzurum'))
                                    ),
                                    column(2,
                                           selectInput("month.end", "Ending Month",
                                                       c("Jan","Feb","Mar","Apr","May","Jun",
                                                         "Jul","Aug","Sep","Oct","Nov","Dec"))
                                           ),
                                    column(2,
                                           selectInput("day.end","Ending Day",c(1:31))
                                    ),
                                
                                    
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
                                    column(4),
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
