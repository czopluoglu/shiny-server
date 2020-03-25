# Deceased Statistics - TURKEY
library(shiny)

library(shinyWidgets)

###############################################################3

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    titlePanel(""),

    fluidRow(
        column(2,
               h4("Select Cities"),
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
        ),
        
        column(10,
               mainPanel(
                   tabsetPanel(
                       
                       tabPanel("By Date", align='center',
                                
                                fluidRow(
                                    h5("The data presented in this dashboard are compiled from www.turkiye.gov.tr 
                                    and updated daily. You can compare the number of deceased individuals in 2020
                                    for any given day to the number of deceased individuals in the past 10 years
                                       on the same day.")
                                ),
                                
                                fluidRow(
                                    h5("Feel free to reach out for questions and reporting any bugs."),
                                    uiOutput("info")
                                ),
                                
                                fluidRow(
                                    h5("Enter date in DD/MM/YYYY format. The data goes back to 01/01/2010")
                                ),
                                
                                fluidRow(
                                    column(4),
                                    column(4,
                                           textInput('date',label=NULL,value="22/04/2010")
                                    ),
                                    column(4,
                                           actionBttn("submit", 
                                                      label = "Submit",
                                                      icon=icon("play"), 
                                                      style="simple",
                                                      color="primary",
                                                      size = "sm"))
                                ),
                                
                                fluidRow(
                                    column(5,
                                           plotOutput('plot',width = "100%")
                                           ),
                                    
                                    column(2,'AAA'),
                                    
                                    column(5,
                                           plotOutput('plot2',width = "100%")
                                           )
                                ),
                                
                                fluidRow(
                                    column(5,'AAA'),
                                    column(2),
                                    column(5,'AAA')
                                )
                                
                                
                       ),
                       
                       tabPanel("By Month", align='center',
                                
                                sliderInput("n", 
                                            label = h4("AAA"), 
                                            min = 5, max = 15, value = 15)
                       )
                   )
               )
        )
    )
                       
                       
    
        
               
    
))
