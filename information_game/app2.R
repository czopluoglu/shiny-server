library(shiny)
library(sortable)
library(shinyWidgets)
library(shinyDND)

########################################################################


ui <- fluidPage(
  
  tags$head(tags$style("#nstep{color: black;
                                 font-size: 14px;
                                 font-weight: bold;
                                 }"
  )
  ),
  
  tags$head(tags$style("#maxlogL{color: black;
                                 font-size: 20px;
                                 }"
  )
  ),
  
  tags$head(tags$style("#nr{color: white;
                                 font-size: 1px;
                                 }"
  )
  ),
  
  
  
  setBackgroundColor(
    color = c("#ffffff", "#ffffff"),
    gradient = "linear",
    direction = "bottom"
  ),
  
  titlePanel(""),
  
  fluidRow(
    column(2,
           
           fluidRow(
             column(12,
                    actionBttn("reset", 
                               label = "RESET EVERTYTHING",
                               style="simple",
                               color="primary",
                               size = "md")
             )
           ),
           
           br(),
           
           fluidRow(
             column(12,
                    selectInput("model", label = h4("Select the Model"), 
                                choices = list("1PL" = 1, 
                                               "2PL" = 2, 
                                               "3PL" = 3,
                                               "4PL" = 4), 
                                selected = 2),
             )
           ),
           
           fluidRow(
             column(12,
                    mainPanel(tableOutput("ipar"))
             )
           ),
           
    ),
    
    column(10,
           fluidRow(
             column(4,
                    
                    column(3,
                           dragUI("div1","Item 1"),
                           br(),
                           dragUI("div2","Item 2"),
                           br(),
                           dragUI("div3","Item 3"),
                           br(),
                           dragUI("div4","Item 4"),
                           br(),
                           dragUI("div5","Item 5")
                    ),
                    
                    column(3,
                           dragUI("div6","Item 6"),
                           br(),
                           dragUI("div7","Item 7"),
                           br(),
                           dragUI("div8","Item 8"),
                           br(),
                           dragUI("div9","Item 9"),
                           br(),
                           dragUI("div10","Item 10")
                           ),
                    column(3,
                           dragUI("div11","Item 11"),
                           br(),
                           dragUI("div12","Item 12"),
                           br(),
                           dragUI("div13","Item 13"),
                           br(),
                           dragUI("div14","Item 14"),
                           br(),
                           dragUI("div15","Item 15")
                    ),
                    column(3,
                           dragUI("div16","Item 16"),
                           br(),
                           dragUI("div17","Item 17"),
                           br(),
                           dragUI("div18","Item 18"),
                           br(),
                           dragUI("div19","Item 19"),
                           br(),
                           dragUI("div20","Item 20")
                    )
                    ),
             
             column(4,
                    dropUI("div6",row_n=2,col_n=5)
                    ),
             
             column(4,
                    dropUI("div6",row_n=2,col_n=5)
                    )
             
           ),
           
           fluidRow(
             plotOutput('plot',width = "100%")
           )
           
    )
  )
)


#######################################################################################

server <- function(input,output) {
  
  output$plot <- renderPlot({
    
    plot(1:10,(1:10)^2)
    
  })
  
  output$results_1 <-
    renderPrint(
      input$rank_list_1 # This matches the input_id of the first rank list
    )
  output$results_2 <-
    renderPrint(
      input$rank_list_2 # This matches the input_id of the second rank list
    )
  output$results_3 <-
    renderPrint(
      input$bucket_list_group # Matches the group_name of the bucket list
    )
}

#######################################################################################

shinyApp(ui, server)
