library(shiny)
library(sortable)
library(shinyWidgets)

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
               h3("Item Bank"),
               fluidRow(
                   column(12,
                       bucket_list(
                           header = "",
                           group_name = "bucket_list_group",
                           options = sortable_options(
                               sort=TRUE),
                           orientation = "horizontal",
                           add_rank_list(
                               text = "",
                               labels = list(
                                   "Item 1",
                                   "Item 2",
                                   "Item 3",
                                   "Item 4",
                                   "Item 5"
                               ),
                               input_id = "rank_list_1"
                           ),
                           add_rank_list(
                               text = "",
                               labels = list(
                                   "Item 6",
                                   "Item 7",
                                   "Item 8",
                                   "Item 9",
                                   "Item 10"
                               ),
                               input_id = "rank_list_2"
                           ),
                           add_rank_list(
                               text = "",
                               labels = list(
                                   "Item 11",
                                   "Item 12",
                                   "Item 13",
                                   "Item 14",
                                   "Item 15"
                               ),
                               input_id = "rank_list_3"
                           ),
                           add_rank_list(
                               text = "",
                               labels = list(
                                   "Item 16",
                                   "Item 17",
                                   "Item 18",
                                   "Item 19",
                                   "Item 20"
                               ),
                               input_id = "rank_list_4"
                           )
                       )
                   ),
               ),
               
               fluidRow(
                   column(6,
                          bucket_list(
                              header = "Drag the items to Form 1",
                              group_name = "bucket_list_group",
                              options = sortable_options(
                                  sort=TRUE),
                              orientation = "vertical",
                              add_rank_list(
                                  text = "",
                                  labels= NULL,
                                  input_id = "rank_list_5"
                              )
                          )
                          ),
                   
                   column(6,
                          bucket_list(
                              header = "Drag the items to Form 2",
                              group_name = "bucket_list_group",
                              options = sortable_options(
                                  sort=TRUE),
                              orientation = "vertical",
                              add_rank_list(
                                  text = "",
                                  labels= NULL,
                                  input_id = "rank_list_6"
                              )
                          )
                   )
                   ),
               
               fluidRow(
                   column(6,
                          tags$p("input$rank_list_5"),
                          verbatimTextOutput("results_5")
                          ),
                   column(6,
                          tags$p("input$rank_list_6"),
                          verbatimTextOutput("results_6")
                   ),
               )
               
             )
        )
    )


#######################################################################################

server <- function(input,output) {
    
    output$results_5 <-
        renderPrint(
            input$rank_list_5 # This matches the input_id of the first rank list
        )
    output$results_6 <-
        renderPrint(
            input$rank_list_6 # This matches the input_id of the second rank list
        )

}

#######################################################################################

shinyApp(ui, server)
