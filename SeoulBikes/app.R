#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Seoul Bike Sharing"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons("seasons","Seasons",
                       choices = list("All", "Spring", "Summer", 
                                          "Autumn", "Winter")
                       #,choiceValues = list("all","english","spanish", "other")
                        ),
            sliderInput("rain",
                        "Range of Rainfall",
                        min = 0,
                        max = 50,
                        value = c(0,10)
                        ),
            actionButton("subset_data","Update Data!")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel("About", "content"),
            tabPanel("Data Download", 
                     DT::dataTableOutput("data_set")#,
                     #downloadButton('downloadData', 'Download')
                     ),
            tabPanel("Data Exploration", "content")
          )
           #plotOutput("distPlot")
        )
    )
)

# Define server logic 
server <- function(input, output, session) {
  data <-  reactiveValues(bike_subset = bike)
  
  observeEvent(input$subset_data,{
    
    if(input$seasons == "all"){
      seasons_sub <- c("Spring", "Summer", "Autumn", "Winter")
    } else if(input$seasons == "Spring"){
      seasons_sub <- "Spring"
    } else if(input$seasons == "Summer"){
      seasons_sub <- "Summer"
    } else if(input$seasons == "Autumn"){
      seasons_sub <- "Autumn"
    }
    else {
      seasons_sub <- "Winter"
    }
    
    
    data$bike_subset <- bike |>
          filter(
            Seasons %in% seasons_sub

          )
   #utput$data_set <- DT::renderDataTable(head(data$bike_subset))
  }
  )

  # output$data_download <- renderUI({
  #   bike_subset <- bike |>
  #     filter(#cat vars first
  #       Seasons %in% seasons_sub
  #       # FSfac %in% fs_sub,
  #       # SCHLfac %in% schl_sub
  #     )
     output$data_set <- DT::renderDataTable(head(data$bike_subset))
  #   
  # })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

# output$downloadData <- downloadHandler(
#   filename = function() {
#     paste('data-', Sys.Date(), '.csv', sep='')
#   },
#   content = function(con) {
#     write.csv(data, con)
#   }
# )