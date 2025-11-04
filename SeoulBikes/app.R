#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

#bike <- read.csv("SeoulBikeData.csv")

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
            radioButtons("holiday","Holidays",
                         choices = list("All", "Holiday", "No Holiday" 
                        )
            ),
            
            selectInput('num1', 'Select First Numeric variable', 
                        c("bike_count", "hour", "temp", "humidity",
                          "wind_speed", "rainfall", "snowfall", "visibility",
                          "dew_point_temp", "solar_radiation"), 
                        selectize=FALSE),
            selectInput('num2', 'Select Second Numeric variable', 
                        c("bike_count", "hour", "temp", "humidity",
                          "wind_speed", "rainfall", "snowfall", "visibility",
                          "dew_point_temp", "solar_radiation"), 
                        selectize=FALSE),
      
            uiOutput("slider1"),
            uiOutput("slider2"),

            actionButton("subset_data","Update Data!")
        
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel("About", 
                     tags$p("This app allows users to explore a data set about rental bike in Seoul
                           South Korea. The use can create sub sets of the data, download said data
                           and learn meaningful insights through graphical numerical summaries."),
                     
                     tags$p(
                       "You can check out the data's Kaggle link ",
                       tags$a("here", href = "https://www.kaggle.com/datasets/saurabhshahane/seoul
                              -bike-sharing-demand-prediction?resource=download")
                     ), 
                     "The data was originally put together for V E, Sathishkumar (2020), Seoul
                     Bike Sharing Demand Prediction paper. The counts of rental bikes per hour comes
                     from the South Korea website named SEOUL OPEN DATA PLAZA. This data set spans
                     from December 2017 to November 2018 (365 days). The variables to describe
                     climate conditions comes from Korea Meteorological Administration",
                     
                     "The sidebar allows the user to subset two categroical variables, Season and
                     Holiday, and to subset any of the numeric variables but only two at a time. The
                     data will not be subseted until the 'Update Data!' button is clicked. In the
                     Data Download tab, the user can see a snipet of data they have subetted and
                     then click the action button at the bottom of the page to download the data as
                     a CSV. Finally, the data exploration tab allows the user to see graphical and
                     numerical summaries of their subsetted data. This will include contingency
                     tables, bar charts, scatter plots, and summary statistics."
                     fluidRow(
                     column(6,imageOutput("bikeimage")),
                     column(6,imageOutput("seoulimage"))
                     )
            ),
            tabPanel("Data Download", 
                     DT::dataTableOutput("data_set"),
                     downloadButton('downloadData', 'Download the data!')
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
  
  #add images to About tab
  output$bikeimage <- renderImage( 
    { 
      list(src = "bike-stock.jpg", height = "50%") 
    }, 
    deleteFile = FALSE 
  )
  
  output$seoulimage <- renderImage( 
    { 
      list(src = "south-korea.jpg", height = "50%") 
    }, 
    deleteFile = FALSE 
  )
  
  
  #Update second numeric variable selection
  observeEvent(input$num1, {
    num1 <- input$num1
    num2 <- input$num2
    choices <- c("bike_count", "hour", "temp", "humidity",
                 "wind_speed", "rainfall", "snowfall", "visibility",
                 "dew_point_temp", "solar_radiation")
    if (num1 != num2){
      choices <- choices[-which(choices == num1)]
      updateSelectizeInput(session,
                           "num2",
                           choices = choices,
                           selected = num2)
    }
  })
  
  #Update first numeric variable selection
  observeEvent(input$num2, {
    num1 <- input$num1
    num2 <- input$num2
    choices <- c("bike_count", "hour", "temp", "humidity",
                 "wind_speed", "rainfall", "snowfall", "visibility",
                 "dew_point_temp", "solar_radiation")
    if (num2 != num1){
      choices <- choices[-which(choices == num2)]
      updateSelectizeInput(session,
                           "num1",
                           choices = choices,
                           selected = num1)
    }
  })
  
  
  
  output$slider1 <- renderUI({
    # validate(
    #   need(input$num1 != "", "Please select a variable.")
    # )
    selected_column1 <- bike[[input$num1]]
    
    sliderInput("num_range1",
                "Range of First Numeric Variable",
                min = min(selected_column1, na.rm = TRUE),
                max = max(selected_column1, na.rm = TRUE),
                value = c(min(selected_column1, na.rm = TRUE),
                          max(selected_column1, na.rm = TRUE))
    )
  })
  output$slider2 <- renderUI({
    validate(
      need(input$num2 != "", "Please select a variable.")
    )
    selected_column2 <- bike[[input$num2]]
    
    sliderInput("num_range2",
                "Range of Second Numeric Variable",
                min = min(selected_column2, na.rm = TRUE),
                max = max(selected_column2, na.rm = TRUE),
                value = c(min(selected_column2, na.rm = TRUE),
                          max(selected_column2, na.rm = TRUE))
    )
  })
  
  observeEvent(input$subset_data,{
    
    if(input$seasons == "All"){
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
    
    
    if(input$holiday == "All"){
      holiday_sub <- c("Holiday", "No Holiday")
    } else if(input$holiday == "Holiday"){
      holiday_sub <- "Holiday"
    } else if(input$holiday == "No Holiday"){
      holiday_sub <- "No Holiday"
    }
    
    
    data$bike_subset <- bike |>
          filter(
            Seasons %in% seasons_sub,
            Holiday %in% holiday_sub,
            !!sym(input$num1) >= input$num_range1[1] & 
              !!sym(input$num1) <= input$num_range1[2],
            !!sym(input$num2) >= input$num_range2[1] &
              !!sym(input$num2) <= input$num_range2[2]

          )
  }
  )


     output$data_set <- DT::renderDataTable(data$bike_subset)
      
     output$downloadData <- downloadHandler(
       filename = function() {
         # create file name
         paste("SeoulBikeData", ".csv", sep = "")
       },
       content = function(file) {
         # write csv
         write.csv(data$bike_subset, file, row.names = FALSE)
       }
     )
    
}

# Run the application 
shinyApp(ui = ui, server = server)


