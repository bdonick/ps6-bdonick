library(tidyverse)
library(plotly)
library(readr)

car_data <- read_delim("data/car_data.csv.zip")

ui <- fluidPage(
  
  mainPanel(
    HTML("<h2>Welcome to the Car Data App</h2>
        <p>This app allows you to explore information about numerous types of cars.</p>
        <p>Use the tabs above to navigate through the app.</p>
        <br>
        <p><strong>Data Summary:</strong></p>
        <p>Number of cars available to look at: <strong>", nrow(car_data), "</strong></p>
        <p>Range of years of the cars: <strong>", min(car_data$Year), "</strong> - <strong>", max(car_data$Year), "</strong></p>"
    ),
    HTML("<strong>The data is used from cars.com</strong>")
  ),
  

  tabsetPanel(
    
    tabPanel("General Information",
             textOutput("dataset_info")
    ),
    
   
    tabPanel("Plot",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("mileage", "Mileage on car", 
                             min = 0, 
                             max = 115000, 
                             value = 20000),
                 radioButtons("color", "Choose color",
                              choices = c("skyblue", "lawngreen", "red", "purple", "gold"))
               ),
               mainPanel(
                 plotOutput("plot"),
              
               textOutput("plot_text"),
               ),
             )
    ),
    
    
    tabPanel("Table",
             sidebarLayout(
               sidebarPanel(
                 textOutput("table_text"),
                 selectInput("model",
                             "Select a model:",
                             choices = unique(car_data$Model))
               ),
               mainPanel(
                 dataTableOutput("table")
               )
             )
    )
  )
)

server <- function(input, output) {
  
  output$dataset_info <- renderText({
    "This data set is composed of various makes of cars and important information about them, such as mileage, price, and year."
  })
  
  
  output$plot <- renderPlot({
    mileage_data <- car_data %>%
      filter(Mileage <= input$mileage) %>%
      mutate(Mileage = as.numeric(str_replace_all(Mileage, "[^[:digit:]]", "")),
             Price = as.numeric(str_replace_all(Price, "[^[:digit:]]", ""))) %>%
      na.omit() %>% 
      ggplot(aes(Year, Price)) +
      geom_col(col=input$color) +
      ggtitle(paste("The status of cars given the maximum mileage entered, along with the year of the car"))
    mileage_data
  })
  
  
  output$plot_text <- renderText({
    mileage_data <- car_data %>%
      filter(Mileage <= input$mileage)
    n_total <- nrow(mileage_data)
    n_missing <- sum(is.na(mileage_data$Year) | is.na(mileage_data$Status))
    n_non_missing <- n_total - n_missing
    paste("The number of cars that have at most ", input$mileage, " miles: ", n_non_missing)
  })
  
  
  output$table <- renderDataTable({
    car_data %>%
      filter(Model == input$model)
  })
  
  output$table_text <- renderText({
    selected_data <- car_data %>% filter(Model == input$model)
    n_obs <- nrow(selected_data)
    paste0("Selected model has ", n_obs, " cars avaliable")
  })
  
}

shinyApp(ui = ui, server = server)
      