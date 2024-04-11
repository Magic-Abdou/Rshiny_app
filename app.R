#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(gridExtra)

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Dataset Uploader"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for uploading files and creating new dataset
    sidebarPanel(
      fileInput("file", "Choose CSV File", accept = c(".csv")),
      actionButton("create_new_dataset", "Create New Dataset"),
      actionButton("draw_graphics", "Draw Graphics")
    ),
    
    # Main panel to display the uploaded dataset and the new dataset
    mainPanel(
      # Output: Data tables
      tabsetPanel(
        tabPanel("Uploaded Dataset", tableOutput("uploaded_data_table")),
        tabPanel("New Dataset", tableOutput("new_data_table")),
        tabPanel("Graphics", plotOutput("plot_output"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Initialize uploaded dataset and new dataset
  uploaded_data <- reactiveVal(NULL)
  new_data <- reactiveVal(NULL)
  
  # Function to read uploaded dataset
  observeEvent(input$file, {
    req(input$file)
    df <- read.csv(input$file$datapath)
    # Select only desired columns
    selected_cols <- c("dateTime", "c1", "c2", "temp", "hum")
    df <- df[, selected_cols, drop = FALSE]
    
    # Modify format of dateTime column
    df$dateTime <- gsub("Thu |GMT\\+0000 \\(Coordinated Universal Time\\)", "", df$dateTime)
    
    # Convert dateTime column to datetime object
    df$dateTime <- parse_date_time(df$dateTime, "%b %d %Y %H:%M:%S", locale = "en")
    
    uploaded_data(df)
  })
  
  # Generate new dataset when button is clicked
  observeEvent(input$create_new_dataset, {
    req(uploaded_data())
    initial_data <- uploaded_data()
    
    # Create new dataset with dateTime incremented by 1 second for each row
    new_data_df <- initial_data
    new_data_df$dateTime <- initial_data$dateTime + seconds(1:nrow(initial_data))
    new_data(new_data_df)
  })
  
  # Render the uploaded dataset as a table
  output$uploaded_data_table <- renderTable({
    uploaded_data()
  })
  
  # Render the new dataset as a table
  output$new_data_table <- renderTable({
    new_data()
  })
  
  # Render graphics when button is clicked
  observeEvent(input$draw_graphics, {
    req(new_data())
    data_5s <- new_data()
    
    # Draw first set of graphics
    p1 <- ggplot(data_5s, aes(x = dateTime, y = temp)) +
      geom_line(color = "blue") +
      labs(x = "Time", y = "Temperature", title = "Temperature over Time") +
      theme_minimal()
    
    p2 <- ggplot(data_5s, aes(x = dateTime, y = hum)) +
      geom_line(color = "red") +
      labs(x = "Time", y = "Humidity", title = "Humidity over Time") +
      theme_minimal()
    
    # Draw second set of graphics
    p3 <- ggplot(data_5s, aes(x = dateTime)) +
      geom_line(aes(y = c1, color = "e1")) +
      geom_line(aes(y = c2, color = "e2")) +
      labs(x = "Time", y = "Voltage", color = "Variables", title = "Plot of e1 and e2") +
      theme_minimal()
    
    # Arrange plots in a grid
    plot_output <- grid.arrange(p1, p2, p3, ncol = 2)
    
    output$plot_output <- renderPlot({
      plot_output
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
