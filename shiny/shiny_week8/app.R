#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Library calls
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

# Dataset 
shiny_data <- read_csv("shiny_data.csv")

# Define UI for application
ui <- fluidPage(
  
  # Define application title because it helps users identify the app's purpose.
  titlePanel("Scatterplot App"),
  
  # Side bar creation
  sidebarLayout(
    
    sidebarPanel(
      # Gender drop down menu 
      selectInput("gender", "Select Gender:", choices = c("All", "Male", "Female"), selected = "All"),
      
      # Error band drop down 
      selectInput("errorBand", "Error Band:", choices = c("Display Error Band", "Suppress Error Band"), selected = "Display Error Band"),
      
      # Date Exclusion 
      selectInput("dates", "Participants before July 1, 2017:", choices = c("Include", "Exclude"), selected = "Include")
    ),
    
    mainPanel(
      # Scatterplot 
      plotOutput("scatterPlot")
    )
  )
)
  
     

# Server logic for scatter plot 
server <- function(input, output) {
  
  output$scatterPlot <- renderPlot({
    
    # Local copy of data 
    shiny_dat <- shiny_data
    
    # gender selection logic 
    if (input$gender != "All") {
      shiny_dat <- filter(shiny_dat, gender == input$gender)
    }
    
    # Exclusion based on date logic
    if (input$dates == "Exclude") {
      # Exact date range
     shiny_dat <- filter(shiny_dat, timeStart >= as.Date("2017-07-01"))
    }
    
    # Error band logic 
    show_band <- input$errorBand == "Display Error Band"
    
    # Plot from markdown file 
    ggplot(shiny_dat, aes(x = m_q1_q6, y = m_q8_q10)) +
      geom_jitter(width = 0.2, height = 0.2) +
      geom_smooth(method = "lm", color = "purple", se = show_band) +
      labs(title = "Relationship Between Mean Scores of Q1-Q6 and Q8-Q10",
           x = "Means of Questions 1-6",
           y = "Means of Questions 8-10")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
