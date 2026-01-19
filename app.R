#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
# https://shiny.posit.co/
#
#Author -Abdul Rahman

library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)

# reading from the dataset
data_source =read.csv("Heart_Disease_Prediction.csv")



# UI (User Interface)
ui <- dashboardPage(
  dashboardHeader(title = "Rahman-Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dataset", tabName = "dashboard", icon = icon("table")),
      menuItem("Data Visualization(BP)", tabName = "analytics", icon = icon("chart-bar")),
      menuItem("Data Visualization(Cholesterol)", tabName = "analytics2", icon = icon("chart-bar"))
      
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              h2("This dataset contains clinical and demographic features used to predict whether a patient has heart disease."),
              
              #displaying the data in a table
              DTOutput("table")
              
      ),
      # Second tab content
      tabItem(tabName = "analytics",
              h2("Data Visualization Section for BP"),
              sidebarLayout(
                sidebarPanel (
                  selectInput("condition", "Select the Heart Disease Status:",
                              choices =  unique(data_source$Heart.Disease),
                              selected = unique(data_source$Heart.Disease)[1] )
                ),
                mainPanel()
              ),
              
              #Analytic explanation 
              p("Please each Histogram displays the clinical and demographic features used to predict whether a patient has heart disease.."),
              plotOutput("my_plot")
              
      ),
      # Third tab content
      tabItem(tabName = "analytics2",
              h2("Data Visualization Section for Cholesterol"),
              sidebarLayout(
                sidebarPanel (
                  selectInput("condition", "Select the Heart Disease Status:",
                              choices =  unique(data_source$Heart.Disease),
                              selected = unique(data_source$Heart.Disease)[1] )
                ),
                mainPanel()
              ),
              
              #Analytic explanation 
              p("Please each Histogram displays the clinical and demographic features used to predict whether a patient has heart disease.."),
              plotOutput("my_plot2")
              
      )
    )
  )
)

# Server (Backend Logic)
server <- function(input, output) {
  #table rendering
  output$table <- renderDT(data_source, options = list(scrollX = TRUE))
  
  
  
  #cleaning data for selected Year
  filtered_data <- reactive({
    subset(data_source, Heart.Disease == input$condition)
    
  })
  
  
  #visualization rendering in a histogram for BP
  output$my_plot <- renderPlot({
    ggplot(filtered_data(), aes(x=Age , y = BP )) +
      #scale_x_continuous(limits = c(20,100))+
      scale_y_continuous(limits = c(0,200))+
      geom_col(fill ="skyblue")+
      #graph labels
      labs (
        title =paste("Data Analysis for", input$condition, "Heart Attack Prediction using BP"),
        x = "Age of Clients",
        y = "BP Of Clients"
      )+
      theme_minimal()
  })
  
  #visualization rendering in a histogram for Cholesterol
  output$my_plot2 <- renderPlot({
    ggplot(filtered_data(), aes(x=Age , y = Cholesterol )) +
      #scale_x_continuous(limits = c(20,100))+
      scale_y_continuous(limits = c(0,600))+
      geom_col(fill ="skyblue")+
      #graph labels
      labs (
        title =paste("Data Analysis for", input$condition, "Heart Attack Prediction using Cholesterol"),
        x = "Age of Clients",
        y = "Cholesterol level Of Clients"
      )+
      theme_minimal()
  })
}

# Run the app
shinyApp(ui, server)

