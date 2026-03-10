# Basic Titanic Analysis - Interactive Shiny App
# Author: Jignesh Trivedi
# Date: 2026-03-05
# Description: A simple Shiny app that allows users to explore Titanic survival
# data by filtering passengers based on class and gender.

#  Header Section (Loading a required libraries)  ----

#  Checking if the user has the required packages installed, if not; it installs the required packages

if(!require("shiny"))install.packages("shiny")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("plotly")) install.packages("plotly")

library(shiny)      # helps to make an interactive app for users
library(dplyr)      # For data manipulation
library(ggplot2)    # For data visualization
library(plotly)     # For plotting interactive graphs

data("Titanic")     # Loading the Titanic data set

titanic <- base::as.data.frame(Titanic)     # converts it to a usable format for R

# -----------------------------
# Creating the User Interface  
# -----------------------------


# The main panel displays the filtered table and a survival chart

ui <- shiny::fluidPage(                              # Starts a responsive web layout that automatically scales to the user's screen.
  shiny::titlePanel("Titanic Survival Explorer"),
  shiny::sidebarLayout(                              # Divides the page into a narrow sidebar for controls and a wider area for results.
    shiny::sidebarPanel(                             # Organizes the page into a narrow column for inputs and a wider column for outputs.
      shiny::selectInput(                            # Creates a clickable dropdown menu that allows users to pick a single option from a list
        
        "Class",
        "Select Passenger Class:",
        choices = unique(titanic$Class),
        selected = "1st"
      ),
      
      shiny::selectInput(                            
        
        "Sex",
        "Select Gender:",
        choices = unique(titanic$Sex),
        selected = "Male"
      ),
      shiny::selectInput(
        
        "Age",
        "Select Age Group:",
        choices = unique(titanic$Age),
        selected = "Adult"
      )
    ),
    
    shiny::mainPanel(                               # Defines the larger area of the page used to display outputs like charts and tables.
      
      shiny::h3("Survival Rate"),
      shiny::textOutput("survival_rate"),
      shiny::tableOutput("table"),
      plotly::plotlyOutput("plot"),
      shiny::downloadButton("download_data", "Download Filtered Data")
    )
  )
)


#  Creating the server logic  ----

server <- function(input, output)  {
  
  filtered_data <- shiny::reactive({          # Automatically updates when user selections change
    
    dplyr::filter(                            # Filter the dataset using the selected class and gender  
      titanic,
      Class == input$Class,
      Sex == input$Sex,
      Age == input$Age
    )
  })
  
  output$survival_rate <- shiny::renderText({     # Instructs the server to create and update a reactive string of text for the UI.
    
    data <- filtered_data()
    
    if(nrow(data) == 0) {
      return("No data available!")
    }
    else{
      survived <- data%>%
      dplyr::filter(Survived == "Yes") %>%
      dplyr::summarise(total = sum(Freq))
      total <- sum(data$Freq)
      rate <- round((survived$total/total)*100, 2)
      paste0(rate, "%")
    }
  })
  
  output$table <- shiny::renderTable({        # Displays filtered data
    
    filtered_data()
  })
  
  output$plot <- plotly::renderPlotly({         # Instructs the server to generate and update an interactive graphic for the UI.
    
    data <- filtered_data()
    
    if(nrow(data) == 0){
      plot.new()
      title("No data available for the Selected filters!")
    }
    else {
      p <- ggplot2::ggplot(
        filtered_data(),
        ggplot2::aes(x = Survived, y = Freq, fill = Survived)
      ) + 
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::labs(
          title = paste("Survival Distribution for",
                        input$Sex,
                        input$Age,
                        "Passengers in",
                        input$Class,
                        "Class"
          ),
          x = "Survival status",
          y = "Number of Passengers"  
        )
      
      plotly::ggplotly(p)              # Converts a static ggplot2 object into an interactive, web-based visualization.
      }
  })
  
  #Download Handler
  output$download_data <- shiny::downloadHandler(          # Sets up the server-side logic to generate and deliver a downloadable file to the user.
    filename = function() {
      base::paste0("titanic_filtered_data_", base::Sys.Date(), ".csv")
    },
    
    content = function(file) {
      data <- filtered_data()
      
      utils::write.csv(data, file, row.names = FALSE)
    }
  )
}

# -----------------------------
# Running the Shiny Application
# -----------------------------

shiny::shinyApp(ui = ui, server = server)

