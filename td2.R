#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# ----------------------------------------------------------------------------

# Load packages
# ----------------------------------------------------------------------------
library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)

# Load data 
# ----------------------------------------------------------------------------

hospital_stay_raw <- read.csv2("data/hospital_stay.csv")
hospital_raw <- read.csv2("data/hospital.csv")
location_raw <- read.csv2("data/location.csv")
patient_raw <- read.csv2("data/patient.csv")

str(hospital_stay_raw) 
patient_location = patient_raw %>%
  merge(location_raw, by = "location_id")

hospital_stay = hospital_stay_raw %>%
  merge(hospital_raw, by = "hospital_id") %>%
  merge(patient_location, by = "patient_id" ) %>%
  mutate(admission_date = as.character(ymd(admission_date)), 
         discharge_date = as.character(ymd(discharge_date)), 
         birth_date = as.character(ymd(birth_date)))

# User Interface
# ----------------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("TD 1 - Printing records according to a filter"),
  
  # Box with choices input for locations 
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "select_location",
                         "Patients from", choices = c(location_raw$location)
                         )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tableOutput("table_number_stay"), 
      plotOutput("plot_number_stay")
    )
  )
)

# Server
# ----------------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$table_number_stay <- renderTable({
    hospital_stay %>%
      filter(location %in% input$select_location) %>% 
      group_by(hospital) %>%
      summarise(number_stays = n())
      
       
    })
  output$plot_number_stay <- renderPlot({
    hospital_stay %>%
      filter(location %in% input$select_location) %>% 
      group_by(hospital) %>%
      summarise(number_stays = n()) %>%
      ggplot() +
      geom_bar(aes(x = hospital, y = number_stays),
               stat = "identity", 
               color = "darkblue", 
               fill = "Lightblue") +
      labs(x = "Hopital",
           y = "Nombre de séjours", 
           title = "Nombre de séjours par hopital") +
      theme_bw() 
  })
}


# Web application
# ----------------------------------------------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)