# microDNADistribution_production_test.R
# A simple test file for microDNADistribution

# Required libraries
library(shiny)
library(bs4Dash)
library(plotly)
library(dplyr)
library(DT)
library(shinyjs)

# Source db utilities for connection
db_connect_path <- file.path("update_scripts", "global_scripts", "02_db_utils", "fn_dbConnect_from_list.R")
if(file.exists(db_connect_path)) {
  source(db_connect_path)
}

# Source the component file
component_path <- file.path("update_scripts", "global_scripts", "10_rshinyapp_components", 
                          "micro", "microDNADistribution", "microDNADistribution.R")
source(component_path)

# Define function to connect to app database
connect_to_app_database <- function() {
  # Check if dbConnect_from_list is available
  if (!exists("dbConnect_from_list", mode = "function")) {
    stop("dbConnect_from_list function not available")
  }
  
  # Connect to app_data database with simplified Dropbox sync handling
  tryCatch({
    # Set DROPBOX_SYNC to FALSE to simplify connection 
    if (!exists("DROPBOX_SYNC", envir = .GlobalEnv)) {
      assign("DROPBOX_SYNC", FALSE, envir = .GlobalEnv)
    }
    
    # Connect to app_data database
    app_data_conn <- dbConnect_from_list("app_data", read_only = TRUE, verbose = TRUE)
    message("Connected to app_data database successfully")
    return(app_data_conn)
  }, error = function(e) {
    message("Error connecting to app_data database: ", e$message)
    return(NULL)
  })
}

# Simple translation function
translate <- function(text) {
  return(text)
}

# Define UI
ui <- bs4Dash::bs4DashPage(
  title = "DNA Distribution Analysis",
  bs4Dash::bs4DashNavbar(title = "DNA Distribution"),
  bs4Dash::bs4DashSidebar(
    bs4Dash::bs4SidebarMenu(
      bs4Dash::bs4SidebarMenuItem(
        text = "DNA Distribution",
        tabName = "dna_distribution"
      )
    )
  ),
  bs4Dash::bs4DashBody(
    bs4Dash::bs4TabItems(
      bs4Dash::bs4TabItem(
        tabName = "dna_distribution",
        microDNADistribution("dna_distribution")$ui()
      )
    )
  )
)

# Define server function
server <- function(input, output, session) {
  # Connect to database
  app_connection <- connect_to_app_database()
  
  # Create config
  config <- list(platform_id = "6")
  
  # Initialize component
  component <- microDNADistribution("dna_distribution")
  result <- component$server("dna_distribution", app_data_connection = app_connection, config = config)
  
  # Clean up on session end
  session$onSessionEnded(function() {
    if (!is.null(app_connection)) {
      if (exists("dbDisconnect_from_list")) {
        dbDisconnect_from_list("app_data", con = app_connection)
      } else {
        DBI::dbDisconnect(app_connection)
      }
    }
  })
}

# Run the app
shiny::shinyApp(ui, server)