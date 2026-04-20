# microCustomer_test_R76.R
# Component test for microCustomer module - R76 Compliant Version
# Follows MP51 (Test Data Design), R74 (Shiny Test Data), R75 (Test Script Initialization), and R76 (Module Data Connection)

# Set to TRUE to skip tests and just launch the app
DEMO_MODE <- TRUE

# 1. Environment Setup
source_find_root <- function(start_path = getwd()) {
  current_path <- normalizePath(start_path, winslash = "/")
  while (!file.exists(file.path(current_path, "app.R"))) {
    parent_path <- dirname(current_path)
    if (parent_path == current_path) {
      stop("Cannot find project root directory containing app.R")
    }
    current_path <- parent_path
  }
  return(current_path)
}

if (!exists("script_triggered_from_app_mode") || !script_triggered_from_app_mode) {
  root_path <- source_find_root()
  setwd(root_path)
  
  # 2. Mode Initialization
  source(file.path("update_scripts", "global_scripts", "00_principles", 
                   "sc_initialization_app_mode.R"))
}

# 3. Required Libraries
library(shiny)
library(bs4Dash)
library(dplyr)
library(testthat)

# 4. Source the R76-compliant module
source("update_scripts/global_scripts/10_rshinyapp_components/micro/microCustomer/microCustomer.R")

# 5. Test Data
customer_test_data <- list(
  # Complete customer data
  df_customer_profile = data.frame(
    customer_id = 1:5,
    buyer_name = c("Zhang San", "Li Si", "Wang Wu", "Zhao Liu", "Chen Qi"),
    email = c("zhang@example.com", "li@example.com", "wang@example.com", 
              "zhao@example.com", "chen@example.com"),
    stringsAsFactors = FALSE
  ),
  
  # Partial DNA data (only for customers 1-3)
  df_dna_by_customer = data.frame(
    customer_id = 1:3,
    time_first = as.Date(c("2022-01-15", "2021-03-20", "2020-11-05")),
    time_first_to_now = c(450, 780, 920),
    r_label = c("極近", "一般", "非常久遠"),
    r_value = c(5, 45, 120),
    f_label = c("非常高", "一般", "低"),
    f_value = c(12, 4, 8),
    m_label = c("非常高", "一般", "低"),
    m_value = c(15000, 3500, 7800),
    cai_label = c("非常活躍", "一般活躍", "不活躍"),
    cai = c(0.85, 0.35, 0.15),
    ipt_mean = c(38.5, 97.2, 65.8),
    pcv = c(180000, 15000, 62400),
    clv = c(250000, 25000, 89000),
    cri = c(0.92, 0.45, 0.72),
    nrec = c(0.75, 0.35, 0.55),
    nes_status = c("主力型", "成長型", "沉睡型"),
    nt = c(1500, 900, 1200),
    e0t = c(2500, 1500, 1800),
    stringsAsFactors = FALSE
  ),
  
  # Test scenarios
  scenarios = list(
    complete = list(
      df_customer_profile = data.frame(
        customer_id = 1:5,
        buyer_name = c("Zhang San", "Li Si", "Wang Wu", "Zhao Liu", "Chen Qi"),
        email = c("zhang@example.com", "li@example.com", "wang@example.com", 
                  "zhao@example.com", "chen@example.com"),
        stringsAsFactors = FALSE
      ),
      df_dna_by_customer = data.frame(
        customer_id = 1:3,
        time_first = as.Date(c("2022-01-15", "2021-03-20", "2020-11-05")),
        time_first_to_now = c(450, 780, 920),
        r_label = c("極近", "一般", "非常久遠"),
        r_value = c(5, 45, 120),
        f_label = c("非常高", "一般", "低"),
        f_value = c(12, 4, 8),
        m_label = c("非常高", "一般", "低"),
        m_value = c(15000, 3500, 7800),
        cai_label = c("非常活躍", "一般活躍", "不活躍"),
        cai = c(0.85, 0.35, 0.15),
        ipt_mean = c(38.5, 97.2, 65.8),
        pcv = c(180000, 15000, 62400),
        clv = c(250000, 25000, 89000),
        cri = c(0.92, 0.45, 0.72),
        nrec = c(0.75, 0.35, 0.55),
        nes_status = c("主力型", "成長型", "沉睡型"),
        nt = c(1500, 900, 1200),
        e0t = c(2500, 1500, 1800),
        stringsAsFactors = FALSE
      )
    ),
    
    incomplete = list(
      df_customer_profile = data.frame(
        customer_id = 1:5,
        buyer_name = c("Zhang San", "Li Si", "Wang Wu", "Zhao Liu", "Chen Qi"),
        email = c("zhang@example.com", "li@example.com", "wang@example.com", 
                  "zhao@example.com", "chen@example.com"),
        stringsAsFactors = FALSE
      ),
      df_dna_by_customer = data.frame(
        customer_id = 1:2,
        time_first = as.Date(c("2022-01-15", "2021-03-20")),
        time_first_to_now = c(450, 780),
        r_label = c("極近", "一般"),
        r_value = c(5, 45),
        f_label = c("非常高", "一般"),
        f_value = c(12, 4),
        m_label = c("非常高", "一般"),
        m_value = c(15000, 3500),
        cai_label = c("非常活躍", "一般活躍"),
        cai = c(0.85, 0.35),
        ipt_mean = c(38.5, 97.2),
        pcv = c(180000, 15000),
        clv = c(250000, 25000),
        cri = c(0.92, 0.45),
        nrec = c(0.75, 0.35),
        nes_status = c("主力型", "成長型"),
        nt = c(1500, 900),
        e0t = c(2500, 1500),
        stringsAsFactors = FALSE
      )
    ),
    
    empty = list(
      df_customer_profile = data.frame(
        customer_id = integer(0),
        buyer_name = character(0),
        email = character(0),
        stringsAsFactors = FALSE
      ),
      df_dna_by_customer = data.frame(
        customer_id = integer(0),
        time_first = as.Date(character(0)),
        time_first_to_now = numeric(0),
        r_label = character(0),
        r_value = numeric(0),
        f_label = character(0),
        f_value = numeric(0),
        m_label = character(0),
        m_value = numeric(0),
        cai_label = character(0),
        cai = numeric(0),
        ipt_mean = numeric(0),
        pcv = numeric(0),
        clv = numeric(0),
        cri = numeric(0),
        nrec = numeric(0),
        nes_status = character(0),
        nt = numeric(0),
        e0t = numeric(0),
        stringsAsFactors = FALSE
      )
    )
  ),
  
  # Metadata about this test data
  metadata = list(
    description = "Standard customer test data for microCustomer module",
    customer_count = 5,
    dna_coverage_pct = 60,
    includes_edge_cases = TRUE,
    key_relationship = "df_customer_profile (key = customer_id) is complete, df_dna_by_customer (key = customer_id) is partial"
  )
)

# 6. Create R76-compatible data connections
create_test_data_connection <- function(scenario_data) {
  # Create a data connection object that simulates a database connection with 
  # query functions to retrieve data
  connection <- list(
    # DBI-style query function to get DNA data
    query_dna_data = function() {
      return(scenario_data$df_dna_by_customer)
    },
    
    # DBI-style query function to get customer profiles
    query_customer_profiles = function() {
      return(scenario_data$df_customer_profile)
    },
    
    # Data access functions following R76 style
    get_dna_data = function() {
      return(scenario_data$df_dna_by_customer)
    },
    
    get_customer_profiles = function() {
      return(scenario_data$df_customer_profile)
    },
    
    # Include direct references as fallback
    df_dna_by_customer = scenario_data$df_dna_by_customer,
    df_customer_profile = scenario_data$df_customer_profile,
    
    # Add connection metadata
    connection_type = "mock_dbi_connection",
    is_test_connection = TRUE,
    current_scenario = names(which(scenario_data == test_data$scenarios))
  )
  
  # Add class attribute to make it behave more like a real connection
  class(connection) <- c("mock_dbi_connection", "list")
  
  cat("Created test data connection with scenario:", 
      names(which(scenario_data == test_data$scenarios)), "\n")
  
  return(connection)
}

# 7. Test Functions

#' Validate data structure
#' @param data Data to validate
#' @param required_fields Required fields to check for
#' @param data_name Name to use in messages
#' @return TRUE if valid, FALSE otherwise
validate_data <- function(data, required_fields = NULL, data_name = "Data") {
  if (is.null(data)) {
    message(data_name, " is NULL")
    return(FALSE)
  }
  
  if (!is.data.frame(data)) {
    message(data_name, " is not a data frame")
    return(FALSE)
  }
  
  if (nrow(data) == 0) {
    message(data_name, " has 0 rows")
    return(FALSE)
  }
  
  if (!is.null(required_fields) && !all(required_fields %in% colnames(data))) {
    missing_fields <- setdiff(required_fields, colnames(data))
    message(data_name, " is missing required fields: ", 
            paste(missing_fields, collapse = ", "))
    return(FALSE)
  }
  
  return(TRUE)
}

#' Create a test app
#' @param test_data Test data to use
#' @return Shiny app
create_test_app <- function(test_data) {
  ui <- fluidPage(
    title = "microCustomer Module Test (R76 Compliant)",
    
    # CSS styles
    tags$head(
      tags$style(HTML("
        .test-control-panel {
          background-color: #f8f9fa;
          padding: 15px;
          margin-bottom: 20px;
          border-radius: 5px;
          border: 1px solid #dee2e6;
        }
        .test-header {
          margin-bottom: 20px;
          padding-bottom: 10px;
          border-bottom: 1px solid #dee2e6;
        }
        .test-section {
          border: 1px solid #dee2e6;
          border-radius: 5px;
          padding: 15px;
          margin-top: 20px;
        }
      "))
    ),
    
    # Test header
    div(class = "test-header",
        h1("microCustomer Module Test (R76 Compliant)"),
        p("This test application validates the R76 compliant server architecture")
    ),
    
    # Test control panel
    div(class = "test-control-panel",
        fluidRow(
          column(12,
                 h3("Test Controls"),
                 selectInput("test_scenario", "Test Scenario:", 
                           choices = c(
                             "Complete Data" = "complete",
                             "Incomplete Data" = "incomplete",
                             "Empty Data" = "empty"
                           ),
                           selected = "complete"
                 ),
                 hr(),
                 checkboxInput("show_filter", "Show Filter Interface", value = TRUE),
                 checkboxInput("show_debug", "Show Debug Information", value = TRUE)
          )
        )
    ),
    
    # Test results area
    fluidRow(
      column(12,
             h3("Module Test Results"),
             conditionalPanel(
               condition = "input.show_filter == true",
               div(class = "test-section",
                   h4("Filter Component"),
                   microCustomerFilterUI("test_module")
               )
             ),
             div(class = "test-section",
                 h4("Customer Profile Component"),
                 microCustomerUI("test_module")
             )
      )
    ),
    
    # Debug information
    conditionalPanel(
      condition = "input.show_debug == true",
      fluidRow(
        column(12,
               div(class = "test-section",
                   h3("Debug Information"),
                   verbatimTextOutput("debug_info")
               )
        )
      )
    )
  )
  
  server <- function(input, output, session) {
    # Create a reactive data connection based on the selected scenario
    data_connection <- reactive({
      scenario <- input$test_scenario
      
      # Get the data for the selected scenario
      if (scenario %in% names(test_data$scenarios)) {
        scenario_data <- test_data$scenarios[[scenario]]
      } else {
        # Default to complete data if scenario not found
        scenario_data <- test_data$scenarios$complete
      }
      
      # Create and return a data connection object
      create_test_data_connection(scenario_data)
    })
    
    # Initialize the module with the data connection
    # R76 Compliance: Pass the data connection, not the filtered data
    
    # Monitor scenario changes
    observe({
      scenario <- input$test_scenario
      cat("DEBUG: Scenario changed to:", scenario, "\n")
    })
    
    # Create a proxy for actual database connection that reactively updates when scenario changes
    db_connection_proxy <- reactive({
      # Log when connection is being evaluated
      scenario <- input$test_scenario
      cat("DEBUG: Creating connection for scenario:", scenario, "\n")
      
      # Create a connection object that simulates a DBI connection but uses our test data
      conn <- data_connection()
      
      # Validate connection
      if (is.null(conn)) {
        cat("ERROR: Failed to create connection\n")
        return(NULL)
      }
      
      # Log successful connection
      cat("DEBUG: Connection created successfully for scenario:", scenario, "\n")
      return(conn)
    })
    
    # Initialize the microCustomer module with the DB connection proxy
    filtered_data <- microCustomerServer(
      "test_module", 
      db_connection_proxy
    )
    
    # Debug output
    output$debug_info <- renderPrint({
      # Get current data connection
      connection <- db_connection_proxy()
      
      # Safely check data
      tryCatch({
        # Extract data directly from the connection for debugging
        # Try both function-based and direct access methods
        if (is.function(connection$get_dna_data)) {
          dna_data <- connection$get_dna_data()
          cat("Retrieved DNA data using get_dna_data() function\n")
        } else if ("df_dna_by_customer" %in% names(connection)) {
          dna_data <- connection$df_dna_by_customer
          cat("Retrieved DNA data using direct access\n")
        } else {
          dna_data <- NULL
          cat("No DNA data available\n")
        }
        
        if (is.function(connection$get_customer_profiles)) {
          profiles <- connection$get_customer_profiles()
          cat("Retrieved profiles using get_customer_profiles() function\n")
        } else if ("df_customer_profile" %in% names(connection)) {
          profiles <- connection$df_customer_profile
          cat("Retrieved profiles using direct access\n")
        } else {
          profiles <- NULL
          cat("No profile data available\n")
        }
        
        cat("\nConnection Information:\n")
        cat("Connection type:", paste(class(connection), collapse=", "), "\n")
        cat("Connection fields:", paste(names(connection), collapse=", "), "\n")
        cat("Current scenario:", input$test_scenario, "\n\n")
        
        list(
          test_scenario = input$test_scenario,
          connection_info = list(
            type = class(connection),
            fields = names(connection),
            is_test = if("is_test_connection" %in% names(connection)) connection$is_test_connection else FALSE
          ),
          profile_data = list(
            available = !is.null(profiles),
            valid = if(!is.null(profiles)) validate_data(profiles) else FALSE,
            rows = if(!is.null(profiles) && validate_data(profiles)) nrow(profiles) else 0,
            columns = if(!is.null(profiles) && validate_data(profiles)) ncol(profiles) else 0
          ),
          dna_data = list(
            available = !is.null(dna_data),
            valid = if(!is.null(dna_data)) validate_data(dna_data) else FALSE,
            rows = if(!is.null(dna_data) && validate_data(dna_data)) nrow(dna_data) else 0,
            columns = if(!is.null(dna_data) && validate_data(dna_data)) ncol(dna_data) else 0
          ),
          filtered_data = list(
            valid = validate_data(filtered_data()),
            rows = if(validate_data(filtered_data())) nrow(filtered_data()) else 0
          ),
          module_status = "Using R76 Compliant Mock DBI Connection"
        )
      }, error = function(e) {
        cat("Error inspecting data:", e$message, "\n")
        list(
          error = e$message,
          test_scenario = input$test_scenario
        )
      })
    })
  }
  
  # Launch the app with specific options to ensure it opens in the browser
  # Don't set launch.browser in both options() and shinyApp() to avoid recursion
  shinyApp(ui, server, options = list(
    port = 8765,            # Use a specific port
    host = "127.0.0.1"      # Listen on localhost only
  ))
}

# 8. Test Execution

#' Run validation tests for microCustomer module
run_tests <- function(interactive_mode = FALSE) {
  # Check if in demo mode
  if (DEMO_MODE) {
    message("Running in DEMO_MODE - launching app without tests")
    create_test_app(customer_test_data)
    return()
  }
  
  # Test data connection functionality
  test_that("Data connection works correctly", {
    connection <- create_test_data_connection(customer_test_data$scenarios$complete)
    
    # Test function-based connection
    expect_true(is.function(connection$get_dna_data))
    expect_true(is.function(connection$get_customer_profiles))
    
    # Test data retrieval
    dna_data <- connection$get_dna_data()
    profiles <- connection$get_customer_profiles()
    
    expect_true(is.data.frame(dna_data))
    expect_true(is.data.frame(profiles))
    expect_true(nrow(dna_data) > 0)
    expect_true(nrow(profiles) > 0)
  })
  
  # Test data structure
  test_that("Test data structure follows R74", {
    expect_true("df_customer_profile" %in% names(customer_test_data))
    expect_true("df_dna_by_customer" %in% names(customer_test_data))
    expect_true("scenarios" %in% names(customer_test_data))
    expect_true("metadata" %in% names(customer_test_data))
  })
  
  # Test data relationships
  test_that("Data relationships are maintained", {
    # Check that customers without DNA data exist
    customer_ids <- customer_test_data$df_customer_profile$customer_id
    dna_customer_ids <- customer_test_data$df_dna_by_customer$customer_id
    
    # There should be more customers than DNA records
    expect_true(length(customer_ids) >= length(dna_customer_ids))
    
    # All DNA customer IDs should be in the profiles
    expect_true(all(dna_customer_ids %in% customer_ids))
  })
  
  # Run the test app in interactive mode
  if (interactive() && !exists("skip_interactive") && !isTRUE(getOption("test.mode"))) {
    message("Starting interactive test app...")
    create_test_app(customer_test_data)
  } else {
    message("Test validation complete. Run in interactive mode to see the test app.")
  }
}

# Execute in DEMO_MODE - This will directly show the app without requiring interactive mode
if (DEMO_MODE) {
  message("DEMO_MODE is enabled - launching the app directly")
  
  # Set only the global launch.browser option to avoid recursion
  options(shiny.launch.browser = TRUE)
  
  # Launch the test app
  create_test_app(customer_test_data)
} else {
  # Run normal tests when not in DEMO_MODE
  run_tests()
}
