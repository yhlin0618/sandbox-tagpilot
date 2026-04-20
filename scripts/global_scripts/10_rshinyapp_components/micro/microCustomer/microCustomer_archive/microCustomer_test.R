# microCustomer_test.R
# Component test for microCustomer module
# Follows MP51 (Test Data Design), R74 (Shiny Test Data), and R75 (Test Script Initialization)

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

# 4. Module Under Test
source(file.path("update_scripts", "global_scripts", "10_rshinyapp_components", 
                 "micro", "microCustomer", "microCustomer.R"))

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
      df_customer_profile = NULL,
      df_dna_by_customer = NULL
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

# 6. Test Functions

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
    title = "microCustomer Module Test",
    
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
        h1("microCustomer Module Test"),
        p("This test application validates the functionality and usability of the customer analysis module")
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
    # Reactive data sources based on selected scenario
    current_data <- reactive({
      scenario <- input$test_scenario
      
      # Return appropriate data based on scenario
      if (scenario %in% names(test_data$scenarios)) {
        test_data$scenarios[[scenario]]
      } else {
        # Default to complete data if scenario not found
        test_data$scenarios$complete
      }
    })
    
    # Initialize the module with current data
    filtered_data <- microCustomerServer(
      "test_module", 
      current_data()$df_dna_by_customer, 
      current_data()$df_customer_profile
    )
    
    # Debug output
    output$debug_info <- renderPrint({
      current <- current_data()
      
      list(
        test_scenario = input$test_scenario,
        profile_data = list(
          valid = validate_data(current$df_customer_profile),
          rows = if(validate_data(current$df_customer_profile)) nrow(current$df_customer_profile) else 0,
          columns = if(validate_data(current$df_customer_profile)) ncol(current$df_customer_profile) else 0
        ),
        dna_data = list(
          valid = validate_data(current$df_dna_by_customer),
          rows = if(validate_data(current$df_dna_by_customer)) nrow(current$df_dna_by_customer) else 0,
          columns = if(validate_data(current$df_dna_by_customer)) ncol(current$df_dna_by_customer) else 0
        ),
        filtered_data = list(
          valid = validate_data(filtered_data()),
          rows = if(validate_data(filtered_data())) nrow(filtered_data()) else 0
        ),
        module_metadata = test_data$metadata
      )
    })
  }
  
  shinyApp(ui, server)
}

# 7. Test Execution

#' Run validation tests for microCustomer module
run_tests <- function() {
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

# Run tests
run_tests()
