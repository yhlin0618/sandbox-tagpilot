#' @principle P15 Debug Efficiency Exception
#' @r21_exception This file contains a minimal test application for microCustomer module
#' @justification This test app needs to be closely paired with the microCustomer module for debugging
#' @refactor_plan To be refactored after microCustomer module is finalized (est. Q3 2025)

#' Minimal Test Application for microCustomer Module
#'
#' This file provides a standalone Shiny application for testing the microCustomer module.
#' It demonstrates proper data integration and usage of the UI-server-defaults triple.
#'
#' Run this file directly with: `shiny::runApp("path/to/example_test_app.R")`
#'

library(shiny)
library(bs4Dash)
library(dplyr)
library(lubridate)

# Source the microCustomer module
source_file <- file.path(dirname(sys.frame(1)$ofile), "../microCustomer.R")
source(source_file)

# Create sample customer data
create_sample_data <- function() {
  # Create a dataframe with 5 sample customers
  set.seed(123) # For reproducible results
  
  customer_data <- data.frame(
    customer_id = 1:5,
    customer_name = c("張三", "李四", "王五", "趙六", "錢七"),
    customer_email = c(
      "zhang@example.com", 
      "li@example.com", 
      "wang@example.com", 
      "zhao@example.com", 
      "qian@example.com"
    ),
    time_first = seq(
      from = as.Date("2022-01-01"), 
      to = as.Date("2022-05-01"), 
      length.out = 5
    ),
    time_first_tonow = c(450, 400, 350, 300, 250),
    rlabel = c(1, 2, 3, 4, 5),
    rvalue = c(5, 15, 30, 60, 90),
    flabel = c(5, 4, 3, 2, 1),
    fvalue = c(20, 15, 10, 5, 2),
    mlabel = c(4, 5, 3, 2, 1),
    mvalue = c(5000, 8000, 3000, 1500, 500),
    cailabel = c(4, 5, 3, 2, 1),
    cai = c(0.85, 0.95, 0.65, 0.35, 0.15),
    ipt_mean = c(14.5, 7.2, 21.3, 45.7, 90.2),
    pcv = c(12000, 20000, 8000, 4000, 1000),
    clv = c(18000, 30000, 10000, 5000, 1200),
    cri = c(0.85, 0.92, 0.78, 0.55, 0.32),
    nrec = c(0.85, 0.95, 0.45, 0.25, 0.05),
    nesstatus = c("主力客", "主力客", "一般客", "低頻客", "待喚醒"),
    nt = c(1500, 2000, 800, 500, 300),
    e0t = c(2500, 3500, 1200, 700, 400)
  )
  
  return(customer_data)
}

# Define UI
ui <- bs4Dash::dashboardPage(
  title = "microCustomer Test",
  header = bs4Dash::dashboardHeader(
    title = "微觀客戶測試"
  ),
  sidebar = bs4Dash::dashboardSidebar(
    bs4Dash::sidebarMenu(
      id = "sidebar",
      bs4Dash::menuItem(
        text = "客戶資料",
        tabName = "customer",
        icon = icon("user")
      ),
      selectInput(
        "customer_selector", 
        "選擇客戶", 
        choices = NULL
      )
    )
  ),
  body = bs4Dash::dashboardBody(
    bs4Dash::tabItems(
      bs4Dash::tabItem(
        tabName = "customer",
        microCustomerUI("micro_customer")
      )
    )
  ),
  controlbar = bs4Dash::dashboardControlbar(),
  footer = bs4Dash::dashboardFooter(
    left = "Precision Marketing MAMBA",
    right = format(Sys.Date(), "%Y")
  ),
  dark = FALSE
)

# Define server
server <- function(input, output, session) {
  # Generate sample data
  sample_data <- create_sample_data()
  
  # Update customer selector
  observe({
    customer_choices <- setNames(
      sample_data$customer_id,
      paste(sample_data$customer_name, "-", sample_data$customer_email)
    )
    updateSelectInput(session, "customer_selector", choices = customer_choices)
  })
  
  # Filter data based on selected customer
  selected_customer <- reactive({
    req(input$customer_selector)
    filter(sample_data, customer_id == input$customer_selector)
  })
  
  # Initialize microCustomer module with selected customer data
  microCustomerServer("micro_customer", data_source = list(
    primary = selected_customer,
    sales_by_customer = selected_customer
  ))
}

# Run the application
shinyApp(ui = ui, server = server)