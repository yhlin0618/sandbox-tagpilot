#LOCK FILE
#
# microCustomer2_test.R 
#
# Test file for the microCustomer2 component with configurable fields
#

# Initialize in APP_MODE using the standard initialization script
init_script_path <- file.path("update_scripts", "global_scripts", "00_principles", 
                            "sc_initialization_app_mode.R")
# Source the initialization script
source(init_script_path)

# Load required libraries
library(shiny)
library(shinydashboard)
library(bs4Dash)
library(dplyr)

# Source the component file
component_path <- file.path("update_scripts", "global_scripts", "10_rshinyapp_components", 
                          "micro", "microCustomer", "microCustomer2.R")
source(component_path)
# 
# # Create mock data connection function
# universal_data_accessor <- function(data_connection, data_name, query_template = NULL, log_level = 2) {
#   # Return mock data for testing
#   if (data_name == "dna_by_customer") {
#     # Create mock DNA data
#     n_customers <- 10
#     
#     # Sample data with customer IDs 101-110
#     set.seed(123)
#     data.frame(
#       customer_id = 101:110,
#       time_first = as.Date("2023-01-01") - sample(0:300, n_customers, replace = TRUE),
#       time_first_to_now = sample(30:500, n_customers, replace = TRUE),
#       r_label = sample(c("極近", "近期", "一般", "久遠", "非常久遠"), n_customers, replace = TRUE),
#       r_value = sample(1:90, n_customers, replace = TRUE),
#       f_label = sample(c("極低", "低", "一般", "高", "非常高"), n_customers, replace = TRUE),
#       f_value = sample(1:20, n_customers, replace = TRUE),
#       m_label = sample(c("極低", "低", "一般", "高", "非常高"), n_customers, replace = TRUE),
#       m_value = sample(100:10000, n_customers, replace = TRUE) / 100,
#       cai_label = sample(c("不活躍", "低度活躍", "一般活躍", "活躍", "非常活躍"), n_customers, replace = TRUE),
#       cai = sample(1:100, n_customers, replace = TRUE) / 10,
#       ipt_mean = sample(5:60, n_customers, replace = TRUE),
#       pcv = sample(1000:50000, n_customers, replace = TRUE) / 100,
#       clv = sample(2000:100000, n_customers, replace = TRUE) / 100,
#       cri = sample(1:100, n_customers, replace = TRUE) / 100,
#       nes_status = sample(c("活躍", "流失風險", "沉睡", "已流失"), n_customers, replace = TRUE),
#       nt = sample(500:2000, n_customers, replace = TRUE) / 10,
#       e0t = sample(1000:5000, n_customers, replace = TRUE) / 10
#     )
#   } else if (data_name == "customer_profile") {
#     # Create mock customer profile data
#     n_customers <- 10
#     
#     data.frame(
#       customer_id = 101:110,
#       buyer_name = paste0("Customer-", 101:110),
#       email = paste0("customer", 101:110, "@example.com"),
#       gender = sample(c("Male", "Female"), n_customers, replace = TRUE),
#       age = sample(20:60, n_customers, replace = TRUE),
#       location = sample(c("New York", "Los Angeles", "Chicago", "Houston", "Phoenix"), 
#                        n_customers, replace = TRUE)
#     )
#   } else {
#     # Return NULL for unknown data names
#     NULL
#   }
# }

# Define the app UI with tabs for different configurations
ui <- bs4Dash::dashboardPage(
  title = "microCustomer2 Test App",
  header = bs4Dash::dashboardHeader(
    title = "microCustomer2 Test"
  ),
  sidebar = bs4Dash::dashboardSidebar(
    bs4Dash::sidebarMenu(
      id = "tabs",
      bs4Dash::menuItem("Full View", tabName = "full", icon = icon("th")),
      bs4Dash::menuItem("RFM Only", tabName = "rfm", icon = icon("chart-pie")),
      bs4Dash::menuItem("Value Metrics", tabName = "value", icon = icon("dollar-sign")),
      bs4Dash::menuItem("Custom Config", tabName = "custom", icon = icon("cogs"))
    )
  ),
  body = bs4Dash::dashboardBody(
    tags$head(
      tags$style(HTML("
        .component-header {
          margin-bottom: 15px;
          border-bottom: 1px solid #eee;
          padding-bottom: 10px;
        }
        .component-output {
          border: 1px solid #ddd;
          border-radius: 4px;
          padding: 15px;
          background-color: #f9f9f9;
        }
        .stats-container {
          display: flex;
          flex-wrap: wrap;
          margin-top: 10px;
        }
        .stat-box {
          flex: 1 0 22%;
          margin: 5px;
          padding: 10px;
          background-color: #f5f5f5;
          border-radius: 4px;
          text-align: center;
        }
      "))
    ),
    bs4Dash::tabItems(
      # Tab 1: Full component (all fields)
      bs4Dash::tabItem(
        tabName = "full",
        fluidRow(
          column(width = 3,
                 div(
                   class = "component-container",
                   h3("Full Component"),
                   p("Shows all available fields"),
                   # Filter component for Full View
                   uiOutput("full_filter")
                 )
          ),
          column(width = 9,
                 # Main component for Full View
                 uiOutput("full_content")
          )
        )
      ),
      
      # Tab 2: RFM only view
      bs4Dash::tabItem(
        tabName = "rfm",
        fluidRow(
          column(width = 3,
                 div(
                   class = "component-container",
                   h3("RFM Only"),
                   p("Shows only Recency, Frequency, and Monetary metrics"),
                   # Filter component for RFM View
                   uiOutput("rfm_filter")
                 )
          ),
          column(width = 9,
                 # Main component for RFM View
                 uiOutput("rfm_content")
          )
        )
      ),
      
      # Tab 3: Value metrics only
      bs4Dash::tabItem(
        tabName = "value",
        fluidRow(
          column(width = 3,
                 div(
                   class = "component-container",
                   h3("Value Metrics"),
                   p("Shows only value-related metrics (PCV, CLV, CRI)"),
                   # Filter component for Value View
                   uiOutput("value_filter")
                 )
          ),
          column(width = 9,
                 # Main component for Value View
                 uiOutput("value_content")
          )
        )
      ),
      
      # Tab 4: Custom configuration
      bs4Dash::tabItem(
        tabName = "custom",
        fluidRow(
          column(width = 3,
                 div(
                   class = "component-container",
                   h3("Custom Configuration"),
                   p("Select which fields to display:"),
                   checkboxGroupInput("selected_fields", "Show Fields:",
                                    choiceNames = list(
                                      "History",
                                      "Recency (R)",
                                      "Frequency (F)",
                                      "Monetary (M)",
                                      "Customer Activity (CAI)",
                                      "Inter-purchase Time",
                                      "Past Customer Value",
                                      "Customer Lifetime Value",
                                      "Customer Regularity Index",
                                      "NES Status",
                                      "New Transaction",
                                      "E0 Transaction"
                                    ),
                                    choiceValues = list(
                                      "history",
                                      "recency",
                                      "frequency",
                                      "monetary",
                                      "cai",
                                      "ipt",
                                      "pcv",
                                      "clv",
                                      "cri",
                                      "nes",
                                      "nt",
                                      "e0t"
                                    ),
                                    selected = c("history", "recency", "frequency", "monetary")),
                   actionButton("update_custom", "Update Display", class = "btn-primary"),
                   hr(),
                   # Filter component for Custom View
                   uiOutput("custom_filter")
                 )
          ),
          column(width = 9,
                 # Main component for Custom View
                 uiOutput("custom_content")
          )
        )
      )
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  # Initialize the full component
  full_component <- microCustomer2Initialize(
    id = "full_component",
    app_data_connection = NULL # Using mock data
  )
  
  # Initialize the RFM-only component
  rfm_component <- microCustomer2Initialize(
    id = "rfm_component",
    app_data_connection = NULL, # Using mock data
    include_fields = c("recency", "frequency", "monetary")
  )
  
  # Initialize the value-metrics component
  value_component <- microCustomer2Initialize(
    id = "value_component",
    app_data_connection = NULL, # Using mock data
    include_fields = c("pcv", "clv", "cri")
  )
  
  # Custom component state
  custom_component <- reactiveVal(NULL)
  
  # Initialize the custom component with default selections
  observe({
    custom_component(microCustomer2Initialize(
      id = "custom_component",
      app_data_connection = NULL, # Using mock data
      include_fields = c("history", "recency", "frequency", "monetary")
    ))
  })
  
  # Update custom component when button is clicked
  observeEvent(input$update_custom, {
    selected <- input$selected_fields
    if (length(selected) > 0) {
      custom_component(microCustomer2Initialize(
        id = "custom_component",
        app_data_connection = NULL, # Using mock data
        include_fields = selected
      ))
    } else {
      showNotification("Please select at least one field", type = "warning")
    }
  })
  
  # Render the UI components
  output$full_filter <- renderUI({
    full_component$ui$filter
  })
  
  output$full_content <- renderUI({
    full_component$ui$display
  })
  
  output$rfm_filter <- renderUI({
    rfm_component$ui$filter
  })
  
  output$rfm_content <- renderUI({
    rfm_component$ui$display
  })
  
  output$value_filter <- renderUI({
    value_component$ui$filter
  })
  
  output$value_content <- renderUI({
    value_component$ui$display
  })
  
  output$custom_filter <- renderUI({
    req(custom_component())
    custom_component()$ui$filter
  })
  
  output$custom_content <- renderUI({
    req(custom_component())
    custom_component()$ui$display
  })
  
  # Initialize the server components
  full_filtered_data <- full_component$server(input, output, session)
  rfm_filtered_data <- rfm_component$server(input, output, session)
  value_filtered_data <- value_component$server(input, output, session)
  
  # Observe custom component changes and initialize server if needed
  observe({
    comp <- custom_component()
    if (!is.null(comp)) {
      comp$server(input, output, session)
    }
  })
}

# Run the test app
shinyApp(ui, server)