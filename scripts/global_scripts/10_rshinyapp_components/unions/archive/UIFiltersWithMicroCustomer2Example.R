# Example demonstrating the UIFiltersUnion component with microCustomer2
# @principle MP56 Connected Component Principle
# @principle R91 Universal Data Access Pattern

# Load required libraries
library(shiny)
library(bs4Dash)
library(shinyjs)
library(dplyr)

# Source the required components
source("update_scripts/global_scripts/10_rshinyapp_components/unions/ComponentsUnion.R")
source("update_scripts/global_scripts/10_rshinyapp_components/micro/microCustomer/microCustomer2.R")

# Demo app to showcase the UIFiltersUnion component with microCustomer2
UIFiltersWithMicroCustomer2Example <- function() {
  
  # Create sample data for microCustomer2
  sample_dna_data <- data.frame(
    customer_id = 1:10,
    time_first = Sys.Date() - sample(30:500, 10),
    time_first_to_now = sample(30:500, 10),
    r_label = sample(c("極近", "近期", "一般", "久遠", "非常久遠"), 10, replace = TRUE),
    r_value = sample(1:100, 10),
    f_label = sample(c("極低", "低", "一般", "高", "非常高"), 10, replace = TRUE),
    f_value = sample(1:20, 10),
    m_label = sample(c("極低", "低", "一般", "高", "非常高"), 10, replace = TRUE),
    m_value = round(runif(10, 100, 10000), 2),
    cai_label = sample(c("不活躍", "低度活躍", "一般活躍", "活躍", "非常活躍"), 10, replace = TRUE),
    cai = round(runif(10, 0, 1), 2),
    ipt_mean = round(runif(10, 5, 60), 1),
    pcv = round(runif(10, 100, 5000), 2),
    clv = round(runif(10, 1000, 20000), 2),
    cri = round(runif(10, 0, 1), 2),
    nes_status = sample(c("新客", "主力", "休眠", "流失"), 10, replace = TRUE),
    nt = round(runif(10, 50, 500), 2),
    e0t = round(runif(10, 100, 1000), 2)
  )
  
  sample_customer_profiles <- data.frame(
    customer_id = 1:10,
    buyer_name = paste0("Customer ", LETTERS[1:10]),
    email = paste0("customer", 1:10, "@example.com"),
    phone = paste0("0912345", sprintf("%03d", 1:10)),
    address = paste0("Address ", 1:10)
  )
  
  # Create a mock app_data_connection
  app_data_connection <- list(
    dna_by_customer = function() { sample_dna_data },
    customer_profile = function() { sample_customer_profiles }
  )
  
  # Initialize microCustomer2 for RFM-only
  rfm_customer_component <- microCustomer2Initialize(
    id = "rfm_customer",
    app_data_connection = app_data_connection,
    include_fields = c("recency", "frequency", "monetary")
  )
  
  # Initialize microCustomer2 for Value metrics
  value_customer_component <- microCustomer2Initialize(
    id = "value_customer",
    app_data_connection = app_data_connection,
    include_fields = c("pcv", "clv", "cri")
  )
  
  # Create additional filter components
  
  # Date Range Filter
  date_filter <- div(
    h4("Date Filter", style = "color: #3c8dbc;"),
    dateRangeInput(
      "date_range", 
      "Select Date Range:", 
      start = Sys.Date() - 30, 
      end = Sys.Date()
    ),
    checkboxInput(
      "compare_previous",
      "Compare with previous period",
      value = FALSE
    )
  )
  
  # Region Filter
  region_filter <- div(
    h4("Region Filter", style = "color: #3c8dbc;"),
    selectInput(
      "region", 
      "Select Region:", 
      choices = c("All Regions" = "all", "North America" = "na", "Europe" = "eu", "Asia" = "as"),
      selected = "all"
    )
  )
  
  # Common filters that always appear
  common_filters <- div(
    h4("Common Settings", style = "color: #666; border-bottom: 1px solid #eee; padding-bottom: 5px;"),
    radioButtons(
      "time_granularity",
      "Time Granularity:",
      choices = c("Daily", "Weekly", "Monthly"),
      selected = "Daily"
    )
  )
  
  # Initialize the Union component
  filters_union <- UnionComponentsInitialize(
    id = "sidebar_filters",
    filters = list(
      rfm = rfm_customer_component$ui$filter,
      value = value_customer_component$ui$filter,
      date = date_filter,
      region = region_filter
    ),
    initial_tab = "rfm",
    container_type = "sidebar",
    container_style = NULL,  # Use default
    title = "Analysis Filters",
    common_filters = common_filters
  )
  
  # Create the UI
  ui <- bs4Dash::dashboardPage(
    title = "UIFiltersUnion with microCustomer2 Example",
    dark = FALSE,
    
    # Header
    header = bs4Dash::dashboardHeader(
      title = bs4Dash::dashboardBrand(
        title = "UIFiltersUnion + microCustomer2 Demo",
        color = "primary"
      )
    ),
    
    # Sidebar with filter union
    sidebar = filters_union$ui,
    
    # Body content
    body = bs4Dash::dashboardBody(
      shinyjs::useShinyjs(),
      
      # Buttons to switch between filter types
      fluidRow(
        column(
          width = 12,
          box(
            title = "Filter Controls",
            width = 12,
            status = "primary",
            
            fluidRow(
              column(width = 3, actionButton("show_rfm", "RFM Customer", class = "btn-block btn-primary")),
              column(width = 3, actionButton("show_value", "Value Customer", class = "btn-block btn-success")),
              column(width = 3, actionButton("show_date", "Date Filter", class = "btn-block btn-info")),
              column(width = 3, actionButton("show_region", "Region Filter", class = "btn-block btn-warning"))
            )
          )
        )
      ),
      
      # Tab content with different displays
      bs4Dash::tabsetPanel(
        id = "main_tabs",
        type = "pills",
        
        # RFM customer tab
        bs4Dash::tabPanel(
          title = "RFM Analysis",
          value = "rfm_tab",
          fluidRow(
            column(
              width = 12,
              # RFM microCustomer2 display
              rfm_customer_component$ui$display
            )
          )
        ),
        
        # Value metrics tab
        bs4Dash::tabPanel(
          title = "Value Analysis",
          value = "value_tab",
          fluidRow(
            column(
              width = 12,
              # Value microCustomer2 display
              value_customer_component$ui$display
            )
          )
        ),
        
        # Date analysis tab
        bs4Dash::tabPanel(
          title = "Date Analysis",
          value = "date_tab",
          fluidRow(
            column(
              width = 12,
              # Placeholder for date analysis dashboard
              box(
                title = "Date Analysis Dashboard",
                width = 12,
                status = "info",
                solidHeader = TRUE,
                
                # Date filter summary
                verbatimTextOutput("date_summary")
              )
            )
          )
        ),
        
        # Region analysis tab
        bs4Dash::tabPanel(
          title = "Region Analysis",
          value = "region_tab",
          fluidRow(
            column(
              width = 12,
              # Placeholder for region analysis dashboard
              box(
                title = "Region Analysis Dashboard",
                width = 12,
                status = "warning",
                solidHeader = TRUE,
                
                # Region filter summary
                verbatimTextOutput("region_summary")
              )
            )
          )
        )
      ),
      
      # Display current filter state
      fluidRow(
        column(
          width = 12,
          box(
            title = "Current Filter State",
            width = 12,
            status = "secondary",
            
            # Show which filters are currently visible
            verbatimTextOutput("visible_filters")
          )
        )
      )
    )
  )
  
  # Server logic
  server <- function(input, output, session) {
    # Initialize the UIFiltersUnion server
    union_state <- filters_union$server(input, output, session)
    
    # Initialize the microCustomer2 components
    rfm_data <- rfm_customer_component$server(input, output, session)
    value_data <- value_customer_component$server(input, output, session)
    
    # Sync tab changes with filter visibility
    observeEvent(input$main_tabs, {
      # Show the appropriate filter based on active tab
      switch(input$main_tabs,
        "rfm_tab" = union_state$toggle_component("rfm"),
        "value_tab" = union_state$toggle_component("value"),
        "date_tab" = union_state$toggle_component("date"),
        "region_tab" = union_state$toggle_component("region")
      )
    })
    
    # Handle filter button clicks
    observeEvent(input$show_rfm, {
      updateTabsetPanel(session, "main_tabs", selected = "rfm_tab")
      union_state$toggle_component("rfm")
    })
    
    observeEvent(input$show_value, {
      updateTabsetPanel(session, "main_tabs", selected = "value_tab")
      union_state$toggle_component("value")
    })
    
    observeEvent(input$show_date, {
      updateTabsetPanel(session, "main_tabs", selected = "date_tab")
      union_state$toggle_component("date")
    })
    
    observeEvent(input$show_region, {
      updateTabsetPanel(session, "main_tabs", selected = "region_tab")
      union_state$toggle_component("region")
    })
    
    # Display which filters are currently visible
    output$visible_filters <- renderPrint({
      visible <- union_state$visible_components()
      
      # Format the output
      if (length(visible) == 0) {
        return("No filters currently visible")
      } else {
        list(
          visible_filters = visible,
          common_settings = list(
            time_granularity = input$time_granularity
          )
        )
      }
    })
    
    # Display filter summaries
    output$date_summary <- renderPrint({
      list(
        date_range = c(input$date_range[1], input$date_range[2]),
        compare_previous = input$compare_previous,
        time_granularity = input$time_granularity
      )
    })
    
    output$region_summary <- renderPrint({
      list(
        region = input$region,
        time_granularity = input$time_granularity
      )
    })
  }
  
  # Run the app
  shinyApp(ui, server)
}

# Execute the example application when this script is sourced
if (interactive()) {
  UIFiltersWithMicroCustomer2Example()
}