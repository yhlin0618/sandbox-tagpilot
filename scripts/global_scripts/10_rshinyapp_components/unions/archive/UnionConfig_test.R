#' @principle MP56 Connected Component Principle
#' @principle MP52 Unidirectional Data Flow
#' @principle MP54 UI-Server Correspondence
#' @principle R91 Universal Data Access Pattern
#' @title Union Component with App Configuration Test
#' @description Demonstrates how to use the Union pattern with a complete app configuration

# Load necessary libraries (uncomment if running directly)
# library(shiny)
# library(bs4Dash)
# library(shinyjs)
# library(dplyr)

#' Assume the following files have been sourced:
#' - microCustomer.R -> provides microCustomerComponent
#' - microCustomer2.R -> provides microCustomer2Component
#' - Union.R -> provides Union function

# Simple data accessor for testing
simple_data_accessor <- function(data_connection, data_name, ...) {
  if (data_name %in% names(data_connection)) {
    return(data_connection[[data_name]])
  }
  return(NULL)
}

# Create sample app config
create_app_config <- function() {
  list(
    theme = list(
      version = 5,
      bootswatch = "cosmo"
    ),
    
    layout = "navbar",
    
    brand = list(
      name = "WISER",
      language = "zh_TW.UTF-8",
      raw_data_folder = "../rawdata_WISER",
      parameters_folder = "default"
    ),
    
    components = list(
      micro = list(
        customer_profile = list(
          primary = "customer_details",
          history = "customer_history"
        ),
        sales = "sales_by_customer_dta"
      ),
      macro = list(
        trends = list(
          data_source = "sales_trends",
          parameters = list(
            show_kpi = TRUE,
            refresh_interval = 300
          )
        )
      )
    ),
    
    company = list(
      marketing_channels = list(
        eBay = "ebay",
        `Official Website` = "officialwebsite"
      ),
      default_channel = "eBay"
    ),
    
    parameters = list(
      platform = data.frame(
        platform_name_english = c("eBay", "Official Website"),
        platform_name_chinese = c("eBay", "官網"),
        include = c(1, 1)
      ),
      platform_source = "app",
      
      product_line = data.frame(
        product_line_name_english = c("all", "Turbo", "Pressure Relief Valve", 
                                      "Iron Fin", "Aluminum Fin", "Repair Kit", "Water Hose Kit"),
        product_line_name_chinese = c("所有產品", "渦輪", "洩壓閥", 
                                       "鐵葉(渦輪葉輪)", "鋁葉(壓縮葉輪)", "修理包", "油水管包")
      ),
      product_line_source = "app",
      
      ui_terminology_dictionary = data.frame(
        `en_US.UTF-8` = c("Application Settings", "Marketing Channel", "Product Category", 
                           "Region", "Date Range", "Time Scale", "Year", "Quarter", "Month", "Customer Segment"),
        `zh_TW.UTF-8` = c("應用程式設定", "行銷通路", "商品種類", 
                           "地區", "日期範圍", "時間尺度", "年", "季", "月", "顧客區隔")
      ),
      ui_terminology_dictionary_source = "app"
    )
  )
}

# Create sample data
create_sample_data <- function() {
  # Create customer profile data
  customer_profile <- data.frame(
    customer_id = 1:5,
    buyer_name = paste0("Customer ", LETTERS[1:5]),
    email = paste0("customer", LETTERS[1:5], "@example.com"),
    platform_id = sample(c("ebay", "officialwebsite"), 5, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  # Create DNA data
  customer_dna <- data.frame(
    customer_id = 1:5,
    time_first = Sys.Date() - sample(30:300, 5),
    r_value = sample(1:30, 5),
    f_value = sample(1:10, 5),
    m_value = round(runif(5, 100, 1000), 2),
    stringsAsFactors = FALSE
  )
  
  # Create sales data
  sales_data <- data.frame(
    customer_id = rep(1:5, each = 3),
    order_date = sample(seq(Sys.Date() - 365, Sys.Date(), by = "day"), 15),
    order_amount = round(runif(15, 50, 500), 2),
    product_category = sample(c("Turbo", "Pressure Relief Valve", "Repair Kit"), 15, replace = TRUE),
    platform = sample(c("ebay", "officialwebsite"), 15, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  # Return data as a list structure
  list(
    customer_profile = customer_profile,
    dna_by_customer = customer_dna,
    sales_by_customer_dta = sales_data
  )
}

# Translation function
translate <- function(text) {
  # In a real app, this would look up translations from the config
  # Here we just return the text
  text
}

# Define UI
ui <- bs4Dash::dashboardPage(
  title = "Union Config Test",
  dark = FALSE,
  header = bs4Dash::dashboardHeader(
    title = "WISER Analysis Platform"
  ),
  
  # Sidebar with navigation and filters
  sidebar = bs4Dash::dashboardSidebar(
    bs4Dash::sidebarMenu(
      id = "sidebar_menu",
      bs4Dash::menuItem(
        text = translate("Customer Analysis"),
        tabName = "customer",
        icon = icon("user")
      ),
      bs4Dash::menuItem(
        text = translate("Advanced Analysis"),
        tabName = "advanced",
        icon = icon("chart-bar")
      ),
      bs4Dash::menuItem(
        text = translate("Sales Analysis"),
        tabName = "sales",
        icon = icon("dollar-sign")
      )
    ),
    
    # Common filters
    div(
      class = "common-filters",
      style = "padding: 15px; border-bottom: 1px solid #eee;",
      selectInput(
        "channel_filter",
        translate("Marketing Channel"),
        choices = c("eBay" = "ebay", "Official Website" = "officialwebsite"),
        selected = "ebay"
      ),
      
      selectInput(
        "product_filter",
        translate("Product Category"),
        choices = c(
          "All Products" = "all",
          "Turbo" = "turbo",
          "Pressure Relief Valve" = "valve",
          "Repair Kit" = "kit"
        ),
        selected = "all"
      )
    ),
    
    # Component-specific filters will be loaded here
    div(
      id = "component_filters",
      uiOutput("union_filters")
    )
  ),
  
  # Main content area with tabs
  body = bs4Dash::dashboardBody(
    shinyjs::useShinyjs(),
    
    # CSS styling
    tags$style(HTML("
      #component_filters > div { display: none; }
      #component_filters > div.active { display: block; }
    ")),
    
    # Tab content
    bs4Dash::tabItems(
      bs4Dash::tabItem(
        tabName = "customer",
        fluidRow(
          column(12,
            h3(translate("Customer Analysis")),
            uiOutput("customer_display")
          )
        )
      ),
      bs4Dash::tabItem(
        tabName = "advanced",
        fluidRow(
          column(12,
            h3(translate("Advanced Customer Analysis")),
            uiOutput("advanced_display")
          )
        )
      ),
      bs4Dash::tabItem(
        tabName = "sales",
        fluidRow(
          column(12,
            h3(translate("Sales Analysis")),
            uiOutput("sales_display")
          )
        )
      )
    )
  ),
  
  # Footer
  footer = bs4Dash::dashboardFooter(
    left = "WISER Analysis Platform",
    right = paste("Version", "1.0")
  )
)

# Define server
server <- function(input, output, session) {
  # Create app config
  app_config <- create_app_config()
  
  # Create sample data
  app_data <- create_sample_data()
  
  # Define simple data accessor if not available
  if (!exists("universal_data_accessor")) {
    universal_data_accessor <- simple_data_accessor
  }
  
  # Component configurations based on app config
  customer_config <- list(
    primary = app_config$components$micro$customer_profile$primary,
    history = app_config$components$micro$customer_profile$history
  )
  
  advanced_config <- list(
    include_fields = c("history", "recency", "frequency", "monetary"),
    filter_options = list(
      title = translate("Advanced Filter"),
      placeholder = translate("Search for a customer..."),
      background_color = "#f0f8ff"
    )
  )
  
  sales_config <- list(
    data_source = app_config$components$micro$sales
  )
  
  # Create components
  customer_component <- microCustomerComponent(
    id = "customer_comp",
    app_data_connection = app_data,
    config = customer_config,
    translate = translate
  )
  
  advanced_component <- microCustomer2Component(
    id = "advanced_comp",
    app_data_connection = app_data,
    include_fields = advanced_config$include_fields,
    filter_options = advanced_config$filter_options,
    translate = translate
  )
  
  # For this example, we'll use microCustomer2Component for sales too
  # In a real app, you would have a dedicated sales component
  sales_component <- microCustomer2Component(
    id = "sales_comp",
    app_data_connection = app_data,
    include_fields = c("monetary", "frequency"), # Focus on sales metrics
    filter_options = list(
      title = translate("Sales Filter"),
      background_color = "#fff0f0"
    ),
    translate = translate
  )
  
  # Create union configuration
  union_config <- list(
    initial_visibility = list(
      customer = TRUE,
      advanced = FALSE,
      sales = FALSE
    ),
    # Include relevant app-level configuration
    theme = app_config$theme,
    layout = app_config$layout,
    brand = app_config$brand
  )
  
  # Create the union
  components <- list(
    customer = customer_component,
    advanced = advanced_component,
    sales = sales_component
  )
  
  union <- Union(
    id = "main_union",
    customer = customer_component,
    advanced = advanced_component,
    sales = sales_component,
    config = union_config
  )
  
  # Render union filter UI
  output$union_filters <- renderUI({
    union$ui$filter("main_union")
  })
  
  # Render component displays for each tab
  output$customer_display <- renderUI({
    # Display for the customer tab
    div(
      id = "customer_tab_content",
      union$ui$display("main_union")
    )
  })
  
  output$advanced_display <- renderUI({
    # Display for the advanced tab
    div(
      id = "advanced_tab_content", 
      union$ui$display("main_union")
    )
  })
  
  output$sales_display <- renderUI({
    # Display for the sales tab
    div(
      id = "sales_tab_content",
      union$ui$display("main_union")
    )
  })
  
  # Initialize union server
  union_server <- union$server("main_union", app_data, session)
  
  # Handle sidebar menu selection
  observeEvent(input$sidebar_menu, {
    selected_tab <- input$sidebar_menu
    
    # Map tab names to component names
    component_map <- list(
      customer = "customer",
      advanced = "advanced",
      sales = "sales"
    )
    
    # Get the component name corresponding to the selected tab
    component_name <- component_map[[selected_tab]]
    
    if (!is.null(component_name)) {
      # Hide all components
      for (name in names(components)) {
        show_component <- name == component_name
        union_server$component_state$toggle_component(name, show = show_component)
      }
      
      # Update filter visibility with direct DOM manipulation
      shinyjs::runjs(sprintf('
        $("#component_filters > div").removeClass("active");
        $("#component_filters > div[id$=\\"%s\\"]").addClass("active");
      ', component_name))
    }
  })
  
  # Handle channel filter changes
  observeEvent(input$channel_filter, {
    channel <- input$channel_filter
    # In a real app, you would update the data connection or filter the data
    message("Channel filter changed to: ", channel)
  })
  
  # Handle product filter changes
  observeEvent(input$product_filter, {
    product <- input$product_filter
    # In a real app, you would update the data connection or filter the data
    message("Product filter changed to: ", product)
  })
  
  # Return union server for debugging
  return(union_server)
}

# Only run the app if this script is executed directly
if (interactive() && !exists("running_test_mode")) {
  shinyApp(ui, server)
}