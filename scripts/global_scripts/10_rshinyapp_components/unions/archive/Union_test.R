#' @principle MP56 Connected Component Principle
#' @principle MP52 Unidirectional Data Flow
#' @principle MP54 UI-Server Correspondence
#' @principle R91 Universal Data Access Pattern
#' @title Union Component Test App
#' @description This is a test app demonstrating the union of microCustomer and microCustomer2 components

# Load necessary libraries
library(shiny)
library(bs4Dash)
library(shinyjs)

init_script_path <- file.path("update_scripts", "global_scripts", "00_principles", 
                              "sc_initialization_app_mode.R")
source(init_script_path)

# Load universal data accessor if not already loaded
if (!exists("universal_data_accessor")) {
  source("update_scripts/global_scripts/00_principles/02_db_utils/fn_universal_data_accessor.R")
}

# Create sample data
create_sample_data <- function() {
  # Create sample data
  customer_profile <- data.frame(
    customer_id = 1:10,
    buyer_name = paste0("Demo User ", 1:10),
    email = paste0("user", 1:10, "@example.com"),
    platform_id = sample(c(1, 2, 6), 10, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  # Create a consistent sample dataset - using set.seed for consistency
  set.seed(456)
  customer_dna <- data.frame(
    customer_id = 1:10,
    time_first = as.Date(Sys.Date() - sample(100:500, 10)),
    time_first_to_now = sample(100:500, 10),
    r_value = sample(5:200, 10),
    r_label = sample(c("極近", "近期", "一般", "久遠", "非常久遠"), 10, replace = TRUE),
    f_value = sample(1:20, 10),
    f_label = sample(c("極低", "低", "一般", "高", "非常高"), 10, replace = TRUE),
    m_value = round(runif(10, 50, 5000), 2),
    m_label = sample(c("極低", "低", "一般", "高", "非常高"), 10, replace = TRUE),
    cai = round(runif(10, 0, 1), 2),
    cai_label = sample(c("不活躍", "低度活躍", "一般活躍", "活躍", "非常活躍"), 10, replace = TRUE),
    ipt_mean = round(runif(10, 20, 120), 1),
    pcv = round(runif(10, 1000, 50000), 2),
    clv = round(runif(10, 2000, 100000), 2),
    cri = round(runif(10, 0, 1), 2),
    nrec = round(runif(10, 0, 1), 2),
    nes_status = sample(c("主力型", "成長型", "沉睡型"), 10, replace = TRUE),
    nt = round(runif(10, 500, 2000), 2),
    e0t = round(runif(10, 1000, 3000), 2),
    stringsAsFactors = FALSE
  )
  
  # Create a connection-like object that works with universal_data_accessor
  connection <- list(
    df_customer_profile = customer_profile,
    df_dna_by_customer = customer_dna
  )
  
  # Add class for universal_data_accessor compatibility
  class(connection) <- c("list_connection", "list")
  
  return(connection)
}

# Create the app
ui <- bs4Dash::dashboardPage(
  title = "Union Component Test",
  fullscreen = TRUE,
  dark = FALSE,
  
  # Dashboard header
  header = bs4Dash::dashboardHeader(
    title = bs4Dash::dashboardBrand(
      title = "Union Test"
    ),
    fixed = TRUE
  ),
  
  # Sidebar for filter controls and component selection
  sidebar = bs4Dash::dashboardSidebar(
    fixed = TRUE,
    skin = "light",
    status = "primary",
    elevation = 3,
    
    # Component selection controls
    div(
      class = "component-controls",
      h4("Component Controls"),
      div(
        class = "control-buttons",
        actionButton("show_customer1", "Show Customer 1", class = "btn-primary"),
        actionButton("hide_customer1", "Hide Customer 1", class = "btn-outline-primary"),
        actionButton("show_customer2", "Show Customer 2", class = "btn-success"),
        actionButton("hide_customer2", "Hide Customer 2", class = "btn-outline-success")
      )
    ),
    
    # Placeholder for component filters
    div(
      id = "filter_container",
      class = "filter-container",
      uiOutput("union_filters")
    )
  ),
  
  # Body with component display
  body = bs4Dash::dashboardBody(
    shinyjs::useShinyjs(),
    
    # CSS styling
    tags$style(HTML("
      .control-buttons { margin-bottom: 20px; }
      .control-buttons .btn { margin-bottom: 5px; width: 100%; }
      .union-component-display, .union-component-filter { transition: opacity 0.3s ease; }
    ")),
    
    # Component display area
    div(
      class = "content-container",
      h2("Union Component Demo"),
      p("This demo shows how to combine microCustomer and microCustomer2 components using the Union function."),
      hr(),
      
      # Display area for components
      div(
        id = "display_container",
        class = "display-container",
        uiOutput("union_displays")
      )
    )
  ),
  
  # Footer
  footer = bs4Dash::dashboardFooter(
    fixed = FALSE,
    left = "Union Component Test",
    right = paste("Version", "1.0.0")
  )
)

server <- function(input, output, session) {
  # Create sample data connection
  app_data_connection <- create_sample_data()
  
  # Create components with different configurations
  customer1_config <- NULL  # Default configuration
  
  customer2_config <- list(
    include_fields = c("history", "recency", "frequency", "monetary", "cai", "ipt"),
    filter_options = list(
      title = "Customer 2 Filter",
      placeholder = "Search for a customer...",
      clear_button = TRUE,
      clear_label = "Reset",
      background_color = "#f0f8ff"
    )
  )
  
  # Create a translator function for demonstration
  translator <- function(x) {
    # Simple translation demonstration
    translations <- list(
      "微觀" = "Micro View",
      "客戶篩選" = "Customer Filter",
      "Select Customer:" = "Choose a Customer:",
      "清除篩選" = "Clear Filter"
    )
    
    if (x %in% names(translations)) {
      return(translations[[x]])
    }
    
    return(x)
  }
  
  # Create components
  customer1 <- microCustomerComponent("customer1", app_data_connection, customer1_config)
  customer2 <- microCustomer2Component("customer2", app_data_connection, 
                                      include_fields = customer2_config$include_fields,
                                      filter_options = customer2_config$filter_options,
                                      translate = translator)
  
  # Create the union with visibility configuration
  union_config <- list(
    initial_visibility = list(
      customer1 = TRUE,
      customer2 = FALSE
    )
  )
  
  # Create the union
  union <- Union("customer_union", customer1 = customer1, customer2 = customer2, config = union_config)
  
  # Initialize the union's filter UI
  output$union_filters <- renderUI({
    union$ui$filter("customer_union")
  })
  
  # Initialize the union's display UI
  output$union_displays <- renderUI({
    union$ui$display("customer_union")
  })
  
  # Initialize the union server
  union_server <- union$server("customer_union", app_data_connection, session)
  
  # Handle component visibility controls
  observeEvent(input$show_customer1, {
    union_server$component_state$toggle_component("customer1", TRUE)
  })
  
  observeEvent(input$hide_customer1, {
    union_server$component_state$toggle_component("customer1", FALSE)
  })
  
  observeEvent(input$show_customer2, {
    union_server$component_state$toggle_component("customer2", TRUE)
  })
  
  observeEvent(input$hide_customer2, {
    union_server$component_state$toggle_component("customer2", FALSE)
  })
}

# Run the app
shinyApp(ui, server)