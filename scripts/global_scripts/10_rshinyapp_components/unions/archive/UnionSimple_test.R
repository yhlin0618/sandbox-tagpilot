#' @principle MP56 Connected Component Principle
#' @principle MP52 Unidirectional Data Flow
#' @principle MP54 UI-Server Correspondence
#' @principle R91 Universal Data Access Pattern
#' @title Union Component Simple Test
#' @description A simplified test of the Union component pattern

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

# Create sample data
create_sample_data <- function() {
  # Create customer profile data
  customer_profile <- data.frame(
    customer_id = 1:5,
    buyer_name = paste0("Customer ", LETTERS[1:5]),
    email = paste0("customer", LETTERS[1:5], "@example.com"),
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
  
  # Return data as a list structure
  list(
    customer_profile = customer_profile,
    dna_by_customer = customer_dna
  )
}

# Define UI
ui <- bs4Dash::dashboardPage(
  title = "Union Simple Test",
  dark = FALSE,
  header = bs4Dash::dashboardHeader(
    title = "Union Demo"
  ),
  
  # Sidebar with navigation and filters
  sidebar = bs4Dash::dashboardSidebar(
    bs4Dash::sidebarMenu(
      id = "sidebar_menu",
      bs4Dash::menuItem(
        text = "Customer Analysis",
        tabName = "customer1",
        icon = icon("user")
      ),
      bs4Dash::menuItem(
        text = "Advanced Analysis",
        tabName = "customer2",
        icon = icon("chart-bar")
      )
    ),
    
    # Filters will be loaded here
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
        tabName = "customer1",
        h3("Basic Customer Analysis"),
        uiOutput("union_displays")
      ),
      bs4Dash::tabItem(
        tabName = "customer2",
        h3("Advanced Customer Analysis"),
        uiOutput("union_displays2")
      )
    )
  ),
  
  # Footer
  footer = bs4Dash::dashboardFooter(
    left = "Union Simple Test",
    right = paste("Version", "1.0")
  )
)

# Define server
server <- function(input, output, session) {
  # Create sample data
  app_data <- create_sample_data()
  
  # Define simple data accessor if not available
  if (!exists("universal_data_accessor")) {
    universal_data_accessor <- simple_data_accessor
  }
  
  # Component configurations
  customer1_config <- list(
    # Basic config - no special settings
  )
  
  customer2_config <- list(
    # Include only specific fields
    include_fields = c("history", "recency", "frequency", "monetary"),
    filter_options = list(
      title = "Advanced Filter",
      placeholder = "Search for a customer...",
      background_color = "#f0f8ff"
    )
  )
  
  # Create components
  customer1 <- microCustomerComponent(
    id = "customer1",
    app_data_connection = app_data,
    config = customer1_config
  )
  
  customer2 <- microCustomer2Component(
    id = "customer2",
    app_data_connection = app_data,
    include_fields = customer2_config$include_fields,
    filter_options = customer2_config$filter_options
  )
  
  # Create union configuration
  union_config <- list(
    initial_visibility = list(
      customer1 = TRUE,
      customer2 = FALSE
    )
  )
  
  # Create the union
  union <- Union(
    id = "main_union",
    customer1 = customer1,
    customer2 = customer2,
    config = union_config
  )
  
  # Render union filter UI
  output$union_filters <- renderUI({
    union$ui$filter("main_union")
  })
  
  # Render union display UI
  output$union_displays <- renderUI({
    # Only customer1 component will actually be visible
    # based on initial_visibility setting in union_config
    union$ui$display("main_union")
  })
  
  # Initialize union server
  union_server <- union$server("main_union", app_data, session)
  
  # Handle sidebar menu selection
  observeEvent(input$sidebar_menu, {
    selected_tab <- input$sidebar_menu
    
    # Show appropriate component based on selection
    if (selected_tab == "customer1") {
      union_server$component_state$toggle_component("customer1", show = TRUE)
      union_server$component_state$toggle_component("customer2", show = FALSE)
      
      # Update filter visibility with direct DOM manipulation
      shinyjs::runjs('
        $("#component_filters > div").removeClass("active");
        $("#component_filters > div:first-child").addClass("active");
      ')
    } else if (selected_tab == "customer2") {
      union_server$component_state$toggle_component("customer1", show = FALSE)
      union_server$component_state$toggle_component("customer2", show = TRUE)
      
      # Update filter visibility with direct DOM manipulation
      shinyjs::runjs('
        $("#component_filters > div").removeClass("active");
        $("#component_filters > div:nth-child(2)").addClass("active");
      ')
    }
  })
  
  # Return union state (useful for debugging)
  return(union_server)
}

# Only run the app if this script is executed directly
if (interactive() && !exists("running_test_mode")) {
  shinyApp(ui, server)
}