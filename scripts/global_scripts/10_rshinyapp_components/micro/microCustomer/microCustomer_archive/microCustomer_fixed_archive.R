# Fixed version of microCustomerDisplayUI for compatibility with Union pattern
# This fixes the incompatibility with nav_panel
# Following MP107 Root Cause Resolution Principle - Fix the real problem at its source

#' Micro Customer Display UI Component (Fixed Version)
#'
#' This component provides the UI elements for displaying detailed customer analytics
#' in the micro-level view of the application.
#'
#' @param id The module ID
#'
#' @return A UI component
#' @export
microCustomerDisplayUI_fixed <- function(id) {
  ns <- NS(id)
  
  # Ensure we're using shiny namespacing
  cat("Creating microCustomerDisplayUI_fixed with ID:", id, "\n")
  
  # Use HTML() for title to avoid encoding issues
  # Use simple div structure for maximum compatibility following R19: Component Visibility Guarantee
  div(
    id = paste0("micro-customer-display-", id),
    class = "microCustomer-display display-component customer-display",
    style = "display: block !important; visibility: visible !important; opacity: 1 !important; border: 1px solid #e9ecef; border-radius: 5px; padding: 15px; margin-bottom: 20px;",
    
    # Add debug message visible in the UI to confirm rendering
    tags$div(
      class = "debug-message",
      style = "background: rgba(255,255,0,0.1); padding: 5px; margin-bottom: 10px; font-size: 10px;",
      "Component microCustomerDisplayUI_fixed rendering with ID: ", id
    ),
    
    # Title with encoding safety
    HTML("<h3 class='section-title'>微觀客戶分析</h3>"),
    
    # Add comprehensive visibility guarantee tags for all children
    tags$div(
      class = "visibility-guarantee-wrapper",
      `data-component`="microCustomer", 
      `data-visibility`="force-visible",
      style = "display: block !important; visibility: visible !important; opacity: 1 !important;",
      
      # Simple div structure instead of card for better compatibility
      div(
        class = "simple-card",
        style = "width: 100%; margin-bottom: 20px; border: 1px solid #eee; border-radius: 5px; padding: 15px; display: block !important;",
        div(
          class = "card-body",
          style = "display: block !important;",
          # Header: Customer Name and Email
          fluidRow(
            column(12,
                   div(
                     class = "customer-profile-header",
                     style = "margin-bottom: 15px; padding-bottom: 15px; border-bottom: 1px solid #eee; text-align: center;",
                     div(
                       style = "font-size: 1.4rem; font-weight: 600;",
                       htmlOutput(ns("customer_name"))
                     ),
                     div(
                       style = "font-size: 1rem; color: #666;",
                       htmlOutput(ns("customer_email"))
                     )
                   )
            )
          ),
          # Row 1: 顧客資歷、最近購買日(R)、購買頻率(F)
          fluidRow(
            column(4, bs4Dash::valueBoxOutput(ns("dna_time_first"), width = 12)),
            column(4, bs4Dash::valueBoxOutput(ns("dna_recency"), width = 12)),
            column(4, bs4Dash::valueBoxOutput(ns("dna_frequency"), width = 12))
          ),
          # Row 2: 購買金額(M)、顧客活躍度(CAI)、顧客平均購買週期(IPT)
          fluidRow(
            column(4, bs4Dash::valueBoxOutput(ns("dna_monetary"), width = 12)),
            column(4, bs4Dash::valueBoxOutput(ns("dna_cai"), width = 12)),
            column(4, bs4Dash::valueBoxOutput(ns("dna_ipt"), width = 12))
          ),
          # Row 3: 過去價值(PCV)、顧客終身價值(CLV)、顧客交易穩定度 (CRI)
          fluidRow(
            column(4, bs4Dash::valueBoxOutput(ns("dna_pcv"), width = 12)),
            column(4, bs4Dash::valueBoxOutput(ns("dna_clv"), width = 12)),
            column(4, bs4Dash::valueBoxOutput(ns("dna_cri"), width = 12))
          ),
          # Row 4: 顧客狀態(NES)、新客單價、主力客單價
          fluidRow(
            column(4, bs4Dash::valueBoxOutput(ns("dna_nes"), width = 12)),
            column(4, bs4Dash::valueBoxOutput(ns("dna_nt"), width = 12)),
            column(4, bs4Dash::valueBoxOutput(ns("dna_e0t"), width = 12))
          )
        )
      )
    )
  ) 
}

#' Micro Customer Filter UI
#'
#' Creates the filter UI for the micro customer component.
#' 
#' @param id The module ID
#' @param translate Translation function, defaults to identity function
#'
#' @return A UI filter component
#' @export
microCustomerFilterUI <- function(id, translate = function(x) x) {
  ns <- NS(id)
  
  # Create a simple filter UI
  div(
    id = paste0("micro-customer-filter-", id),
    class = "microCustomer-filter filter-component customer-filter",
    style = "display: block !important; visibility: visible !important; opacity: 1 !important;",
    
    # Filter title
    h4(translate("Customer Filter"), class = "filter-title"),
    
    # Customer search
    selectizeInput(
      inputId = ns("customer_id"),
      label = translate("Search Customer"),
      choices = NULL,
      options = list(
        placeholder = translate("Type to search..."),
        closeAfterSelect = TRUE,
        searchField = c("name", "email"),
        render = I("{
          option: function(item, escape) {
            return '<div>' +
              '<strong>' + escape(item.name) + '</strong><br>' +
              '<small>' + escape(item.email) + '</small>' +
            '</div>';
          }
        }")
      ),
      multiple = FALSE
    ),
    
    # Date range filter
    dateRangeInput(
      inputId = ns("date_range"),
      label = translate("Date Range"),
      start = Sys.Date() - 90,
      end = Sys.Date(),
      format = "yyyy-mm-dd",
      separator = " to "
    ),
    
    # Apply button
    actionButton(
      inputId = ns("apply_filter"),
      label = translate("Apply Filter"),
      icon = icon("filter"),
      class = "btn-primary",
      width = "100%"
    )
  )
}

#' Fixed Micro Customer Component with Universal Data Access
#'
#' This component combines the filter and display UI components and 
#' adds the server logic for the micro customer view.
#'
#' @param id Module ID
#' @param app_data_connection App data connection (any supported connection type)
#' @param config Optional configuration parameters
#' @param translate Translation function, defaults to identity function
#'
#' @return A list containing UI elements and server function
#' @export
microCustomerComponent_fixed <- function(id, app_data_connection = NULL, config = NULL, translate = function(x) x) {
  cat("Creating microCustomerComponent_fixed with ID:", id, "\n")
  
  # Create a display function that we'll verify works
  display_fn <- function(display_id) {
    cat("Display function called with ID:", display_id, "\n")
    result <- microCustomerDisplayUI_fixed(display_id)
    cat("Display function returning UI element of class:", class(result)[1], "\n")
    return(result)
  }
  
  # Create the component with explicit function references
  result <- list(
    ui = list(
      filter = microCustomerFilterUI(id, translate),
      display = display_fn  # Use our wrapper function for better diagnostics
    ),
    server = function(input, output, session) {
      cat("Server function for microCustomerComponent_fixed called with ID:", id, "\n")
      # Pass the session for proper namespace handling
      filtered_data <- microCustomerServer(
        id, 
        app_data_connection,
        session
      )
      
      # Return the filtered data for potential outside use
      return(filtered_data)
    },
    id = id,  # Store the ID for debugging
    original_id = id  # Required by Union pattern
  )
  
  # Make globally accessible with three different naming patterns to ensure it's found
  cat("Registering component globally with multiple names\n")
  assign(paste0("microCustomer_", id), result, envir = .GlobalEnv)
  assign(paste0("micro_customer_component"), result, envir = .GlobalEnv)
  assign(paste0("customer_component"), result, envir = .GlobalEnv)
  
  # Verify registration
  cat("Verifying global registration...\n")
  if (exists(paste0("microCustomer_", id), envir = .GlobalEnv)) {
    cat("Component registered as", paste0("microCustomer_", id), "\n")
  }
  
  return(result)
}

#' Micro Customer Server Function
#'
#' Server-side logic for the micro customer component.
#'
#' @param id The module ID
#' @param app_data_connection Data connection object
#' @param session The Shiny session object
#'
#' @return A reactive expression with filtered data
#' @export
microCustomerServer <- function(id, app_data_connection, session = getDefaultReactiveDomain()) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Log server initialization
    cat("Initializing microCustomerServer with ID:", id, "\n")
    
    # Initialize reactive values for customer data
    customer_data <- reactiveVal(NULL)
    selected_customer <- reactiveVal(NULL)
    
    # When the connection is ready, initialize customer data
    observe({
      req(app_data_connection)
      
      # For demonstration, create some placeholder data
      # In a real implementation, this would fetch data from the connection
      demo_data <- data.frame(
        customer_id = paste0("CUST", 1:5),
        buyer_name = c("John Smith", "Alice Wong", "Maria Garcia", "Robert Lee", "Sarah Johnson"),
        email = c("john@example.com", "alice@example.com", "maria@example.com", 
                  "robert@example.com", "sarah@example.com"),
        stringsAsFactors = FALSE
      )
      
      customer_data(demo_data)
      cat("Customer data initialized with", nrow(demo_data), "records\n")
    })
    
    # Update the customer selectize input when data is available
    observe({
      req(customer_data())
      
      # Create choices for selectize
      data <- customer_data()
      choices <- lapply(1:nrow(data), function(i) {
        list(
          id = data$customer_id[i],
          name = data$buyer_name[i],
          email = data$email[i],
          label = paste0(data$buyer_name[i], " (", data$email[i], ")")
        )
      })
      
      # Update the selectize input
      updateSelectizeInput(
        session,
        "customer_id",
        choices = choices,
        server = TRUE
      )
    })
    
    # When a customer is selected, update selected_customer
    observeEvent(input$customer_id, {
      req(input$customer_id, customer_data())
      
      data <- customer_data()
      selected <- data[data$customer_id == input$customer_id, ]
      
      if (nrow(selected) > 0) {
        selected_customer(selected)
        cat("Selected customer:", selected$buyer_name[1], "\n")
      }
    })
    
    # Output renderers for customer profile
    output$customer_name <- renderText({
      req(selected_customer())
      selected_customer()$buyer_name[1]
    })
    
    output$customer_email <- renderText({
      req(selected_customer())
      selected_customer()$email[1]
    })
    
    # ValueBox outputs for DNA metrics (placeholders in this version)
    output$dna_time_first <- renderValueBox({
      bs4Dash::valueBox(
        value = "2021-05-10",
        subtitle = "First Purchase",
        icon = icon("calendar"),
        color = "primary"
      )
    })
    
    output$dna_recency <- renderValueBox({
      bs4Dash::valueBox(
        value = "15",
        subtitle = "Days Since Last Purchase",
        icon = icon("clock"),
        color = "info"
      )
    })
    
    output$dna_frequency <- renderValueBox({
      bs4Dash::valueBox(
        value = "12",
        subtitle = "Purchase Frequency",
        icon = icon("shopping-cart"),
        color = "success"
      )
    })
    
    output$dna_monetary <- renderValueBox({
      bs4Dash::valueBox(
        value = "$1,250",
        subtitle = "Average Purchase",
        icon = icon("dollar-sign"),
        color = "warning"
      )
    })
    
    output$dna_cai <- renderValueBox({
      bs4Dash::valueBox(
        value = "0.85",
        subtitle = "Customer Activity Index",
        icon = icon("chart-line"),
        color = "primary"
      )
    })
    
    output$dna_ipt <- renderValueBox({
      bs4Dash::valueBox(
        value = "45",
        subtitle = "Average Days Between Purchases",
        icon = icon("calendar-alt"),
        color = "info"
      )
    })
    
    output$dna_pcv <- renderValueBox({
      bs4Dash::valueBox(
        value = "$15,000",
        subtitle = "Past Customer Value",
        icon = icon("history"),
        color = "success"
      )
    })
    
    output$dna_clv <- renderValueBox({
      bs4Dash::valueBox(
        value = "$42,500",
        subtitle = "Customer Lifetime Value",
        icon = icon("user-tag"),
        color = "warning"
      )
    })
    
    output$dna_cri <- renderValueBox({
      bs4Dash::valueBox(
        value = "0.79",
        subtitle = "Customer Reliability Index",
        icon = icon("handshake"),
        color = "primary"
      )
    })
    
    output$dna_nes <- renderValueBox({
      bs4Dash::valueBox(
        value = "E2",
        subtitle = "Customer Status",
        icon = icon("star"),
        color = "info"
      )
    })
    
    output$dna_nt <- renderValueBox({
      bs4Dash::valueBox(
        value = "$750",
        subtitle = "New Customer Average Value",
        icon = icon("user-plus"),
        color = "success"
      )
    })
    
    output$dna_e0t <- renderValueBox({
      bs4Dash::valueBox(
        value = "$1,500",
        subtitle = "Core Customer Average Value",
        icon = icon("users"),
        color = "warning"
      )
    })
    
    # Return reactive expression with filtered data
    return(reactive({
      list(
        customer_id = if (!is.null(selected_customer())) selected_customer()$customer_id[1] else NULL,
        date_range = input$date_range
      )
    }))
  })
}