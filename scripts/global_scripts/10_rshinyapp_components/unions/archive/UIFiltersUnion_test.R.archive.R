# Example demonstrating the UIFiltersUnion component
# @principle MP56 Connected Component Principle

# Load required libraries
library(shiny)
library(bs4Dash)
library(shinyjs)

# Source the UIFiltersUnion component
source("update_scripts/global_scripts/10_rshinyapp_components/unions/ComponentsUnion.R")

# Demo app to showcase the UIFiltersUnion component
UIFiltersUnionExample <- function() {
  
  # Create various filter components to be combined
  # These could come from any source, including other Shiny modules
  
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
    ),
    checkboxGroupInput(
      "sub_regions",
      "Sub-regions:",
      choices = c("East", "West", "North", "South"),
      selected = character(0)
    )
  )
  
  # Product Filter
  product_filter <- div(
    h4("Product Filter", style = "color: #3c8dbc;"),
    selectInput(
      "product_category", 
      "Product Category:", 
      choices = c("All Categories" = "all", "Electronics" = "elec", "Clothing" = "cloth", "Food" = "food"),
      selected = "all"
    ),
    selectizeInput(
      "product_id",
      "Product ID:",
      choices = NULL,
      options = list(
        placeholder = "Type to search products...",
        onInitialize = I('function() { this.setValue(""); }')
      )
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
    id = "demo_filters",
    filters = list(
      date = date_filter,
      region = region_filter,
      product = product_filter
    ),
    initial_tab = "date",
    container_type = "sidebar",
    container_style = NULL,  # Use default
    title = "Analysis Filters",
    common_filters = common_filters
  )
  
  # Create the UI
  ui <- bs4Dash::dashboardPage(
    title = "UIFiltersUnion Example",
    dark = FALSE,
    
    # Header
    header = bs4Dash::dashboardHeader(
      title = bs4Dash::dashboardBrand(
        title = "UIFiltersUnion Demo",
        color = "primary"
      )
    ),
    
    # Sidebar with filter union
    sidebar = filters_union$ui,
    
    # Body content
    body = bs4Dash::dashboardBody(
      shinyjs::useShinyjs(),
      
      # Buttons to control filter visibility
      fluidRow(
        column(
          width = 12,
          box(
            title = "Filter Controls",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
            
            fluidRow(
              column(
                width = 4,
                actionButton("show_date", "Show Date Filter", class = "btn-block btn-primary")
              ),
              column(
                width = 4,
                actionButton("show_region", "Show Region Filter", class = "btn-block btn-info")
              ),
              column(
                width = 4,
                actionButton("show_product", "Show Product Filter", class = "btn-block btn-success")
              )
            )
          )
        )
      ),
      
      # Display current selections
      fluidRow(
        column(
          width = 12,
          box(
            title = "Current Selections",
            width = 12,
            status = "primary",
            
            # Display selections from each filter
            fluidRow(
              column(
                width = 4,
                h4("Date Selections"),
                verbatimTextOutput("date_selection")
              ),
              column(
                width = 4,
                h4("Region Selections"),
                verbatimTextOutput("region_selection")
              ),
              column(
                width = 4,
                h4("Product Selections"),
                verbatimTextOutput("product_selection")
              )
            ),
            
            # Show which filters are currently visible
            fluidRow(
              column(
                width = 12,
                h4("Visible Filters"),
                verbatimTextOutput("visible_filters")
              )
            )
          )
        )
      )
    )
  )
  
  # Server logic
  server <- function(input, output, session) {
    # Initialize the UIFiltersUnion server
    union_state <- filters_union$server(input, output, session)
    
    # Handle button clicks to toggle filters
    observeEvent(input$show_date, {
      union_state$toggle_component("date", show = TRUE)
    })
    
    observeEvent(input$show_region, {
      union_state$toggle_component("region", show = TRUE)
    })
    
    observeEvent(input$show_product, {
      union_state$toggle_component("product", show = TRUE)
    })
    
    # Display current filter selections
    output$date_selection <- renderPrint({
      list(
        date_range = c(input$date_range[1], input$date_range[2]),
        compare_previous = input$compare_previous
      )
    })
    
    output$region_selection <- renderPrint({
      list(
        region = input$region,
        sub_regions = input$sub_regions
      )
    })
    
    output$product_selection <- renderPrint({
      list(
        product_category = input$product_category,
        product_id = input$product_id
      )
    })
    
    # Display which filters are currently visible
    output$visible_filters <- renderPrint({
      union_state$visible_components()
    })
    
    # Simulate product ID options based on category
    observe({
      category <- input$product_category
      
      # Generate different product IDs based on selected category
      products <- switch(category,
        "all" = 1:10,
        "elec" = 101:110,
        "cloth" = 201:210,
        "food" = 301:310,
        1:10  # default
      )
      
      # Format product IDs with names
      product_choices <- setNames(
        as.character(products),
        paste0("Product ", products)
      )
      
      # Update the selectize input
      updateSelectizeInput(
        session,
        "product_id",
        choices = product_choices,
        server = TRUE
      )
    })
  }
  
  # Run the app
  shinyApp(ui, server)
}

# Execute the example application when this script is sourced
if (interactive()) {
  UIFiltersUnionExample()
}