# Example of using unionUI for sidebar filters
# Following NSQL set theory and P22 CSS controls

#' Create Filter Components for Example
#'
#' Helper function to create filter components for the example
#'
#' @param id Namespace ID
#' @return List of filter components
createFilterComponents <- function(id) {
  ns <- NS(id)
  
  # Common filters component
  common_filters <- div(
    id = ns("common_filters_inner"),
    class = "common-filters",
    style = "padding: 10px 15px; margin-bottom: 10px; border-bottom: 1px dashed #eee;",
    
    h4(translate("Marketing Channel"), style = "color: #007bff;"),
    radioButtons(
      ns("common_channel"), 
      translate("Select Channel:"), 
      choices = c("Amazon" = "amazon", "Official Website" = "officialwebsite"),
      selected = "amazon"
    ),
    
    h4(translate("Product Category"), style = "color: #007bff;"),
    selectInput(
      ns("common_category"), 
      translate("Select Category:"), 
      choices = c("All Products" = "000", "Kitchen" = "001", "Home" = "002", "Electronics" = "003"),
      selected = "000"
    )
  )
  
  # Micro tab filters
  micro_filters <- div(
    id = ns("micro_filters_inner"),
    class = "micro-filters",
    style = "padding: 10px 15px;",
    
    tags$div(
      tags$strong(paste0(translate("Micro Analysis Filters:"), " "), style = "color: #007bff; font-size: 14px;"),
      style = "margin-bottom: 10px;"
    ),
    
    h4(translate("Region"), style = "color: #007bff;"),
    selectInput(
      ns("micro_region"), 
      translate("Select Region:"), 
      choices = c("All Regions" = "000", "North America" = "001", "Europe" = "002", "Asia" = "003"),
      selected = "000"
    ),
    
    helpText(translate("Filters apply automatically"), 
             style = "font-style: italic; color: #6c757d; margin-top: 15px; text-align: center;")
  )
  
  # Macro tab filters
  macro_filters <- div(
    id = ns("macro_filters_inner"),
    class = "macro-filters",
    style = "padding: 10px 15px;",
    
    tags$div(
      tags$strong(paste0(translate("Macro Analysis Filters:"), " "), style = "color: #28a745; font-size: 14px;"),
      style = "margin-bottom: 10px;"
    ),
    
    h4(translate("Aggregation"), style = "color: #28a745;"),
    selectInput(
      ns("macro_aggregation"), 
      translate("Aggregation Level:"), 
      choices = c("Product Category", "Region", "Channel", "Customer Segment"),
      selected = "Product Category"
    ),
    
    h4(translate("Comparison"), style = "color: #28a745;"),
    checkboxInput(
      ns("macro_comparison"),
      translate("Enable Comparison"),
      value = FALSE
    ),
    
    h4(translate("Time Range"), style = "color: #28a745;"),
    dateRangeInput(
      ns("macro_daterange"),
      translate("Select Period:"),
      start = Sys.Date() - 90,
      end = Sys.Date()
    ),
    
    helpText(translate("Changes apply automatically"), 
             style = "font-style: italic; color: #6c757d; margin-top: 15px; text-align: center;")
  )
  
  # Target tab filters
  target_filters <- div(
    id = ns("target_filters_inner"),
    class = "target-filters",
    style = "padding: 10px 15px;",
    
    tags$div(
      tags$strong(paste0(translate("Target Marketing Filters:"), " "), style = "color: #dc3545; font-size: 14px;"),
      style = "margin-bottom: 10px;"
    ),
    
    h4(translate("Campaign"), style = "color: #dc3545;"),
    selectInput(
      ns("target_campaign"), 
      translate("Select Campaign:"), 
      choices = c("Summer Promotion", "New Product Launch", "Black Friday"),
      selected = "Summer Promotion"
    ),
    
    h4(translate("Target Audience"), style = "color: #dc3545;"),
    checkboxGroupInput(
      ns("target_audience"),
      translate("Select Segments:"),
      choices = c("New Customers", "Returning Customers", "VIP"),
      selected = "New Customers"
    ),
    
    helpText(translate("Filters apply instantly"), 
             style = "font-style: italic; color: #6c757d; margin-top: 15px; text-align: center;")
  )
  
  return(list(
    common = common_filters,
    micro = micro_filters,
    macro = macro_filters,
    target = target_filters
  ))
}

#' Sidebar Union Example Using Mathematical Union
#'
#' Example of how to use the unionUI component for sidebar filters
#' Creates a mathematical union of filter components that is evaluated once at startup
#' Then controls visibility via CSS per P22
#'
#' @param id The module ID
#' @return A dashboardSidebar component with unified filter sections
#' @export
sidebarUnionExampleUI <- function(id) {
  ns <- NS(id)
  
  # Get filter components
  filters <- createFilterComponents(id)
  
  # Create the sidebar with a header and unionUI for filters
  bs4Dash::dashboardSidebar(
    fixed = TRUE,
    skin = "light",
    status = "primary",
    elevation = 3,
    
    # Sidebar header
    div(
      class = "filters-header",
      style = "padding: 15px; border-bottom: 1px solid rgba(60, 141, 188, 0.2); margin-bottom: 15px;",
      h3(
        style = "margin: 0; font-size: 18px; font-weight: 600;",
        translate("Application Settings")
      )
    ),
    
    # Create mathematical union of all filter components
    # This creates all components ONCE at startup, then controls visibility with CSS
    unionUI(
      common = filters$common,
      micro = filters$micro,
      macro = filters$macro,
      target = filters$target,
      id = ns("filter_union"),
      initial_visibility = list(
        common = TRUE,    # Common filters always visible
        micro = TRUE,     # Start with micro tab active
        macro = FALSE,
        target = FALSE
      )
    )
  )
}

#' Server function for Sidebar Union Example
#'
#' @param id Module ID
#' @param active_tab Reactive expression returning the active tab
#' @return Reactive expression with filter values
#' @export
sidebarUnionExampleServer <- function(id, active_tab) {
  moduleServer(id, function(input, output, session) {
    # Control union component visibility based on active tab
    visible_components <- unionServer(
      "filter_union",
      visibility_conditions = list(
        common = reactive(TRUE),  # Common filters always visible
        micro = reactive(active_tab() == "micro_tab"),
        macro = reactive(active_tab() == "macro_tab"),
        target = reactive(active_tab() == "target_tab")
      )
    )
    
    # Return reactive values for all filter inputs
    return(reactive({
      # Common filters
      common_filters <- list(
        channel = input$common_channel,
        category = input$common_category
      )
      
      # Tab-specific filters
      tab_filters <- list(
        micro = list(
          region = input$micro_region
        ),
        macro = list(
          aggregation = input$macro_aggregation,
          comparison = input$macro_comparison,
          date_range = input$macro_daterange
        ),
        target = list(
          campaign = input$target_campaign,
          audience = input$target_audience
        )
      )
      
      # Return union of common and tab-specific filters
      # This matches the mathematical concept: Filters = CommonFilters ∪ TabFilters[ActiveTab]
      list(
        common = common_filters,
        tab_specific = tab_filters,
        visible_components = visible_components()
      )
    }))
  })
}

#' Usage Example - How to use in app.R
if (FALSE) {
  # UI Definition
  ui <- bs4Dash::dashboardPage(
    header = bs4Dash::dashboardHeader(title = "NSQL Union Example"),
    sidebar = sidebarUnionExampleUI("sidebar"),
    body = bs4Dash::dashboardBody(
      # Tab navigation buttons
      div(
        actionButton("micro_tab", "Micro Analysis", class = "tab-button active"),
        actionButton("macro_tab", "Macro Analysis", class = "tab-button"),
        actionButton("target_tab", "Target Marketing", class = "tab-button")
      ),
      
      # Content sections would be defined here
      div(id = "micro_content", "Micro content here"),
      div(id = "macro_content", style = "display: none;", "Macro content here"),
      div(id = "target_content", style = "display: none;", "Target content here")
    )
  )
  
  # Server function
  server <- function(input, output, session) {
    # Track active tab
    active_tab <- reactiveVal("micro_tab")
    
    # Tab button observers
    observeEvent(input$micro_tab, { active_tab("micro_tab") })
    observeEvent(input$macro_tab, { active_tab("macro_tab") })
    observeEvent(input$target_tab, { active_tab("target_tab") })
    
    # Initialize sidebar with active tab
    filters <- sidebarUnionExampleServer("sidebar", active_tab)
    
    # Use filter values in the app
    observe({
      filter_values <- filters()
      str(filter_values)  # For debugging
    })
  }
  
  # Run the app
  shinyApp(ui, server)
}