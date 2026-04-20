# @principle MP55 Computation Allocation
# @principle MP56 Connected Component Principle
# @principle R91 Universal Data Access Pattern
# @principle P22 CSS Component Display Controls

#' Union of UI Filter Components
#'
#' Creates a dynamic union of UI filter components that can be shown/hidden based on
#' application state. Designed to glue different UI filters into a unified interface.
#'
#' @param id The module ID
#' @param ... Named UI filter components to include in the union
#' @param initial_visibility Named logical vector indicating which components should be initially visible
#' @param container_style Container styling attributes (CSS)
#' @param container_type Type of container to use ('sidebar', 'card', 'box', 'div', etc.)
#' @param title Optional title for the container
#' @param common_filters Optional list of filter components that should always be visible
#' @return A UI component representing the mathematical union of all filter inputs
#' @export
UnionFiltersUI <- function(id, 
                         ..., 
                         initial_visibility = NULL,
                         container_style = NULL,
                         container_type = "div",
                         title = NULL,
                         common_filters = NULL) {
  # Create namespace function
  ns <- NS(id)
  
  # Get the named components
  components <- list(...)
  component_names <- names(components)
  
  # If no names provided, generate them
  if (is.null(component_names) || any(component_names == "")) {
    missing_names <- which(component_names == "" | is.null(component_names))
    component_names[missing_names] <- paste0("filter_", missing_names)
    names(components) <- component_names
  }
  
  # Set default initial visibility if not provided
  if (is.null(initial_visibility)) {
    initial_visibility <- setNames(
      c(TRUE, rep(FALSE, length(components) - 1)),
      component_names
    )
  }
  
  # Process filter components to add IDs and set initial visibility
  processed_components <- lapply(component_names, function(name) {
    component <- components[[name]]
    
    # Determine initial visibility - handle case when name is not in initial_visibility
    is_visible <- if (name %in% names(initial_visibility)) {
      initial_visibility[[name]]
    } else {
      # Default to visible for first component, hidden for others
      name == component_names[1]
    }
    
    display_style <- if (is_visible) "block" else "none"
    
    # Create a container div with ID and initial visibility
    div(
      id = ns(paste0("filter_", name)),
      class = "filter-component",
      style = paste0("display: ", display_style, "; margin-bottom: 15px; border-bottom: 1px dashed #eee; padding-bottom: 10px;"),
      component
    )
  })
  
  # Default container style if not provided
  if (is.null(container_style)) {
    container_style <- "padding: 15px; border: 1px solid #ddd; border-radius: 5px; background-color: #f9f9f9;"
  }
  
  # Create the containing element based on container_type
  container_function <- switch(container_type,
    "sidebar" = function(...) bs4Dash::dashboardSidebar(
      fixed = TRUE,
      skin = "light",
      status = "primary",
      elevation = 3,
      ...
    ),
    "card" = function(...) bs4Dash::box(
      title = NULL,
      width = 12,
      ...
    ),
    "box" = function(...) bs4Dash::box(width = 12, ...),
    div
  )
  
  # Build title element if provided
  title_element <- if (!is.null(title)) {
    div(
      class = "filters-header",
      style = "padding-bottom: 15px; border-bottom: 1px solid rgba(0, 0, 0, 0.1); margin-bottom: 15px;",
      h3(
        style = "margin: 0; font-size: 18px; font-weight: 600;",
        title
      )
    )
  }
  
  # Create the common filters section if provided
  common_filters_element <- if (!is.null(common_filters)) {
    div(
      id = ns("common_filters"),
      class = "common-filters",
      style = "padding: 10px 0; margin-bottom: 15px; border-bottom: 1px solid rgba(0, 0, 0, 0.1);",
      common_filters
    )
  }
  
  # Create the containing union container with all components
  union_container <- container_function(
    id = ns("filters_union_container"),
    class = "filters-union-container",
    style = container_style,
    
    # Include mathematical notation in comments for clarity
    HTML("<!-- Mathematical Union (∪) of UI Filter Components -->"),
    HTML(paste0("<!-- Union created ONCE at initialization: U = ", 
                paste(component_names, collapse = " ∪ "), " -->")),
    
    # Add title if provided
    if (!is.null(title)) title_element,
    
    # Add common filters if provided
    if (!is.null(common_filters)) common_filters_element,
    
    # Add all component filters
    div(
      id = ns("filter_components"),
      class = "filter-components",
      processed_components
    ),
    
    # Include minimal CSS to ensure proper display
    tags$style(HTML(paste0("
      #", ns("filters_union_container"), " .filter-component {
        transition: opacity 0.2s ease;
      }
    ")))
  )
  
  return(union_container)
}

#' Server logic for component unions
#'
#' Controls visibility of filter components using efficient CSS toggles
#' rather than rebuilding DOM elements.
#'
#' @param id The union component ID
#' @param visibility_conditions Named list of reactive expressions that determine component visibility
#' @param auto_hide Should other filters be automatically hidden when one is shown? (default: TRUE)
#' @return A reactive expression with the currently visible components
#' @export
ServerUnion <- function(id, visibility_conditions, auto_hide = TRUE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Track currently visible components
    visible_components <- reactiveVal(character(0))
    
    # For each component, observe its visibility condition
    observe({
      currently_visible <- character(0)
      
      # Loop through each visibility condition
      for (name in names(visibility_conditions)) {
        # Get visibility for this component
        tryCatch({
          is_visible <- visibility_conditions[[name]]()
          
          # Component ID
          component_id <- paste0("filter_", name)
          
          # If auto_hide is TRUE and at least one component is visible,
          # hide all other components
          if (auto_hide && is_visible) {
            # Hide all components
            for (other_name in names(visibility_conditions)) {
              other_component_id <- paste0("filter_", other_name)
              shinyjs::hide(id = other_component_id, anim = TRUE)
            }
            # Then show only this one
            shinyjs::show(id = component_id, anim = TRUE)
            currently_visible <- c(currently_visible, name)
            break
          } else {
            # Normal behavior without auto_hide
            if (is_visible) {
              shinyjs::show(id = component_id, anim = TRUE)
              currently_visible <- c(currently_visible, name)
            } else {
              shinyjs::hide(id = component_id, anim = TRUE)
            }
          }
        }, error = function(e) {
          # Log errors but continue processing other components
          cat("Error in visibility condition for", name, ":", conditionMessage(e), "\n")
        })
      }
      
      # Update the reactive value with currently visible components
      visible_components(currently_visible)
    })
    
    # Manual toggle function to show/hide specific components
    toggleComponent <- function(name, show = TRUE) {
      if (!(name %in% names(visibility_conditions))) {
        return(FALSE)
      }
      
      component_id <- paste0("filter_", name)
      
      if (show) {
        if (auto_hide) {
          # Hide all first
          for (other_name in names(visibility_conditions)) {
            other_component_id <- paste0("filter_", other_name)
            shinyjs::hide(id = other_component_id, anim = TRUE)
          }
        }
        # Show the component
        shinyjs::show(id = component_id, anim = TRUE)
        
        # Update tracking
        current <- visible_components()
        if (!(name %in% current)) {
          visible_components(c(current, name))
        }
      } else {
        # Hide component
        shinyjs::hide(id = component_id, anim = TRUE)
        
        # Update tracking
        current <- visible_components()
        visible_components(setdiff(current, name))
      }
      
      return(TRUE)
    }
    
    # Return as a list with reactive expression and toggle function
    list(
      visible_components = visible_components,
      toggle_component = toggleComponent
    )
  })
}

#' Initialize a Union of UI Components
#'
#' Conveniently sets up a union component with the provided UI elements.
#'
#' @param id The module ID
#' @param filters Named list of filter UI components
#' @param initial_tab Initial tab/component to show
#' @param container_type Type of container ('sidebar', 'card', 'box', 'div')
#' @param container_style CSS styling for the container
#' @param title Container title
#' @param common_filters Components that should always be visible
#' @return A list with UI and server components
#' @export
#' @examples
#' # Create some filter components
#' date_filter <- dateRangeInput("date_range", "Select Date Range:", 
#'   start = Sys.Date() - 30, end = Sys.Date())
#'   
#' region_filter <- selectInput("region", "Select Region:", 
#'   choices = c("All", "North", "South", "East", "West"))
#'
#' # Initialize union
#' filters_union <- UnionComponentsInitialize(
#'   id = "sidebar_filters",
#'   filters = list(
#'     date = date_filter,
#'     region = region_filter
#'   ),
#'   container_type = "sidebar",
#'   title = "Analysis Filters"
#' )
#' 
#' # In UI
#' ui <- dashboardPage(
#'   filters_union$ui,
#'   dashboardBody(...)
#' )
#' 
#' # In server
#' server <- function(input, output, session) {
#'   filters_union$server(input, output, session)
#' }
UnionComponentsInitialize <- function(id, 
                                   filters, 
                                   initial_tab = NULL,
                                   container_type = "div",
                                   container_style = NULL,
                                   title = "Filters",
                                   common_filters = NULL) {
  # Set default initial visibility
  filter_names <- names(filters)
  if (is.null(initial_tab)) {
    initial_tab <- filter_names[1]  # Default to first filter
  }
  
  initial_visibility <- setNames(
    sapply(filter_names, function(name) name == initial_tab),
    filter_names
  )
  
  # Create the UI
  ui <- UnionFiltersUI(
    id = id,
    ... = filters,
    initial_visibility = initial_visibility,
    container_type = container_type,
    container_style = container_style,
    title = title,
    common_filters = common_filters
  )
  
  # Create a server function
  server_fn <- function(input, output, session) {
    # Default visibility based on the tabPanel being selected
    visibility_conditions <- setNames(
      lapply(filter_names, function(name) {
        # Returns a reactive expression
        reactive({ TRUE })  # Initially all could be visible
      }),
      filter_names
    )
    
    # Initialize the server
    union_state <- ServerUnion(id, visibility_conditions, auto_hide = TRUE)
    
    # Make the union state available
    return(union_state)
  }
  
  # Return both UI and server components
  return(list(
    ui = ui,
    server = server_fn
  ))
}