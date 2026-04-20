# Mathematical Union Component
# Following NSQL set theory principles and P22 CSS controls

#' Create a UI Component Union (Static Set Union at App Initialization)
#'
#' Creates a static set-theoretical union of UI components during app initialization.
#' The union is created ONCE at startup, with all possible components included in the DOM.
#' Visibility is controlled by CSS to avoid expensive DOM rebuilding.
#'
#' @param ... Named UI components to include in the union
#' @param id Namespace ID for the union
#' @param initial_visibility Named logical vector indicating which components should be initially visible
#' @return A UI component representing the mathematical union of all inputs
#' @export
unionUI <- function(..., id = NULL, initial_visibility = NULL) {
  # Create namespace function
  ns <- if (!is.null(id)) NS(id) else function(x) x
  
  # Get the named components
  components <- list(...)
  component_names <- names(components)
  
  # If no names provided, generate them
  if (is.null(component_names) || any(component_names == "")) {
    missing_names <- which(component_names == "" | is.null(component_names))
    component_names[missing_names] <- paste0("component_", missing_names)
    names(components) <- component_names
  }
  
  # Set default initial visibility if not provided
  if (is.null(initial_visibility)) {
    initial_visibility <- setNames(
      c(TRUE, rep(FALSE, length(components) - 1)),
      component_names
    )
  }
  
  # Process components to add IDs and set initial visibility
  processed_components <- lapply(component_names, function(name) {
    component <- components[[name]]
    
    # Determine initial visibility
    is_visible <- initial_visibility[[name]]
    display_style <- if (is_visible) "block" else "none"
    
    # Create a container div with ID and initial visibility
    div(
      id = ns(paste0("union_", name)),
      class = "union-component",
      style = paste0("display: ", display_style, ";"),
      component
    )
  })
  
  # Create the containing div that holds the entire union
  union_container <- div(
    id = ns("union_container"),
    class = "union-container",
    # Include mathematical notation in comments for clarity
    HTML("<!-- Mathematical Union (∪) of UI Components -->"),
    HTML(paste0("<!-- Union created ONCE at initialization: U = ", 
                paste(component_names, collapse = " ∪ "), " -->")),
    
    # Add all components to the DOM at initialization
    processed_components,
    
    # Include minimal CSS to ensure proper display
    tags$style(HTML(paste0("
      #", ns("union_container"), " {
        position: relative;
      }
      
      #", ns("union_container"), " .union-component {
        transition: opacity 0.2s ease;
      }
    ")))
  )
  
  return(union_container)
}

#' Server logic for UnionUI
#'
#' Controls visibility of union components using efficient CSS toggles
#' rather than rebuilding DOM elements.
#'
#' @param id The unionUI ID
#' @param visibility_conditions Named list of reactive expressions that determine component visibility
#' @return A reactive expression with the currently visible components
#' @export
unionServer <- function(id, visibility_conditions) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Track currently visible components
    visible_components <- reactiveVal(character(0))
    
    # For each component, observe its visibility condition
    observe({
      currently_visible <- character(0)
      
      for (name in names(visibility_conditions)) {
        # Get visibility for this component
        is_visible <- visibility_conditions[[name]]()
        
        # Use shinyjs to show/hide based on the condition
        component_id <- paste0("union_", name)
        if (is_visible) {
          shinyjs::show(id = component_id)
          currently_visible <- c(currently_visible, name)
        } else {
          shinyjs::hide(id = component_id)
        }
      }
      
      # Update the reactive value with currently visible components
      visible_components(currently_visible)
    })
    
    # Return the currently visible components
    return(visible_components)
  })
}