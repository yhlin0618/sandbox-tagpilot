# Default configurations for Union components
# Following NSQL set theory principles

#' Default Configuration for Union Components
#'
#' Provides default values for union component settings
#'
#' @return A list of default configurations for union components
#' @export
unionDefaults <- function() {
  list(
    # Default transition speed for showing/hiding components
    transition_speed_ms = 200,
    
    # Default style options
    styles = list(
      container = list(
        position = "relative",
        overflow = "hidden"
      ),
      component = list(
        transition = "opacity 0.2s ease"
      )
    ),
    
    # Default visibility behavior
    visibility = list(
      # Show first component by default
      show_first = TRUE,
      
      # Default easing function for transitions
      easing = "ease",
      
      # Strategies for handling multiple visible components
      overlap_strategy = "stack" # Options: stack, hide_previous, show_all
    )
  )
}

#' Default Union Configuration for Sidebar Use Case
#'
#' Specialized defaults for sidebar union components
#'
#' @return A list of default configurations for sidebar unions
#' @export
sidebarUnionDefaults <- function() {
  # Start with general defaults
  defaults <- unionDefaults()
  
  # Override with sidebar-specific settings
  defaults$styles$container <- list(
    position = "relative",
    padding = "0",
    overflow = "visible"
  )
  
  defaults$visibility$overlap_strategy <- "hide_previous"
  
  # Return modified defaults
  return(defaults)
}

#' Default Union Configuration for Content Use Case
#'
#' Specialized defaults for content area union components
#'
#' @return A list of default configurations for content unions
#' @export
contentUnionDefaults <- function() {
  # Start with general defaults
  defaults <- unionDefaults()
  
  # Override with content-specific settings
  defaults$styles$container <- list(
    position = "relative",
    padding = "10px",
    min_height = "300px"
  )
  
  defaults$styles$component <- list(
    transition = "opacity 0.3s ease, transform 0.3s ease",
    transform = "translateY(0)"
  )
  
  defaults$transition_speed_ms <- 300
  
  # Return modified defaults
  return(defaults)
}