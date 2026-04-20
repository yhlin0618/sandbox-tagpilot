#' @principle MP56 Connected Component Principle
#' @principle MP52 Unidirectional Data Flow
#' @principle MP54 UI-Server Correspondence
#' @principle R88 Shiny Module ID Handling
#' @principle R91 Universal Data Access Pattern
#' @principle P81 Tidyverse-Shiny Terminology Alignment
#' @principle P62 Separation of Concerns
#' @principle R09 UI-Server-Defaults Triple Rule
#' @principle MP17 Modularity Principle
#' @principle MP93 Global Accessibility Principle
#' @principle P102 Defensive Error Handling Principle
#' @principle R19 Component Visibility Guarantee
#' @principle MP107 Root Cause Resolution Principle

####Union####

#' Union Component Factory
#'
#' Creates a union of multiple components, combining their UI and server parts
#' into a single consolidated component. This allows for modular composition
#' of complex UI and server logic from simpler building blocks.
#'
#' Following the UI-Server-Defaults Triple Rule (R09), this function maintains
#' separation between UI, server, and defaults parts of components while
#' combining them into a unified interface.
#'
#' @param id The base ID for the union component
#' @param ... Named components to include in the union.
#'   Each component should be a list with at least ui$filter, ui$display, and server elements,
#'   and optionally a defaults function.
#' @param config Optional configuration parameters for the union
#' @param global Whether to make the union globally accessible (default: TRUE)
#'
#' @return A component with combined parts from all input components
#' @export
Union <- function(id, ..., config = NULL, global = TRUE) {
  # Collect all components
  components <- list(...)
  
  # Check if components list is named
  if (is.null(names(components)) || any(names(components) == "")) {
    stop("All components must be named in the Union function call")
  }
  
  # Initialize containers for UI parts
  filters <- list()
  displays <- list()
  servers <- list()
  defaults <- list()
  components_meta <- list()
  
  # Extract parts from each component
  for (comp_name in names(components)) {
    component <- components[[comp_name]]
    
    # Validate component structure following R09 UI-Server-Defaults Triple Rule
    if (!is.list(component) || 
        !all(c("ui", "server") %in% names(component)) ||
        !all(c("filter", "display") %in% names(component$ui))) {
      stop(paste("Component", comp_name, "does not have the required structure (ui.filter, ui.display, server)"))
    }
    
    # Check for defaults function (optional but recommended)
    if (!"defaults" %in% names(component)) {
      warning(paste("Component", comp_name, "does not have a defaults function. This is not required but recommended for consistency."))
    } else if (!is.function(component$defaults)) {
      stop(paste("Component", comp_name, "has a defaults element that is not a function. It must be a function that returns default values."))
    }
    
    # Store component parts with appropriate namespacing
    comp_id <- paste0(id, "_", comp_name)
    
    # Store UI elements with their component names for identification
    filters[[comp_name]] <- list(
      id = comp_id,
      ui = component$ui$filter,
      name = comp_name
    )
    
    displays[[comp_name]] <- list(
      id = comp_id,
      ui = component$ui$display,
      name = comp_name
    )
    
    # Store server function
    servers[[comp_name]] <- list(
      id = comp_id,
      server_fn = component$server,
      name = comp_name
    )
    
    # Store defaults if available
    if ("defaults" %in% names(component)) {
      defaults[[comp_name]] <- component$defaults
    }
    
    # Store metadata
    components_meta[[comp_name]] <- list(
      id = comp_id,
      original_id = if ("original_id" %in% names(component)) component$original_id else NULL,
      active = TRUE
    )
  }
  
####makeComponentState####
  
#' Create component state manager for visibility control
#'
#' Creates a reactive value that tracks which components are visible
#' and provides methods to toggle visibility.
#'
#' @param components_meta List of component metadata with names
#' @param config Configuration for initial visibility
#'
#' @return A list with state reactive values and methods
#' @noRd
makeComponentState <- function(components_meta, config = NULL) {
  # Create a reactive value that can be used to store component visibility
  state <- shiny::reactiveValues()
  
  # Keep track of component visibility in a non-reactive structure for diagnostics
  visibility_state <- list()
  
  # Add all components to the state with their initial visibility
  for (comp_name in names(components_meta)) {
    # Initialize component as visible if no config specifies otherwise
    is_visible <- TRUE
    if (!is.null(config) && !is.null(config$initial_visibility) && 
        comp_name %in% names(config$initial_visibility)) {
      is_visible <- config$initial_visibility[[comp_name]]
    }
    
    # Set in reactive state
    state[[comp_name]] <- is_visible
    
    # Also set in non-reactive tracking state
    visibility_state[[comp_name]] <- is_visible
    
    # Log initial visibility for debugging
    message("Component '", comp_name, "' initial visibility: ", is_visible)
  }
  
  # Add a function to toggle component visibility
  toggle_component <- function(comp_name, show = NULL) {
    # Detailed logging for debugging
    message("Toggling component '", comp_name, "' visibility: ", 
           if(is.null(show)) "TOGGLE" else if(show) "SHOW" else "HIDE")
    
    # Check if component exists
    if (!comp_name %in% names(state)) {
      warning(paste("Component", comp_name, "does not exist in this union"))
      message("ERROR: Component '", comp_name, "' not found in state. Available components: ", 
             paste(names(state), collapse=", "))
      return(FALSE)
    }
    
    # Log current state before change
    message("Current state before toggle: ", state[[comp_name]])
    
    # If show is NULL, toggle current state
    if (is.null(show)) {
      new_state <- !state[[comp_name]]
      state[[comp_name]] <- new_state
      visibility_state[[comp_name]] <- new_state
    } else {
      # Otherwise set to requested state
      new_state <- as.logical(show)
      state[[comp_name]] <- new_state
      visibility_state[[comp_name]] <- new_state
    }
    
    # Log new state after change
    message("New state after toggle: ", state[[comp_name]])
    
    return(TRUE)
  }
  
  # Create a list with the reactive values and methods
  component_state <- list(
    state = state,
    visibility_state = visibility_state,
    toggle_component = toggle_component,
    is_visible = function(comp_name) {
      if (!comp_name %in% names(state)) {
        message("Component '", comp_name, "' not found in visibility state")
        return(FALSE)
      }
      visibility <- state[[comp_name]]
      message("Checking visibility for '", comp_name, "': ", visibility)
      return(visibility)
    },
    get_visible_components = function() {
      visible <- c()
      for (name in names(state)) {
        current_state <- state[[name]]
        if (!is.null(current_state) && current_state) {
          visible <- c(visible, name)
        }
      }
      message("Currently visible components: ", paste(visible, collapse=", "))
      return(visible)
    },
    debug_visibility = function() {
      # Return a non-reactive summary for debugging
      result <- list()
      for (name in names(visibility_state)) {
        result[[name]] <- visibility_state[[name]]
      }
      message("Current visibility state (non-reactive): ", 
             paste(names(result)[unlist(result)], collapse=", "))
      return(result)
    }
  )
  
  # Immediately log initial setup state
  message("Component state initialized with ", length(names(components_meta)), " components")
  message("Visibility configuration: ", paste(names(visibility_state)[unlist(visibility_state)], collapse=", "))
  
  return(component_state)
}
  

####unionFilterUI####  
  
#' Create the union filter UI
#'
#' Creates a combined UI element containing all filter components
#' with proper namespacing and visibility controls.
#'
#' @param id ID for the UI element
#' @param filters List of filter UI elements from components
#'
#' @return A div containing all filter UI elements
#' @noRd
unionFilterUI <- function(id, filters) {
  ns <- NS(id)
  
  # Create a div to contain all filters
  filter_container <- div(
    id = ns("filter_container"),
    class = "union-filter-container",
    
    # Add filters from each component
    lapply(names(filters), function(comp_name) {
      filter_info <- filters[[comp_name]]
      filter_id <- filter_info$id
      filter_ui <- filter_info$ui
      
      # Add debug message for filter rendering
      message("Rendering filter UI for component: ", comp_name, " with ID: ", filter_id)
      
      # Apply function if it's a function, or use as is if it's already a UI element
      filter_element <- tryCatch({
        if (is.function(filter_ui)) {
          message("Filter UI for ", comp_name, " is a function - applying with ID: ", filter_id)
          filter_ui(filter_id)
        } else {
          message("Filter UI for ", comp_name, " is not a function - using directly")
          filter_ui
        }
      }, error = function(e) {
        message("ERROR rendering filter for component ", comp_name, ": ", e$message)
        # Return a placeholder to avoid breaking the UI
        div(class = "filter-error", 
            p(paste("Error in filter for", comp_name)))
      })
      
      # Log successful filter rendering
      message("Successfully rendered filter UI for component: ", comp_name)
      
      # Wrap in a container div with the component name for CSS targeting
      # Important: we ensure every component renders correctly even if initial visibility is off
      div(
        id = ns(paste0("filter_", comp_name)),
        class = paste0("union-component-filter ", comp_name, "-filter"),
        style = "display: none;", # Initially hidden, will be shown by the JS
        filter_element
      )
    })
  )
  
  # Log successful filter container creation
  message("Created filter container with ", length(names(filters)), " filters")
  
  return(filter_container)
}
  
####unionDisplayUI####  
  
#' Create the union display UI
#'
#' Creates a combined UI element containing all display components
#' with proper namespacing and visibility controls.
#'
#' @param id ID for the UI element
#' @param displays List of display UI elements from components
#'
#' @return A div containing all display UI elements
#' @noRd
unionDisplayUI <- function(id, displays) {
  ns <- NS(id)
  
  # Create a container for all displays
  display_container <- div(
    id = ns("display_container"),
    class = "union-display-container",
    
    # Add displays from each component
    lapply(names(displays), function(comp_name) {
      display_info <- displays[[comp_name]]
      display_id <- display_info$id
      display_ui <- display_info$ui
      
      # Add debug message for display rendering
      message("Rendering display UI for component: ", comp_name, " with ID: ", display_id)
      
      # Apply function if it's a function, or use as is if it's already a UI element
      display_element <- tryCatch({
        if (is.function(display_ui)) {
          message("Display UI for ", comp_name, " is a function - applying with ID: ", display_id)
          display_ui(display_id)
        } else {
          message("Display UI for ", comp_name, " is not a function - using directly")
          display_ui
        }
      }, error = function(e) {
        message("ERROR rendering display for component ", comp_name, ": ", e$message)
        # Return a placeholder to avoid breaking the UI
        div(class = "display-error",
            h4("Display Error", class = "text-warning"),
            p(paste("Error in display for", comp_name, ":", e$message)))
      })
      
      # Log successful display rendering
      message("Successfully rendered display UI for component: ", comp_name)
      
      # Wrap in a container div with the component name for CSS targeting
      # Important: Use inline style attribute with !important to guarantee visibility (R19 Component Visibility Guarantee)
      div(
        id = ns(paste0("display_", comp_name)),
        class = paste0("union-component-display ", comp_name, "-display"),
        # Remove initial hidden style to prevent display issues (R19 Component Visibility Guarantee)
        # style = "display: none;", # Initially hidden, will be shown by JS
        
        # Add enhanced rendering container with guaranteed visibility
        div(
          class = "component-content-wrapper",
          style = "display: block !important; visibility: visible !important; opacity: 1 !important;",
          
          # Add diagnostic information in a visible element for debugging
          tags$div(
            class = "debug-info",
            # Make debug info visible for troubleshooting (P102 Defensive Error Handling)
            style = "background: rgba(240,240,240,0.1); font-size: 9px; color: #aaa; padding: 2px; margin: 2px 0;",
            paste0("Component: ", comp_name, ", ID: ", display_id, 
                   ", Time: ", format(Sys.time(), "%H:%M:%S"))
          ),
          
          # The actual component display with explicitly guaranteed visibility
          tags$div(
            class = paste0("component-display-wrapper ", comp_name, "-wrapper"),
            style = "display: block !important; visibility: visible !important; opacity: 1 !important;",
            display_element
          )
        )
      )
    })
  )
  
  # Log successful display container creation
  message("Created display container with ", length(names(displays)), " displays")
  
  return(display_container)
}
  
####unionServer####
  
#' Create the union server function
#'
#' Creates a server function that initializes all component servers
#' and manages component visibility.
#'
#' @param id ID for the server module
#' @param servers List of server functions from components
#' @param components_meta Metadata about components
#' @param config Configuration options
#' @param app_data_connection Data connection to pass to servers
#' @param session Shiny session object
#'
#' @return A moduleServer function 
#' @noRd
unionServer <- function(id, servers, components_meta, config = NULL, 
                        app_data_connection = NULL, session = getDefaultReactiveDomain()) {
  moduleServer(id, function(input, output, session) {
    # Create reactive values to track component state
    component_state <- makeComponentState(components_meta, config)
    
    # Initialize server functions for all components
    server_instances <- list()
    server_outputs <- list()
    
    # Start all component servers
    for (comp_name in names(servers)) {
      server_info <- servers[[comp_name]]
      server_id <- server_info$id
      server_fn <- server_info$server_fn
      
      # Create a new environment for each server to avoid conflicts
      server_env <- new.env(parent = parent.frame())
      
      # Run the server function in its own environment
      server_result <- NULL
      tryCatch({
        server_result <- server_fn(input, output, session)
      }, error = function(e) {
        warning(paste("Error starting server for component", comp_name, ":", e$message))
      })
      
      # Store the server result
      server_outputs[[comp_name]] <- server_result
      server_instances[[comp_name]] <- server_env
    }
    
    # Set up observers to handle component visibility changes
    observe({
      # Get the list of visible components
      visible_components <- component_state$get_visible_components()
      message("Union observer updating component visibility. Visible components: ", 
             paste(visible_components, collapse=", "))
      
      # Use detailed error handling for visibility updates
      tryCatch({
        # Update visibility of all components
        for (comp_name in names(components_meta)) {
          is_visible <- comp_name %in% visible_components
          
          # Track each attempt with detailed logging
          message("Updating visibility for component: ", comp_name, " -> ", 
                 ifelse(is_visible, "VISIBLE", "HIDDEN"))
          
          # Safely update filter visibility
          filter_id <- paste0("filter_", comp_name)
          tryCatch({
            if (is_visible) {
              shinyjs::show(filter_id)
              message("Showed filter for ", comp_name)
            } else {
              shinyjs::hide(filter_id)
              message("Hid filter for ", comp_name)
            }
          }, error = function(e) {
            message("ERROR updating filter visibility for ", comp_name, ": ", e$message)
          })
          
          # Safely update display visibility
          display_id <- paste0("display_", comp_name)
          tryCatch({
            if (is_visible) {
              shinyjs::show(display_id)
              message("Showed display for ", comp_name)
            } else {
              shinyjs::hide(display_id)
              message("Hid display for ", comp_name)
            }
          }, error = function(e) {
            message("ERROR updating display visibility for ", comp_name, ": ", e$message)
          })
        }
      }, error = function(e) {
        message("ERROR in component visibility observer: ", e$message)
      })
    }, label = "component_visibility_observer")
    
    # Add additional observer specifically for the first component to ensure it's visible
    # This is critical for making sure at least one component is always shown
    observe({
      # Ensure the first component is visible if nothing else is
      if (length(component_state$get_visible_components()) == 0 && length(names(components_meta)) > 0) {
        first_comp <- names(components_meta)[1]
        message("No components visible, forcing visibility of first component: ", first_comp)
        component_state$toggle_component(first_comp, TRUE)
      }
    }, label = "ensure_visible_component_observer")
    
    # Create a reactive for combined filter values from all visible components
    filter_values <- reactive({
      # Start with an empty list
      all_values <- list()
      
      # Get visible components
      visible_components <- component_state$get_visible_components()
      
      # Collect filter values from visible components only
      for (comp_name in visible_components) {
        # Get the server output if it exists
        if (comp_name %in% names(server_outputs)) {
          server_output <- server_outputs[[comp_name]]
          
          # If server output is a reactive function, get its value
          if (is.function(server_output)) {
            # Try to get values 
            tryCatch({
              comp_values <- server_output()
              
              # Merge with collected values if it's a list
              if (is.list(comp_values)) {
                all_values <- c(all_values, comp_values)
              }
            }, error = function(e) {
              # Silently continue if we can't get values
            })
          }
        }
      }
      
      return(all_values)
    })
    
    # Add a permanent record of component states in a non-reactive structure
    # This allows for debugging and recovery even if the reactive components fail
    permanent_record <- list(
      component_names = names(components_meta),
      visibility_state = component_state$visibility_state,
      initialization_time = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
    
    # Store this in the global environment for emergency recovery
    union_debug_key <- paste0("union_debug_", id)
    assign(union_debug_key, permanent_record, envir = .GlobalEnv)
    message("Created permanent debug record for union '", id, "' with key: ", union_debug_key)
    
    # Return the combined state, server outputs, and filter_values function
    # with additional debugging helpers
    list(
      component_state = component_state,
      server_outputs = server_outputs,
      filter_values = filter_values,
      debug = list(
        force_visibility = function(component_name, visible = TRUE) {
          message("Forcing visibility of component: ", component_name, " to ", visible)
          component_state$toggle_component(component_name, visible)
          return(TRUE)
        },
        get_component_names = function() {
          return(names(components_meta))
        },
        debug_record = permanent_record
      )
    )
  })
}
  
  
####unionDefaults####
  
#' Create union defaults function
#'
#' Creates a function that merges default values from all components
#' with appropriate namespacing to avoid conflicts.
#'
#' @param components List of components with their defaults functions
#'
#' @return A function that returns merged default values
#' @noRd
unionDefaults <- function(components) {
  function() {
    # Merge all default values
    combined_defaults <- list()
    
    for (comp_name in names(components)) {
      component <- components[[comp_name]]
      
      # Check if component has a defaults function
      if ("defaults" %in% names(component) && is.function(component$defaults)) {
        # Get component defaults or empty list if the function fails
        comp_defaults <- tryCatch({
          component$defaults()
        }, error = function(e) {
          warning(paste("Error getting defaults for component", comp_name, ":", e$message))
          return(list())
        })
        
        # Skip if defaults are NULL or not a list
        if (is.null(comp_defaults) || !is.list(comp_defaults)) {
          next
        }
        
        # Prefix default keys with component name to avoid conflicts
        prefixed_defaults <- setNames(
          comp_defaults, 
          paste0(comp_name, "_", names(comp_defaults))
        )
        
        # Merge with combined defaults
        combined_defaults <- c(combined_defaults, prefixed_defaults)
      }
    }
    
    return(combined_defaults)
  }
}
  
####createUnionComponent####  
  
#' Union Component Factory
#'
#' Creates a union of multiple components, combining their UI and server parts
#' into a single consolidated component. This main function orchestrates the
#' creation of a union component using the modular functions above.
#'
#' @param id The base ID for the union component
#' @param components List of components to combine
#' @param config Optional configuration for the union
#'
#' @return A complete union component
#' @noRd
createUnionComponent <- function(id, components, config) {
  # Build the union component following R09 UI-Server-Defaults Triple Rule
  list(
    ui = list(
      filter = function(id) unionFilterUI(id, filters),
      display = function(id) unionDisplayUI(id, displays) 
    ),
    server = function(id, app_data_connection = NULL, session = getDefaultReactiveDomain()) {
      unionServer(id, servers, components_meta, config, app_data_connection, session)
    },
    defaults = unionDefaults(components),  # Always include defaults function
    components = components_meta,
    original_id = id
  )
}

# Create the union component
union_component <- createUnionComponent(id, components, config)

# Make the union component globally accessible if requested (MP93: Global Accessibility Principle)
if (global) {
  # Create a unique global variable name based on the ID
  union_var_name <- gsub("[^a-zA-Z0-9_]", "_", id)
  
  # If this is a special union name we know about, use that exact name for backward compatibility
  if (id == "top_level_union") {
    union_var_name <- "top_level_union"
  } else if (id == "micro_components_union") {
    union_var_name <- "micro_components_union"
  } else if (id == "sidebar_union") {
    union_var_name <- "sidebar_union"
  }
  
  # Add logging for transparency
  message("Creating global Union component: '", union_var_name, "' from ID '", id, "'")
  
  # Defensively handle assignment with error logging (P102: Defensive Error Handling Principle)
  tryCatch({
    # Assign to .GlobalEnv for guaranteed global visibility
    assign(union_var_name, union_component, envir = .GlobalEnv)
    message("Successfully created global Union component: ", union_var_name)
  }, error = function(e) {
    # Log the error but don't stop execution
    message("WARNING: Failed to create global Union component '", union_var_name, "': ", e$message)
  })
}

# Return the union component
return(union_component)
}


####UnionComponent####

#' Initialize the Union Component
#'
#' Creates a fully initialized union component with all UI parts, 
#' server functions, and default values properly connected.
#'
#' This function follows the UI-Server-Defaults Triple Rule (R09) by
#' maintaining separation between these concerns while packaging them
#' into a unified component.
#'
#' @param id The ID for the union component
#' @param components Named list of components to include in the union
#' @param app_data_connection Data connection to pass to server functions
#' @param config Optional configuration parameters
#' @param global Whether to make the union globally accessible (default: TRUE)
#'
#' @return A fully initialized union component with ui, server, and defaults
#' @export
UnionComponent <- function(id, components, app_data_connection = NULL, config = NULL, global = TRUE) {
  # Create arguments for the Union function
  union_args <- c(list(id = id), components, list(config = config, global = FALSE))
  
  # Create the union using do.call to handle the dynamic number of components
  # We set global=FALSE here to prevent Union from creating globals, we'll do it ourselves
  union <- do.call(Union, union_args)
  
  # Build the fully initialized component with UI, server, and defaults at the same level (R09)
  # Build the fully initialized component with UI, server, and defaults at the same level (R09)
  initialized_union <- list(
    ui = list(
      filter = union$ui$filter(id),
      display = union$ui$display(id)
    ),
    server = function(input, output, session) {
      # Initialize the union server
      union_server <- union$server(id, app_data_connection, session)
      
      # Create a wrapper for the combined filter values
      filter_values <- reactive({
        # Check if union_server has a filter_values function
        if (is.function(union_server$filter_values)) {
          return(union_server$filter_values())
        } else {
          return(list())
        }
      })
      
      # Add filter_values function to the union_server
      union_server$filter_values <- filter_values
      
      # Return the augmented union server results
      return(union_server)
    },
    # Keep defaults as a function, not a value (P62 Separation of Concerns)
    defaults = function() {
      # Pass through the defaults from the union
      if (!is.null(union$defaults) && is.function(union$defaults)) {
        return(union$defaults())
      } else {
        return(list())
      }
    },
    components = union$components,
    original_id = id
  )
  
  # Make the union component globally accessible if requested (MP93: Global Accessibility Principle)
  if (global) {
    # Create a unique global variable name based on the ID
    union_var_name <- gsub("[^a-zA-Z0-9_]", "_", id)
    
    # If this is a special union name we know about, use that exact name for backward compatibility
    if (id == "top_level_union") {
      union_var_name <- "top_level_union"
    } else if (id == "micro_components_union") {
      union_var_name <- "micro_components_union"
    } else if (id == "sidebar_union") {
      union_var_name <- "sidebar_union"
    }
    
    # Add logging for transparency
    message("Creating global UnionComponent: '", union_var_name, "' from ID '", id, "'")
    
    # Defensively handle assignment with error logging (P102: Defensive Error Handling Principle)
    tryCatch({
      # Assign to .GlobalEnv for guaranteed global visibility
      assign(union_var_name, initialized_union, envir = .GlobalEnv)
      message("Successfully created global UnionComponent: ", union_var_name)
    }, error = function(e) {
      # Log the error but don't stop execution
      message("WARNING: Failed to create global UnionComponent '", union_var_name, "': ", e$message)
    })
  }
  
  # Return the union component
  return(initialized_union)
}

####CreateUnion####

#' Create a Simple Union Component with Configuration
#'
#' A simpler interface to create a union component with a list of component creator functions.
#' This function handles the initialization of individual components before combining them.
#'
#' The function follows the UI-Server-Defaults Triple Rule (R09) by ensuring that
#' all components maintain separation between UI, server, and defaults.
#'
#' @param id The ID for the union component
#' @param component_creators Named list of component creator functions 
#'   (e.g., microCustomerComponent, microCustomer2Component)
#' @param app_data_connection Data connection to pass to component creators
#' @param config Optional configuration parameters for the union
#' @param component_configs Optional named list of configurations for individual components
#' @param global Whether to make the union globally accessible (default: TRUE)
#'
#' @return A fully initialized union component with ui, server, and defaults
#' @export
CreateUnion <- function(id, component_creators, app_data_connection = NULL, 
                        config = NULL, component_configs = NULL, global = TRUE) {
  # Initialize components by calling their creator functions
  components <- list()
  
  for (comp_name in names(component_creators)) {
    creator_fn <- component_creators[[comp_name]]
    
    # Get component-specific config if available
    comp_config <- NULL
    if (!is.null(component_configs) && comp_name %in% names(component_configs)) {
      comp_config <- component_configs[[comp_name]]
    }
    
    # Create the component and validate its structure
    component <- creator_fn(
      paste0(id, "_", comp_name),
      app_data_connection,
      comp_config
    )
    
    # Validate that the component follows R09 UI-Server-Defaults Triple Rule
    if (!is.list(component) || 
        !all(c("ui", "server") %in% names(component)) ||
        !all(c("filter", "display") %in% names(component$ui))) {
      stop(paste("Component", comp_name, "does not follow required structure (ui.filter, ui.display, server)"))
    }
    
    # Check for defaults function (recommended)
    if (!"defaults" %in% names(component)) {
      warning(paste("Component", comp_name, "is missing a defaults function. Consider adding one for consistency with R09."))
    } else if (!is.function(component$defaults)) {
      stop(paste("Component", comp_name, "has a defaults element that is not a function."))
    }
    
    # Store the component
    components[[comp_name]] <- component
  }
  
  # Create the union component, passing the global parameter
  UnionComponent(id, components, app_data_connection, config, global = global)
}

#' Example usage:
#' ```r
#' # Create union of micro customer components
#' customer_union <- CreateUnion(
#'   id = "customer_union",
#'   component_creators = list(
#'     customer1 = microCustomerComponent,
#'     customer2 = microCustomer2Component
#'   ),
#'   app_data_connection = app_conn,
#'   config = list(
#'     initial_visibility = list(
#'       customer1 = TRUE,
#'       customer2 = FALSE
#'     )
#'   )
#' )
#' 
#' # Use in app.R
#' ui <- dashboardPage(
#'   # ... other UI elements ...
#'   body = dashboardBody(
#'     # Render the filter in sidebar
#'     sidebar = dashboardSidebar(
#'       customer_union$ui$filter
#'     ),
#'     
#'     # Render the display in body
#'     customer_union$ui$display
#'   )
#' )
#' 
#' server <- function(input, output, session) {
#'   # Initialize the union server
#'   union_server <- customer_union$server(input, output, session)
#'   
#'   # Use the component state to control visibility
#'   observeEvent(input$show_customer2, {
#'     union_server$component_state$toggle_component("customer2", TRUE)
#'   })
#' }
#' ```