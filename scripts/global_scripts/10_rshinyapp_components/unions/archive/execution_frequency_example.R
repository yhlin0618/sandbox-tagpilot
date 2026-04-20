# Example implementation demonstrating execution frequency adjectives
# Following NSQL temporal logic

#' One-time Union Component
#'
#' Creates a union of UI components that is initialized exactly once at startup.
#' Following the ⚡¹(one-time) execution adjective pattern.
#'
#' @param ... UI components to combine
#' @param id Namespace ID
#' @return UI component representing the static union
#' @export
oneTimeUnionUI <- function(..., id = NULL) {
  components <- list(...)
  ns <- if (!is.null(id)) NS(id) else function(x) x
  
  # Create container with all components included
  container <- div(
    id = ns("union_container"),
    class = "one-time-union",
    
    # This comment documents the execution frequency
    HTML("<!-- ⚡¹ One-time union operation performed at initialization -->"),
    
    # Process all components
    lapply(names(components), function(name) {
      div(
        id = ns(paste0("component_", name)),
        style = "display: none;",  # Initially hidden
        components[[name]]
      )
    })
  )
  
  return(container)
}

#' Many-time Visibility Controller
#'
#' Controls component visibility reactively, executing many times during app lifecycle.
#' Following the ⚡*(many-time) execution adjective pattern.
#'
#' @param id Module ID
#' @param visibility_conditions Reactive expressions controlling visibility
#' @return Reactive expression with currently visible components
#' @export
manyTimeVisibilityServer <- function(id, visibility_conditions) {
  moduleServer(id, function(input, output, session) {
    # This reactive observer follows the many-time pattern
    # It executes whenever any visibility condition changes
    observe({
      # This comment documents the execution frequency
      message("⚡* Many-time visibility control executing")
      
      for (name in names(visibility_conditions)) {
        component_id <- paste0("component_", name)
        is_visible <- visibility_conditions[[name]]()
        
        if (is_visible) {
          shinyjs::show(id = component_id)
        } else {
          shinyjs::hide(id = component_id)
        }
      }
    })
  })
}

#' Two-time Validation Component
#'
#' Performs validation exactly twice - once on client side and once on server.
#' Following the ⚡²(two-time) execution adjective pattern.
#'
#' @param value Value to validate
#' @param rules Validation rules
#' @return Validated value or error
#' @export
twoTimeValidator <- function(value, rules) {
  # This comment documents the execution frequency
  message("⚡² Two-time validator executing (first time)")
  
  # First execution: Client-side validation
  client_validation <- function(value, rules) {
    # Client-side validation logic
    for (rule in rules) {
      if (!eval(bquote(.(rule)(value)))) {
        return(list(valid = FALSE, message = "Client validation failed"))
      }
    }
    return(list(valid = TRUE, value = value))
  }
  
  # Second execution: Server-side validation
  server_validation <- function(value, rules) {
    # This comment documents the execution frequency
    message("⚡² Two-time validator executing (second time)")
    
    # Server-side validation logic
    for (rule in rules) {
      if (!eval(bquote(.(rule)(value)))) {
        return(list(valid = FALSE, message = "Server validation failed"))
      }
    }
    return(list(valid = TRUE, value = value))
  }
  
  # Run client validation
  client_result <- client_validation(value, rules)
  if (!client_result$valid) {
    return(client_result)
  }
  
  # Run server validation
  return(server_validation(value, rules))
}

#' Conditional-time Feature Component
#'
#' Implements a feature that only executes when a condition is met.
#' Following the ⚡?(conditional-time) execution adjective pattern.
#'
#' @param condition Condition determining if feature should execute
#' @param feature_function Function implementing the feature
#' @param ... Arguments to pass to feature_function
#' @return Feature result or NULL
#' @export
conditionalTimeFeature <- function(condition, feature_function, ...) {
  # Only execute if condition is met
  if (condition) {
    # This comment documents the execution frequency
    message("⚡? Conditional-time feature executing (condition met)")
    return(feature_function(...))
  } else {
    message("⚡? Conditional-time feature skipped (condition not met)")
    return(NULL)
  }
}

#' Periodic-time Refresh Component
#'
#' Refreshes data at regular intervals.
#' Following the ⚡ᵖ(periodic-time) execution adjective pattern.
#'
#' @param interval_ms Refresh interval in milliseconds
#' @param refresh_function Function to call on each refresh
#' @param session Shiny session
#' @return Timer ID
#' @export
periodicTimeRefresh <- function(interval_ms, refresh_function, session = getDefaultReactiveDomain()) {
  # This comment documents the execution frequency
  message("⚡ᵖ Periodic-time refresh initialized with interval ", interval_ms, "ms")
  
  # Set up the periodic timer
  timer_id <- NULL
  timer_id <- shiny::invalidateLater(interval_ms, session)
  
  # Create an observer that will re-execute periodically
  observe({
    # Depend on the invalidation timer
    shiny::invalidateLater(interval_ms, session)
    
    # This comment documents each execution
    message("⚡ᵖ Periodic-time refresh executing")
    
    # Call the refresh function
    refresh_function()
  })
  
  return(timer_id)
}

#' Zero-time Placeholder Function
#'
#' Contains code that is never executed but is preserved for future use.
#' Following the ⚡⁰(zero-time) execution adjective pattern.
#'
#' @param ... Parameters for future implementation
#' @return NULL (never actually returns)
#' @export
zeroTimePlaceholder <- function(...) {
  # This function is never called
  message("⚡⁰ Zero-time placeholder - this message will never appear")
  
  # Future implementation code preserved here
  # ...
  
  return(NULL)
}

#' Every-time Event Handler
#'
#' Executes on every occurrence of a specific event.
#' Following the ⚡ᵉ(every-time) execution adjective pattern.
#'
#' @param event_id Event to handle
#' @param handler_function Function to call on each event
#' @param session Shiny session
#' @return Observer
#' @export
everyTimeHandler <- function(event_id, handler_function, session = getDefaultReactiveDomain()) {
  # This comment documents the execution frequency pattern
  message("⚡ᵉ Every-time handler registered for event ", event_id)
  
  # Return an observer that executes on every event
  observeEvent(input[[event_id]], {
    # This executes on every occurrence of the event
    message("⚡ᵉ Every-time handler executing for event ", event_id)
    handler_function(input[[event_id]])
  })
}

#' Usage Example - Complete App with Temporal Adjectives
if (FALSE) {
  # UI with combination of execution frequency patterns
  ui <- fluidPage(
    shinyjs::useShinyjs(),
    
    # One-time union of UI components
    oneTimeUnionUI(
      main = div(
        h1("Temporal Adjectives Demo"),
        actionButton("refresh", "Refresh Data"),
        conditionalPanel(
          "input.advanced_mode == true",
          actionButton("advanced_action", "Advanced Action")
        )
      ),
      
      data_section = div(
        h2("Data Section"),
        verbatimTextOutput("data_output")
      ),
      
      settings = div(
        h2("Settings"),
        checkboxInput("advanced_mode", "Advanced Mode", FALSE),
        sliderInput("refresh_interval", "Refresh Interval (seconds)", 
                   min = 1, max = 60, value = 5)
      ),
      
      id = "main_ui"
    )
  )
  
  # Server with different execution frequency patterns
  server <- function(input, output, session) {
    # Many-time visibility control
    manyTimeVisibilityServer(
      "main_ui",
      visibility_conditions = list(
        main = reactive(TRUE),  # Always visible
        data_section = reactive(TRUE),  # Always visible
        settings = reactive(input$advanced_mode)  # Conditionally visible
      )
    )
    
    # Two-time validation for input
    validated_interval <- reactive({
      interval <- input$refresh_interval
      # Apply two-time validation (client and server)
      result <- twoTimeValidator(
        interval,
        list(
          function(x) x >= 1,
          function(x) x <= 60
        )
      )
      if (result$valid) return(result$value) else return(5)
    })
    
    # Conditional-time feature
    observeEvent(input$advanced_action, {
      conditionalTimeFeature(
        condition = input$advanced_mode,
        feature_function = function() {
          # Advanced feature implementation
          message("Advanced feature executed")
          showNotification("Advanced action performed!")
        }
      )
    })
    
    # Periodic-time refresh
    observe({
      # Get validated interval
      interval_ms <- validated_interval() * 1000
      
      # Set up periodic refresh
      periodicTimeRefresh(
        interval_ms = interval_ms,
        refresh_function = function() {
          # Refresh data
          output$data_output <- renderPrint({
            paste("Data refreshed at", Sys.time())
          })
        }
      )
    })
    
    # Every-time event handler
    everyTimeHandler(
      "refresh",
      function(value) {
        # Handle refresh button click
        output$data_output <- renderPrint({
          paste("Manual refresh at", Sys.time())
        })
      }
    )
    
    # Zero-time placeholder for future implementation
    zeroTimePlaceholder(
      # This will never execute but preserves code for future use
      function() {
        # Future advanced analytics feature
      }
    )
  }
}