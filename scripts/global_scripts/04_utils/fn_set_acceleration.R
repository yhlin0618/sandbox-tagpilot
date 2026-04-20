# Performance Acceleration Control Function
# This function allows you to set the acceleration level before initialization

#' Set the application acceleration level
#' 
#' This function sets the performance acceleration level for the application.
#' Call this function BEFORE sourcing the initialization script to control 
#' how performance optimizations are applied.
#' 
#' @param level Numeric (0-3) indicating acceleration level:
#'        0 = Disabled (no optimizations)
#'        1 = Basic optimizations (minimal overhead)
#'        2 = Enhanced optimizations (balanced)
#'        3 = Maximum optimizations (highest performance)
#' @param verbose Logical indicating whether to print details about acceleration
#' 
#' @return Invisibly returns prior setting
#' 
#' @examples
#' # Set maximum acceleration
#' set_acceleration(3)
#' 
#' # Disable acceleration completely
#' set_acceleration(0)
set_acceleration <- function(level, verbose = TRUE) {
  # Store prior setting to return
  prior_level <- if(exists("ACCELERATION_LEVEL")) ACCELERATION_LEVEL else 2
  
  # Validate level
  level <- as.numeric(level)
  if (is.na(level) || level < 0 || level > 3) {
    warning("Invalid acceleration level. Using default (2).")
    level <- 2
  }
  
  # Set acceleration level
  assign("ACCELERATION_LEVEL", level, envir = .GlobalEnv)
  
  # Print status if verbose
  if (verbose) {
    level_desc <- switch(as.character(level),
                        "0" = "Disabled (no optimizations)",
                        "1" = "Basic optimizations (minimal overhead)",
                        "2" = "Enhanced optimizations (balanced)",
                        "3" = "Maximum optimizations (highest performance)")
    message("Performance acceleration level set to ", level, ": ", level_desc)
  }
  
  # Return prior setting invisibly
  invisible(prior_level)
}

message("Performance acceleration control function loaded")