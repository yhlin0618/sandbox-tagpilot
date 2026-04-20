#' Grid Components Configuration Script
#' 
#' This script loads the grid components parameters and provides functions
#' for working with condition grids as defined in D02 derivation.
#'
#' File: sc_grid_components.R

# Load parameters
app_data_path <- file.path(getwd(), "app_data")
params_file <- file.path(app_data_path, "parameters", "list_grid_components.R")
source(params_file)

#' Generate the full condition grid from components
#' @param components list of component vectors (optional)
#' @return data.frame with all condition combinations
get_condition_grid <- function(components = grid_components) {
  require(tidyr)
  do.call(expand_grid, components)
}

#' Get a subset of the condition grid with specific filter values
#' @param time_values Character vector of time conditions to include
#' @param product_values Numeric vector of product line IDs to include
#' @param state_values Character vector of state filters to include
#' @param source_values Character vector of source filters to include
#' @return data.frame with filtered condition combinations
get_filtered_condition_grid <- function(
    time_values = NULL,
    product_values = NULL,
    state_values = NULL,
    source_values = NULL) {
  
  # Start with components from parameters
  filtered_components <- grid_components
  
  # Filter components if specified
  if (!is.null(time_values)) {
    filtered_components$time_condition <- intersect(
      filtered_components$time_condition, time_values)
  }
  
  if (!is.null(product_values)) {
    filtered_components$product_line_id_filter <- intersect(
      filtered_components$product_line_id_filter, product_values)
  }
  
  if (!is.null(state_values)) {
    filtered_components$state_filter <- intersect(
      filtered_components$state_filter, state_values)
  }
  
  if (!is.null(source_values)) {
    filtered_components$source_filter <- intersect(
      filtered_components$source_filter, source_values)
  }
  
  # Generate grid with filtered components
  get_condition_grid(filtered_components)
}