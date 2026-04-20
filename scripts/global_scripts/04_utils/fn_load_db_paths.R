#' load_db_paths ---------------------------------------------------------------
#' Load database paths from db_paths.yaml configuration file.
#'
#' Reads the db_paths.yaml file and constructs full paths by combining
#' relative paths with APP_DIR. Returns a list of database paths compatible
#' with the legacy db_path_list format.
#'
#' Following principles:
#' - DM_R004: Data Storage Organization (6-layer architecture)
#' - DM_R048: Format Selection (YAML for configuration)
#' - MP136: Configuration Initialization Pattern
#'
#' @param app_dir Character. Base directory for path construction.
#'        Default: uses APP_DIR from global environment.
#' @param yaml_path Character. Path to db_paths.yaml file.
#'        Default: constructs from GLOBAL_DIR.
#' @param attach Logical. Whether to assign db_path_list to global environment.
#'        Default: TRUE.
#' @param verbose Logical. Whether to print loading messages. Default: FALSE.
#'
#' @return Named list of full database paths.
#' @export
#' @importFrom yaml read_yaml
load_db_paths <- function(app_dir = NULL,
                          yaml_path = NULL,
                          attach = TRUE,
                          verbose = FALSE) {
  # Resolve APP_DIR
  if (is.null(app_dir)) {
    if (!exists("APP_DIR", inherits = TRUE)) {
      stop("APP_DIR is not defined. Call autoinit() first.")
    }
    app_dir <- get("APP_DIR", inherits = TRUE)
  }

  # Resolve yaml_path
  if (is.null(yaml_path)) {
    global_dir <- get0("GLOBAL_DIR", inherits = TRUE, ifnotfound = NULL)
    if (!is.null(global_dir)) {
      yaml_path <- file.path(global_dir, "30_global_data", "parameters",
                             "scd_type1", "db_paths.yaml")
    } else {
      # Fallback: construct from app_dir
      yaml_path <- file.path(app_dir, "scripts", "global_scripts", "30_global_data",
                             "parameters", "scd_type1", "db_paths.yaml")
    }
  }

  # Check file exists
  if (!file.exists(yaml_path)) {
    warning("db_paths.yaml not found: ", yaml_path)
    return(NULL)
  }

  # Load YAML
  config <- yaml::read_yaml(yaml_path)

  # Construct db_path_list from databases section
  db_path_list <- list()

  # Process main databases section
  if (!is.null(config$databases)) {
    for (name in names(config$databases)) {
      rel_path <- config$databases[[name]]
      db_path_list[[name]] <- file.path(app_dir, rel_path)
    }
  }

  # Process domain section (merge into main list for backward compatibility)
  if (!is.null(config$domain)) {
    for (name in names(config$domain)) {
      rel_path <- config$domain[[name]]
      db_path_list[[name]] <- file.path(app_dir, rel_path)
    }
  }

  if (verbose) {
    message("Loaded ", length(db_path_list), " database paths from ", yaml_path)
  }

  # Attach to global environment
  if (isTRUE(attach)) {
    assign("db_path_list", db_path_list, envir = .GlobalEnv)
    # Also assign individual paths for backward compatibility
    for (name in names(db_path_list)) {
      assign(name, db_path_list[[name]], envir = .GlobalEnv)
    }
    if (verbose) {
      message("Database paths assigned to global environment")
    }
  }

  invisible(db_path_list)
}
