#' @file fn_dropbox_sync.R
#' @use_package httr
#' @use_package jsonlite
#' @use_package tools
#' @use_package ps
#' @note Implements MP080 (Database Synchronization)
#' 
#' @title Dropbox Synchronization Utility Functions
#'
#' @description
#' Functions to detect, control, and manage Dropbox synchronization status.
#' Implements MP080 Database Synchronization for controlling Dropbox sync
#' during database operations.
#'
#' @details
#' These functions provide utilities to:
#' 1. Detect if Dropbox is running and actively synchronizing
#' 2. Get the status of sync (on/off/paused)
#' 3. Set the global DROPBOX_SYNC variable based on detected status
#'
#' The functions use various methods to detect Dropbox status:
#' - System process checks to detect if Dropbox is running
#' - File change monitoring to detect sync activity
#' - Optional Dropbox API integration for more reliable status checks
#'
#' @note
#' These functions implement MP080 (Database Synchronization) which requires
#' control of Dropbox sync during database operations to prevent conflicts.

#' @title Check if Dropbox is Running (PS method)
#'
#' @description
#' Detects if Dropbox is currently running on the system by looking for
#' Dropbox-related processes using the ps package.
#'
#' @param verbose Logical. Whether to print status messages (default: FALSE)
#'
#' @return Logical. TRUE if Dropbox is running, FALSE otherwise
#'
#' @examples
#' # Check if Dropbox is running
#' is_running <- is_dropbox_running_ps()
#' 
#' # Check with status messages
#' is_running <- is_dropbox_running_ps(verbose = TRUE)
#'
#' @export
is_dropbox_running_ps <- function(verbose = FALSE) {
  if (!requireNamespace("ps", quietly = TRUE)) {
    if (verbose) {
      message("Package 'ps' is required for Dropbox process detection. Install with: install.packages('ps')")
    }
    return(NA)
  }
  
  # Get all running processes
  processes <- tryCatch({
    ps::ps_pids()
  }, error = function(e) {
    if (verbose) {
      warning("Could not access process list: ", e$message)
    }
    return(NULL)
  })
  
  if (is.null(processes)) {
    return(NA)
  }
  
  # Look for Dropbox processes
  dropbox_found <- FALSE
  for (pid in processes) {
    # Get process name
    proc_name <- tryCatch({
      proc <- ps::ps_handle(pid)
      info <- ps::ps_name(proc)
      tolower(info)
    }, error = function(e) {
      ""
    })
    
    # Check if it's a Dropbox process
    if (grepl("dropbox", proc_name, ignore.case = TRUE)) {
      dropbox_found <- TRUE
      if (verbose) {
        message("Dropbox process found: ", proc_name, " (PID: ", pid, ")")
      }
      break
    }
  }
  
  if (verbose && !dropbox_found) {
    message("No Dropbox processes detected")
  }
  
  return(dropbox_found)
}

#' @title Check if Dropbox is Running (System Command)
#'
#' @description
#' Detects if Dropbox is currently running on the system using direct system commands.
#' Currently supports macOS, with limited support for other platforms.
#'
#' @param verbose Logical. Whether to print status messages (default: FALSE)
#'
#' @return Logical. TRUE if Dropbox is running, FALSE otherwise, NA if unknown
#'
#' @examples
#' # Check if Dropbox is running
#' is_running <- is_dropbox_running()
#' 
#' @export
is_dropbox_running <- function(verbose = FALSE) {
  result <- NA
  
  # First try system-specific commands
  system_name <- Sys.info()["sysname"]
  
  if (system_name == "Darwin") {  # macOS
    output <- tryCatch({
      system("pgrep Dropbox", intern = TRUE, ignore.stderr = TRUE)
    }, error = function(e) {
      if (verbose) warning("Error checking Dropbox process: ", e$message)
      return(character(0))
    })
    
    result <- length(output) > 0
    
    if (verbose) {
      if (result) {
        message("Dropbox is running (macOS detection)")
      } else {
        message("Dropbox is not running (macOS detection)")
      }
    }
  } else if (system_name == "Windows") {
    # Windows detection (using tasklist)
    output <- tryCatch({
      system('tasklist /FI "IMAGENAME eq Dropbox.exe" /NH', intern = TRUE, ignore.stderr = TRUE)
    }, error = function(e) {
      if (verbose) warning("Error checking Dropbox process: ", e$message)
      return(character(0))
    })
    
    result <- any(grepl("Dropbox.exe", output, ignore.case = TRUE))
    
    if (verbose) {
      if (result) {
        message("Dropbox is running (Windows detection)")
      } else {
        message("Dropbox is not running (Windows detection)")
      }
    }
  } else if (system_name == "Linux") {
    # Linux detection (using ps)
    output <- tryCatch({
      system("ps aux | grep -v grep | grep -i dropbox", intern = TRUE, ignore.stderr = TRUE)
    }, error = function(e) {
      if (verbose) warning("Error checking Dropbox process: ", e$message)
      return(character(0))
    })
    
    result <- length(output) > 0
    
    if (verbose) {
      if (result) {
        message("Dropbox is running (Linux detection)")
      } else {
        message("Dropbox is not running (Linux detection)")
      }
    }
  } else {
    if (verbose) {
      message("Unsupported operating system for Dropbox detection: ", system_name)
    }
  }
  
  # Fall back to ps package if direct detection failed
  if (is.na(result)) {
    if (verbose) {
      message("Falling back to ps package for Dropbox detection...")
    }
    result <- is_dropbox_running_ps(verbose = verbose)
  }
  
  return(result)
}

#' @title Check Dropbox Sync Status (Basic Method)
#'
#' @description
#' Attempts to determine if Dropbox is currently syncing by monitoring
#' file changes in the Dropbox directory. This is a best-effort determination
#' and may not always be accurate.
#'
#' @param check_dir Character. Path to a Dropbox directory to check (default: NULL)
#' @param timeout Integer. Time in seconds to watch for changes (default: 2)
#' @param verbose Logical. Whether to print status messages (default: FALSE)
#'
#' @return Integer. 
#'  - 1: Sync active (changes detected)
#'  - 0: Sync inactive (no changes detected) 
#'  - -1: Could not determine (Dropbox not running or errors)
#'
#' @examples
#' # Check sync status in current directory
#' status <- check_dropbox_sync_status()
#' 
#' # Check in specific directory with longer timeout
#' status <- check_dropbox_sync_status("~/Dropbox/Documents", timeout = 5)
#'
#' @export
check_dropbox_sync_status <- function(check_dir = NULL, timeout = 2, verbose = FALSE) {
  # First check if Dropbox is running
  is_running <- is_dropbox_running(verbose = verbose)
  
  if (is.na(is_running)) {
    if (verbose) {
      message("Could not determine if Dropbox is running")
    }
    return(-1)
  }
  
  if (!is_running) {
    if (verbose) {
      message("Dropbox is not currently running")
    }
    return(-1)
  }
  
  # If no directory provided, use the current directory
  if (is.null(check_dir)) {
    check_dir <- getwd()
    
    # Check if the current directory is in Dropbox
    if (!grepl("Dropbox", check_dir, ignore.case = TRUE)) {
      if (verbose) {
        message("Current directory does not appear to be in Dropbox. Sync status cannot be determined.")
        message("To check sync status, provide a path within your Dropbox folder.")
      }
      return(-1)
    }
  }
  
  # Check if directory exists
  if (!dir.exists(check_dir)) {
    if (verbose) {
      message("Directory does not exist: ", check_dir)
    }
    return(-1)
  }
  
  # Get initial snapshot of files
  initial_files <- tryCatch({
    list.files(check_dir, recursive = FALSE, all.files = TRUE, include.dirs = TRUE)
  }, error = function(e) {
    if (verbose) {
      warning("Error listing directory contents: ", e$message)
    }
    return(NULL)
  })
  
  if (is.null(initial_files)) {
    return(-1)
  }
  
  # Create a test file to potentially trigger sync
  test_file <- NULL
  tryCatch({
    # Create a temporary file in the directory
    test_file <- tempfile(pattern = "dropbox_sync_test_", tmpdir = check_dir)
    file.create(test_file)
    
    if (verbose) {
      message("Created test file: ", basename(test_file))
    }
  }, error = function(e) {
    if (verbose) {
      warning("Could not create test file: ", e$message)
    }
  })
  
  # Wait for the specified timeout
  if (verbose) {
    message("Monitoring for changes for ", timeout, " seconds...")
  }
  Sys.sleep(timeout)
  
  # Get new snapshot of files
  new_files <- tryCatch({
    list.files(check_dir, recursive = FALSE, all.files = TRUE, include.dirs = TRUE)
  }, error = function(e) {
    if (verbose) {
      warning("Error listing directory contents: ", e$message)
    }
    return(NULL)
  })
  
  # Clean up test file
  if (!is.null(test_file) && file.exists(test_file)) {
    tryCatch({
      file.remove(test_file)
      if (verbose) {
        message("Removed test file")
      }
    }, error = function(e) {
      if (verbose) {
        warning("Could not remove test file: ", e$message)
      }
    })
  }
  
  if (is.null(new_files)) {
    return(-1)
  }
  
  # Check for changes in the directory
  has_changes <- FALSE
  
  # Compare modification times for files
  for (f in new_files) {
    if (f %in% initial_files) {
      initial_time <- file.info(file.path(check_dir, f))$mtime[1]
      new_time <- file.info(file.path(check_dir, f))$mtime[1]
      
      if (!is.na(initial_time) && !is.na(new_time) && initial_time != new_time) {
        has_changes <- TRUE
        if (verbose) {
          message("Change detected in file: ", f)
        }
        break
      }
    }
  }
  
  # Check for new or deleted files
  if (length(initial_files) != length(new_files) || 
      !all(sort(initial_files) == sort(new_files))) {
    has_changes <- TRUE
    if (verbose) {
      message("Files added or removed in the directory")
    }
  }
  
  if (verbose) {
    if (has_changes) {
      message("Dropbox sync appears to be ACTIVE (changes detected)")
    } else {
      message("Dropbox sync appears to be INACTIVE or PAUSED (no changes detected)")
    }
  }
  
  return(if (has_changes) 1 else 0)
}

#' @title Comprehensive Dropbox Status Check
#'
#' @description
#' Performs a comprehensive check of Dropbox status by examining:
#' 1. If Dropbox is running
#' 2. If file changes are occurring (potential sync activity)
#' 3. Optional Dropbox API status (if access token provided)
#'
#' @param dropbox_path Character. Path to a Dropbox directory to check.
#' @param access_token Character. Optional Dropbox API access token for more reliable checks.
#' @param verbose Logical. Whether to print status messages (default: FALSE).
#' @param timeout Integer. Time in seconds to monitor for file changes (default: 5).
#'
#' @return List. Contains various status elements:
#'   - dropbox_running: Whether Dropbox is running
#'   - files_changed: Whether file changes were detected
#'   - api_status: Dropbox API status (if access token provided)
#'   - overall_status: Text description of overall status
#'   - sync_active: Logical assessment of whether sync appears active
#'
#' @examples
#' # Basic check with only local information
#' status <- check_comprehensive_dropbox_status("~/Dropbox/Documents")
#'
#' # With API access (requires token)
#' # status <- check_comprehensive_dropbox_status("~/Dropbox/Documents", 
#' #                                            access_token = "your_token")
#'
#' @export
check_comprehensive_dropbox_status <- function(dropbox_path, access_token = NULL, 
                                              verbose = FALSE, timeout = 5) {
  results <- list()
  
  # Check if Dropbox is running
  if (verbose) message("Checking if Dropbox is running...")
  
  # Use system-specific detection (macOS, Windows, Linux)
  if (Sys.info()["sysname"] == "Darwin") {  # macOS
    dropbox_running <- length(system("pgrep Dropbox", intern = TRUE)) > 0
    if (verbose) message("macOS detection method: Dropbox ", if(dropbox_running) "is running" else "is not running")
  } else if (Sys.info()["sysname"] == "Windows") { # Windows
    cmd_result <- tryCatch({
      system('tasklist /FI "IMAGENAME eq Dropbox.exe" /NH', intern = TRUE)
    }, error = function(e) character(0))
    dropbox_running <- any(grepl("Dropbox.exe", cmd_result))
    if (verbose) message("Windows detection method: Dropbox ", if(dropbox_running) "is running" else "is not running")
  } else if (Sys.info()["sysname"] == "Linux") { # Linux
    cmd_result <- tryCatch({
      system("ps aux | grep -v grep | grep -i dropbox", intern = TRUE)
    }, error = function(e) character(0))
    dropbox_running <- length(cmd_result) > 0
    if (verbose) message("Linux detection method: Dropbox ", if(dropbox_running) "is running" else "is not running")
  } else {
    # Fallback to ps package if available
    if (requireNamespace("ps", quietly = TRUE)) {
      dropbox_running <- is_dropbox_running_ps(verbose = FALSE)
      if (verbose) message("PS package detection: Dropbox ", if(dropbox_running) "is running" else "is not running")
    } else {
      dropbox_running <- FALSE  # Unable to determine
      if (verbose) message("Unable to determine if Dropbox is running - unsupported system")
    }
  }
  
  results$dropbox_running <- dropbox_running
  
  # Check if directory exists
  if (!dir.exists(dropbox_path)) {
    if (verbose) {
      message("Dropbox path does not exist: ", dropbox_path)
    }
    results$files_changed <- NA
    results$sync_active <- NA
    results$overall_status <- "Dropbox path not found"
    return(results)
  }
  
  # Check for file changes
  if (verbose) message("Monitoring Dropbox directory for changes...")
  
  files_before <- tryCatch({
    list.files(dropbox_path, recursive = FALSE)
  }, error = function(e) {
    if (verbose) warning("Error listing directory: ", e$message)
    return(character(0))
  })
  
  # Create a test file to potentially trigger sync
  test_file <- NULL
  tryCatch({
    # Create a temporary file in the directory
    test_file <- tempfile(pattern = "dropbox_sync_test_", tmpdir = dropbox_path)
    file.create(test_file)
    
    if (verbose) {
      message("Created test file: ", basename(test_file))
    }
  }, error = function(e) {
    if (verbose) {
      warning("Could not create test file: ", e$message)
    }
  })
  
  # Wait for the specified timeout
  if (verbose) message("Waiting for ", timeout, " seconds to monitor activity...")
  Sys.sleep(timeout)
  
  files_after <- tryCatch({
    list.files(dropbox_path, recursive = FALSE)
  }, error = function(e) {
    if (verbose) warning("Error listing directory: ", e$message)
    return(character(0))
  })
  
  # Clean up test file
  if (!is.null(test_file) && file.exists(test_file)) {
    tryCatch({
      file.remove(test_file)
      if (verbose) {
        message("Removed test file")
      }
    }, error = function(e) {
      if (verbose) {
        warning("Could not remove test file: ", e$message)
      }
    })
  }
  
  # Check for any changes in file list
  results$files_changed <- !identical(files_before, files_after)
  
  if (verbose) {
    if (results$files_changed) {
      message("File changes detected in Dropbox directory")
    } else {
      message("No file changes detected in Dropbox directory")
    }
  }
  
  # Check for changes in file modification times
  mod_times_changed <- FALSE
  common_files <- intersect(files_before, files_after)
  
  for (file in common_files) {
    file_path <- file.path(dropbox_path, file)
    
    mod_time_before <- file.info(file_path)$mtime[1]
    Sys.sleep(1)  # Brief pause
    mod_time_after <- file.info(file_path)$mtime[1]
    
    if (!is.na(mod_time_before) && !is.na(mod_time_after) && 
        mod_time_before != mod_time_after) {
      mod_times_changed <- TRUE
      if (verbose) message("Modification time changed for file: ", file)
      break
    }
  }
  
  results$mod_times_changed <- mod_times_changed
  
  # If provided with an access token, check API status
  if (!is.null(access_token)) {
    if (verbose) message("Checking Dropbox API status...")
    
    if (!requireNamespace("httr", quietly = TRUE)) {
      if (verbose) message("Package 'httr' required for API checks. Install with: install.packages('httr')")
      results$api_status <- NA
    } else {
      api_response <- tryCatch({
        httr::GET(
          url = "https://api.dropboxapi.com/2/users/get_space_usage",
          httr::add_headers(
            Authorization = paste("Bearer", access_token),
            "Content-Type" = "application/json"
          )
        )
      }, error = function(e) {
        if (verbose) warning("API request error: ", e$message)
        return(NULL)
      })
      
      results$api_status <- !is.null(api_response) && httr::status_code(api_response) == 200
      
      if (verbose) {
        if (!is.null(api_response)) {
          message("API request status code: ", httr::status_code(api_response))
        } else {
          message("API request failed")
        }
      }
    }
  } else {
    results$api_status <- NA
  }
  
  # Make overall assessment
  if (results$dropbox_running) {
    if (results$files_changed || results$mod_times_changed) {
      results$sync_active <- TRUE
      results$overall_status <- "Dropbox is running and actively syncing (file changes detected)"
    } else if (!is.na(results$api_status) && results$api_status) {
      results$sync_active <- TRUE
      results$overall_status <- "Dropbox is running and likely active (API connection successful)"
    } else {
      results$sync_active <- FALSE
      results$overall_status <- "Dropbox is running but appears to be paused or inactive (no changes detected)"
    }
  } else {
    results$sync_active <- FALSE
    results$overall_status <- "Dropbox is not running"
  }
  
  if (verbose) {
    message("Overall status: ", results$overall_status)
  }
  
  return(results)
}

#' @title Initialize Dropbox Sync Variable
#'
#' @description
#' Detects Dropbox sync status and sets the global DROPBOX_SYNC variable
#' for use with the MP080 Database Synchronization principle. Uses the
#' comprehensive check method to detect sync status more accurately.
#'
#' @param verbose Logical. Whether to print status messages (default: TRUE)
#' @param dropbox_path Character. Path to a Dropbox directory to check (default: NULL)
#' @param access_token Character. Optional Dropbox API access token (default: NULL)
#' @param interactive Logical. Whether to ask for confirmation if status is uncertain (default: TRUE)
#'
#' @return Logical. The value assigned to DROPBOX_SYNC
#'
#' @examples
#' # Initialize DROPBOX_SYNC variable
#' initialize_dropbox_sync()
#'
#' # Initialize with specific path and non-interactive mode
#' initialize_dropbox_sync(dropbox_path = "~/Dropbox/Documents", interactive = FALSE)
#'
#' @export
initialize_dropbox_sync <- function(verbose = TRUE, dropbox_path = NULL, 
                                   access_token = NULL, interactive = TRUE) {
  # Default to assuming sync is on for safety
  sync_status <- TRUE
  
  # If no path provided, try to find a Dropbox path
  if (is.null(dropbox_path)) {
    # Common Dropbox locations
    potential_paths <- c(
      file.path(Sys.getenv("HOME"), "Dropbox"),
      file.path(Sys.getenv("USERPROFILE"), "Dropbox")
    )
    
    for (path in potential_paths) {
      if (dir.exists(path)) {
        dropbox_path <- path
        if (verbose) message("Found Dropbox path: ", dropbox_path)
        break
      }
    }
    
    # If still no path, use current directory if it contains "Dropbox"
    if (is.null(dropbox_path) && grepl("Dropbox", getwd(), ignore.case = TRUE)) {
      dropbox_path <- getwd()
      if (verbose) message("Using current directory as Dropbox path: ", dropbox_path)
    }
    
    # If still no path, we can only check if Dropbox is running
    if (is.null(dropbox_path)) {
      if (verbose) message("No Dropbox path found. Limited status check available.")
      
      is_running <- is_dropbox_running(verbose = verbose)
      
      if (is.na(is_running)) {
        if (verbose) message("Could not determine if Dropbox is running")
        sync_status <- TRUE  # Assume sync is active for safety
      } else if (!is_running) {
        if (verbose) message("Dropbox is not running. Setting DROPBOX_SYNC to FALSE.")
        sync_status <- FALSE
      } else {
        if (verbose) message("Dropbox is running but no Dropbox path was found for detailed checks.")
        
        if (interactive) {
          message("MP080: Is Dropbox sync currently paused? Type 'paused' if yes.")
          message("       Otherwise, enter anything else.")
          response <- readline("Dropbox sync status [paused/active]: ")
          
          if (tolower(response) == "paused") {
            sync_status <- FALSE
            if (verbose) message("Setting DROPBOX_SYNC to FALSE based on user confirmation.")
          } else {
            sync_status <- TRUE
            if (verbose) message("Setting DROPBOX_SYNC to TRUE based on user confirmation.")
          }
        } else {
          if (verbose) message("In non-interactive mode, assuming sync is active for safety.")
          sync_status <- TRUE
        }
      }
      
      # Set the global variable
      assign("DROPBOX_SYNC", sync_status, envir = .GlobalEnv)
      
      if (verbose) {
        message("DROPBOX_SYNC has been set to ", sync_status)
      }
      
      return(sync_status)
    }
  }
  
  # Perform comprehensive check
  if (verbose) message("Performing comprehensive Dropbox status check...")
  status <- check_comprehensive_dropbox_status(
    dropbox_path = dropbox_path,
    access_token = access_token,
    verbose = verbose
  )
  
  if (!status$dropbox_running) {
    # Dropbox is not running, so sync must be off
    if (verbose) message("Dropbox is not running. Setting DROPBOX_SYNC to FALSE.")
    sync_status <- FALSE
  } else {
    # Dropbox is running, determine sync status
    if (status$sync_active) {
      if (verbose) message("Dropbox appears to be actively syncing.")
      
      if (interactive) {
        message("MP080: Database operations require Dropbox sync to be paused.")
        message("       Please pause Dropbox sync now if you plan to work with databases.")
        message("       Type 'paused' when sync is paused, or 'active' to proceed with active sync.")
        response <- readline("Dropbox sync status [paused/active]: ")
        
        if (tolower(response) == "paused") {
          sync_status <- FALSE
          if (verbose) message("Setting DROPBOX_SYNC to FALSE based on user confirmation.")
        } else {
          sync_status <- TRUE
          if (verbose) message("Setting DROPBOX_SYNC to TRUE based on user confirmation.")
          message("MP080 Warning: Database operations with active Dropbox sync may lead to conflicts.")
        }
      } else {
        sync_status <- TRUE
        if (verbose) {
          message("In non-interactive mode with active sync, setting DROPBOX_SYNC to TRUE.")
          message("MP080 Warning: Database operations with active Dropbox sync may lead to conflicts.")
        }
      }
    } else {
      if (verbose) message("Dropbox is running but appears to be paused or inactive.")
      
      if (interactive) {
        message("MP080: Is Dropbox sync currently paused? Our checks suggest it is.")
        message("       Type 'paused' to confirm sync is paused.")
        message("       Type anything else if sync is actually active.")
        response <- readline("Confirm Dropbox sync status [paused/active]: ")
        
        if (tolower(response) == "paused") {
          sync_status <- FALSE
          if (verbose) message("Setting DROPBOX_SYNC to FALSE based on user confirmation.")
        } else {
          sync_status <- TRUE
          if (verbose) message("Setting DROPBOX_SYNC to TRUE based on user correction.")
        }
      } else {
        sync_status <- FALSE
        if (verbose) message("In non-interactive mode with inactive sync, setting DROPBOX_SYNC to FALSE.")
      }
    }
  }
  
  # Set the global variable
  assign("DROPBOX_SYNC", sync_status, envir = .GlobalEnv)
  
  if (verbose) {
    message("DROPBOX_SYNC has been set to ", sync_status)
    if (sync_status) {
      message("MP080 Note: For database operations, you should pause Dropbox sync")
      message("             and then set DROPBOX_SYNC <- FALSE manually.")
    }
  }
  
  return(sync_status)
}

#' @note This file implements MP080 (Database Synchronization) which requires
#' utilities for controlling Dropbox sync during database operations.