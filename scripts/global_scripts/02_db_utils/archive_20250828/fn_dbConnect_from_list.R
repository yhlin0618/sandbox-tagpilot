#' @file fn_dbConnect_from_list.R
#' @use_package DBI
#' @use_package duckdb
#' @use_package zip
#' @note Depends 11_rshinyapp_utils/fn_check_data_access.R
#' @note Depends 02_db_utils/db_utils.R
#' @note Depends 04_utils/fn_dropbox_sync.R
#' @note Implements MP080 (Database Synchronization)
#' 
#' @title Database Connection Function with Permission Checking and Synchronization Control
#'
#' Establishes a connection to a specified DuckDB database from the predefined list
#' and stores the connection in the global environment. This function enforces
#' the Data Source Hierarchy Principle by checking permissions based on current
#' operating mode. It also implements MP080 Database Synchronization for handling
#' Dropbox sync and creating database backups.
#'
#' @param dataset Character. The name of the database to connect to (must exist in path_list)
#' @param path_list List. The list of database paths (defaults to db_path_list)
#' @param read_only Logical. Whether to open the database in read-only mode. This can be overridden
#'        based on operating mode access permissions.
#' @param create_dir Logical. Whether to create parent directories if they don't exist (defaults to TRUE)
#' @param verbose Logical. Whether to display information about the connection (defaults to TRUE)
#' @param force_mode_check Logical. Whether to force permission checking even for custom paths (defaults to TRUE)
#' @param sync_control Logical. Whether to implement Dropbox sync control per MP080 (defaults to TRUE)
#' @param auto_backup Logical. Whether to create a backup when closing the connection (defaults to TRUE)
#'
#' @return Connection object. The established database connection
#'
#' @details
#' The function performs the following steps:
#' 1. If sync_control=TRUE, prompts user to pause Dropbox synchronization
#' 2. Validates that the requested dataset exists in the path list
#' 3. Gets the database path from the list
#' 4. Determines data layer and checks permission based on operating mode
#' 5. Creates parent directories if needed and requested (if permissions allow)
#' 6. Establishes a connection to the DuckDB database with appropriate read/write permissions
#' 7. Displays all tables in the connected database if verbose is TRUE
#' 8. Assigns the connection to a global variable with the same name as the dataset
#' 9. Returns the connection object for chaining
#'
#' @note
#' - The connection is stored in the global environment with the same name as the dataset
#' - This allows other functions to access the connection without explicitly passing it
#' - Connections should be closed with dbDisconnect_from_list() when no longer needed
#' - For multiple connections, use dbDisconnect_all() to close all at once
#' - Permission checks enforce Data Source Hierarchy Principle by controlling read/write access
#' - The function implements MP080 Database Synchronization for Dropbox sync control
#' - When sync_control=TRUE, the user must manually pause Dropbox sync before operation
#' - When auto_backup=TRUE, a timestamped backup is created in database_backup.zip
#'
#' @examples
#' # Connect to raw_data database - permission checks will enforce appropriate mode
#' raw_data <- fn_dbConnect_from_list("raw_data")
#'
#' # Connect to app_data database - in APP_MODE this will enforce read_only=TRUE
#' app_data <- dbConnect_from_list("app_data", read_only = FALSE)
#'
#' # Connect with a custom path list
#' custom_paths <- list(my_db = "path/to/custom.duckdb")
#' my_db <- fn_dbConnect_from_list("my_db", path_list = custom_paths)
#'
#' # Connect without Dropbox sync control
#' raw_data <- fn_dbConnect_from_list("raw_data", sync_control = FALSE)
#'
#' @export
dbConnect_from_list <- function(dataset, path_list = db_path_list, read_only = FALSE, 
                                create_dir = TRUE, verbose = TRUE, force_mode_check = TRUE,
                                sync_control = FALSE, auto_backup = TRUE) {
  # ---------------------------------------------------------------------------
  # Deprecation notice
  # ---------------------------------------------------------------------------
  warning("dbConnect_from_list() 已棄用，將於未來版本移除，請改用 dbConnectDuckdb() 或 dbConnectFromList()。",
          call. = FALSE, immediate. = TRUE)
  # MP080: 實作 Dropbox 同步控制 - simplified implementation
  if (sync_control) {
    # Only check if DROPBOX_SYNC is already defined and FALSE
    if (exists("DROPBOX_SYNC", envir = .GlobalEnv) && !DROPBOX_SYNC) {
      if (verbose) {
        message("MP080 Database Synchronization: DROPBOX_SYNC is FALSE, assuming sync is already paused")
      }
      sync_confirmed <- TRUE
    } else {
      # Simple manual confirmation
      message("MP080 Database Synchronization: Please manually pause Dropbox synchronization now")
      message("IMPORTANT: Pause Dropbox sync to prevent conflicts while database is in use")
      message("(Set DROPBOX_SYNC <- FALSE in global environment to skip this prompt)")
      user_confirmation <- readline("Type 'confirmed' when Dropbox sync is paused: ")
      
      sync_confirmed <- tolower(user_confirmation) == "confirmed"
      if (!sync_confirmed) {
        stop("Database connection aborted: Dropbox sync control is required per MP080")
      } else {
        # User confirmed they paused Dropbox sync, set global flag to FALSE
        # This will be preserved throughout the session
        if (verbose) message("Setting DROPBOX_SYNC to FALSE based on confirmation")
        assign("DROPBOX_SYNC", FALSE, envir = .GlobalEnv)
      }
    }
    
    # 將同步控制狀態記錄在隱藏變數中，供後續關閉連線時使用
    sync_control_var_name <- paste0(".sync_control_", dataset)
    assign(sync_control_var_name, TRUE, envir = .GlobalEnv)
    
    # 記錄開始時間
    sync_start_time_var <- paste0(".sync_start_time_", dataset)
    assign(sync_start_time_var, Sys.time(), envir = .GlobalEnv)
    
    if (verbose) {
      message("Dropbox sync control activated at ", format(get(sync_start_time_var), "%Y-%m-%d %H:%M:%S"))
    }
  }
  
  # 紀錄是否需要備份
  backup_var_name <- paste0(".auto_backup_", dataset)
  assign(backup_var_name, auto_backup, envir = .GlobalEnv)
  
  # 檢查指定的 dataset 是否存在於 path_list 中
  if (!dataset %in% names(path_list)) {
    stop("指定的 dataset '", dataset, "' 不在清單中。請確認清單中的名稱：", paste(names(path_list), collapse = ", "))
  }
  
  # 取得指定 dataset 的資料庫路徑
  db_path <- path_list[[dataset]]
  
  # 確定資料存取層級（data layer）並檢查存取權限
  # 檢查是否已載入存取權限檢查功能 
  # 注意：如果在 APP_MODE 下，check_data_access 可能尚未載入（因為它在 11_rshinyapp_utils 中）
  has_permission_check <- exists("check_data_access", mode = "function") && 
                          exists("get_data_layer", mode = "function")
                          
  # 如果函數不存在但目前在 APP_MODE，實作簡化版的權限檢查
  if (!has_permission_check && exists("OPERATION_MODE") && OPERATION_MODE == "APP_MODE") {
    # APP_MODE 下的簡化存取規則：強制資料庫為唯讀模式
    if (!read_only) {
      if (verbose) {
        message("注意：在 APP_MODE 中，資料庫連線被強制設為唯讀模式。")
      }
      read_only <- TRUE
    }
  } else if (dataset == "cleanse_data") {
    # Handle legacy cleanse_data name
    warning("'cleanse_data' is deprecated, use 'cleansed_data' instead. Redirecting connection.")
    dataset <- "cleansed_data"
    # Update the global environment to maintain backward compatibility
    if (verbose) message("Redirecting cleanse_data to cleansed_data for compatibility")
  }
  
  # 根據資料庫名稱判斷資料層級
  data_layer <- NULL
  if (has_permission_check) {
    # 通用對應規則
    layer_mapping <- list(
      "app" = c("app_data"),
      "processing" = c("raw_data", "cleansed_data", "cleanse_data", "processed_data", "sales_by_customer_date_data", 
                      "slowly_changing_data", "comment_property_rating"),
      "global" = c("global_scd_type1")
    )
    
    # 嘗試從資料庫名稱判斷層級
    for (layer in names(layer_mapping)) {
      if (dataset %in% layer_mapping[[layer]]) {
        data_layer <- layer
        break
      }
    }
    
    # 若無法從名稱判斷，則嘗試從路徑判斷
    if (is.null(data_layer) && force_mode_check) {
      data_layer <- get_data_layer(db_path)
    }
    
    # 根據存取類型（讀/寫）檢查權限
    access_type <- if (read_only) "read" else "write"
    
    # 檢查權限
    if (!is.null(data_layer)) {
      has_permission <- check_data_access(data_layer, access_type, db_path)
      
      # 如果無寫入權限但請求寫入，則強制唯讀模式
      if (!has_permission && access_type == "write") {
        if (verbose) {
          message("注意：根據目前操作模式的存取權限，資料庫 '", dataset, "' (",
                 data_layer, " 層) 被強制設為唯讀模式。")
        }
        read_only <- TRUE
      }
      
      # 如果連讀取權限都沒有，則中止操作
      if (!has_permission && access_type == "read") {
        stop("存取被拒絕：目前操作模式下無法存取資料庫 '", dataset, "' (",
             data_layer, " 層).")
      }
    }
  }
  
  # 檢查資料庫目錄是否存在
  db_dir <- dirname(db_path)
  if (!dir.exists(db_dir) && db_dir != ".") {
    # 創建目錄需要寫入權限，檢查是否有權限
    if (has_permission_check && !is.null(data_layer) && 
        !check_data_access(data_layer, "write", db_dir) && force_mode_check) {
      stop("無法創建資料庫目錄 '", db_dir, "' - 目前操作模式無寫入權限")
    }
    
    if (create_dir) {
      if (verbose) message("資料庫目錄 '", db_dir, "' 不存在。正在創建...")
      dir.create(db_dir, recursive = TRUE, showWarnings = FALSE)
      if (!dir.exists(db_dir)) {
        stop("無法創建資料庫目錄 '", db_dir, "'")
      } else if (verbose) {
        message("成功創建資料庫目錄 '", db_dir, "'")
      }
    } else {
      stop("資料庫目錄 '", db_dir, "' 不存在，且 create_dir=FALSE")
    }
  }
  
  # 連線到 DuckDB
  tryCatch({
    # 使用新版封裝函式建立連線
    con <- dbConnectDuckdb(db_path = db_path, read_only = read_only)
  }, error = function(e) {
    stop("連接到資料庫 '", dataset, "' 時發生錯誤: ", e$message, 
         "\n路徑: ", db_path, 
         "\n讀取模式: ", ifelse(read_only, "唯讀", "可寫入"))
  })
  
  if (verbose) {
    # 列印目前連線到的資料庫中的所有資料表
    table_count <- length(DBI::dbListTables(con))
    access_mode_suffix <- ""
    if (has_permission_check && !is.null(data_layer)) {
      access_mode_suffix <- paste0("（", 
                                 if (exists("OPERATION_MODE")) OPERATION_MODE else "未知模式", 
                                 "，", data_layer, " 層）")
    }
    
    message("目前連線到 '", dataset, "' 資料庫，",
            ifelse(read_only, "唯讀模式", "寫入模式"), "，",
            "包含 ", table_count, " 個資料表. ", access_mode_suffix)
    
    if (table_count > 0) {
      print(DBI::dbListTables(con))
    } else {
      message("（資料庫中沒有任何資料表）")
    }
  }
  
  # 將連線物件存入全域變數，變數名稱與 dataset 相同
  assign(dataset, con, envir = .GlobalEnv)
  
  # 回傳連線物件
  return(con)
}

#' @title Disconnect from database and handle synchronization
#'
#' @description
#' Disconnects from a database connection created with dbConnect_from_list.
#' If sync_control was enabled when creating the connection, this function
#' will prompt the user to resume Dropbox synchronization. If auto_backup
#' was enabled, it will create a timestamped backup of the database.
#'
#' @param dataset Character. The name of the database to disconnect from
#' @param con Connection object. The database connection (if NULL, retrieves from global environment)
#' @param force_backup Logical. Whether to force a backup even if auto_backup was not set (defaults to FALSE)
#' @param verbose Logical. Whether to display information about the operations (defaults to TRUE)
#'
#' @return Logical. TRUE if disconnection was successful
#'
#' @examples
#' # Connect to database
#' raw_data <- dbConnect_from_list("raw_data")
#' 
#' # Perform operations...
#' 
#' # Disconnect and handle sync control
#' dbDisconnect_from_list("raw_data")
#'
#' @export
dbDisconnect_from_list <- function(dataset, con = NULL, force_backup = FALSE, verbose = TRUE) {
  # Get the connection if not provided
  if (is.null(con)) {
    if (!exists(dataset, envir = .GlobalEnv)) {
      if (verbose) {
        warning("No connection found for '", dataset, "' in global environment")
      }
      return(FALSE)
    }
    con <- get(dataset, envir = .GlobalEnv)
  }
  
  # Check if valid connection
  if (!inherits(con, "DBIConnection") || !DBI::dbIsValid(con)) {
    if (verbose) {
      warning("Invalid connection for '", dataset, "'")
    }
    return(FALSE)
  }
  
  # Close the connection
  DBI::dbDisconnect(con)
  if (verbose) {
    message("Database connection to '", dataset, "' closed")
  }
  
  # Check if backup is needed
  backup_var_name <- paste0(".auto_backup_", dataset)
  do_backup <- force_backup
  if (exists(backup_var_name, envir = .GlobalEnv)) {
    do_backup <- do_backup || get(backup_var_name, envir = .GlobalEnv)
    rm(list = backup_var_name, envir = .GlobalEnv)
  }
  
  # Create backup if needed
  if (do_backup) {
    backup_path <- create_database_backup(dataset, path_list = db_path_list, verbose = verbose)
    if (!is.null(backup_path) && verbose) {
      message("Database backup created at: ", backup_path)
    }
  }
  
  # Check if sync control was used
  sync_control_var_name <- paste0(".sync_control_", dataset)
  if (exists(sync_control_var_name, envir = .GlobalEnv) && get(sync_control_var_name, envir = .GlobalEnv)) {
    # Get sync start time
    sync_start_time_var <- paste0(".sync_start_time_", dataset)
    start_time <- if (exists(sync_start_time_var, envir = .GlobalEnv)) {
      get(sync_start_time_var, envir = .GlobalEnv)
    } else {
      NULL
    }
    
    # Check if we need to prompt for resuming Dropbox sync
    # If DROPBOX_SYNC is FALSE, we don't need to ask the user to resume sync
    if (exists("DROPBOX_SYNC", envir = .GlobalEnv) && !DROPBOX_SYNC) {
      if (verbose) {
        message("MP080 Database Synchronization: Database connection closed")
        if (!is.null(start_time)) {
          duration <- difftime(Sys.time(), start_time, units = "mins")
          message(sprintf("Dropbox sync control was active for %.1f minutes", as.numeric(duration)))
        }
        message("DROPBOX_SYNC is still FALSE - no Dropbox sync resume needed")
        message("When you're ready to resume Dropbox sync, manually set: DROPBOX_SYNC <- TRUE")
      }
    } else {
      # Need to prompt for sync resume
      message("MP080 Database Synchronization: Database connection closed")
      if (!is.null(start_time)) {
        duration <- difftime(Sys.time(), start_time, units = "mins")
        message(sprintf("Dropbox sync was paused for %.1f minutes", as.numeric(duration)))
      }
      message("IMPORTANT: You may now resume Dropbox synchronization")
      message("(Set DROPBOX_SYNC <- TRUE in global environment after resuming sync)")
      user_confirmation <- readline("Type 'confirmed' when Dropbox sync is resumed: ")
      
      if (tolower(user_confirmation) != "confirmed") {
        warning("Please remember to resume Dropbox synchronization!")
      } else if (verbose) {
        message("Dropbox synchronization resumed at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
        # Update global flag only if not FALSE (preserve user setting)
        if (!exists("DROPBOX_SYNC", envir = .GlobalEnv) || DROPBOX_SYNC) {
          assign("DROPBOX_SYNC", TRUE, envir = .GlobalEnv)
        } else if (verbose) {
          message("Preserving DROPBOX_SYNC=FALSE setting for future database operations")
        }
      }
    }
    
    # Clean up sync control variables
    # Note: We clean up only the session variables, not the global DROPBOX_SYNC
    # so that it persists between sessions as the user expects
    if (exists(sync_control_var_name, envir = .GlobalEnv)) {
      rm(list = sync_control_var_name, envir = .GlobalEnv)
    }
    if (exists(sync_start_time_var, envir = .GlobalEnv)) {
      rm(list = sync_start_time_var, envir = .GlobalEnv)
    }
  }
  
  # Remove connection from global environment
  if (exists(dataset, envir = .GlobalEnv)) {
    rm(list = dataset, envir = .GlobalEnv)
  }
  
  return(TRUE)
}

#' @title Create database backup
#'
#' @description
#' Creates a backup of a database in the ROOT_DIR directory
#' with a consistent naming convention.
#'
#' @param dataset Character. The name of the database to backup
#' @param path_list List. The list of database paths (defaults to db_path_list)
#' @param verbose Logical. Whether to display information about the operations (defaults to TRUE)
#'
#' @return Character. The path to the created backup file, or NULL if backup failed
#'
#' @details
#' Implements MP080 Database Synchronization by creating database backups
#' that can be synchronized via Dropbox.
#'
#' @export
create_database_backup <- function(dataset, path_list = db_path_list, verbose = TRUE) {
  # Check if dataset exists in path_list
  if (!dataset %in% names(path_list)) {
    if (verbose) {
      warning("Dataset '", dataset, "' not found in path list")
    }
    return(NULL)
  }
  
  # Get database path
  db_path <- path_list[[dataset]]
  if (!file.exists(db_path)) {
    if (verbose) {
      warning("Database file not found at '", db_path, "'")
    }
    return(NULL)
  }
  
  # Use ROOT_DIR as the backup location
  backup_root <- if (exists("ROOT_DIR")) {
    get("ROOT_DIR")
  } else {
    # Fallback if ROOT_DIR is not defined
    if (verbose) {
      warning("ROOT_DIR not found, attempting to detect project root")
    }
    
    # Try to find project root by looking for specific directories
    current_dir <- getwd()
    while (current_dir != dirname(current_dir)) {
      if (file.exists(file.path(current_dir, "precision_marketing_app"))) {
        current_dir
        break
      }
      current_dir <- dirname(current_dir)
    }
    
    if (current_dir == dirname(current_dir)) {
      # If not found, use parent directory of the database
      if (verbose) {
        warning("Project root not detected, using parent directory of database")
      }
      dirname(dirname(db_path))
    } else {
      current_dir
    }
  }
  
  # Create backup directory (using the dataset name)
  backup_dir <- file.path(backup_root, paste0(dataset, "_backup.zip"))
  if (!dir.exists(backup_dir)) {
    dir.create(backup_dir, recursive = TRUE)
    if (!dir.exists(backup_dir)) {
      if (verbose) {
        warning("Failed to create backup directory at '", backup_dir, "'")
      }
      return(NULL)
    }
  }
  
  # Create backup filename (without timestamp)
  backup_file <- file.path(backup_dir, paste0(dataset, "_backup.zip"))
  
  # Create zip archive, replacing any existing file
  tryCatch({
    if (requireNamespace("zip", quietly = TRUE)) {
      zip::zip(backup_file, db_path, mode = "cherry-pick")
    } else {
      # Fallback to system zip if package not available
      if (file.exists(backup_file)) {
        unlink(backup_file)
      }
      system2("zip", c("-q", shQuote(backup_file), shQuote(db_path)))
    }
    
    if (!file.exists(backup_file)) {
      if (verbose) {
        warning("Failed to create backup file at '", backup_file, "'")
      }
      return(NULL)
    } else if (verbose) {
      message("Created/updated backup file at '", backup_file, "'")
    }
  }, error = function(e) {
    if (verbose) {
      warning("Error creating backup: ", e$message)
    }
    return(NULL)
  })
  
  return(backup_file)
}

#' @note
#' This function follows R0103 (Dependency-Based Sourcing) which requires 
#' explicit dependency annotations. The dependencies are:
#' - DBI package - Required for database interface functions
#' - duckdb package - Required for DuckDB connections
#' - zip package - Required for creating backup archives
#' - fn_check_data_access.R - Required for permission validation
#' - db_utils.R - Required for database path definitions
#' 
#' @note
#' This function implements MP080 (Database Synchronization) which requires:
#' - Manual Dropbox sync control before and after database operations
#' - Automatic timestamped database backups to prevent data loss

# -----------------------------------------------------------------------------
# CamelCase wrappers for naming consistency
# -----------------------------------------------------------------------------

#' @export
dbConnectFromList <- function(...) {
  dbConnect_from_list(...)
}

#' @export
dbDisconnectFromList <- function(...) {
  dbDisconnect_from_list(...)
}
