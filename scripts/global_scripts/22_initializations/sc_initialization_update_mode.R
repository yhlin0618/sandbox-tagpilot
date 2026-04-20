# UPDATE_MODE Initialization --------------------------------------------------
# Minimal initialization for update/batch scripts.

# Ensure autoinit() has been executed -----------------------------------------
if (!exists(".InitEnv") || !is.environment(.InitEnv)) {
  stop("autoinit() must be executed before sourcing sc_initialization_app_mode.R")
}

# Abort if already initialized -------------------------------------------------
if (exists("INITIALIZATION_COMPLETED") && INITIALIZATION_COMPLETED) {
  message("Initialization already completed – skipping.")
}

# -----------------------------------------------------------------------------
# 1. Environment flags
# -----------------------------------------------------------------------------
if (!exists("OPERATION_MODE", envir = .GlobalEnv)) {
  stop("OPERATION_MODE is not defined. Please set OPERATION_MODE before sourcing this script.")
}
options(app.verbose = getOption("app.verbose", FALSE))


# -----------------------------------------------------------------------------
# 2. Load environment variables from .env file
# -----------------------------------------------------------------------------
# Following MP099: Real-time progress reporting
message("UPDATE_MODE: Checking for .env file...")

# Check for .env file in APP_DIR (project root)
env_file <- file.path(APP_DIR, ".env")
if (file.exists(env_file)) {
  # Use dotenv package if available, otherwise use readRenviron
  if (requireNamespace("dotenv", quietly = TRUE)) {
    dotenv::load_dot_env(file = env_file)
    message("UPDATE_MODE: ✅ Environment variables loaded from .env using dotenv package")
  } else {
    readRenviron(env_file)
    message("UPDATE_MODE: ✅ Environment variables loaded from .env using readRenviron")
  }
  
  # Verify critical environment variables for UPDATE_MODE
  # Check for database-related variables
  db_vars_check <- c("EBY_SSH_HOST", "EBY_SQL_HOST", "EBY_SQL_DATABASE")
  for (var in db_vars_check) {
    if (nzchar(Sys.getenv(var))) {
      message(sprintf("UPDATE_MODE: ✓ %s is set", var))
    } else {
      warning(sprintf("UPDATE_MODE: ⚠ %s is not set", var))
    }
  }
} else {
  warning("UPDATE_MODE: No .env file found in ", APP_DIR, " - environment variables may not be available")
}

# -----------------------------------------------------------------------------
# 3. Package initialization
# -----------------------------------------------------------------------------
source(file.path(GLOBAL_DIR, "04_utils", "fn_initialize_packages.R"))
source(file.path(GLOBAL_DIR, "04_utils", "base", "fn_library2.R"))
initialize_packages(mode = OPERATION_MODE,
                    verbose = getOption("update.verbose", FALSE),
                    force_update = FALSE)

# -----------------------------------------------------------------------------
# 4. Load global scripts in deterministic order
# -----------------------------------------------------------------------------
source(file.path(GLOBAL_DIR, "04_utils", "fn_get_r_files_recursive.R"))
load_dirs <- c(
  "14_sql_utils",
  "02_db_utils",
  "04_utils",
  "03_config",
  "01_db",
  "05_etl_utils",
  "06_queries",
  "05_data_processing",
  "07_models",
  "08_ai",
  "09_python_scripts",
  "10_rshinyapp_components",
  "11_rshinyapp_utils",
  "17_transform"
)

# Enhanced debugging for file loading
cat("🔧 Starting to load global scripts...\n")
total_files_loaded <- 0
total_errors <- 0

for (d in load_dirs) {
  dir_path <- file.path(GLOBAL_DIR, d)
  if (!dir.exists(dir_path)) {
    cat("⏭️  Skipping non-existent directory:", d, "\n")
    next
  }
  
  cat("📁 Loading directory:", d, "\n")
  r_files <- sort(get_r_files_recursive(dir_path))
  
  if (length(r_files) == 0) {
    cat("   ℹ️  No R files found in", d, "\n")
    next
  }
  
  cat("   📋 Found", length(r_files), "R files\n")
  
  # Load each file with individual error handling
  for (file_path in r_files) {
    tryCatch({
      cat("   🔄 Loading:", basename(file_path), "...")
      source(file_path, local = FALSE)
      cat(" ✅\n")
      total_files_loaded <- total_files_loaded + 1
    }, error = function(e) {
      cat(" ❌\n")
      cat("   ⚠️  ERROR in file:", file_path, "\n")
      cat("   📄 Error message:", e$message, "\n")
      cat("   🔍 Call stack:", deparse(e$call), "\n")
      total_errors <- total_errors + 1
      
      # Continue loading other files instead of stopping
      warning("Failed to load: ", file_path, " - ", e$message)
    })
  }
  cat("   ✅ Completed directory:", d, "\n\n")
}

cat("📊 Loading Summary:\n")
cat("   ✅ Successfully loaded:", total_files_loaded, "files\n")
cat("   ❌ Failed to load:", total_errors, "files\n")
if (total_errors > 0) {
  cat("   ⚠️  Check warnings() for detailed error information\n")
}
cat("🎯 Global scripts loading completed!\n\n")

# -----------------------------------------------------------------------------
# 5. Finalize
# -----------------------------------------------------------------------------
INITIALIZATION_COMPLETED <- TRUE
message("UPDATE_MODE initialization finished. Databases available: ",
        paste(names(db_path_list), collapse = ", "))

