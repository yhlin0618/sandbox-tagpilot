#' Connect to the application database
#'
#' @description
#' Establishes a connection to the DuckDB database used by the application.
#' Supports dual-mode: DuckDB locally, Parquet on Posit Connect.
#'
#' @param db_path Path to the DuckDB database file
#' @param parquet_dir Path to parquet directory for deployment mode
#' @return A database connection object
#' @export
#' @implements MP45 Automatic Data Availability Detection
#' @implements DM_R056 Posit Connect Deployment Assets
connect_to_app_database <- function(
    db_path = "data/app_data/app_data.duckdb",
    parquet_dir = "data/app_data/parquet"
) {
  tryCatch({
    if (!requireNamespace("DBI", quietly = TRUE)) {
      message("Installing DBI package...")
      install.packages("DBI")
    }

    if (!requireNamespace("duckdb", quietly = TRUE)) {
      message("Installing duckdb package...")
      install.packages("duckdb")
    }

    # Check if DuckDB file exists and is valid (not LFS pointer)
    duckdb_valid <- FALSE
    if (file.exists(db_path)) {
      file_size <- file.info(db_path)$size
      # LFS pointer files are ~130-150 bytes; valid DuckDB should be >1KB
      if (file_size > 1000) {
        duckdb_valid <- TRUE
      } else {
        message("DuckDB file appears to be LFS pointer (", file_size, " bytes), switching to Parquet mode")
      }
    }

    # Mode 1: Use DuckDB directly (local development)
    if (duckdb_valid) {
      conn <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
      message("Successfully connected to DuckDB: ", db_path)
      return(conn)
    }

    # Mode 2: Use Parquet files (Posit Connect deployment)
    if (dir.exists(parquet_dir)) {
      parquet_files <- list.files(parquet_dir, pattern = "\\.parquet$", full.names = TRUE)

      if (length(parquet_files) == 0) {
        stop("Parquet directory exists but contains no .parquet files: ", parquet_dir)
      }

      message("Using Parquet mode with ", length(parquet_files), " files from: ", parquet_dir)

      # Create in-memory DuckDB and register parquet files as views
      conn <- DBI::dbConnect(duckdb::duckdb())

      for (pf in parquet_files) {
        table_name <- tools::file_path_sans_ext(basename(pf))
        query <- sprintf("CREATE VIEW %s AS SELECT * FROM read_parquet('%s')", table_name, pf)
        DBI::dbExecute(conn, query)
      }

      message("Successfully connected via Parquet files")
      return(conn)
    }

    # Neither available
    stop(
      "No app_data source found!\n",
      "  - DuckDB: ", db_path, " (not found or invalid)\n",
      "  - Parquet: ", parquet_dir, " (not found)\n",
      "For deployment: Run export_app_data_to_parquet.R first"
    )

  }, error = function(e) {
    message("Error connecting to database: ", e$message)
    return(NULL)
  })
}
