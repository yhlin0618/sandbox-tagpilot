#' getAppDataConnection
#'
#' Dual-mode connection for app_data: DuckDB locally, Parquet on Posit Connect.
#'
#' This function automatically detects the deployment environment:
#' - Local development: Uses app_data.duckdb directly
#' - Posit Connect: Uses Parquet files (creates VIEWs for SQL compatibility)
#'
#' The detection logic:
#' 1. If DuckDB file exists AND is valid (not LFS pointer, >1KB), use DuckDB
#' 2. Otherwise, if parquet directory exists, use Parquet files
#' 3. If neither, error with clear message
#'
#' @param duckdb_path Character. Path to DuckDB file. Default: "data/app_data/app_data.duckdb"
#' @param parquet_dir Character. Path to parquet directory. Default: "data/app_data/parquet"
#' @param verbose Logical. Print connection mode message. Default: TRUE
#'
#' @return DBI connection object with tables/views accessible
#'
#' @details
#' Following Principles:
#'   - DM_R056: Posit Connect Deployment Assets
#'   - DEV_R039: Side-Effect-Free Function Files
#'   - SO_R007: One Function One File
#'
#' @export
#' @importFrom DBI dbConnect dbExecute
#' @use_package DBI
#' @use_package duckdb

getAppDataConnection <- function(
    duckdb_path = "data/app_data/app_data.duckdb",
    parquet_dir = "data/app_data/parquet",
    verbose = TRUE
) {

  # Check if DuckDB file exists and is valid (not LFS pointer)
  duckdb_valid <- FALSE
  if (file.exists(duckdb_path)) {
    file_size <- file.info(duckdb_path)$size
    # LFS pointer files are typically ~130-150 bytes
    # A valid DuckDB file should be much larger (at least 1KB for minimal data)
    if (file_size > 1000) {
      duckdb_valid <- TRUE
    } else if (verbose) {
      message("DuckDB file exists but appears to be LFS pointer (", file_size, " bytes)")
    }
  }

  # Mode 1: Use DuckDB directly
  if (duckdb_valid) {
    if (verbose) {
      message("Using DuckDB file: ", duckdb_path)
    }

    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = duckdb_path, read_only = TRUE)
    attr(con, "connection_type") <- "app_data_duckdb"
    attr(con, "connection_time") <- Sys.time()
    return(con)
  }

  # Mode 2: Use Parquet files
  if (dir.exists(parquet_dir)) {
    parquet_files <- list.files(parquet_dir, pattern = "\\.parquet$", full.names = TRUE)

    if (length(parquet_files) == 0) {
      stop("Parquet directory exists but contains no .parquet files: ", parquet_dir)
    }

    if (verbose) {
      message("Using Parquet files from: ", parquet_dir)
      message("  Found ", length(parquet_files), " parquet files")
    }

    # Create in-memory DuckDB and register parquet files as views
    con <- DBI::dbConnect(duckdb::duckdb())

    for (pf in parquet_files) {
      table_name <- tools::file_path_sans_ext(basename(pf))
      # Use read_parquet to create a view
      query <- sprintf("CREATE VIEW %s AS SELECT * FROM read_parquet('%s')", table_name, pf)
      DBI::dbExecute(con, query)

      if (verbose) {
        message("    - Registered view: ", table_name)
      }
    }

    attr(con, "connection_type") <- "app_data_parquet"
    attr(con, "connection_time") <- Sys.time()
    attr(con, "parquet_tables") <- tools::file_path_sans_ext(basename(parquet_files))
    return(con)
  }

  # Mode 3: Neither available - error

  stop(
    "No app_data source found!\n",
    "  - DuckDB path: ", duckdb_path, " (", if (file.exists(duckdb_path)) "exists but invalid" else "not found", ")\n",
    "  - Parquet dir: ", parquet_dir, " (", if (dir.exists(parquet_dir)) "empty" else "not found", ")\n\n",
    "For local development: Ensure app_data.duckdb exists\n",
    "For deployment: Run export script to create parquet files"
  )
}
