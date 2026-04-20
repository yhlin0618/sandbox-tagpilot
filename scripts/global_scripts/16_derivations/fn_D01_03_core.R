#' @title D01_03 Core Function - Customer DNA Analysis (Cross-Company)
#' @description Core logic for DNA analysis, using customer RFM and customer-by-date tables.
#'              Follows DEV_R038: Core Function + Platform Wrapper Pattern.
#'              This function is shared across all companies via git subrepo.
#' @param platform_id Character. Platform identifier (e.g., "cbz", "amz", "eby", "shopify")
#' @param config Optional list. Platform config. If NULL, uses default table patterns.
#' @return List with success status and summary
#' @principle DEV_R038, DEV_R037, DM_R044, MP064, MP145

run_D01_03 <- function(platform_id, config = NULL) {
  # Validate platform_id is provided
  if (missing(platform_id) || is.null(platform_id) || !nzchar(platform_id)) {
    stop("platform_id is required")
  }

  if (is.null(config) && exists("get_platform_config", mode = "function", inherits = TRUE)) {
    config <- tryCatch(get_platform_config(platform_id, warn = FALSE), error = function(e) NULL)
  }

  # ===========================================================================
  # PART 1: INITIALIZE
  # ===========================================================================

  connection_created_processed <- FALSE
  connection_created_cleansed <- FALSE

  # Use a shared environment for variables that need to be modified in tryCatch
  state <- new.env(parent = emptyenv())
  state$error_occurred <- FALSE
  state$test_passed <- FALSE
  state$rows_processed <- 0
  start_time <- Sys.time()
  drv_batch_id <- format(Sys.time(), "%Y%m%d_%H%M%S")
  drv_script_name <- sprintf("%s_D01_03", platform_id)

  # Default table patterns (can be overridden by config)
  input_customer_table_pattern <- if (!is.null(config$input_customer_table_pattern)) {
    config$input_customer_table_pattern
  } else {
    "df_%s_customer_rfm"
  }

  input_by_date_table_pattern <- if (!is.null(config$input_by_date_table_pattern)) {
    config$input_by_date_table_pattern
  } else {
    "df_%s_sales_by_customer_by_date"
  }

  output_dna_table <- if (!is.null(config$output_dna_table)) {
    config$output_dna_table
  } else {
    "df_customer_dna___cleansed"
  }

  # Load analysis_dna function
  analysis_dna_path <- file.path(GLOBAL_DIR, "04_utils", "fn_analysis_dna.R")
  if (file.exists(analysis_dna_path)) {
    source(analysis_dna_path)
  } else {
    stop("Missing required function file: fn_analysis_dna.R")
  }

  if (!exists("analysis_dna", mode = "function")) {
    stop("Missing required function analysis_dna")
  }

  # Load metadata function
  drv_metadata_fn_path <- file.path(GLOBAL_DIR, "04_utils", "fn_add_drv_metadata.R")
  if (file.exists(drv_metadata_fn_path)) source(drv_metadata_fn_path)

  # Establish database connections
  if (!exists("processed_data") || !inherits(processed_data, "DBIConnection")) {
    processed_data <- dbConnectDuckdb(db_path_list$processed_data, read_only = TRUE)
    connection_created_processed <- TRUE
  }

  if (!exists("cleansed_data") || !inherits(cleansed_data, "DBIConnection")) {
    cleansed_data <- dbConnectDuckdb(db_path_list$cleansed_data, read_only = FALSE)
    connection_created_cleansed <- TRUE
  }

  # Helper: Write platform-specific data to shared table
  # Handles schema evolution with try-catch approach
  write_platform_table <- function(con, table_name, data, platform_value) {
    # Ensure customer_id is INTEGER in output (unified ID system)
    # Convert from character if needed (backward compatibility)
    if ("customer_id" %in% names(data)) {
      data$customer_id <- as.integer(data$customer_id)
    }

    if (!DBI::dbExistsTable(con, table_name)) {
      DBI::dbWriteTable(con, table_name, data, overwrite = TRUE)
      return(invisible())
    }

    # Try append approach first - if schema mismatch, drop and recreate
    table_id <- DBI::dbQuoteIdentifier(con, table_name)
    tryCatch({
      DBI::dbExecute(
        con,
        sprintf("DELETE FROM %s WHERE platform_id = ?", table_id),
        params = list(platform_value)
      )
      DBI::dbWriteTable(con, table_name, data, append = TRUE, overwrite = FALSE)
    }, error = function(e) {
      # Schema mismatch - drop and recreate
      message(sprintf("[%s] Schema mismatch detected, recreating table...", platform_value))
      message(sprintf("[%s] Error was: %s", platform_value, e$message))
      DBI::dbRemoveTable(con, table_name)
      DBI::dbWriteTable(con, table_name, data, overwrite = TRUE)
    })
  }

  # Helper: Ensure all DNA fields exist (defensive fallback)
  ensure_dna_fields <- function(df) {
    if (!"cai" %in% names(df)) {
      df$cai <- if ("cai_value" %in% names(df)) df$cai_value else NA_real_
    }
    if (!"cai_value" %in% names(df)) {
      df$cai_value <- if ("cai" %in% names(df)) df$cai else NA_real_
    }
    if (!"dna_m_score" %in% names(df)) {
      df$dna_m_score <- if ("m_ecdf" %in% names(df)) df$m_ecdf else NA_real_
    }
    if (!"dna_f_score" %in% names(df)) {
      df$dna_f_score <- if ("f_ecdf" %in% names(df)) df$f_ecdf else NA_real_
    }
    if (!"dna_r_score" %in% names(df)) {
      df$dna_r_score <- if ("r_ecdf" %in% names(df)) 1 - df$r_ecdf else NA_real_
    }
    if (!"dna_segment" %in% names(df)) {
      m_segment <- dplyr::case_when(
        is.na(df$dna_m_score) ~ NA_character_,
        df$dna_m_score >= 0.75 ~ "M4",
        df$dna_m_score >= 0.50 ~ "M3",
        df$dna_m_score >= 0.25 ~ "M2",
        TRUE ~ "M1"
      )
      f_segment <- dplyr::case_when(
        is.na(df$dna_f_score) ~ NA_character_,
        df$dna_f_score >= 0.67 ~ "F3",
        df$dna_f_score >= 0.33 ~ "F2",
        TRUE ~ "F1"
      )
      r_segment <- dplyr::case_when(
        is.na(df$dna_r_score) ~ NA_character_,
        df$dna_r_score >= 0.75 ~ "R4",
        df$dna_r_score >= 0.50 ~ "R3",
        df$dna_r_score >= 0.25 ~ "R2",
        TRUE ~ "R1"
      )
      df$dna_segment <- dplyr::if_else(
        is.na(m_segment) | is.na(f_segment) | is.na(r_segment),
        NA_character_,
        paste0(m_segment, f_segment, r_segment)
      )
    }
    df
  }

  # ===========================================================================
  # PART 2: MAIN
  # ===========================================================================

  tryCatch({
    customer_rfm_table <- sprintf(input_customer_table_pattern, platform_id)
    sales_by_date_table <- sprintf(input_by_date_table_pattern, platform_id)

    if (!DBI::dbExistsTable(processed_data, customer_rfm_table)) {
      stop(sprintf("Required input table %s not found in processed_data", customer_rfm_table))
    }
    if (!DBI::dbExistsTable(processed_data, sales_by_date_table)) {
      stop(sprintf("Required input table %s not found in processed_data", sales_by_date_table))
    }

    message(sprintf("[%s] MAIN: Loading customer RFM data from %s...", platform_id, customer_rfm_table))
    customer_rfm <- dplyr::tbl(processed_data, customer_rfm_table) |>
      dplyr::collect()

    if (nrow(customer_rfm) == 0) {
      stop("No customer RFM data found in source table")
    }

    message(sprintf("[%s] MAIN: Loading customer-by-date data from %s...", platform_id, sales_by_date_table))
    sales_by_customer_by_date <- dplyr::tbl(processed_data, sales_by_date_table) |>
      dplyr::collect()

    if (nrow(sales_by_customer_by_date) == 0) {
      stop("No customer-by-date data found in source table")
    }

    if (!"platform_id" %in% names(customer_rfm)) {
      customer_rfm$platform_id <- platform_id
    }

    message(sprintf("[%s] MAIN: Running analysis_dna()...", platform_id))
    dna_results <- analysis_dna(
      df_sales_by_customer = customer_rfm,
      df_sales_by_customer_by_date = sales_by_customer_by_date,
      skip_within_subject = TRUE,
      verbose = TRUE
    )

    customer_dna <- dna_results$data_by_customer

    if (is.null(customer_dna) || nrow(customer_dna) == 0) {
      stop("analysis_dna() returned empty results")
    }

    customer_dna <- ensure_dna_fields(customer_dna)

    message(sprintf("[%s] MAIN: DNA analysis completed for %d customers", platform_id, nrow(customer_dna)))

    customer_dna <- customer_dna |>
      dplyr::mutate(
        platform_id = platform_id,
        product_line_id_filter = "all"
      )

    if (exists("add_drv_metadata", mode = "function")) {
      customer_dna <- add_drv_metadata(customer_dna, drv_script_name, drv_batch_id)
    }

    # Convert difftime columns to numeric for DuckDB compatibility
    convert_difftime_to_numeric <- function(df) {
      for (col in names(df)) {
        if (inherits(df[[col]], "difftime")) {
          df[[col]] <- as.numeric(df[[col]], units = "days")
        }
      }
      df
    }
    customer_dna <- convert_difftime_to_numeric(customer_dna)
    # Capture row count before writes (in case writes fail)
    state$rows_processed <- nrow(customer_dna)

    message(sprintf("[%s] MAIN: Writing outputs...", platform_id))

    write_platform_table(cleansed_data, output_dna_table, customer_dna, platform_id)

  }, error = function(e) {
    state$error_occurred <- TRUE
    message(sprintf("[%s] MAIN: ERROR - %s", platform_id, e$message))
  })

  # ===========================================================================
  # PART 3: TEST
  # ===========================================================================

  if (!state$error_occurred) {
    tryCatch({
      if (!DBI::dbExistsTable(cleansed_data, output_dna_table)) {
        stop(sprintf("Output table %s was not created in cleansed_data", output_dna_table))
      }

      dna_sample <- dplyr::tbl(cleansed_data, output_dna_table) |>
        dplyr::filter(platform_id == !!platform_id) |>
        head(5) |>
        dplyr::collect()

      required_cols <- c(
        "customer_id",
        "nes_status",
        "platform_id",
        "product_line_id_filter",
        "dna_m_score",
        "dna_f_score",
        "dna_r_score",
        "cai",
        "dna_segment"
      )
      missing_cols <- setdiff(required_cols, names(dna_sample))

      if (length(missing_cols) > 0) {
        stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
      }

      state$test_passed <- TRUE
      message(sprintf("[%s] TEST: Output table verified", platform_id))

    }, error = function(e) {
      state$test_passed <- FALSE
      message(sprintf("[%s] TEST: ERROR - %s", platform_id, e$message))
    })
  }

  # ===========================================================================
  # PART 4: SUMMARIZE
  # ===========================================================================

  execution_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  summary_report <- list(
    success = !state$error_occurred && state$test_passed,
    platform_id = platform_id,
    rows_processed = state$rows_processed,
    execution_time_secs = execution_time,
    outputs = c(output_dna_table)
  )

  message(sprintf("[%s] SUMMARY: %s", platform_id, ifelse(summary_report$success, "SUCCESS", "FAILED")))
  message(sprintf("[%s] SUMMARY: Rows processed: %d", platform_id, state$rows_processed))
  message(sprintf("[%s] SUMMARY: Execution time (secs): %.2f", platform_id, execution_time))

  # ===========================================================================
  # PART 5: DEINITIALIZE
  # ===========================================================================

  if (connection_created_processed && DBI::dbIsValid(processed_data)) {
    DBI::dbDisconnect(processed_data, shutdown = FALSE)
  }
  if (connection_created_cleansed && DBI::dbIsValid(cleansed_data)) {
    DBI::dbDisconnect(cleansed_data, shutdown = FALSE)
  }

  summary_report
}
