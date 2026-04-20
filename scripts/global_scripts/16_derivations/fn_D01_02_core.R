#' @title D01_02 Core Function - RFM Calculation (Cross-Company)
#' @description Calculates RFM metrics from customer aggregation tables.
#' @param platform_id Character. Platform identifier (e.g., "cbz", "amz", "eby")
#' @param config Optional list. Platform config. If NULL, uses default table patterns.
#' @return List with success status and summary
#' @principle DM_R044, MP064, MP145, DEV_R037, DEV_R038

run_D01_02 <- function(platform_id, config = NULL) {
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
  state <- new.env(parent = emptyenv())
  state$error_occurred <- FALSE
  state$test_passed <- FALSE
  state$rows_processed <- 0
  start_time <- Sys.time()
  drv_batch_id <- format(Sys.time(), "%Y%m%d_%H%M%S")
  drv_script_name <- sprintf("%s_D01_02", platform_id)

  input_table_pattern <- if (!is.null(config$input_table_pattern)) {
    config$input_table_pattern
  } else {
    "df_%s_sales_by_customer"
  }

  output_table_pattern <- if (!is.null(config$output_table_pattern)) {
    config$output_table_pattern
  } else {
    "df_%s_customer_rfm"
  }

  metadata_fn_path <- file.path(GLOBAL_DIR, "04_utils", "fn_add_drv_metadata.R")
  if (file.exists(metadata_fn_path)) source(metadata_fn_path)

  if (!exists("processed_data") || !inherits(processed_data, "DBIConnection")) {
    processed_data <- dbConnectDuckdb(db_path_list$processed_data, read_only = FALSE)
    connection_created_processed <- TRUE
  }

  # ===========================================================================
  # PART 2: MAIN
  # ===========================================================================

  tryCatch({
    input_table <- sprintf(input_table_pattern, platform_id)
    output_table <- sprintf(output_table_pattern, platform_id)

    if (!DBI::dbExistsTable(processed_data, input_table)) {
      stop(sprintf("Required input table %s not found in processed_data", input_table))
    }

    customer_agg <- dplyr::tbl(processed_data, input_table) |>
      dplyr::collect()

    if (nrow(customer_agg) == 0) {
      stop("No customer aggregation data found")
    }

    required_cols <- c(
      "customer_id",
      "sum_sales_by_customer",
      "sum_transactions_by_customer",
      "ipt",
      "min_time_by_date",
      "max_time_by_date"
    )
    missing_cols <- setdiff(required_cols, names(customer_agg))
    if (length(missing_cols) > 0) {
      stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
    }

    if (!"platform_id" %in% names(customer_agg)) {
      customer_agg$platform_id <- platform_id
    }
    if (!"product_line_id_filter" %in% names(customer_agg)) {
      customer_agg$product_line_id_filter <- "all"
    }

    reference_date <- max(customer_agg$max_time_by_date, na.rm = TRUE)
    if (is.infinite(reference_date) || is.na(reference_date)) {
      stop("Unable to determine reference_date from max_time_by_date")
    }

    customer_rfm <- customer_agg |>
      dplyr::mutate(
        r_value = as.numeric(difftime(reference_date, max_time_by_date, units = "days")),
        f_value = as.numeric(sum_transactions_by_customer),
        m_value = dplyr::if_else(sum_transactions_by_customer > 0,
                                 sum_sales_by_customer / sum_transactions_by_customer,
                                 NA_real_),
        customer_tenure_days = as.numeric(difftime(reference_date, min_time_by_date, units = "days"))
      ) |>
      dplyr::mutate(
        r_ecdf = dplyr::percent_rank(desc(r_value)),
        f_ecdf = dplyr::percent_rank(f_value),
        m_ecdf = dplyr::percent_rank(m_value),
        r_label = dplyr::case_when(
          r_ecdf >= 0.67 ~ "Recent Buyer",
          r_ecdf >= 0.33 ~ "Medium Inactive",
          TRUE ~ "Long Inactive"
        ),
        f_label = dplyr::case_when(
          f_ecdf >= 0.67 ~ "High Frequency",
          f_ecdf >= 0.33 ~ "Medium Frequency",
          TRUE ~ "Low Frequency"
        ),
        m_label = dplyr::case_when(
          m_ecdf >= 0.67 ~ "High Value",
          m_ecdf >= 0.33 ~ "Medium Value",
          TRUE ~ "Low Value"
        )
      )

    if (exists("add_drv_metadata", mode = "function")) {
      customer_rfm <- add_drv_metadata(customer_rfm, drv_script_name, drv_batch_id)
    }

    DBI::dbWriteTable(
      processed_data,
      output_table,
      as.data.frame(customer_rfm),
      overwrite = TRUE
    )

    state$rows_processed <- nrow(customer_rfm)

  }, error = function(e) {
    state$error_occurred <- TRUE
    message(sprintf("[%s] MAIN: ERROR - %s", platform_id, e$message))
  })

  # ===========================================================================
  # PART 3: TEST
  # ===========================================================================

  if (!state$error_occurred) {
    tryCatch({
      output_table <- sprintf(output_table_pattern, platform_id)
      if (!DBI::dbExistsTable(processed_data, output_table)) {
        stop(sprintf("Output table %s was not created", output_table))
      }

      sample <- dplyr::tbl(processed_data, output_table) |>
        head(5) |>
        dplyr::collect()

      required_out_cols <- c(
        "customer_id",
        "platform_id",
        "r_value",
        "f_value",
        "m_value",
        "ipt",
        "customer_tenure_days"
      )
      missing_out <- setdiff(required_out_cols, names(sample))
      if (length(missing_out) > 0) {
        stop(sprintf("Missing required columns in output: %s", paste(missing_out, collapse = ", ")))
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
    outputs = c(sprintf(output_table_pattern, platform_id))
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

  summary_report
}
