#' Import Amazon Sales Data from Excel Files
#'
#' This function imports Amazon sales data from Excel files into the DuckDB database.
#' It processes all Excel files in a specified folder and its subfolders, 
#' performs basic data cleaning and validation, and appends the data to 
#' the df_amazon_sales table.
#'
#' @param folder_path Character string. Path to the folder containing Amazon sales Excel files.
#' @param connection DBI connection object. An active connection to a DuckDB database.
#' @param clean_columns Logical. Whether to apply standardized column name cleaning. Default is TRUE.
#' @param overwrite Logical. Whether to overwrite the existing table or append to it. Default is FALSE (append).
#' @param verbose Logical. Whether to print detailed processing information. Default is TRUE.
#'
#' @return The database connection object for chaining operations.
#'
#' @details 
#' The function finds all Excel files (.xlsx or .xls) in the provided folder and its subfolders,
#' then attempts to read and process each file. It performs the following operations:
#' 1. Standardizes column names to snake_case
#' 2. Validates that required columns (sku, purchase_date) exist
#' 3. Converts date columns to proper date/time format
#' 4. Appends or overwrites data to the amazon_sales_dta table
#'
#' If errors occur during processing, warning messages are displayed but the function continues
#' with the next file.
#'
#' @examples
#' \dontrun{
#' # Connect to a DuckDB database
#' con <- dbConnect(duckdb::duckdb(), dbdir = "path/to/database.duckdb")
#'
#' # Import Amazon sales data
#' import_df_amazon_sales("path/to/amazon_sales_files", con)
#'
#' # Import and overwrite existing data
#' import_df_amazon_sales("path/to/amazon_sales_files", con, overwrite = TRUE)
#' }
#'
#' @export
import_df_amazon_sales <- function(folder_path, connection, clean_columns = TRUE, 
                                    overwrite = FALSE, verbose = TRUE) {
  # Input validation
  if (!dir.exists(folder_path)) {
    stop("Folder path does not exist: ", folder_path)
  }
  
  if (!inherits(connection, "DBIConnection")) {
    stop("Connection must be a DBI database connection")
  }
  
  # Check if table exists
  table_exists <- DBI::dbExistsTable(connection, "df_amazon_sales")
  
  # If overwriting and table exists, drop it
  if (overwrite && table_exists) {
    if (verbose) message("Dropping existing df_amazon_sales table")
    DBI::dbExecute(connection, "DROP TABLE IF EXISTS df_amazon_sales")
    table_exists <- FALSE
  }
  
  # Find all Excel files in the folder and subfolder
  all_files <- list.files(folder_path, pattern = "\\.(xlsx|xls)$", 
                         recursive = TRUE, full.names = TRUE)
  
  # If no files found, return with a message
  if (length(all_files) == 0) {
    message("No Excel files found in ", folder_path)
    return(connection)
  }
  
  # Print the number of files found
  if (verbose) message("Found ", length(all_files), " Excel files to process")
  
  # Process each file
  imported_count <- 0
  total_rows <- 0
  skipped_files <- 0
  
  for (file_path in all_files) {
    # Try to read the Excel file
    tryCatch({
      if (verbose) message("Importing: ", basename(file_path))
      
      # Read the Excel file
      data <- readxl::read_excel(file_path)
      
      # Skip empty files
      if (nrow(data) == 0) {
        if (verbose) message("  Skipping empty file")
        skipped_files <- skipped_files + 1
        next
      }
      
      # Clean column names if requested
      if (clean_columns) {
        # First convert all column names to lowercase
        names(data) <- tolower(names(data))
        
        # Replace hyphens with underscores
        names(data) <- gsub("-", "_", names(data))
        
        # Replace spaces with underscores
        names(data) <- gsub(" ", "_", names(data))
        
        # Remove any non-alphanumeric characters (except underscores)
        names(data) <- gsub("[^a-z0-9_]", "", names(data))
      }
      
      # Check for required columns
      required_cols <- c("sku", "purchase_date")
      missing_cols <- setdiff(required_cols, names(data))
      
      if (length(missing_cols) > 0) {
        warning("Missing required columns in file ", basename(file_path), ": ", 
              paste(missing_cols, collapse = ", "))
        skipped_files <- skipped_files + 1
        next
      }
      
      # Format date columns if they exist
      date_cols <- c("purchase_date", "payments_date", "shipment_date", 
                    "reporting_date", "estimated_arrival_date")
      for (col in intersect(date_cols, names(data))) {
        if (!inherits(data[[col]], "POSIXct")) {
          data[[col]] <- as.POSIXct(data[[col]])
        }
      }
      
      # Check if table doesn't exist yet (first import)
      if (!table_exists) {
        if (verbose) message("  Creating new df_amazon_sales table")
        DBI::dbWriteTable(connection, "df_amazon_sales", data, 
                          row.names = FALSE)
        table_exists <- TRUE
      } else {
        # Append data to the existing table
        DBI::dbWriteTable(connection, "df_amazon_sales", data, 
                         append = TRUE, row.names = FALSE)
      }
      
      # Increment counters
      imported_count <- imported_count + 1
      total_rows <- total_rows + nrow(data)
      if (verbose) message("  Successfully imported with ", nrow(data), " rows")
      
    }, error = function(e) {
      warning("Error processing file ", basename(file_path), ": ", e$message)
      skipped_files <- skipped_files + 1
    })
  }
  
  # Summary message
  if (verbose) {
    message("Import summary:")
    message("  - Files processed: ", length(all_files))
    message("  - Files successfully imported: ", imported_count)
    message("  - Files skipped or failed: ", skipped_files)
    message("  - Total rows imported: ", total_rows)
  }
  
  # Return the connection for chaining
  return(connection)
}

#' Process Amazon sales data
#'
#' Processes raw Amazon sales data from the database, performs transformations,
#' and writes the processed data to a destination table.
#'
#' @param raw_data DBI connection. Connection to the database containing raw data.
#' @param Data DBI connection. Connection to the database where processed data will be stored.
#' @param verbose Logical. Whether to display progress messages. Default is TRUE.
#'
#' @return Invisibly returns the Data connection for chaining.
#'
#' @details
#' This function performs the following operations:
#' 1. Filters records with valid email addresses
#' 2. Extracts customer_id from buyer_email
#' 3. Renames columns for consistency
#' 4. Joins with product_property_dictionary for additional product information
#' 5. Filters for US-only orders and required fields
#' 6. Writes the processed data to the destination database
#'
#' @examples
#' \dontrun{
#' # Connect to raw and processed data databases
#' raw_con <- dbConnect(duckdb::duckdb(), dbdir = "raw_data.duckdb")
#' proc_con <- dbConnect(duckdb::duckdb(), dbdir = "processed_data.duckdb")
#'
#' # Process Amazon sales data
#' process_amazon_sales(raw_con, proc_con)
#' }
#'
#' @export
process_amazon_sales <- function(raw_data, Data, verbose = TRUE) {
  # Load required packages
  library(dplyr)
  library(dbplyr)
  library(stringr)
  
  # Input validation
  if (!inherits(raw_data, "DBIConnection")) {
    stop("raw_data must be a DBI database connection")
  }
  
  if (!inherits(Data, "DBIConnection")) {
    stop("Data must be a DBI database connection")
  }
  
  # Check if required tables exist
  if (!DBI::dbExistsTable(raw_data, "df_amazon_sales")) {
    stop("Table 'df_amazon_sales' does not exist in the raw_data database")
  }
  
  if (!DBI::dbExistsTable(raw_data, "product_property_dictionary")) {
    stop("Table 'product_property_dictionary' does not exist in the raw_data database")
  }
  
  if (verbose) message("Processing Amazon sales data...")
  
  # Reference tables
  amazon_sales_dta <- tbl(raw_data, "df_amazon_sales")
  product_property_dictionary <- tbl(raw_data, "product_property_dictionary")
  
  # Process data
  tryCatch({
    result <- amazon_sales_dta %>% 
      filter(str_detect(buyer_email, "@")) %>%
      mutate(
        customer_id = sql("LOWER(SUBSTR(buyer_email, 1, POSITION('@' IN buyer_email) - 1))"),
        time = purchase_date
      ) %>%
      rename(
        lineproduct_price = product_price,
        zip_code = shipping_postal_code
      ) %>%
      left_join(product_property_dictionary, by = join_by(sku)) %>%
      filter(
        !is.na(customer_id) & !is.na(time) & 
          !is.na(sku) & !is.na(asin) & !is.na(product_line_id)
      ) %>% 
      filter(shipping_country_code == "US") %>% 
      select(customer_id, time, sku, lineproduct_price, everything()) %>% 
      collect()
    
    # Write to destination database
    DBI::dbWriteTable(Data, "df_amazon_sales", result, overwrite = TRUE, temporary = FALSE)
    
    if (verbose) {
      message("Amazon sales data processing complete:")
      message("  - Processed rows: ", nrow(result))
      message("  - Data written to 'df_amazon_sales' table in destination database")
    }
    
  }, error = function(e) {
    stop("Error processing Amazon sales data: ", e$message)
  })
  
  # Return connection for chaining
  invisible(Data)
}