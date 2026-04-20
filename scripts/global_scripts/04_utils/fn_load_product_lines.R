#' Load Product Line Data into Database and Memory
#'
#' Reads product line data from a CSV file, validates it, and stores it in both
#' the SQLite database and memory (globalenv). This function follows the R119
#' Memory-Resident Parameters Rule.
#'
#' @param conn A DBI connection object to the SQLite database
#' @param csv_path Path to the CSV file containing product line data
#'
#' @return Invisibly returns the product_lines data frame
#'
#' @examples
#' \dontrun{
#' conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' product_lines <- load_product_lines(conn)
#' }
#'
#' @export
load_product_lines <- function(
  conn,
  csv_path = file.path("app_data", "parameters", "scd_type1", "df_product_line.csv")
) {
  # Read product line data from CSV
  message("⇢ Loading product line data from: ", csv_path)
  product_lines <- read_csvxlsx(csv_path)
  
  # Validate required fields
  required_fields <- c("product_line_name_english", "product_line_name_chinese", 
                     "product_line_id", "included")
  
  missing_fields <- setdiff(required_fields, names(product_lines))
  if (length(missing_fields) > 0) {
    stop("CSV file missing required fields: ", paste(missing_fields, collapse = ", "))
  }
  
  # Validate lowercase product_line_id (R118)
  uppercase_ids <- product_lines$product_line_id[grepl("[A-Z]", product_lines$product_line_id)]
  if (length(uppercase_ids) > 0) {
    warning("Converting uppercase product_line_ids to lowercase: ", 
            paste(uppercase_ids, collapse = ", "))
    product_lines$product_line_id <- tolower(product_lines$product_line_id)
  }
  
  # Store in database
  message("⇢ Storing product line data in database")
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS df_product_line_profile (
      product_line_id CHAR(3) PRIMARY KEY,
      product_line_name_english TEXT NOT NULL,
      product_line_name_chinese TEXT,
      included BOOL NOT NULL
    )
  ")
  
  # Write to database (replace existing)
  DBI::dbWriteTable(conn, "df_product_line_profile", product_lines, overwrite = TRUE)
  
  return(product_lines)
}