#' Copy Table Between Database Connections
#'
#' This function copies a table from one database connection to another by reading
#' the data into memory and writing it to the target database. It supports copying
#' with optional renaming and various write options.
#'
#' @param source_con A DBI connection object for the source database
#' @param target_con A DBI connection object for the target database
#' @param source_table Character string. Name of the table to copy from source database
#' @param target_table Character string. Name of the table to create in target database (default: same as source_table)
#' @param overwrite Logical. Whether to overwrite existing table in target database (default: FALSE)
#' @param temporary Logical. Whether to create a temporary table in target database (default: FALSE)
#'
#' @return Invisible NULL. The function creates a table as a side effect.
#'
#' @details
#' This function performs a complete copy of a table by:
#' 1. Reading all data from the source table into memory using dplyr::collect()
#' 2. Writing the data to the target database using DBI::dbWriteTable()
#' 
#' Note: For large tables, this operation may consume significant memory as all
#' data is loaded into R before being written to the target database.
#'
#' @examples
#' \dontrun{
#' # Copy table with same name
#' dbCopyTable(
#'   source_con = raw_data,
#'   target_con = processed_data,
#'   source_table = "df_sales_data"
#' )
#' 
#' # Copy table with different name and overwrite if exists
#' dbCopyTable(
#'   source_con = raw_data,
#'   target_con = comment_property_rating,
#'   source_table = "df_all_comment_property",
#'   target_table = "df_comment_property_backup",
#'   overwrite = TRUE
#' )
#' 
#' # Copy as temporary table
#' dbCopyTable(
#'   source_con = processed_data,
#'   target_con = temp_db,
#'   source_table = "df_eby_review",
#'   temporary = TRUE
#' )
#' }
#'
#' @export
dbCopyTable <- function(source_con, target_con, source_table, target_table = source_table,
                        overwrite = FALSE, temporary = FALSE) {

  # -------------------------------------------------------------------------
  # Fast path: DuckDB -> DuckDB 直接使用 ATTACH + CTAS，避免進入 R 記憶體
  # -------------------------------------------------------------------------
  try_fast <- inherits(source_con, "duckdb_connection") &&
              inherits(target_con, "duckdb_connection") &&
              !temporary

  if (try_fast) {
    src_info   <- DBI::dbGetInfo(source_con)
    tgt_info   <- DBI::dbGetInfo(target_con)

    if (!is.null(src_info$dbdir) && !is.null(tgt_info$dbdir)) {
      src_path <- normalizePath(src_info$dbdir, mustWork = FALSE)
      tgt_path <- normalizePath(tgt_info$dbdir, mustWork = FALSE)

      if (src_path == tgt_path) {
        warning("Source and target database are identical; falling back to in-memory copy")
      } else {
        # ensure source table exists
        if (!source_table %in% DBI::dbListTables(source_con)) {
          stop("Table '", source_table, "' does not exist in source database")
        }

        # ATTACH target inside source connection
        DBI::dbExecute(source_con, sprintf("ATTACH '%s' AS tgt (READ_ONLY FALSE)", tgt_path))

        if (overwrite) {
          DBI::dbExecute(source_con, sprintf("DROP TABLE IF EXISTS tgt.%s", DBI::SQL(target_table)))
        }

        DBI::dbExecute(source_con, sprintf(
          "CREATE TABLE tgt.%s AS SELECT * FROM main.%s",
          DBI::SQL(target_table), DBI::SQL(source_table)))

        DBI::dbExecute(source_con, "DETACH tgt")
        message(sprintf("Successfully copied table %s -> %s via ATTACH", source_table, target_table))
        return(invisible(TRUE))
      }
    }
  }

  # -------------------------------------------------------------------------
  # Fallback: 讀入 R 記憶體再寫入（任何連線類型皆適用）
  # -------------------------------------------------------------------------
  if (!DBI::dbIsValid(source_con) || !DBI::dbIsValid(target_con)) {
    stop("Invalid DBI connection(s)")
  }

  if (!source_table %in% DBI::dbListTables(source_con)) {
    stop("Table '", source_table, "' does not exist in source database")
  }

  message(sprintf("[Fallback] Reading table %s into R...", source_table))
  data <- DBI::dbReadTable(source_con, source_table)

  message(sprintf("Writing to target table %s ...", target_table))
  DBI::dbWriteTable(target_con, target_table, data, overwrite = overwrite, temporary = temporary)
  message("Copy completed via in-memory fallback")

  invisible(TRUE)
}