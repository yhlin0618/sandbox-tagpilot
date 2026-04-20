#' Import Amazon Competitor Sales Data
#'
#' Imports Amazon competitor sales data from CSV files in subfolders and writes
#' it to a database table.
#'
#' @param main_folder The main folder containing subfolders with competitor sales data files
#' @param db_connection A database connection object
#' @return None. The function writes data directly to the database.
#' @export
#'
#' @importFrom readr read_csv cols col_character
#' @importFrom dplyr mutate rename select rename_with
#' @importFrom purrr walk
#' @importFrom stringr str_replace_all str_extract str_remove
#' @importFrom DBI dbWriteTable dbGetQuery dbExecute
#'
library(DBI)
library(readr)
library(dplyr)
library(purrr)
library(stringr)

#------------------------------------------------
# 建立資料表（如尚未存在）
#------------------------------------------------
initialize_table_if_absent <- function(db_connection) {
  message("建立並取代資料表 df_amz_competitor_sales ...")

  create_sql <- generate_create_table_query(
    or_replace = TRUE,
    con = db_connection,
    target_table = "df_amz_competitor_sales",
    column_defs = list(
      list(name = "asin",  type = "VARCHAR", not_null = TRUE),
      list(name = "date",  type = "DATE", not_null = TRUE),
      list(name = "product_line_id", type = "VARCHAR", not_null = TRUE),
      list(name = "sales", type = "INTEGER"),
      list(name = "trend_line", type = "NUMERIC"),
      list(name = "seven_day_moving_average", type = "NUMERIC")
    ),
    primary_key = c("asin", "date")
  )

  print_query(create_sql, "建立 df_amz_competitor_sales 的 SQL")
  dbExecute(db_connection, create_sql)
}

#------------------------------------------------
# 安全讀檔（僅支援 CSV）
#------------------------------------------------
safe_read_file <- function(path, skip = 0) {
  ext <- tolower(tools::file_ext(path))
  if (ext != "csv") stop("不支援的副檔名（僅限 CSV）：", ext)
  readr::read_csv(path, col_types = cols(.default = col_character()))
}

#------------------------------------------------
# 補 meta 欄、統一欄名
#------------------------------------------------
add_meta_cols <- function(df, asin, product_line_id) {
  df |>
    rename(
      date                      = Time,
      sales                     = Sales,
      trend_line                = `Trend Line`,
      seven_day_moving_average = `7-Day Moving Average`
    ) |>
    mutate(
      asin            = asin,
      product_line_id = product_line_id,
      date            = as.Date(date, origin = "1899-12-30"),
      sales           = ceiling(as.numeric(sales)),
    ) |>
    select(asin, date, product_line_id, sales,
           trend_line, seven_day_moving_average)
}

#------------------------------------------------
# 主函式（僅處理 CSV 檔案）
#------------------------------------------------
import_df_amz_competitor_sales <- function(main_folder, db_connection) {

  # 初始化表格（如不存在則建立）
  initialize_table_if_absent(db_connection)

  sub_folders <- list.dirs(main_folder, full.names = TRUE, recursive = FALSE)

  for (folder in sub_folders) {
    folder_name <- basename(folder)
    # 原始：取出三碼數字
    index_str <- str_extract(folder_name, "\\d{3}")
    
    # 轉成數值後 +1，取出對應的 product_line_id
    index <- as.integer(index_str) + 1
    product_line_id <- df_product_line[index, "product_line_id", drop = TRUE]
    
    message("\u25b6 \u8655\u7406\u8cc7\u6599\u593e：", folder_name)

    file_names <- list.files(folder, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)

    walk(file_names, function(file_name) {
      if (!file.exists(file_name)) {
        warning("檔案不存在（雲端尚未同步？）：", file_name)
        return(NULL)
      }

      asin <- str_remove(basename(file_name), "\\.csv$")

      peek <- suppressMessages(safe_read_file(file_name, skip = 0))
      if (is.null(peek)) return(NULL)
      first_row_all_na <- all(is.na(peek[1, ]))
      skip_rows <- ifelse(first_row_all_na, 1, 0)

      df <- suppressMessages(safe_read_file(file_name, skip = skip_rows))
      if (is.null(df)) return(NULL)

      expect_cols <- c("Time", "Sales", "Trend Line", "7-Day Moving Average")
      missing_cols <- setdiff(expect_cols, names(df))
      if (length(missing_cols)) {
        warning("缺少欄位：", paste(missing_cols, collapse = ", "),
                " ——跳過 ", basename(file_name))
        return(NULL)
      }

      df_prepared <- add_meta_cols(df, asin, product_line_id)

      dbWriteTable(db_connection, "df_amz_competitor_sales",
                   df_prepared, append = TRUE, row.names = FALSE)
      message("　↳ 已寫入 ", nrow(df_prepared), " 筆：", asin)
    })

    message("✓ ", folder_name, " 處理完畢")
  }

  message("--------- 資料表結構 ---------")
  print(dbGetQuery(db_connection, "PRAGMA table_info('df_amz_competitor_sales')"))
  message("--------- DB 使用量 ---------")
  print(dbGetQuery(db_connection, "PRAGMA database_size;"))
}