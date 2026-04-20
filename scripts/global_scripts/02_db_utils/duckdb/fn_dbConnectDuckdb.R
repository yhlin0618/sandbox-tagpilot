#' dbConnectDuckdb
#'
#' 精簡封裝 `DBI::dbConnect(duckdb::duckdb())`，只處理錯誤訊息與唯讀旗標。
#'
#' @param db_path  Character. DuckDB 檔案路徑；可使用 ":memory:" 建立記憶體資料庫。
#' @param read_only Logical. 是否唯讀開啟，預設 FALSE。
#'
#' @return duckdb_connection 物件。
#'
#' @export
#' @importFrom DBI dbConnect dbIsValid
#' @use_package DBI
#' @use_package duckdb
#' @rdname dbConnectDuckdb

dbConnectDuckdb <- function(db_path = ":memory:", read_only = FALSE) {
  # 檢查 db_path 是否有效
  if (is.null(db_path) || is.na(db_path) || length(db_path) == 0) {
    stop("資料庫路徑無效：db_path 為 NULL、NA 或空值")
  }
  
  # 如果不是記憶體資料庫，檢查並建立目錄
  if (!is.na(db_path) && db_path != ":memory:" && !grepl("^:memory:", db_path)) {
    db_dir <- dirname(db_path)
    if (!dir.exists(db_dir) && db_dir != ".") {
      tryCatch({
        dir.create(db_dir, recursive = TRUE, showWarnings = FALSE)
        message("建立資料庫目錄：", db_dir)
      }, error = function(e) {
        warning("無法建立資料庫目錄 ", db_dir, ": ", e$message)
      })
    }
  }
  
  tryCatch({
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = read_only)
    attr(con, "connection_type") <- ifelse(read_only, "duckdb_readonly", "duckdb_rw")
    attr(con, "connection_time") <- Sys.time()
    con
  }, error = function(e) {
    stop("DuckDB 連線失敗：", e$message, "\n路徑：", db_path,
         "\n模式：", ifelse(read_only, "唯讀", "可寫"))
  })
} 