dbCopyorReadTemp <- function(conn) {
  # 從 S4 物件中利用 @ 運算子取得 driver 與 dbdir 資訊
  drv <- tryCatch({
    conn@driver
  }, error = function(e) {
    stop("無法從連線物件中取得 driver: ", e$message)
  })
  
  original_db_path <- tryCatch({
    drv@dbdir
  }, error = function(e) {
    stop("無法從 driver 中取得 dbdir: ", e$message)
  })
  
  if (is.null(original_db_path) || original_db_path == "") {
    stop("原始資料庫路徑無效。")
  }
  
  # 取得原始資料庫所在的目錄與檔名
  original_dir <- dirname(original_db_path)
  original_filename <- basename(original_db_path)
  
  # 組合備份檔名：例如 comment_property_rating.duckdb 變成 comment_property_rating_temp.duckdb
  backup_filename <- sub("(\\.duckdb)$", "_temp\\1", original_filename)
  backup_db_path <- file.path(original_dir, backup_filename)
  
  # 若備份檔案已存在，直接連線該備份資料庫
  if (file.exists(backup_db_path)) {
    message("備份資料庫檔案已存在，直接連線到：", backup_db_path)
    backup_conn <- dbConnect(duckdb::duckdb(), backup_db_path)
  } else {
    # 複製原始資料庫檔案到備份檔案（若已存在則覆蓋）
    if (!file.copy(from = original_db_path, to = backup_db_path, overwrite = TRUE)) {
      stop("備份資料庫失敗，請檢查檔案權限或路徑。")
    }
    message("已備份資料庫到：", backup_db_path)
    backup_conn <- dbConnect(duckdb::duckdb(), backup_db_path)
    message("已建立備份資料庫連線：", backup_db_path)
  }
  
  # 將備份連線設定為全域變數 comment_property_rating_temp
  assign("comment_property_rating_temp", backup_conn, envir = .GlobalEnv)
  
  return(backup_conn)
}