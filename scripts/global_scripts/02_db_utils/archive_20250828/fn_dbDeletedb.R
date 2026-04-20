dbDeletedb <- function(conn, wait_time = 5) {
  # 從 S4 物件中利用 @ 運算子取得 driver 與 dbdir 資訊
  drv <- tryCatch({
    conn@driver
  }, error = function(e) {
    stop("無法從連線物件中取得 driver: ", e$message)
  })
  
  db_path <- tryCatch({
    drv@dbdir
  }, error = function(e) {
    stop("無法從 driver 中取得 dbdir: ", e$message)
  })
  
  if (!file.exists(db_path)) {
    warning("資料庫檔案不存在：", db_path)
    return(invisible(FALSE))
  }
  
  message("資料庫檔案路徑：", db_path)
  
  # 斷開資料庫連線（若有 shutdown 參數則設定之）
  tryCatch({
    dbDisconnect(conn, shutdown = TRUE)
    message("已斷開資料庫連線。")
  }, error = function(e) {
    message("斷開資料庫連線時發生錯誤：", e$message)
  })
  
  # 等待指定的秒數，確保連線完全釋放
  message("等待 ", wait_time, " 秒以確保連線釋放...")
  Sys.sleep(wait_time)
  
  # 刪除資料庫檔案
  if (file.remove(db_path)) {
    message("已成功刪除資料庫檔案：", db_path)
  } else {
    stop("無法刪除資料庫檔案：", db_path)
  }
  
  return(invisible(TRUE))
}