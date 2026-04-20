################################################################################
# 2⃣  module_upload.R ----------------------------------------------------------
################################################################################

#' Upload & Preview Module – UI
uploadModuleUI <- function(id) {
  ns <- NS(id)
  div(id = ns("step1_box"),
      h4("步驟 1：上傳資料"),
      
      # DNA 分析資料上傳
      div(
        h5("上傳 DNA 分析資料"),
        div(style = "background-color: #e7f3ff; padding: 15px; border-left: 4px solid #007bff; margin-bottom: 15px;",
          h6(style = "margin-top: 0;", tags$strong("📊 資料需求")),
          tags$ul(style = "margin-bottom: 5px;",
            tags$li("請上傳至少", tags$strong(style = "color: #dc3545;", "1~3 年"), "的顧客交易數據"),
            tags$li("資料檔須包含顧客 ID、購買時間和購買金額"),
            tags$li("可同時上傳多個月份檔案"),
            tags$li("建議", tags$strong(style = "color: #dc3545;", "12-36 個月")),
            tags$li(tags$strong(style = "color: #dc3545;", "檔案數上限：36 個檔案"), "（3 年資料）"),
            tags$li("檔案格式：", tags$strong("CSV"), "（推薦）、XLSX")
          )
        ),
        div(style = "background-color: #f8f9fa; padding: 15px; border-left: 4px solid #28a745; margin-bottom: 15px;",
          h6(style = "margin-top: 0;", tags$strong("📋 必填欄位")),
          tags$ol(style = "margin-bottom: 5px;",
            tags$li(tags$strong("ID"), " 或 ", tags$strong("Email"), " - 識別顧客的代碼",
                    tags$br(), tags$small(style = "color: #6c757d;", "系統可識別：customer_id, email, buyer_email, "),
                    tags$span(style = "color: #007bff; font-weight: bold;", "ship-postal-code")),
            tags$li(tags$strong("purchase_time"), " - 顧客購買產品的時間點",
                    tags$br(), tags$small(style = "color: #6c757d;", "格式須符合 EXCEL 規範，如 YYYY-MM-DD"),
                    tags$br(), tags$small(style = "color: #6c757d;", "系統可識別：payment_time, purchase_time, "),
                    tags$span(style = "color: #007bff; font-weight: bold;", "purchase-date"),
                    tags$small(style = "color: #6c757d;", ", purchase_date, date")),
            tags$li(tags$strong("price"), " - 購買產品所花費的金額（數值型態）",
                    tags$br(), tags$small(style = "color: #6c757d;", "系統可識別：lineitem_price, amount, price, "),
                    tags$span(style = "color: #007bff; font-weight: bold;", "item-price"))
          ),
          p(style = "margin-bottom: 0; color: #6c757d; font-size: 0.9em;",
            "💡 系統會自動偵測並對應以上欄位，支援多種命名格式；當系統無法讀取上述變數時，通常是因為顧客 ID、購買時間和購買金額這三個變數名稱無法被識別。您可以根據這三個變數的英文名稱進行修改，這樣系統就能正確對應和識別。"),
          p(style = "margin-bottom: 0; color: #dc3545; font-size: 0.9em;",
            "💡 提醒：price 係指顧客購買產品所花費的金額，若為跨境電商交易，須注意幣值需換算和統一。")
        ),
        fileInput(ns("dna_files"), "📁 多檔案上傳（自動合併）", multiple = TRUE, accept = c(".xlsx", ".xls", ".csv")),
        br()
      ),
      
      actionButton(ns("load_btn"), "上傳並預覽", class = "btn-success"), br(),
      verbatimTextOutput(ns("step1_msg")),
      
      # 資料預覽
      DTOutput(ns("dna_preview_tbl")),
      br(),
      actionButton(ns("to_step2"), "下一步 ➡️", class = "btn-info")
  )
}

#' Upload & Preview Module – Server
#'
#' @param user_info reactive – passed from login module
#' @return reactive containing the uploaded (raw) data.frame or NULL
uploadModuleServer <- function(id, con, user_info) {
  moduleServer(id, function(input, output, session) {
    # 存儲DNA分析資料
    dna_data <- reactiveVal(NULL)
    
    # ---- 欄位偵測函數 --------------------------------------------------------
    detect_fields <- function(df) {
      cols <- tolower(names(df))
      
      # 客戶ID欄位偵測（優先電子郵件）
      customer_field <- NULL
      email_patterns <- c("buyer email", "buyer_email", "email")
      id_patterns <- c("customer_id", "customer", "buyer_id", "user_id", "ship-postal-code")
      
      for (pattern in email_patterns) {
        if (any(grepl(pattern, cols, fixed = TRUE))) {
          customer_field <- names(df)[grepl(pattern, cols, fixed = TRUE)][1]
          break
        }
      }
      
      if (is.null(customer_field)) {
        for (pattern in id_patterns) {
          if (any(grepl(pattern, cols, fixed = TRUE))) {
            customer_field <- names(df)[grepl(pattern, cols, fixed = TRUE)][1]
            break
          }
        }
      }
      
      # 時間欄位偵測
      time_field <- NULL
      time_patterns <- c("purchase date", "payments date", "payment_time", "purchase-date", "date", "time", "datetime")
      for (pattern in time_patterns) {
        if (any(grepl(pattern, cols, fixed = TRUE))) {
          time_field <- names(df)[grepl(pattern, cols, fixed = TRUE)][1]
          break
        }
      }
      
      # 金額欄位偵測
      amount_field <- NULL
      amount_patterns <- c("item price", "item-price", "lineitem_price", "amount", "sales", "price", "total")
      for (pattern in amount_patterns) {
        if (any(grepl(pattern, cols, fixed = TRUE))) {
          amount_field <- names(df)[grepl(pattern, cols, fixed = TRUE)][1]
          break
        }
      }
      
      return(list(customer_id = customer_field, time = time_field, amount = amount_field))
    }
    
    # ---- 上傳 & 儲存 --------------------------------------------------------
    observeEvent(input$load_btn, {
      req(user_info())
      
      # 重置之前的資料
      dna_data(NULL)
      output$step1_msg <- renderText("處理中...")
      
      # 處理DNA分析資料上傳
      if (!is.null(input$dna_files) && nrow(input$dna_files) > 0) {
        tryCatch({
          files <- input$dna_files

          # ✅ 檔案數量限制檢查（上限36個檔案 = 3年資料）
          MAX_FILES <- 36
          if (nrow(files) > MAX_FILES) {
            output$step1_msg <- renderText(
              sprintf("⚠️ 檔案數量超過上限！\n已上傳 %d 個檔案，但系統最多只能處理 %d 個檔案（3年資料）。\n請減少檔案數量後重新上傳。",
                      nrow(files), MAX_FILES)
            )
            return()
          }

          # 顯示檔案數量資訊
          if (nrow(files) >= 12) {
            output$step1_msg <- renderText(
              sprintf("✅ 已選擇 %d 個檔案（約 %.1f 年資料），符合建議。正在處理中...",
                      nrow(files), nrow(files)/12)
            )
          } else {
            output$step1_msg <- renderText(
              sprintf("⚠️ 已選擇 %d 個檔案（不足1年資料）。建議上傳至少12個月份的資料以獲得更準確的分析結果。\n正在處理中...",
                      nrow(files))
            )
          }

          all_data <- list()

          for (i in seq_len(nrow(files))) {
            ext <- tolower(tools::file_ext(files$name[i]))
            if (!ext %in% c("xlsx", "xls", "csv")) {
              output$step1_msg <- renderText(sprintf("⚠️ 檔案 '%s' 格式不支援，只能上傳 .xlsx / .xls/.csv", files$name[i]))
              return()
            }
            
            # 嘗試讀取檔案
            dat <- if (ext == "csv") {
              read.csv(files$datapath[i], stringsAsFactors = FALSE, check.names = FALSE)
            } else {
              as.data.frame(readxl::read_excel(files$datapath[i]))
            }
            
            # 檢查資料是否有效
            if (nrow(dat) == 0) {
              output$step1_msg <- renderText(sprintf("⚠️ 檔案 '%s' 是空的", files$name[i]))
              return()
            }
            
            # 添加來源檔案資訊
            dat$source_file <- files$name[i]
            all_data[[i]] <- dat
          }
        
        # 合併所有檔案
        all_columns <- unique(unlist(lapply(all_data, names)))
        for (i in seq_along(all_data)) {
          missing_cols <- setdiff(all_columns, names(all_data[[i]]))
          for (col in missing_cols) {
            all_data[[i]][[col]] <- NA
          }
          all_data[[i]] <- all_data[[i]][all_columns]
        }
        
        combined_data <- do.call(rbind, all_data)
        
        # 偵測並標準化欄位
        fields <- detect_fields(combined_data)

        # ✅ 任務 1.2: 增強欄位驗證，提供清楚的錯誤訊息
        missing_fields <- c()
        if (is.null(fields$customer_id)) missing_fields <- c(missing_fields, "客戶ID")
        if (is.null(fields$time)) missing_fields <- c(missing_fields, "交易時間")
        if (is.null(fields$amount)) missing_fields <- c(missing_fields, "交易金額")

        if (length(missing_fields) > 0) {
          error_message <- paste0(
            "❌ 缺少必填欄位：", paste(missing_fields, collapse = "、"), "\n\n",
            "請確保上傳的檔案包含以下欄位：\n",
            "• 客戶ID - customer_id, email, buyer_email, ship-postal-code\n",
            "• 交易時間 - payment_time, purchase-date, purchase_date, date\n",
            "• 交易金額 - lineitem_price, amount, price, item-price\n\n",
            "目前檔案的欄位：", paste(names(combined_data), collapse = ", ")
          )
          output$step1_msg <- renderText(error_message)
          return()
        }

        # 所有必填欄位都存在，繼續標準化
        if (!is.null(fields$customer_id) && !is.null(fields$time) && !is.null(fields$amount)) {
          # 標準化資料格式
          standardized_data <- combined_data
          names(standardized_data)[names(standardized_data) == fields$customer_id] <- "customer_id"
          names(standardized_data)[names(standardized_data) == fields$time] <- "payment_time"
          names(standardized_data)[names(standardized_data) == fields$amount] <- "lineitem_price"
          
          # 基本資料清理
          standardized_data <- standardized_data[!is.na(standardized_data$customer_id) & 
                                                !is.na(standardized_data$payment_time) & 
                                                !is.na(standardized_data$lineitem_price), ]
          
          # 根據配置決定是否上傳到SQL
          if (!exists("SKIP_SQL_UPLOAD") || !SKIP_SQL_UPLOAD) {
            # 儲存DNA資料（使用 db_execute）
            tryCatch({
              uid <- as.character(user_info()$user_id)
              upload_time <- as.character(Sys.time())
              json_data <- as.character(jsonlite::toJSON(standardized_data, dataframe = "rows", auto_unbox = TRUE))

              message(sprintf("📤 [UPLOAD] user_id: %s, 資料筆數: %d", uid, nrow(standardized_data)))

              # 使用 db_execute（自動處理 Pool 和參數轉換）
              result <- db_execute(
                "INSERT INTO public.rawdata (user_id, uploaded_at, json) VALUES (?, ?, ?::jsonb)",
                params = list(uid, upload_time, json_data)
              )

              message(sprintf("✅ [UPLOAD] 資料庫插入成功！影響 %d 列", result))
            }, error = function(e) {
              message(sprintf("❌ [UPLOAD] 資料庫插入失敗: %s", e$message))
              stop(e)
            })

            upload_msg <- "✅ 已上傳DNA分析資料，共 %d 筆 (✅ 已偵測到DNA分析欄位)"
          } else {
            upload_msg <- "✅ 已載入DNA分析資料，共 %d 筆 (✅ 已偵測到DNA分析欄位，已跳過SQL上傳)"
          }
          
          dna_data(standardized_data)
          output$dna_preview_tbl <- renderDT(head(standardized_data, 1000), options = list(scrollX = TRUE), selection = "none")
          output$step1_msg <- renderText(sprintf(upload_msg, nrow(standardized_data)))
        } else {
          # 如果無法偵測DNA欄位，當作一般資料儲存
          dna_data(combined_data)
          output$dna_preview_tbl <- renderDT(head(combined_data, 1000), options = list(scrollX = TRUE), selection = "none")
          output$step1_msg <- renderText(sprintf("✅ 已上傳資料，共 %d 筆 (⚠️ 未偵測到完整DNA分析欄位)", nrow(combined_data)))
        }
        }, error = function(e) {
          output$step1_msg <- renderText(paste("❌ 檔案處理錯誤:", e$message))
          return()
        })
      } else {
        output$step1_msg <- renderText("⚠️ 請選擇要上傳的檔案")
      }
    })
    
    # 禁止沒資料時按下一步
    observeEvent(input$to_step2, {
      if (is.null(dna_data()) || nrow(dna_data()) == 0) {
        showNotification("⚠️ 請先上傳並預覽DNA分析資料", type = "error")
        return()
      }

      # 導向到顧客價值頁面
      updateTabItems(session = session, inputId = "sidebar_menu", selected = "rfm_analysis")
    })
    
    # ---- export ------------------------------------------------------------
    list(
      dna_data     = reactive(dna_data()),         # DNA分析資料
      proceed_step = reactive({ input$to_step2 })   # a trigger to switch step outside
    )
  })
}