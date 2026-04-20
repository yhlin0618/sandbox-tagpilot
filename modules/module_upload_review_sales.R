################################################################################
# 2⃣  module_upload.R ----------------------------------------------------------
################################################################################

# 載入必要的資料存取工具
source("utils/data_access.R")  # 包含 tbl2 函數
source("scripts/global_scripts/04_utils/string/fn_remove_illegal_utf8.R")  # UTF-8 清理函數

# NULL 合併運算子
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

#' Upload & Preview Module – UI
uploadModuleUI <- function(id, module_config = NULL, lang_texts = NULL) {
  ns <- NS(id)

  # 載入模組配置
  if (is.null(module_config)) {
    if (exists("app_config") && !is.null(app_config$module_configs$upload)) {
      module_config <- app_config$module_configs$upload
    } else {
      # 使用預設值
      module_config <- list(
        data_limits = list(
          reviews = list(max_brands = 10, max_records_per_brand = 500),
          sales = list(max_brands = 10, max_records_per_brand = 2000)
        )
      )
    }
  }

  # 從配置取得參數
  review_limits <- module_config$data_limits$reviews
  sales_limits <- module_config$data_limits$sales

  # 從 lang_texts 獲取當前語言（處理 reactive 情況）
  current_language <- if (!is.null(lang_texts)) {
    if (is.function(lang_texts)) {
      # lang_texts 是 reactive
      texts <- try(lang_texts(), silent = TRUE)
      if (!inherits(texts, "try-error") && !is.null(texts) && !is.null(texts$language)) {
        texts$language
      } else {
        "zh_TW"
      }
    } else if (!is.null(lang_texts$language)) {
      # lang_texts 是普通物件
      lang_texts$language
    } else {
      "zh_TW"
    }
  } else {
    "zh_TW"
  }

  # 載入對應語言的 hints - 使用統一載入機制
  hints_df <- if (exists("load_module_hints") && is.function(load_module_hints)) {
    load_module_hints(current_language, "upload")
  } else if (exists("load_hints") && is.function(load_hints)) {
    tryCatch({
      result <- load_hints(language = current_language)
      cat("📝 Upload module UI - 載入hints語言:", current_language, "\n")
      result
    }, error = function(e) {
      cat("❌ Upload module - hints載入失敗:", e$message, "\n")
      NULL
    })
  } else {
    cat("⚠️ Upload module - 無可用的hints載入函數\n")
    NULL
  }

  # 處理 lang_texts（可能是 reactive 或普通物件）
  actual_texts <- if (!is.null(lang_texts)) {
    if (is.function(lang_texts)) {
      # lang_texts 是 reactive
      texts <- try(lang_texts(), silent = TRUE)
      if (!inherits(texts, "try-error")) texts else NULL
    } else {
      # lang_texts 是普通物件
      lang_texts
    }
  } else {
    NULL
  }

  # 使用語言內容（如果有提供）- 加入錯誤報告
  title_text <- if (!is.null(actual_texts) && !is.null(actual_texts$title)) {
    actual_texts$title
  } else {
    warning(paste0(
      "❌ [Upload Module UI] 語言內容缺失: title\n",
      "  - 請求: actual_texts$title\n",
      "  - 狀態: ", if(is.null(actual_texts)) "actual_texts 為 NULL" else "缺少 title 欄位", "\n",
      "  - 使用預設: 步驟 1：上傳資料"
    ))
    "步驟 1：上傳資料"
  }

  upload_button_text <- if (!is.null(actual_texts) && !is.null(actual_texts$buttons) && !is.null(actual_texts$buttons$upload)) {
    actual_texts$buttons$upload
  } else {
    warning(paste0(
      "❌ [Upload Module UI] 語言內容缺失: buttons.upload\n",
      "  - 請求: actual_texts$buttons$upload\n",
      "  - 狀態: ",
      if(is.null(actual_texts)) "actual_texts 為 NULL"
      else if(is.null(actual_texts$buttons)) "缺少 buttons 結構"
      else "缺少 upload 欄位", "\n",
      "  - 使用預設: 上傳並預覽"
    ))
    "上傳並預覽"
  }

  next_button_text <- if (!is.null(actual_texts) && !is.null(actual_texts$buttons) && !is.null(actual_texts$buttons$proceed)) {
    actual_texts$buttons$proceed
  } else {
    warning(paste0(
      "❌ [Upload Module UI] 語言內容缺失: buttons.proceed\n",
      "  - 請求: actual_texts$buttons$proceed\n",
      "  - 狀態: ",
      if(is.null(actual_texts)) "actual_texts 為 NULL"
      else if(is.null(actual_texts$buttons)) "缺少 buttons 結構"
      else "缺少 proceed 欄位", "\n",
      "  - 使用預設: 下一步 ➡️"
    ))
    "下一步 ➡️"
  }

  # 新增：獲取所有需要的語言文字
  review_upload_text <- if (!is.null(actual_texts) && !is.null(actual_texts$sections$review_upload)) {
    actual_texts$sections$review_upload
  } else {
    "1.1 上傳評論資料"
  }

  sales_upload_text <- if (!is.null(actual_texts) && !is.null(actual_texts$sections$sales_upload)) {
    actual_texts$sections$sales_upload
  } else {
    "1.2 上傳銷售資料 (選填)"
  }

  review_file_label <- if (!is.null(actual_texts) && !is.null(actual_texts$sections$review_file_label)) {
    actual_texts$sections$review_file_label
  } else {
    "下圖可上傳多個顧客評論檔案(系統會自動合併)"
  }

  sales_file_label <- if (!is.null(actual_texts) && !is.null(actual_texts$sections$sales_file_label)) {
    actual_texts$sections$sales_file_label
  } else {
    "下圖可上傳多個銷售資料檔案(系統會自動合併)"
  }

  review_tab_text <- if (!is.null(actual_texts) && !is.null(actual_texts$tabs$review_preview)) {
    actual_texts$tabs$review_preview
  } else {
    "評論資料預覽"
  }

  sales_tab_text <- if (!is.null(actual_texts) && !is.null(actual_texts$tabs$sales_preview)) {
    actual_texts$tabs$sales_preview
  } else {
    "銷售資料預覽"
  }

  div(id = ns("step1_box"),
      h4(title_text),

      # 評論資料上傳
      div(
        h5(review_upload_text,
           if (exists("add_info_icon")) add_info_icon("upload_reviews", hints_df)),
        # 使用反應式語言化 Markdown 載入
        uiOutput(ns("notification_markdown")),
        div(
          style = "background: #fff3cd; border: 1px solid #ffc107; padding: 10px; border-radius: 5px; margin-bottom: 10px;",
          icon("info-circle"),
          " ",
          if (!is.null(actual_texts) && !is.null(actual_texts$sections$review_limit)) {
            gsub("\\{max_brands\\}", review_limits$max_brands %||% 10,
                 gsub("\\{max_records\\}", review_limits$max_records_per_brand %||% 500,
                      actual_texts$sections$review_limit))
          } else {
            paste0("限制：最多", review_limits$max_brands %||% 10, "個品牌，",
                   "每個品牌最多", review_limits$max_records_per_brand %||% 500, "筆評論")
          }
        ),
        if (exists("add_hint")) {
          add_hint(
            fileInput(ns("excel_multiple"), review_file_label,
                     multiple = TRUE, accept = c(".xlsx", ".xls", ".csv")),
            "upload_reviews", hints_df
          )
        } else {
          fileInput(ns("excel_multiple"), review_file_label,
                   multiple = TRUE, accept = c(".xlsx", ".xls", ".csv"))
        },
        br()
      ),

      # Sales 資料上傳
      div(
        h5(sales_upload_text,
           if (exists("add_info_icon")) add_info_icon("upload_sales", hints_df)),
        div(
          style = "background: #fff3cd; border: 1px solid #ffc107; padding: 10px; border-radius: 5px; margin-bottom: 10px;",
          icon("info-circle"),
          " ",
          if (!is.null(actual_texts) && !is.null(actual_texts$sections$sales_limit)) {
            gsub("\\{max_brands\\}", sales_limits$max_brands %||% 10,
                 gsub("\\{max_records\\}", sales_limits$max_records_per_brand %||% 2000,
                      actual_texts$sections$sales_limit))
          } else {
            paste0("限制：最多", sales_limits$max_brands %||% 10, "個品牌，",
                   "每個品牌最多", sales_limits$max_records_per_brand %||% 2000, "筆銷售資料")
          }
        ),
        if (exists("add_hint")) {
          add_hint(
            fileInput(ns("sales_multiple"), sales_file_label,
                     multiple = TRUE, accept = c(".xlsx", ".xls", ".csv")),
            "upload_sales", hints_df
          )
        } else {
          fileInput(ns("sales_multiple"), sales_file_label,
                   multiple = TRUE, accept = c(".xlsx", ".xls", ".csv"))
        },
        br()
      ),
      
      actionButton(ns("load_btn"), upload_button_text, class = "btn-success"), br(),
      verbatimTextOutput(ns("step1_msg")),
      
      # 分頁顯示兩種資料
      tabsetPanel(
        tabPanel(review_tab_text, DTOutput(ns("preview_tbl"))),
        tabPanel(sales_tab_text, DTOutput(ns("sales_preview_tbl")))
      ),
      br(),
      actionButton(ns("to_step2"), next_button_text, class = "btn-info")
  )
}

#' Upload & Preview Module – Server
#'
#' @param user_info reactive – passed from login module
#' @return reactive containing the uploaded (raw) data.frame or NULL
uploadModuleServer <- function(id, con, user_info, module_config = NULL, lang_texts = NULL) {
  moduleServer(id, function(input, output, session) {

    # ============================================
    # 🌍 語言管理狀態
    # ============================================
    # 追蹤當前語言和內容 - 優先使用傳入的 lang_texts 參數
    current_language <- reactive({
      # 優先從 lang_texts 獲取（檢查是否為 reactive）
      if (!is.null(lang_texts)) {
        if (is.function(lang_texts)) {
          # lang_texts 是 reactive 表達式
          texts <- try(lang_texts(), silent = TRUE)
          if (!inherits(texts, "try-error") && !is.null(texts) && !is.null(texts$language)) {
            return(texts$language)
          }
        } else if (!is.null(lang_texts$language)) {
          # lang_texts 是普通物件
          return(lang_texts$language)
        }
      }
      # 預設值
      return("zh_TW")
    })
    current_hints <- reactiveVal(NULL)
    current_prompts <- reactiveVal(NULL)

    # ============================================
    # 🔄 語言變更觀察器 - 使用傳入的 lang_texts 參數
    # ============================================
    # 追蹤上一次的語言以偵測變更
    last_language <- reactiveVal("zh_TW")

    observe({
      # 從 current_language() 獲取當前語言（派生自 lang_texts）
      new_language <- current_language()
      old_language <- isolate(last_language())

      if (!is.null(new_language) && new_language != old_language) {
        cat("\n🔄 [Upload Module Observer] 語言變更偵測\n")
        cat("  🔄 舊語言:", old_language, "\n")
        cat("  ✅ 新語言:", new_language, "\n")
        last_language(new_language)

        # 1. 重新載入 hints
        cat("\n  📝 步驟 1: 重新載入 Hints\n")
        new_hints <- tryCatch({
          if (exists("load_hints") && is.function(load_hints)) {
            result <- load_hints(language = new_language)
            cat("    ✅ [ upload ] 成功載入hints語言:", new_language, " - 提示數量:", if(!is.null(result)) nrow(result) else 0, "\n")
            result
          } else {
            cat("    ⚠️ load_hints 函數不存在\n")
            NULL
          }
        }, error = function(e) {
          cat("    ❌ Hints 載入失敗:", e$message, "\n")
          NULL
        })
        current_hints(new_hints)

        # 2. 重新載入 prompts
        cat("\n  📦 步驟 2: 重新載入 Prompts\n")
        new_prompts <- tryCatch({
          if (exists("load_prompts") && is.function(load_prompts)) {
            result <- load_prompts(language = new_language)
            cat("    ✅ Prompts 載入成功\n")
            result
          } else {
            cat("    ⚠️ load_prompts 函數不存在\n")
            NULL
          }
        }, error = function(e) {
          cat("    ❌ Prompts 載入失敗:", e$message, "\n")
          NULL
        })
        current_prompts(new_prompts)

        cat("\n✅ [Upload Module] 語言切換完成:", new_language, "\n")
      }
    })

    # ── 輔助函數：安全獲取語言值 ────────────────────────────────────
    get_lang_value <- function(path, default = NULL) {
      # 建立錯誤報告
      error_details <- list()
      error_details$requested_path <- path
      error_details$attempted_steps <- character()

      # 處理 reactive 和 non-reactive 兩種情況
      texts <- if (is.function(lang_texts)) {
        tryCatch({
          result <- lang_texts()
          result
        }, error = function(e) {
          error_msg <- paste0(
            "❌ [Upload Module] Reactive 語言內容錯誤:\n",
            "  - 錯誤類型: reactive 調用失敗\n",
            "  - 請求路徑: ", path, "\n",
            "  - 錯誤訊息: ", e$message, "\n",
            "  - 使用預設: ", default
          )
          cat(error_msg, "\n")
          warning(error_msg)
          NULL
        })
      } else {
        lang_texts
      }

      if (is.null(texts)) {
        error_msg <- paste0(
          "❌ [Upload Module] 語言內容錯誤:\n",
          "  - 錯誤類型: lang_texts 為 NULL\n",
          "  - 請求路徑: ", path, "\n",
          "  - 使用預設: ", default
        )
        cat(error_msg, "\n")
        warning(error_msg)
        return(default)
      }

      # 解析路徑 - 支援 $ 和 . 分隔符
      keys <- if (grepl("[$]", path)) {
        unlist(strsplit(path, "[$]", perl = TRUE))
      } else {
        unlist(strsplit(path, "[.]", perl = TRUE))
      }

      result <- texts
      error_details$path_components <- keys

      for (i in seq_along(keys)) {
        key <- keys[i]
        error_details$attempted_steps <- c(error_details$attempted_steps, key)

        if (!is.null(result) && is.list(result) && key %in% names(result)) {
          result <- result[[key]]
        } else {
          available_keys <- if (!is.null(result) && is.list(result)) names(result) else NULL

          error_msg <- paste0(
            "❌ [Upload Module] 路徑解析錯誤:\n",
            "  - 錯誤類型: 找不到路徑元素\n",
            "  - 完整路徑: ", path, "\n",
            "  - 失敗步驟: ", i, " (", key, ")\n",
            "  - 已嘗試: ", paste(error_details$attempted_steps, collapse = " -> "), "\n",
            "  - 可用鍵值: ",
            if (!is.null(available_keys)) paste(available_keys, collapse = ", ") else "無(非列表或NULL)", "\n",
            "  - 使用預設: ", default
          )
          cat(error_msg, "\n")
          warning(error_msg)
          return(default)
        }
      }

      if (is.null(result)) {
        error_msg <- paste0(
          "❌ [Upload Module] 值為 NULL 錯誤:\n",
          "  - 錯誤類型: 路徑存在但值為 NULL\n",
          "  - 完整路徑: ", path, "\n",
          "  - 使用預設: ", default
        )
        cat(error_msg, "\n")
        warning(error_msg)
        return(default)
      }

      return(result)
    }
    # 渲染語言化 Markdown - 響應語言變更
    output$notification_markdown <- renderUI({
      # 監聽語言更新觸發器
      if (exists("module_ui_update_trigger") && is.function(module_ui_update_trigger)) {
        module_ui_update_trigger()  # 監聽語言更新事件
      }

      # 使用當前語言狀態（從 lang_texts 參數派生）
      lang <- current_language()
      cat("\n📝 [Upload Module Markdown] 渲染 Markdown\n")
      cat("  🌐 當前語言:", lang, "\n")

      # 根據語言代碼決定語言資料夾
      language_folder <- if (exists("get_language_dir_from_config")) {
        get_language_dir_from_config(lang)
      } else {
        # 備用映射
        switch(lang,
          "en_US" = "english",
          "ja_JP" = "japanese",
          "zh_TW" = "chinese",
          "chinese" = "chinese",
          "english" = "english",
          "japanese" = "japanese",
          "chinese"  # 預設
        )
      }

      language_md_path <- file.path("database", "content", language_folder, "markdown", "notification.md")
      cat("📝 Markdown 路徑:", language_md_path, "\n")

      if (file.exists(language_md_path)) {
        includeMarkdown(language_md_path)
      } else if (file.exists("md/notification.md")) {
        cat("📝 語言化 markdown 不存在，使用預設路徑\n")
        includeMarkdown("md/notification.md")
      } else {
        div(
          class = "alert alert-info",
          style = "margin: 10px 0;",
          h5("📁 檔案上傳說明"),
          p("請選擇包含評論資料的 Excel 檔案進行上傳分析。")
        )
      }
    })
    # 載入模組配置
    if (is.null(module_config)) {
      if (exists("app_config") && !is.null(app_config$module_configs$upload)) {
        module_config <- app_config$module_configs$upload
      } else {
        # 使用預設值
        module_config <- list(
          data_limits = list(
            reviews = list(max_brands = 10, max_records_per_brand = 500),
            sales = list(max_brands = 10, max_records_per_brand = 2000)
          )
        )
      }
    }

    # 從配置取得限制參數
    review_limits <- module_config$data_limits$reviews
    sales_limits <- module_config$data_limits$sales
    # 分別存儲兩種資料
    working_data <- reactiveVal(NULL)
    sales_data <- reactiveVal(NULL)
    
    # ---- 上傳 & 儲存 --------------------------------------------------------
    observeEvent(input$load_btn, {
      req(user_info())
      
      # 處理評論資料上傳
      if (!is.null(input$excel_multiple) && nrow(input$excel_multiple) > 0) {
        files <- input$excel_multiple
        all_data <- list()
        for (i in seq_len(nrow(files))) {
          ext <- tolower(tools::file_ext(files$name[i]))
          if (!ext %in% c("xlsx", "xls", "csv")) {
            error_msg_template <- get_lang_value("messages$file_format_error",
                                                "⚠️ 檔案 '{filename}' 格式不支援，只能上傳 .xlsx / .xls/.csv")
            error_msg <- gsub("\\{filename\\}", files$name[i], error_msg_template)
            output$step1_msg <- renderText(error_msg)
            return()
          }
          dat <- if (ext == "csv") {
            # 嘗試用 UTF-8 讀取，如果失敗則用 Latin-1 (Windows-1252 相容)
            dat_raw <- tryCatch({
              readr::read_csv(files$datapath[i], locale = readr::locale(encoding = "UTF-8"), show_col_types = FALSE)
            }, error = function(e) {
              readr::read_csv(files$datapath[i], locale = readr::locale(encoding = "Latin1"), show_col_types = FALSE)
            })
            # 清理非 UTF-8 字元 - 使用標準函數
            remove_illegal_utf8(dat_raw)
          } else {
            readxl::read_excel(files$datapath[i])
          }

          # 標準化欄位名稱（大小寫不敏感）
          names(dat) <- sapply(names(dat), function(col) {
            col_lower <- tolower(col)
            if (col_lower == "variation") return("Variation")
            if (col_lower == "asin") return("ASIN")
            if (col_lower == "title") return("Title")
            if (col_lower == "body") return("Body")
            return(col)
          })

          # 如果沒有 Variation 但有 ASIN，用 ASIN 取代 Variation
          if (!"Variation" %in% names(dat) && "ASIN" %in% names(dat)) {
            dat$Variation <- dat$ASIN
            cat("📝 使用 ASIN 作為 Variation\n")
          }

          must <- c("Variation", "Title", "Body")
          if (!all(must %in% names(dat))) {
            error_msg_template <- get_lang_value("messages$missing_columns",
                                                "⚠️ 檔案 '{filename}' 缺少 Variation (或 ASIN) / Title / Body 欄位")
            error_msg <- gsub("\\{filename\\}", files$name[i], error_msg_template)
            output$step1_msg <- renderText(error_msg)
            return()
          }
          all_data[[i]] <- dat
        }
        dat_all <- do.call(rbind, all_data)
        
        # 實施樣本數限制：每個品牌最多 N 筆評論（從配置讀取）
        max_records <- review_limits$max_records_per_brand %||% 500
        dat_all <- dat_all %>%
          group_by(Variation) %>%
          slice_head(n = max_records) %>%
          ungroup()

        # 檢查品牌數量限制（從配置讀取）
        max_brands <- review_limits$max_brands %||% 10
        unique_brands <- unique(dat_all$Variation)
        if (length(unique_brands) > max_brands) {
          dat_all <- dat_all %>%
            filter(Variation %in% unique_brands[1:max_brands])
          # 使用語言內容或預設文字
          warning_msg_template <- get_lang_value("messages$brand_limit",
                                                "⚠️ 已限制為前{max_brands}個品牌，每品牌最多{max_records}筆評論")
          warning_msg <- gsub("\\{max_brands\\}", max_brands, warning_msg_template)
          warning_msg <- gsub("\\{max_records\\}", max_records, warning_msg)
          showNotification(warning_msg, type = "warning", duration = 5)
        }

        # 最終清理：確保所有字元欄位都是有效 UTF-8 - 使用標準函數
        dat_all <- remove_illegal_utf8(dat_all)

        # 準備新記錄資料（僅預覽，不寫入資料庫）
        json_str <- jsonlite::toJSON(dat_all, dataframe = "rows", auto_unbox = TRUE)
        json_str <- iconv(as.character(json_str), from = "", to = "UTF-8", sub = "")

        should_write <- isTRUE(module_config$enable_db_write) && !is.null(con)
        if (should_write) {
          new_rawdata <- data.frame(
            user_id   = NA_integer_,
            user_uuid = as.character(user_info()$user_id),
            uploaded_at = as.character(Sys.time()),
            json = json_str,
            stringsAsFactors = FALSE
          )
          tryCatch({
            DBI::dbWriteTable(con, "rawdata", new_rawdata, append = TRUE)
          }, error = function(e) {
            cat("⚠️ [Upload Review] DB write skipped due to error:", e$message, "\n")
          })
        } else {
          cat("ℹ️ [Upload Review] Skipping DB write (preview only)\n")
        }
        working_data(dat_all)
        output$preview_tbl <- renderDT(dat_all, selection = "none")
      }
      
      # 處理銷售資料上傳
      if (!is.null(input$sales_multiple) && nrow(input$sales_multiple) > 0) {
        files <- input$sales_multiple
        all_sales <- list()
        for (i in seq_len(nrow(files))) {
          ext <- tolower(tools::file_ext(files$name[i]))
          if (!ext %in% c("xlsx", "xls", "csv")) {
            error_msg_template <- get_lang_value("messages$file_format_error",
                                                "⚠️ 檔案 '{filename}' 格式不支援，只能上傳 .xlsx / .xls/.csv")
            error_msg <- gsub("\\{filename\\}", files$name[i], error_msg_template)
            output$step1_msg <- renderText(error_msg)
            return()
          }
          dat <- if (ext == "csv") {
            # 嘗試用 UTF-8 讀取，如果失敗則用 Latin-1 (Windows-1252 相容)
            dat_raw <- tryCatch({
              readr::read_csv(files$datapath[i], locale = readr::locale(encoding = "UTF-8"), show_col_types = FALSE)
            }, error = function(e) {
              readr::read_csv(files$datapath[i], locale = readr::locale(encoding = "Latin1"), show_col_types = FALSE)
            })
            # 清理非 UTF-8 字元 - 使用標準函數
            remove_illegal_utf8(dat_raw)
          } else {
            readxl::read_excel(files$datapath[i])
          }

          # 標準化欄位名稱（大小寫不敏感）
          names(dat) <- sapply(names(dat), function(col) {
            col_lower <- tolower(col)
            if (col_lower == "variation") return("Variation")
            if (col_lower == "asin") return("ASIN")
            if (col_lower == "sales") return("Sales")
            return(col)
          })

          # 如果沒有 Variation 但有 ASIN，用 ASIN 取代 Variation
          if (!"Variation" %in% names(dat) && "ASIN" %in% names(dat)) {
            dat$Variation <- dat$ASIN
            cat("📝 [Sales] 使用 ASIN 作為 Variation\n")
          }

          # 如果資料中沒有 Variation 欄位，從檔名提取
          if (!"Variation" %in% names(dat)) {
            filename_base <- sub("\\.[^.]+$", "", files$name[i])  # 移除副檔名

            # 檢查是否為 _sales 格式
            if (grepl("_sales$", filename_base)) {
              variation_id <- sub("^(.*?)_sales$", "\\1", filename_base)
            } else {
              # 如果不是標準格式，使用整個檔名（去除副檔名）
              variation_id <- filename_base
            }

            dat$Variation <- variation_id
            cat("📝 [Sales] 從檔名提取 Variation:", variation_id, "\n")
          }
          dat$檔案來源 <- files$name[i]  # 記錄原始檔名便於追蹤
          
          all_sales[[i]] <- dat
        }
        sales_all <- do.call(rbind, all_sales)

        # 標準化欄位名稱：將常見的銷售欄位名稱統一為 Sales
        if ("price" %in% names(sales_all) && !"Sales" %in% names(sales_all)) {
          sales_all$Sales <- sales_all$price
          cat("📝 [Upload] 已將 'price' 欄位重新命名為 'Sales'\n")
        }
        if ("revenue" %in% names(sales_all) && !"Sales" %in% names(sales_all)) {
          sales_all$Sales <- sales_all$revenue
          cat("📝 [Upload] 已將 'revenue' 欄位重新命名為 'Sales'\n")
        }
        if ("sales" %in% names(sales_all) && !"Sales" %in% names(sales_all)) {
          sales_all$Sales <- sales_all$sales
          cat("📝 [Upload] 已將 'sales' 欄位重新命名為 'Sales'\n")
        }

        # 實施樣本數限制：每個品牌最多 N 筆銷售資料（從配置讀取）
        max_sales_records <- sales_limits$max_records_per_brand %||% 2000
        sales_all <- sales_all %>%
          group_by(Variation) %>%
          slice_head(n = max_sales_records) %>%
          ungroup()

        # 檢查品牌數量限制（從配置讀取）
        max_sales_brands <- sales_limits$max_brands %||% 10
        unique_brands <- unique(sales_all$Variation)
        if (length(unique_brands) > max_sales_brands) {
          sales_all <- sales_all %>%
            filter(Variation %in% unique_brands[1:max_sales_brands])
          # 使用語言內容或預設文字
          warning_msg_template <- get_lang_value("messages$sales_brand_limit",
                                                "⚠️ 已限制為前{max_brands}個品牌，每品牌最多{max_records}筆銷售資料")
          warning_msg <- gsub("\\{max_brands\\}", max_sales_brands, warning_msg_template)
          warning_msg <- gsub("\\{max_records\\}", max_sales_records, warning_msg)
          showNotification(warning_msg, type = "warning", duration = 5)
        }
        
        # 準備銷售資料記錄（僅預覽，不寫 DB，除非開啟 enable_db_write）
        should_write <- isTRUE(module_config$enable_db_write) && !is.null(con)
        if (should_write) {
          new_salesdata <- data.frame(
            user_id = as.character(user_info()$user_id),  # 明確轉為字串避免類型推斷錯誤
            uploaded_at = as.character(Sys.time()),
            json = jsonlite::toJSON(sales_all, dataframe = "rows", auto_unbox = TRUE),
            stringsAsFactors = FALSE
          )
          tryCatch({
            DBI::dbWriteTable(con, "salesdata", new_salesdata, append = TRUE)
          }, error = function(e) {
            cat("⚠️ [Upload Sales] DB write skipped due to error:", e$message, "\n")
          })
        } else {
          cat("ℹ️ [Upload Sales] Skipping DB write (preview only)\n")
        }
        sales_data(sales_all)
        output$sales_preview_tbl <- renderDT(sales_all, selection = "none")
      }
      
      # 更新上傳狀態訊息
      msg <- character(0)
      if (!is.null(working_data())) {
        n_brands <- length(unique(working_data()$Variation))
        msg_template <- get_lang_value("messages$review_uploaded", NULL)
        review_msg <- if (!is.null(msg_template)) {
          msg_template <- gsub("\\{brands\\}", n_brands, msg_template)
          msg_template <- gsub("\\{records\\}", nrow(working_data()), msg_template)
          gsub("\\{limit\\}", review_limits$max_records_per_brand %||% 500, msg_template)
        } else {
          sprintf("✅ 已上傳評論資料：%d 個品牌，共 %d 筆（每品牌上限500筆）",
                  n_brands, nrow(working_data()))
        }
        msg <- c(msg, review_msg)
      }
      if (!is.null(sales_data())) {
        n_brands <- length(unique(sales_data()$Variation))
        msg_template <- get_lang_value("messages$sales_uploaded", NULL)
        sales_msg <- if (!is.null(msg_template)) {
          msg_template <- gsub("\\{brands\\}", n_brands, msg_template)
          msg_template <- gsub("\\{records\\}", nrow(sales_data()), msg_template)
          gsub("\\{limit\\}", sales_limits$max_records_per_brand %||% 2000, msg_template)
        } else {
          sprintf("✅ 已上傳銷售資料：%d 個品牌，共 %d 筆（每品牌上限2000筆）",
                  n_brands, nrow(sales_data()))
        }
        msg <- c(msg, sales_msg)
      }
      if (length(msg) == 0) {
        msg <- get_lang_value("messages$at_least_upload", "⚠️ 請至少上傳評論資料")
      }
      output$step1_msg <- renderText(paste(msg, collapse = "\n"))
    })
    
    # 禁止沒資料時按下一步
    observeEvent(input$to_step2, {
      if (is.null(working_data()) || nrow(working_data()) == 0) {
        # 使用 get_lang_value 輔助函數來安全獲取語言內容
        error_msg <- get_lang_value("messages$no_data", "⚠️ 請先上傳並預覽評論資料")
        showNotification(error_msg, type = "error")
        return()
      }
    })
    
    # ---- export ------------------------------------------------------------
    # 統一導出介面：同時支援 InsightForge (data) 和 BrandEdge (review_data)
    list(
      data         = reactive(working_data()),
      review_data  = reactive(working_data()),
      sales_data   = reactive(sales_data()),
      proceed_step = reactive({ input$to_step2 })   # a trigger to switch step outside
    )
  })
}
