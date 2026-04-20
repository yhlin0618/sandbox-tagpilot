################################################################################
# BrandEdge Upload Module - Framework Version
################################################################################

# 載入 UTF-8 清理函數
source("scripts/global_scripts/04_utils/string/fn_remove_illegal_utf8.R")

#' Upload & Preview Module – UI
#' @param id module id
#' @param module_config Module configuration (optional)
#' @param lang_texts Static language texts (NOT reactive)
uploadModuleUI <- function(id, module_config = NULL, lang_texts = NULL) {
  ns <- NS(id)

  # Extract texts with fallback
  # DO NOT call reactive() here - this runs during UI construction
  texts <- lang_texts
  ui_texts <- if (!is.null(texts) && !is.null(texts$ui)) texts$ui else list()
  upload_texts <- if (!is.null(ui_texts$upload)) ui_texts$upload else list()
  preview_texts <- if (!is.null(ui_texts$preview)) ui_texts$preview else list()
  buttons_texts <- if (!is.null(ui_texts$buttons)) ui_texts$buttons else list()

  div(id = ns("step1_box"),
      h4(upload_texts$title %||% "步驟 1：上傳資料"),

      # BrandEdge 評論資料上傳
      div(
        h5(upload_texts$subtitle %||% "上傳品牌評論資料"),
        p(upload_texts$instruction %||% "支援格式：",
          tags$strong(upload_texts$supported_formats %||% "Excel 或 CSV 檔案")),
        p(upload_texts$auto_detect %||% "必要欄位：Variation（品牌名稱）、Title（評論標題）、Body（評論內容）"),
        p("選填欄位：Rating（評分）、Date（日期）"),
        fileInput(ns("review_files"),
                 upload_texts$file_input_label %||% "多檔案上傳 (自動合併)",
                 multiple = TRUE,
                 accept = c(".xlsx", ".xls", ".csv")),
        br()
      ),

      actionButton(ns("load_btn"),
                  buttons_texts$upload_preview %||% "上傳並預覽",
                  class = "btn-success"),
      br(),
      verbatimTextOutput(ns("step1_msg")),

      # 資料預覽
      h5(preview_texts$title %||% "資料預覽"),
      DTOutput(ns("review_preview_tbl")),
      br(),
      actionButton(ns("to_step2"),
                  buttons_texts$next_step %||% "下一步 ➡️",
                  class = "btn-info")
  )
}

#' Upload & Preview Module – Server
#'
#' @param con Database connection
#' @param user_info reactive – passed from login module
#' @param lang_texts Reactive language texts from unified_language_manager
#' @param module_config Module configuration from YAML
#' @return reactive containing the uploaded (raw) data.frame or NULL
uploadModuleServer <- function(id, con, user_info, lang_texts = reactive(NULL), module_config = NULL) {
  moduleServer(id, function(input, output, session) {
    # 存儲BrandEdge評論資料
    review_data <- reactiveVal(NULL)

    # Load configuration with fallback defaults
    cfg_max_rows <- if (!is.null(module_config$validation$max_rows)) {
      module_config$validation$max_rows
    } else { 1000 }

    cfg_max_brands <- if (!is.null(module_config$validation$max_brands)) {
      module_config$validation$max_brands
    } else { 20 }

    cfg_preview_rows <- if (!is.null(module_config$preview$max_rows)) {
      module_config$preview$max_rows
    } else { 1000 }

    message("📝 [upload_brandedge] 配置載入: max_rows=", cfg_max_rows,
            ", max_brands=", cfg_max_brands, ", preview_rows=", cfg_preview_rows)

    # Helper function to get message text
    get_msg <- function(path, fallback = "") {
      cat("\n🔍 [Upload Module get_msg] DEBUG START ===\n")
      cat("   Path requested:", path, "\n")
      cat("   Fallback:", fallback, "\n")

      # ⚡ FIXED: Read from global_language_state instead of static lang_texts
      texts <- tryCatch({
        if (exists("global_language_state", envir = .GlobalEnv)) {
          cat("   ✅ global_language_state EXISTS\n")
          lang_state <- get("global_language_state", envir = .GlobalEnv)

          if (!is.null(lang_state$language_content)) {
            current_lang <- lang_state$language_content$language
            cat("   Current language:", current_lang, "\n")

            # ⚡ FIX: Correct path is content$modules$upload_brandedge, not content$upload_brandedge
            if (!is.null(lang_state$language_content$content$modules)) {
              module_content <- lang_state$language_content$content$modules$upload_brandedge
              cat("   Module content retrieved:", !is.null(module_content), "\n")
              if (!is.null(module_content)) {
                cat("   Module content keys:", paste(names(module_content), collapse = ", "), "\n")
              }
              module_content
            } else {
              cat("   ⚠️ content$modules is NULL\n")
              NULL
            }
          } else {
            cat("   ⚠️ lang_state$language_content is NULL\n")
            NULL
          }
        } else {
          cat("   ⚠️ global_language_state DOES NOT EXIST - using fallback\n")
          # Fallback to lang_texts for backward compatibility
          if (!is.null(lang_texts) && is.reactive(lang_texts) && is.function(lang_texts)) {
            lang_texts()
          } else {
            lang_texts
          }
        }
      }, error = function(e) {
        cat("   ❌ ERROR:", e$message, "\n")
        NULL
      })

      if (is.null(texts)) {
        cat("   ❌ texts is NULL - returning fallback:", fallback, "\n")
        cat("=== DEBUG END ===\n\n")
        return(fallback)
      }

      # Navigate path like "messages.upload.success"
      parts <- strsplit(path, "\\.")[[1]]
      cat("   Navigating path parts:", paste(parts, collapse = " -> "), "\n")
      result <- texts
      for (i in seq_along(parts)) {
        part <- parts[i]
        cat("   Step", i, "- Looking for:", part, "\n")
        if (!is.null(result[[part]])) {
          result <- result[[part]]
          cat("      ✅ Found (type:", class(result)[1], ")\n")
        } else {
          cat("      ❌ NOT FOUND - returning fallback\n")
          cat("      Available keys:", paste(names(result), collapse = ", "), "\n")
          cat("=== DEBUG END ===\n\n")
          return(fallback)
        }
      }
      cat("   ✅ FINAL RESULT:", as.character(result), "\n")
      cat("=== DEBUG END ===\n\n")
      return(result)
    }
    
    # ---- BrandEdge 欄位偵測函數 --------------------------------------------------------
    detect_review_fields <- function(df) {
      cols <- tolower(names(df))
      original_names <- names(df)

      # Variation 欄位偵測（品牌/產品名稱）
      variation_field <- NULL
      variation_patterns <- c("variation", "brand", "product", "產品", "品牌")
      for (pattern in variation_patterns) {
        idx <- which(grepl(pattern, cols, fixed = TRUE))
        if (length(idx) > 0) {
          variation_field <- original_names[idx[1]]
          break
        }
      }

      # Title 欄位偵測（評論標題）
      title_field <- NULL
      title_patterns <- c("title", "標題", "subject")
      for (pattern in title_patterns) {
        idx <- which(grepl(pattern, cols, fixed = TRUE))
        if (length(idx) > 0) {
          title_field <- original_names[idx[1]]
          break
        }
      }

      # Body 欄位偵測（評論內容）
      body_field <- NULL
      body_patterns <- c("body", "content", "review", "text", "內容", "評論")
      for (pattern in body_patterns) {
        idx <- which(grepl(pattern, cols, fixed = TRUE))
        if (length(idx) > 0) {
          body_field <- original_names[idx[1]]
          break
        }
      }

      # Rating 欄位偵測（評分，選填）
      rating_field <- NULL
      rating_patterns <- c("rating", "star", "score", "評分", "星級")
      for (pattern in rating_patterns) {
        idx <- which(grepl(pattern, cols, fixed = TRUE))
        if (length(idx) > 0) {
          rating_field <- original_names[idx[1]]
          break
        }
      }

      # Date 欄位偵測（日期，選填）
      date_field <- NULL
      date_patterns <- c("date", "time", "datetime", "日期", "時間")
      for (pattern in date_patterns) {
        idx <- which(grepl(pattern, cols, fixed = TRUE))
        if (length(idx) > 0) {
          date_field <- original_names[idx[1]]
          break
        }
      }

      return(list(
        variation = variation_field,
        title = title_field,
        body = body_field,
        rating = rating_field,
        date = date_field
      ))
    }
    
    # ---- 上傳 & 儲存 --------------------------------------------------------
    observeEvent(input$load_btn, {
      req(user_info())
      
      # 重置之前的資料
      review_data(NULL)
      output$step1_msg <- renderText(
        get_msg("messages.processing.uploading", "處理中...")
      )

      # 處理BrandEdge評論資料上傳
      if (!is.null(input$review_files) && nrow(input$review_files) > 0) {
        tryCatch({
          files <- input$review_files
          all_data <- list()

          for (i in seq_len(nrow(files))) {
            ext <- tolower(tools::file_ext(files$name[i]))
            if (!ext %in% c("xlsx", "xls", "csv")) {
              msg <- get_msg("messages.error.unsupported_format", "⚠️ 檔案 '{filename}' 格式不支援，只能上傳 .xlsx / .xls/.csv")
              msg <- gsub("\\{filename\\}", files$name[i], msg)
              output$step1_msg <- renderText(msg)
              return()
            }

            # 嘗試讀取檔案
            dat <- if (ext == "csv") {
              read.csv(files$datapath[i], stringsAsFactors = FALSE, check.names = FALSE)
            } else {
              as.data.frame(readxl::read_excel(files$datapath[i]))
            }

            # 清理非 UTF-8 字元
            dat <- remove_illegal_utf8(dat)

            # 檢查資料是否有效
            if (nrow(dat) == 0) {
              msg <- get_msg("messages.error.empty_file", "⚠️ 檔案 '{filename}' 是空的")
              msg <- gsub("\\{filename\\}", files$name[i], msg)
              output$step1_msg <- renderText(msg)
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

        # 最終清理：確保所有字元欄位都是有效 UTF-8
        combined_data <- remove_illegal_utf8(combined_data)

        # 偵測BrandEdge評論欄位
        fields <- detect_review_fields(combined_data)

        # 檢查必要欄位
        missing_fields <- c()
        if (is.null(fields$variation)) missing_fields <- c(missing_fields, "Variation")
        if (is.null(fields$title)) missing_fields <- c(missing_fields, "Title")
        if (is.null(fields$body)) missing_fields <- c(missing_fields, "Body")

        if (length(missing_fields) > 0) {
          # 缺少必要欄位
          msg <- get_msg("messages.error.missing_columns", "❌ 缺少必要欄位: {columns}")
          msg <- gsub("\\{columns\\}", paste(missing_fields, collapse = ", "), msg)
          output$step1_msg <- renderText(msg)
          return()
        }

        # 標準化資料格式
        standardized_data <- combined_data
        names(standardized_data)[names(standardized_data) == fields$variation] <- "Variation"
        names(standardized_data)[names(standardized_data) == fields$title] <- "Title"
        names(standardized_data)[names(standardized_data) == fields$body] <- "Body"

        # 標準化選填欄位
        if (!is.null(fields$rating)) {
          names(standardized_data)[names(standardized_data) == fields$rating] <- "Rating"
        }
        if (!is.null(fields$date)) {
          names(standardized_data)[names(standardized_data) == fields$date] <- "Date"
        }

        # 基本資料清理（移除缺少必要欄位的行）
        standardized_data <- standardized_data[
          !is.na(standardized_data$Variation) &
          !is.na(standardized_data$Title) &
          !is.na(standardized_data$Body) &
          nzchar(as.character(standardized_data$Variation)) &
          nzchar(as.character(standardized_data$Title)) &
          nzchar(as.character(standardized_data$Body)),
        ]

        # 驗證資料限制 (使用配置值)
        if (nrow(standardized_data) > cfg_max_rows) {
          msg <- get_msg("messages.error.too_many_rows", "❌ 資料筆數超過限制（最多 {max_rows} 筆）")
          msg <- gsub("\\{max_rows\\}", as.character(cfg_max_rows), msg)
          output$step1_msg <- renderText(msg)
          return()
        }

        # 檢查品牌數量 (使用配置值)
        brand_count <- length(unique(standardized_data$Variation))
        if (brand_count > cfg_max_brands) {
          msg <- get_msg("messages.error.too_many_brands", "❌ 品牌數量超過限制（最多 {max_brands} 個）")
          msg <- gsub("\\{max_brands\\}", as.character(cfg_max_brands), msg)
          output$step1_msg <- renderText(msg)
          return()
        }

        # 儲存評論資料（使用現有的rawdata表）
        # Following InsightForge pattern: use dbWriteTable instead of dbExecute
        tryCatch({
          new_rawdata <- data.frame(
            user_id = as.character(user_info()$user_id),  # 明確轉為字串避免類型推斷錯誤
            uploaded_at = as.character(Sys.time()),
            json = jsonlite::toJSON(standardized_data, dataframe = "rows", auto_unbox = TRUE),
            stringsAsFactors = FALSE
          )

          DBI::dbWriteTable(con, "rawdata", new_rawdata, append = TRUE)
        }, error = function(e) {
          msg <- get_msg("messages.error.db_save_failed", "❌ 資料庫儲存失敗: {error}")
          msg <- gsub("\\{error\\}", e$message, msg)
          output$step1_msg <- renderText(msg)
          return()
        })

        review_data(standardized_data)
        output$review_preview_tbl <- renderDT(head(standardized_data, cfg_preview_rows), options = list(scrollX = TRUE), selection = "none")

        # Success message
        msg <- get_msg("messages.upload_with_detection",
                      "✅ 已上傳評論資料，共 {count} 筆，{brands} 個品牌")
        msg <- gsub("\\{count\\}", nrow(standardized_data), msg)
        msg <- gsub("\\{brands\\}", brand_count, msg)
        output$step1_msg <- renderText(msg)
        }, error = function(e) {
          msg <- get_msg("messages.error.processing_error", "❌ 檔案處理錯誤: {error}")
          msg <- gsub("\\{error\\}", e$message, msg)
          output$step1_msg <- renderText(msg)
          return()
        })
      } else {
        output$step1_msg <- renderText(
          get_msg("messages.error.no_file_selected", "⚠️ 請選擇要上傳的檔案")
        )
      }
    })
    
    # 「下一步」按鈕處理 - 引導用戶使用側邊欄導航
    observeEvent(input$to_step2, {
      if (is.null(review_data()) || nrow(review_data()) == 0) {
        msg <- get_msg("messages.error.no_data_uploaded", "⚠️ 請先上傳並預覽評論資料")
        showNotification(msg, type = "error")
        return()
      }

      # 資料已上傳，顯示成功提示並引導用戶使用側邊欄
      # ⚡ FIXED: Path should be "messages.ready_for_analysis" per YAML structure
      msg <- get_msg("messages.ready_for_analysis",
                    "✅ 資料上傳成功！請從左側選單選擇分析模組開始分析")
      showNotification(msg, type = "message", duration = 5)
    })

    # ---- export ------------------------------------------------------------
    list(
      review_data  = reactive(review_data()),      # BrandEdge評論資料
      proceed_step = reactive({ input$to_step2 })   # a trigger to switch step outside
    )
  })
}