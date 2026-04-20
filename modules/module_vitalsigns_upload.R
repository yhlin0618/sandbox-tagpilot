################################################################################
# VitalSigns Upload Module - Multi-File CSV/Excel Upload
################################################################################
# 此模組負責：
# 1. 多檔案上傳（CSV/Excel），支援Amazon格式
# 2. 自動欄位偵測與對應（customer_id, payment_time, lineitem_price）
# 3. 資料驗證與清理
# 4. 資料預覽與統計摘要
# Version: 1.0.0
# Last Updated: 2025-10-11
# Framework: InsightForge v4.0 + VitalSigns Framework
################################################################################

library(shiny)
library(bs4Dash)
library(dplyr)
library(DT)
library(readxl)

# Source utilities
source("utils/config_manager.R", encoding = "UTF-8")
source("utils/language_manager.R", encoding = "UTF-8")
source("utils/hint_system.R", encoding = "UTF-8")
source("scripts/global_scripts/04_utils/string/fn_remove_illegal_utf8.R")  # UTF-8 清理函數

# Note: %||% operator is now defined in language_manager.R

################################################################################
# UI Function
################################################################################

#' VitalSigns Upload Module UI
#' @param id Module namespace ID
#' @param module_config Module configuration from YAML (optional)
#' @param lang_texts Static language texts (NOT reactive)
#' @param enable_hints Enable hint system (default: TRUE)
#' @return tagList of UI elements
uploadVitalsignsModuleUI <- function(id, module_config = NULL, lang_texts = NULL, enable_hints = TRUE) {
  ns <- NS(id)

  # Load hints if enabled
  hints_df <- NULL
  if (enable_hints) {
    # Extract current language from lang_texts parameter (NOT from Sys.getenv!)
    # lang_texts$language is set by get_module_texts() and updates on language switch
    current_lang <- if (!is.null(lang_texts) && !is.null(lang_texts$language)) {
      lang_texts$language
    } else {
      "zh_TW"  # fallback
    }

    cat("🔍 [Language Detection] Input code:", current_lang, "\n")

    tryCatch({
      hints_df <- load_hints(language = current_lang, app_name = "vitalsigns")
      cat("✅ [Upload Module UI] Loaded", nrow(hints_df), "hints for language:", current_lang, "\n")
    }, error = function(e) {
      cat("⚠️ [Upload Module UI] Failed to load hints:", e$message, "\n")
    })
  }

  # Extract texts with fallback - following BrandEdge pattern
  # DO NOT call reactive() here - this runs during UI construction
  texts <- lang_texts
  ui_texts <- if (!is.null(texts) && !is.null(texts$ui)) texts$ui else list()
  buttons_texts <- if (!is.null(ui_texts$buttons)) ui_texts$buttons else list()
  sections_texts <- if (!is.null(ui_texts$sections)) ui_texts$sections else list()
  instructions_texts <- if (!is.null(ui_texts$instructions)) ui_texts$instructions else list()
  warnings_texts <- if (!is.null(ui_texts$warnings)) ui_texts$warnings else list()
  info_texts <- if (!is.null(ui_texts$info)) ui_texts$info else list()
  fields_texts <- if (!is.null(ui_texts$fields)) ui_texts$fields else list()
  inputs_texts <- if (!is.null(ui_texts$inputs)) ui_texts$inputs else list()
  example_texts <- if (!is.null(ui_texts$example)) ui_texts$example else list()

  # Get main texts with fallback
  title <- if (!is.null(texts$title)) texts$title else "步驟 1：資料上傳"
  subtitle <- if (!is.null(texts$subtitle)) texts$subtitle else "上傳銷售資料（支援多檔案CSV/Excel）"

  # Load configuration values with safe nested access
  max_files <- if (!is.null(module_config) &&
                   !is.null(module_config$upload_limits) &&
                   !is.null(module_config$upload_limits$max_files)) {
    module_config$upload_limits$max_files
  } else { 12 }

  max_size_mb <- if (!is.null(module_config) &&
                     !is.null(module_config$upload_limits) &&
                     !is.null(module_config$upload_limits$max_file_size_mb)) {
    module_config$upload_limits$max_file_size_mb
  } else { 200 }

  allowed_ext <- if (!is.null(module_config) &&
                     !is.null(module_config$upload_limits) &&
                     !is.null(module_config$upload_limits$allowed_extensions)) {
    module_config$upload_limits$allowed_extensions
  } else { c("csv", "xlsx", "xls") }

  tagList(
    # Initialize hint system
    if (enable_hints) init_hint_system() else NULL,

    div(
      id = ns("upload_container"),
      class = "vitalsigns-upload-module",

      # Header
      h4(icon("upload"), " ", title),
      p(subtitle, style = "color: #666; margin-bottom: 20px;"),

      # Upload Instructions Section
      bs4Card(
        title = sections_texts$instructions %||% "📋 上傳說明",
        status = "info",
        solidHeader = FALSE,
        width = 12,
        collapsible = TRUE,
        collapsed = FALSE,

        fluidRow(
          # Left column: Format & field info
          column(6,
            div(
              h5(instructions_texts$supported_formats %||% "📊 支援格式"),
              if (enable_hints) add_info_icon("data_format", hints_df, language = current_lang) else NULL
            ),
            tags$ul(
              tags$li(tags$strong(instructions_texts$amazon_report %||% "Amazon 銷售報告"),
                     " - ", instructions_texts$general_text %||% "一般文字記載"),
              tags$li("Excel (.xlsx, .xls)"),
              tags$li("CSV ", instructions_texts$format %||% "格式"),
              tags$li(instructions_texts$multi_file %||% "可以月為單位，同時匯入多筆 CSV 檔案")
            ),

            div(
              class = "alert alert-warning",
              icon("exclamation-triangle"),
              tags$b(" ", warnings_texts$important %||% "重要提醒", "："),
              warnings_texts$data_months %||% "請上傳至少 3-12 個月以上的數據，以確保分析準確性。"
            ),

            div(
              h5(instructions_texts$auto_detect %||% "📦 自動偵測欄位"),
              if (enable_hints) add_info_icon("field_detection", hints_df, language = current_lang) else NULL
            ),
            p(instructions_texts$auto_detect_desc %||%
              "系統將自動偵測以下欄位，並轉換為標準變數名稱："),
            tags$ul(
              tags$li(
                div(
                  HTML(paste0("<strong style='color: #007bff;'>customer_id</strong> ← ",
                             fields_texts$customer_id %||% "客戶識別")),
                  if (enable_hints) add_info_icon("customer_id_field", hints_df, size = "sm", language = current_lang) else NULL
                ),
                tags$ul(style = "font-size: 0.9em; color: #6c757d;",
                  tags$li(fields_texts$customer_priority %||%
                          "優先: buyer email, buyer_email, email"),
                  tags$li(fields_texts$customer_fallback %||%
                          "次選: customer_id, customer, buyer_id, user_id")
                )
              ),
              tags$li(
                div(
                  HTML(paste0("<strong style='color: #007bff;'>payment_time</strong> ← ",
                             fields_texts$payment_time %||% "交易時間")),
                  if (enable_hints) add_info_icon("payment_time_field", hints_df, size = "sm", language = current_lang) else NULL
                ),
                tags$ul(style = "font-size: 0.9em; color: #6c757d;",
                  tags$li(fields_texts$time_detect %||%
                          "偵測: purchase date, payments date, payment_time"),
                  tags$li(fields_texts$time_other %||%
                          "其他: date, time, datetime")
                )
              ),
              tags$li(
                div(
                  HTML(paste0("<strong style='color: #007bff;'>lineitem_price</strong> ← ",
                             fields_texts$lineitem_price %||% "交易金額")),
                  if (enable_hints) add_info_icon("lineitem_price_field", hints_df, size = "sm", language = current_lang) else NULL
                ),
                tags$ul(style = "font-size: 0.9em; color: #6c757d;",
                  tags$li(fields_texts$price_detect %||%
                          "偵測: item price, lineitem_price, amount"),
                  tags$li(fields_texts$price_other %||%
                          "其他: sales, price, total")
                )
              )
            ),

            div(
              class = "alert alert-info",
              icon("info-circle"),
              HTML(paste0("<b>", info_texts$standardization %||% "💡 標準化說明", ":</b><br>")),
              HTML(info_texts$standardization_desc %||%
                   "無論您的原始欄位名稱為何，系統都會轉換為上述藍色標準變數名稱進行分析。"),
              tags$br(),
              HTML(info_texts$example %||%
                   "例如：'buyer email' → <strong>customer_id</strong>，'purchase date' → <strong>payment_time</strong>")
            )
          ),

          # Right column: Example image
          column(6,
            h5(sections_texts$example %||% "📷 Amazon CSV 範例"),
            div(
              style = "border: 1px solid #dee2e6; padding: 10px; border-radius: 5px; background: #f8f9fa;",
              tags$img(
                src = "database/img/amazon_csv_example.png",
                width = "100%",
                alt = example_texts$alt_text %||% "Amazon CSV 範例",
                style = "max-width: 100%; height: auto; border: 1px solid #dee2e6;"
              ),
              tags$p(
                class = "text-muted text-center mt-2",
                tags$small(example_texts$caption %||%
                           "上圖為 Amazon 銷售報告 CSV 格式範例")
              )
            )
          )
        )
      ),

      # Upload Section
      div(
        class = "section-box",
        style = "background: #f8f9fa; padding: 20px; border-radius: 8px; margin-bottom: 20px;",

        # Section title with hint
        div(
          h5(icon("folder-open"), " ", sections_texts$upload_files %||% "📁 上傳銷售數據"),
          if (enable_hints) add_info_icon("data_format", hints_df, language = current_lang) else NULL
        ),

        # File input with hint wrapper
        add_hint_bs4(
          fileInput(
            ns("dna_files"),
            inputs_texts$file_input %||% paste0("多檔案上傳 (自動合併，最多", max_files, "個檔案)"),
            multiple = TRUE,
            accept = paste0(".", allowed_ext, collapse = ",")
          ),
          var_id = "dna_files",
          hints_df = hints_df,
          enable_hints = enable_hints,
          language = current_lang
        ),

        # Upload button with hint
        add_hint_bs4(
          actionButton(
            ns("load_btn"),
            buttons_texts$upload %||% "上傳並預覽",
            class = "btn-success",
            icon = icon("cloud-upload")
          ),
          var_id = "load_btn",
          hints_df = hints_df,
          enable_hints = enable_hints,
          language = current_lang
        ),

        br(), br(),

        # Status message
        verbatimTextOutput(ns("step1_msg"))
      ),

      # Preview Section
      div(
        id = ns("preview_section"),
        class = "section-box",
        style = "background: #f8f9fa; padding: 20px; border-radius: 8px; margin-bottom: 20px;",

        # Preview title with hint
        div(
          h5(icon("table"), " ", sections_texts$preview %||% "資料預覽"),
          if (enable_hints) add_info_icon("dna_preview_tbl", hints_df, language = current_lang) else NULL
        ),

        DTOutput(ns("dna_preview_tbl")),

        br(),

        # Next step button with hint
        add_hint_bs4(
          actionButton(
            ns("to_step2"),
            buttons_texts[["next"]] %||% "下一步 ➡️",
            class = "btn-info",
            icon = icon("arrow-right")
          ),
          var_id = "to_step2",
          hints_df = hints_df,
          enable_hints = enable_hints,
          language = current_lang
        )
      )
    )
  )
}

################################################################################
# Server Function
################################################################################

#' VitalSigns Upload Module Server
#' @param id Module namespace ID
#' @param con Database connection (optional)
#' @param user_info Reactive user information
#' @param lang_texts Reactive language texts
#' @param module_config Module configuration from YAML
#' @return List of reactive values for downstream modules
uploadVitalsignsModuleServer <- function(id, con = NULL, user_info = reactive(NULL),
                                         lang_texts = reactive(NULL), module_config = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Helper function to get message text - following BrandEdge pattern
    get_msg <- function(path, fallback = "") {
      texts <- tryCatch({
        if (!is.null(lang_texts) && is.reactive(lang_texts) && is.function(lang_texts)) {
          lang_texts()
        } else {
          lang_texts
        }
      }, error = function(e) {
        NULL
      })
      if (is.null(texts)) return(fallback)

      # Navigate path like "messages.upload.success"
      parts <- strsplit(path, "\\.")[[1]]
      result <- texts
      for (part in parts) {
        if (!is.null(result[[part]])) {
          result <- result[[part]]
        } else {
          return(fallback)
        }
      }
      return(result)
    }

    # Reactive values
    dna_data <- reactiveVal(NULL)
    upload_metadata <- reactiveVal(NULL)

    # 資料預覽表：只要 dna_data 有內容就會即時更新
    output$dna_preview_tbl <- renderDT({
      req(dna_data())
      df <- as.data.frame(head(dna_data(), 100))
      cat("🧾 [Upload Preview] Rendering table, rows:", nrow(dna_data()), "display rows:", nrow(df), "\n")
      datatable(
        df,
        options = list(scrollX = TRUE, pageLength = 10),
        selection = "none"
      )
    })
    outputOptions(output, "dna_preview_tbl", suspendWhenHidden = FALSE)

    # Load configuration with safe nested access - following BrandEdge pattern
    cfg_max_files <- if (!is.null(module_config) &&
                         !is.null(module_config$upload_limits) &&
                         !is.null(module_config$upload_limits$max_files)) {
      module_config$upload_limits$max_files
    } else { 12 }

    cfg_allowed_ext <- if (!is.null(module_config) &&
                           !is.null(module_config$upload_limits) &&
                           !is.null(module_config$upload_limits$allowed_extensions)) {
      module_config$upload_limits$allowed_extensions
    } else { c("csv", "xlsx", "xls") }

    # ---- Field Detection Function ----
    detect_fields <- function(df, config) {
      cols <- tolower(names(df))

      # Customer ID detection with safe config access
      customer_field <- NULL
      priority_patterns <- if (!is.null(config) &&
                              !is.null(config$column_mapping) &&
                              !is.null(config$column_mapping$customer_id) &&
                              !is.null(config$column_mapping$customer_id$priority_columns)) {
        config$column_mapping$customer_id$priority_columns
      } else {
        c("buyer email", "buyer_email", "email")
      }

      fallback_patterns <- if (!is.null(config) &&
                              !is.null(config$column_mapping) &&
                              !is.null(config$column_mapping$customer_id) &&
                              !is.null(config$column_mapping$customer_id$fallback_columns)) {
        config$column_mapping$customer_id$fallback_columns
      } else {
        c("customer_id", "customer", "buyer_id", "user_id")
      }

      for (pattern in priority_patterns) {
        if (any(grepl(pattern, cols, fixed = TRUE))) {
          customer_field <- names(df)[grepl(pattern, cols, fixed = TRUE)][1]
          break
        }
      }

      if (is.null(customer_field)) {
        for (pattern in fallback_patterns) {
          if (any(grepl(pattern, cols, fixed = TRUE))) {
            customer_field <- names(df)[grepl(pattern, cols, fixed = TRUE)][1]
            break
          }
        }
      }

      # Time field detection with safe config access
      time_field <- NULL
      time_priority <- if (!is.null(config) &&
                          !is.null(config$column_mapping) &&
                          !is.null(config$column_mapping$payment_time) &&
                          !is.null(config$column_mapping$payment_time$priority_columns)) {
        config$column_mapping$payment_time$priority_columns
      } else {
        c("purchase date", "payments date", "payment_time")
      }

      time_fallback <- if (!is.null(config) &&
                          !is.null(config$column_mapping) &&
                          !is.null(config$column_mapping$payment_time) &&
                          !is.null(config$column_mapping$payment_time$fallback_columns)) {
        config$column_mapping$payment_time$fallback_columns
      } else {
        c("date", "time", "datetime")
      }

      for (pattern in time_priority) {
        if (any(grepl(pattern, cols, fixed = TRUE))) {
          time_field <- names(df)[grepl(pattern, cols, fixed = TRUE)][1]
          break
        }
      }

      if (is.null(time_field)) {
        for (pattern in time_fallback) {
          if (any(grepl(pattern, cols, fixed = TRUE))) {
            time_field <- names(df)[grepl(pattern, cols, fixed = TRUE)][1]
            break
          }
        }
      }

      # Amount field detection with safe config access
      amount_field <- NULL
      amount_priority <- if (!is.null(config) &&
                            !is.null(config$column_mapping) &&
                            !is.null(config$column_mapping$lineitem_price) &&
                            !is.null(config$column_mapping$lineitem_price$priority_columns)) {
        config$column_mapping$lineitem_price$priority_columns
      } else {
        c("item price", "lineitem_price", "amount")
      }

      amount_fallback <- if (!is.null(config) &&
                            !is.null(config$column_mapping) &&
                            !is.null(config$column_mapping$lineitem_price) &&
                            !is.null(config$column_mapping$lineitem_price$fallback_columns)) {
        config$column_mapping$lineitem_price$fallback_columns
      } else {
        c("sales", "price", "total")
      }

      for (pattern in amount_priority) {
        if (any(grepl(pattern, cols, fixed = TRUE))) {
          amount_field <- names(df)[grepl(pattern, cols, fixed = TRUE)][1]
          break
        }
      }

      if (is.null(amount_field)) {
        for (pattern in amount_fallback) {
          if (any(grepl(pattern, cols, fixed = TRUE))) {
            amount_field <- names(df)[grepl(pattern, cols, fixed = TRUE)][1]
            break
          }
        }
      }

      return(list(
        customer_id = customer_field,
        time = time_field,
        amount = amount_field
      ))
    }

    # ---- Upload Handler ----
    observeEvent(input$load_btn, {
      cat("🔄 [Upload Module] Upload button clicked\n")

      # Reset previous data
      dna_data(NULL)
      upload_metadata(NULL)
      output$step1_msg <- renderText(
        get_msg("messages.processing", "處理中...")
      )

      # Validate file input
      if (is.null(input$dna_files) || nrow(input$dna_files) == 0) {
        output$step1_msg <- renderText(
          get_msg("messages.error.no_file", "⚠️ 請選擇要上傳的檔案")
        )
        return()
      }

      tryCatch({
        files <- input$dna_files
        all_data <- list()

        cat("📁 [Upload Module] Processing", nrow(files), "file(s)\n")

        # Read each file
        for (i in seq_len(nrow(files))) {
          ext <- tolower(tools::file_ext(files$name[i]))

          # Validate extension using pre-loaded config
          if (!ext %in% cfg_allowed_ext) {
            msg <- get_msg("messages.error.invalid_format",
                          "⚠️ 檔案 '{filename}' 格式不支援")
            msg <- gsub("\\{filename\\}", files$name[i], msg)
            output$step1_msg <- renderText(msg)
            return()
          }

          # Read file
          dat <- if (ext == "csv") {
            read.csv(files$datapath[i], stringsAsFactors = FALSE, check.names = FALSE)
          } else {
            as.data.frame(readxl::read_excel(files$datapath[i]))
          }

          # 清理非 UTF-8 字元
          dat <- remove_illegal_utf8(dat)

          # Check if empty
          if (nrow(dat) == 0) {
            msg <- get_msg("messages.error.empty_file",
                          "⚠️ 檔案 '{filename}' 是空的")
            msg <- gsub("\\{filename\\}", files$name[i], msg)
            output$step1_msg <- renderText(msg)
            return()
          }

          # Add source file info
          dat$source_file <- files$name[i]
          all_data[[i]] <- dat

          cat("  ✅ [Upload Module] Loaded file", i, ":", files$name[i],
              "with", nrow(dat), "rows\n")
        }

        # Merge all files
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

        cat("📊 [Upload Module] Combined data:", nrow(combined_data), "rows,",
            ncol(combined_data), "columns\n")

        # Detect and standardize fields
        fields <- detect_fields(combined_data, module_config)

        cat("🔍 [Upload Module] Field detection results:\n")
        cat("  - customer_id:", fields$customer_id, "\n")
        cat("  - payment_time:", fields$time, "\n")
        cat("  - lineitem_price:", fields$amount, "\n")

        if (!is.null(fields$customer_id) && !is.null(fields$time) && !is.null(fields$amount)) {
          # Standardize data format
          standardized_data <- combined_data
          names(standardized_data)[names(standardized_data) == fields$customer_id] <- "customer_id"
          names(standardized_data)[names(standardized_data) == fields$time] <- "payment_time"
          names(standardized_data)[names(standardized_data) == fields$amount] <- "lineitem_price"

          # Basic data cleaning
          standardized_data <- standardized_data[
            !is.na(standardized_data$customer_id) &
            !is.na(standardized_data$payment_time) &
            !is.na(standardized_data$lineitem_price),
          ]

          # Convert data types
          standardized_data$lineitem_price <- as.numeric(standardized_data$lineitem_price)
          standardized_data$payment_time <- as.POSIXct(standardized_data$payment_time)

          # Store data
          dna_data(standardized_data)

          # Create metadata
          metadata <- list(
            upload_timestamp = Sys.time(),
            file_names = files$name,
            row_count = nrow(standardized_data),
            customer_count = length(unique(standardized_data$customer_id)),
            date_range = range(standardized_data$payment_time, na.rm = TRUE),
            total_revenue = sum(standardized_data$lineitem_price, na.rm = TRUE)
          )
          upload_metadata(metadata)

          # Success message
          msg <- get_msg("messages.success.upload_complete",
                        "✅ 已上傳資料，共 {row_count} 筆 (✅ 已偵測到完整欄位)")
          msg <- gsub("\\{row_count\\}", nrow(standardized_data), msg)
          output$step1_msg <- renderText(msg)

          cat("✅ [Upload Module] Upload complete:", nrow(standardized_data), "rows,",
              length(unique(standardized_data$customer_id)), "unique customers\n")

          showNotification(
            get_msg("messages.success.notification", "✅ 資料上傳成功！"),
            type = "message",
            duration = 3
          )

        } else {
          # Fields not fully detected
          dna_data(combined_data)

          msg <- get_msg("messages.warning.incomplete_fields",
                        "✅ 已上傳資料，共 {row_count} 筆 (⚠️ 未偵測到完整欄位)")
          msg <- gsub("\\{row_count\\}", nrow(combined_data), msg)
          output$step1_msg <- renderText(msg)

          cat("⚠️ [Upload Module] Incomplete field detection\n")

          showNotification(
            get_msg("messages.warning.notification", "⚠️ 未偵測到完整欄位，請檢查資料格式"),
            type = "warning",
            duration = 5
          )
        }

      }, error = function(e) {
        cat("❌ [Upload Module Error]", e$message, "\n")

        msg <- get_msg("messages.error.upload_failed", "❌ 檔案處理錯誤: {error}")
        msg <- gsub("\\{error\\}", e$message, msg)
        output$step1_msg <- renderText(msg)

        showNotification(
          paste(get_msg("messages.error.notification", "❌ 上傳失敗:"), e$message),
          type = "error",
          duration = 5
        )
      })
    })

    # Prevent next step without data
    observeEvent(input$to_step2, {
      if (is.null(dna_data()) || nrow(dna_data()) == 0) {
        showNotification(
          get_msg("messages.error.no_data_next", "⚠️ 請先上傳並預覽資料"),
          type = "error"
        )
        return()
      }
    })

    # Return reactive values
    return(list(
      data = reactive({ dna_data() }),
      metadata = reactive({ upload_metadata() }),
      is_uploaded = reactive({ !is.null(dna_data()) }),
      proceed_step = reactive({ input$to_step2 })
    ))
  })
}
