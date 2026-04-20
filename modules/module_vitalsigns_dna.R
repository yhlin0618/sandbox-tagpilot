# Multi-File DNA Analysis Module
# Supports Amazon sales data and general transaction files

library(shiny)
library(shinyjs)
library(dplyr)
library(DT)
library(plotly)
library(readxl)
library(later)
library(markdown)

# These old utilities have been removed and replaced with YAML-based language system
# All text is now managed through get_lang_text() function

# Helper functions from microDNADistribution component
`%+%` <- function(x, y) paste0(x, y)
`%||%` <- function(x, y) if (is.null(x)) y else x
nrow2 <- function(x) {
  if (is.null(x)) return(0)
  if (!is.data.frame(x) && !is.matrix(x)) return(0)
  return(nrow(x))
}

# Source DNA analysis function
if (file.exists("scripts/global_scripts/04_utils/fn_analysis_dna.R")) {
  source("scripts/global_scripts/04_utils/fn_left_join_remove_duplicate2.R")
source("scripts/global_scripts/04_utils/fn_fct_na_value_to_level.R")
source("scripts/global_scripts/04_utils/fn_analysis_dna.R")
}

# Source DNA marketing utilities
if (file.exists("utils/dna_marketing_utils.R")) {
  source("utils/dna_marketing_utils.R")
}

# UI Function
dnaVitalsignsModuleUI <- function(id, module_config = NULL, lang_texts = NULL) {
  ns <- NS(id)

  cat("🔍 [DNA Module UI] Called with id:", id, "\n")
  cat("  - module_config is.null:", is.null(module_config), "\n")
  cat("  - lang_texts is.null:", is.null(lang_texts), "\n")
  if (!is.null(lang_texts)) {
    cat("  - lang_texts keys:", paste(names(lang_texts), collapse = ", "), "\n")
  }

  # Helper function to safely get language text - following VitalSigns framework pattern
  get_text <- function(key, default = "") {
    if (is.null(lang_texts)) return(default)

    # Navigate through nested structure
    parts <- strsplit(key, "\\.")[[1]]
    value <- lang_texts

    for (part in parts) {
      if (is.list(value) && part %in% names(value)) {
        value <- value[[part]]
      } else {
        return(default)
      }
    }

    if (is.null(value)) default else as.character(value)
  }

  # Build UI
    ui_result <- div(
    # 初始化 shinyjs
    useShinyjs(),

    h3(paste0("📊 ", get_text("title", "客戶DNA分析"))),

    # 操作說明
    div(
      style = "background: #f8f9fa; padding: 15px; margin: 15px 0; border-radius: 8px; border-left: 4px solid #007bff;",
      h5(paste0("📋 ", get_text("ui.sections.instructions", "操作說明")),
         style = "color: #007bff; margin-bottom: 10px;"),
      tags$ol(
        style = "margin-bottom: 0; padding-left: 20px;",
        tags$li(HTML(paste0("<b>", get_text("help.step1", "步驟一："), "</b>",
                           get_text("help.step1_desc", "點選你要分析的顧客標籤，例如，你可以點選購買金額。")))),
        tags$li(HTML(paste0("<b>", get_text("help.step2", "步驟二："), "</b>",
                           get_text("help.step2_desc", "點選你的分析工具，例如，你可以點選執行客戶分群。")))),
        tags$li(HTML(paste0("<b>", get_text("help.step3", "步驟三："), "</b>",
                           get_text("help.step3_desc", "點選檢視選項，例如，你可以點選統計圖表。"))))
      ),
      p(get_text("help.repeat_steps", "重複上述步驟，假設您想查看顧客最近來店時間，則可重複上述步驟檢視分析結果和AI行銷建議。"),
        style = "margin-top: 10px; margin-bottom: 0; color: #6c757d; font-size: 14px;")
    ),

    # Waiting message (always visible when show_results = false)
    conditionalPanel(
      condition = paste0("output['", ns("show_results"), "'] == false"),
      div(
        id = ns("waiting_message"),
        style = "padding: 40px; background: #f8f9fa; margin: 20px 0; border-radius: 8px; text-align: center; border: 2px dashed #dee2e6;",
        icon("hourglass-half", "fa-3x", style = "color: #6c757d; margin-bottom: 20px;"),
        h4(get_text("messages.info.waiting_data", "⏳ 等待資料上傳與分析..."),
           style = "color: #495057; margin-bottom: 10px;"),
        p(get_text("messages.info.upload_instructions", "請先在「資料上傳」頁面上傳交易資料，系統將自動進行DNA分析。"),
          style = "color: #6c757d; font-size: 16px; margin: 0;")
      )
    ),

    # Results (only visible when show_results = true)
    conditionalPanel(
      condition = paste0("output['", ns("show_results"), "'] == true"),
      
      # ============================================================
      # 2024-12-28 UI 重新設計：Tab 式佈局（仿照營收脈能 AI 洞察區塊）
      # ============================================================

      # Hidden buttons for compatibility with existing server logic
      shinyjs::hidden(
        div(
          actionButton(ns("show_m"), "M"),
          actionButton(ns("show_r"), "R"),
          actionButton(ns("show_f"), "F"),
          actionButton(ns("show_ipt"), "IPT"),
          actionButton(ns("show_cai"), "CAI"),
          actionButton(ns("show_pcv"), "PCV"),
          actionButton(ns("show_cri"), "CRI"),
          actionButton(ns("show_nes"), "NES"),
          radioButtons(ns("view_mode"), NULL,
            choices = c("ai_results" = "ai_results"),
            selected = "ai_results"
          )
        )
      ),

      # 主要區塊：AI 洞察（仿照營收脈能）
      bs4Card(
        title = paste0("🤖 ", get_text("ui.sections.ai_insight_title", "AI 洞察")),
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,

        # 生成 AI 洞察按鈕
        fluidRow(
          column(12,
            actionButton(ns("run_segmentation"),
              paste0("✨ ", get_text("ui.buttons.generate_ai_insight", "生成 AI 洞察")),
              class = "btn-primary",
              style = "margin-bottom: 15px; font-size: 16px; padding: 10px 20px;"),
            hr()
          )
        ),

        # Tab 選項：8 個指標（使用 pills 樣式橫向排列）
        tabsetPanel(
          id = ns("ai_metric_tabs"),
          type = "pills",

          # Tab 1: 最近購買日 (R)
          tabPanel(
            paste0("🕒 ", get_text("dna_metrics.recency.name", "最近購買日")),
            div(style = "padding: 20px;",
              conditionalPanel(
                condition = paste0("output['", ns("has_ai_analysis"), "'] == false"),
                div(style = "text-align: center; padding: 30px; color: #6c757d;",
                  icon("clock", class = "fa-2x"),
                  p(get_text("messages.info.click_generate", "請點擊「生成 AI 洞察」按鈕"), style = "margin-top: 15px;")
                )
              ),
              conditionalPanel(
                condition = paste0("output['", ns("has_ai_analysis"), "'] == true"),
                uiOutput(ns("ai_tab_result_r"))
              )
            ),
            value = "tab_r"
          ),

          # Tab 2: 購買頻率 (F)
          tabPanel(
            paste0("🔁 ", get_text("dna_metrics.frequency.name", "購買頻率")),
            div(style = "padding: 20px;",
              conditionalPanel(
                condition = paste0("output['", ns("has_ai_analysis"), "'] == false"),
                div(style = "text-align: center; padding: 30px; color: #6c757d;",
                  icon("sync", class = "fa-2x"),
                  p(get_text("messages.info.click_generate", "請點擊「生成 AI 洞察」按鈕"), style = "margin-top: 15px;")
                )
              ),
              conditionalPanel(
                condition = paste0("output['", ns("has_ai_analysis"), "'] == true"),
                uiOutput(ns("ai_tab_result_f"))
              )
            ),
            value = "tab_f"
          ),

          # Tab 3: 購買金額 (M)
          tabPanel(
            paste0("💰 ", get_text("dna_metrics.monetary.name", "購買金額")),
            div(style = "padding: 20px;",
              conditionalPanel(
                condition = paste0("output['", ns("has_ai_analysis"), "'] == false"),
                div(style = "text-align: center; padding: 30px; color: #6c757d;",
                  icon("dollar-sign", class = "fa-2x"),
                  p(get_text("messages.info.click_generate", "請點擊「生成 AI 洞察」按鈕"), style = "margin-top: 15px;")
                )
              ),
              conditionalPanel(
                condition = paste0("output['", ns("has_ai_analysis"), "'] == true"),
                uiOutput(ns("ai_tab_result_m"))
              )
            ),
            value = "tab_m"
          ),

          # Tab 4: 購買週期 (IPT)
          tabPanel(
            paste0("⏱️ ", get_text("dna_metrics.ipt.name", "購買週期")),
            div(style = "padding: 20px;",
              conditionalPanel(
                condition = paste0("output['", ns("has_ai_analysis"), "'] == false"),
                div(style = "text-align: center; padding: 30px; color: #6c757d;",
                  icon("hourglass-half", class = "fa-2x"),
                  p(get_text("messages.info.click_generate", "請點擊「生成 AI 洞察」按鈕"), style = "margin-top: 15px;")
                )
              ),
              conditionalPanel(
                condition = paste0("output['", ns("has_ai_analysis"), "'] == true"),
                uiOutput(ns("ai_tab_result_ipt"))
              )
            ),
            value = "tab_ipt"
          ),

          # Tab 5: 顧客活躍度 (CAI)
          tabPanel(
            paste0("📈 ", get_text("dna_metrics.cai.name", "顧客活躍度")),
            div(style = "padding: 20px;",
              conditionalPanel(
                condition = paste0("output['", ns("has_ai_analysis"), "'] == false"),
                div(style = "text-align: center; padding: 30px; color: #6c757d;",
                  icon("chart-line", class = "fa-2x"),
                  p(get_text("messages.info.click_generate", "請點擊「生成 AI 洞察」按鈕"), style = "margin-top: 15px;")
                )
              ),
              conditionalPanel(
                condition = paste0("output['", ns("has_ai_analysis"), "'] == true"),
                uiOutput(ns("ai_tab_result_cai"))
              )
            ),
            value = "tab_cai"
          ),

          # Tab 6: 過去價值 (PCV)
          tabPanel(
            paste0("💎 ", get_text("dna_metrics.pcv.name", "過去價值")),
            div(style = "padding: 20px;",
              conditionalPanel(
                condition = paste0("output['", ns("has_ai_analysis"), "'] == false"),
                div(style = "text-align: center; padding: 30px; color: #6c757d;",
                  icon("gem", class = "fa-2x"),
                  p(get_text("messages.info.click_generate", "請點擊「生成 AI 洞察」按鈕"), style = "margin-top: 15px;")
                )
              ),
              conditionalPanel(
                condition = paste0("output['", ns("has_ai_analysis"), "'] == true"),
                uiOutput(ns("ai_tab_result_pcv"))
              )
            ),
            value = "tab_pcv"
          ),

          # Tab 7: 顧客穩定度 (CRI)
          tabPanel(
            paste0("🎯 ", get_text("dna_metrics.cri.name", "顧客穩定度")),
            div(style = "padding: 20px;",
              conditionalPanel(
                condition = paste0("output['", ns("has_ai_analysis"), "'] == false"),
                div(style = "text-align: center; padding: 30px; color: #6c757d;",
                  icon("bullseye", class = "fa-2x"),
                  p(get_text("messages.info.click_generate", "請點擊「生成 AI 洞察」按鈕"), style = "margin-top: 15px;")
                )
              ),
              conditionalPanel(
                condition = paste0("output['", ns("has_ai_analysis"), "'] == true"),
                uiOutput(ns("ai_tab_result_cri"))
              )
            ),
            value = "tab_cri"
          ),

          # Tab 8: 顧客狀態 (NES)
          tabPanel(
            paste0("👥 ", get_text("customer_lifecycle.title", "顧客狀態")),
            div(style = "padding: 20px;",
              conditionalPanel(
                condition = paste0("output['", ns("has_ai_analysis"), "'] == false"),
                div(style = "text-align: center; padding: 30px; color: #6c757d;",
                  icon("users", class = "fa-2x"),
                  p(get_text("messages.info.click_generate", "請點擊「生成 AI 洞察」按鈕"), style = "margin-top: 15px;")
                )
              ),
              conditionalPanel(
                condition = paste0("output['", ns("has_ai_analysis"), "'] == true"),
                uiOutput(ns("ai_tab_result_nes"))
              )
            ),
            value = "tab_nes"
          )
        ),

        tags$style(HTML(sprintf("
          #%s .nav-pills {
            display: flex;
            flex-wrap: wrap;
            gap: 8px;
          }
          #%s .nav-pills > li {
            flex: 1 1 calc(25%% - 8px);
            margin-bottom: 8px;
          }
        ", ns("ai_metric_tabs"), ns("ai_metric_tabs"))))

        ,
        # Deep AI analysis (summary + marketing recs)
        conditionalPanel(
          condition = paste0("output['", ns("has_ai_analysis"), "'] == true"),
          div(
            style = "margin-top: 15px;",
            uiOutput(ns("ai_analysis_results"))
          )
        ),
        conditionalPanel(
          condition = paste0("output['", ns("has_ai_analysis"), "'] == false"),
          div(
            style = "margin-top: 15px; color: #6c757d;",
            icon("info-circle"),
            span(get_text("messages.info.click_generate", "請點擊「生成 AI 洞察」按鈕"))
          )
        )
      ),

      br(),

      # 資料表獨立成新的區塊
      h4(get_text("ui.sections.customer_info", "顧客資訊")),
    tabsetPanel(
      tabPanel(paste0("📄 ", get_text("ui.tabs.dna_analysis", "顧客標籤分析")),
        fluidRow(
          column(12,
            wellPanel(
              fluidRow(
                column(4, checkboxInput(ns("convert_to_text"),
                  get_text("ui.options.convert_to_text", "將數值轉換為高中低文字"),
                  value = FALSE)),
                column(4,
                  downloadButton(ns("download_analysis_data"),
                    paste0("📥 ", get_text("ui.options.download_data", "下載分析資料")),
                    class = "btn-primary btn-sm",
                    style = "margin-top: 20px;")
                ),
                column(4, div(style = "margin-top: 25px;",
                             textOutput(ns("table_info"), inline = TRUE)))
              )
            )
          )
        ),
        DTOutput(ns("dna_table"))
      ),
      tabPanel(paste0("📖 ", get_text("ui.tabs.explanation", "結果說明")),
        div(style = "padding: 20px;",
          htmlOutput(ns("dna_explanation"))
        )
      )
    )
  )
  )

  # Debug output before returning
  cat("🔍 [DNA Module UI] UI object created, class:", class(ui_result), "\n")
  cat("🔍 [DNA Module UI] UI object is.null:", is.null(ui_result), "\n")
  cat("🔍 [DNA Module UI] UI object length:", length(ui_result), "\n")

  return(ui_result)
}

# Server Function
dnaVitalsignsModuleServer <- function(id, uploaded_data = reactive(NULL),
                                      con = NULL, user_info = reactive(NULL),
                                      module_config = NULL, lang_texts = reactive(NULL),
                                      parent_session = NULL, api_config = NULL) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # 從共用 API 設定讀取模型（fallback 到 gpt-5-nano）
    cfg_ai_model <- if (!is.null(api_config$default_model)) {
      api_config$default_model
    } else {
      "gpt-5-nano"
    }

    # Helper function to safely get language text - reactive version
    get_lang_text <- function(key, default = "") {
      texts <- lang_texts()
      if (is.null(texts)) return(default)

      parts <- strsplit(key, "\\.")[[1]]
      value <- texts

      for (part in parts) {
        if (is.list(value) && part %in% names(value)) {
          value <- value[[part]]
        } else {
          return(default)
        }
      }

      if (is.null(value)) default else as.character(value)
    }

    # Reactive values
    values <- reactiveValues(
      files_info = NULL,
      combined_data = NULL,
      dna_results = NULL,
      current_metric = "M",
      status_text = NULL,  # Will be set based on language
      ai_insights = NULL,
      segmented_data = NULL,
      ai_analysis_done = FALSE,
      ai_recommendations = NULL,
      ai_analysis_summary = NULL,
      ai_summary = NULL,
      temp_chinese_table = NULL,
      clean_dna_data = NULL,
      ai_summary_cache = list(),
      ai_recommendations_cache = list(),
      ai_analysis_done_by_metric = list()
    )

    # Initialize status text with proper language
    observe({
      if (is.null(values$status_text)) {
        values$status_text <- get_lang_text("messages.info.waiting_upload", "等待檔案上傳...")
      }
    }, priority = 1000)

    # ==============================================================
    # GPT 請求節流：避免同一份資料重複呼叫 API
    # ==============================================================
    ai_cache <- reactiveValues(data = list())

    get_ai_cache_key <- function(analysis_type, cache_data, language = "zh_TW") {
      if (is.null(cache_data)) return(NULL)
      key_raw <- list(
        type = analysis_type,
        lang = language,
        data = digest::digest(cache_data, algo = "sha256")
      )
      digest::digest(key_raw, algo = "sha256")
    }

    get_cached_ai <- function(key) {
      if (is.null(key)) return(NULL)
      if (!is.null(ai_cache$data[[key]])) {
        cached <- ai_cache$data[[key]]
        cached$used_at <- Sys.time()
        ai_cache$data[[key]] <- cached
        cat("[DNA Cache] Hit:", substr(key, 1, 8), "\n")
        return(cached$content)
      }
      cat("[DNA Cache] Miss:", substr(key, 1, 8), "\n")
      NULL
    }

    set_cached_ai <- function(key, content) {
      if (is.null(key) || is.null(content)) return()
      ai_cache$data[[key]] <- list(
        content = content,
        cached_at = Sys.time(),
        used_at = Sys.time()
      )
      cat("[DNA Cache] Stored:", substr(key, 1, 8), "\n")
    }

    # 當 DNA 資料變化時清除快取
    observeEvent(values$dna_results, {
      ai_cache$data <- list()
      cat("🧹 [DNA] DNA results changed -> AI cache cleared\n")
    }, ignoreNULL = FALSE)

    # Extract current language from lang_texts reactive (CRITICAL for language switching)
    # This ensures prompts, hints, and all text use the CURRENT language, not default zh_TW
    current_language <- reactive({
      lang <- lang_texts()
      if (!is.null(lang) && !is.null(lang$metadata) && !is.null(lang$metadata$language)) {
        return(lang$metadata$language)
      } else if (!is.null(lang) && !is.null(lang$language)) {
        return(lang$language)
      } else {
        return("zh_TW")  # fallback only
      }
    })

    # Clean DNA result columns for UI/export (focus on Chinese-facing fields)
    clean_dna_data <- function(df) {
      if (is.null(df)) return(NULL)

      data <- as.data.frame(df)

      if (!"pcv" %in% names(data) && "total_spent" %in% names(data)) {
        data$pcv <- data$total_spent
      }
      if (!"ipt_mean" %in% names(data) && "ipt_value" %in% names(data)) {
        data$ipt_mean <- data$ipt_value
      }
      if (!"cri" %in% names(data) && "cri_value" %in% names(data)) {
        data$cri <- data$cri_value
      }
      if (!"times" %in% names(data) && "ni" %in% names(data)) {
        data$times <- data$ni
      }

      # NES backfill：依購買次數補新客；times<=1 一律視為 N，其餘缺值設為 E0
      if (!"nes_status" %in% names(data)) {
        if ("times" %in% names(data)) {
          data$nes_status <- ifelse(data$times <= 1, "N", "E0")
        }
      } else {
        data$nes_status <- as.character(data$nes_status)
        if ("times" %in% names(data)) {
          data$nes_status <- ifelse(data$times <= 1, "N", data$nes_status)
          data$nes_status <- ifelse(is.na(data$nes_status) & data$times > 1, "E0", data$nes_status)
        } else {
          data$nes_status[is.na(data$nes_status)] <- "E0"
        }
      }

      drop_cols <- intersect(
        c(
          "total_spend", "total_sum", "sum_spend_by_date", "sum_spent_by_date",
          "difftime", "nes_ratio", "count_transactions_by_date", "f_ecdf", "r_ecdf",
          "mle", "wmle", "cai_ecdf", "time_first", "nt", "time_first_to_now",
          "e0t", "ge", "ie", "be", "cri_ecdf", "nrec_prob", "nes_value", "segment"
        ),
        names(data)
      )

      if (length(drop_cols) > 0) {
        data <- data[, setdiff(names(data), drop_cols), drop = FALSE]
      }

      preferred_order <- c(
        "customer_id", "r_value", "f_value", "m_value", "times", "ipt_mean",
        "cai_value", "pcv", "cri", "be2", "nes_status", "clv"
      )

      existing <- intersect(preferred_order, names(data))
      if (length(existing) > 0) {
        data <- data[, existing, drop = FALSE]
      }

      return(data)
    }

    # Helper to retrieve the active, cleaned DNA dataset
    get_active_dna_data <- function() {
      if (!is.null(values$clean_dna_data)) {
        return(values$clean_dna_data)
      }
      if (!is.null(values$dna_results) && !is.null(values$dna_results$data_by_customer)) {
        return(clean_dna_data(values$dna_results$data_by_customer))
      }
      return(NULL)
    }

    # 檢查是否有從步驟1傳來的資料
    observe({
      if (!is.null(uploaded_data) && !is.null(uploaded_data())) {
        values$combined_data <- uploaded_data()
        values$status_text <- sprintf(
          get_lang_text("messages.success.data_loaded", "✅ 已從步驟1載入資料，共 %d 筆記錄，%d 位客戶。正在自動進行DNA分析..."),
          nrow(uploaded_data()),
          length(unique(uploaded_data()$customer_id))
        )

        # 設置標記表示資料已準備好自動分析
        values$auto_analyze_ready <- TRUE
      }
    })

    # 控制是否顯示上傳區塊
    output$has_uploaded_data <- reactive({
      !is.null(uploaded_data) && !is.null(uploaded_data()) && nrow(uploaded_data()) > 0
    })
    outputOptions(output, "has_uploaded_data", suspendWhenHidden = FALSE)

    # 控制是否顯示分析結果 - CRITICAL: Initialize to FALSE
    output$show_results <- reactive({
      !is.null(values$dna_results)
    })
    outputOptions(output, "show_results", suspendWhenHidden = FALSE)
    cat("✅ [DNA Module Server] show_results initialized\n")

    # 自動分析觸發器
    observe({
      if (!is.null(values$auto_analyze_ready) && values$auto_analyze_ready &&
          !is.null(values$combined_data) && is.null(values$dna_results)) {
        values$auto_analyze_ready <- FALSE  # 防止重複觸發
        values$status_text <- get_lang_text("messages.processing.auto_start_analysis", "🚀 自動開始DNA分析...")
        analyze_data(values$combined_data, 1, 0.1)  # 固定最少交易次數為1，時間折扣因子為0.1
      }
    })
    
    # File upload handler
    observeEvent(input$multi_files, {
      req(input$multi_files)
      
      values$files_info <- input$multi_files
      file_names <- input$multi_files$name
      
      output$file_info <- renderText({
        paste(
          get_lang_text("messages.file_handling.files_selected", "已選擇"),
          length(file_names),
          get_lang_text("messages.file_handling.files_count", "個檔案"),
          get_lang_text("messages.file_handling.files_list_separator", ":\n"),
          paste(file_names, collapse = "\n")
        )
      })

      values$status_text <- paste(
        get_lang_text("messages.file_handling.files_uploaded", "已上傳"),
        length(file_names),
        get_lang_text("messages.file_handling.files_count", "個檔案"),
        "，",
        get_lang_text("messages.file_handling.preparing_process", "準備處理...")
      )
    })
    
    
    # 通用分析函數
    analyze_data <- function(data, min_transactions, delta_factor) {
      tryCatch({
        values$status_text <- get_lang_text("messages.processing.preparing_data", "📊 準備分析資料...")

        # Filter by minimum transactions
        customer_counts <- data %>%
          group_by(customer_id) %>%
          summarise(n_transactions = n(), .groups = "drop")

        valid_customers <- customer_counts %>%
          filter(n_transactions >= min_transactions) %>%
          pull(customer_id)

        filtered_data <- data %>%
          filter(customer_id %in% valid_customers)

        values$status_text <- sprintf(
          get_lang_text("messages.success.filtered_customers", "✅ 篩選後客戶: %s 筆交易: %s"),
          length(valid_customers),
          nrow(filtered_data)
        )

        if (nrow(filtered_data) == 0) {
          values$status_text <- get_lang_text("messages.error.no_qualified_customers", "❌ 沒有符合最少交易次數的客戶")
          return()
        }
        
        # 確保platform_id欄位存在
        if (!"platform_id" %in% names(filtered_data)) {
          filtered_data$platform_id <- "upload"
        }
        
        # Prepare data for DNA analysis
        sales_by_customer_by_date <- filtered_data %>%
          mutate(
            date = as.Date(payment_time)
            # customer_id 已經是數值型，不需要再次轉換
          ) %>%
          group_by(customer_id, date) %>%
          summarise(
            sum_spent_by_date = sum(lineitem_price),
            count_transactions_by_date = n(),
            payment_time = min(payment_time),  # 添加時間欄位供analysis_dna使用
            platform_id = "upload",  # 固定值，避免first()錯誤
            .groups = "drop"
          )
        
        sales_by_customer <- filtered_data %>%
          # customer_id 已經是數值型，不需要再次轉換
          group_by(customer_id) %>%
          summarise(
            total_spent = sum(lineitem_price),
            times = n(),
            first_purchase = min(payment_time),
            last_purchase = max(payment_time),
            platform_id = "upload",  # 固定值，避免first()錯誤
            .groups = "drop"
          ) %>%
          mutate(
            # customer_id 已經是數值型，不需要再次轉換
            ipt = pmax(as.numeric(difftime(last_purchase, first_purchase, units = "days")), 1),  # 避免0值
            r_value = as.numeric(difftime(Sys.time(), last_purchase, units = "days")),
            f_value = times,
            m_value = total_spent / times,
            ni = times,
            # 計算客戶生命週期
            customer_lifespan = as.numeric(difftime(last_purchase, first_purchase, units = "days"))
          ) %>%
          # 確保所有必要欄位存在且有正確的資料類型
          select(customer_id, total_spent, times, first_purchase, last_purchase, 
                 ipt, r_value, f_value, m_value, ni, platform_id, customer_lifespan)
        
        # Run DNA analysis
        values$status_text <- get_lang_text("messages.processing.executing_dna", "🧬 執行 DNA 分析...")

        if (exists("analysis_dna")) {
          # 設定完整的全域參數
          complete_global_params <- list(
            delta = delta_factor,
            ni_threshold = 4,  # 固定 ni_threshold 為 4（用於 CAI 計算）
            cai_breaks = c(0, 0.1, 0.9, 1),
            text_cai_label = c(
              get_lang_text("metric_analysis.trending_static", "逐漸不活躍"),
              get_lang_text("metric_analysis.stable", "穩定"),
              get_lang_text("metric_analysis.trending_active", "日益活躍")
            ),
            f_breaks = c(-0.0001, 1.1, 2.1, Inf),
            text_f_label = c(
              get_lang_text("metric_analysis.low_frequency", "低頻率"),
              get_lang_text("metric_analysis.mid_frequency", "中頻率"),
              get_lang_text("metric_analysis.high_frequency", "高頻率")
            ),
            r_breaks = c(-0.0001, 0.1, 0.9, 1.0001),
            text_r_label = c(
              get_lang_text("metric_analysis.low_active", "長期不活躍"),
              get_lang_text("metric_analysis.mid_active", "中期不活躍"),
              get_lang_text("metric_analysis.high_active", "近期購買")
            ),
            m_breaks = c(-0.0001, 0.1, 0.9, 1.0001),
            text_m_label = c(
              get_lang_text("metric_analysis.low_value", "低價值"),
              get_lang_text("metric_analysis.mid_value", "中價值"),
              get_lang_text("metric_analysis.high_value", "高價值")
            ),
            nes_breaks = c(0, 1, 2, 2.5, Inf),
            text_nes_label = c("E0", "S1", "S2", "S3")
          )
          
          # 執行 DNA 分析並加入錯誤處理
          dna_results <- tryCatch({
            results <- analysis_dna(
              df_sales_by_customer = as.data.frame(sales_by_customer),
              df_sales_by_customer_by_date = as.data.frame(sales_by_customer_by_date),
              skip_within_subject = FALSE,
              verbose = TRUE,
              global_params = complete_global_params
            )
            
            # 驗證結果結構
            if (is.null(results) || !is.list(results)) {
              stop(get_lang_text("messages.error.dna_result_empty", "DNA分析結果為空或格式不正確"))
            }

            if (is.null(results$data_by_customer) || !is.data.frame(results$data_by_customer)) {
              # 如果data_by_customer不是數據框，嘗試轉換
              if (is.list(results$data_by_customer)) {
                results$data_by_customer <- as.data.frame(results$data_by_customer, stringsAsFactors = FALSE)
              } else {
                stop(get_lang_text("messages.error.invalid_data_structure", "data_by_customer 不是有效的數據結構"))
              }
            }

            # 確保必要欄位存在
            required_cols <- c("customer_id", "r_value", "f_value", "m_value")
            missing_cols <- setdiff(required_cols, names(results$data_by_customer))
            if (length(missing_cols) > 0) {
              # 嘗試從其他來源補充缺失的欄位
              if(!"r_value" %in% names(results$data_by_customer) && "r" %in% names(results$data_by_customer)) {
                results$data_by_customer$r_value <- results$data_by_customer$r
              }
              if(!"f_value" %in% names(results$data_by_customer) && "f" %in% names(results$data_by_customer)) {
                results$data_by_customer$f_value <- results$data_by_customer$f
              }
              if(!"m_value" %in% names(results$data_by_customer) && "m" %in% names(results$data_by_customer)) {
                results$data_by_customer$m_value <- results$data_by_customer$m
              }

              # 再次檢查
              missing_cols <- setdiff(required_cols, names(results$data_by_customer))
              if (length(missing_cols) > 0) {
                stop(sprintf(
                  get_lang_text("messages.error.missing_required_fields", "缺少必要欄位: %s"),
                  paste(missing_cols, collapse = ", ")
                ))
              }
            }
            
            # 補充可能缺失的額外欄位
            if(!"cri" %in% names(results$data_by_customer) && 
               all(c("r_value", "f_value", "m_value") %in% names(results$data_by_customer))) {
              # 計算 CRI (Customer Regularity Index)
              results$data_by_customer$cri <- with(results$data_by_customer,
                0.3 * pmin(pmax(r_value, 0), 1) + 
                0.3 * pmin(pmax(f_value, 0), 1) + 
                0.4 * pmin(pmax(m_value, 0), 1)
              )
            }
            
            if(!"cai_value" %in% names(results$data_by_customer) && "cai" %in% names(results$data_by_customer)) {
              results$data_by_customer$cai_value <- results$data_by_customer$cai
            }
            
            if(!"pcv" %in% names(results$data_by_customer) && "total_spent" %in% names(results$data_by_customer)) {
              results$data_by_customer$pcv <- results$data_by_customer$total_spent
            }
            
            results
            
          }, error = function(e) {
            values$status_text <- sprintf(
              get_lang_text("messages.error.dna_analysis_error", "❌ DNA分析錯誤: %s"),
              e$message
            )
            return(NULL)
          })
          
          if (!is.null(dna_results)) {
            # 補充額外的計算欄位
            if(!"total_spent" %in% names(dna_results$data_by_customer) && 
               "m_value" %in% names(dna_results$data_by_customer) && 
               "f_value" %in% names(dna_results$data_by_customer)) {
              dna_results$data_by_customer$total_spent <- 
                dna_results$data_by_customer$m_value * dna_results$data_by_customer$f_value
            }
            
            # 確保 CRI 存在
            if(!"cri" %in% names(dna_results$data_by_customer)) {
              # R值需要反向（值越小越好）
              r_norm <- 1 - normalize_01(dna_results$data_by_customer$r_value)
              f_norm <- normalize_01(dna_results$data_by_customer$f_value)
              m_norm <- normalize_01(dna_results$data_by_customer$m_value)
              
              dna_results$data_by_customer$cri <- 0.3 * r_norm + 0.3 * f_norm + 0.4 * m_norm
            }
            
            # 確保 CAI 值存在（如果有必要的數據）
            if(!"cai_value" %in% names(dna_results$data_by_customer) && 
               !"cai" %in% names(dna_results$data_by_customer)) {
              # 簡單的活躍度計算：基於最近購買時間的變化
              # 這裡使用簡化的方法：r_value < 30 為活躍(1), 30-90 為穩定(0), >90 為靜止(-1)
              dna_results$data_by_customer$cai_value <- ifelse(
                dna_results$data_by_customer$r_value < 30, 1,
                ifelse(dna_results$data_by_customer$r_value > 90, -1, 0)
              )
            }
            
            # 使用原始結果並僅補足必要欄位（不覆蓋既有 NES 標籤）
            enriched <- dna_results$data_by_customer
            if (!"nes_status" %in% names(enriched)) {
              if ("times" %in% names(enriched)) {
                enriched$nes_status <- ifelse(enriched$times <= 1, "N", "E0")
              }
            } else {
              enriched$nes_status <- as.character(enriched$nes_status)
              if ("times" %in% names(enriched)) {
                enriched$nes_status <- ifelse(enriched$times <= 1, "N", enriched$nes_status)
                enriched$nes_status <- ifelse(is.na(enriched$nes_status) & enriched$times > 1, "E0", enriched$nes_status)
              } else {
                enriched$nes_status[is.na(enriched$nes_status)] <- "E0"
              }
            }
            if (!"total_spent" %in% names(enriched)) {
              if ("m_value" %in% names(enriched) && "f_value" %in% names(enriched)) {
                enriched$total_spent <- enriched$m_value * enriched$f_value
              } else if ("m_value" %in% names(enriched) && "times" %in% names(enriched)) {
                enriched$total_spent <- enriched$m_value * enriched$times
              }
            }
            if (!"purchase_count" %in% names(enriched) && "times" %in% names(enriched)) {
              enriched$purchase_count <- enriched$times
            }
            if (!"avg_order_value" %in% names(enriched) && "m_value" %in% names(enriched)) {
              enriched$avg_order_value <- enriched$m_value
            }
            dna_results$data_by_customer <- enriched
            values$dna_results <- dna_results

            # 供本模組表格使用的精簡資料
            cleaned <- clean_dna_data(enriched)
            values$clean_dna_data <- cleaned
            values$status_text <- get_lang_text("messages.success.dna_analysis_complete", "🎉 DNA 分析完成！")
            
            # 自動執行完整分群（預設使用 M 指標），避免後續再提示「請先執行分群」
            values$current_metric <- "M"
            updateMetricDescription("M")
            performCompleteSegmentation()
            
            # ---- AI 洞察分析（使用 Prompt 系統）--------------------------------
            if(enable_prompts && !is.null(chat_api)) {
              values$status_text <- get_lang_text("messages.processing.generating_ai_insights", "🤖 正在生成 AI 洞察...")
              
              # 準備分析數據摘要
              customer_data <- dna_results$data_by_customer
              summary_data <- list(
                total_customers = nrow(customer_data),
                avg_rfm_r = round(mean(customer_data$r_value, na.rm = TRUE), 2),
                avg_rfm_f = round(mean(customer_data$f_value, na.rm = TRUE), 2), 
                avg_rfm_m = round(mean(customer_data$m_value, na.rm = TRUE), 2),
                avg_clv = round(mean(customer_data$clv, na.rm = TRUE), 2),
                high_value_customers = sum(customer_data$m_value > 0.7, na.rm = TRUE),
                loyal_customers = sum(customer_data$f_value > 0.7, na.rm = TRUE),
                recent_customers = sum(customer_data$r_value > 0.7, na.rm = TRUE)
              )
              
              # 使用集中管理的 prompt 生成洞察（含快取）
              tryCatch({
                # === 快取檢查 ===
                cache_key <- get_ai_cache_key("segmentation_analysis", summary_data, current_language())
                cached_result <- get_cached_ai(cache_key)

                if (!is.null(cached_result)) {
                  cat("[DNA] 使用快取的客戶分群 AI 洞察\n")
                  values$ai_insights <- cached_result
                  values$status_text <- get_lang_text("messages.success.dna_and_ai_complete", "✅ DNA 分析與 AI 洞察完成！")
                } else {
                  ai_result <- execute_gpt_request(
                    var_id = "customer_segmentation_analysis",
                    variables = list(
                      customer_data = jsonlite::toJSON(summary_data, auto_unbox = TRUE),
                      segment_data = jsonlite::toJSON(
                        customer_data %>%
                          select(customer_id, r_value, f_value, m_value, clv, nes_status) %>%
                          head(10),
                        auto_unbox = TRUE
                      )
                    ),
                    chat_api_function = chat_api,
                    model = cfg_ai_model,
                    prompts_df = prompts_df,
                    language = current_language()
                  )

                  set_cached_ai(cache_key, ai_result)
                  values$ai_insights <- ai_result
                  values$status_text <- get_lang_text("messages.success.dna_and_ai_complete", "✅ DNA 分析與 AI 洞察完成！")
                }

              }, error = function(e) {
                values$ai_insights <- get_lang_text("messages.error.ai_analysis_unavailable", "AI 分析暫時無法使用，請稍後再試。")
                cat(get_lang_text("messages.error.ai_analysis_error", "AI 分析錯誤:"), e$message, "\n")
                values$status_text <- get_lang_text("messages.success.dna_complete_ai_unavailable", "✅ DNA 分析完成！（AI 洞察暫時無法使用）")
              })
            }

            # show_results is now reactive based on values$dna_results (initialized in server setup)
            cat("✅ [DNA Module] Analysis complete, show_results will become TRUE\n")
          }
          
        } else {
          values$status_text <- get_lang_text("messages.error.function_not_exists", "❌ analysis_dna 函數不存在，請檢查 global_scripts")
        }

      }, error = function(e) {
        values$status_text <- sprintf(
          get_lang_text("messages.error.analysis_error", "❌ 分析錯誤: %s"),
          e$message
        )
      })
    }
    
    # 分析已上傳的資料 - 已改為自動分析，不需要按鈕
    # observeEvent(input$analyze_uploaded, {
    #   req(values$combined_data)
    #   
    #   # 清除 AI 管理器的所有快取（因為數據已更新）
    #   clear_all_results(ai_manager)
    #   
    #   min_trans <- 1  # 固定最少交易次數為1
    #   delta_val <- 0.1  # 固定時間折扣因子為0.1
    #   
    #   analyze_data(values$combined_data, min_trans, delta_val)
    # })
    
    # Process files
    observeEvent(input$process_files, {
      req(values$files_info)

      values$status_text <- get_lang_text("messages.processing.processing_files", "開始處理檔案...")

      tryCatch({
        all_data <- list()

        # Read all files
        for (i in seq_len(nrow(values$files_info))) {
          file_path <- values$files_info$datapath[i]
          file_name <- values$files_info$name[i]

          values$status_text <- sprintf(
            get_lang_text("messages.processing.reading_file", "讀取檔案: %s"),
            file_name
          )
          
          if (grepl("\\.csv$", file_name, ignore.case = TRUE)) {
            df <- read.csv(file_path, stringsAsFactors = FALSE)
          } else if (grepl("\\.(xlsx|xls)$", file_name, ignore.case = TRUE)) {
            df <- readxl::read_excel(file_path)
            df <- as.data.frame(df)
          }
          
          df$source_file <- file_name
          all_data[[i]] <- df
        }
        
        values$status_text <- get_lang_text("messages.processing.merging_files", "合併檔案...")

        # Combine files
        all_columns <- unique(unlist(lapply(all_data, names)))
        for (i in seq_along(all_data)) {
          missing_cols <- setdiff(all_columns, names(all_data[[i]]))
          for (col in missing_cols) {
            all_data[[i]][[col]] <- NA
          }
          all_data[[i]] <- all_data[[i]][all_columns]
        }

        combined_raw <- do.call(rbind, all_data)

        # 如果已有從步驟1的資料，合併它們
        if (!is.null(values$combined_data)) {
          existing_data <- values$combined_data
          # 確保欄位一致
          all_cols_combined <- unique(c(names(combined_raw), names(existing_data)))
          for (col in setdiff(all_cols_combined, names(combined_raw))) {
            combined_raw[[col]] <- NA
          }
          for (col in setdiff(all_cols_combined, names(existing_data))) {
            existing_data[[col]] <- NA
          }
          combined_raw <- rbind(existing_data, combined_raw)
        }

        values$status_text <- sprintf(
          get_lang_text("messages.success.files_merge_complete", "合併完成，共 %s 筆記錄"),
          nrow(combined_raw)
        )
        
        # Detect fields
        if (input$auto_detect) {
          fields <- detect_and_standardize_fields(combined_raw)
          
          if (is.null(fields$customer_id) || is.null(fields$time) || is.null(fields$amount)) {
            values$status_text <- get_lang_text("messages.error.field_detection_failed", "錯誤: 無法偵測必要欄位")
            return()
          }
          
          # Standardize data
          standardized_data <- combined_raw %>%
            rename(
              customer_id = !!sym(fields$customer_id),
              payment_time = !!sym(fields$time),
              lineitem_price = !!sym(fields$amount)
            )
        } else {
          standardized_data <- combined_raw
        }
        
        # Clean and process data
        processed_data <- standardized_data %>%
          mutate(
            # 保留原始電子郵件以供參考
            original_customer_id = customer_id,
            # 創建電子郵件到數字ID的一對一映射
            customer_id = if(is.character(customer_id)) {
              if(any(grepl("@", customer_id, fixed = TRUE))) {
                # 為每個唯一的電子郵件創建數字ID
                as.integer(as.factor(customer_id))
              } else {
                as.integer(customer_id)
              }
            } else {
              as.integer(customer_id)
            },
            payment_time = as.POSIXct(payment_time),
            lineitem_price = as.numeric(lineitem_price),
            platform_id = "upload"
          ) %>%
          filter(
            !is.na(customer_id),
            !is.na(payment_time),
            !is.na(lineitem_price),
            lineitem_price > 0
          ) %>%
          arrange(customer_id, payment_time)
        
        # 創建電子郵件到數字ID的映射表以供後續使用
        if(any(grepl("@", standardized_data$customer_id, fixed = TRUE))) {
          email_to_id_mapping <- processed_data %>%
            select(original_customer_id, customer_id) %>%
            distinct() %>%
            arrange(customer_id)
          values$email_mapping <- email_to_id_mapping
          values$status_text <- sprintf(
            get_lang_text("messages.success.email_mapping_complete", "✅ 電子郵件映射完成: {count} 個唯一客戶"),
            count = nrow(email_to_id_mapping)
          )
        }

        values$combined_data <- processed_data
        values$status_text <- sprintf(
          get_lang_text("messages.success.data_processing_complete", "✅ 資料處理完成，有效記錄: {records} 客戶數: {customers}"),
          records = nrow(processed_data),
          customers = length(unique(processed_data$customer_id))
        )
        
        # 使用通用分析函數
        analyze_data(processed_data, 1, 0.1)  # 固定最少交易次數為1，時間折扣因子為0.1
        
      }, error = function(e) {
        values$status_text <- sprintf(
          get_lang_text("messages.error.processing_error", "處理錯誤: {error}"),
          error = e$message
        )
      })
    })
    
    # Status output
    output$status <- renderText({
      values$status_text
    })

    # ========== Helper Functions ==========

    # Update metric description display
    updateMetricDescription <- function(metric) {
      data <- get_active_dna_data()
      req(data)

      # Load hints (if hint system is available)
      hints_df <- tryCatch(load_hints(language = current_language(), app_name = "vitalsigns"), error = function(e) NULL)

      # Get hint var_id based on metric
      hint_var_id <- switch(metric,
        "M" = "monetary_stat",
        "R" = "recency_stat",
        "F" = "frequency_stat",
        "IPT" = "ipt_stat",
        "CAI" = "cai_stat",
        "PCV" = "pcv_stat",
        "CRI" = "cri_stat",
        "NES" = "customer_segmentation"
      )

      # Get hint text
      hint_text <- if(!is.null(hints_df) && hint_var_id %in% hints_df$var_id) {
        hints_df[hints_df$var_id == hint_var_id, "description"][1]
      } else {
        ""
      }

      description <- switch(metric,
        "M" = {
          m_values <- data$m_value[!is.na(data$m_value)]
          m_median <- median(m_values)
          m_mean <- mean(m_values)
          m_var <- var(m_values)
          m_q25 <- quantile(m_values, 0.25)
          m_q75 <- quantile(m_values, 0.75)

          # 80/20 rule grouping
          m_q20 <- quantile(m_values, 0.2)
          m_q80 <- quantile(m_values, 0.8)
          high_count <- sum(m_values >= m_q80)
          low_count <- sum(m_values <= m_q20)
          mid_count <- length(m_values) - high_count - low_count

          paste0(
            "💰 <b>", get_lang_text("metric_analysis.monetary_title", "購買金額分析"), "</b>",
            if(hint_text != "") {
              paste0(" <i class='fas fa-info-circle' style='font-size: 12px; color: #17a2b8;' ",
                     "data-toggle='tooltip' data-placement='top' title='", hint_text, "'></i>")
            } else "",
            "<br>",
            "• <b>", get_lang_text("statistics.definition", "定義"), "：</b>", get_lang_text("metric_analysis.monetary_definition", "平均單次購買金額"), "<br>",
            "• <b>", get_lang_text("statistics.mean", "平均數"), "：</b>$", round(m_mean, 2), "<br>",
            "• <b>", get_lang_text("statistics.median", "中位數"), "：</b>$", round(m_median, 2), "<br>",
            "• <b>", get_lang_text("statistics.quartiles", "25%/75%分位數"), "：</b>$", round(m_q25, 2), " / $", round(m_q75, 2), "<br>",
            "• <b>", get_lang_text("statistics.variance", "變異數"), "：</b>", round(m_var, 2), "<br>",
            "• <b>", get_lang_text("metric_analysis.overall_profile", "整體輪廓"), "：</b>",
            if(m_var > (m_mean^2 * 0.5)) get_lang_text("metric_analysis.monetary_dispersed", "購買金額離散程度大") else get_lang_text("metric_analysis.monetary_concentrated", "購買金額相對集中"),
            "<br><br>",
            "📊 <b>", get_lang_text("metric_analysis.pareto_segmentation", "80/20分組"), "：</b><br>",
            "• ", get_lang_text("metric_analysis.high_value", "高價值"), "(≥$", round(m_q80, 2), ")：", high_count, get_lang_text("metric_analysis.people_unit", "人"), "<br>",
            "• ", get_lang_text("metric_analysis.mid_value", "中價值"), "：", mid_count, get_lang_text("metric_analysis.people_unit", "人"), "<br>",
            "• ", get_lang_text("metric_analysis.low_value", "低價值"), "(≤$", round(m_q20, 2), ")：", low_count, get_lang_text("metric_analysis.people_unit", "人")
          )
        },
        "R" = {
          r_values <- data$r_value[!is.na(data$r_value)]
          r_median <- median(r_values)
          r_mean <- mean(r_values)
          r_var <- var(r_values)
          r_q25 <- quantile(r_values, 0.25)
          r_q75 <- quantile(r_values, 0.75)

          # 80/20 rule - lower R is better (more recent)
          r_q20 <- quantile(r_values, 0.2)
          r_q80 <- quantile(r_values, 0.8)
          active_count <- sum(r_values <= r_q20)
          inactive_count <- sum(r_values >= r_q80)
          mid_count <- length(r_values) - active_count - inactive_count

          paste0(
            "🕒 <b>", get_lang_text("metric_analysis.recency_title", "最近來店時間分析"), "</b>",
            if(hint_text != "") {
              paste0(" <i class='fas fa-info-circle' style='font-size: 12px; color: #17a2b8;' ",
                     "data-toggle='tooltip' data-placement='top' title='", hint_text, "'></i>")
            } else "",
            "<br>",
            "• <b>", get_lang_text("statistics.definition", "定義"), "：</b>", get_lang_text("metric_analysis.recency_definition", "距離最近一次購買的天數"), "<br>",
            "• <b>", get_lang_text("statistics.mean", "平均數"), "：</b>", round(r_mean, 0), " ", get_lang_text("units.days", "天"), "<br>",
            "• <b>", get_lang_text("statistics.median", "中位數"), "：</b>", round(r_median, 0), " ", get_lang_text("units.days", "天"), "<br>",
            "• <b>", get_lang_text("statistics.quartiles", "25%/75%分位數"), "：</b>", round(r_q25, 0), " / ", round(r_q75, 0), " ", get_lang_text("units.days", "天"), "<br>",
            "• <b>", get_lang_text("statistics.variance", "變異數"), "：</b>", round(r_var, 0), "<br>",
            "• <b>", get_lang_text("metric_analysis.overall_profile", "整體輪廓"), "：</b>",
            if(r_var > (r_mean^2 * 0.5)) get_lang_text("metric_analysis.recency_dispersed", "來店時間離散程度大") else get_lang_text("metric_analysis.recency_concentrated", "來店時間相對一致"),
            "<br><br>",
            "📊 <b>", get_lang_text("metric_analysis.pareto_segmentation", "80/20分組"), "：</b><br>",
            "• ", get_lang_text("metric_analysis.high_active", "高度活躍"), "(≤", round(r_q20, 0), get_lang_text("units.days", "天"), ")：", active_count, get_lang_text("metric_analysis.people_unit", "人"), "<br>",
            "• ", get_lang_text("metric_analysis.mid_active", "中度活躍"), "：", mid_count, get_lang_text("metric_analysis.people_unit", "人"), "<br>",
            "• ", get_lang_text("metric_analysis.low_active", "不活躍"), "(≥", round(r_q80, 0), get_lang_text("units.days", "天"), ")：", inactive_count, get_lang_text("metric_analysis.people_unit", "人")
          )
        },
        "F" = {
          f_values <- data$f_value[!is.na(data$f_value)]
          f_median <- median(f_values)
          f_mean <- mean(f_values)
          f_var <- var(f_values)
          f_q25 <- quantile(f_values, 0.25)
          f_q75 <- quantile(f_values, 0.75)

          # 80/20 grouping
          f_q20 <- quantile(f_values, 0.2)
          f_q80 <- quantile(f_values, 0.8)
          high_freq <- sum(f_values >= f_q80)
          low_freq <- sum(f_values <= f_q20)
          mid_freq <- length(f_values) - high_freq - low_freq

          paste0(
            "🔁 <b>", get_lang_text("metric_analysis.frequency_title", "購買頻率分析"), "</b>",
            if(hint_text != "") {
              paste0(" <i class='fas fa-info-circle' style='font-size: 12px; color: #17a2b8;' ",
                     "data-toggle='tooltip' data-placement='top' title='", hint_text, "'></i>")
            } else "",
            "<br>",
            "• <b>", get_lang_text("statistics.definition", "定義"), "：</b>", get_lang_text("metric_analysis.frequency_definition", "觀察期間內的總購買次數"), "<br>",
            "• <b>", get_lang_text("statistics.mean", "平均數"), "：</b>", round(f_mean, 1), " ", get_lang_text("units.times", "次"), "<br>",
            "• <b>", get_lang_text("statistics.median", "中位數"), "：</b>", round(f_median, 1), " ", get_lang_text("units.times", "次"), "<br>",
            "• <b>", get_lang_text("statistics.quartiles", "25%/75%分位數"), "：</b>", round(f_q25, 1), " / ", round(f_q75, 1), " ", get_lang_text("units.times", "次"), "<br>",
            "• <b>", get_lang_text("statistics.variance", "變異數"), "：</b>", round(f_var, 2), "<br>",
            "• <b>", get_lang_text("metric_analysis.overall_profile", "整體輪廓"), "：</b>",
            if(f_var > (f_mean^2 * 0.5)) get_lang_text("metric_analysis.frequency_dispersed", "購買頻率差異大需分群管理") else get_lang_text("metric_analysis.frequency_concentrated", "購買頻率較為一致"),
            "<br><br>",
            "📊 <b>", get_lang_text("metric_analysis.pareto_segmentation", "80/20分組"), "：</b><br>",
            "• ", get_lang_text("metric_analysis.high_frequency", "高頻客戶"), "(≥", round(f_q80, 1), get_lang_text("units.times", "次"), ")：", high_freq, get_lang_text("metric_analysis.people_unit", "人"), "<br>",
            "• ", get_lang_text("metric_analysis.mid_frequency", "中頻客戶"), "：", mid_freq, get_lang_text("metric_analysis.people_unit", "人"), "<br>",
            "• ", get_lang_text("metric_analysis.low_frequency", "低頻客戶"), "(≤", round(f_q20, 1), get_lang_text("units.times", "次"), ")：", low_freq, get_lang_text("metric_analysis.people_unit", "人")
          )
        },
        "IPT" = {
          if("ipt_mean" %in% names(data)) {
            ipt_values <- data$ipt_mean[!is.na(data$ipt_mean)]
            ipt_median <- median(ipt_values)
            ipt_mean <- mean(ipt_values)
            ipt_var <- var(ipt_values)
            ipt_q25 <- quantile(ipt_values, 0.25)
            ipt_q75 <- quantile(ipt_values, 0.75)

            ipt_q20 <- quantile(ipt_values, 0.2)
            ipt_q80 <- quantile(ipt_values, 0.8)
            short_cycle <- sum(ipt_values <= ipt_q20)
            long_cycle <- sum(ipt_values >= ipt_q80)
            mid_cycle <- length(ipt_values) - short_cycle - long_cycle

            paste0(
              "⏱️ <b>", get_lang_text("metric_analysis.ipt_title", "購買週期分析"), "</b>",
              if(hint_text != "") {
                paste0(" <i class='fas fa-info-circle' style='font-size: 12px; color: #17a2b8;' ",
                       "data-toggle='tooltip' data-placement='top' title='", hint_text, "'></i>")
              } else "",
              "<br>",
              "• <b>", get_lang_text("statistics.definition", "定義"), "：</b>", get_lang_text("metric_analysis.ipt_definition", "平均購買間隔天數"), "<br>",
              "• <b>", get_lang_text("statistics.mean", "平均數"), "：</b>", round(ipt_mean, 1), " ", get_lang_text("units.days", "天"), "<br>",
              "• <b>", get_lang_text("statistics.median", "中位數"), "：</b>", round(ipt_median, 1), " ", get_lang_text("units.days", "天"), "<br>",
              "• <b>", get_lang_text("statistics.quartiles", "25%/75%分位數"), "：</b>", round(ipt_q25, 1), " / ", round(ipt_q75, 1), " ", get_lang_text("units.days", "天"), "<br>",
              "• <b>", get_lang_text("statistics.variance", "變異數"), "：</b>", round(ipt_var, 2), "<br>",
              "• <b>", get_lang_text("metric_analysis.overall_profile", "整體輪廓"), "：</b>",
              if(ipt_var > (ipt_mean^2 * 0.5)) get_lang_text("metric_analysis.ipt_dispersed", "購買週期差異大需分群管理") else get_lang_text("metric_analysis.ipt_concentrated", "購買週期較為一致"),
              "<br><br>",
              "📊 <b>", get_lang_text("metric_analysis.cycle_segmentation", "週期分組"), "：</b><br>",
              "• ", get_lang_text("metric_analysis.short_cycle", "短週期客戶"), "(≤", round(ipt_q20, 1), get_lang_text("units.days", "天"), ")：", short_cycle, get_lang_text("metric_analysis.people_unit", "人"), "<br>",
              "• ", get_lang_text("metric_analysis.mid_cycle", "中週期客戶"), "：", mid_cycle, get_lang_text("metric_analysis.people_unit", "人"), "<br>",
              "• ", get_lang_text("metric_analysis.long_cycle", "長週期客戶"), "(≥", round(ipt_q80, 1), get_lang_text("units.days", "天"), ")：", long_cycle, get_lang_text("metric_analysis.people_unit", "人")
            )
          } else {
            paste0("⏱️ <b>", get_lang_text("metric_analysis.ipt_title", "購買週期分析"), "</b><br>• ", get_lang_text("metric_analysis.no_ipt_data", "無購買週期數據"))
          }
        },
        "CAI" = {
          if("cai_value" %in% names(data)) {
            cai_values <- data$cai_value[!is.na(data$cai_value)]
            cai_mean <- mean(cai_values)
            cai_median <- median(cai_values)
            cai_var <- var(cai_values)
            cai_q25 <- quantile(cai_values, 0.25)
            cai_q75 <- quantile(cai_values, 0.75)

            cai_active <- sum(cai_values > 0, na.rm = TRUE)
            cai_stable <- sum(abs(cai_values) <= 0.1, na.rm = TRUE)
            cai_declining <- sum(cai_values < -0.1, na.rm = TRUE)
            total <- length(cai_values)

            cai_q20 <- quantile(cai_values, 0.2)
            cai_q80 <- quantile(cai_values, 0.8)
            high_active <- sum(cai_values >= cai_q80, na.rm = TRUE)
            low_active <- sum(cai_values <= cai_q20, na.rm = TRUE)

            paste0(
              "📈 <b>", get_lang_text("metric_analysis.cai_title", "顧客活躍度分析"), "</b>",
              if(hint_text != "") {
                paste0(" <i class='fas fa-info-circle' style='font-size: 12px; color: #17a2b8;' ",
                       "data-toggle='tooltip' data-placement='top' title='", hint_text, "'></i>")
              } else "",
              "<br>",
              "• <b>", get_lang_text("statistics.definition", "定義"), "：</b>", get_lang_text("metric_analysis.cai_definition", "購買行為趨勢指標 (>0漸趨活躍，<0漸趨靜止)"), "<br>",
              "• <b>", get_lang_text("metric_analysis.management_meaning", "管理意涵"), "：</b>", get_lang_text("metric_analysis.cai_meaning", "預測客戶未來行為，及早識別流失風險"), "<br><br>",
              "📊 <b>", get_lang_text("metric_analysis.statistical_summary", "統計摘要"), "：</b><br>",
              "• <b>", get_lang_text("statistics.mean", "平均數"), "：</b>", round(cai_mean, 3), "<br>",
              "• <b>", get_lang_text("statistics.median", "中位數"), "：</b>", round(cai_median, 3), "<br>",
              "• <b>", get_lang_text("statistics.quartiles", "25%/75%分位數"), "：</b>", round(cai_q25, 3), " / ", round(cai_q75, 3), "<br>",
              "• <b>", get_lang_text("statistics.variance", "變異數"), "：</b>", round(cai_var, 5), "<br>",
              "• <b>", get_lang_text("metric_analysis.overall_profile", "整體輪廓"), "：</b>",
              if(cai_var > 0.01) get_lang_text("metric_analysis.cai_dispersed", "活躍度差異大需分群管理") else get_lang_text("metric_analysis.cai_concentrated", "活躍度較為一致"),
              "<br><br>",
              "📊 <b>", get_lang_text("metric_analysis.pareto_segmentation", "80/20分組"), "：</b><br>",
              "• ", get_lang_text("metric_analysis.high_activity", "高活躍"), "(≥", round(cai_q80, 3), ")：", high_active, get_lang_text("metric_analysis.people_unit", "人"), "<br>",
              "• ", get_lang_text("metric_analysis.mid_activity", "中活躍"), "：", total - high_active - low_active, get_lang_text("metric_analysis.people_unit", "人"), "<br>",
              "• ", get_lang_text("metric_analysis.low_activity", "低活躍"), "(≤", round(cai_q20, 3), ")：", low_active, get_lang_text("metric_analysis.people_unit", "人"), "<br><br>",
              "💡 <b>", get_lang_text("metric_analysis.status_distribution", "狀態分布"), "：</b><br>",
              "• <b>", get_lang_text("metric_analysis.trending_active", "漸趨活躍"), "(>0)：</b>", cai_active, " ", get_lang_text("metric_analysis.people_unit", "人"), " (", round(cai_active/total*100, 1), "%)<br>",
              "• <b>", get_lang_text("metric_analysis.stable", "穩定"), "(≈0)：</b>", cai_stable, " ", get_lang_text("metric_analysis.people_unit", "人"), " (", round(cai_stable/total*100, 1), "%)<br>",
              "• <b>", get_lang_text("metric_analysis.trending_static", "漸趨靜止"), "(<0)：</b>", cai_declining, " ", get_lang_text("metric_analysis.people_unit", "人"), " (", round(cai_declining/total*100, 1), "%)<br><br>",
              "🎯 <b>", get_lang_text("metric_analysis.marketing_suggestions", "行銷建議"), "：</b><br>",
              "• ", get_lang_text("metric_analysis.active_customers", "活躍客戶"), "：", get_lang_text("metric_analysis.active_suggestion", "把握成長動能，推薦新品"), "<br>",
              "• ", get_lang_text("metric_analysis.stable_customers", "穩定客戶"), "：", get_lang_text("metric_analysis.stable_suggestion", "維持服務水準，定期關懷"), "<br>",
              "• ", get_lang_text("metric_analysis.static_customers", "靜止客戶"), "：", get_lang_text("metric_analysis.static_suggestion", "緊急挽回措施，特殊優惠")
            )
          } else {
            paste0("📈 <b>", get_lang_text("metric_analysis.cai_title", "顧客活躍度"), "</b><br>• ", get_lang_text("metric_analysis.insufficient_data", "資料不足"))
          }
        },
        "PCV" = {
          if("pcv" %in% names(data) || "total_spent" %in% names(data)) {
            pcv_col <- if("pcv" %in% names(data)) "pcv" else "total_spent"
            pcv_values <- data[[pcv_col]][!is.na(data[[pcv_col]])]
            pcv_mean <- mean(pcv_values)
            pcv_median <- median(pcv_values)
            pcv_q25 <- quantile(pcv_values, 0.25)
            pcv_q75 <- quantile(pcv_values, 0.75)
            pcv_var <- var(pcv_values)

            pcv_q20 <- quantile(pcv_values, 0.2)
            pcv_q80 <- quantile(pcv_values, 0.8)
            high_value <- sum(pcv_values >= pcv_q80)
            low_value <- sum(pcv_values <= pcv_q20)
            mid_value <- length(pcv_values) - high_value - low_value

            paste0(
              "💎 <b>", get_lang_text("modules.vitalsigns_dna.metric_analysis.pcv_title", "過去價值分析"), "</b>",
              if(hint_text != "") {
                paste0(" <i class='fas fa-info-circle' style='font-size: 12px; color: #17a2b8;' ",
                       "data-toggle='tooltip' data-placement='top' title='", hint_text, "'></i>")
              } else "",
              "<br>",
              "• <b>", get_lang_text("modules.vitalsigns_dna.statistics.definition", "定義"), "：</b>",
              get_lang_text("modules.vitalsigns_dna.metric_analysis.pcv_definition", "歷史累積消費總額"), "<br>",
              "• <b>", get_lang_text("modules.vitalsigns_dna.statistics.mean", "平均數"), "：</b>$", round(pcv_mean, 2), "<br>",
              "• <b>", get_lang_text("modules.vitalsigns_dna.statistics.median", "中位數"), "：</b>$", round(pcv_median, 2), "<br>",
              "• <b>", get_lang_text("modules.vitalsigns_dna.statistics.quartiles", "25%/75%分位數"), "：</b>$", round(pcv_q25, 2), " / $", round(pcv_q75, 2), "<br>",
              "• <b>", get_lang_text("modules.vitalsigns_dna.statistics.variance", "變異數"), "：</b>", round(pcv_var, 2), "<br><br>",
              "📊 <b>", get_lang_text("modules.vitalsigns_dna.metric_analysis.pareto_segmentation", "80/20分組"), "：</b><br>",
              "• ", get_lang_text("modules.vitalsigns_dna.metric_analysis.high_value", "高價值"),
              "(≥$", round(pcv_q80, 2), ")：", high_value, get_lang_text("modules.vitalsigns_dna.metric_analysis.people_unit", "人"), "<br>",
              "• ", get_lang_text("modules.vitalsigns_dna.metric_analysis.mid_value", "中價值"),
              "：", mid_value, get_lang_text("modules.vitalsigns_dna.metric_analysis.people_unit", "人"), "<br>",
              "• ", get_lang_text("modules.vitalsigns_dna.metric_analysis.low_value", "低價值"),
              "(≤$", round(pcv_q20, 2), ")：", low_value, get_lang_text("modules.vitalsigns_dna.metric_analysis.people_unit", "人"), "<br><br>",
              "🎯 <b>", get_lang_text("modules.vitalsigns_dna.metric_analysis.marketing_suggestions", "行銷建議"), "：</b><br>",
              "• ", get_lang_text("modules.vitalsigns_dna.metric_analysis.high_value", "高價值"), "：",
              get_lang_text("modules.vitalsigns_dna.marketing.pcv_high_value", "專屬VIP服務"), "<br>",
              "• ", get_lang_text("modules.vitalsigns_dna.metric_analysis.mid_value", "中價值"), "：",
              get_lang_text("modules.vitalsigns_dna.marketing.pcv_mid_value", "提升計劃"), "<br>",
              "• ", get_lang_text("modules.vitalsigns_dna.metric_analysis.low_value", "低價值"), "：",
              get_lang_text("modules.vitalsigns_dna.marketing.pcv_low_value", "激活策略")
            )
          } else {
            paste0("💎 <b>", get_lang_text("modules.vitalsigns_dna.metric_analysis.pcv_title", "過去價值分析"), "</b><br>• ",
                   get_lang_text("metric_analysis.insufficient_data", "資料不足"))
          }
        },
        "CRI" = {
          if("cri" %in% names(data)) {
            cri_values <- data$cri[!is.na(data$cri)]
            cri_mean <- mean(cri_values)
            cri_median <- median(cri_values)
            cri_var <- var(cri_values)
            cri_q25 <- quantile(cri_values, 0.25)
            cri_q75 <- quantile(cri_values, 0.75)

            cri_q20 <- quantile(cri_values, 0.2)
            cri_q80 <- quantile(cri_values, 0.8)
            high_value <- sum(cri_values >= cri_q80, na.rm = TRUE)
            low_value <- sum(cri_values <= cri_q20, na.rm = TRUE)
            total <- length(cri_values)

            paste0(
              "🎯 <b>", get_lang_text("modules.vitalsigns_dna.metric_analysis.cri_title", "顧客穩定度指標分析"), "</b>",
              if(hint_text != "") {
                paste0(" <i class='fas fa-info-circle' style='font-size: 12px; color: #17a2b8;' ",
                       "data-toggle='tooltip' data-placement='top' title='", hint_text, "'></i>")
              } else "",
              "<br>",
              "• <b>", get_lang_text("modules.vitalsigns_dna.statistics.definition", "定義"), "：</b>",
              get_lang_text("modules.vitalsigns_dna.metric_analysis.cri_definition", "顧客交易穩定度綜合評分"), "<br>",
              "• <b>", get_lang_text("modules.vitalsigns_dna.metric_analysis.calculation_method", "計算方式"), "：</b>",
              get_lang_text("modules.vitalsigns_dna.metric_analysis.cri_calculation", "RFM加權分數 (0.3×R + 0.3×F + 0.4×M) 標準化至0-100分"), "<br>",
              "• <b>", get_lang_text("modules.vitalsigns_dna.metric_analysis.management_implication", "管理意涵"), "：</b>",
              get_lang_text("modules.vitalsigns_dna.metric_analysis.cri_meaning", "識別穩定貢獻客群，採用80/20法則優化資源配置"), "<br><br>",
              "📊 <b>", get_lang_text("modules.vitalsigns_dna.statistics.summary", "統計摘要"), "：</b><br>",
              "• <b>", get_lang_text("modules.vitalsigns_dna.statistics.average_score", "平均分數"), "：</b>", round(cri_mean * 100, 1), " ",
              get_lang_text("modules.vitalsigns_dna.metric_analysis.score_unit", "分"), "<br>",
              "• <b>", get_lang_text("modules.vitalsigns_dna.statistics.median", "中位數"), "：</b>", round(cri_median * 100, 1), " ",
              get_lang_text("modules.vitalsigns_dna.metric_analysis.score_unit", "分"), "<br>",
              "• <b>", get_lang_text("modules.vitalsigns_dna.statistics.quartiles", "25%/75%分位數"), "：</b>", round(cri_q25 * 100, 1), " / ", round(cri_q75 * 100, 1), " ",
              get_lang_text("modules.vitalsigns_dna.metric_analysis.score_unit", "分"), "<br>",
              "• <b>", get_lang_text("modules.vitalsigns_dna.statistics.variance", "變異數"), "：</b>", round(cri_var * 10000, 2), "<br>",
              "• <b>", get_lang_text("modules.vitalsigns_dna.metric_analysis.overall_profile", "整體輪廓"), "：</b>",
              if(cri_var > 0.01) get_lang_text("modules.vitalsigns_dna.metric_analysis.cri_dispersed", "穩定度差異大，需差異化服務策略")
              else get_lang_text("modules.vitalsigns_dna.metric_analysis.cri_concentrated", "穩定度較為平均"),
              "<br><br>",
              "📊 <b>", get_lang_text("modules.vitalsigns_dna.metric_analysis.pareto_rule_grouping"), "</b><br>",
              "• ", get_lang_text("modules.vitalsigns_dna.metric_analysis.high_stability"),
              "(Top 20%, ≥", round(cri_q80 * 100, 1), get_lang_text("modules.vitalsigns_dna.metric_analysis.score_unit"), ")：", high_value,
              get_lang_text("modules.vitalsigns_dna.metric_analysis.people_unit"), "<br>",
              "• ", get_lang_text("modules.vitalsigns_dna.metric_analysis.mid_stability"),
              "(Middle 60%)：", total - high_value - low_value, get_lang_text("modules.vitalsigns_dna.metric_analysis.people_unit"), "<br>",
              "• ", get_lang_text("modules.vitalsigns_dna.metric_analysis.low_stability"),
              "(Bottom 20%, ≤", round(cri_q20 * 100, 1), get_lang_text("modules.vitalsigns_dna.metric_analysis.score_unit"), ")：", low_value,
              get_lang_text("modules.vitalsigns_dna.metric_analysis.people_unit"), "<br><br>",
              "💡 <b>", get_lang_text("modules.vitalsigns_dna.metric_analysis.stability_interpretation"), "</b><br>",
              "• Top 20%：", get_lang_text("modules.vitalsigns_dna.marketing.cri_top_20_desc"), "<br>",
              "• Middle 60%：", get_lang_text("modules.vitalsigns_dna.marketing.cri_middle_60_desc"), "<br>",
              "• Bottom 20%：", get_lang_text("modules.vitalsigns_dna.marketing.cri_bottom_20_desc")
            )
          } else {
            paste0("🎯 <b>", get_lang_text("modules.vitalsigns_dna.metric_analysis.cri_title", "顧客穩定度指標分析"), "</b><br>• ",
                   get_lang_text("metric_analysis.insufficient_data", "資料不足"))
          }
        },
        "NES" = {
          nes_table <- table(data$nes_status)
          paste0(
            "👥 <b>", get_lang_text("modules.vitalsigns_dna.metric_analysis.nes_title", "顧客狀態分析"), "</b>",
            if(hint_text != "") {
              paste0(" <i class='fas fa-info-circle' style='font-size: 12px; color: #17a2b8;' ",
                     "data-toggle='tooltip' data-placement='top' title='", hint_text, "'></i>")
            } else "",
            "<br>",
            "• ", get_lang_text("modules.vitalsigns_dna.metric_analysis.new_customer", "新客"), "(N)：", nes_table["N"] %||% 0, " ",
            get_lang_text("modules.vitalsigns_dna.metric_analysis.people_unit", "人"), "<br>",
            "• ", get_lang_text("modules.vitalsigns_dna.metric_analysis.main_customer", "主力"), "(E0)：", nes_table["E0"] %||% 0, " ",
            get_lang_text("modules.vitalsigns_dna.metric_analysis.people_unit", "人"), "<br>",
            "• ", get_lang_text("modules.vitalsigns_dna.metric_analysis.sleeping_customer", "沉睡"), "(S1-S3)：",
            sum(nes_table[c("S1", "S2", "S3")], na.rm = TRUE), " ",
            get_lang_text("modules.vitalsigns_dna.metric_analysis.people_unit", "人")
          )
        }
      )

      output$metric_ai_description <- renderUI({
        req(lang_texts())  # React to language changes
        HTML(description)
      })
    }

    # Update display for selected metric
    updateDisplayForMetric <- function(metric) {
      # 若尚未有完整分群資料，嘗試自動執行一次完整分群，避免彈出提示
      if (is.null(values$complete_segmented_data) && !is.null(values$dna_results)) {
        performCompleteSegmentation()
      }
      if (is.null(values$complete_segmented_data)) {
        return(NULL)
      }

      data <- values$complete_segmented_data
      values$segmentation_completed <- TRUE
      values$segmentation_completed <- TRUE

      # Get segment column based on metric
      segment_col <- switch(metric,
        "R" = "segment_r",
        "F" = "segment_f",
        "M" = "segment_m",
        "IPT" = "segment_ipt",
        "CAI" = "segment_cai",
        "PCV" = "segment_pcv",
        "CRI" = "segment_cri",
        "NES" = "segment_nes"
      )

      if(segment_col %in% names(data)) {
        data$segment <- data[[segment_col]]
        values$segmented_data <- data
        values$current_metric <- metric

        # Store display data (using original data, column names handled by language system)
        values$temp_chinese_table <- data
      }
    }

    # ========== End Helper Functions ==========

    # Metric button handlers - 只切換顯示，不執行分群
    observeEvent(input$show_m, {
      values$current_metric <- "M"
      updateMetricDescription("M")
      updateDisplayForMetric("M")  # 更新顯示的分群資料
    })
    observeEvent(input$show_r, {
      values$current_metric <- "R"
      updateMetricDescription("R")
      updateDisplayForMetric("R")  # 更新顯示的分群資料
    })
    observeEvent(input$show_f, {
      values$current_metric <- "F"
      updateMetricDescription("F")
      updateDisplayForMetric("F")  # 更新顯示的分群資料
    })
    observeEvent(input$show_ipt, {
      values$current_metric <- "IPT"
      values$current_metric_name <- get_lang_text("dna_metrics.ipt.full_name", "Inter-Purchase Time")
      updateMetricDescription("IPT")
      updateDisplayForMetric("IPT")  # 更新顯示的分群資料
    })

    observeEvent(input$show_cai, {
      values$current_metric <- "CAI"
      updateMetricDescription("CAI")
      updateDisplayForMetric("CAI")  # 更新顯示的分群資料
    })
    observeEvent(input$show_pcv, {
      values$current_metric <- "PCV"
      updateMetricDescription("PCV")
      updateDisplayForMetric("PCV")  # 更新顯示的分群資料
    })
    observeEvent(input$show_cri, {
      values$current_metric <- "CRI"
      updateMetricDescription("CRI")
      updateDisplayForMetric("CRI")  # 更新顯示的分群資料
    })
    observeEvent(input$show_nes, {
      values$current_metric <- "NES"
      updateMetricDescription("NES")
      updateDisplayForMetric("NES")  # 更新顯示的分群資料
    })
    
    # Tab 切換：同步 current_metric & 內容
    observeEvent(input$ai_metric_tabs, {
      tab_val <- input$ai_metric_tabs
      metric <- switch(tab_val,
        "tab_r" = "R",
        "tab_f" = "F",
        "tab_m" = "M",
        "tab_ipt" = "IPT",
        "tab_cai" = "CAI",
        "tab_pcv" = "PCV",
        "tab_cri" = "CRI",
        "tab_nes" = "NES",
        values$current_metric
      )
      values$current_metric <- metric
      updateMetricDescription(metric)
      updateDisplayForMetric(metric)
    }, ignoreInit = TRUE)

    # 新增：執行客戶分群按鈕 (已整合 AI 分析)
    observeEvent(input$run_segmentation, {
      req(values$dna_results)

      # 禁用按鈕避免重複點擊
      tryCatch({
        shinyjs::disable("run_segmentation")
      }, error = function(e) {
        # 如果 shinyjs 出錯，繼續執行
      })

      # 執行完整的分群分析
      performCompleteSegmentation()

      # 執行 AI 分析（2024-12-28 新增：整合 AI 分析到分群按鈕）
      tryCatch({
        ai_summary <- generateAIAnalysisSummary()
        ai_recommendations <- generateAIRecommendations()

        values$ai_analysis_summary <- ai_summary
        values$ai_recommendations <- ai_recommendations
        values$ai_analysis_done <- TRUE  # 關鍵：設置 AI 分析完成標記
        values$ai_summary_cache[[values$current_metric]] <- ai_summary
        values$ai_recommendations_cache[[values$current_metric]] <- ai_recommendations
        values$ai_analysis_done_by_metric[[values$current_metric]] <- TRUE

        showNotification(
          get_lang_text("messages.success.ai_complete", "✅ AI分析和洞察已完成！"),
          type = "message",
          duration = 2
        )
      }, error = function(e) {
        showNotification(
          paste(get_lang_text("messages.error.ai_analysis_error", "AI分析錯誤:"), e$message),
          type = "error",
          duration = 5
        )
      })

      # 重新啟用按鈕
      tryCatch({
        shinyjs::enable("run_segmentation")
      }, error = function(e) {
        # 如果 shinyjs 出錯，忽略
      })
    })
    
    # AI Analysis button handler（專門用於AI增強分析）
    # NOTE: This is a simplified version that works without the AI manager
    # The full AI analysis manager from VitalSigns requires utils/ai_analysis_manager.R
    observeEvent(input$run_ai_analysis, {
      req(values$current_metric, values$dna_results, values$segmented_data)

      # 暫時停用按鈕避免重複點擊
      tryCatch({
        shinyjs::disable("run_ai_analysis")
      }, error = function(e) {
        # If shinyjs fails, continue anyway
      })

      # 生成AI分析結果（不使用 AI 管理器，直接生成）
      tryCatch({
        # 直接呼叫函數（它們在同一個模組作用域內）
        ai_summary <- generateAIAnalysisSummary()
        ai_recommendations <- generateAIRecommendations()

        # 更新值
        values$ai_analysis_summary <- ai_summary
        values$ai_recommendations <- ai_recommendations
        values$ai_analysis_done <- TRUE
        values$ai_summary_cache[[values$current_metric]] <- ai_summary
        values$ai_recommendations_cache[[values$current_metric]] <- ai_recommendations
        values$ai_analysis_done_by_metric[[values$current_metric]] <- TRUE

        showNotification(
          get_lang_text("messages.success.ai_complete", "✅ AI增強分析完成！"),
          type = "message",
          duration = 2
        )

      }, error = function(e) {
        showNotification(
          paste(get_lang_text("messages.error.ai_analysis_error", "AI分析錯誤:"), e$message),
          type = "error",
          duration = 5
        )
      })

      # 重新啟用按鈕
      tryCatch({
        shinyjs::enable("run_ai_analysis")
      }, error = function(e) {
        # If shinyjs fails, ignore
      })
    })
    
    # 生成AI分析摘要
    generateAIAnalysisSummary <- function() {
      req(values$segmented_data)
      metric <- values$current_metric %||% "M"

      # 若已經有該指標的快取，直接回傳
      cached <- values$ai_summary_cache[[metric]]
      if (!is.null(cached)) {
        return(cached)
      }

      # 計算各項統計（仍然需要計算供CSV匯出使用）
      segment_stats <- values$segmented_data %>%
        group_by(segment) %>%
        summarise(
          count = n(),
          avg_r = mean(r_value, na.rm = TRUE),
          avg_f = mean(f_value, na.rm = TRUE),
          avg_m = mean(m_value, na.rm = TRUE),
          total_revenue = sum(m_value * f_value, na.rm = TRUE),
          .groups = "drop"
        )

      # 生成簡化的HTML格式分析摘要（移除重複的分組表格）
      metric_name <- switch(values$current_metric,
        "R" = get_lang_text("ui.metrics.recency.name", "Recency"),
        "F" = get_lang_text("ui.metrics.frequency.name", "Frequency"),
        "M" = get_lang_text("ui.metrics.monetary.name", "Monetary"),
        "IPT" = get_lang_text("ui.metrics.ipt.name", "Inter-Purchase Time"),
        "CAI" = get_lang_text("ui.metrics.cai.name", "Customer Activity Index"),
        "PCV" = get_lang_text("ui.metrics.pcv.name", "Past Customer Value"),
        "CRI" = get_lang_text("ui.metrics.cri.name", "Customer Retention Index"),
        "NES" = get_lang_text("ui.metrics.nes.name", "Customer Status")
      )

      # 計算關鍵洞察 - 使用80/20法則（前20%為高價值客戶）
      # 按總營收排序，計算前20%客戶的貢獻
      segment_stats_sorted <- segment_stats[order(-segment_stats$total_revenue), ]
      cumsum_count <- cumsum(segment_stats_sorted$count)
      total_customers <- sum(segment_stats$count)

      # 找出累積到20%客戶數的分群
      high_value_threshold <- total_customers * 0.2
      high_value_segments <- segment_stats_sorted[cumsum_count <= high_value_threshold, ]

      # 如果沒有找到，至少取第一個最高價值的分群
      if(nrow(high_value_segments) == 0) {
        high_value_segments <- segment_stats_sorted[1, ]
      }

      high_value_pct <- round(sum(high_value_segments$count) / total_customers * 100, 1)
      high_value_revenue_pct <- round(sum(high_value_segments$total_revenue) / sum(segment_stats$total_revenue) * 100, 1)

      # 檢查是否可以使用 AI 增強分析
      use_ai <- !is.null(load_prompts(app_name = "vitalsigns")) &&
                Sys.getenv("OPENAI_API_KEY") != "" &&
                exists("execute_gpt_request") &&
                exists("chat_api")

      ai_insights <- NULL

      # 嘗試使用 AI 生成深度洞察
      if (use_ai) {
        tryCatch({
          prompts_df <- load_prompts(app_name = "vitalsigns", language = current_language())

          # 準備分析數據 (Following R092: get_lang_text for all AI prompt text)
          customer_summary <- paste0(
            get_lang_text("ai_prompt_text.total_customers", "總客戶數"), "：", nrow(values$segmented_data),
            get_lang_text("ai_prompt_text.people_unit", "人"), "\n",
            get_lang_text("ai_prompt_text.analysis_metric", "分析指標"), "：", metric_name, " (", values$current_metric, ")\n",
            get_lang_text("ai_prompt_text.pareto_rule", "80/20法則"), "：",
            get_lang_text("ai_prompt_text.high_value_customers", "高價值顧客"), "（", high_value_pct, "%）",
            get_lang_text("ai_prompt_text.contributed", "貢獻了"), " ", high_value_revenue_pct, "% ",
            get_lang_text("ai_prompt_text.of_revenue", "的營收")
          )

          segment_summary <- segment_stats %>%
            mutate(
              summary = paste0(
                get_lang_text("ai_prompt_text.segment", "客群"), "：", segment, "，",
                get_lang_text("ai_prompt_text.count", "人數"), "：", count,
                get_lang_text("ai_prompt_text.people_unit", "人"), "，",
                get_lang_text("ai_prompt_text.avg_r", "平均R"), "：", round(avg_r, 1),
                get_lang_text("ai_prompt_text.days_unit", "天"), "，",
                get_lang_text("ai_prompt_text.avg_f", "平均F"), "：", round(avg_f, 1),
                get_lang_text("ai_prompt_text.times_unit", "次"), "，",
                get_lang_text("ai_prompt_text.avg_m", "平均M"), "：$", round(avg_m, 2)
              )
            ) %>%
            pull(summary) %>%
            paste(collapse = "\n")

          # 使用 AI 生成洞察（含快取）
          # === 快取檢查 ===
          cache_data <- list(
            metric = values$current_metric,
            customer_summary = customer_summary,
            segment_summary = segment_summary,
            high_value_pct = high_value_pct,
            high_value_revenue_pct = high_value_revenue_pct
          )
          cache_key <- get_ai_cache_key("extended_analysis", cache_data, current_language())
          cached_result <- get_cached_ai(cache_key)

          if (!is.null(cached_result)) {
            cat("[DNA] 使用快取的延伸分析 AI 洞察\n")
            ai_insights <- cached_result
          } else {
            ai_result <- execute_gpt_request(
              var_id = "customer_segmentation_analysis",
              variables = list(
                customer_data = customer_summary,
                segment_data = segment_summary
              ),
              chat_api_function = chat_api,
              model = cfg_ai_model,
              prompts_df = prompts_df,
              language = current_language()
            )

            if (!is.null(ai_result) && nchar(ai_result) > 0) {
              set_cached_ai(cache_key, ai_result)
              ai_insights <- ai_result
              cat("✅ AI 洞察生成成功\n")
            }
          }
        }, error = function(e) {
          cat("⚠️ AI 洞察生成失敗:", e$message, "\n")
          ai_insights <- NULL
        })
      }

      # 構建 HTML 摘要
      summary_html <- paste0(
        "<div style='padding: 15px;'>",
        "<h5>📊 ", metric_name, " ", get_lang_text("ai_analysis_extended.analysis_focus"), "</h5>",
        "<p style='color: #666; font-size: 14px;'>",
        "<b>", get_lang_text("ai_analysis_extended.analysis_time"), "：</b>", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "<br>",
        "<b>", get_lang_text("ai_analysis_extended.total_customers"), "：</b>", format(nrow(values$segmented_data), big.mark = ","), " ", get_lang_text("ai_analysis_extended.customers_unit"),
        "</p>",
        "<br>",
        "<div style='background: #f8f9fa; padding: 15px; border-radius: 8px; border-left: 4px solid #007bff;'>",
        "<h6>💡 ", get_lang_text("ai_analysis_extended.key_insights"), "</h6>",
        "<ul style='margin-bottom: 0;'>",
        "<li><b>", get_lang_text("ai_analysis_extended.pareto_rule"), "：</b>",
        get_lang_text("ai_analysis_extended.high_value_customers"), "（", high_value_pct, "%）",
        get_lang_text("ai_analysis_extended.contributed"), " ", high_value_revenue_pct, "% ",
        get_lang_text("ai_analysis_extended.of_revenue"), "</li>",
        "<li><b>", get_lang_text("ai_analysis_extended.metric_focus"), "：</b>",
        get_lang_text("ai_analysis_extended.through"), " ", metric_name, " ",
        get_lang_text("ai_analysis_extended.metric"), "，", get_lang_text("ai_analysis_extended.identified"), " ", nrow(segment_stats), " ",
        get_lang_text("ai_analysis_extended.different_segments"), "</li>",
        "<li><b>", get_lang_text("ai_analysis_extended.strategy_suggestion"), "：</b>",
        get_lang_text("ai_analysis_extended.refer_recommendations"), "</li>",
        "</ul>",
        "</div>"
      )

      # 如果有 AI 洞察，添加到摘要中並使用 markdown 渲染
      if (!is.null(ai_insights)) {
        # 將 AI markdown 轉換為 HTML
        ai_insights_html <- tryCatch({
          markdown::markdownToHTML(
            text = ai_insights,
            fragment.only = TRUE
          )
        }, error = function(e) {
          # 如果 markdown 轉換失敗，使用 pre-wrap 顯示原始文本
          paste0("<div style='white-space: pre-wrap;'>", ai_insights, "</div>")
        })

        summary_html <- paste0(
          summary_html,
          "<br>",
          "<div style='background: #e7f3ff; padding: 15px; border-radius: 8px; border-left: 4px solid #0066cc;'>",
          "<h6>🤖 ", get_lang_text("ai_analysis_extended.deep_insights"), "</h6>",
          "<div style='font-size: 14px;'>", ai_insights_html, "</div>",
          "</div>"
        )
      }

      summary_html <- paste0(summary_html, "</div>")

      values$ai_analysis_summary <- summary_html

      # 保存原始數據供CSV匯出使用
      values$ai_summary <- list(
        total_customers = nrow(values$segmented_data),
        segment_stats = segment_stats,
        metric = metric,
        analysis_time = Sys.time(),
        ai_insights = ai_insights
      )

      values$ai_summary_cache[[metric]] <- summary_html

      # 返回 HTML 摘要供 AI 管理器使用
      return(summary_html)
    }
    
    # 下載分析資料
    output$download_analysis_data <- downloadHandler(
      filename = function() {
        paste0("dna_analysis_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        data <- get_active_dna_data()
        req(data)

        export_cols <- c(
          "customer_id", "r_value", "f_value", "m_value", "times",
          "ipt_mean", "cai_value", "pcv", "cri", "be2", "nes_status", "clv"
        )
        available_cols <- intersect(export_cols, names(data))
        export_data <- data[, available_cols, drop = FALSE]

        # 欄位名稱中文對照
        chinese_names <- c(
          "customer_id" = "客戶ID",
          "r_value" = "最近購買日",
          "f_value" = "購買頻率",
          "m_value" = "購買金額",
          "times" = "購買次數",
          "ipt_mean" = "購買週期",
          "cai_value" = "顧客活躍度",
          "pcv" = "過去價值",
          "cri" = "顧客穩定度",
          "be2" = "顧客交易穩定度(be2)",
          "nes_status" = "顧客狀態",
          "clv" = "終身價值"
        )

        # 重命名欄位為中文
        names(export_data) <- chinese_names[names(export_data)]
        write.csv(export_data, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )
    
    # 下載客群分組資料
    output$download_segments_csv <- downloadHandler(
      filename = function() {
        paste0("customer_segments_", values$current_metric, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        req(values$segmented_data)
        export_data <- prepareExportData()
        write.csv(export_data, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )
    
    # 下載詳細資料
    output$download_detail_csv <- downloadHandler(
      filename = function() {
        paste0("segment_details_", values$current_metric, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        req(values$segmented_data)
        # MODIFIED 2024-12-28: 選擇需要的欄位並改為中文名稱
        detail_data <- values$segmented_data

        # 選擇需要匯出的欄位（移除重複和不需要的欄位）
        export_cols <- c(
          "customer_id", "r_value", "f_value", "m_value", "times", "ipt_mean",
          "cai_value", "pcv", "cri", "be2", "nes_status", "segment",
          "segment_m", "segment_r", "segment_f", "segment_ipt", "segment_pcv",
          "segment_pcv_quintile", "segment_cri", "segment_nes"
        )
        available_cols <- intersect(export_cols, names(detail_data))
        detail_data <- detail_data[, available_cols, drop = FALSE]

        # 欄位名稱中文對照
        chinese_names <- c(
          "customer_id" = "客戶ID",
          "r_value" = "最近購買日",
          "f_value" = "購買頻率",
          "m_value" = "購買金額",
          "times" = "購買次數",
          "ipt_mean" = "購買週期",
          "cai_value" = "顧客活躍度",
          "pcv" = "過去價值",
          "cri" = "顧客穩定度",
          "be2" = "顧客交易穩定度(be2)",
          "nes_status" = "顧客狀態",
          "segment" = "當前分群",
          "segment_m" = "購買金額分群",
          "segment_r" = "最近購買分群",
          "segment_f" = "購買頻率分群",
          "segment_ipt" = "購買週期分群",
          "segment_pcv" = "過去價值分群(80/20)",
          "segment_pcv_quintile" = "過去價值分群(五等分)",
          "segment_cri" = "顧客穩定度分群",
          "segment_nes" = "顧客狀態分群"
        )

        # 重命名欄位為中文
        for(i in seq_along(names(detail_data))) {
          old_name <- names(detail_data)[i]
          if(old_name %in% names(chinese_names)) {
            names(detail_data)[i] <- chinese_names[old_name]
          }
        }

        write.csv(detail_data, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )
    
    # 下載客戶詳細名單（新增）
    output$download_customer_details <- downloadHandler(
      filename = function() {
        metric_name <- if(!is.null(values$current_metric)) values$current_metric else "all"
        paste0("customer_details_", metric_name, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        req(values$temp_chinese_table)
        
        # 準備與顯示相同的欄位
        display_data <- values$temp_chinese_table
        metric <- values$current_metric
        
        # 根據當前指標選擇對應的分群欄位
        segment_col_mapping <- list(
          "R" = "R分群",
          "F" = "F分群",
          "M" = "M分群",
          "CAI" = "活躍度分群",
          "PCV" = "價值分群",
          "CRI" = "穩定度分群",
          "NES" = "狀態分群"
        )
        
        # 找到對應的分群欄位
        segment_col <- segment_col_mapping[[metric]]
        segment_col_text <- get_lang_text("table.columns.segment", "客群分組")
        if(is.null(segment_col) || !segment_col %in% names(display_data)) {
          segment_col <- segment_col_text
        }

        # 更新客群分組欄位名稱
        if(segment_col != segment_col_text && segment_col %in% names(display_data)) {
          display_data[[segment_col_text]] <- display_data[[segment_col]]
        }

        # 選擇要顯示的欄位（與畫面上顯示的相同）
        customer_id_text <- get_lang_text("table.columns.customer_id", "客戶ID")
        display_cols <- c(customer_id_text, segment_col_text)
        
        # 根據不同指標增加對應的欄位
        recency_days_text <- get_lang_text("table.columns.recency_days", "最近購買(天)")
        frequency_count_text <- get_lang_text("table.columns.frequency_count", "購買頻率(次)")
        purchase_count_text <- get_lang_text("table.columns.purchase_count", "購買次數")
        avg_amount_text <- get_lang_text("table.columns.avg_amount", "平均金額($)")
        activity_index_text <- get_lang_text("table.columns.activity_index", "活躍度指數")
        past_value_text <- get_lang_text("table.columns.past_value", "過去價值($)")
        total_spent_text <- get_lang_text("table.columns.total_spent", "總消費($)")
        stability_text <- get_lang_text("table.columns.stability", "顧客穩定度")
        engagement_score_text <- get_lang_text("table.columns.engagement_score", "參與度分數")
        customer_status_text <- get_lang_text("table.columns.customer_status", "客戶狀態")

        if(metric == "R") {
          if(recency_days_text %in% names(display_data)) display_cols <- c(display_cols, recency_days_text)
        } else if(metric == "F") {
          if(frequency_count_text %in% names(display_data)) {
            display_cols <- c(display_cols, frequency_count_text)
          } else if(purchase_count_text %in% names(display_data)) {
            display_cols <- c(display_cols, purchase_count_text)
          }
        } else if(metric == "M") {
          if(avg_amount_text %in% names(display_data)) display_cols <- c(display_cols, avg_amount_text)
        } else if(metric == "IPT") {
          purchase_cycle_text <- get_lang_text("table.columns.purchase_cycle", "購買週期(天)")
          if(purchase_cycle_text %in% names(display_data)) display_cols <- c(display_cols, purchase_cycle_text)
        } else if(metric == "CAI") {
          if(activity_index_text %in% names(display_data)) display_cols <- c(display_cols, activity_index_text)
        } else if(metric == "PCV") {
          if(past_value_text %in% names(display_data)) {
            display_cols <- c(display_cols, past_value_text)
          } else if(total_spent_text %in% names(display_data)) {
            display_cols <- c(display_cols, total_spent_text)
          }
        } else if(metric == "CRI") {
          if(stability_text %in% names(display_data)) {
            display_cols <- c(display_cols, stability_text)
          } else if(engagement_score_text %in% names(display_data)) {
            display_cols <- c(display_cols, engagement_score_text)
          }
        } else if(metric == "NES") {
          if(customer_status_text %in% names(display_data)) display_cols <- c(display_cols, customer_status_text)
        }
        
        # 確保欄位存在
        display_cols <- intersect(display_cols, names(display_data))
        
        # 建立匯出資料框
        if(length(display_cols) > 0) {
          export_data <- display_data[, display_cols, drop = FALSE]
        } else {
          export_data <- display_data
        }
        
        # 按分組排序
        segment_col_text <- get_lang_text("table.columns.segment", "客群分組")
        if(segment_col_text %in% names(export_data)) {
          export_data <- export_data[order(export_data[[segment_col_text]]), ]
        }
        
        write.csv(export_data, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )
    
    # 更新指標說明（整合hint系統）
    updateMetricDescription <- function(metric) {
      req(values$dna_results)
      data <- values$dna_results$data_by_customer
      
      # 載入hints
      hints_df <- load_hints(language = current_language(), app_name = "vitalsigns")

      # 根據指標取得對應的hint var_id
      hint_var_id <- switch(metric,
        "M" = "monetary_stat",
        "R" = "recency_stat",
        "F" = "frequency_stat",
        "IPT" = "ipt_stat",
        "CAI" = "cai_stat",
        "PCV" = "pcv_stat",
        "CRI" = "cri_stat",
        "NES" = "customer_segmentation"
      )
      
      # 取得hint描述
      hint_text <- if(!is.null(hints_df) && hint_var_id %in% hints_df$var_id) {
        hints_df[hints_df$var_id == hint_var_id, "description"][1]
      } else {
        ""
      }
      
      description <- switch(metric,
        "M" = {
          m_values <- data$m_value[!is.na(data$m_value)]
          m_median <- median(m_values)
          m_mean <- mean(m_values)
          m_var <- var(m_values)
          m_q25 <- quantile(m_values, 0.25)
          m_q75 <- quantile(m_values, 0.75)

          # 區分高中低 (80/20法則)
          m_q20 <- quantile(m_values, 0.2)
          m_q80 <- quantile(m_values, 0.8)
          high_count <- sum(m_values >= m_q80)
          low_count <- sum(m_values <= m_q20)
          mid_count <- length(m_values) - high_count - low_count

          paste0(
            "💰 <b>", get_lang_text("fallback_analysis.monetary.title", "購買金額分析"), "</b>",
            if(hint_text != "") {
              paste0(" <i class='fas fa-info-circle' style='font-size: 12px; color: #17a2b8;' ",
                     "data-toggle='tooltip' data-placement='top' title='", hint_text, "'></i>")
            } else "",
            "<br>",
            "• <b>", get_lang_text("fallback_analysis.monetary.definition", "定義：平均單次購買金額"), "</b><br>",
            "• <b>", get_lang_text("statistics.mean", "平均數"), "：</b>$", round(m_mean, 2), "<br>",
            "• <b>", get_lang_text("statistics.median", "中位數"), "：</b>$", round(m_median, 2), "<br>",
            "• <b>", get_lang_text("statistics.quartiles", "25%/75%分位數"), "：</b>$", round(m_q25, 2), " / $", round(m_q75, 2), "<br>",
            "• <b>", get_lang_text("statistics.variance", "變異數"), "：</b>", round(m_var, 2), "<br>",
            "• <b>", get_lang_text("fallback_analysis.monetary.overall_profile", "整體輪廓："), "</b>",
            if(m_var > (m_mean^2 * 0.5)) get_lang_text("fallback_analysis.monetary.profile_scattered", "購買金額離散程度大") else get_lang_text("fallback_analysis.monetary.profile_concentrated", "購買金額相對集中"),
            "<br><br>",
            "📊 <b>", get_lang_text("fallback_analysis.monetary.grouping_80_20", "80/20分組："), "</b><br>",
            "• ", get_lang_text("fallback_analysis.monetary.high_value", "高價值"), "(≮$", round(m_q80, 2), ")：", high_count, get_lang_text("fallback_analysis.monetary.unit_people", "人"), "<br>",
            "• ", get_lang_text("fallback_analysis.monetary.mid_value", "中價值"), "：", mid_count, get_lang_text("fallback_analysis.monetary.unit_people", "人"), "<br>",
            "• ", get_lang_text("fallback_analysis.monetary.low_value", "低價值"), "(≤$", round(m_q20, 2), ")：", low_count, get_lang_text("fallback_analysis.monetary.unit_people", "人")
          )
        },
        "R" = {
          r_values <- data$r_value[!is.na(data$r_value)]
          r_median <- median(r_values)
          r_mean <- mean(r_values)
          r_var <- var(r_values)
          r_q25 <- quantile(r_values, 0.25)
          r_q75 <- quantile(r_values, 0.75)

          # 區分高中低 (80/20法則) - 注意：R值越低越好
          r_q20 <- quantile(r_values, 0.2)
          r_q80 <- quantile(r_values, 0.8)
          active_count <- sum(r_values <= r_q20)  # 最近購買(活躍)
          inactive_count <- sum(r_values >= r_q80)  # 久未購買(不活躍)
          mid_count <- length(r_values) - active_count - inactive_count

          paste0(
            "🕒 <b>", get_lang_text("fallback_analysis.recency.title", "最近來店時間分析"), "</b>",
            if(hint_text != "") {
              paste0(" <i class='fas fa-info-circle' style='font-size: 12px; color: #17a2b8;' ",
                     "data-toggle='tooltip' data-placement='top' title='", hint_text, "'></i>")
            } else "",
            "<br>",
            "• <b>", get_lang_text("fallback_analysis.recency.definition", "定義：距離最近一次購買的天數"), "</b><br>",
            "• <b>", get_lang_text("statistics.mean", "平均數"), "：</b>", round(r_mean, 0), " ", get_lang_text("fallback_analysis.recency.unit_days", "天"), "<br>",
            "• <b>", get_lang_text("statistics.median", "中位數"), "：</b>", round(r_median, 0), " ", get_lang_text("fallback_analysis.recency.unit_days", "天"), "<br>",
            "• <b>", get_lang_text("statistics.quartiles", "25%/75%分位數"), "：</b>", round(r_q25, 0), " / ", round(r_q75, 0), " ", get_lang_text("fallback_analysis.recency.unit_days", "天"), "<br>",
            "• <b>", get_lang_text("statistics.variance", "變異數"), "：</b>", round(r_var, 0), "<br>",
            "• <b>", get_lang_text("fallback_analysis.recency.overall_profile", "整體輪廓："), "</b>",
            if(r_var > (r_mean^2 * 0.5)) get_lang_text("fallback_analysis.recency.profile_scattered", "來店時間離散程度大") else get_lang_text("fallback_analysis.recency.profile_consistent", "來店時間相對一致"),
            "<br><br>",
            "📊 <b>", get_lang_text("fallback_analysis.recency.grouping_80_20", "80/20分組："), "</b><br>",
            "• ", get_lang_text("fallback_analysis.recency.highly_active", "高度活躍"), "(≤", round(r_q20, 0), get_lang_text("fallback_analysis.recency.unit_days", "天"), ")：", active_count, get_lang_text("fallback_analysis.recency.unit_people", "人"), "<br>",
            "• ", get_lang_text("fallback_analysis.recency.moderately_active", "中度活躍"), "：", mid_count, get_lang_text("fallback_analysis.recency.unit_people", "人"), "<br>",
            "• ", get_lang_text("fallback_analysis.recency.inactive", "不活躍"), "(≮", round(r_q80, 0), get_lang_text("fallback_analysis.recency.unit_days", "天"), ")：", inactive_count, get_lang_text("fallback_analysis.recency.unit_people", "人")
          )
        },
        "F" = {
          f_values <- data$f_value[!is.na(data$f_value)]
          f_median <- median(f_values)
          f_mean <- mean(f_values)
          f_var <- var(f_values)
          f_q25 <- quantile(f_values, 0.25)
          f_q75 <- quantile(f_values, 0.75)

          # 區分高中低 (80/20法則)
          f_q20 <- quantile(f_values, 0.2)
          f_q80 <- quantile(f_values, 0.8)
          high_freq <- sum(f_values >= f_q80)
          low_freq <- sum(f_values <= f_q20)
          mid_freq <- length(f_values) - high_freq - low_freq

          paste0(
            "🔁 <b>", get_lang_text("fallback_analysis.frequency.title", "購買頻率分析"), "</b>",
            if(hint_text != "") {
              paste0(" <i class='fas fa-info-circle' style='font-size: 12px; color: #17a2b8;' ",
                     "data-toggle='tooltip' data-placement='top' title='", hint_text, "'></i>")
            } else "",
            "<br>",
            "• <b>", get_lang_text("fallback_analysis.frequency.definition", "定義：觀察期間內的總購買次數"), "</b><br>",
            "• <b>", get_lang_text("statistics.mean", "平均數"), "：</b>", round(f_mean, 1), " ", get_lang_text("fallback_analysis.frequency.unit_times", "次"), "<br>",
            "• <b>", get_lang_text("statistics.median", "中位數"), "：</b>", round(f_median, 1), " ", get_lang_text("fallback_analysis.frequency.unit_times", "次"), "<br>",
            "• <b>", get_lang_text("statistics.quartiles", "25%/75%分位數"), "：</b>", round(f_q25, 1), " / ", round(f_q75, 1), " ", get_lang_text("fallback_analysis.frequency.unit_times", "次"), "<br>",
            "• <b>", get_lang_text("statistics.variance", "變異數"), "：</b>", round(f_var, 2), "<br>",
            "• <b>", get_lang_text("fallback_analysis.frequency.overall_profile", "整體輪廓："), "</b>",
            if(f_var > (f_mean^2 * 0.5)) get_lang_text("fallback_analysis.frequency.profile_needs_segmentation", "購買頻率差異大需分群管理") else get_lang_text("fallback_analysis.frequency.profile_consistent", "購買頻率較為一致"),
            "<br><br>",
            "📊 <b>", get_lang_text("fallback_analysis.frequency.grouping_80_20", "80/20分組："), "</b><br>",
            "• ", get_lang_text("fallback_analysis.frequency.high_freq_customers", "高頻客戶"), "(≮", round(f_q80, 1), get_lang_text("fallback_analysis.frequency.unit_times", "次"), ")：", high_freq, get_lang_text("fallback_analysis.frequency.unit_people", "人"), "<br>",
            "• ", get_lang_text("fallback_analysis.frequency.mid_freq_customers", "中頻客戶"), "：", mid_freq, get_lang_text("fallback_analysis.frequency.unit_people", "人"), "<br>",
            "• ", get_lang_text("fallback_analysis.frequency.low_freq_customers", "低頻客戶"), "(≤", round(f_q20, 1), get_lang_text("fallback_analysis.frequency.unit_times", "次"), ")：", low_freq, get_lang_text("fallback_analysis.frequency.unit_people", "人")
          )
        },
        "IPT" = {
          if("ipt_mean" %in% names(data)) {
            ipt_values <- data$ipt_mean[!is.na(data$ipt_mean)]
            ipt_median <- median(ipt_values)
            ipt_mean <- mean(ipt_values)
            ipt_var <- var(ipt_values)
            ipt_q25 <- quantile(ipt_values, 0.25)
            ipt_q75 <- quantile(ipt_values, 0.75)

            # 區分短中長週期 (80/20法則)
            ipt_q20 <- quantile(ipt_values, 0.2)
            ipt_q80 <- quantile(ipt_values, 0.8)
            short_cycle <- sum(ipt_values <= ipt_q20)
            long_cycle <- sum(ipt_values >= ipt_q80)
            mid_cycle <- length(ipt_values) - short_cycle - long_cycle

            paste0(
              "⏱️ <b>", get_lang_text("fallback_analysis.ipt.title", "購買週期分析"), "</b>",
              if(hint_text != "") {
                paste0(" <i class='fas fa-info-circle' style='font-size: 12px; color: #17a2b8;' ",
                       "data-toggle='tooltip' data-placement='top' title='", hint_text, "'></i>")
              } else "",
              "<br>",
              "• <b>", get_lang_text("fallback_analysis.ipt.definition", "定義：平均購買間隔天數"), "</b><br>",
              "• <b>", get_lang_text("statistics.mean", "平均數"), "：</b>", round(ipt_mean, 1), " ", get_lang_text("fallback_analysis.ipt.unit_days", "天"), "<br>",
              "• <b>", get_lang_text("statistics.median", "中位數"), "：</b>", round(ipt_median, 1), " ", get_lang_text("fallback_analysis.ipt.unit_days", "天"), "<br>",
              "• <b>", get_lang_text("statistics.quartiles", "25%/75%分位數"), "：</b>", round(ipt_q25, 1), " / ", round(ipt_q75, 1), " ", get_lang_text("fallback_analysis.ipt.unit_days", "天"), "<br>",
              "• <b>", get_lang_text("statistics.variance", "變異數"), "：</b>", round(ipt_var, 2), "<br>",
              "• <b>", get_lang_text("fallback_analysis.ipt.overall_profile", "整體輪廓："), "</b>",
              if(ipt_var > (ipt_mean^2 * 0.5)) get_lang_text("fallback_analysis.ipt.profile_needs_segmentation", "購買週期差異大需分群管理") else get_lang_text("fallback_analysis.ipt.profile_consistent", "購買週期較為一致"),
              "<br><br>",
              "📊 <b>", get_lang_text("fallback_analysis.ipt.grouping_cycle", "週期分組："), "</b><br>",
              "• ", get_lang_text("fallback_analysis.ipt.short_cycle_customers", "短週期客戶"), "(≤", round(ipt_q20, 1), get_lang_text("fallback_analysis.ipt.unit_days", "天"), ")：", short_cycle, get_lang_text("fallback_analysis.ipt.unit_people", "人"), "<br>",
              "• ", get_lang_text("fallback_analysis.ipt.mid_cycle_customers", "中週期客戶"), "：", mid_cycle, get_lang_text("fallback_analysis.ipt.unit_people", "人"), "<br>",
              "• ", get_lang_text("fallback_analysis.ipt.long_cycle_customers", "長週期客戶"), "(≮", round(ipt_q80, 1), get_lang_text("fallback_analysis.ipt.unit_days", "天"), ")：", long_cycle, get_lang_text("fallback_analysis.ipt.unit_people", "人")
            )
          } else {
            paste0("⏱️ <b>", get_lang_text("fallback_analysis.ipt.title", "購買週期分析"), "</b><br>• ", get_lang_text("fallback_analysis.ipt.no_data", "無購買週期數據"))
          }
        },
        "CAI" = {
          if("cai_value" %in% names(data)) {
            cai_values <- data$cai_value[!is.na(data$cai_value)]
            cai_mean <- mean(cai_values)
            cai_median <- median(cai_values)
            cai_var <- var(cai_values)
            cai_q25 <- quantile(cai_values, 0.25)
            cai_q75 <- quantile(cai_values, 0.75)
            
            cai_active <- sum(cai_values > 0, na.rm = TRUE)
            cai_stable <- sum(abs(cai_values) <= 0.1, na.rm = TRUE)
            cai_declining <- sum(cai_values < -0.1, na.rm = TRUE)
            total <- length(cai_values)
            
            # 80/20分組
            cai_q20 <- quantile(cai_values, 0.2)
            cai_q80 <- quantile(cai_values, 0.8)
            high_active <- sum(cai_values >= cai_q80, na.rm = TRUE)
            low_active <- sum(cai_values <= cai_q20, na.rm = TRUE)
            
            paste0(
              "📈 <b>", get_lang_text("metric_analysis.cai_analysis_title", "顧客活躍度分析"), "</b>",
              if(hint_text != "") {
                paste0(" <i class='fas fa-info-circle' style='font-size: 12px; color: #17a2b8;' ",
                       "data-toggle='tooltip' data-placement='top' title='", hint_text, "'></i>")
              } else "",
              "<br>",
              "• <b>", get_lang_text("metric_analysis.cai_definition_label", "定義："), "</b>",
              get_lang_text("metric_analysis.cai_definition_text", "購買行為趨勢指標 (>0漸趨活躍，<0漸趨靜止)"), "<br>",
              "• <b>", get_lang_text("metric_analysis.cai_management_meaning_label", "管理意涵："), "</b>",
              get_lang_text("metric_analysis.cai_management_meaning_text", "預測客戶未來行為，及早識別流失風險"), "<br><br>",
              "📊 <b>", get_lang_text("metric_analysis.cai_stat_summary_label", "統計摘要："), "</b><br>",
              "• <b>", get_lang_text("metric_analysis.cai_mean_label", "平均數："), "</b>", round(cai_mean, 3), "<br>",
              "• <b>", get_lang_text("metric_analysis.cai_median_label", "中位數："), "</b>", round(cai_median, 3), "<br>",
              "• <b>", get_lang_text("metric_analysis.cai_quartiles_label", "25%/75%分位數："), "</b>",
              round(cai_q25, 3), " / ", round(cai_q75, 3), "<br>",
              "• <b>", get_lang_text("metric_analysis.cai_variance_label", "變異數："), "</b>", round(cai_var, 5), "<br>",
              "• <b>", get_lang_text("metric_analysis.cai_overall_profile_label", "整體輪廓："), "</b>",
              if(cai_var > 0.01) {
                get_lang_text("metric_analysis.cai_profile_high_variance", "活躍度差異大需分群管理")
              } else {
                get_lang_text("metric_analysis.cai_profile_low_variance", "活躍度較為一致")
              },
              "<br><br>",
              "📊 <b>", get_lang_text("metric_analysis.cai_pareto_label", "80/20分組："), "</b><br>",
              "• ", get_lang_text("metric_analysis.cai_high_activity", "高活躍"), "(≥", round(cai_q80, 3), ")：",
              high_active, " ", get_lang_text("statistics.units.people", "人"), "<br>",
              "• ", get_lang_text("metric_analysis.cai_mid_activity", "中活躍"), "：",
              total - high_active - low_active, " ", get_lang_text("statistics.units.people", "人"), "<br>",
              "• ", get_lang_text("metric_analysis.cai_low_activity", "低活躍"), "(≤", round(cai_q20, 3), ")：",
              low_active, " ", get_lang_text("statistics.units.people", "人"), "<br><br>",
              "💡 <b>", get_lang_text("metric_analysis.cai_status_distribution_label", "狀態分布："), "</b><br>",
              "• <b>", get_lang_text("metric_analysis.cai_trending_active_label", "漸趨活躍(>0)："), "</b>",
              cai_active, " ", get_lang_text("statistics.units.people", "人"), " (",
              round(cai_active/total*100, 1), "%)<br>",
              "• <b>", get_lang_text("metric_analysis.cai_stable_label", "穩定(≈0)："), "</b>",
              cai_stable, " ", get_lang_text("statistics.units.people", "人"), " (",
              round(cai_stable/total*100, 1), "%)<br>",
              "• <b>", get_lang_text("metric_analysis.cai_trending_static_label", "漸趨靜止(<0)："), "</b>",
              cai_declining, " ", get_lang_text("statistics.units.people", "人"), " (",
              round(cai_declining/total*100, 1), "%)<br><br>",
              "🎯 <b>", get_lang_text("metric_analysis.cai_marketing_suggestions_label", "行銷建議："), "</b><br>",
              "• ", get_lang_text("metric_analysis.cai_active_customers_label", "活躍客戶："),
              get_lang_text("metric_analysis.cai_active_suggestion", "把握成長動能，推薦新品"), "<br>",
              "• ", get_lang_text("metric_analysis.cai_stable_customers_label", "穩定客戶："),
              get_lang_text("metric_analysis.cai_stable_suggestion", "維持服務水準，定期關懷"), "<br>",
              "• ", get_lang_text("metric_analysis.cai_static_customers_label", "靜止客戶："),
              get_lang_text("metric_analysis.cai_static_suggestion", "緊急挽回措施，特殊優惠")
            )
          } else {
            paste0("📈 <b>", get_lang_text("metric_analysis.cai_analysis_title", "顧客活躍度"), "</b><br>• ",
                   get_lang_text("metric_analysis.cai_no_data", "資料不足"))
          }
        },
        "PCV" = {
          if("pcv" %in% names(data) || "total_spent" %in% names(data)) {
            pcv_col <- if("pcv" %in% names(data)) "pcv" else "total_spent"
            pcv_values <- data[[pcv_col]][!is.na(data[[pcv_col]])]
            pcv_mean <- mean(pcv_values)
            pcv_median <- median(pcv_values)
            pcv_q25 <- quantile(pcv_values, 0.25)
            pcv_q75 <- quantile(pcv_values, 0.75)
            pcv_var <- var(pcv_values)

            # 80/20分組
            pcv_q20 <- quantile(pcv_values, 0.2)
            pcv_q80 <- quantile(pcv_values, 0.8)
            high_value <- sum(pcv_values >= pcv_q80)
            low_value <- sum(pcv_values <= pcv_q20)
            mid_value <- length(pcv_values) - high_value - low_value

            paste0(
              "💎 <b>", get_lang_text("modules.vitalsigns_dna.metric_analysis.pcv_title", "過去價值分析"), "</b>",
              if(hint_text != "") {
                paste0(" <i class='fas fa-info-circle' style='font-size: 12px; color: #17a2b8;' ",
                       "data-toggle='tooltip' data-placement='top' title='", hint_text, "'></i>")
              } else "",
              "<br>",
              "• <b>", get_lang_text("modules.vitalsigns_dna.statistics.definition", "定義："), "</b>",
              get_lang_text("modules.vitalsigns_dna.metric_analysis.pcv_definition", "歷史累積消費總額"), "<br>",
              "• <b>", get_lang_text("modules.vitalsigns_dna.statistics.mean", "平均數："), "</b>$", round(pcv_mean, 2), "<br>",
              "• <b>", get_lang_text("modules.vitalsigns_dna.statistics.median", "中位數："), "</b>$", round(pcv_median, 2), "<br>",
              "• <b>", get_lang_text("modules.vitalsigns_dna.statistics.quartiles", "25%/75%分位數："), "</b>$", round(pcv_q25, 2), " / $", round(pcv_q75, 2), "<br>",
              "• <b>", get_lang_text("modules.vitalsigns_dna.statistics.variance", "變異數："), "</b>", round(pcv_var, 2), "<br><br>",
              "📊 <b>", get_lang_text("modules.vitalsigns_dna.metric_analysis.pareto_segmentation", "80/20分組："), "</b><br>",
              "• ", get_lang_text("modules.vitalsigns_dna.metric_analysis.high_value", "高價值"),
              "(≥$", round(pcv_q80, 2), ")：", high_value, get_lang_text("modules.vitalsigns_dna.metric_analysis.people_unit", "人"), "<br>",
              "• ", get_lang_text("modules.vitalsigns_dna.metric_analysis.mid_value", "中價值"),
              "：", mid_value, get_lang_text("modules.vitalsigns_dna.metric_analysis.people_unit", "人"), "<br>",
              "• ", get_lang_text("modules.vitalsigns_dna.metric_analysis.low_value", "低價值"),
              "(≤$", round(pcv_q20, 2), ")：", low_value, get_lang_text("modules.vitalsigns_dna.metric_analysis.people_unit", "人"), "<br><br>",
              "🎯 <b>", get_lang_text("modules.vitalsigns_dna.metric_analysis.marketing_suggestions", "行銷建議："), "</b><br>",
              "• ", get_lang_text("modules.vitalsigns_dna.metric_analysis.high_value", "高價值"), "：",
              get_lang_text("modules.vitalsigns_dna.marketing.pcv_high_value", "專屬VIP服務"), "<br>",
              "• ", get_lang_text("modules.vitalsigns_dna.metric_analysis.mid_value", "中價值"), "：",
              get_lang_text("modules.vitalsigns_dna.marketing.pcv_mid_value", "提升計劃"), "<br>",
              "• ", get_lang_text("modules.vitalsigns_dna.metric_analysis.low_value", "低價值"), "：",
              get_lang_text("modules.vitalsigns_dna.marketing.pcv_low_value", "激活策略")
            )
          } else {
            paste0("💎 <b>", get_lang_text("modules.vitalsigns_dna.metric_analysis.pcv_title", "過去價值分析"), "</b><br>• ",
                   get_lang_text("metric_analysis.insufficient_data", "資料不足"))
          }
        },
        "CRI" = {
          if("cri" %in% names(data)) {
            cri_values <- data$cri[!is.na(data$cri)]
            cri_mean <- mean(cri_values)
            cri_median <- median(cri_values)
            cri_var <- var(cri_values)
            cri_q25 <- quantile(cri_values, 0.25)
            cri_q75 <- quantile(cri_values, 0.75)

            # 80/20分組
            cri_q20 <- quantile(cri_values, 0.2)
            cri_q80 <- quantile(cri_values, 0.8)
            high_value <- sum(cri_values >= cri_q80, na.rm = TRUE)
            low_value <- sum(cri_values <= cri_q20, na.rm = TRUE)
            total <- length(cri_values)

            paste0(
              "🎯 <b>", get_lang_text("modules.vitalsigns_dna.metric_analysis.cri_title", "顧客穩定度指標分析"), "</b>",
              if(hint_text != "") {
                paste0(" <i class='fas fa-info-circle' style='font-size: 12px; color: #17a2b8;' ",
                       "data-toggle='tooltip' data-placement='top' title='", hint_text, "'></i>")
              } else "",
              "<br>",
              "• <b>", get_lang_text("modules.vitalsigns_dna.statistics.definition", "定義："), "</b>",
              get_lang_text("modules.vitalsigns_dna.metric_analysis.cri_definition", "顧客交易穩定度綜合評分"), "<br>",
              "• <b>", get_lang_text("modules.vitalsigns_dna.statistics.calculation_method", "計算方式："), "</b>",
              get_lang_text("modules.vitalsigns_dna.metric_analysis.cri_calculation", "RFM加權分數 (0.3×R + 0.3×F + 0.4×M) 標準化至0-100分"), "<br>",
              "• <b>", get_lang_text("modules.vitalsigns_dna.statistics.management_implication", "管理意涵："), "</b>",
              get_lang_text("modules.vitalsigns_dna.metric_analysis.cri_meaning", "識別穩定貢獻客群，採用80/20法則優化資源配置"), "<br><br>",
              "📊 <b>", get_lang_text("modules.vitalsigns_dna.statistics.summary", "統計摘要："), "</b><br>",
              "• <b>", get_lang_text("modules.vitalsigns_dna.statistics.average_score", "平均分數："), "</b>", round(cri_mean * 100, 1), " ",
              get_lang_text("modules.vitalsigns_dna.metric_analysis.score_unit", "分"), "<br>",
              "• <b>", get_lang_text("modules.vitalsigns_dna.statistics.median", "中位數："), "</b>", round(cri_median * 100, 1), " ",
              get_lang_text("modules.vitalsigns_dna.metric_analysis.score_unit", "分"), "<br>",
              "• <b>", get_lang_text("modules.vitalsigns_dna.statistics.quartiles", "25%/75%分位數："), "</b>", round(cri_q25 * 100, 1), " / ", round(cri_q75 * 100, 1), " ",
              get_lang_text("modules.vitalsigns_dna.metric_analysis.score_unit", "分"), "<br>",
              "• <b>", get_lang_text("modules.vitalsigns_dna.statistics.variance", "變異數："), "</b>", round(cri_var * 10000, 2), "<br>",
              "• <b>", get_lang_text("modules.vitalsigns_dna.statistics.overall_profile", "整體輪廓："), "</b>",
              if(cri_var > 0.01) get_lang_text("modules.vitalsigns_dna.metric_analysis.cri_dispersed", "穩定度差異大，需差異化服務策略")
              else get_lang_text("modules.vitalsigns_dna.metric_analysis.cri_concentrated", "穩定度較為平均"),
              "<br><br>",
              "📊 <b>", get_lang_text("modules.vitalsigns_dna.metric_analysis.pareto_rule_grouping", "80/20法則分組："), "</b><br>",
              "• ", get_lang_text("modules.vitalsigns_dna.metric_analysis.high_stability", "高穩定"),
              "(Top 20%, ≥", round(cri_q80 * 100, 1), get_lang_text("modules.vitalsigns_dna.metric_analysis.score_unit", "分"), ")：", high_value,
              get_lang_text("modules.vitalsigns_dna.metric_analysis.people_unit", "人"), "<br>",
              "• ", get_lang_text("modules.vitalsigns_dna.metric_analysis.mid_stability", "中穩定"),
              "(Middle 60%)：", total - high_value - low_value, get_lang_text("modules.vitalsigns_dna.metric_analysis.people_unit", "人"), "<br>",
              "• ", get_lang_text("modules.vitalsigns_dna.metric_analysis.low_stability", "低穩定"),
              "(Bottom 20%, ≤", round(cri_q20 * 100, 1), get_lang_text("modules.vitalsigns_dna.metric_analysis.score_unit", "分"), ")：", low_value,
              get_lang_text("modules.vitalsigns_dna.metric_analysis.people_unit", "人"), "<br><br>",
              "💡 <b>", get_lang_text("modules.vitalsigns_dna.metric_analysis.stability_interpretation", "穩定度層級解讀："), "</b><br>",
              "• Top 20%：", get_lang_text("modules.vitalsigns_dna.marketing.cri_top_20_desc", "VIP穩定客群，提供專屬服務維持忠誠"), "<br>",
              "• Middle 60%：", get_lang_text("modules.vitalsigns_dna.marketing.cri_middle_60_desc", "一般穩定客群，定期關懷提升價值"), "<br>",
              "• Bottom 20%：", get_lang_text("modules.vitalsigns_dna.marketing.cri_bottom_20_desc", "不穩定風險客群，需積極挽留策略")
            )
          } else {
            paste0("🎯 <b>", get_lang_text("modules.vitalsigns_dna.metric_analysis.cri_title", "顧客穩定度指標分析"), "</b><br>• ",
                   get_lang_text("metric_analysis.insufficient_data", "資料不足"))
          }
        },
        "NES" = {
          nes_table <- table(data$nes_status)
          paste0(
            "👥 <b>", get_lang_text("modules.vitalsigns_dna.metric_analysis.nes_title", "顧客狀態分析"), "</b>",
            if(hint_text != "") {
              paste0(" <i class='fas fa-info-circle' style='font-size: 12px; color: #17a2b8;' ",
                     "data-toggle='tooltip' data-placement='top' title='", hint_text, "'></i>")
            } else "",
            "<br>",
            "• ", get_lang_text("modules.vitalsigns_dna.metric_analysis.new_customer", "新客"), "(N)：", nes_table["N"] %||% 0, " ",
            get_lang_text("modules.vitalsigns_dna.metric_analysis.people_unit", "人"), "<br>",
            "• ", get_lang_text("modules.vitalsigns_dna.metric_analysis.main_customer", "主力"), "(E0)：", nes_table["E0"] %||% 0, " ",
            get_lang_text("modules.vitalsigns_dna.metric_analysis.people_unit", "人"), "<br>",
            "• ", get_lang_text("modules.vitalsigns_dna.metric_analysis.sleeping_customer", "沉睡"), "(S1-S3)：",
            sum(nes_table[c("S1", "S2", "S3")], na.rm = TRUE), " ",
            get_lang_text("modules.vitalsigns_dna.metric_analysis.people_unit", "人")
          )
        }
      )
      
      output$metric_ai_description <- renderUI({
        req(lang_texts())  # React to language changes
        HTML(description)
      })
    }
    
    # 統計概覽輸出
    output$statistics_summary <- renderUI({
      req(lang_texts())  # React to language changes
      req(values$current_metric, values$dna_results)
      data <- get_active_dna_data()
      if (is.null(data)) {
        showNotification(
          get_lang_text("messages.warning.no_dna_analysis", "⚠️ 請先執行 DNA 分析"),
          type = "warning", duration = 3)
        return()
      }
      metric <- values$current_metric
      
      # 根據選定的指標取得資料欄位
      metric_col <- switch(metric,
        "M" = "m_value",
        "R" = "r_value",
        "F" = "f_value",
        "CAI" = "cai_value",
        "PCV" = "pcv",
        "CRI" = "cri",
        "NES" = "nes_status"
      )
      
      if(!metric_col %in% names(data)) {
        return(HTML("<p class='text-muted'>該指標資料不存在</p>"))
      }
      
      if(metric != "NES") {
        values_vec <- data[[metric_col]]
        values_vec <- values_vec[!is.na(values_vec)]
        
        # 計算統計量
        q25 <- quantile(values_vec, 0.25)
        q50 <- quantile(values_vec, 0.50)
        q75 <- quantile(values_vec, 0.75)
        mean_val <- mean(values_vec)
        sd_val <- sd(values_vec)
        cv_val <- sd_val / mean_val  # 變異係數
        
        # 根據指標類型格式化數值
        format_value <- function(x, type = metric) {
          if(type %in% c("M", "PCV")) {
            paste0("$", format(round(x, 2), big.mark = ","))
          } else if(type == "R") {
            paste0(round(x, 0), " ", get_lang_text("statistics.units.days", "天"))
          } else if(type == "F") {
            paste0(round(x, 1), " ", get_lang_text("statistics.units.times", "次"))
          } else if(type %in% c("CAI", "CRI")) {
            paste0(round(x * 100, 1), " ", get_lang_text("statistics.units.points", "分"))
          } else {
            round(x, 2)
          }
        }
        
        # 生成統計摘要
        tagList(
          fluidRow(
            column(3,
              bs4InfoBox(
                title = get_lang_text("statistics.labels.mean", "平均數"),
                value = format_value(mean_val),
                icon = icon("calculator"),
                color = "info",
                width = 12
              )
            ),
            column(3,
              bs4InfoBox(
                title = get_lang_text("statistics.labels.median", "中位數"),
                value = format_value(q50),
                icon = icon("chart-line"),
                color = "primary",
                width = 12
              )
            ),
            column(3,
              bs4InfoBox(
                title = get_lang_text("statistics.labels.percentile_25_75", "25% / 75%"),
                value = HTML(paste0(
                  format_value(q25), "<br>",
                  format_value(q75)
                )),
                icon = icon("chart-area"),
                color = "warning",
                width = 12
              )
            ),
            column(3,
              bs4InfoBox(
                title = get_lang_text("statistics.labels.cv", "變異係數"),
                value = paste0(round(cv_val * 100, 1), "%"),
                icon = icon("percentage"),
                color = if(cv_val > 0.5) "danger" else "success",
                width = 12
              )
            )
          ),
          br(),
          div(
            class = "alert alert-info",
            h5(icon("info-circle"), " ", get_lang_text("statistics.interpretation.title", "統計解讀")),
            p(
              if(cv_val > 0.5) {
                paste0(
                  get_lang_text("statistics.interpretation.cv_label", "變異係數"), " ",
                  round(cv_val * 100, 1), "%，",
                  get_lang_text("statistics.interpretation.indicates", "表示"),
                  switch(metric,
                    "R" = get_lang_text("statistics.interpretation.high_cv.recency", "顧客來店時間差異很大，需要分群管理"),
                    "F" = get_lang_text("statistics.interpretation.high_cv.frequency", "購買頻率差異顯著，存在明顯的忠誠度分層"),
                    "M" = get_lang_text("statistics.interpretation.high_cv.monetary", "消費水平差異很大，需要差異化定價策略"),
                    get_lang_text("statistics.interpretation.high_cv.default", "顧客群體差異較大")
                  ))
              } else {
                paste0(
                  get_lang_text("statistics.interpretation.cv_label", "變異係數"), " ",
                  round(cv_val * 100, 1), "%，",
                  get_lang_text("statistics.interpretation.indicates", "表示"),
                  switch(metric,
                    "R" = get_lang_text("statistics.interpretation.low_cv.recency", "顧客來店時間相對穩定"),
                    "F" = get_lang_text("statistics.interpretation.low_cv.frequency", "購買頻率較為一致"),
                    "M" = get_lang_text("statistics.interpretation.low_cv.monetary", "消費水平相對集中"),
                    get_lang_text("statistics.interpretation.low_cv.default", "顧客群體較為同質")
                  ))
              }
            )
          )
        )
      } else {
        # NES 狀態的特殊處理
        nes_table <- table(data$nes_status)
        nes_pct <- prop.table(nes_table) * 100
        
        tagList(
          fluidRow(
            column(12,
              h5(get_lang_text("statistics.nes_status.title", "顧客狀態分布")),
              tags$table(
                class = "table table-striped",
                tags$thead(
                  tags$tr(
                    tags$th(get_lang_text("statistics.nes_status.status", "狀態")),
                    tags$th(get_lang_text("statistics.nes_status.count", "人數")),
                    tags$th(get_lang_text("statistics.nes_status.percentage", "比例"))
                  )
                ),
                tags$tbody(
                  lapply(names(nes_table), function(status) {
                    tags$tr(
                      tags$td(status),
                      tags$td(nes_table[status]),
                      tags$td(paste0(round(nes_pct[status], 1), "%"))
                    )
                  })
                )
              )
            )
          )
        )
      }
    })
    
    # 統計摘要（改進的文字說明）
    output$stat_summary <- renderUI({
      req(lang_texts())  # React to language changes
      req(values$current_metric, values$dna_results)
      data <- values$dna_results$data_by_customer
      metric <- values$current_metric
      
      insight <- switch(metric,
        "M" = {
          col_data <- data$m_value[!is.na(data$m_value)]
          q25 <- quantile(col_data, 0.25)
          q50 <- quantile(col_data, 0.5)
          q75 <- quantile(col_data, 0.75)
          mean_val <- mean(col_data)
          var_val <- var(col_data)
          
          # 80/20分組
          q20 <- quantile(col_data, 0.2)
          q80 <- quantile(col_data, 0.8)
          
          HTML(paste0(
            "• <b>", get_lang_text("statistics.labels.mean", "平均數"), "：</b>$", round(mean_val, 2), "<br>",
            "<span style='color: #666; font-size: 12px;'>　（", get_lang_text("statistics.explanations.mean_monetary", "顧客平均購買金額"), "）</span><br>",
            "• <b>", get_lang_text("statistics.labels.median", "中位數"), "：</b>$", round(q50, 2), "<br>",
            "<span style='color: #666; font-size: 12px;'>　（", get_lang_text("statistics.explanations.median_monetary", "一半顧客購買金額高於此值"), "）</span><br>",
            "• <b>", get_lang_text("statistics.labels.q25", "第25百分位數"), "：</b>$", round(q25, 2), "<br>",
            "<span style='color: #666; font-size: 12px;'>　（", get_lang_text("statistics.explanations.q25_monetary", "25%顧客購買金額低於此值"), "）</span><br>",
            "• <b>", get_lang_text("statistics.labels.q75", "第75百分位數"), "：</b>$", round(q75, 2), "<br>",
            "<span style='color: #666; font-size: 12px;'>　（", get_lang_text("statistics.explanations.q75_monetary", "75%顧客購買金額低於此值"), "）</span><br>",
            "• <b>", get_lang_text("statistics.labels.variance_degree", "變異程度"), "：</b>",
            if(var_val > (mean_val^2 * 0.5)) get_lang_text("statistics.explanations.variance_high_monetary", "顧客之間的購買金額差異很大") else get_lang_text("statistics.explanations.variance_low_monetary", "顧客之間的購買金額差異較小"),
            "<br><br>",
            "🎯 <b>", get_lang_text("statistics.labels.ai_marketing_suggestions", "AI行銷建議"), "：</b><br>",
            "• ", get_lang_text("segment_labels.monetary.high_value", "高價值"), get_lang_text("statistics.labels.customer", "客戶"), "(≥$", round(q80, 2), ")：", get_lang_text("statistics.marketing_suggestions.monetary.high_value", "VIP服務"), "<br>",
            "• ", get_lang_text("segment_labels.monetary.mid_value", "中價值"), get_lang_text("statistics.labels.customer", "客戶"), "：", get_lang_text("statistics.marketing_suggestions.monetary.mid_value", "升級推薦"), "<br>",
            "• ", get_lang_text("segment_labels.monetary.low_value", "低價值"), get_lang_text("statistics.labels.customer", "客戶"), "(≤$", round(q20, 2), ")：", get_lang_text("statistics.marketing_suggestions.monetary.low_value", "入門優惠")
          ))
        },
        "R" = {
          col_data <- data$r_value[!is.na(data$r_value)]
          q25 <- quantile(col_data, 0.25)
          q50 <- quantile(col_data, 0.5)
          q75 <- quantile(col_data, 0.75)
          mean_val <- mean(col_data)
          var_val <- var(col_data)
          
          # 80/20分組
          q20 <- quantile(col_data, 0.2)
          q80 <- quantile(col_data, 0.8)
          
          HTML(paste0(
            "• <b>", get_lang_text("statistics.labels.mean", "平均數"), "：</b>", round(mean_val, 0), " ", get_lang_text("statistics.units.days", "天"), "<br>",
            "<span style='color: #666; font-size: 12px;'>　（", get_lang_text("statistics.explanations.mean_recency", "顧客平均距離上次購買天數"), "）</span><br>",
            "• <b>", get_lang_text("statistics.labels.median", "中位數"), "：</b>", round(q50, 0), " ", get_lang_text("statistics.units.days", "天"), "<br>",
            "<span style='color: #666; font-size: 12px;'>　（", get_lang_text("statistics.explanations.median_recency", "一半顧客的最近購買時間"), "）</span><br>",
            "• <b>", get_lang_text("statistics.labels.q25", "第25百分位數"), "：</b>", round(q25, 0), " ", get_lang_text("statistics.units.days", "天"), "<br>",
            "<span style='color: #666; font-size: 12px;'>　（", get_lang_text("statistics.explanations.q25_recency", "25%顧客在此天數內購買"), "）</span><br>",
            "• <b>", get_lang_text("statistics.labels.q75", "第75百分位數"), "：</b>", round(q75, 0), " ", get_lang_text("statistics.units.days", "天"), "<br>",
            "<span style='color: #666; font-size: 12px;'>　（", get_lang_text("statistics.explanations.q75_recency", "75%顧客在此天數內購買"), "）</span><br>",
            "• <b>", get_lang_text("statistics.labels.variance_degree", "變異程度"), "：</b>",
            if(var_val > (mean_val^2 * 0.5)) get_lang_text("statistics.explanations.variance_high_recency", "顧客之間的購買時間差異很大") else get_lang_text("statistics.explanations.variance_low_recency", "顧客之間的購買時間差異較小"),
            "<br><br>",
            "🎯 <b>", get_lang_text("statistics.labels.ai_marketing_suggestions", "AI行銷建議"), "：</b><br>",
            "• ", get_lang_text("statistics.marketing_suggestions.recency.active_customers", "活躍客戶"), "(≤", round(q20, 0), get_lang_text("statistics.units.days", "天"), ")：", get_lang_text("statistics.marketing_suggestions.recency.cross_selling", "交叉銷售"), "<br>",
            "• ", get_lang_text("statistics.marketing_suggestions.recency.mid_active", "中度活躍"), "：", get_lang_text("statistics.marketing_suggestions.recency.mid_active_action", "定期關懷"), "<br>",
            "• ", get_lang_text("statistics.marketing_suggestions.recency.inactive_customers", "不活躍"), "(≥", round(q80, 0), get_lang_text("statistics.units.days", "天"), ")：", get_lang_text("statistics.marketing_suggestions.recency.winback_campaign", "召回活動")
          ))
        },
        "F" = {
          col_data <- data$f_value[!is.na(data$f_value)]
          q25 <- quantile(col_data, 0.25)
          q50 <- quantile(col_data, 0.5)
          q75 <- quantile(col_data, 0.75)
          mean_val <- mean(col_data)
          var_val <- var(col_data)
          
          # 80/20分組
          q20 <- quantile(col_data, 0.2)
          q80 <- quantile(col_data, 0.8)
          
          HTML(paste0(
            "• <b>", get_lang_text("statistics.labels.mean", "平均數"), "：</b>", round(mean_val, 1), " ", get_lang_text("statistics.units.times", "次"), "<br>",
            "<span style='color: #666; font-size: 12px;'>　（", get_lang_text("statistics.explanations.mean_frequency", "顧客平均購買次數"), "）</span><br>",
            "• <b>", get_lang_text("statistics.labels.median", "中位數"), "：</b>", round(q50, 1), " ", get_lang_text("statistics.units.times", "次"), "<br>",
            "<span style='color: #666; font-size: 12px;'>　（", get_lang_text("statistics.explanations.median_frequency", "一半顧客購買次數高於此值"), "）</span><br>",
            "• <b>", get_lang_text("statistics.labels.q25", "第25百分位數"), "：</b>", round(q25, 1), " ", get_lang_text("statistics.units.times", "次"), "<br>",
            "<span style='color: #666; font-size: 12px;'>　（", get_lang_text("statistics.explanations.q25_frequency", "25%顧客購買次數低於此值"), "）</span><br>",
            "• <b>", get_lang_text("statistics.labels.q75", "第75百分位數"), "：</b>", round(q75, 1), " ", get_lang_text("statistics.units.times", "次"), "<br>",
            "<span style='color: #666; font-size: 12px;'>　（", get_lang_text("statistics.explanations.q75_frequency", "75%顧客購買次數低於此值"), "）</span><br>",
            "• <b>", get_lang_text("statistics.labels.variance_degree", "變異程度"), "：</b>",
            if(var_val > (mean_val^2 * 0.5)) get_lang_text("statistics.explanations.variance_high_frequency", "顧客之間的購買頻率差異很大") else get_lang_text("statistics.explanations.variance_low_frequency", "顧客之間的購買頻率差異較小"),
            "<br><br>",
            "🎯 <b>", get_lang_text("statistics.labels.ai_marketing_suggestions", "AI行銷建議"), "：</b><br>",
            "• ", get_lang_text("statistics.marketing_suggestions.frequency.high_frequency", "高頻客戶"), "(≥", round(q80, 1), get_lang_text("statistics.units.times", "次"), ")：", get_lang_text("statistics.marketing_suggestions.frequency.high_frequency_action", "VIP計劃"), "<br>",
            "• ", get_lang_text("statistics.marketing_suggestions.frequency.mid_frequency", "中頻客戶"), "：", get_lang_text("statistics.marketing_suggestions.frequency.mid_frequency_action", "忠誠度獎勵"), "<br>",
            "• ", get_lang_text("statistics.marketing_suggestions.frequency.low_frequency", "低頻客戶"), "(≤", round(q20, 1), get_lang_text("statistics.units.times", "次"), ")：", get_lang_text("statistics.marketing_suggestions.frequency.low_frequency_action", "試用優惠")
          ))
        },
        "IPT" = {
          col_data <- if("ipt_mean" %in% names(data)) {
            data$ipt_mean[!is.na(data$ipt_mean)]
          } else {
            numeric(0)
          }
          
          if(length(col_data) > 0) {
            q25 <- quantile(col_data, 0.25)
            q50 <- quantile(col_data, 0.5)
            q75 <- quantile(col_data, 0.75)
            mean_val <- mean(col_data)
            var_val <- var(col_data)
            
            # 80/20分組
            q20 <- quantile(col_data, 0.2)
            q80 <- quantile(col_data, 0.8)
            
            HTML(paste0(
              "• <b>", get_lang_text("statistics.labels.mean", "平均數"), "：</b>", round(mean_val, 1), " ", get_lang_text("statistics.units.days", "天"), "<br>",
              "<span style='color: #666; font-size: 12px;'>　（", get_lang_text("statistics.explanations.mean_ipt", "顧客平均購買間隔天數"), "）</span><br>",
              "• <b>", get_lang_text("statistics.labels.median", "中位數"), "：</b>", round(q50, 1), " ", get_lang_text("statistics.units.days", "天"), "<br>",
              "<span style='color: #666; font-size: 12px;'>　（", get_lang_text("statistics.explanations.median_ipt", "一半顧客購買週期短於此值"), "）</span><br>",
              "• <b>", get_lang_text("statistics.labels.q25", "第25百分位數"), "：</b>", round(q25, 1), " ", get_lang_text("statistics.units.days", "天"), "<br>",
              "<span style='color: #666; font-size: 12px;'>　（", get_lang_text("statistics.explanations.q25_ipt", "25%顧客購買週期短於此值"), "）</span><br>",
              "• <b>", get_lang_text("statistics.labels.q75", "第75百分位數"), "：</b>", round(q75, 1), " ", get_lang_text("statistics.units.days", "天"), "<br>",
              "<span style='color: #666; font-size: 12px;'>　（", get_lang_text("statistics.explanations.q75_ipt", "75%顧客購買週期短於此值"), "）</span><br>",
              "• <b>", get_lang_text("statistics.labels.variance_degree", "變異程度"), "：</b>",
              if(var_val > (mean_val^2 * 0.5)) get_lang_text("statistics.explanations.variance_high_ipt", "顧客之間的購買週期差異很大") else get_lang_text("statistics.explanations.variance_low_ipt", "顧客之間的購買週期差異較小"),
              "<br><br>",
              "🎯 <b>", get_lang_text("statistics.labels.ai_marketing_suggestions", "AI行銷建議"), "：</b><br>",
              "• ", get_lang_text("statistics.marketing_suggestions.ipt.short_cycle", "短週期客戶"), "(≤", round(q20, 1), get_lang_text("statistics.units.days", "天"), ")：", get_lang_text("statistics.marketing_suggestions.ipt.short_cycle_action", "常規提醒"), "<br>",
              "• ", get_lang_text("statistics.marketing_suggestions.ipt.mid_cycle", "中週期客戶"), "：", get_lang_text("statistics.marketing_suggestions.ipt.mid_cycle_action", "定期推送"), "<br>",
              "• ", get_lang_text("statistics.marketing_suggestions.ipt.long_cycle", "長週期客戶"), "(≥", round(q80, 1), get_lang_text("statistics.units.days", "天"), ")：", get_lang_text("statistics.marketing_suggestions.ipt.long_cycle_action", "特殊激勵")
            ))
          } else {
            HTML(paste0("• ", get_lang_text("distribution_insights.ipt.no_ipt_data", "無購買週期數據")))
          }
        },
        "CAI" = {
          col_data <- if("cai_value" %in% names(data)) {
            data$cai_value[!is.na(data$cai_value)]
          } else if("cai" %in% names(data)) {
            data$cai[!is.na(data$cai)]
          } else {
            numeric(0)
          }
          
          if(length(col_data) > 0) {
            q25 <- quantile(col_data, 0.25)
            q50 <- quantile(col_data, 0.5)
            q75 <- quantile(col_data, 0.75)
            mean_val <- mean(col_data)
            var_val <- var(col_data)
            
            # 80/20分組
            q20 <- quantile(col_data, 0.2)
            q80 <- quantile(col_data, 0.8)
            
            HTML(paste0(
              "• <b>", get_lang_text("statistics.labels.mean", "平均數"), "：</b>", round(mean_val, 2), "<br>",
              "<span style='color: #666; font-size: 12px;'>　（", get_lang_text("statistics.explanations.mean_cai", "顧客平均活躍度指數"), "）</span><br>",
              "• <b>", get_lang_text("statistics.labels.median", "中位數"), "：</b>", round(q50, 2), "<br>",
              "<span style='color: #666; font-size: 12px;'>　（", get_lang_text("statistics.explanations.median_cai", "一半顧客活躍度高於此值"), "）</span><br>",
              "• <b>", get_lang_text("statistics.labels.q25", "第25百分位數"), "：</b>", round(q25, 2), "<br>",
              "<span style='color: #666; font-size: 12px;'>　（", get_lang_text("statistics.explanations.q25_cai", "25%顧客活躍度高於此值"), "）</span><br>",
              "• <b>", get_lang_text("statistics.labels.q75", "第75百分位數"), "：</b>", round(q75, 2), "<br>",
              "<span style='color: #666; font-size: 12px;'>　（", get_lang_text("statistics.explanations.q75_cai", "75%顧客活躍度低於此值"), "）</span><br>",
              "• <b>", get_lang_text("statistics.labels.variance_degree", "變異程度"), "：</b>",
              if(var_val > (mean_val^2 * 0.5)) get_lang_text("statistics.explanations.variance_high_cai", "顧客之間的活躍度差異很大") else get_lang_text("statistics.explanations.variance_low_cai", "顧客之間的活躍度差異較小"),
              "<br><br>",
              "🎯 <b>", get_lang_text("statistics.labels.ai_marketing_suggestions", "AI行銷建議"), "：</b><br>",
              "• ", get_lang_text("segment_labels.recency.high_active", "高活躍"), get_lang_text("statistics.labels.customer", "客戶"), "(≥", round(q80, 2), ")：", get_lang_text("statistics.marketing_suggestions.recency.high_active", "維持策略"), "<br>",
              "• ", get_lang_text("segment_labels.recency.general", "一般活躍"), get_lang_text("statistics.labels.customer", "客戶"), "：", get_lang_text("statistics.marketing_suggestions.recency.mid_active", "激勵計劃"), "<br>",
              "• ", get_lang_text("segment_labels.recency.churned", "流失"), get_lang_text("statistics.labels.customer", "客戶"), "(≤", round(q20, 2), ")：", get_lang_text("statistics.marketing_suggestions.recency.low_active", "喚醒策略")
            ))
          } else {
            HTML(paste0("• ", get_lang_text("distribution_insights.cai.no_activity_data", "無活躍度數據")))
          }
        },
        "PCV" = {
          col_data <- if("pcv" %in% names(data)) {
            data$pcv[!is.na(data$pcv)]
          } else if("total_spent" %in% names(data)) {
            data$total_spent[!is.na(data$total_spent)]
          } else {
            numeric(0)
          }
          
          if(length(col_data) > 0) {
            q25 <- quantile(col_data, 0.25)
            q50 <- quantile(col_data, 0.5)
            q75 <- quantile(col_data, 0.75)
            mean_val <- mean(col_data)
            var_val <- var(col_data)
            
            # 80/20分組
            q20 <- quantile(col_data, 0.2)
            q80 <- quantile(col_data, 0.8)
            
            HTML(paste0(
              "• <b>", get_lang_text("statistics.labels.mean", "平均數"), "：</b>$", round(mean_val, 2), "<br>",
              "<span style='color: #666; font-size: 12px;'>　（", get_lang_text("statistics.explanations.mean_pcv", "顧客平均過去消費總額"), "）</span><br>",
              "• <b>", get_lang_text("statistics.labels.median", "中位數"), "：</b>$", round(q50, 2), "<br>",
              "<span style='color: #666; font-size: 12px;'>　（", get_lang_text("statistics.explanations.median_pcv", "一半顧客過去消費高於此值"), "）</span><br>",
              "• <b>", get_lang_text("statistics.labels.q25", "第25百分位數"), "：</b>$", round(q25, 2), "<br>",
              "<span style='color: #666; font-size: 12px;'>　（", get_lang_text("statistics.explanations.q25_pcv", "25%顧客過去消費高於此值"), "）</span><br>",
              "• <b>", get_lang_text("statistics.labels.q75", "第75百分位數"), "：</b>$", round(q75, 2), "<br>",
              "<span style='color: #666; font-size: 12px;'>　（", get_lang_text("statistics.explanations.q75_pcv", "75%顧客過去消費低於此值"), "）</span><br>",
              "• <b>", get_lang_text("statistics.labels.variance_degree", "變異程度"), "：</b>",
              if(var_val > (mean_val^2 * 0.5)) get_lang_text("statistics.explanations.variance_high_pcv", "顧客之間的過去消費差異很大") else get_lang_text("statistics.explanations.variance_low_pcv", "顧客之間的過去消費差異較小"),
              "<br><br>",
              paste0("🎯 <b>", get_lang_text("statistics.labels.ai_marketing_suggestions", "AI行銷建議"), "：</b><br>"),
              "• ", get_lang_text("statistics.marketing_suggestions.pcv.high_contributor", "高貢獻客戶"), "(≥$", round(q80, 2), ")：", get_lang_text("statistics.marketing_suggestions.pcv.high_contributor_action", "專屬優惠"), "<br>",
              "• ", get_lang_text("statistics.marketing_suggestions.pcv.mid_contributor", "中貢獻客戶"), "：", get_lang_text("statistics.marketing_suggestions.pcv.mid_contributor_action", "提升方案"), "<br>",
              "• ", get_lang_text("statistics.marketing_suggestions.pcv.low_contributor", "低貢獻客戶"), "(≤$", round(q20, 2), ")：", get_lang_text("statistics.marketing_suggestions.pcv.low_contributor_action", "培養策略")
            ))
          } else {
            HTML(paste0("• ", get_lang_text("errors.no_pcv_data", "無PCV數據可用")))
          }
        },
        "CRI" = {
          col_data <- if("cri" %in% names(data)) {
            data$cri[!is.na(data$cri)]
          } else {
            numeric(0)
          }
          
          if(length(col_data) > 0) {
            q25 <- quantile(col_data, 0.25)
            q50 <- quantile(col_data, 0.5)
            q75 <- quantile(col_data, 0.75)
            mean_val <- mean(col_data)
            var_val <- var(col_data)
            
            # 80/20分組
            q20 <- quantile(col_data, 0.2)
            q80 <- quantile(col_data, 0.8)
            
            HTML(paste0(
              "• <b>", get_lang_text("statistics.labels.mean", "平均數"), "：</b>", round(mean_val * 100, 1), " ", get_lang_text("statistics.units.score", "分"), "<br>",
              "<span style='color: #666; font-size: 12px;'>　（", get_lang_text("statistics.explanations.mean_cri", "顧客平均穩定度分數"), "）</span><br>",
              "• <b>", get_lang_text("statistics.labels.median", "中位數"), "：</b>", round(q50 * 100, 1), " ", get_lang_text("statistics.units.score", "分"), "<br>",
              "<span style='color: #666; font-size: 12px;'>　（", get_lang_text("statistics.explanations.median_cri", "一半顧客穩定度高於此值"), "）</span><br>",
              "• <b>", get_lang_text("statistics.labels.q25", "第25百分位數"), "：</b>", round(q25 * 100, 1), " ", get_lang_text("statistics.units.score", "分"), "<br>",
              "<span style='color: #666; font-size: 12px;'>　（", get_lang_text("statistics.explanations.q25_cri", "25%顧客穩定度高於此值"), "）</span><br>",
              "• <b>", get_lang_text("statistics.labels.q75", "第75百分位數"), "：</b>", round(q75 * 100, 1), " ", get_lang_text("statistics.units.score", "分"), "<br>",
              "<span style='color: #666; font-size: 12px;'>　（", get_lang_text("statistics.explanations.q75_cri", "75%顧客穩定度低於此值"), "）</span><br>",
              "• <b>", get_lang_text("statistics.labels.variance_degree", "變異程度"), "：</b>",
              if(var_val > (mean_val^2 * 0.5)) get_lang_text("statistics.explanations.variance_high_cri", "顧客之間的穩定度差異很大") else get_lang_text("statistics.explanations.variance_low_cri", "顧客之間的穩定度差異較小"),
              "<br><br>",
              paste0("🎯 <b>", get_lang_text("statistics.labels.ai_marketing_suggestions_80_20", "AI行銷建議（80/20法則）"), "：</b><br>"),
              "• ", get_lang_text("segment_labels.cri.high_stability", "高穩定"), " Top 20%(≥", round(q80 * 100, 1), get_lang_text("statistics.units.score", "分"), ")：", get_lang_text("statistics.marketing_suggestions.cri.high_stability", "VIP維護策略"), "<br>",
              "• ", get_lang_text("segment_labels.cri.mid_stability", "中穩定"), " Middle 60%：", get_lang_text("statistics.marketing_suggestions.cri.mid_stability", "忠誠度提升計劃"), "<br>",
              "• ", get_lang_text("segment_labels.cri.low_stability", "低穩定"), " Bottom 20%(≤", round(q20 * 100, 1), get_lang_text("statistics.units.score", "分"), ")：", get_lang_text("statistics.marketing_suggestions.cri.low_stability", "挽留激活方案")
            ))
          } else {
            HTML(paste0("• ", get_lang_text("errors.no_cri_data", "無CRI數據可用")))
          }
        },
        "NES" = {
          if("nes_status" %in% names(data)) {
            nes_counts <- table(data$nes_status)
            total <- sum(nes_counts)
            
            HTML(paste0(
              "<b>", get_lang_text("ai_insights.nes_distribution.title"), "：</b><br>",
              if("N" %in% names(nes_counts)) paste0(
                "• ", get_lang_text("ai_insights.nes_distribution.new_customer"), "(N)：",
                nes_counts["N"], " ", get_lang_text("statistics.units.people", "人"), " (",
                round(nes_counts["N"]/total*100, 1), "%)<br>"
              ) else "",
              if("E0" %in% names(nes_counts)) paste0(
                "• ", get_lang_text("ai_insights.nes_distribution.core_customer"), "(E0)：",
                nes_counts["E0"], " ", get_lang_text("statistics.units.people", "人"), " (",
                round(nes_counts["E0"]/total*100, 1), "%)<br>"
              ) else "",
              if("S1" %in% names(nes_counts)) paste0(
                "• ", get_lang_text("ai_insights.nes_distribution.dozing_customer"), "(S1)：",
                nes_counts["S1"], " ", get_lang_text("statistics.units.people", "人"), " (",
                round(nes_counts["S1"]/total*100, 1), "%)<br>"
              ) else "",
              if("S2" %in% names(nes_counts)) paste0(
                "• ", get_lang_text("ai_insights.nes_distribution.half_asleep_customer"), "(S2)：",
                nes_counts["S2"], " ", get_lang_text("statistics.units.people", "人"), " (",
                round(nes_counts["S2"]/total*100, 1), "%)<br>"
              ) else "",
              if("S3" %in% names(nes_counts)) paste0(
                "• ", get_lang_text("ai_insights.nes_distribution.sleeping_customer"), "(S3)：",
                nes_counts["S3"], " ", get_lang_text("statistics.units.people", "人"), " (",
                round(nes_counts["S3"]/total*100, 1), "%)<br><br>"
              ) else "",
              "🎯 <b>", get_lang_text("ai_insights.marketing_suggestions.title"), "</b><br>",
              "• ", get_lang_text("ai_insights.marketing_suggestions.new_customer"), "<br>",
              "• ", get_lang_text("ai_insights.marketing_suggestions.core_customer"), "<br>",
              "• ", get_lang_text("ai_insights.marketing_suggestions.dozing_customer"), "<br>",
              "• ", get_lang_text("ai_insights.marketing_suggestions.sleeping_customer")
            ))
          } else {
            HTML(paste0("• ", get_lang_text("ai_insights.no_nes_data")))
          }
        }
      )
      
      return(insight)
    })
    
    # 直方圖洞察
    output$hist_insight <- renderUI({
      req(lang_texts())  # React to language changes
      req(values$current_metric, values$dna_results)
      data <- values$dna_results$data_by_customer
      metric <- values$current_metric
      
      insight <- switch(metric,
        "M" = {
          col_data <- data$m_value[!is.na(data$m_value)]
          mode_val <- as.numeric(names(sort(table(round(col_data, 0)), decreasing = TRUE)[1]))
          mode_count <- max(table(round(col_data, 0)))
          
          # 使用80/20法則計算高中低群體比例
          q20 <- quantile(col_data, 0.2)
          q80 <- quantile(col_data, 0.8)
          high_pct <- round(sum(col_data >= q80) / length(col_data) * 100, 1)
          low_pct <- round(sum(col_data <= q20) / length(col_data) * 100, 1)
          mid_pct <- 100 - high_pct - low_pct
          
          HTML(paste0(
            "📊 <b>", get_lang_text("distribution_insights.title_colon", "分布洞察："), "</b><br>",
            "• ", get_lang_text("distribution_insights.monetary.most_common_amount", "最常見金額"), "：$", mode_val, "<br>",
            "• ", get_lang_text("distribution_insights.monetary.high_mid_low_ratio", "高中低比例"), "：", high_pct, "% / ", mid_pct, "% / ", low_pct, "%<br>",
            "• ", ifelse(var(col_data) > (mean(col_data)^2 * 0.5),
                        get_lang_text("distribution_insights.monetary.need_differentiation", "需差異化策略"),
                        get_lang_text("distribution_insights.monetary.relatively_homogeneous", "客群相對同質"))
          ))
        },
        "R" = {
          col_data <- data$r_value[!is.na(data$r_value)]
          recent <- sum(col_data <= 30)
          medium <- sum(col_data > 30 & col_data <= 90)
          old <- sum(col_data > 90)
          
          HTML(paste0(
            "📊 <b>", get_lang_text("distribution_insights.title_colon", "分布洞察："), "</b><br>",
            "• ", get_lang_text("distribution_insights.recency.within_30_days", "30天內"), "：", recent, " ", get_lang_text("distribution_insights.common.unit_people", "位"), " (", round(recent/length(col_data)*100, 1), "%)<br>",
            "• ", get_lang_text("distribution_insights.recency.days_31_to_90", "31-90天"), "：", medium, " ", get_lang_text("distribution_insights.common.unit_people", "位"), " (", round(medium/length(col_data)*100, 1), "%)<br>",
            "• ", get_lang_text("distribution_insights.recency.over_90_days", ">90天"), "：", old, " ", get_lang_text("distribution_insights.common.unit_people", "位"), " (", round(old/length(col_data)*100, 1), "%)"
          ))
        },
        "F" = {
          col_data <- data$f_value[!is.na(data$f_value)]
          one_time <- sum(col_data == 1)
          two_three <- sum(col_data >= 2 & col_data <= 3)
          frequent <- sum(col_data > 3)
          
          HTML(paste0(
            "📊 <b>", get_lang_text("distribution_insights.title_colon", "分布洞察："), "</b><br>",
            "• ", get_lang_text("distribution_insights.frequency.one_time_purchase", "單次購買"), "：", one_time, " ", get_lang_text("distribution_insights.common.unit_people", "位"), " (", round(one_time/length(col_data)*100, 1), "%)<br>",
            "• ", get_lang_text("distribution_insights.frequency.two_to_three_times", "2-3次"), "：", two_three, " ", get_lang_text("distribution_insights.common.unit_people", "位"), " (", round(two_three/length(col_data)*100, 1), "%)<br>",
            "• ", get_lang_text("distribution_insights.frequency.over_three_times", ">3次"), "：", frequent, " ", get_lang_text("distribution_insights.common.unit_people", "位"), " (", round(frequent/length(col_data)*100, 1), "%)"
          ))
        },
        "IPT" = {
          if("ipt_mean" %in% names(data)) {
            col_data <- data$ipt_mean[!is.na(data$ipt_mean)]
            short <- sum(col_data <= 30)
            medium <- sum(col_data > 30 & col_data <= 90)
            long <- sum(col_data > 90)
            
            HTML(paste0(
              "📊 <b>", get_lang_text("distribution_insights.title_colon", "分布洞察："), "</b><br>",
              "• ", get_lang_text("distribution_insights.ipt.cycle_30_days_or_less", "≤30天週期"), "：", short, " ", get_lang_text("distribution_insights.common.unit_people", "位"), " (", round(short/length(col_data)*100, 1), "%)<br>",
              "• ", get_lang_text("distribution_insights.ipt.cycle_31_to_90_days", "31-90天週期"), "：", medium, " ", get_lang_text("distribution_insights.common.unit_people", "位"), " (", round(medium/length(col_data)*100, 1), "%)<br>",
              "• ", get_lang_text("distribution_insights.ipt.cycle_over_90_days", ">90天週期"), "：", long, " ", get_lang_text("distribution_insights.common.unit_people", "位"), " (", round(long/length(col_data)*100, 1), "%)<br>",
              "• ", ifelse(var(col_data) > (mean(col_data)^2 * 0.5),
                          get_lang_text("distribution_insights.ipt.cycle_variance_high", "週期差異大"),
                          get_lang_text("distribution_insights.ipt.cycle_variance_low", "週期相對穩定"))
            ))
          } else {
            HTML(paste0("📊 <b>", get_lang_text("distribution_insights.title_colon", "分布洞察："), "</b><br>• ", get_lang_text("distribution_insights.ipt.no_ipt_data", "無購買週期數據")))
          }
        },
        "CAI" = {
          if("cai_value" %in% names(data) || "cai" %in% names(data)) {
            col_data <- if("cai_value" %in% names(data)) {
              data$cai_value[!is.na(data$cai_value)]
            } else {
              data$cai[!is.na(data$cai)]
            }
            
            inactive <- sum(col_data <= 0.1)
            stable <- sum(col_data > 0.1 & col_data <= 0.9)
            active <- sum(col_data > 0.9)
            
            HTML(paste0(
              "📊 <b>", get_lang_text("distribution_insights.title_colon", "分布洞察："), "</b><br>",
              "• ", get_lang_text("distribution_insights.cai.trending_static", "漸趨靜止"), get_lang_text("distribution_insights.cai.range_static", "(≤0.1)"), "：", inactive, " ", get_lang_text("distribution_insights.common.unit_people", "位"), " (", round(inactive/length(col_data)*100, 1), "%)<br>",
              "• ", get_lang_text("distribution_insights.cai.stable_consumption", "穩定消費"), get_lang_text("distribution_insights.cai.range_stable", "(0.1-0.9)"), "：", stable, " ", get_lang_text("distribution_insights.common.unit_people", "位"), " (", round(stable/length(col_data)*100, 1), "%)<br>",
              "• ", get_lang_text("distribution_insights.cai.trending_active", "漸趨活躍"), get_lang_text("distribution_insights.cai.range_active", "(>0.9)"), "：", active, " ", get_lang_text("distribution_insights.common.unit_people", "位"), " (", round(active/length(col_data)*100, 1), "%)<br>",
              "• ", ifelse(active > inactive,
                          get_lang_text("distribution_insights.cai.activity_good", "客群活躍度佳"),
                          get_lang_text("distribution_insights.cai.needs_improvement", "需提升活躍度"))
            ))
          } else {
            HTML(paste0("📊 <b>", get_lang_text("distribution_insights.title_colon", "分布洞察："), "</b><br>• ", get_lang_text("distribution_insights.cai.no_activity_data", "無活躍度數據")))
          }
        },
        "PCV" = {
          if("pcv" %in% names(data) || "total_spent" %in% names(data)) {
            col_data <- if("pcv" %in% names(data)) {
              data$pcv[!is.na(data$pcv)]
            } else {
              data$total_spent[!is.na(data$total_spent)]
            }
            
            q20 <- quantile(col_data, 0.2)
            q80 <- quantile(col_data, 0.8)
            low_value <- sum(col_data <= q20)
            mid_value <- sum(col_data > q20 & col_data < q80)
            high_value <- sum(col_data >= q80)
            
            HTML(paste0(
              "📊 <b>", get_lang_text("ai_insights.distribution.title", "分布洞察"), "：</b><br>",
              "• ", get_lang_text("ai_insights.distribution.low_value", "低價值"), "(≤$", round(q20, 0), ")：", low_value, " ", get_lang_text("statistics.units.people", "位"), " (20%)<br>",
              "• ", get_lang_text("ai_insights.distribution.mid_value", "中價值"), "：", mid_value, " ", get_lang_text("statistics.units.people", "位"), " (60%)<br>",
              "• ", get_lang_text("ai_insights.distribution.high_value", "高價值"), "(≥$", round(q80, 0), ")：", high_value, " ", get_lang_text("statistics.units.people", "位"), " (20%)<br>",
              "• ", get_lang_text("ai_insights.distribution.80_20_rule", "80/20法則"), "：20%",
              get_lang_text("ai_insights.distribution.customers_contribute", "客戶貢獻"),
              round(sum(col_data[col_data >= q80])/sum(col_data)*100, 1), "%",
              get_lang_text("ai_insights.distribution.revenue", "營收")
            ))
          } else {
            HTML(paste0("📊 <b>", get_lang_text("ai_insights.distribution.title", "分布洞察"), "：</b><br>• ",
                       get_lang_text("ai_insights.no_pcv_data", "無過去價值數據")))
          }
        },
        "CRI" = {
          if("cri" %in% names(data)) {
            col_data <- data$cri[!is.na(data$cri)]
            # 使用80/20法則
            q20 <- quantile(col_data, 0.2)
            q80 <- quantile(col_data, 0.8)
            low_stability <- sum(col_data <= q20)
            mid_stability <- sum(col_data > q20 & col_data < q80)
            high_stability <- sum(col_data >= q80)
            
            # 計算Top 20%客戶的營收貢獻（假設CRI與營收正相關）
            if("total_spent" %in% names(data) || "m_value" %in% names(data)) {
              revenue_col <- if("total_spent" %in% names(data)) "total_spent" else "m_value"
              top20_revenue <- sum(data[data$cri >= q80, revenue_col], na.rm = TRUE)
              total_revenue <- sum(data[[revenue_col]], na.rm = TRUE)
              revenue_ratio <- round(top20_revenue/total_revenue*100, 1)
            } else {
              revenue_ratio <- NA
            }
            
            HTML(paste0(
              "📊 <b>", get_lang_text("ai_insights.distribution.title_80_20", "分布洞察（80/20法則）"), "：</b><br>",
              "• ", get_lang_text("ai_insights.distribution.low_stability", "低穩定(Bottom 20%)"), "：", low_stability, " ", get_lang_text("statistics.units.people", "位"), "<br>",
              "• ", get_lang_text("ai_insights.distribution.mid_stability", "中穩定(Middle 60%)"), "：", mid_stability, " ", get_lang_text("statistics.units.people", "位"), "<br>",
              "• ", get_lang_text("ai_insights.distribution.high_stability", "高穩定(Top 20%)"), "：", high_stability, " ", get_lang_text("statistics.units.people", "位"), "<br>",
              if(!is.na(revenue_ratio)) {
                paste0("• <b>", get_lang_text("ai_insights.distribution.80_20_verification", "80/20驗證"), "：</b>",
                       get_lang_text("ai_insights.distribution.top_20_contribution", "Top 20%客戶貢獻"), revenue_ratio, "%",
                       get_lang_text("ai_insights.distribution.revenue", "營收"), "<br>")
              } else "",
              "• <b>", get_lang_text("ai_insights.strategy.title", "策略建議"), "：</b>",
              ifelse(high_stability/low_stability > 1,
                     get_lang_text("ai_insights.strategy.healthy_structure", "穩定客群結構健康"),
                     get_lang_text("ai_insights.strategy.needs_improvement", "需強化客戶穩定度"))
            ))
          } else {
            HTML(paste0("📊 <b>", get_lang_text("ai_insights.distribution.title", "分布洞察"), "：</b><br>• ",
                       get_lang_text("ai_insights.no_stability_data", "無穩定度數據")))
          }
        },
        "NES" = {
          nes_table <- table(data$nes_status)
          max_status <- names(which.max(nes_table))
          HTML(paste0(
            "📊 <b>", get_lang_text("distribution_insights.title_colon", "分布洞察："), "</b><br>",
            "• ", get_lang_text("distribution_insights.nes.most_common_status", "最多顧客狀態"), "：", max_status, "<br>",
            "• ", get_lang_text("distribution_insights.nes.total_customers", "共有"), " ", max(nes_table), " ", get_lang_text("distribution_insights.nes.unit_customers", "位顧客")
          ))
        }
      )
      
      return(insight)
    })
    
    # ECDF Plot - 已移除，不再使用
    # output$plot_ecdf <- renderPlotly({
    #     #   tryCatch({
    #         req(values$dna_results, values$current_metric)
    #         
    #         data <- values$dna_results$data_by_customer
    #         metric <- values$current_metric
    #         
    #         metric_mapping <- list(
    #           "M" = "m_value",
    #           "R" = "r_value", 
    #           "F" = "f_value",
    #           "CAI" = if("cai_value" %in% names(data)) "cai_value" else "cai",
    #           "PCV" = if("pcv" %in% names(data)) "pcv" else "total_spent",
    #           "CRI" = "cri",
    #           "NES" = "nes_status"  # NES 使用狀態分類而非數值
    #         )
    #         
    #         col_name <- metric_mapping[[metric]]
    #         
    #         # Handle missing data gracefully
    #         if (is.null(data) || nrow(data) == 0 || is.null(col_name) || !col_name %in% names(data)) {
    #           return(plotly_empty(type = "scatter") %>%
    #             add_annotations(
    #               text = paste("無法找到", metric, "數據\n請檢查數據是否已正確載入"),
    #               x = 0.5, y = 0.5,
    #               xref = "paper", yref = "paper",
    #               showarrow = FALSE,
    #               font = list(size = 16)
    #             ))
    #         }
    #         
    #         # Get metric values and remove NA
    #         metric_values <- data[[col_name]]
    #         metric_values <- metric_values[!is.na(metric_values)]
    #         
    #         if (length(metric_values) == 0) {
    #           return(plotly_empty(type = "scatter") %>%
    #             add_annotations(
    #               text = paste(metric, "數據為空或全為 NA 值"),
    #               x = 0.5, y = 0.5,
    #               xref = "paper", yref = "paper",
    #               showarrow = FALSE,
    #               font = list(size = 16)
    #             ))
    #         }
    #         
    #         # Special handling for categorical data (NES) 
    #         if (metric == "NES") {
    #           # For NES, show informative message in ECDF position
    #           return(plotly_empty(type = "scatter") %>%
    #             add_annotations(
    #               text = "NES 狀態分佈\n請查看右側直方圖",
    #               x = 0.5, y = 0.5,
    #               xref = "paper", yref = "paper",
    #               showarrow = FALSE,
    #               font = list(size = 18, color = "#1F77B4")
    #             ) %>%
    #             layout(
    #               title = list(text = "ECDF - NES", font = list(size = 16)),
    #               xaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
    #               yaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
    #             ))
    #         } else {
    #           # Compute ECDF for continuous data
    #           sorted_values <- sort(unique(metric_values))
    #           ecdf_fn <- ecdf(metric_values)
    #           ecdf_values <- ecdf_fn(sorted_values)
    #           
    #           # Create plotly visualization with proper hover text
    #           plot_ly(
    #             x = sorted_values, 
    #             y = ecdf_values,
    #             type = "scatter", 
    #             mode = "lines",
    #             line = list(color = "#1F77B4", width = 2),
    #             hoverinfo = "text",
    #             text = paste0(
    #               metric, ": ", format(sorted_values, big.mark = ","),
    #               "<br>累積百分比: ", format(ecdf_values * 100, digits = 2), "%"
    #             )
    #           ) %>%
    #             layout(
    #               title = list(text = paste("ECDF -", metric), font = list(size = 16)),
    #               xaxis = list(title = metric),
    #               yaxis = list(title = "累積機率", tickformat = ".0%"),
    #               showlegend = FALSE
    #             )
    #         }
    #         
    #       }, error = function(e) {
    #         plotly_empty(type = "scatter") %>%
    #           add_annotations(
    #             text = paste("視覺化錯誤:", e$message),
    #             x = 0.5, y = 0.5,
    #             xref = "paper", yref = "paper",
    #             showarrow = FALSE,
    #             font = list(size = 14, color = "red")
    #           )
    #       })
    #     })
    
    # Histogram Plot - Based on microDNADistribution component  
    output$plot_hist <- renderPlotly({
      tryCatch({
        req(values$dna_results, values$current_metric)
        
        data <- values$dna_results$data_by_customer
        metric <- values$current_metric
        
        metric_mapping <- list(
          "M" = "m_value",
          "R" = "r_value",
          "F" = "f_value",
          "IPT" = "ipt_mean",  # 購買週期
          "CAI" = if("cai_value" %in% names(data)) "cai_value" else "cai",
          "PCV" = if("pcv" %in% names(data)) "pcv" else "total_spent",
          "CRI" = "cri",
          "NES" = "nes_status"  # NES 使用狀態分類而非數值
        )
        
        col_name <- metric_mapping[[metric]]
        
        # Handle missing data gracefully
        if (is.null(data) || nrow(data) == 0 || is.null(col_name) || !col_name %in% names(data)) {
          error_msg <- paste(
            get_lang_text("charts.errors.data_not_found", "無法找到"),
            metric,
            get_lang_text("charts.errors.check_data", "數據\n請檢查數據是否已正確載入")
          )
          return(plotly_empty(type = "bar") %>%
            add_annotations(
              text = error_msg,
              x = 0.5, y = 0.5,
              xref = "paper", yref = "paper",
              showarrow = FALSE,
              font = list(size = 16)
            ))
        }
        
        # Get metric values and remove NA
        metric_values <- data[[col_name]]
        metric_values <- metric_values[!is.na(metric_values)]
        
        if (length(metric_values) == 0) {
          error_msg <- paste(metric, get_lang_text("charts.errors.empty_data", "數據為空或全為 NA 值"))
          return(plotly_empty(type = "bar") %>%
            add_annotations(
              text = error_msg,
              x = 0.5, y = 0.5,
              xref = "paper", yref = "paper",
              showarrow = FALSE,
              font = list(size = 16)
            ))
        }
        
        # Choose visualization based on metric type
        if (metric == "F" || metric == "NES") {
          # For categorical/frequency data, use bar plot
          if (metric == "F") {
            # For frequency, ensure proper integer ordering
            max_f <- max(metric_values, na.rm = TRUE)
            freq_levels <- as.character(1:min(max_f, 50))
            cnt <- table(factor(metric_values, levels = freq_levels))
          } else {
            # For NES, use predefined levels
            cnt <- table(factor(metric_values, levels = c("N", "E0", "S1", "S2", "S3")))
          }
          
          # Convert to data frame for plotting
          df <- data.frame(x = names(cnt), y = as.numeric(cnt))
          total <- sum(df$y)
          
          # Get language-aware metric display name for bar charts
          metric_display_name <- if(metric == "NES") {
            "NES"
          } else {
            get_lang_text(
              paste0("marketing_recommendations.metric_names.", metric),
              metric  # fallback to metric code
            )
          }

          plot_ly(
            df,
            x = ~x,
            y = ~y,
            type = "bar",
            marker = list(color = "#1F77B4"),
            hoverinfo = "text",
            text = ~paste0(
              get_lang_text("charts.labels.value", "值"), ": ", x,
              "<br>", get_lang_text("charts.labels.count", "計數"), ": ", format(y, big.mark = ","),
              "<br>", get_lang_text("charts.labels.percentage", "百分比"), ": ", round(y / total * 100, 1), "%"
            )
          ) %>%
            layout(
              title = list(
                text = if(metric == "NES") {
                  get_lang_text("charts.titles.nes_distribution", "NES 狀態分佈")
                } else {
                  paste(get_lang_text("charts.titles.distribution", "分佈圖"), "-", metric_display_name)
                },
                font = list(size = 16)
              ),
              xaxis = list(
                title = if(metric == "NES") {
                  get_lang_text("charts.axes.nes_status", "NES 狀態")
                } else {
                  get_lang_text("charts.axes.value", "值")
                },
                categoryorder = "array",
                categoryarray = df$x
              ),
              yaxis = list(
                title = if(metric == "NES") {
                  get_lang_text("charts.axes.customer_count", "客戶數")
                } else {
                  get_lang_text("charts.axes.count", "計數")
                }
              )
            )
        } else {
          # For continuous data (M, R), use histogram
          # Get language-aware metric display name
          metric_display_name <- get_lang_text(
            paste0("marketing_recommendations.metric_names.", metric),
            metric  # fallback to metric code
          )

          plot_ly(
            x = ~metric_values,
            type = "histogram",
            marker = list(color = "#1F77B4"),
            hovertemplate = paste0(
              metric_display_name, ": %{x}<br>",
              get_lang_text("charts.labels.count", "計數"), ": %{y}<br>",
              "<extra></extra>"
            )
          ) %>%
            layout(
              title = list(
                text = paste(get_lang_text("charts.titles.histogram", "直方圖"), "-", metric_display_name),
                font = list(size = 16)
              ),
              xaxis = list(title = metric_display_name),
              yaxis = list(title = get_lang_text("charts.axes.frequency", "頻率")),
              showlegend = FALSE
            )
        }
        
      }, error = function(e) {
        error_msg <- paste(get_lang_text("charts.errors.visualization_error", "視覺化錯誤"), ":", e$message)
        plotly_empty(type = "bar") %>%
          add_annotations(
            text = error_msg,
            x = 0.5, y = 0.5,
            xref = "paper", yref = "paper",
            showarrow = FALSE,
            font = list(size = 14, color = "red")
          )
      })
    })
    
    # Email Mapping Table
    output$email_mapping_table <- renderDT({
      req(lang_texts())  # React to language changes
      req(values$email_mapping)

      values$email_mapping %>%
        rename(
          "原始電子郵件" = original_customer_id,
          "數字客戶ID" = customer_id
        ) %>%
        arrange(`數字客戶ID`)
      
    }, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
    
    # Check if email mapping exists
    output$has_email_mapping <- reactive({
      !is.null(values$email_mapping)
    })
    outputOptions(output, "has_email_mapping", suspendWhenHidden = FALSE)
    
    # Table info output (Following R092: get_lang_text for all UI text)
    output$table_info <- renderText({
      req(lang_texts())  # React to language changes
      if (!is.null(input$convert_to_text) && input$convert_to_text) {
        get_lang_text("ui.display_mode.text_classification", "顯示模式：文字分類 (高/中/低)")
      } else {
        get_lang_text("ui.display_mode.numeric_format", "顯示模式：數值 (小數點後2位)")
      }
    })
    
    # DNA Results Table
    output$dna_table <- renderDT({
      req(lang_texts())
      tryCatch({
        table_data <- get_active_dna_data()
        req(table_data)

        # 生成AI分析洞察（沿用舊邏輯，使用清理後資料）
        values$ai_insights <- generate_ai_insights(table_data)

        convert_values <- isTRUE(input$convert_to_text)
        numeric_cols <- intersect(
          c("r_value", "f_value", "m_value", "ipt_mean", "cai_value", "pcv", "cri", "be2", "clv"),
          names(table_data)
        )

        if (convert_values) {
          low_label <- get_lang_text("segment_labels.low", "低")
          mid_label <- get_lang_text("segment_labels.mid", "中")
          high_label <- get_lang_text("segment_labels.high", "高")

          for (col in setdiff(numeric_cols, "cai_value")) {
            if (is.numeric(table_data[[col]])) {
              quantiles <- quantile(table_data[[col]], c(0.33, 0.67), na.rm = TRUE)
              table_data[[col]] <- factor(
                ifelse(table_data[[col]] <= quantiles[1], low_label,
                  ifelse(table_data[[col]] <= quantiles[2], mid_label, high_label)
                ),
                levels = c(low_label, mid_label, high_label)
              )
            }
          }

          if ("cai_value" %in% names(table_data) && is.numeric(table_data$cai_value)) {
            decline_label <- get_lang_text("cai_status.declining", "漸趨靜止")
            stable_label <- get_lang_text("cai_status.stable", "穩定")
            grow_label <- get_lang_text("cai_status.growing", "漸趨活躍")
            table_data$cai_value <- factor(
              ifelse(table_data$cai_value < -0.33, decline_label,
                ifelse(table_data$cai_value > 0.33, grow_label, stable_label)
              ),
              levels = c(decline_label, stable_label, grow_label)
            )
          }
        } else {
          for (col in numeric_cols) {
            if (is.numeric(table_data[[col]])) {
              table_data[[col]] <- round(table_data[[col]], 2)
            }
          }
        }

        rename_map <- c(
          customer_id = get_lang_text("table.columns.customer_id", "顧客ID"),
          r_value = get_lang_text("table.columns.r_value", "最近購買日"),
          f_value = get_lang_text("table.columns.f_value", "購買頻率"),
          m_value = get_lang_text("table.columns.m_value", "購買金額"),
          times = get_lang_text("table.columns.purchase_count", "購買次數"),
          ipt_mean = get_lang_text("table.columns.ipt_mean", "購買週期"),
          cai_value = get_lang_text("table.columns.cai_value", "顧客活躍度"),
          pcv = get_lang_text("table.columns.pcv", "過去價值"),
          cri = get_lang_text("table.columns.cri", "顧客穩定度"),
          be2 = get_lang_text("table.columns.be2", "顧客交易穩定度(be2)"),
          nes_status = get_lang_text("table.columns.nes_status", "顧客狀態"),
          clv = get_lang_text("table.columns.clv", "終身價值")
        )

        display_order <- intersect(names(rename_map), names(table_data))
        table_data <- table_data[, display_order, drop = FALSE]
        names(table_data) <- rename_map[display_order]

        DT::datatable(table_data, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
      }, error = function(e) {
        error_label <- get_lang_text("modules.vitalsigns_dna.table.errors.table_generation_error", "表格生成錯誤")
        DT::datatable(data.frame(Error = paste(error_label, ":", e$message)),
          options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
      })
    })
    
    # Summary Table
    output$summary_table <- renderDT({
      req(lang_texts())  # React to language changes
      tryCatch({
        data <- get_active_dna_data()
        req(data)

        # 確保數據是數據框格式
        if (!is.data.frame(data)) {
          if (is.list(data)) {
            data <- as.data.frame(data)
          } else {
            error_msg <- get_lang_text("errors.invalid_data_format", "數據格式錯誤：不是有效的二維數據結構")
            return(DT::datatable(data.frame(Error = error_msg),
                                 options = list(dom = 't'), rownames = FALSE))
          }
        }

        # 檢查數據是否為空
        if (nrow(data) == 0) {
          error_msg <- get_lang_text("errors.no_data_to_display", "沒有客戶數據可顯示")
          return(DT::datatable(data.frame(Error = error_msg),
                               options = list(dom = 't'), rownames = FALSE))
        }
        
        # 檢查必要欄位是否存在
        required_cols <- c("r_value", "f_value", "m_value", "clv")
        missing_cols <- setdiff(required_cols, names(data))
        
        if (length(missing_cols) > 0) {
          return(DT::datatable(data.frame(
            Error = sprintf(
              get_lang_text("messages.error.missing_required_fields", "缺少必要欄位: %s"),
              paste(missing_cols, collapse = ", ")
            )
          ), options = list(dom = 't'), rownames = FALSE))
        }

        summary_stats <- data.frame(
          Metric = c(
            get_lang_text("statistics.total_customers", "客戶總數"),
            get_lang_text("statistics.avg_r_value", "平均R值"),
            get_lang_text("statistics.avg_f_value", "平均F值"),
            get_lang_text("statistics.avg_m_value", "平均M值"),
            get_lang_text("statistics.avg_clv", "平均CLV")
          ),
          Value = c(
            nrow(data),
            round(mean(data$r_value, na.rm = TRUE), 2),
            round(mean(data$f_value, na.rm = TRUE), 2),
            round(mean(data$m_value, na.rm = TRUE), 2),
            round(mean(data$clv, na.rm = TRUE), 2)
          )
        )
        
        DT::datatable(summary_stats, options = list(dom = 't'), rownames = FALSE)
        
      }, error = function(e) {
        error_label <- get_lang_text("modules.vitalsigns_dna.table.errors.summary_statistics_error", "統計摘要錯誤")
        DT::datatable(data.frame(Error = paste(error_label, ":", e$message)),
                      options = list(dom = 't'), rownames = FALSE)
      })
    })
    
    # DNA Explanation from MD file
    output$dna_explanation <- renderUI({
      req(lang_texts())  # React to language changes
      tryCatch({
        # Get current language using the same function as other parts of the module
        current_lang <- current_language()

        # Use directory-based approach for language-specific markdown
        # Map language codes (zh_TW, en_US, ja_JP) to language directories
        lang_dir <- if(is.null(current_lang) || current_lang %in% c("chinese", "zh_TW")) {
          "chinese"
        } else if(current_lang %in% c("english", "en_US")) {
          "english"
        } else if(current_lang %in% c("japanese", "ja_JP")) {
          "japanese"
        } else {
          "chinese"  # fallback to Chinese
        }

        # All languages use the same filename in their respective directories
        # Following database/content/{language}/markdown/ structure
        md_path <- file.path("database", "content", lang_dir, "markdown", "dna_results_explanation.md")

        # Debug output
        cat("📄 Loading markdown:\n")
        cat("  Current language:", current_lang, "\n")
        cat("  Language directory:", lang_dir, "\n")
        cat("  Markdown path:", md_path, "\n")
        cat("  File exists:", file.exists(md_path), "\n")

        if (file.exists(md_path)) {
          md_content <- readLines(md_path, encoding = "UTF-8")

          # Process line by line for better control
          html_lines <- c()
          in_list <- FALSE
          in_nested_list <- FALSE

          for (line in md_content) {
            # Handle horizontal rules first
            if (grepl("^---+$", line) || grepl("^\\*\\*\\*+$", line)) {
              if (in_nested_list) { html_lines <- c(html_lines, "</ul>"); in_nested_list <- FALSE }
              if (in_list) { html_lines <- c(html_lines, "</ul>"); in_list <- FALSE }
              html_lines <- c(html_lines, "<hr style='margin: 20px 0; border: none; border-top: 2px solid #e0e0e0;'>")
            # Handle headers - IMPORTANT: Check longer patterns first (#### before ### before ## before #)
            } else if (grepl("^#### ", line)) {
              if (in_nested_list) { html_lines <- c(html_lines, "</ul>"); in_nested_list <- FALSE }
              if (in_list) { html_lines <- c(html_lines, "</ul>"); in_list <- FALSE }
              header_text <- gsub("^#### ", "", line)
              html_lines <- c(html_lines, paste0("<h5 style='color: #2c3e50; margin-top: 15px;'>", header_text, "</h5>"))
            } else if (grepl("^### ", line)) {
              if (in_nested_list) { html_lines <- c(html_lines, "</ul>"); in_nested_list <- FALSE }
              if (in_list) { html_lines <- c(html_lines, "</ul>"); in_list <- FALSE }
              header_text <- gsub("^### ", "", line)
              html_lines <- c(html_lines, paste0("<h4 style='color: #2c3e50; margin-top: 20px;'>", header_text, "</h4>"))
            } else if (grepl("^## ", line)) {
              if (in_nested_list) { html_lines <- c(html_lines, "</ul>"); in_nested_list <- FALSE }
              if (in_list) { html_lines <- c(html_lines, "</ul>"); in_list <- FALSE }
              header_text <- gsub("^## ", "", line)
              html_lines <- c(html_lines, paste0("<h3 style='color: #34495e; margin-top: 25px;'>", header_text, "</h3>"))
            } else if (grepl("^# ", line)) {
              if (in_nested_list) { html_lines <- c(html_lines, "</ul>"); in_nested_list <- FALSE }
              if (in_list) { html_lines <- c(html_lines, "</ul>"); in_list <- FALSE }
              header_text <- gsub("^# ", "", line)
              html_lines <- c(html_lines, paste0("<h2 style='color: #2c3e50; margin-top: 30px; font-weight: bold;'>", header_text, "</h2>"))
            } else if (grepl("^  - ", line)) {
              # Handle nested list items (2 spaces indent)
              if (!in_list) {
                html_lines <- c(html_lines, "<ul style='margin: 10px 0; padding-left: 20px;'>")
                in_list <- TRUE
              }
              if (!in_nested_list) {
                html_lines <- c(html_lines, "<ul style='margin: 5px 0; padding-left: 20px;'>")
                in_nested_list <- TRUE
              }
              list_text <- gsub("^  - ", "", line)
              # Process bold text within list items
              list_text <- gsub("\\*\\*(.*?)\\*\\*", "<strong>\\1</strong>", list_text)
              html_lines <- c(html_lines, paste0("<li style='margin: 3px 0;'>", list_text, "</li>"))
            } else if (grepl("^- ", line)) {
              # Handle top-level list items
              if (in_nested_list) { html_lines <- c(html_lines, "</ul>"); in_nested_list <- FALSE }
              if (!in_list) {
                html_lines <- c(html_lines, "<ul style='margin: 10px 0; padding-left: 20px;'>")
                in_list <- TRUE
              }
              list_text <- gsub("^- ", "", line)
              # Process bold text within list items
              list_text <- gsub("\\*\\*(.*?)\\*\\*", "<strong>\\1</strong>", list_text)
              html_lines <- c(html_lines, paste0("<li style='margin: 5px 0;'>", list_text, "</li>"))
            } else if (grepl("^[0-9]+\\.", line)) {
              # Handle numbered lists
              if (in_nested_list) { html_lines <- c(html_lines, "</ul>"); in_nested_list <- FALSE }
              if (in_list) { html_lines <- c(html_lines, "</ul>"); in_list <- FALSE }
              # Extract number and text
              number <- sub("^([0-9]+)\\..*", "\\1", line)
              text <- sub("^[0-9]+\\.\\s*", "", line)
              # Process bold text
              text <- gsub("\\*\\*(.*?)\\*\\*", "<strong>\\1</strong>", text)
              html_lines <- c(html_lines, paste0("<p style='margin: 8px 0;'><strong>", number, ".</strong> ", text, "</p>"))
            } else if (line == "") {
              # Handle empty lines
              if (in_nested_list) { html_lines <- c(html_lines, "</ul>"); in_nested_list <- FALSE }
              if (in_list) { html_lines <- c(html_lines, "</ul>"); in_list <- FALSE }
              html_lines <- c(html_lines, "<br>")
            } else {
              # Handle regular paragraphs
              if (in_nested_list) { html_lines <- c(html_lines, "</ul>"); in_nested_list <- FALSE }
              if (in_list) { html_lines <- c(html_lines, "</ul>"); in_list <- FALSE }
              if (line != "") {
                # Process bold text
                processed_line <- gsub("\\*\\*(.*?)\\*\\*", "<strong>\\1</strong>", line)
                html_lines <- c(html_lines, paste0("<p style='margin: 10px 0; text-align: justify;'>", processed_line, "</p>"))
              }
            }
          }

          # Close any remaining lists
          if (in_nested_list) {
            html_lines <- c(html_lines, "</ul>")
          }
          if (in_list) {
            html_lines <- c(html_lines, "</ul>")
          }
          
          # Combine all lines
          html_content <- paste(html_lines, collapse = "\n")
          
          # Add overall styling
          styled_content <- paste0(
            "<div style='font-family: system-ui, -apple-system, sans-serif; line-height: 1.6; color: #2c3e50; max-width: 100%; padding: 20px; background: #f8f9fa; border-radius: 8px;'>",
            html_content,
            "</div>"
          )

          return(HTML(styled_content))
        } else {
          error_title <- get_lang_text("errors.file_not_found", "錯誤")
          error_msg <- get_lang_text("errors.explanation_file_missing", "找不到說明文件")
          return(HTML(paste0("<div style='color: red; padding: 20px; background: #fff5f5; border: 1px solid #fed7d7; border-radius: 8px;'><strong>", error_title, ":</strong> ", error_msg, ": ", md_path, "</div>")))
        }
      }, error = function(e) {
        load_error <- get_lang_text("errors.file_load_error", "載入錯誤")
        return(HTML(paste0("<div style='color: red; padding: 20px; background: #fff5f5; border: 1px solid #fed7d7; border-radius: 8px;'><strong>", load_error, ":</strong> ", e$message, "</div>")))
      })
    })
    
    # 新增：AI分析洞察生成函數
    generate_ai_insights <- function(data) {
      insights <- list()
      
      # 1. 購買金額分析
      m_quantiles <- quantile(data$m_value, c(0.25, 0.5, 0.75), na.rm = TRUE)
      m_mean <- mean(data$m_value, na.rm = TRUE)

      monetary_assessment <- if(m_mean > m_quantiles[3]) {
        get_lang_text("ai_insights_templates.monetary.assessment_high_variance", "顧客群體消費能力較強，但存在較大差異")
      } else if(m_mean < m_quantiles[1]) {
        get_lang_text("ai_insights_templates.monetary.assessment_conservative", "顧客群體消費較為保守")
      } else {
        get_lang_text("ai_insights_templates.monetary.assessment_balanced", "顧客消費較為均衡")
      }

      insights$monetary <- sprintf(
        "%s\n        • %s\n        • %s\n        • %s\n        • %s\n        %s%s",
        get_lang_text("ai_insights_templates.monetary.title", "購買金額分析："),
        sprintf(get_lang_text("ai_insights_templates.monetary.median_text", "中位數購買金額為 %.2f 元，表示50%%的顧客單次消費低於此金額"), m_quantiles[2]),
        sprintf(get_lang_text("ai_insights_templates.monetary.q25_text", "25%%的顧客單次消費低於 %.2f 元"), m_quantiles[1]),
        sprintf(get_lang_text("ai_insights_templates.monetary.q75_text", "25%%的顧客單次消費高於 %.2f 元"), m_quantiles[3]),
        sprintf(get_lang_text("ai_insights_templates.monetary.mean_text", "平均單次消費為 %.2f 元"), m_mean),
        get_lang_text("ai_insights_templates.monetary.overall_assessment", "整體評估："),
        monetary_assessment
      )
      
      # 2. 最近購買時間分析
      r_quantiles <- quantile(data$r_value, c(0.25, 0.5, 0.75), na.rm = TRUE)
      r_mean <- mean(data$r_value, na.rm = TRUE)

      recency_assessment <- if(r_mean > 180) {
        get_lang_text("ai_insights_templates.recency.assessment_needs_retention", "需要加強客戶維繫，考慮召回活動")
      } else if(r_mean < 30) {
        get_lang_text("ai_insights_templates.recency.assessment_highly_active", "客戶活躍度高，可考慮提升客單價")
      } else {
        get_lang_text("ai_insights_templates.recency.assessment_moderate", "客戶活躍度適中")
      }

      insights$recency <- sprintf(
        "%s\n        • %s\n        • %s\n        • %s\n        • %s\n        %s%s",
        get_lang_text("ai_insights_templates.recency.title", "最近購買時間分析："),
        sprintf(get_lang_text("ai_insights_templates.recency.median_text", "中位數距離上次購買時間為 %.0f 天"), r_quantiles[2]),
        sprintf(get_lang_text("ai_insights_templates.recency.q25_text", "25%%的顧客在 %.0f 天內有購買"), r_quantiles[1]),
        sprintf(get_lang_text("ai_insights_templates.recency.q75_text", "25%%的顧客超過 %.0f 天未購買"), r_quantiles[3]),
        sprintf(get_lang_text("ai_insights_templates.recency.mean_text", "平均未購買天數為 %.0f 天"), r_mean),
        get_lang_text("ai_insights_templates.recency.overall_assessment", "整體評估："),
        recency_assessment
      )
      
      # 3. 購買頻率分析
      f_quantiles <- quantile(data$f_value, c(0.25, 0.5, 0.75), na.rm = TRUE)
      f_mean <- mean(data$f_value, na.rm = TRUE)

      frequency_assessment <- if(f_mean > f_quantiles[3]) {
        get_lang_text("ai_insights_templates.frequency.assessment_stable_high_freq", "有穩定的高頻購買客群")
      } else if(f_mean < f_quantiles[1]) {
        get_lang_text("ai_insights_templates.frequency.assessment_occasional", "多為偶發性購買，需加強會員經營")
      } else {
        get_lang_text("ai_insights_templates.frequency.assessment_average", "購買頻率分布平均")
      }

      insights$frequency <- sprintf(
        "%s\n        • %s\n        • %s\n        • %s\n        • %s\n        %s%s",
        get_lang_text("ai_insights_templates.frequency.title", "購買頻率分析："),
        sprintf(get_lang_text("ai_insights_templates.frequency.median_text", "中位數購買次數為 %.0f 次"), f_quantiles[2]),
        sprintf(get_lang_text("ai_insights_templates.frequency.q25_text", "25%%的顧客購買次數少於 %.0f 次"), f_quantiles[1]),
        sprintf(get_lang_text("ai_insights_templates.frequency.q75_text", "25%%的顧客購買次數多於 %.0f 次"), f_quantiles[3]),
        sprintf(get_lang_text("ai_insights_templates.frequency.mean_text", "平均購買次數為 %.1f 次"), f_mean),
        get_lang_text("ai_insights_templates.frequency.overall_assessment", "整體評估："),
        frequency_assessment
      )
      
      # 4. 顧客狀態分析
      if (!"nes_status" %in% names(data)) {
        insights$nes <- get_lang_text("ai_deep_analysis.nes_distribution.no_data", "顧客狀態資料不足，請先完成DNA分析")
      } else {
        nes_table <- table(data$nes_status)
        nes_counts <- c(N = 0, E0 = 0, S1 = 0, S2 = 0, S3 = 0)
        if (length(nes_table) > 0) {
          nes_counts[names(nes_table)] <- as.numeric(nes_table)
        }
        total_nes <- sum(nes_counts)

        if (total_nes == 0) {
          status_summary <- get_lang_text("ai_deep_analysis.nes_distribution.empty", "尚無顧客狀態可統計")
          assessment_text <- get_lang_text("ai_deep_analysis.nes_distribution.assessment_average", "客戶參與度分布平均")
        } else {
          nes_pct <- round(nes_counts / total_nes * 100, 1)
          status_summary <- paste(
            names(nes_counts),
            sprintf("%.1f%%", nes_pct),
            sep = ": ",
            collapse = "\n• "
          )

          # Get language-aware assessment text (避免 NA 比較)
          s2s3_pct <- sum(nes_pct[c("S2", "S3")], na.rm = TRUE)
          e0_pct <- nes_pct[["E0"]]
          assessment_text <- if (!is.na(s2s3_pct) && s2s3_pct > 50) {
            get_lang_text("ai_deep_analysis.nes_distribution.assessment_high", "高度參與客群占比較大，可開發深度服務")
          } else if (!is.na(e0_pct) && e0_pct > 40) {
            get_lang_text("ai_deep_analysis.nes_distribution.assessment_low", "需要加強客戶參與度，建議開發入門級服務")
          } else {
            get_lang_text("ai_deep_analysis.nes_distribution.assessment_average", "客戶參與度分布平均")
          }
        }

        insights$nes <- sprintf(
          "%s\n• %s\n%s%s",
          paste0(get_lang_text("ai_deep_analysis.nes_distribution.title", "顧客狀態分布"), "："),
          status_summary,
          get_lang_text("ai_deep_analysis.nes_distribution.overall_assessment_label", "整體評估："),
          assessment_text
        )
      }
      
      insights
    }
    
    # 執行客群分組
    performSegmentation <- function() {
      # 檢查必要條件
      if(is.null(values$dna_results)) {
        showNotification(
          get_lang_text("messages.warning.no_dna_analysis", "⚠️ 請先執行 DNA 分析"),
          type = "warning", duration = 3)
        return()
      }
      if(is.null(values$current_metric)) {
        showNotification(
          get_lang_text("messages.warning.no_metric_selected", "⚠️ 請選擇分析指標"),
          type = "warning", duration = 3)
        return()
      }

      data <- values$dna_results$data_by_customer
      metric <- values$current_metric

      # 顯示分組進行中的訊息
      showNotification(
        sprintf(get_lang_text("messages.processing.segmenting_metric", "📊 正在進行 %s 指標的客群分組..."), metric),
        type = "message", duration = 2)
      
      # 確保必要的欄位存在
      if(!"cai_value" %in% names(data) && "cai" %in% names(data)) {
        data$cai_value <- data$cai
      }
      if(!"pcv" %in% names(data) && "total_spent" %in% names(data)) {
        data$pcv <- data$total_spent
      }
      
      # 如果缺少CAI值，計算簡單的活躍度
      if(!"cai_value" %in% names(data) && !"cai" %in% names(data)) {
        if("r_value" %in% names(data)) {
          # 基於最近購買時間計算活躍度
          data$cai_value <- ifelse(
            data$r_value < 30, 1,  # 30天內購買：活躍
            ifelse(data$r_value > 90, -1, 0)  # 90天以上：靜止，30-90天：穩定
          )
        }
      }
      
      # 如果缺少CRI，計算它
      if(!"cri" %in% names(data) && all(c("r_value", "f_value", "m_value") %in% names(data))) {
        r_norm <- 1 - normalize_01(data$r_value)  # R值反向
        f_norm <- normalize_01(data$f_value)
        m_norm <- normalize_01(data$m_value)
        
        data$cri <- 0.3 * r_norm + 0.3 * f_norm + 0.4 * m_norm
      }
      
      # 根據選定的指標取得資料欄位
      metric_col <- switch(metric,
        "M" = "m_value",
        "R" = "r_value",
        "F" = "f_value",
        "CAI" = if("cai_value" %in% names(data)) "cai_value" else "cai",
        "PCV" = if("pcv" %in% names(data)) "pcv" else "total_spent",
        "CRI" = "cri",
        "NES" = "nes_status"
      )
      
      # 檢查欄位是否存在
      if(!metric_col %in% names(data)) {
        showNotification(
          sprintf(get_lang_text("messages.warning.metric_not_found", "⚠️ 找不到 %s 的資料欄位"), metric),
          type = "warning", duration = 3)
        return()
      }
      
      if(metric_col %in% names(data) && metric != "NES") {
        values_vec <- data[[metric_col]]
        values_vec[is.na(values_vec)] <- median(values_vec, na.rm = TRUE)
        
        # 使用80/20法則進行分組
        q20 <- quantile(values_vec, 0.2, na.rm = TRUE)
        q80 <- quantile(values_vec, 0.8, na.rm = TRUE)
        
        # 對於R值（最近來店時間），數值越小越好 (language-aware labels)
        if(metric == "R") {
          data$segment <- ifelse(values_vec <= q20, get_lang_text("segment_labels.recency.high_active", "高活躍"),
                                ifelse(values_vec >= q80, get_lang_text("segment_labels.recency.churned", "低活躍"),
                                       get_lang_text("segment_labels.recency.general", "中活躍")))
        } else if(metric == "CAI") {
          # 顧客活躍度特殊處理
          data$segment <- ifelse(values_vec > 0, get_lang_text("segment_labels.cai.trending_active", "漸趨活躍"),
                                ifelse(values_vec < -0.1, get_lang_text("segment_labels.cai.trending_static", "漸趨靜止"),
                                       get_lang_text("segment_labels.cai.stable", "穩定")))
        } else {
          # 其他指標，數值越大越好
          data$segment <- ifelse(values_vec >= q80, get_lang_text("segment_labels.monetary.high_value", "高價值"),
                                ifelse(values_vec <= q20, get_lang_text("segment_labels.monetary.low_value", "低價值"),
                                       get_lang_text("segment_labels.monetary.mid_value", "中價值")))
        }

        # 儲存分組結果（包含新增的欄位）
        values$segmented_data <- data
        values$dna_results$data_by_customer <- data  # 更新原始資料
        values$current_segmentation <- list(
          metric = metric,
          metric_col = metric_col,
          q20 = q20,
          q80 = q80,
          timestamp = Sys.time()
        )
        
        # 顯示分組完成訊息
        showNotification(
          sprintf(get_lang_text("messages.success.segmentation_complete", "✅ %s 客群分組完成！"), metric),
          type = "message", duration = 2)

        # Store display data (column names handled by language system)
        values$temp_chinese_table <- data

        # 自動生成預設行銷建議（不需要AI）
        generateDefaultRecommendations()

      } else if(metric == "NES" && "nes_status" %in% names(data)) {
        # NES特殊處理
        data$segment <- data$nes_status
        values$segmented_data <- data
        values$dna_results$data_by_customer <- data  # 更新原始資料
        values$current_segmentation <- list(
          metric = metric,
          metric_col = "nes_status",
          timestamp = Sys.time()
        )

        # 顯示分組完成訊息
        showNotification(
          sprintf(get_lang_text("messages.success.segmentation_complete", "✅ %s 客群分組完成！"), metric),
          type = "message", duration = 2)
        
        # Store display data (column names handled by language system)
        values$temp_chinese_table <- data
        
        # 自動生成預設行銷建議（不需要AI）
        generateDefaultRecommendations()
      }
    }
    
    # 執行完整的客戶分群分析
    performCompleteSegmentation <- function() {
      tryCatch({
        if(is.null(values$dna_results)) {
          showNotification(
            get_lang_text("messages.warning.no_dna_analysis", "⚠️ 請先執行 DNA 分析"),
            type = "warning", duration = 3)
          return()
        }
        data <- get_active_dna_data()
        if (is.null(data)) {
          showNotification(
            get_lang_text("messages.warning.no_dna_analysis", "⚠️ 請先執行 DNA 分析"),
            type = "warning", duration = 3)
          return()
        }
        
        # 設定分群已完成標記
        values$segmentation_completed <- TRUE
      
      # 確保必要的欄位存在
      if(!"cai_value" %in% names(data) && "cai" %in% names(data)) {
        data$cai_value <- data$cai
      }
      if(!"pcv" %in% names(data) && "total_spent" %in% names(data)) {
        data$pcv <- data$total_spent
      }
      
      # 如果缺少CAI值，計算簡單的活躍度
      if(!"cai_value" %in% names(data) && !"cai" %in% names(data)) {
        if("r_value" %in% names(data)) {
          data$cai_value <- ifelse(
            data$r_value < 30, 1,  # 30天內購買：活躍
            ifelse(data$r_value > 90, -1, 0)  # 90天以上：靜止，30-90天：穩定
          )
        }
      }
      
      # 如果缺少CRI，計算它
      if(!"cri" %in% names(data) && all(c("r_value", "f_value", "m_value") %in% names(data))) {
        r_norm <- 1 - normalize_01(data$r_value)  # R值反向
        f_norm <- normalize_01(data$f_value)
        m_norm <- normalize_01(data$m_value)
        
        data$cri <- 0.3 * r_norm + 0.3 * f_norm + 0.4 * m_norm
      }
      
      # 為每個指標計算分群
      # R值分群
      if("r_value" %in% names(data) && !all(is.na(data$r_value))) {
        r_values <- data$r_value[!is.na(data$r_value)]
        if(length(unique(r_values)) > 2) {
          r_quantiles <- quantile(r_values, c(0.33, 0.67))
          # 確保 breaks 是唯一的
          if(r_quantiles[1] == r_quantiles[2]) {
            # FIXED 2024-12-28: 當分位數相同時，使用 rank-based 20/60/20 分類
            high_label <- get_lang_text("segment_labels.recency.high_active", "高活躍")
            mid_label <- get_lang_text("segment_labels.recency.general", "中活躍")
            low_label <- get_lang_text("segment_labels.recency.churned", "低活躍")

            n <- nrow(data)
            rank_values <- rank(data$r_value, ties.method = "first", na.last = "keep")
            # R值越小越活躍，所以 rank 越小 = 高活躍
            data$segment_r <- dplyr::case_when(
              is.na(data$r_value) ~ NA_character_,
              rank_values <= n * 0.2 ~ high_label,
              rank_values > n * 0.8 ~ low_label,
              TRUE ~ mid_label
            )
            data$segment_r <- factor(data$segment_r, levels = c(high_label, mid_label, low_label))
          } else {
            high_label <- get_lang_text("segment_labels.recency.high_active", "高活躍")
            mid_label <- get_lang_text("segment_labels.recency.general", "中活躍")
            low_label <- get_lang_text("segment_labels.recency.churned", "低活躍")
            data$segment_r <- cut(data$r_value,
                                  breaks = c(-Inf, r_quantiles[1], r_quantiles[2], Inf),
                                  labels = c(high_label, mid_label, low_label),
                                  include.lowest = TRUE)
          }
        } else {
          # 值太少，全部歸為一組
          high_label <- get_lang_text("segment_labels.recency.high_active", "高活躍")
          mid_label <- get_lang_text("segment_labels.recency.general", "中活躍")
          low_label <- get_lang_text("segment_labels.recency.churned", "低活躍")
          data$segment_r <- factor(mid_label, levels = c(high_label, mid_label, low_label))
        }
      }
      
      # F值分群
      if("f_value" %in% names(data) && !all(is.na(data$f_value))) {
        f_values <- data$f_value[!is.na(data$f_value)]
        if(length(unique(f_values)) > 2) {
          f_quantiles <- quantile(f_values, c(0.33, 0.67))
          # 確保 breaks 是唯一的
          if(f_quantiles[1] == f_quantiles[2]) {
            # FIXED 2024-12-28: 當分位數相同時，使用 rank-based 20/60/20 分類
            low_label <- get_lang_text("segment_labels.frequency.low_frequency", "低頻率")
            mid_label <- get_lang_text("segment_labels.frequency.mid_frequency", "中頻率")
            high_label <- get_lang_text("segment_labels.frequency.high_frequency", "高頻率")

            n <- nrow(data)
            rank_values <- rank(data$f_value, ties.method = "first", na.last = "keep")
            # F值越大越頻繁，所以 rank 越大 = 高頻率
            data$segment_f <- dplyr::case_when(
              is.na(data$f_value) ~ NA_character_,
              rank_values <= n * 0.2 ~ low_label,
              rank_values > n * 0.8 ~ high_label,
              TRUE ~ mid_label
            )
            data$segment_f <- factor(data$segment_f, levels = c(low_label, mid_label, high_label))
          } else {
            low_label <- get_lang_text("segment_labels.frequency.low_frequency", "低頻率")
            mid_label <- get_lang_text("segment_labels.frequency.mid_frequency", "中頻率")
            high_label <- get_lang_text("segment_labels.frequency.high_frequency", "高頻率")
            data$segment_f <- cut(data$f_value,
                                  breaks = c(-Inf, f_quantiles[1], f_quantiles[2], Inf),
                                  labels = c(low_label, mid_label, high_label),
                                  include.lowest = TRUE)
          }
        } else {
          # 值太少，全部歸為一組
          low_label <- get_lang_text("segment_labels.frequency.low_frequency", "低頻率")
          mid_label <- get_lang_text("segment_labels.frequency.mid_frequency", "中頻率")
          high_label <- get_lang_text("segment_labels.frequency.high_frequency", "高頻率")
          data$segment_f <- factor(mid_label, levels = c(low_label, mid_label, high_label))
        }
      }
      
      # M值分群
      if("m_value" %in% names(data) && !all(is.na(data$m_value))) {
        m_values <- data$m_value[!is.na(data$m_value)]
        if(length(unique(m_values)) > 2) {
          m_quantiles <- quantile(m_values, c(0.33, 0.67))
          # 確保 breaks 是唯一的
          if(m_quantiles[1] == m_quantiles[2]) {
            # FIXED 2024-12-28: 當分位數相同時，使用 rank-based 20/60/20 分類
            low_label <- get_lang_text("segment_labels.monetary.low_value", "低價值")
            mid_label <- get_lang_text("segment_labels.monetary.mid_value", "中價值")
            high_label <- get_lang_text("segment_labels.monetary.high_value", "高價值")

            n <- nrow(data)
            rank_values <- rank(data$m_value, ties.method = "first", na.last = "keep")
            data$segment_m <- dplyr::case_when(
              is.na(data$m_value) ~ NA_character_,
              rank_values <= n * 0.2 ~ low_label,
              rank_values > n * 0.8 ~ high_label,
              TRUE ~ mid_label
            )
            data$segment_m <- factor(data$segment_m, levels = c(low_label, mid_label, high_label))
          } else {
            low_label <- get_lang_text("segment_labels.monetary.low_value", "低價值")
            mid_label <- get_lang_text("segment_labels.monetary.mid_value", "中價值")
            high_label <- get_lang_text("segment_labels.monetary.high_value", "高價值")
            data$segment_m <- cut(data$m_value,
                                  breaks = c(-Inf, m_quantiles[1], m_quantiles[2], Inf),
                                  labels = c(low_label, mid_label, high_label),
                                  include.lowest = TRUE)
          }
        } else {
          # 值太少，全部歸為一組
          low_label <- get_lang_text("segment_labels.monetary.low_value", "低價值")
          mid_label <- get_lang_text("segment_labels.monetary.mid_value", "中價值")
          high_label <- get_lang_text("segment_labels.monetary.high_value", "高價值")
          data$segment_m <- factor(mid_label, levels = c(low_label, mid_label, high_label))
        }
      }
      
      # IPT分群（購買週期）
      if("ipt_mean" %in% names(data) && !all(is.na(data$ipt_mean))) {
        ipt_values <- data$ipt_mean[!is.na(data$ipt_mean)]
        if(length(unique(ipt_values)) > 2) {
          ipt_quantiles <- quantile(ipt_values, c(0.33, 0.67))
          # 確保 breaks 是唯一的
          if(ipt_quantiles[1] == ipt_quantiles[2]) {
            # FIXED 2024-12-28: 當分位數相同時，使用 rank-based 20/60/20 分類
            short_label <- get_lang_text("segment_labels.ipt.short_cycle", "短週期")
            mid_label <- get_lang_text("segment_labels.ipt.mid_cycle", "中週期")
            long_label <- get_lang_text("segment_labels.ipt.long_cycle", "長週期")

            n <- nrow(data)
            rank_values <- rank(data$ipt_mean, ties.method = "first", na.last = "keep")
            # IPT越小代表購買週期短，所以 rank 越小 = 短週期
            data$segment_ipt <- dplyr::case_when(
              is.na(data$ipt_mean) ~ NA_character_,
              rank_values <= n * 0.2 ~ short_label,
              rank_values > n * 0.8 ~ long_label,
              TRUE ~ mid_label
            )
            data$segment_ipt <- factor(data$segment_ipt, levels = c(short_label, mid_label, long_label))
          } else {
            # 注意：IPT越小代表購買頻繁，所以標籤順序與數值相反
            short_label <- get_lang_text("segment_labels.ipt.short_cycle", "短週期")
            mid_label <- get_lang_text("segment_labels.ipt.mid_cycle", "中週期")
            long_label <- get_lang_text("segment_labels.ipt.long_cycle", "長週期")
            data$segment_ipt <- cut(data$ipt_mean,
                                   breaks = c(-Inf, ipt_quantiles[1], ipt_quantiles[2], Inf),
                                   labels = c(short_label, mid_label, long_label),
                                   include.lowest = TRUE)
          }
        } else {
          # 值太少，全部歸為一組
          short_label <- get_lang_text("segment_labels.ipt.short_cycle", "短週期")
          mid_label <- get_lang_text("segment_labels.ipt.mid_cycle", "中週期")
          long_label <- get_lang_text("segment_labels.ipt.long_cycle", "長週期")
          data$segment_ipt <- factor(mid_label, levels = c(short_label, mid_label, long_label))
        }
      }
      
      # CAI分群
      if("cai_value" %in% names(data) && !all(is.na(data$cai_value))) {
        cai_values <- data$cai_value[!is.na(data$cai_value)]
        if(length(unique(cai_values)) > 1) {
          # CAI 值通常在 -1 到 1 之間 (language-aware)
          static_label <- get_lang_text("segment_labels.cai.trending_static", "漸趨靜止")
          stable_label <- get_lang_text("segment_labels.cai.stable", "穩定")
          active_label <- get_lang_text("segment_labels.cai.trending_active", "漸趨活躍")
          data$segment_cai <- cut(data$cai_value,
                                  breaks = c(-Inf, -0.33, 0.33, Inf),
                                  labels = c(static_label, stable_label, active_label),
                                  include.lowest = TRUE)
        } else {
          # 值太少，全部歸為穩定
          static_label <- get_lang_text("segment_labels.cai.trending_static", "漸趨靜止")
          stable_label <- get_lang_text("segment_labels.cai.stable", "穩定")
          active_label <- get_lang_text("segment_labels.cai.trending_active", "漸趨活躍")
          data$segment_cai <- factor(stable_label, levels = c(static_label, stable_label, active_label))
        }
      }
      
      # PCV分群（80/20三類 + 五等分）
      if("pcv" %in% names(data) && !all(is.na(data$pcv))) {
        pcv_values <- data$pcv[!is.na(data$pcv)]
        if(length(unique(pcv_values)) > 1) {
          high_label <- get_lang_text("segment_labels.monetary.high_value", "高價值")
          mid_label <- get_lang_text("segment_labels.monetary.mid_value", "中價值")
          low_label <- get_lang_text("segment_labels.monetary.low_value", "低價值")
          pareto_q <- quantile(pcv_values, c(0.2, 0.8))

          if(pareto_q[1] == pareto_q[2]) {
            n <- nrow(data)
            rank_values <- rank(data$pcv, ties.method = "first", na.last = "keep")
            data$segment_pcv <- dplyr::case_when(
              is.na(data$pcv) ~ NA_character_,
              rank_values <= n * 0.2 ~ low_label,
              rank_values > n * 0.8 ~ high_label,
              TRUE ~ mid_label
            )
          } else {
            data$segment_pcv <- cut(
              data$pcv,
              breaks = c(-Inf, pareto_q[1], pareto_q[2], Inf),
              labels = c(low_label, mid_label, high_label),
              include.lowest = TRUE
            )
          }

          quintile_breaks <- quantile(pcv_values, probs = seq(0, 1, 0.2))
          quintile_labels <- c(
            get_lang_text("segment_labels.quintile.q1", "最低20%"),
            get_lang_text("segment_labels.quintile.q2", "次低20%"),
            get_lang_text("segment_labels.quintile.q3", "中間20%"),
            get_lang_text("segment_labels.quintile.q4", "次高20%"),
            get_lang_text("segment_labels.quintile.q5", "最高20%")
          )

          if(length(unique(quintile_breaks)) < 6) {
            n <- nrow(data)
            rank_values <- rank(data$pcv, ties.method = "first", na.last = "keep")
            quintile_index <- ceiling(rank_values / (n / 5))
            quintile_index[quintile_index > 5] <- 5
            data$segment_pcv_quintile <- factor(
              quintile_labels[quintile_index],
              levels = quintile_labels,
              ordered = TRUE
            )
          } else {
            data$segment_pcv_quintile <- cut(
              data$pcv,
              breaks = quintile_breaks,
              labels = quintile_labels,
              include.lowest = TRUE,
              ordered_result = TRUE
            )
          }
        } else {
          high_label <- get_lang_text("segment_labels.monetary.high_value", "高價值")
          mid_label <- get_lang_text("segment_labels.monetary.mid_value", "中價值")
          low_label <- get_lang_text("segment_labels.monetary.low_value", "低價值")
          data$segment_pcv <- factor(mid_label, levels = c(low_label, mid_label, high_label))
          data$segment_pcv_quintile <- factor(
            get_lang_text("segment_labels.quintile.q3", "中間20%"),
            levels = c(
              get_lang_text("segment_labels.quintile.q1", "最低20%"),
              get_lang_text("segment_labels.quintile.q2", "次低20%"),
              get_lang_text("segment_labels.quintile.q3", "中間20%"),
              get_lang_text("segment_labels.quintile.q4", "次高20%"),
              get_lang_text("segment_labels.quintile.q5", "最高20%")
            ),
            ordered = TRUE
          )
        }
      }
      
      # CRI分群（顧客穩定度指標分群 - 80/20法則）
      if("cri" %in% names(data) && !all(is.na(data$cri))) {
        cri_values <- data$cri[!is.na(data$cri)]
        if(length(unique(cri_values)) > 2) {
          # 使用80/20法則分群
          cri_quantiles <- quantile(cri_values, c(0.2, 0.8))
          # 確保 breaks 是唯一的
          if(cri_quantiles[1] == cri_quantiles[2]) {
            # FIXED 2024-12-28: 當分位數相同時，使用 rank-based 20/60/20 分類
            low_label <- get_lang_text("segment_labels.cri.low_stability", "低穩定(20%)")
            mid_label <- get_lang_text("segment_labels.cri.mid_stability", "中穩定(60%)")
            high_label <- get_lang_text("segment_labels.cri.high_stability", "高穩定(20%)")

            n <- nrow(data)
            rank_values <- rank(data$cri, ties.method = "first", na.last = "keep")
            # CRI越大越穩定，所以 rank 越大 = 高穩定
            data$segment_cri <- dplyr::case_when(
              is.na(data$cri) ~ mid_label,  # NA 視為中穩定
              rank_values <= n * 0.2 ~ low_label,
              rank_values > n * 0.8 ~ high_label,
              TRUE ~ mid_label
            )
            data$segment_cri <- factor(data$segment_cri, levels = c(low_label, mid_label, high_label))
          } else {
            low_label <- get_lang_text("segment_labels.cri.low_stability", "低穩定(20%)")
            mid_label <- get_lang_text("segment_labels.cri.mid_stability", "中穩定(60%)")
            high_label <- get_lang_text("segment_labels.cri.high_stability", "高穩定(20%)")
            data$segment_cri <- cut(data$cri,
                                    breaks = c(-Inf, cri_quantiles[1], cri_quantiles[2], Inf),
                                    labels = c(low_label, mid_label, high_label),
                                    include.lowest = TRUE)

            # Handle NA CRI values by assigning them to mid stability group
            data$segment_cri[is.na(data$cri)] <- mid_label
            cat("ℹ️ Assigned", sum(is.na(data$cri)), "NA CRI values to", mid_label, "\n")
          }
        } else {
          # 值太少，全部歸為一組
          low_label <- get_lang_text("segment_labels.cri.low_stability", "低穩定(20%)")
          mid_label <- get_lang_text("segment_labels.cri.mid_stability", "中穩定(60%)")
          high_label <- get_lang_text("segment_labels.cri.high_stability", "高穩定(20%)")
          data$segment_cri <- factor(mid_label, levels = c(low_label, mid_label, high_label))
        }
      }
      
      # NES分群（直接使用狀態）
      if("nes_status" %in% names(data)) {
        data$segment_nes <- data$nes_status
      }
      
      # 儲存完整的分群數據
      values$complete_segmented_data <- data
      
      # Store display data (column names handled by language system)
      values$temp_chinese_table <- data
      
      # 設定初始的分群資料（使用當前或預設M）
      metric_to_use <- values$current_metric
      if (is.null(metric_to_use)) metric_to_use <- "M"
      init_segment_col <- switch(metric_to_use,
        "R" = "segment_r",
        "F" = "segment_f",
        "M" = "segment_m",
        "IPT" = "segment_ipt",
        "CAI" = "segment_cai",
        "PCV" = "segment_pcv",
        "CRI" = "segment_cri",
        "NES" = "segment_nes",
        "segment_m"
      )

      if(init_segment_col %in% names(data)) {
        data$segment <- data[[init_segment_col]]
        values$segmented_data <- data
        values$current_metric <- metric_to_use
        updateMetricDescription(metric_to_use)
      }
      
        # 若需要提示可在此加入通知；預設關閉以避免干擾使用者
      }, error = function(e) {
        values$status_text <- paste(
          get_lang_text("messages.error.analysis_error", "❌ 分析錯誤:"),
          e$message)
        showNotification(
          paste(get_lang_text("messages.error.clustering_failed", "分群分析失敗:"), e$message),
          type = "error", duration = 5)
        values$segmentation_completed <- FALSE
      })
    }

    # 更新顯示的分群資料（不重新執行分群）
    updateDisplayForMetric <- function(metric) {
      # 如果沒有完整分群資料，嘗試自動執行一次完整分群，避免提示訊息
      if (is.null(values$complete_segmented_data) && !is.null(values$dna_results)) {
        performCompleteSegmentation()
      }
      if (is.null(values$complete_segmented_data)) {
        return(NULL)
      }
      
      data <- values$complete_segmented_data
      
      # 根據選擇的指標更新 segment 欄位
      segment_col <- switch(metric,
        "R" = "segment_r",
        "F" = "segment_f",
        "M" = "segment_m",
        "IPT" = "segment_ipt",
        "CAI" = "segment_cai",
        "PCV" = "segment_pcv",
        "CRI" = "segment_cri",
        "NES" = "segment_nes"
      )
      
      if(segment_col %in% names(data)) {
        data$segment <- data[[segment_col]]
        values$segmented_data <- data
        values$current_metric <- metric

        # Store updated display data (column names handled by language system)
        values$temp_chinese_table <- data
      }
    }
    
    
    # 生成預設行銷建議（不需要AI）
    generateDefaultRecommendations <- function() {
      req(values$segmented_data)
      
      segments <- unique(values$segmented_data$segment)
      segments <- segments[!is.na(segments)]  # Filter out NA segments
      cat("📊 Generating marketing recommendations for", length(segments), "segments:", paste(segments, collapse=", "), "\n")
      recommendations <- list()

      for(seg in segments) {
        recommendations[[seg]] <- get_default_marketing_recommendations(values$current_metric, seg)
      }
      
      values$ai_recommendations <- recommendations
      
      # 返回建議供 AI 管理器使用
      return(recommendations)
    }
    
    # 生成AI行銷建議（使用集中管理的prompt）
    generateAIRecommendations <- function() {
      req(values$segmented_data)
      metric <- values$current_metric %||% "M"

      cached <- values$ai_recommendations_cache[[metric]]
      if (!is.null(cached)) {
        return(cached)
      }
      
      # 強制清除舊的建議
      values$ai_recommendations <- NULL

      # 載入prompts (使用 vitalsigns app prompts)
      prompts_df <- load_prompts(app_name = "vitalsigns", language = current_language())

      # 根據不同客群生成建議
      segments <- unique(values$segmented_data$segment)
      segments <- segments[!is.na(segments)]  # Filter out NA segments
      cat("📊 Generating marketing recommendations for", length(segments), "segments:", paste(segments, collapse=", "), "\n")
      recommendations <- list()

      # 檢查是否有GPT API設定
      use_ai <- !is.null(prompts_df) && 
                Sys.getenv("OPENAI_API_KEY") != "" && 
                exists("execute_gpt_request") &&
                !is.null(chat_api)  # 確保chat_api存在
      
      for(seg in segments) {
        # Skip invalid segments
        if(is.na(seg) || seg == "NA" || seg == "" || trimws(seg) == "") {
          cat("⚠️ Skipping invalid segment:", seg, "\n")
          next
        }

        seg_data <- values$segmented_data[values$segmented_data$segment == seg, ]

        # Skip if no data for segment
        if(nrow(seg_data) == 0) {
          cat("⚠️ No data for segment:", seg, "- skipping\n")
          next
        }

        # 如果可以使用AI，嘗試生成AI建議（含快取）
        if(use_ai) {
          tryCatch({
            # 準備分群數據
            segment_stats <- seg_data %>%
              summarise(
                count = n(),
                avg_r = mean(r_value, na.rm = TRUE),
                avg_f = mean(f_value, na.rm = TRUE),
                avg_m = mean(m_value, na.rm = TRUE),
                total_revenue = sum(m_value * f_value, na.rm = TRUE)
              )

            # === 快取檢查（每個 segment 獨立快取）===
            segment_cache_data <- list(
              segment = seg,
              metric = values$current_metric,
              count = segment_stats$count,
              avg_r = round(segment_stats$avg_r, 2),
              avg_f = round(segment_stats$avg_f, 2),
              avg_m = round(segment_stats$avg_m, 2)
            )
            segment_cache_key <- get_ai_cache_key(
              paste0("segment_marketing_", seg),
              segment_cache_data,
              current_language()
            )
            cached_segment_result <- get_cached_ai(segment_cache_key)

            ai_result <- NULL
            if (!is.null(cached_segment_result)) {
              cat("[DNA] 使用快取的 segment 行銷建議:", seg, "\n")
              ai_result <- cached_segment_result
            } else {
              # 使用集中管理的prompt (Following R092: get_lang_text for all prompt variables)
              ai_result <- execute_gpt_request(
                var_id = "dna_segment_marketing",
                variables = list(
                  segment_name = seg,
                  segment_characteristics = paste0(
                    get_lang_text("ai_prompt_text.avg_r", "平均R"), ": ", round(segment_stats$avg_r, 1),
                    get_lang_text("ai_prompt_text.days_unit", "天"), ", ",
                    get_lang_text("ai_prompt_text.avg_f", "平均F"), ": ", round(segment_stats$avg_f, 1),
                    get_lang_text("ai_prompt_text.times_unit", "次"), ", ",
                    get_lang_text("ai_prompt_text.avg_m", "平均M"), ": $", round(segment_stats$avg_m, 2)
                  ),
                  avg_metrics = jsonlite::toJSON(segment_stats, auto_unbox = TRUE),
                  segment_size = paste0(segment_stats$count, get_lang_text("ai_prompt_text.people_unit", "人"))
                ),
                chat_api_function = chat_api,
                model = cfg_ai_model,
                prompts_df = prompts_df,
                language = current_language()
              )

              # 儲存到快取
              if (!is.null(ai_result)) {
                set_cached_ai(segment_cache_key, ai_result)
              }
            }

            # 解析AI回應（若空白則回退預設）
            ai_text <- if (!is.null(ai_result)) trimws(ai_result) else ""
            actions_clean <- ai_text %>%
              strsplit("\n") %>%
              unlist() %>%
              trimws() %>%
              .[nchar(.) > 0] %>%
              gsub("^[•\\-\\d\\.\\s]+", "", .)

            if (length(actions_clean) == 0) {
              recommendations[[seg]] <- get_default_marketing_recommendations(values$current_metric, seg)
            } else {
              recommendations[[seg]] <- list(
                strategy = actions_clean[[1]],
                actions = if (length(actions_clean) > 1) actions_clean[-1] else actions_clean
              )
            }
            
          }, error = function(e) {
            # 如果AI失敗，使用預設建議
            recommendations[[seg]] <- get_default_marketing_recommendations(values$current_metric, seg)
          })
        } else {
          # 使用預設建議
          recommendations[[seg]] <- get_default_marketing_recommendations(values$current_metric, seg)
        }
      }
      
      # 若缺少策略/行動，填入預設占位
      recommendations <- lapply(recommendations, function(rec) {
        if (is.null(rec) || is.null(rec$strategy)) {
          return(list(
            strategy = get_lang_text("marketing_recommendations.default_strategy", "請依該客群提供專屬優惠"),
            actions = c(get_lang_text("marketing_recommendations.default_action", "建立分眾 EDM 或簡訊，強調專屬優惠"))
          ))
        }
        if (is.null(rec$actions) || length(rec$actions) == 0) {
          rec$actions <- c(get_lang_text("marketing_recommendations.default_action", "建立分眾 EDM 或簡訊，強調專屬優惠"))
        }
        rec
      })

      values$ai_recommendations <- recommendations
      values$ai_recommendations_cache[[metric]] <- recommendations
      values$ai_analysis_done_by_metric[[metric]] <- TRUE
      
      # 返回建議供 AI 管理器使用
      return(recommendations)
    }
    
    
    # 準備匯出資料
    prepareExportData <- function() {
      req(values$segmented_data)
      
      # 選擇要匯出的欄位
      export_cols <- c("customer_id", "segment", "r_value", "f_value", "m_value")
      
      # 添加過去價值和參與度分數（如果存在）
      if("pcv" %in% names(values$segmented_data)) {
        export_cols <- c(export_cols, "pcv")
      } else if("total_spent" %in% names(values$segmented_data)) {
        export_cols <- c(export_cols, "total_spent")
      }
      
      if("cri" %in% names(values$segmented_data)) {
        export_cols <- c(export_cols, "cri")
      }
      
      if("cai_value" %in% names(values$segmented_data)) {
        export_cols <- c(export_cols, "cai_value")
      }

      if("segment_pcv_quintile" %in% names(values$segmented_data)) {
        export_cols <- c(export_cols, "segment_pcv_quintile")
      }

      if("segment_nes" %in% names(values$segmented_data)) {
        export_cols <- c(export_cols, "segment_nes")
      }
      
      # 選擇存在的欄位
      export_cols <- intersect(export_cols, names(values$segmented_data))
      
      # 建立匯出資料框
      export_data <- values$segmented_data[, export_cols]
      
      # 重新命名欄位 (language-aware)
      names(export_data) <- sapply(names(export_data), function(x) {
        switch(x,
          "customer_id" = get_lang_text("modules.vitalsigns_dna.table.columns.customer_id", "客戶ID"),
          "segment" = get_lang_text("modules.vitalsigns_dna.table.columns.segment", "客群分組"),
          "r_value" = get_lang_text("modules.vitalsigns_dna.table.columns.r_value_days", "最近購買日"),
          "f_value" = get_lang_text("modules.vitalsigns_dna.table.columns.f_value", "購買頻率"),
          "m_value" = get_lang_text("modules.vitalsigns_dna.table.columns.m_value_avg", "平均購買金額"),
          "pcv" = get_lang_text("modules.vitalsigns_dna.table.columns.pcv", "過去價值"),
          "total_spent" = get_lang_text("modules.vitalsigns_dna.table.columns.total_spent", "累積消費"),
          "cri" = get_lang_text("modules.vitalsigns_dna.table.columns.cri", "顧客穩定度"),
          "cai_value" = get_lang_text("modules.vitalsigns_dna.table.columns.cai_value_index", "活躍度指標"),
          "segment_pcv_quintile" = get_lang_text("modules.vitalsigns_dna.table.columns.segment_pcv_quintile", "過去價值分群(五等分)"),
          "segment_nes" = get_lang_text("modules.vitalsigns_dna.table.columns.segment_nes", "顧客狀態分群"),
          x
        )
      })

      # 按分組排序 (get the translated segment column name)
      segment_col_name <- get_lang_text("modules.vitalsigns_dna.table.columns.segment", "客群分組")
      export_data <- export_data[order(export_data[[segment_col_name]]), ]
      
      return(export_data)
    }
    
    # 客群分組分析函數
    updateSegmentationAnalysis <- function() {
      req(values$current_metric)
      data <- get_active_dna_data()
      req(data)
      metric <- values$current_metric
      
      # 根據選定的指標取得資料欄位
      metric_col <- switch(metric,
        "M" = "m_value",
        "R" = "r_value",
        "F" = "f_value",
        "CAI" = "cai_value",
        "PCV" = "pcv",
        "CRI" = "cri",
        "NES" = "nes_status"
      )
      
      if(metric_col %in% names(data) && metric != "NES") {
        # 使用80/20法則進行分組
        values_vec <- data[[metric_col]]
        
        # 計算分位數
        q20 <- quantile(values_vec, 0.2, na.rm = TRUE)
        q80 <- quantile(values_vec, 0.8, na.rm = TRUE)
        
        # 分組
        data$segment <- ifelse(values_vec <= q20, "低",
                              ifelse(values_vec >= q80, "高", "中"))
        
        # 儲存分組結果
        values$segmented_data <- data
      }
    }
    
    # 客群分組分析輸出
    output$segmentation_analysis <- renderUI({
      req(lang_texts())  # React to language changes
      # 只有在已執行分群且有選擇指標時才顯示
      if(!isTRUE(values$segmentation_completed) || is.null(values$current_metric)) {
        return(
          div(
            class = "alert alert-info",
            icon("info-circle"),
            get_lang_text("messages.info.segmentation_required", "請先執行客戶分群分析，然後選擇指標查看結果")
          )
        )
      }
      
      req(values$segmented_data)
      data <- values$segmented_data
      metric <- values$current_metric
      
      # 根據不同指標計算統計
      if(metric == "NES") {
        # NES 使用狀態分群
        segment_stats <- data %>%
          filter(!is.na(segment)) %>%
          group_by(segment) %>%
          summarise(
            count = n(),
            avg_r = mean(r_value, na.rm = TRUE),
            avg_f = mean(f_value, na.rm = TRUE),
            avg_m = mean(m_value, na.rm = TRUE),
            .groups = "drop"
          )
      } else {
        # 其他指標使用數值分群
        metric_col <- switch(metric,
          "M" = "m_value",
          "R" = "r_value",
          "F" = "f_value",
          "CAI" = "cai_value",
          "PCV" = "pcv",
          "CRI" = "cri"
        )
        
        if(metric_col %in% names(data)) {
          segment_stats <- data %>%
            filter(!is.na(segment)) %>%
            group_by(segment) %>%
            summarise(
              count = n(),
              avg_value = mean(get(metric_col), na.rm = TRUE),
              avg_r = mean(r_value, na.rm = TRUE),
              avg_f = mean(f_value, na.rm = TRUE),
              avg_m = mean(m_value, na.rm = TRUE),
              .groups = "drop"
            )
        } else {
          return(div(class = "alert alert-warning", get_lang_text("modules.vitalsigns_dna.messages.warning.metric_data_not_found", "無法找到指標數據")))
        }
      }
      
      # 根據指標顯示不同標題 (language-aware)
      title_text <- switch(metric,
        "R" = paste0("📊 ", get_lang_text("modules.vitalsigns_dna.segmentation_ui.title.recency", "最近購買時間分群結果")),
        "F" = paste0("📊 ", get_lang_text("modules.vitalsigns_dna.segmentation_ui.title.frequency", "購買頻率分群結果")),
        "M" = paste0("📊 ", get_lang_text("modules.vitalsigns_dna.segmentation_ui.title.monetary", "購買金額分群結果（80/20法則）")),
        "CAI" = paste0("📊 ", get_lang_text("modules.vitalsigns_dna.segmentation_ui.title.cai", "活躍度分群結果")),
        "PCV" = paste0("📊 ", get_lang_text("modules.vitalsigns_dna.segmentation_ui.title.pcv", "過去價值分群結果（80/20法則）")),
        "CRI" = paste0("📊 ", get_lang_text("modules.vitalsigns_dna.segmentation_ui.title.cri", "顧客穩定度分群結果（80/20法則）")),
        "NES" = paste0("📊 ", get_lang_text("modules.vitalsigns_dna.segmentation_ui.title.nes", "顧客狀態分群結果")),
        paste0("📊 ", get_lang_text("modules.vitalsigns_dna.segmentation_ui.title.default", "客群分組結果"))
      )
      
      tagList(
        h5(title_text),
        fluidRow(
          lapply(c("high", "mid", "low"), function(seg) {
            # Map English keys to Chinese for data lookup (segment_stats still uses Chinese labels)
            seg_chinese <- switch(seg,
              "high" = "高",
              "mid" = "中",
              "low" = "低",
              seg
            )
            seg_data <- segment_stats[segment_stats$segment == seg_chinese, ]
            if(nrow(seg_data) > 0) {
              # Get language-aware segment label
              seg_label <- switch(seg,
                "high" = get_lang_text("segment_labels.high", "高"),
                "mid" = get_lang_text("segment_labels.mid", "中"),
                "low" = get_lang_text("segment_labels.low", "低"),
                seg
              )

              column(4,
                bs4Card(
                  title = paste0(seg_label, get_lang_text("modules.vitalsigns_dna.segmentation_ui.value_group", "價值群")),
                  status = if(seg == "high") "success" else if(seg == "mid") "warning" else "danger",
                  solidHeader = TRUE,
                  width = 12,
                  h4(seg_data$count, " ", get_lang_text("modules.vitalsigns_dna.metric_analysis.people_unit", "人")),
                  p(paste0(get_lang_text("modules.vitalsigns_dna.segmentation_ui.percentage", "佔比"), "：",
                           round(seg_data$count / sum(segment_stats$count) * 100, 1), "%")),
                  hr(),
                  h6(get_lang_text("modules.vitalsigns_dna.segmentation_ui.marketing_suggestions", "行銷建議"), "："),
                  p(
                    switch(seg,
                      "high" = switch(metric,
                        "R" = get_lang_text("modules.vitalsigns_dna.marketing.r_high", "近期不活躍，需要召回策略"),
                        "F" = get_lang_text("modules.vitalsigns_dna.marketing.f_high", "高頻客戶，提供VIP服務"),
                        "M" = get_lang_text("modules.vitalsigns_dna.marketing.m_high", "高價值客戶，深度經營"),
                        get_lang_text("modules.vitalsigns_dna.marketing.default_high", "提供專屬優惠")
                      ),
                      "mid" = switch(metric,
                        "R" = get_lang_text("modules.vitalsigns_dna.marketing.r_mid", "適度活躍，維持互動"),
                        "F" = get_lang_text("modules.vitalsigns_dna.marketing.f_mid", "中頻客戶，提升忠誠度"),
                        "M" = get_lang_text("modules.vitalsigns_dna.marketing.m_mid", "中價值客戶，向上銷售"),
                        get_lang_text("modules.vitalsigns_dna.marketing.default_mid", "定期關懷")
                      ),
                      "low" = switch(metric,
                        "R" = get_lang_text("modules.vitalsigns_dna.marketing.r_low", "高度活躍，把握機會"),
                        "F" = get_lang_text("modules.vitalsigns_dna.marketing.f_low", "低頻客戶，激發興趣"),
                        "M" = get_lang_text("modules.vitalsigns_dna.marketing.m_low", "低價值客戶，入門引導"),
                        get_lang_text("modules.vitalsigns_dna.marketing.default_low", "基礎培育")
                      )
                    ),
                    style = "font-size: 13px;"
                  )
                )
              )
            }
          })
        )
      )
    })
    
    # 客群詳細表格 (Following R092: get_lang_text for all UI text)
    output$segment_detail_table <- renderDT({
      req(lang_texts())  # React to language changes
      # 檢查是否已執行分群分析
      if(!isTRUE(values$segmentation_completed) || is.null(values$temp_chinese_table)) {
        hint_label <- get_lang_text("ui.segmentation.hint_label", "提示")
        hint_msg <- get_lang_text("ui.segmentation.hint_message", "請先執行客戶分群分析")
        empty_msg <- get_lang_text("ui.segmentation.empty_message", "請點擊『執行客戶分群』按鈕開始分析")

        return(datatable(
          setNames(data.frame(hint_msg), hint_label),
          options = list(
            dom = 't',
            language = list(emptyTable = empty_msg)
          ),
          rownames = FALSE
        ))
      }
      
      data <- values$temp_chinese_table
      available_cols <- names(data)

      # 取得語言化的欄位名稱
      customer_id_text <- get_lang_text("table.columns.customer_id", "客戶ID")
      segment_text <- get_lang_text("table.columns.segment", "客群分組")
      recency_days_text <- get_lang_text("table.columns.recency_days", "最近購買(天)")
      frequency_count_text <- get_lang_text("table.columns.frequency_count", "購買頻率(次)")
      avg_amount_text <- get_lang_text("table.columns.avg_amount", "平均金額($)")
      purchase_count_text <- get_lang_text("table.columns.purchase_count", "購買次數")
      past_value_text <- get_lang_text("table.columns.past_value", "過去價值($)")
      total_spent_text <- get_lang_text("table.columns.total_spent", "總消費($)")

      # 選擇存在的欄位
      select_cols <- c()
      if(customer_id_text %in% available_cols) select_cols <- c(select_cols, customer_id_text)
      if(segment_text %in% available_cols) select_cols <- c(select_cols, segment_text)
      if(recency_days_text %in% available_cols) select_cols <- c(select_cols, recency_days_text)
      if(frequency_count_text %in% available_cols) select_cols <- c(select_cols, frequency_count_text)
      if(avg_amount_text %in% available_cols) select_cols <- c(select_cols, avg_amount_text)

      # 只選擇實際存在的欄位
      if(length(select_cols) > 0) {
        display_data <- data[, select_cols, drop = FALSE]
      } else {
        no_cols_msg <- get_lang_text("errors.no_columns_to_display", "沒有可顯示的欄位")
        message_col <- get_lang_text("table.columns.message", "訊息")
        display_data <- setNames(data.frame(no_cols_msg), message_col)
      }

      # 定義需要格式化的欄位
      format_round_cols <- c(recency_days_text, frequency_count_text, purchase_count_text)
      format_currency_cols <- c(avg_amount_text, past_value_text, total_spent_text)
      
      dt <- datatable(
        display_data,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          order = list(list(1, 'asc'))
        ),
        rownames = FALSE
      )
      
      # 格式化實際存在的欄位
      existing_round_cols <- intersect(format_round_cols, names(display_data))
      if(length(existing_round_cols) > 0) {
        tryCatch({
          dt <- dt %>% formatRound(existing_round_cols, 0)
        }, error = function(e) {
          message("formatRound error: ", e$message)
        })
      }
      
      existing_currency_cols <- intersect(format_currency_cols, names(display_data))
      if(length(existing_currency_cols) > 0) {
        tryCatch({
          dt <- dt %>% formatCurrency(existing_currency_cols, "$")
        }, error = function(e) {
          message("formatCurrency error: ", e$message)
        })
      }
      
      dt
    })
    
    # 行銷建議函數
    updateMarketingRecommendations <- function() {
      req(values$dna_results, values$current_metric)
      # 觸發行銷建議生成
    }
    
    # AI 行銷建議輸出
    output$marketing_recommendations <- renderUI({
      req(lang_texts())  # React to language changes
      # 只有在已執行分群且有選擇指標時才顯示
      if(!isTRUE(values$segmentation_completed) || is.null(values$current_metric)) {
        return(NULL)
      }

      req(values$current_metric, values$segmented_data)
      data <- values$segmented_data
      metric <- values$current_metric

      # Get metric name using language keys
      metric_name <- switch(metric,
        "R" = get_lang_text("marketing_recommendations.metric_names.R", "Recency"),
        "F" = get_lang_text("marketing_recommendations.metric_names.F", "Frequency"),
        "M" = get_lang_text("marketing_recommendations.metric_names.M", "Monetary"),
        "CAI" = get_lang_text("marketing_recommendations.metric_names.CAI", "Customer Activity Index"),
        "PCV" = get_lang_text("marketing_recommendations.metric_names.PCV", "Past Customer Value"),
        "CRI" = get_lang_text("marketing_recommendations.metric_names.CRI", "Customer Retention Index"),
        "NES" = get_lang_text("marketing_recommendations.metric_names.NES", "Customer Status")
      )

      # 生成針對性的行銷建議
      tagList(
        div(
          class = "alert alert-warning",
          h5(icon("lightbulb"), paste0(" ", get_lang_text("marketing_recommendations.header_title", "AI 智能行銷建議"))),
          p(paste0(
            get_lang_text("marketing_recommendations.header_intro_prefix", "基於 "),
            metric_name,
            get_lang_text("marketing_recommendations.header_intro_suffix", " 分析")
          ))
        ),

        # 根據不同指標提供具體建議
        if(metric == "R") {
          tagList(
            h5(get_lang_text("marketing_recommendations.recency.title", "🎯 最近來店時間行銷策略")),
            bs4Card(
              title = get_lang_text("marketing_recommendations.recency.recall_strategy.title", "召回策略（超過75分位數）"),
              status = "danger",
              width = 12,
              p(get_lang_text("marketing_recommendations.recency.recall_strategy.advice1", "• 發送個人化召回郵件")),
              p(get_lang_text("marketing_recommendations.recency.recall_strategy.advice2", "• 提供限時優惠券（7天內使用）")),
              p(get_lang_text("marketing_recommendations.recency.recall_strategy.advice3", "• 推送新品資訊喚醒興趣"))
            ),
            bs4Card(
              title = get_lang_text("marketing_recommendations.recency.maintain_strategy.title", "維持策略（25-75分位數）"),
              status = "warning",
              width = 12,
              p(get_lang_text("marketing_recommendations.recency.maintain_strategy.advice1", "• 定期推送會員專屬優惠")),
              p(get_lang_text("marketing_recommendations.recency.maintain_strategy.advice2", "• 建立積分獎勵機制")),
              p(get_lang_text("marketing_recommendations.recency.maintain_strategy.advice3", "• 提供生日月優惠"))
            ),
            bs4Card(
              title = get_lang_text("marketing_recommendations.recency.deepen_strategy.title", "深化策略（低於25分位數）"),
              status = "success",
              width = 12,
              p(get_lang_text("marketing_recommendations.recency.deepen_strategy.advice1", "• 推薦相關產品交叉銷售")),
              p(get_lang_text("marketing_recommendations.recency.deepen_strategy.advice2", "• 邀請加入VIP計劃")),
              p(get_lang_text("marketing_recommendations.recency.deepen_strategy.advice3", "• 提供升級服務機會"))
            )
          )
        } else if(metric == "F") {
          tagList(
            h5(get_lang_text("marketing_recommendations.frequency.title", "🎯 購買頻率行銷策略")),
            bs4Card(
              title = get_lang_text("marketing_recommendations.frequency.high_frequency.title", "高頻客戶（前20%）"),
              status = "success",
              width = 12,
              p(get_lang_text("marketing_recommendations.frequency.high_frequency.advice1", "• 建立VIP專屬服務")),
              p(get_lang_text("marketing_recommendations.frequency.high_frequency.advice2", "• 提供批量採購優惠")),
              p(get_lang_text("marketing_recommendations.frequency.high_frequency.advice3", "• 優先體驗新品"))
            ),
            bs4Card(
              title = get_lang_text("marketing_recommendations.frequency.mid_frequency.title", "中頻客戶（20-80%）"),
              status = "warning",
              width = 12,
              p(get_lang_text("marketing_recommendations.frequency.mid_frequency.advice1", "• 設計忠誠度計劃")),
              p(get_lang_text("marketing_recommendations.frequency.mid_frequency.advice2", "• 提供累積消費獎勵")),
              p(get_lang_text("marketing_recommendations.frequency.mid_frequency.advice3", "• 定期關懷提醒"))
            ),
            bs4Card(
              title = get_lang_text("marketing_recommendations.frequency.low_frequency.title", "低頻客戶（後20%）"),
              status = "danger",
              width = 12,
              p(get_lang_text("marketing_recommendations.frequency.low_frequency.advice1", "• 了解購買障礙")),
              p(get_lang_text("marketing_recommendations.frequency.low_frequency.advice2", "• 提供試用優惠")),
              p(get_lang_text("marketing_recommendations.frequency.low_frequency.advice3", "• 簡化購買流程"))
            )
          )
        } else if(metric == "M") {
          tagList(
            h5(get_lang_text("marketing_recommendations.monetary.title", "🎯 購買金額行銷策略")),
            bs4Card(
              title = get_lang_text("marketing_recommendations.monetary.high_value.title", "高價值客戶（前20%）"),
              status = "success",
              width = 12,
              p(get_lang_text("marketing_recommendations.monetary.high_value.advice1", "• 提供專屬客戶經理")),
              p(get_lang_text("marketing_recommendations.monetary.high_value.advice2", "• 客製化產品推薦")),
              p(get_lang_text("marketing_recommendations.monetary.high_value.advice3", "• 尊榮級售後服務"))
            ),
            bs4Card(
              title = get_lang_text("marketing_recommendations.monetary.mid_value.title", "中價值客戶（20-80%）"),
              status = "warning",
              width = 12,
              p(get_lang_text("marketing_recommendations.monetary.mid_value.advice1", "• 推薦升級產品")),
              p(get_lang_text("marketing_recommendations.monetary.mid_value.advice2", "• 捆綁銷售優惠")),
              p(get_lang_text("marketing_recommendations.monetary.mid_value.advice3", "• 分期付款方案"))
            ),
            bs4Card(
              title = get_lang_text("marketing_recommendations.monetary.low_value.title", "低價值客戶（後20%）"),
              status = "info",
              width = 12,
              p(get_lang_text("marketing_recommendations.monetary.low_value.advice1", "• 入門級產品推薦")),
              p(get_lang_text("marketing_recommendations.monetary.low_value.advice2", "• 首購優惠券")),
              p(get_lang_text("marketing_recommendations.monetary.low_value.advice3", "• 教育內容行銷"))
            )
          )
        } else if(metric == "CAI") {
          tagList(
            h5(get_lang_text("marketing_recommendations.cai.title", "🎯 顧客活躍度行銷策略")),
            bs4Card(
              title = get_lang_text("marketing_recommendations.cai.trending_active.title", "漸趨活躍（>0）"),
              status = "success",
              width = 12,
              p(get_lang_text("marketing_recommendations.cai.trending_active.advice1", "• 把握成長動能，加強互動")),
              p(get_lang_text("marketing_recommendations.cai.trending_active.advice2", "• 推薦熱門產品")),
              p(get_lang_text("marketing_recommendations.cai.trending_active.advice3", "• 邀請參與會員活動"))
            ),
            bs4Card(
              title = get_lang_text("marketing_recommendations.cai.stable.title", "穩定（≈0）"),
              status = "warning",
              width = 12,
              p(get_lang_text("marketing_recommendations.cai.stable.advice1", "• 維持現有服務水準")),
              p(get_lang_text("marketing_recommendations.cai.stable.advice2", "• 定期但不過度打擾")),
              p(get_lang_text("marketing_recommendations.cai.stable.advice3", "• 季節性促銷活動"))
            ),
            bs4Card(
              title = get_lang_text("marketing_recommendations.cai.trending_static.title", "漸趨靜止（<0）"),
              status = "danger",
              width = 12,
              p(get_lang_text("marketing_recommendations.cai.trending_static.advice1", "• 緊急挽回措施")),
              p(get_lang_text("marketing_recommendations.cai.trending_static.advice2", "• 了解流失原因")),
              p(get_lang_text("marketing_recommendations.cai.trending_static.advice3", "• 提供特別優惠重新激活"))
            )
          )
        } else {
          div(
            class = "alert alert-info",
            p(get_lang_text("marketing_recommendations.default_message", "請選擇具體指標以獲得詳細行銷建議"))
          )
        }
      )
    })
    
    # 檢查是否有分組資料
    output$has_segmentation <- reactive({
      !is.null(values$segmented_data)
    })
    outputOptions(output, "has_segmentation", suspendWhenHidden = FALSE)
    
    # 檢查是否已執行分群並且有選擇指標
    output$has_ai_recommendations <- reactive({
      isTRUE(values$segmentation_completed) && !is.null(values$current_metric)
    })
    outputOptions(output, "has_ai_recommendations", suspendWhenHidden = FALSE)
    
    # 客群分組視圖
    output$segmentation_view <- renderUI({
      req(lang_texts())  # React to language changes
      # 只有在已執行分群且有選擇指標時才顯示
      if(!isTRUE(values$segmentation_completed) || is.null(values$current_metric)) {
        return(
          div(
            class = "alert alert-info",
            icon("info-circle"),
            get_lang_text("messages.info.segmentation_required", "請先執行客戶分群分析，然後選擇指標查看結果")
          )
        )
      }
      
      req(values$segmented_data)
      
      # 根據指標顯示不同標題
      title_text <- switch(values$current_metric,
        "R" = paste0("📊 ", get_lang_text("segmentation.titles.recency", "最近購買時間分群結果")),
        "F" = paste0("📊 ", get_lang_text("segmentation.titles.frequency", "購買頻率分群結果")),
        "M" = paste0("📊 ", get_lang_text("segmentation.titles.monetary", "購買金額分群結果（80/20法則）")),
        "CAI" = paste0("📊 ", get_lang_text("segmentation.titles.cai", "活躍度分群結果")),
        "PCV" = paste0("📊 ", get_lang_text("segmentation.titles.pcv", "過去價值分群結果（80/20法則）")),
        "CRI" = paste0("📊 ", get_lang_text("segmentation.titles.cri", "顧客穩定度分群結果（80/20法則）")),
        "NES" = paste0("📊 ", get_lang_text("segmentation.titles.nes", "顧客狀態分群結果")),
        paste0("📊 ", get_lang_text("segmentation.titles.default", "客群分組結果"))
      )
      
      tagList(
        h5(title_text),
        uiOutput(ns("segmentation_summary")),
        hr(),
        
        # AI行銷建議（如果有的話）
        conditionalPanel(
          condition = paste0("output['", ns("has_ai_recommendations"), "'] == true"),
          h5(paste0("🎯 ", get_lang_text("ui.headers.ai_advice", "AI 行銷建議"))),
          uiOutput(ns("ai_marketing_advice")),
          hr()
        ),

        h5(paste0("📋 ", get_lang_text("ui.headers.customer_list", "詳細客戶名單"))),
        div(
          style = "margin-bottom: 10px;",
          downloadButton(ns("download_customer_details"), paste0("📥 ", get_lang_text("ui.buttons.download_full_list", "下載完整客戶名單")), 
                        class = "btn-success btn-sm")
        ),
        DTOutput(ns("segmented_customers_table"))
      )
    })
    
    # 當前分群結果摘要（簡化版本，不顯示卡片）
    output$current_segmentation_summary <- renderUI({
      NULL  # 不顯示任何內容
    })
    
    # 當前行銷建議（顯示在右側）
    output$current_marketing_advice <- renderUI({
      req(lang_texts())  # React to language changes
      if(is.null(values$ai_recommendations)) {
        return(div(
          class = "text-muted",
          style = "padding: 8px; font-size: 11px;",
          p("點擊「執行AI增強分析」以獲得行銷建議")
        ))
      }
      
      # 將建議轉換為markdown格式（調整字體大小）
      markdown_content <- paste(
        lapply(names(values$ai_recommendations), function(seg) {
          rec <- values$ai_recommendations[[seg]]
          paste0(
            "##### ", seg, "\n",  # 改用 ##### 讓標題更小
            "**", get_lang_text("marketing_recommendations.labels.strategy", "策略"), "：** ", rec$strategy, "\n\n",
            "**", get_lang_text("marketing_recommendations.labels.action_suggestions", "行動建議"), "：**\n",
            paste(lapply(rec$actions, function(action) {
              paste0("- ", action)
            }), collapse = "\n"),
            "\n"
          )
        }),
        collapse = "\n---\n\n"
      )
      
      # 使用HTML函數渲染markdown，並包裝在div中控制字體大小（改為11px）
      div(
        style = "font-size: 11px; line-height: 1.4;",
        HTML(markdown::markdownToHTML(
          text = markdown_content,
          fragment.only = TRUE,
          options = c("use_xhtml", "smartypants")
        ))
      )
    })
    
    # 客群分組摘要（簡化版本）
    output$segmentation_summary <- renderUI({
      req(lang_texts())  # React to language changes
      req(values$segmented_data, values$current_metric)
      
      # 計算各分組統計，並檢查欄位存在性
      data <- values$segmented_data
      available_cols <- names(data)
      metric <- values$current_metric
      
      # 根據現有欄位計算統計
      segment_summary <- data %>%
        filter(!is.na(segment)) %>%
        group_by(segment) %>%
        summarise(
          count = n(),
          pct = round(n() / nrow(data) * 100, 1),
          avg_r = if("r_value" %in% available_cols) round(mean(r_value, na.rm = TRUE), 1) else NA,
          avg_f = if("f_value" %in% available_cols) round(mean(f_value, na.rm = TRUE), 1) else NA,
          avg_m = if("m_value" %in% available_cols) round(mean(m_value, na.rm = TRUE), 2) else NA,
          .groups = "drop"
        ) %>%
        arrange(desc(count))
      
      # 根據指標決定顯示的欄位標題
      metric_label <- switch(metric,
        "R" = get_lang_text("dna_metrics.recency.short_name", "最近購買"),
        "F" = get_lang_text("dna_metrics.frequency.short_name", "購買頻率"),
        "M" = get_lang_text("dna_metrics.monetary.short_name", "購買金額"),
        "CAI" = get_lang_text("dna_metrics.cai.short_name", "活躍度"),
        "PCV" = get_lang_text("dna_metrics.pcv.short_name", "過去價值"),
        "CRI" = get_lang_text("dna_metrics.cri.short_name", "穩定度"),
        "NES" = get_lang_text("dna_metrics.nes.short_name", "顧客狀態"),
        get_lang_text("general.labels.metric", "指標")
      )

      count_text <- get_lang_text("table.columns.count", "人數")
      percentage_text <- get_lang_text("table.columns.percentage", "佔比")
      segment_suffix <- get_lang_text("general.labels.segmentation", "分群")
      avg_r_text <- get_lang_text("table.columns.avg_r", "R(天)")
      avg_f_text <- get_lang_text("table.columns.avg_f", "F(次)")
      avg_m_text <- get_lang_text("table.columns.avg_m", "M($)")

      # 簡化的表格顯示
      tags$table(
        class = "table table-sm table-hover",
        tags$thead(
          tags$tr(
            tags$th(paste0(metric_label, segment_suffix)),
            tags$th(count_text),
            tags$th(percentage_text),
            if("r_value" %in% available_cols) tags$th(avg_r_text) else NULL,
            if("f_value" %in% available_cols) tags$th(avg_f_text) else NULL,
            if("m_value" %in% available_cols) tags$th(avg_m_text) else NULL
          )
        ),
        tags$tbody(
          lapply(1:nrow(segment_summary), function(i) {
            seg <- segment_summary[i, ]
            tags$tr(
              tags$td(seg$segment),
              tags$td(seg$count),
              tags$td(paste0(seg$pct, "%")),
              if("r_value" %in% available_cols) tags$td(if(!is.na(seg$avg_r)) seg$avg_r else "-") else NULL,
              if("f_value" %in% available_cols) tags$td(if(!is.na(seg$avg_f)) seg$avg_f else "-") else NULL,
              if("m_value" %in% available_cols) tags$td(if(!is.na(seg$avg_m)) paste0("$", seg$avg_m) else "-") else NULL
            )
          })
        )
      )
    })
    
    # AI行銷建議顯示（簡化版本，不使用卡片）
    output$ai_marketing_advice <- renderUI({
      req(lang_texts())  # React to language changes
      # 只有在已執行分群且有選擇指標時才顯示
      if(!isTRUE(values$segmentation_completed) || is.null(values$current_metric)) {
        return(NULL)
      }

      # 使用分群數據生成建議
      req(values$segmented_data, values$current_metric)
      generateMarketingAdviceForMetric(values$current_metric, values$segmented_data)
    })
    
    # 根據指標生成行銷建議
    generateMarketingAdviceForMetric <- function(metric, data) {
      # 計算各分群的統計
      segments <- unique(data$segment)
      segments <- segments[!is.na(segments)]
      
      if(length(segments) == 0) {
        return(p("無分群數據"))
      }
      
      # 根據不同指標提供建議
      tagList(
        lapply(segments, function(seg) {
          seg_data <- data[data$segment == seg & !is.na(data$segment), ]
          
          if(nrow(seg_data) == 0) return(NULL)
          
          # 計算統計數據
          seg_stats <- seg_data %>%
            summarise(
              count = n(),
              avg_r = mean(r_value, na.rm = TRUE),
              avg_f = mean(f_value, na.rm = TRUE),
              avg_m = mean(m_value, na.rm = TRUE)
            )
          
          # 根據指標和分群提供建議
          advice <- getSegmentAdvice(metric, seg, seg_stats)
          
          div(
            style = "margin-bottom: 15px; padding: 10px; background: #f8f9fa; border-radius: 5px;",
            h6(paste0("🔸 ", seg, " (", seg_stats$count, "人)"), 
               style = "color: #2c3e50; font-weight: bold; margin-bottom: 8px;"),
            tags$ul(
              style = "margin: 0; padding-left: 20px;",
              lapply(advice, function(action) {
                tags$li(action, style = "margin: 3px 0; color: #495057;")
              })
            )
          )
        })
      )
    }
    
    # ==========================================
    # OPTION A: Key-Based Advice System
    # ==========================================
    # Helper function to detect segment key from Chinese label
    # Following MP/P/R principles: Use language-independent keys for logic
    detectSegmentKey <- function(segment_label) {
      # Mapping of Chinese patterns to segment keys
      # MP001: Language-independent architecture principle
      # IMPORTANT: Check more specific patterns first to avoid false matches
      if(grepl("高活躍|高度活躍", segment_label)) return("high_active")
      if(grepl("一般活躍|中度活躍", segment_label)) return("general")
      if(grepl("流失|不活躍|低活躍", segment_label)) return("churned")
      if(grepl("高頻", segment_label)) return("high_frequency")
      if(grepl("中頻", segment_label)) return("mid_frequency")
      if(grepl("低頻", segment_label)) return("low_frequency")
      if(grepl("高價值", segment_label)) return("high_value")
      if(grepl("中價值", segment_label)) return("mid_value")
      if(grepl("低價值", segment_label)) return("low_value")
      if(grepl("漸趨活躍|趨向活躍", segment_label)) return("trending_active")
      if(grepl("漸趨靜止|趨向靜止", segment_label)) return("trending_static")
      if(grepl("短週期|短周期", segment_label)) return("short_cycle")
      if(grepl("中週期|中周期", segment_label)) return("mid_cycle")
      if(grepl("長週期|長周期", segment_label)) return("long_cycle")
      # Check stability patterns AFTER more specific patterns
      if(grepl("高穩定", segment_label)) return("high_stability")
      if(grepl("中穩定", segment_label)) return("mid_stability")
      if(grepl("低穩定", segment_label)) return("low_stability")
      if(grepl("穩定", segment_label)) return("stable")
      return("unknown")
    }

    # Get marketing advice based on metric and segment key
    # Following MP/P/R principles: Use get_lang_text() for display only
    getSegmentAdvice <- function(metric, segment_label, stats) {
      # Detect language-independent segment key from label
      segment_key <- detectSegmentKey(segment_label)

      # Map metric to metric key for YAML lookup
      metric_key <- tolower(metric)
      if(metric_key == "r") metric_key <- "recency"
      if(metric_key == "f") metric_key <- "frequency"
      if(metric_key == "m") metric_key <- "monetary"
      if(metric_key == "ipt") metric_key <- "ipt"
      if(metric_key == "cai") metric_key <- "cai"
      if(metric_key == "pcv") metric_key <- "pcv"
      if(metric_key == "cri") metric_key <- "cri"

      # Build YAML path for advice lookup
      # Following MP/P/R: Configuration-driven development
      advice_path <- paste0("marketing_advice.", metric_key, ".", segment_key)

      # Try to get advice from YAML
      advice <- tryCatch({
        advice_list <- get_lang_text(advice_path, NULL)
        if(!is.null(advice_list) && is.list(advice_list)) {
          return(unlist(advice_list))
        }
        NULL
      }, error = function(e) NULL)

      # If advice found in YAML, return it
      if(!is.null(advice)) {
        return(advice)
      }

      # Fallback: Get default advice if specific segment not found
      default_advice <- tryCatch({
        default_list <- get_lang_text("marketing_advice.default", NULL)
        if(!is.null(default_list) && is.list(default_list)) {
          return(unlist(default_list))
        }
        NULL
      }, error = function(e) NULL)

      if(!is.null(default_advice)) {
        return(default_advice)
      }

      # Ultimate fallback (should never happen if YAML is correct)
      return(c(
        get_lang_text("marketing_advice.fallback1", "定期關懷維護"),
        get_lang_text("marketing_advice.fallback2", "提供個人化服務"),
        get_lang_text("marketing_advice.fallback3", "建立長期關係")
      ))
    }
    
    # 分組客戶詳細表格 (Following R092: get_lang_text for all UI text)
    output$segmented_customers_table <- renderDT({
      req(lang_texts())  # React to language changes
      # 檢查是否已執行分群分析
      if(!isTRUE(values$segmentation_completed) || is.null(values$temp_chinese_table)) {
        hint_label <- get_lang_text("ui.segmentation.hint_label", "提示")
        hint_msg <- get_lang_text("ui.segmentation.hint_message", "請先執行客戶分群分析")
        empty_msg <- get_lang_text("ui.segmentation.empty_message", "請點擊『執行客戶分群』按鈕開始分析")

        return(datatable(
          setNames(data.frame(hint_msg), hint_label),
          options = list(
            dom = 't',
            language = list(emptyTable = empty_msg)
          ),
          rownames = FALSE
        ))
      }

      if(nrow(values$temp_chinese_table) == 0) {
        msg_label <- get_lang_text("ui.segmentation.message_label", "訊息")
        msg_text <- get_lang_text("ui.segmentation.empty_data_message", "分組資料為空，請確認資料正確")
        return(datatable(setNames(data.frame(msg_text), msg_label), rownames = FALSE))
      }
      
      # Use data with original English column names
      display_data <- values$temp_chinese_table
      metric <- values$current_metric

      # Map English column names to display (language system handles labels)
      segment_col_mapping <- list(
        "R" = "segment_r",
        "F" = "segment_f",
        "M" = "segment_m",
        "IPT" = "segment_ipt",
        "CAI" = "segment_cai",
        "PCV" = "segment_pcv",
        "CRI" = "segment_cri",
        "NES" = "segment_nes"
      )

      # Find corresponding segment column
      segment_col <- segment_col_mapping[[metric]]
      if(is.null(segment_col) || !segment_col %in% names(display_data)) {
        segment_col <- "segment"  # Default to "segment" column
      }

      # Select columns to display (using English names)
      display_cols <- c("customer_id", segment_col)

      # Add metric-specific columns based on current metric
      if(values$current_metric == "R") {
        if("r_value" %in% names(display_data)) display_cols <- c(display_cols, "r_value")
      } else if(values$current_metric == "F") {
        if("f_value" %in% names(display_data)) display_cols <- c(display_cols, "f_value")
      } else if(values$current_metric == "M") {
        if("m_value" %in% names(display_data)) display_cols <- c(display_cols, "m_value")
      } else if(values$current_metric == "IPT") {
        if("ipt_mean" %in% names(display_data)) display_cols <- c(display_cols, "ipt_mean")
      } else if(values$current_metric == "CAI") {
        if("cai_value" %in% names(display_data)) display_cols <- c(display_cols, "cai_value")
      } else if(values$current_metric == "PCV") {
        if("pcv" %in% names(display_data)) {
          display_cols <- c(display_cols, "pcv")
        } else if("total_spent" %in% names(display_data)) {
          display_cols <- c(display_cols, "total_spent")
        }
      } else if(values$current_metric == "CRI") {
        if("cri" %in% names(display_data)) {
          display_cols <- c(display_cols, "cri")
        }
      } else if(values$current_metric == "NES") {
        if("nes_status" %in% names(display_data)) display_cols <- c(display_cols, "nes_status")
      }

      # Ensure columns exist
      display_cols <- intersect(display_cols, names(display_data))
      
      # 建立顯示資料框
      if(length(display_cols) > 0) {
        display_data <- display_data[, display_cols, drop = FALSE]
      } else {
        message_col <- get_lang_text("table.columns.message", "訊息")
        no_cols_text <- get_lang_text("errors.no_columns_to_display", "沒有可顯示的欄位")
        return(datatable(setNames(data.frame(no_cols_text), message_col), rownames = FALSE))
      }

      # Format columns (using English names)
      format_round_cols <- c("r_value", "f_value", "ipt_mean")
      format_currency_cols <- c("m_value", "pcv", "total_spent", "clv")

      # Sort by segment column
      if(segment_col %in% names(display_data)) {
        display_data <- display_data[order(display_data[[segment_col]]), ]
      }
      
      dt <- datatable(
        display_data,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          order = list(list(1, 'asc')),
          dom = 'frtip',  # 移除 B (buttons)
          language = list(
            search = paste0(get_lang_text("ui.datatable.search", "搜尋"), ":"),
            lengthMenu = get_lang_text("ui.datatable.length_menu", "顯示 _MENU_ 筆資料"),
            info = get_lang_text("ui.datatable.pagination_info", "顯示第 _START_ 至 _END_ 筆，共 _TOTAL_ 筆"),
            paginate = list(
              first = get_lang_text("ui.datatable.first_page", "第一頁"),
              last = get_lang_text("ui.datatable.last_page", "最後一頁"),
              `next` = get_lang_text("ui.datatable.next", "下一頁"),
              previous = get_lang_text("ui.datatable.previous", "上一頁")
            )
          )
        ),
        rownames = FALSE
      )
      
      # 直接檢查並格式化中文欄位
      # 格式化整數欄位
      existing_round <- intersect(format_round_cols, names(display_data))
      if(length(existing_round) > 0) {
        tryCatch({
          dt <- dt %>% formatRound(existing_round, 0)
        }, error = function(e) {
          message("formatRound error: ", e$message)
        })
      }
      
      # 格式化貨幣欄位
      existing_currency <- intersect(format_currency_cols, names(display_data))
      if(length(existing_currency) > 0) {
        tryCatch({
          dt <- dt %>% formatCurrency(existing_currency, "$")
        }, error = function(e) {
          message("formatCurrency error: ", e$message)
        })
      }
      
      dt
    })
    
    # 客群分組結果顯示
    output$segmentation_results <- renderUI({
      req(lang_texts())  # React to language changes
      req(values$segmented_data)

      # 計算各分組統計
      segment_stats <- values$segmented_data %>%
        group_by(segment) %>%
        summarise(
          count = n(),
          avg_r = mean(r_value, na.rm = TRUE),
          avg_f = mean(f_value, na.rm = TRUE),
          avg_m = mean(m_value, na.rm = TRUE),
          .groups = "drop"
        )

      # Get language-specific labels and units
      person_count_label <- get_lang_text("segmentation_results.person_count", "人")
      r_label <- get_lang_text("segmentation_results.metrics.r_label", "R")
      f_label <- get_lang_text("segmentation_results.metrics.f_label", "F")
      m_label <- get_lang_text("segmentation_results.metrics.m_label", "M")
      r_unit <- get_lang_text("segmentation_results.metrics.r_unit", "天")
      f_unit <- get_lang_text("segmentation_results.metrics.f_unit", "次")
      m_prefix <- get_lang_text("segmentation_results.metrics.m_prefix", "$")

      tagList(
        fluidRow(
          lapply(1:nrow(segment_stats), function(i) {
            row <- segment_stats[i,]

            # Get language-aware segment labels for color mapping
            high_value <- get_lang_text("segment_labels.monetary.high_value", "高價值")
            mid_value <- get_lang_text("segment_labels.monetary.mid_value", "中價值")
            low_value <- get_lang_text("segment_labels.monetary.low_value", "低價值")
            high_active <- get_lang_text("segment_labels.recency.high_active", "高活躍")
            mid_active <- get_lang_text("segment_labels.recency.general", "一般活躍")
            low_active <- get_lang_text("segment_labels.recency.churned", "流失")
            trending_active <- get_lang_text("segment_labels.cai.trending_active", "漸趨活躍")
            stable <- get_lang_text("segment_labels.cai.stable", "穩定")
            trending_static <- get_lang_text("segment_labels.cai.trending_static", "漸趨靜止")

            color <- if(row$segment == high_value || row$segment == high_active || row$segment == trending_active) {
              "success"
            } else if(row$segment == mid_value || row$segment == mid_active || row$segment == stable) {
              "warning"
            } else if(row$segment == low_value || row$segment == low_active || row$segment == trending_static) {
              "danger"
            } else {
              "secondary"
            }

            column(4,
              bs4InfoBox(
                title = row$segment,
                value = paste0(row$count, " ", person_count_label),
                subtitle = paste0(
                  r_label, ": ", round(row$avg_r, 1), r_unit, " | ",
                  f_label, ": ", round(row$avg_f, 1), f_unit, " | ",
                  m_label, ": ", m_prefix, round(row$avg_m, 2)
                ),
                icon = icon("users"),
                color = color,
                width = 12
              )
            )
          })
        ),
        hr(),
        h5(get_lang_text("segmentation_results.detailed_customer_list", "詳細客戶名單")),
        DTOutput("segmented_customers_table")
      )
    })
    
    # 新增：AI分析結果顯示
    output$ai_analysis <- renderUI({
      req(values$ai_insights)
      
      insights <- values$ai_insights
      
      HTML(paste0(
        "<div style='padding: 20px; background: #f8f9fa; border-radius: 8px;'>",
        "<h4 style='color: #2c3e50; margin-bottom: 20px;'>🤖 ", get_lang_text("ai_insights_ui.section_title", "AI 分析洞察"), "</h4>",

        "<div style='margin-bottom: 20px;'>",
        "<h5 style='color: #34495e;'>💰 ", get_lang_text("ai_insights_ui.monetary_title", "購買金額分析"), "</h5>",
        "<pre style='background: white; padding: 10px; border-radius: 4px;'>", insights$monetary, "</pre>",
        "</div>",

        "<div style='margin-bottom: 20px;'>",
        "<h5 style='color: #34495e;'>⏰ ", get_lang_text("ai_insights_ui.recency_title", "最近購買時間分析"), "</h5>",
        "<pre style='background: white; padding: 10px; border-radius: 4px;'>", insights$recency, "</pre>",
        "</div>",

        "<div style='margin-bottom: 20px;'>",
        "<h5 style='color: #34495e;'>🔄 ", get_lang_text("ai_insights_ui.frequency_title", "購買頻率分析"), "</h5>",
        "<pre style='background: white; padding: 10px; border-radius: 4px;'>", insights$frequency, "</pre>",
        "</div>",

        "<div style='margin-bottom: 20px;'>",
        "<h5 style='color: #34495e;'>👥 ", get_lang_text("ai_insights_ui.nes_title", "顧客狀態分析"), "</h5>",
        "<pre style='background: white; padding: 10px; border-radius: 4px;'>", insights$nes, "</pre>",
        "</div>",

        "</div>"
      ))
    })
    
    # ---- AI 洞察輸出 --------------------------------------------------------
    output$has_ai_insights <- reactive({
      !is.null(values$ai_insights)
    })
    outputOptions(output, "has_ai_insights", suspendWhenHidden = FALSE)
    
    # AI分析完成標記
    output$has_ai_analysis <- reactive({
      metric <- values$current_metric
      !is.null(values$ai_analysis_done_by_metric[[metric]]) && isTRUE(values$ai_analysis_done_by_metric[[metric]])
    })
    outputOptions(output, "has_ai_analysis", suspendWhenHidden = FALSE)
    
    # AI分析結果顯示
    # NOTE: Using simplified version without AI manager
    # The full version with render_ai_analysis_ui() requires utils/ai_analysis_manager.R
    output$ai_analysis_results <- renderUI({
      metric <- values$current_metric
      if(is.null(metric)) return(NULL)
      if(!isTRUE(values$ai_analysis_done_by_metric[[metric]])) {
        return(
          div(
            class = "alert alert-info",
            p(get_lang_text("messages.processing.generating_ai_results", "尚未為此指標生成 AI 分析結果"))
          )
        )
      }
      
      # 檢查是否有任何結果（僅使用當前指標的快取）
      summary_for_metric <- values$ai_summary_cache[[metric]]
      recs_for_metric <- values$ai_recommendations_cache[[metric]]

      if(is.null(summary_for_metric) && is.null(recs_for_metric)) {
        return(div(
          class = "alert alert-info",
          p(get_lang_text("messages.processing.generating_ai_results", "正在生成AI分析結果，請稍候..."))
        ))
      }
      
      # 使用isolate確保只在需要時更新
      isolate({
        tagList(
          # AI分析摘要
          if(!is.null(summary_for_metric)) {
            bs4Card(
              title = get_lang_text("ai_deep_analysis.overall_summary_title", "📊 整體分析摘要"),
              status = "primary",
              solidHeader = FALSE,
              width = 12,
              collapsible = TRUE,
              HTML(summary_for_metric)
            )
          },
          
          # AI行銷建議（dna_segment_marketing的結果）
          if(!is.null(recs_for_metric)) {
            bs4Card(
              title = paste0("🎯 ", get_lang_text("marketing_recommendations.segment_marketing_title", "分群行銷建議")),
              status = "success",
              solidHeader = FALSE,
              width = 12,
              collapsible = TRUE,
              collapsed = FALSE,
              div(
                style = "font-size: 12px; line-height: 1.5;",
                HTML(
                  paste(
                    lapply(names(recs_for_metric), function(seg) {
                      rec <- recs_for_metric[[seg]]
                      paste0(
                        "<div style='margin-bottom: 15px; padding: 10px; background: #f8f9fa; border-radius: 5px;'>",
                        "<h6 style='color: #2c3e50; margin-bottom: 8px;'>", seg, "</h6>",
                        "<p style='margin-bottom: 5px;'><strong>", get_lang_text("marketing_recommendations.strategy_label", "策略："), "</strong> ", rec$strategy, "</p>",
                        "<p style='margin-bottom: 5px;'><strong>", get_lang_text("marketing_recommendations.action_recommendations_label", "行動建議："), "</strong></p>",
                        "<ul style='margin-left: 20px; margin-bottom: 0;'>",
                        paste(lapply(rec$actions, function(action) {
                          paste0("<li style='margin-bottom: 3px;'>", action, "</li>")
                        }), collapse = ""),
                        "</ul>",
                        "</div>"
                      )
                    }),
                    collapse = ""
                  )
                )
              )
            )
          }
        )
      })  # 結束 isolate
    })
    
    output$ai_insights_content <- renderUI({
      metric <- values$current_metric
      if (is.null(metric)) return(NULL)

      ai_text <- values$ai_summary_cache[[metric]]
      if (is.null(ai_text)) {
        return(NULL)
      }

      # 將 AI 結果轉換為 HTML 格式
      insights_html <- tryCatch({
        markdown::markdownToHTML(
          text = ai_text,
          fragment.only = TRUE
        )
      }, error = function(e) {
        paste0("<div style='white-space: pre-wrap;'>", ai_text, "</div>")
      })

      div(
        class = "ai-insights-section",
        style = "background: #ffffff; border: 1px solid #dee2e6; border-radius: 5px; padding: 15px;",
        HTML(insights_html)
      )
    })

    # ============================================================
    # 2024-12-28 新增：Tab 式 AI 分析結果渲染函數
    # ============================================================

    # 輔助函數：生成指標專屬的 AI 分析結果 HTML
    generate_metric_ai_html <- function(metric_code) {
      if (!isTRUE(values$ai_analysis_done_by_metric[[metric_code]])) {
        return(div(class = "alert alert-info", get_lang_text("messages.processing.generating_ai_results", "尚未為此指標生成 AI 分析結果")))
      }
      data <- get_active_dna_data()
      req(data)

      # 指標名稱對照
      metric_info <- list(
        "R" = list(
          name = get_lang_text("dna_metrics.recency.full_name", "最近購買日分析"),
          icon = "🕒",
          col = "r_value",
          unit = get_lang_text("units.days", "天"),
          description = get_lang_text("dna_metrics.recency.description", "客戶最近一次購買距今天數"),
          interpretation = get_lang_text("dna_metrics.recency.interpretation", "數值越小表示客戶越活躍")
        ),
        "F" = list(
          name = get_lang_text("dna_metrics.frequency.full_name", "購買頻率分析"),
          icon = "🔁",
          col = "f_value",
          unit = get_lang_text("units.times", "次"),
          description = get_lang_text("dna_metrics.frequency.description", "客戶購買次數"),
          interpretation = get_lang_text("dna_metrics.frequency.interpretation", "數值越高表示客戶購買越頻繁")
        ),
        "M" = list(
          name = get_lang_text("dna_metrics.monetary.full_name", "購買金額分析"),
          icon = "💰",
          col = "m_value",
          unit = get_lang_text("units.currency", "元"),
          description = get_lang_text("dna_metrics.monetary.description", "客戶總消費金額"),
          interpretation = get_lang_text("dna_metrics.monetary.interpretation", "數值越高表示客戶價值越高")
        ),
        "IPT" = list(
          name = get_lang_text("dna_metrics.ipt.full_name", "購買週期分析"),
          icon = "⏱️",
          col = "ipt_mean",
          unit = get_lang_text("units.days", "天"),
          description = get_lang_text("dna_metrics.ipt.description", "客戶平均購買間隔天數"),
          interpretation = get_lang_text("dna_metrics.ipt.interpretation", "數值越小表示購買越頻繁")
        ),
        "CAI" = list(
          name = get_lang_text("dna_metrics.cai.full_name", "顧客活躍度分析"),
          icon = "📈",
          col = "cai_value",
          unit = "",
          description = get_lang_text("dna_metrics.cai.description", "綜合評估客戶活躍程度的指數"),
          interpretation = get_lang_text("dna_metrics.cai.interpretation", "數值越高表示客戶越活躍")
        ),
        "PCV" = list(
          name = get_lang_text("dna_metrics.pcv.full_name", "過去價值分析"),
          icon = "💎",
          col = "pcv",
          unit = get_lang_text("units.currency", "元"),
          description = get_lang_text("dna_metrics.pcv.description", "客戶歷史累積貢獻價值"),
          interpretation = get_lang_text("dna_metrics.pcv.interpretation", "數值越高表示歷史價值越高")
        ),
        "CRI" = list(
          name = get_lang_text("dna_metrics.cri.full_name", "顧客穩定度分析"),
          icon = "🎯",
          col = "cri",
          unit = "",
          description = get_lang_text("dna_metrics.cri.description", "客戶購買行為的穩定程度"),
          interpretation = get_lang_text("dna_metrics.cri.interpretation", "數值越高表示購買行為越穩定")
        ),
        "NES" = list(
          name = get_lang_text("customer_lifecycle.full_title", "顧客狀態分析"),
          icon = "👥",
          col = "nes_status",
          unit = "",
          description = get_lang_text("customer_lifecycle.description", "客戶生命週期狀態"),
          interpretation = get_lang_text("customer_lifecycle.interpretation", "新客：僅購買一次；活躍：核心客群；沉睡：購買行為減少")
        )
      )

      info <- metric_info[[metric_code]]
      if (is.null(info)) return(div("指標不存在"))

      # 根據指標類型計算統計
      if (metric_code == "NES") {
        # NES 是類別型態
        if ("nes_status" %in% names(data)) {
          status_counts <- table(data$nes_status)
          total <- sum(status_counts)

          stats_html <- paste0(
            "<div style='display: flex; flex-wrap: wrap; gap: 10px; margin-bottom: 15px;'>",
            paste(sapply(names(status_counts), function(s) {
              count <- status_counts[s]
              pct <- round(count / total * 100, 1)
              paste0(
                "<div style='background: #f8f9fa; padding: 10px 15px; border-radius: 5px; text-align: center;'>",
                "<div style='font-size: 20px; font-weight: bold; color: #007bff;'>", count, "</div>",
                "<div style='font-size: 12px; color: #6c757d;'>", s, " (", pct, "%)</div>",
                "</div>"
              )
            }), collapse = ""),
            "</div>"
          )

          interpretation_html <- paste0(
            "<p>", info$interpretation, "</p>",
            "<ul>",
            "<li><b>新客</b>：僅購買一次的客戶</li>",
            "<li><b>活躍</b>：核心客群，購買行為穩定</li>",
            "<li><b>沉睡</b>：購買行為明顯減少的客戶</li>",
            "</ul>"
          )
        } else {
          stats_html <- "<p style='color: #6c757d;'>無顧客狀態資料</p>"
          interpretation_html <- ""
        }
      } else {
        # 數值型態指標
        col_name <- info$col
        if (col_name %in% names(data)) {
          values_vec <- data[[col_name]]
          values_vec <- values_vec[!is.na(values_vec)]

          avg_val <- round(mean(values_vec), 2)
          med_val <- round(median(values_vec), 2)
          min_val <- round(min(values_vec), 2)
          max_val <- round(max(values_vec), 2)
          sd_val <- round(sd(values_vec), 2)

          stats_html <- paste0(
            "<div style='display: grid; grid-template-columns: repeat(5, 1fr); gap: 10px; margin-bottom: 15px;'>",
            "<div style='background: #e3f2fd; padding: 12px; border-radius: 5px; text-align: center;'>",
            "<div style='font-size: 18px; font-weight: bold; color: #1976d2;'>", format(avg_val, big.mark = ","), "</div>",
            "<div style='font-size: 11px; color: #6c757d;'>平均值</div></div>",
            "<div style='background: #e8f5e9; padding: 12px; border-radius: 5px; text-align: center;'>",
            "<div style='font-size: 18px; font-weight: bold; color: #388e3c;'>", format(med_val, big.mark = ","), "</div>",
            "<div style='font-size: 11px; color: #6c757d;'>中位數</div></div>",
            "<div style='background: #fff3e0; padding: 12px; border-radius: 5px; text-align: center;'>",
            "<div style='font-size: 18px; font-weight: bold; color: #f57c00;'>", format(min_val, big.mark = ","), "</div>",
            "<div style='font-size: 11px; color: #6c757d;'>最小值</div></div>",
            "<div style='background: #fce4ec; padding: 12px; border-radius: 5px; text-align: center;'>",
            "<div style='font-size: 18px; font-weight: bold; color: #c2185b;'>", format(max_val, big.mark = ","), "</div>",
            "<div style='font-size: 11px; color: #6c757d;'>最大值</div></div>",
            "<div style='background: #f3e5f5; padding: 12px; border-radius: 5px; text-align: center;'>",
            "<div style='font-size: 18px; font-weight: bold; color: #7b1fa2;'>", format(sd_val, big.mark = ","), "</div>",
            "<div style='font-size: 11px; color: #6c757d;'>標準差</div></div>",
            "</div>"
          )

          interpretation_html <- paste0("<p>", info$interpretation, "</p>")

          # 額外的分組摘要（PCV 需展示 3 類與 5 類）
          if (metric_code == "PCV" && !is.null(values$complete_segmented_data)) {
            seg_data <- values$complete_segmented_data
            seg_blocks <- c()

            if ("segment_pcv" %in% names(seg_data)) {
              pareto_tbl <- table(seg_data$segment_pcv)
              seg_blocks <- c(
                seg_blocks,
                paste0(
                  get_lang_text("modules.vitalsigns_dna.segmentations.pareto", "80/20三類"),
                  "：",
                  paste(paste0(names(pareto_tbl), "(", pareto_tbl, "人)"), collapse = "，")
                )
              )
            }

            if ("segment_pcv_quintile" %in% names(seg_data)) {
              quint_tbl <- table(seg_data$segment_pcv_quintile)
              seg_blocks <- c(
                seg_blocks,
                paste0(
                  get_lang_text("modules.vitalsigns_dna.segmentations.quintile", "五等分"),
                  "：",
                  paste(paste0(names(quint_tbl), "(", quint_tbl, "人)"), collapse = "，")
                )
              )
            }

            if (length(seg_blocks) > 0) {
              interpretation_html <- paste0(
                interpretation_html,
                "<div style='margin-top: 10px; background: #fff8e1; padding: 10px; border-radius: 6px; border-left: 4px solid #f6c343;'>",
                paste(seg_blocks, collapse = "<br>"),
                "</div>"
              )
            }
          }
        } else {
          stats_html <- paste0("<p style='color: #6c757d;'>無 ", info$name, " 資料</p>")
          interpretation_html <- ""
        }
      }

      # 取得行銷建議（如果有）
      # 行銷建議在「分群行銷建議」卡片已完整呈現，避免重複顯示於每個指標 Tab
      recommendations_html <- ""

      # 組合完整 HTML
      tagList(
        div(
          style = "font-size: 14px;",

          # 標題
          h5(paste0(info$icon, " ", info$name), style = "margin-bottom: 15px;"),

          # 說明
          div(
            style = "background: #f8f9fa; padding: 12px; border-radius: 5px; margin-bottom: 15px;",
            p(info$description, style = "margin: 0; color: #495057;")
          ),

          # 統計數據
          HTML(stats_html),

          # 解讀
          div(
            style = "background: #e8f4f8; padding: 12px; border-radius: 5px; border-left: 4px solid #17a2b8;",
            h6("💡 解讀說明", style = "margin-bottom: 8px;"),
            HTML(interpretation_html)
          ),

          # 行銷建議
          HTML(recommendations_html)
        )
      )
    }

    # Tab 渲染輸出：最近購買日 (R)
    output$ai_tab_result_r <- renderUI({
      generate_metric_ai_html("R")
    })

    # Tab 渲染輸出：購買頻率 (F)
    output$ai_tab_result_f <- renderUI({
      generate_metric_ai_html("F")
    })

    # Tab 渲染輸出：購買金額 (M)
    output$ai_tab_result_m <- renderUI({
      generate_metric_ai_html("M")
    })

    # Tab 渲染輸出：購買週期 (IPT)
    output$ai_tab_result_ipt <- renderUI({
      generate_metric_ai_html("IPT")
    })

    # Tab 渲染輸出：顧客活躍度 (CAI)
    output$ai_tab_result_cai <- renderUI({
      generate_metric_ai_html("CAI")
    })

    # Tab 渲染輸出：過去價值 (PCV)
    output$ai_tab_result_pcv <- renderUI({
      generate_metric_ai_html("PCV")
    })

    # Tab 渲染輸出：顧客穩定度 (CRI)
    output$ai_tab_result_cri <- renderUI({
      generate_metric_ai_html("CRI")
    })

    # Tab 渲染輸出：顧客狀態 (NES)
    output$ai_tab_result_nes <- renderUI({
      generate_metric_ai_html("NES")
    })

    # Return results
    return(reactive({
      list(
        dna_results = values$dna_results,
        combined_data = values$combined_data,
        status = values$status_text,
        ai_insights = values$ai_insights  # 新增：AI洞察結果
      )
    }))
  })
} 
