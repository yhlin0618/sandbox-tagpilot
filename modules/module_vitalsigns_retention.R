# 客戶留存模組 (Customer Retention Module) - YAML Multi-Language Framework
# 衡量基盤穩定度與流失風險

library(shiny)
library(bs4Dash)
library(DT)
library(plotly)
library(dplyr)
library(markdown)
library(digest)

# 載入提示和prompt系統
source("utils/hint_system.R")
source("utils/prompt_manager.R")

# UI Function - Dynamic Rendering for Language Switching
retentionVitalsignsModuleUI <- function(id, module_config = NULL, lang_texts = NULL, enable_hints = TRUE) {
  ns <- NS(id)

  # Return dynamic UI container that will be populated by server
  uiOutput(ns("dynamic_ui"))
}

# Server Function
retentionVitalsignsModuleServer <- function(id, con, user_info, dna_module, module_config = NULL,
                                           lang_texts = NULL, enable_hints = TRUE,
                                           enable_gpt = FALSE, chat_api = NULL,
                                           api_config = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 從共用 API 設定讀取模型（fallback 到 gpt-5-nano）
    cfg_ai_model <- if (!is.null(api_config$default_model)) {
      api_config$default_model
    } else {
      "gpt-5-nano"
    }

    # ===================================================================
    # Current language helper (to use in cache keys / prompts)
    # ===================================================================
    current_language <- reactive({
      texts <- lang_texts()
      if (!is.null(texts)) {
        if (!is.null(texts$metadata) && !is.null(texts$metadata$language)) {
          return(texts$metadata$language)
        }
        if (!is.null(texts$language)) {
          return(texts$language)
        }
      }
      "zh_TW"
    })

    # ===================================================================
    # Dynamic UI Rendering for Language Switching Support
    # ===================================================================
    output$dynamic_ui <- renderUI({
      # NOTE: 不能用 req() 卡住 UI，否則語系/配置尚未載入時整個模組不會 render
      texts <- lang_texts()

      # Helper function for reactive text retrieval (following Acquisition pattern)
      get_text <- function(key, default = "") {
        if (is.null(texts)) return(default)

        # Navigate through nested structure (e.g., "ui.kpis.retention_rate")
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

      tagList(
        # Initialize Bootstrap tooltips for CSV-based hints
        if(enable_hints) init_hint_system() else NULL,

        # 第一排：主要KPI（4個）
        fluidRow(
          column(3,
            bs4ValueBox(
              value = textOutput(ns("retention_rate")),
              subtitle = tags$span(
                get_text("ui.kpis.retention_rate", "顧客留存率"),
                tags$i(
                  class = "fas fa-info-circle ml-1",
                  style = "font-size: 12px; color: #6c757d; cursor: help;",
                  title = get_hint("customer_retention_rate", hints_df(), language = lang_texts()$language),
                  `data-toggle` = "tooltip",
                  `data-placement` = "top"
                )
              ),
              icon = icon("user-check"),
              color = "success",
              width = 12
            )
          ),
          column(3,
            bs4ValueBox(
              value = textOutput(ns("churn_rate")),
              subtitle = tags$span(
                get_text("ui.kpis.churn_rate", "顧客流失率"),
                tags$i(
                  class = "fas fa-info-circle ml-1",
                  style = "font-size: 12px; color: #6c757d; cursor: help;",
                  title = get_hint("customer_churn_rate", hints_df(), language = lang_texts()$language),
                  `data-toggle` = "tooltip",
                  `data-placement` = "top"
                )
              ),
              icon = icon("user-times"),
              color = "danger",
              width = 12
            )
          ),
          column(3,
            bs4ValueBox(
              value = textOutput(ns("at_risk_customers")),
              subtitle = tags$span(
                get_text("ui.kpis.at_risk_customers", "流失風險客戶"),
                tags$i(
                  class = "fas fa-info-circle ml-1",
                  style = "font-size: 12px; color: #6c757d; cursor: help;",
                  title = get_hint("at_risk_customers", hints_df(), language = lang_texts()$language),
                  `data-toggle` = "tooltip",
                  `data-placement` = "top"
                )
              ),
              icon = icon("exclamation-triangle"),
              color = "warning",
              width = 12
            )
          ),
          column(3,
            bs4ValueBox(
              value = textOutput(ns("core_ratio")),
              subtitle = tags$span(
                get_text("ui.kpis.core_ratio", "主力客比率"),
                tags$i(
                  class = "fas fa-info-circle ml-1",
                  style = "font-size: 12px; color: #6c757d; cursor: help;",
                  title = get_hint("core_customer_ratio", hints_df(), language = lang_texts()$language),
                  `data-toggle` = "tooltip",
                  `data-placement` = "top"
                )
              ),
              icon = icon("star"),
              color = "primary",
              width = 12
            )
          )
        ),

        br(),

        # 第二排：客戶狀態細分（5個）
        fluidRow(
          column(2,
            bs4ValueBox(
              value = textOutput(ns("dormant_prediction")),
              subtitle = tags$span(
                get_text("ui.kpis.dormant_prediction", "靜止戶預測"),
                tags$i(
                  class = "fas fa-info-circle ml-1",
                  style = "font-size: 12px; color: #6c757d; cursor: help;",
                  title = get_hint("dormant_prediction", hints_df(), language = lang_texts()$language),
                  `data-toggle` = "tooltip",
                  `data-placement` = "top"
                )
              ),
              icon = icon("clock"),
              color = "secondary",
              width = 12,
              footer = tags$small(get_text("ui.kpis.dormant_prediction_footer", "30天內預測"))
            )
          ),
          column(2,
            bs4ValueBox(
              value = textOutput(ns("new_customers")),
              subtitle = tags$span(
                get_text("ui.kpis.new_customers", "新客(N)"),
                tags$i(
                  class = "fas fa-info-circle ml-1",
                  style = "font-size: 12px; color: #6c757d; cursor: help;",
                  title = get_hint("new_customers", hints_df(), language = lang_texts()$language),
                  `data-toggle` = "tooltip",
                  `data-placement` = "top"
                )
              ),
              icon = icon("user-plus"),
              color = "info",
              width = 12
            )
          ),
          column(2,
            bs4ValueBox(
              value = textOutput(ns("core_customers")),
              subtitle = tags$span(
                get_text("ui.kpis.core_customers", "主力客(E0)"),
                tags$i(
                  class = "fas fa-info-circle ml-1",
                  style = "font-size: 12px; color: #6c757d; cursor: help;",
                  title = get_hint("core_customers", hints_df(), language = lang_texts()$language),
                  `data-toggle` = "tooltip",
                  `data-placement` = "top"
                )
              ),
              icon = icon("crown"),
              color = "success",
              width = 12
            )
          ),
          column(2,
            bs4ValueBox(
              value = textOutput(ns("drowsy_customers")),
              subtitle = tags$span(
                get_text("ui.kpis.drowsy_customers", "瞌睡客(S1)"),
                tags$i(
                  class = "fas fa-info-circle ml-1",
                  style = "font-size: 12px; color: #6c757d; cursor: help;",
                  title = get_hint("drowsy_customers", hints_df(), language = lang_texts()$language),
                  `data-toggle` = "tooltip",
                  `data-placement` = "top"
                )
              ),
              icon = icon("bed"),
              color = "warning",
              width = 12
            )
          ),
          column(2,
            bs4ValueBox(
              value = textOutput(ns("semi_sleeping")),
              subtitle = tags$span(
                get_text("ui.kpis.semi_sleeping", "半睡客(S2)"),
                tags$i(
                  class = "fas fa-info-circle ml-1",
                  style = "font-size: 12px; color: #6c757d; cursor: help;",
                  title = get_hint("semi_sleeping_customers", hints_df(), language = lang_texts()$language),
                  `data-toggle` = "tooltip",
                  `data-placement` = "top"
                )
              ),
              icon = icon("moon"),
              color = "orange",
              width = 12
            )
          ),
          column(2,
            bs4ValueBox(
              value = textOutput(ns("sleeping_customers")),
              subtitle = tags$span(
                get_text("ui.kpis.sleeping_customers", "沉睡客(S3)"),
                tags$i(
                  class = "fas fa-info-circle ml-1",
                  style = "font-size: 12px; color: #6c757d; cursor: help;",
                  title = get_hint("sleeping_customers", hints_df(), language = lang_texts()$language),
                  `data-toggle` = "tooltip",
                  `data-placement` = "top"
                )
              ),
              icon = icon("user-slash"),
              color = "danger",
              width = 12
            )
          )
        ),

        br(),

        # 圖表區
        fluidRow(
          # 客戶狀態結構
          column(6,
            bs4Card(
              title = get_text("ui.cards.customer_structure", "客戶狀態結構"),
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              footer = downloadButton(ns("download_structure"), get_text("ui.buttons.download_structure", "下載CSV"), class = "btn-sm"),
              plotlyOutput(ns("customer_structure"))
            )
          ),

          # 流失風險分析
          column(6,
            bs4Card(
              title = get_text("ui.cards.churn_risk_analysis", "流失風險分析"),
              status = "warning",
              solidHeader = TRUE,
              width = 12,
              footer = downloadButton(ns("download_risk"), get_text("ui.buttons.download_risk", "下載CSV"), class = "btn-sm"),
              plotlyOutput(ns("churn_risk_analysis"))
            )
          )
        ),

        br(),

        # AI 分析建議集中區
        fluidRow(
          column(12,
            bs4Card(
              title = get_text("ui.cards.ai_analysis", "🤖 AI 智能分析與建議"),
              status = "info",
              solidHeader = TRUE,
              width = 12,
              collapsible = TRUE,

              # 使用 tabsetPanel 替代
              tabsetPanel(
                type = "tabs",

                tabPanel(
                  title = get_text("ui.ai_tabs.retention_analysis", "留存分析"),
                  icon = icon("chart-line"),
                  br(),
                  uiOutput(ns("ai_retention_analysis"))
                ),

                tabPanel(
                  title = get_text("ui.ai_tabs.new_customer_strategy", "新客策略"),
                  icon = icon("user-plus"),
                  br(),
                  uiOutput(ns("ai_new_customer"))
                ),

                tabPanel(
                  title = get_text("ui.ai_tabs.core_customer_deepening", "主力客深化"),
                  icon = icon("crown"),
                  br(),
                  uiOutput(ns("ai_core_customer"))
                ),

                tabPanel(
                  title = get_text("ui.ai_tabs.drowsy_awakening", "瞌睡客喚醒"),
                  icon = icon("bell"),
                  br(),
                  uiOutput(ns("ai_drowsy_customer"))
                ),

                tabPanel(
                  title = get_text("ui.ai_tabs.semi_dormant_retention", "半睡客挽留"),
                  icon = icon("exclamation-circle"),
                  br(),
                  uiOutput(ns("ai_semi_dormant_customer"))
                ),

                tabPanel(
                  title = get_text("ui.ai_tabs.sleeping_activation", "沉睡客激活"),
                  icon = icon("redo"),
                  br(),
                  uiOutput(ns("ai_sleeping_customer"))
                ),

                tabPanel(
                  title = get_text("ui.ai_tabs.structure_optimization", "結構優化"),
                  icon = icon("project-diagram"),
                  br(),
                  uiOutput(ns("ai_structure_analysis"))
                )
              )
            )
          )
        )
      )
    })

    # ===================================================================
    # Reactive Hint Loading for Language Switching Support (CSV-based)
    # ===================================================================
    hints_df <- reactive({
      if (!enable_hints) return(NULL)

      # Get current language from lang_texts
      current_lang <- tryCatch({
        texts <- lang_texts()
        if (!is.null(texts) && !is.null(texts$language)) {
          texts$language
        } else {
          "zh_TW"  # fallback to Chinese
        }
      }, error = function(e) {
        cat("⚠️ [Retention Hints] Error getting language:", e$message, "\n")
        "zh_TW"  # fallback on error
      })

      # Load hints for current language from CSV
      cat("🔄 [Retention Hints] Loading hints from CSV for language:", current_lang, "\n")
      load_hints(language = current_lang, app_name = "vitalsigns")
    })

    # Helper function for reactive text retrieval (server-side with nested navigation)
    get_text <- function(key, default = "") {
      tryCatch({
        current_texts <- lang_texts()  # Reactive - triggers on language change
        if (is.null(current_texts)) return(default)

        # Navigate through nested structure (e.g., "server.customer_types.N")
        parts <- strsplit(key, "\\.")[[1]]
        value <- current_texts

        for (part in parts) {
          if (is.list(value) && part %in% names(value)) {
            value <- value[[part]]
          } else {
            return(default)
          }
        }

        if (is.null(value)) default else as.character(value)
      }, error = function(e) {
        return(default)
      })
    }

    # 載入 prompt 管理器 - Make it reactive to language changes
    prompts_df <- reactive({
      if(!enable_gpt) return(NULL)

      # Get current language from lang_texts
      current_lang <- tryCatch({
        texts <- if (is.function(lang_texts)) lang_texts() else lang_texts
        if (!is.null(texts) && !is.null(texts$language)) {
          texts$language
        } else {
          "zh_TW"  # fallback to Chinese
        }
      }, error = function(e) {
        cat("⚠️ [Retention Prompts] Error getting language:", e$message, "\n")
        "zh_TW"
      })

      cat("📂 [Retention Prompts] Loading prompts for language:", current_lang, "\n")
      load_prompts(language = current_lang, app_name = "vitalsigns")
    })

    # 輔助函數：處理 AI 結果的 Markdown 轉換
    render_ai_result <- function(result) {
      if(!is.null(result)) {
        # 檢查是否包含 Markdown 格式
        if(grepl("\\*\\*|##|\\n\\n|^[0-9]+\\.|^-\\s", result)) {
          # 轉換 Markdown 為 HTML
          html_content <- markdownToHTML(text = result, fragment.only = TRUE)
          return(HTML(html_content))
        } else {
          # 純文字直接返回
          return(HTML(result))
        }
      }
      return(NULL)
    }

    # ==============================================================
    # GPT 請求節流：避免同一份 DNA 結果重複呼叫 API
    # 使用 DNA 資料哈希 + tab 名稱 做快取鍵，成功回傳後寫入緩存
    # ==============================================================
    ai_cache <- reactiveValues(data = list())

    get_ai_cache_key <- function(tab_id, dna_results, language = "zh_TW") {
      if (is.null(dna_results) || is.null(dna_results$data_by_customer)) return(NULL)
      # 只取穩定欄位產生 hash，避免順序導致不同
      stable_cols <- c("customer_id", "nes_status", "r_value", "f_value", "m_value", "times", "total_spent")
      data <- dna_results$data_by_customer
      existing <- intersect(stable_cols, names(data))
      if (length(existing) == 0) return(NULL)

      key_raw <- list(
        tab = tab_id,
        lang = language,
        data = data[, existing, drop = FALSE]
      )

      # 使用 digest 產生可重現的鍵
      digest::digest(key_raw, algo = "sha256")
    }

    get_cached_ai <- function(key) {
      if (is.null(key)) return(NULL)
      if (!is.null(ai_cache$data[[key]])) {
        cached <- ai_cache$data[[key]]
        cached$used_at <- Sys.time()
        ai_cache$data[[key]] <- cached
        return(cached$content)
      }
      NULL
    }

    set_cached_ai <- function(key, content) {
      if (is.null(key) || is.null(content)) return()
      ai_cache$data[[key]] <- list(content = content, cached_at = Sys.time(), used_at = Sys.time())
    }

    # 依據 DNA 穩定欄位建立簽章，只有資料真正變動才清空 AI 快取
    dna_signature <- reactive({
      dr <- dna_results()
      if (is.null(dr) || is.null(dr$data_by_customer)) return(NULL)
      stable_cols <- c("customer_id", "nes_status", "r_value", "f_value", "m_value", "times", "total_spent")
      existing <- intersect(stable_cols, names(dr$data_by_customer))
      if (length(existing) == 0) return(NULL)
      digest::digest(dr$data_by_customer[, existing, drop = FALSE], algo = "sha256")
    })

    last_dna_hash <- reactiveVal(NULL)

    # 取得 DNA 分析結果 - Reactive Data Access Pattern (matching acquisition module)
    dna_results <- reactive({
      req(dna_module)

      tryCatch({
        cat("🔍 [Retention] Accessing DNA module data...\n")

        # Handle dna_module as reactive function - call it without isolate
        # to properly observe changes when DNA analysis completes
        if (is.function(dna_module)) {
          result <- dna_module()
        } else {
          warning("DNA module is not a reactive function")
          return(NULL)
        }

        if (is.null(result)) {
          cat("⚠️ [Retention] DNA module returned NULL\n")
          return(NULL)
        }

        # Extract DNA results from the module structure
        if (!is.null(result$dna_results)) {
          dna_data <- result$dna_results

          # Verify it's the data structure we need
          if (!is.null(dna_data$data_by_customer) && is.data.frame(dna_data$data_by_customer)) {
            # Backfill tolerant defaults if upstream lacks fields
            dbc <- dna_data$data_by_customer
            if (!"times" %in% names(dbc) && "f_value" %in% names(dbc)) {
              dbc$times <- dbc$f_value
            }
            if (!"total_spent" %in% names(dbc) && all(c("m_value", "f_value") %in% names(dbc))) {
              dbc$total_spent <- dbc$m_value * dbc$f_value
            }
            if (!"nes_status" %in% names(dbc) && "times" %in% names(dbc)) {
              dbc$nes_status <- ifelse(dbc$times <= 1, "N", "E0")
            }
            dna_data$data_by_customer <- dbc

            # Verify required fields exist
            required_fields <- c("customer_id", "nes_status", "r_value", "f_value",
                                "m_value", "total_spent", "times")
            data_fields <- names(dna_data$data_by_customer)
            missing_fields <- setdiff(required_fields, data_fields)

            if (length(missing_fields) > 0) {
              warning("Missing required fields in DNA data: ", paste(missing_fields, collapse = ", "))
              return(NULL)
            }

            cat("✅ [Retention] DNA data loaded:", nrow(dna_data$data_by_customer), "customers\n")
            return(dna_data)
          }
        }

        warning("DNA module result does not contain expected data structure")
        return(NULL)
      }, error = function(e) {
        cat("❌ [Retention] Error accessing DNA module data:", e$message, "\n")
        return(NULL)
      })
    })

    # 如果 DNA 結果更新則清除相關 GPT 快取，避免使用舊分析
    observeEvent(dna_signature(), {
      new_hash <- dna_signature()
      if (is.null(new_hash)) return()

      # 僅在簽章改變時才清除快取，避免同一份資料被重複評分
      if (!is.null(last_dna_hash()) && identical(new_hash, last_dna_hash())) {
        cat("🔁 [Retention] DNA unchanged, keep AI cache\n")
        return()
      }

      last_dna_hash(new_hash)
      ai_cache$data <- list()
      cat("🧹 [Retention] DNA changed -> AI cache cleared\n")
    })

    # 計算留存指標
    retention_metrics <- reactive({
      dr <- dna_results()
      if (is.null(dr) || is.null(dr$data_by_customer)) return(NULL)

      data <- dr$data_by_customer

      # 修正 NES 狀態順序
      status_order <- c("N", "E0", "S1", "S2", "S3")

      # 計算各狀態客戶數
      status_counts <- data %>%
        mutate(nes_status = factor(nes_status, levels = status_order)) %>%
        group_by(nes_status) %>%
        summarise(count = n(), .groups = "drop") %>%
        complete(nes_status = status_order, fill = list(count = 0))

      # 基本指標
      total_customers <- nrow(data)
      active_customers <- sum(data$nes_status %in% c("N", "E0"), na.rm = TRUE)
      churned_customers <- sum(data$nes_status == "S3", na.rm = TRUE)
      at_risk <- sum(data$nes_status %in% c("S1", "S2"), na.rm = TRUE)

      # 計算率
      metrics <- list(
        retention_rate = (active_customers / total_customers) * 100,
        churn_rate = (churned_customers / total_customers) * 100,
        at_risk_count = at_risk,
        core_ratio = (sum(data$nes_status == "E0", na.rm = TRUE) / total_customers) * 100,

        # 各狀態數量
        new_count = sum(data$nes_status == "N", na.rm = TRUE),
        core_count = sum(data$nes_status == "E0", na.rm = TRUE),
        drowsy_count = sum(data$nes_status == "S1", na.rm = TRUE),
        semi_sleeping_count = sum(data$nes_status == "S2", na.rm = TRUE),
        sleeping_count = sum(data$nes_status == "S3", na.rm = TRUE),

        # 靜止戶預測（基於行為趨勢）
        dormant_prediction = sum(data$nes_status == "S1", na.rm = TRUE) * 0.3 +
                            sum(data$nes_status == "S2", na.rm = TRUE) * 0.5,

        # 狀態分布
        status_distribution = status_counts,

        # 原始數據（用於下載）
        raw_data = data
      )

      return(metrics)
    })

    # KPI 顯示
      output$retention_rate <- renderText({
        metrics <- retention_metrics()
        if (is.null(metrics)) return("…")
        paste0(round(metrics$retention_rate, 1), "%")
      })

      output$churn_rate <- renderText({
        metrics <- retention_metrics()
        if (is.null(metrics)) return("…")
        paste0(round(metrics$churn_rate, 1), "%")
      })

      output$at_risk_customers <- renderText({
        metrics <- retention_metrics()
        if (is.null(metrics)) return("…")
        format(metrics$at_risk_count, big.mark = ",")
      })

      output$core_ratio <- renderText({
        metrics <- retention_metrics()
        if (is.null(metrics)) return("…")
        paste0(round(metrics$core_ratio, 1), "%")
      })

      output$dormant_prediction <- renderText({
        metrics <- retention_metrics()
        if (is.null(metrics)) return("…")
        format(round(metrics$dormant_prediction), big.mark = ",")
      })

      output$new_customers <- renderText({
        metrics <- retention_metrics()
        if (is.null(metrics)) return("…")
        format(metrics$new_count, big.mark = ",")
      })

      output$core_customers <- renderText({
        metrics <- retention_metrics()
        if (is.null(metrics)) return("…")
        format(metrics$core_count, big.mark = ",")
      })

      output$drowsy_customers <- renderText({
        metrics <- retention_metrics()
        if (is.null(metrics)) return("…")
        format(metrics$drowsy_count, big.mark = ",")
      })

      output$semi_sleeping <- renderText({
        metrics <- retention_metrics()
        if (is.null(metrics)) return("…")
        format(metrics$semi_sleeping_count, big.mark = ",")
      })

      output$sleeping_customers <- renderText({
        metrics <- retention_metrics()
        if (is.null(metrics)) return("…")
        format(metrics$sleeping_count, big.mark = ",")
      })

      # 客戶狀態結構圖
      output$customer_structure <- renderPlotly({
        metrics <- retention_metrics()
        if (is.null(metrics)) return(NULL)

        distribution <- metrics$status_distribution

      # 定義顏色
      colors <- c("N" = "#17a2b8", "E0" = "#28a745",
                  "S1" = "#ffc107", "S2" = "#fd7e14", "S3" = "#dc3545")

      # 標籤使用 get_text
      labels <- c(
        "N" = get_text("server.customer_types.N", "新客"),
        "E0" = get_text("server.customer_types.E0", "主力客"),
        "S1" = get_text("server.customer_types.S1", "瞌睡客"),
        "S2" = get_text("server.customer_types.S2", "半睡客"),
        "S3" = get_text("server.customer_types.S3", "沉睡客")
      )

      plot_ly(distribution,
              x = ~nes_status,
              y = ~count,
              type = 'bar',
              text = ~paste0(count, " (", round(count/sum(count)*100, 1), "%)"),
              textposition = "outside",
              marker = list(color = colors[distribution$nes_status]),
              hovertemplate = paste0("%{x}<br>",
                                    get_text("server.charts.customer_count", "客戶數"),
                                    ": %{y}<br>",
                                    get_text("server.charts.ratio", "占比"),
                                    ": %{text}<extra></extra>")) %>%
        layout(
          title = list(text = get_text("ui.charts.customer_structure_title", "客戶狀態分布 (N→E0→S1→S2→S3)"), font = list(size = 14)),
          xaxis = list(
            title = get_text("server.charts.customer_status", "客戶狀態"),
            ticktext = labels,
            tickvals = names(labels)
          ),
          yaxis = list(title = get_text("server.charts.customer_count", "客戶數")),
          showlegend = FALSE
        )
    })

    # 流失風險分析圖
    output$churn_risk_analysis <- renderPlotly({
      req(dna_results())
      data <- dna_results()$data_by_customer

      # 計算風險分數
      risk_data <- data %>%
        mutate(
          risk_score = case_when(
            nes_status == "N" ~ 10,
            nes_status == "E0" ~ 5,
            nes_status == "S1" ~ 40,
            nes_status == "S2" ~ 70,
            nes_status == "S3" ~ 95,
            TRUE ~ 50
          ),
          risk_level = case_when(
            risk_score <= 20 ~ get_text("server.risk_levels.low", "低風險"),
            risk_score <= 50 ~ get_text("server.risk_levels.medium", "中風險"),
            risk_score <= 80 ~ get_text("server.risk_levels.high", "高風險"),
            TRUE ~ get_text("server.risk_levels.critical", "極高風險")
          )
        ) %>%
        group_by(nes_status, risk_level) %>%
        summarise(
          count = n(),
          avg_value = mean(total_spent, na.rm = TRUE)
        ) %>%
        mutate(nes_status = factor(nes_status, levels = c("N", "E0", "S1", "S2", "S3")))

      labels <- c(
        "N" = get_text("server.customer_types.N", "新客"),
        "E0" = get_text("server.customer_types.E0", "主力客"),
        "S1" = get_text("server.customer_types.S1", "瞌睡客"),
        "S2" = get_text("server.customer_types.S2", "半睡客"),
        "S3" = get_text("server.customer_types.S3", "沉睡客")
      )

      # Risk level colors
      risk_colors <- c()
      risk_colors[get_text("server.risk_levels.low", "低風險")] <- "#28a745"
      risk_colors[get_text("server.risk_levels.medium", "中風險")] <- "#ffc107"
      risk_colors[get_text("server.risk_levels.high", "高風險")] <- "#fd7e14"
      risk_colors[get_text("server.risk_levels.critical", "極高風險")] <- "#dc3545"

      plot_ly(risk_data,
              x = ~nes_status,
              y = ~count,
              color = ~risk_level,
              colors = risk_colors,
              type = 'bar',
              text = ~paste0(count, get_text("server.charts.people_unit", "人")),
              hovertemplate = paste0("%{x}<br>%{color}<br>",
                                    get_text("server.charts.customer_count", "客戶數"),
                                    ": %{y}<extra></extra>")) %>%
        layout(
          title = list(text = get_text("ui.charts.risk_distribution_title", "流失風險評估"), font = list(size = 14)),
          xaxis = list(
            title = get_text("server.charts.customer_status", "客戶狀態"),
            ticktext = labels,
            tickvals = names(labels)
          ),
          yaxis = list(title = get_text("server.charts.customer_count", "客戶數")),
          barmode = 'stack'
        )
    })

    # CSV 下載功能
    output$download_structure <- downloadHandler(
      filename = function() {
        paste0("customer_structure_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        req(retention_metrics())
        data <- retention_metrics()$status_distribution
        data$status_name <- c(
          get_text("server.customer_types.N", "新客"),
          get_text("server.customer_types.E0", "主力客"),
          get_text("server.customer_types.S1", "瞌睡客"),
          get_text("server.customer_types.S2", "半睡客"),
          get_text("server.customer_types.S3", "沉睡客")
        )[match(data$nes_status, c("N", "E0", "S1", "S2", "S3"))]
        data$percentage <- round(data$count / sum(data$count) * 100, 2)

        # 使用中文欄位名稱
        export_data <- data[, c("nes_status", "status_name", "count", "percentage")]
        names(export_data) <- c("客戶狀態代碼", "客戶狀態", "客戶數", "佔比(%)")
        write.csv(export_data, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )

    output$download_risk <- downloadHandler(
      filename = function() {
        paste0("churn_risk_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        req(dna_results())
        data <- dna_results()$data_by_customer %>%
          mutate(
            risk_score = case_when(
              nes_status == "N" ~ 10,
              nes_status == "E0" ~ 5,
              nes_status == "S1" ~ 40,
              nes_status == "S2" ~ 70,
              nes_status == "S3" ~ 95,
              TRUE ~ 50
            ),
            risk_level = case_when(
              risk_score <= 20 ~ get_text("server.risk_levels.low", "低風險"),
              risk_score <= 50 ~ get_text("server.risk_levels.medium", "中風險"),
              risk_score <= 80 ~ get_text("server.risk_levels.high", "高風險"),
              TRUE ~ get_text("server.risk_levels.critical", "極高風險")
            )
          ) %>%
          select(customer_id, nes_status, risk_score, risk_level, total_spent, times)

        # 使用中文欄位名稱
        names(data) <- c("客戶ID", "客戶狀態", "風險分數", "風險等級", "總消費金額", "購買次數")
        write.csv(data, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )

    # =====================================================================
    # AI 分析功能區塊
    # =====================================================================

    # AI 留存分析
    output$ai_retention_analysis <- renderUI({
      tryCatch({
        texts <- lang_texts()
        if (is.null(texts)) return(div(class = "alert alert-info", "語系載入中…"))

        metrics <- retention_metrics()
        if (is.null(metrics)) return(div(class = "alert alert-info", "資料準備中…"))

        current_dna <- dna_results()
        if (is.null(current_dna) || is.null(current_dna$data_by_customer)) {
          return(div(class = "alert alert-warning", "DNA 資料尚未就緒，請稍候"))
        }

        cache_key <- get_ai_cache_key("retention", current_dna, current_language())

        if (enable_gpt && !is.null(chat_api) && !is.null(prompts_df())) {
          prompts_now <- prompts_df()
          if (is.null(prompts_now)) {
            return(div(class = "alert alert-info", "AI 提示載入中…"))
          }

          # 先看快取
          cached <- get_cached_ai(cache_key)
          if (!is.null(cached)) {
            return(div(
              style = "padding: 15px; background: #f8f9fa; border-radius: 8px;",
              render_ai_result(cached)
            ))
          }

          # 呼叫 GPT
          tryCatch({
            cat("🤖 [Retention][retention_analysis] GPT start. cache_key:",
                ifelse(is.null(cache_key), "NULL", substr(cache_key, 1, 8)), "\n")

            result <- execute_gpt_request(
              var_id = "customer_retention_analysis",
              variables = list(
                retention_rate = round(metrics$retention_rate, 1),
                churn_rate = round(metrics$churn_rate, 1),
                core_ratio = round(metrics$core_ratio, 1),
                at_risk_count = metrics$at_risk_count,
                active_count = metrics$new_count + metrics$core_count
              ),
              chat_api_function = chat_api,
              model = cfg_ai_model,
              prompts_df = prompts_df()  # Call reactive
            )

            if (!is.null(result) && nzchar(trimws(result))) {
              cat("✅ [Retention][retention_analysis] GPT ok. chars:",
                  nchar(result), "\n")
              set_cached_ai(cache_key, result)
              return(div(
                style = "padding: 15px; background: #f8f9fa; border-radius: 8px;",
                render_ai_result(result)
              ))
            } else {
              cat("⚠️ [Retention][retention_analysis] GPT returned empty, fallback to default.\n")
            }
          }, error = function(e) {
            cat(get_text("server.notifications.ai_analysis_failed", "AI 分析失敗"), ":", e$message, "\n")
            return(div(class = "alert alert-danger", paste("AI 分析失敗:", e$message)))
          })
        }

        # 預設分析
        div(
          style = "padding: 15px; background: #f8f9fa; border-radius: 8px;",
          h5(get_text("server.ai_analysis.retention.title", "📊 留存健康度評估"), style = "color: #2c3e50;"),
          p(paste0(get_text("server.ai_analysis.retention.current_rate", "當前留存率"), " ",
                  round(metrics$retention_rate, 1), "%，",
                  ifelse(metrics$retention_rate > 70,
                        get_text("server.ai_analysis.retention.excellent", "表現優異"),
                        get_text("server.ai_analysis.retention.needs_improvement", "需要改善")))),
          h5(get_text("server.ai_analysis.retention.suggestions_title", "💡 建議策略"), style = "color: #2c3e50; margin-top: 15px;"),
          tags$ul(
            tags$li(get_text("server.ai_analysis.retention.suggestion1", "優先關注"), " ", metrics$at_risk_count, " ", get_text("server.ai_analysis.retention.at_risk_suffix", "位風險客戶")),
            tags$li(get_text("server.ai_analysis.retention.suggestion2", "強化主力客戶經營，提升忠誠度")),
            tags$li(get_text("server.ai_analysis.retention.suggestion3", "建立客戶流失預警機制"))
          )
        )
      }, error = function(e) {
        cat("❌ [Retention][retention_analysis] renderUI error:", e$message, "\n")
        return(div(class = "alert alert-warning",
                   paste("AI 區塊渲染錯誤:", e$message)))
      })
    }) %>% bindCache(dna_signature(), current_language())
    outputOptions(output, "ai_retention_analysis", suspendWhenHidden = TRUE)

    # AI 新客分析
    output$ai_new_customer <- renderUI({
      tryCatch({
        texts <- lang_texts(); if (is.null(texts)) return(div(class="alert alert-info","語系載入中…"))
        metrics <- retention_metrics(); if (is.null(metrics)) return(div(class="alert alert-info","資料準備中…"))
        dr <- dna_results(); if (is.null(dr) || is.null(dr$data_by_customer)) return(div(class="alert alert-warning","DNA 資料尚未就緒，請稍候"))
        data <- dr$data_by_customer

        cache_key <- get_ai_cache_key("new_customer", dr, current_language())
        new_customer_data <- data[data$nes_status == "N", ]

        if(nrow(new_customer_data) > 0 && enable_gpt && !is.null(chat_api) && !is.null(prompts_df())) {
          prompts_now <- prompts_df(); if (is.null(prompts_now)) return(div(class="alert alert-info","AI 提示載入中…"))
          cached <- get_cached_ai(cache_key)
          if (!is.null(cached)) return(div(style="padding:15px; background:#e3f2fd; border-radius:8px;", render_ai_result(cached)))

          avg_aov <- mean(new_customer_data$m_value, na.rm = TRUE)
          avg_days <- mean(new_customer_data$r_value, na.rm = TRUE)

          tryCatch({
            result <- execute_gpt_request(
              var_id = "new_customer_analysis",
              variables = list(
                new_count = metrics$new_count,
                new_ratio = round((metrics$new_count / nrow(data)) * 100, 1),
                new_aov = round(avg_aov, 0),
                days_since = round(avg_days, 0)
              ),
              chat_api_function = chat_api,
              model = cfg_ai_model,
              prompts_df = prompts_now
            )
            if(!is.null(result)) {
              set_cached_ai(cache_key, result)
              return(div(style="padding: 15px; background: #e3f2fd; border-radius: 8px;", render_ai_result(result)))
            }
          }, error=function(e){
            cat(get_text("server.notifications.ai_analysis_failed", "AI 分析失敗"), ":", e$message, "\n")
            return(div(class="alert alert-danger", paste("AI 分析失敗:", e$message)))
          })
        }

        div(
          style = "padding: 15px; background: #e3f2fd; border-radius: 8px;",
          h5(get_text("server.ai_analysis.new_customer.title", "🆕 新客特徵"), style = "color: #1976d2;"),
          p(paste0(get_text("server.ai_analysis.new_customer.count_prefix", "共"), " ",
                  metrics$new_count, " ",
                  get_text("server.ai_analysis.new_customer.count_suffix", "位新客"))),
          h5(get_text("server.ai_analysis.new_customer.strategy_title", "🎯 二購提升策略"), style = "color: #1976d2; margin-top: 15px;"),
          tags$ul(
            tags$li(get_text("server.ai_analysis.new_customer.suggestion1", "首購後 7-14 天發送感謝信與優惠券")),
            tags$li(get_text("server.ai_analysis.new_customer.suggestion2", "推薦相關產品，提升客單價")),
            tags$li(get_text("server.ai_analysis.new_customer.suggestion3", "建立新客專屬社群，增加黏著度"))
          )
        )
      }, error=function(e){
        cat("❌ [Retention][new_customer] renderUI error:", e$message, "\n")
        return(div(class="alert alert-warning", paste("AI 區塊渲染錯誤:", e$message)))
      })
    }) %>% bindCache(dna_signature(), current_language())
    outputOptions(output, "ai_new_customer", suspendWhenHidden = TRUE)

    # AI 主力客分析
    output$ai_core_customer <- renderUI({
      tryCatch({
        texts <- lang_texts(); if (is.null(texts)) return(div(class="alert alert-info","語系載入中…"))
        metrics <- retention_metrics(); if (is.null(metrics)) return(div(class="alert alert-info","資料準備中…"))
        dr <- dna_results(); if (is.null(dr) || is.null(dr$data_by_customer)) return(div(class="alert alert-warning","DNA 資料尚未就緒，請稍候"))
        data <- dr$data_by_customer

        cache_key <- get_ai_cache_key("core_customer", dr, current_language())
        core_data <- data[data$nes_status == "E0", ]

        if(nrow(core_data) > 0 && enable_gpt && !is.null(chat_api) && !is.null(prompts_df())) {
          prompts_now <- prompts_df(); if (is.null(prompts_now)) return(div(class="alert alert-info","AI 提示載入中…"))
          cached <- get_cached_ai(cache_key)
          if (!is.null(cached)) return(div(style="padding:15px; background:#e8f5e9; border-radius:8px;", render_ai_result(cached)))

          avg_frequency <- mean(core_data$f_value, na.rm = TRUE)
          avg_monetary <- mean(core_data$m_value, na.rm = TRUE)
          revenue_contribution <- sum(core_data$m_value * core_data$f_value, na.rm = TRUE) /
            sum(data$m_value * data$f_value, na.rm = TRUE) * 100

          tryCatch({
            result <- execute_gpt_request(
              var_id = "core_customer_strategy",
              variables = list(
                core_count = metrics$core_count,
                core_ratio = round((metrics$core_count / nrow(data)) * 100, 1),
                avg_frequency = round(avg_frequency, 1),
                avg_monetary = round(avg_monetary, 2),
                revenue_contribution = round(revenue_contribution, 1)
              ),
              chat_api_function = chat_api,
              model = cfg_ai_model,
              prompts_df = prompts_now
            )
            if(!is.null(result)) {
              set_cached_ai(cache_key, result)
              return(div(style="padding: 15px; background: #e8f5e9; border-radius: 8px;", render_ai_result(result)))
            }
          }, error=function(e){
            cat(get_text("server.notifications.ai_analysis_failed", "AI 分析失敗"), ":", e$message, "\n")
            return(div(class="alert alert-danger", paste("AI 分析失敗:", e$message)))
          })
        }

        div(
          style = "padding: 15px; background: #e8f5e9; border-radius: 8px;",
          h5(get_text("server.ai_analysis.core_customer.title", "🏆 主力客戶分析"), style = "color: #2e7d32;"),
          p(paste0(get_text("server.ai_analysis.core_customer.count_prefix", "共"), " ",
                  metrics$core_count, " ",
                  get_text("server.ai_analysis.core_customer.count_suffix", "位主力客戶"))),
          h5(get_text("server.ai_analysis.core_customer.strategy_title", "💎 VIP經營策略"), style = "color: #2e7d32; margin-top: 15px;"),
          tags$ul(
            tags$li(get_text("server.ai_analysis.core_customer.suggestion1", "建立VIP會員制度與專屬權益")),
            tags$li(get_text("server.ai_analysis.core_customer.suggestion2", "提供個人化服務與優先支援")),
            tags$li(get_text("server.ai_analysis.core_customer.suggestion3", "定期舉辦專屬活動增強黏著度")),
            tags$li(get_text("server.ai_analysis.core_customer.suggestion4", "推薦高價值產品提升客單價"))
          )
        )
      }, error=function(e){
        cat("❌ [Retention][core_customer] renderUI error:", e$message, "\n")
        return(div(class="alert alert-warning", paste("AI 區塊渲染錯誤:", e$message)))
      })
    }) %>% bindCache(dna_signature(), current_language())
    outputOptions(output, "ai_core_customer", suspendWhenHidden = TRUE)

    # AI 半睡客分析
    output$ai_semi_dormant_customer <- renderUI({
      tryCatch({
        texts <- lang_texts(); if (is.null(texts)) return(div(class="alert alert-info","語系載入中…"))
        metrics <- retention_metrics(); if (is.null(metrics)) return(div(class="alert alert-info","資料準備中…"))
        dr <- dna_results(); if (is.null(dr) || is.null(dr$data_by_customer)) return(div(class="alert alert-warning","DNA 資料尚未就緒，請稍候"))
        data <- dr$data_by_customer

        cache_key <- get_ai_cache_key("semi_dormant", dr, current_language())
        semi_dormant_data <- data[data$nes_status == "S2", ]

        if(nrow(semi_dormant_data) > 0 && enable_gpt && !is.null(chat_api) && !is.null(prompts_df())) {
          prompts_now <- prompts_df(); if (is.null(prompts_now)) return(div(class="alert alert-info","AI 提示載入中…"))
          cached <- get_cached_ai(cache_key)
          if (!is.null(cached)) return(div(style="padding:15px; background:#fff8e1; border-radius:8px;", render_ai_result(cached)))

          avg_sleep_days <- mean(semi_dormant_data$r_value, na.rm = TRUE)
          avg_spend <- mean(semi_dormant_data$total_spent, na.rm = TRUE)
          last_purchase_cycle <- ifelse(mean(semi_dormant_data$f_value, na.rm = TRUE) > 0,
                                        avg_sleep_days / mean(semi_dormant_data$f_value, na.rm = TRUE),
                                        avg_sleep_days)

          tryCatch({
            result <- execute_gpt_request(
              var_id = "semi_dormant_customer_strategy",
              variables = list(
                semi_dormant_count = metrics$semi_sleeping_count,
                semi_dormant_ratio = round((metrics$semi_sleeping_count / nrow(data)) * 100, 1),
                avg_sleep_days = round(avg_sleep_days, 0),
                avg_spend = round(avg_spend, 0),
                last_purchase_cycle = round(last_purchase_cycle, 0)
              ),
              chat_api_function = chat_api,
              model = cfg_ai_model,
              prompts_df = prompts_now
            )
            if(!is.null(result)) {
              set_cached_ai(cache_key, result)
              return(div(style="padding:15px; background:#fff8e1; border-radius:8px;", render_ai_result(result)))
            }
          }, error=function(e){
            cat(get_text("server.notifications.ai_analysis_failed", "AI 分析失敗"), ":", e$message, "\n")
            return(div(class="alert alert-danger", paste("AI 分析失敗:", e$message)))
          })
        }

        div(
          style = "padding: 15px; background: #fff8e1; border-radius: 8px;",
          h5(get_text("server.ai_analysis.semi_dormant.title", "😴 半睡客戶分析"), style = "color: #f9a825;"),
          p(paste0(get_text("server.ai_analysis.semi_dormant.count_prefix", "共"), " ",
                   metrics$semi_sleeping_count, " ",
                   get_text("server.ai_analysis.semi_dormant.count_suffix", "位半睡客戶"))),
          h5(get_text("server.ai_analysis.semi_dormant.strategy_title", "⚡ 深度喚醒策略"), style = "color: #f9a825; margin-top: 15px;"),
          tags$ul(
            tags$li(get_text("server.ai_analysis.semi_dormant.suggestion1", "提供大額折扣券刺激回購")),
            tags$li(get_text("server.ai_analysis.semi_dormant.suggestion2", "推出買一送一或滿額贈活動")),
            tags$li(get_text("server.ai_analysis.semi_dormant.suggestion3", "多管道觸達提醒未結帳商品")),
            tags$li(get_text("server.ai_analysis.semi_dormant.suggestion4", "問卷調查了解流失原因並改善"))
          )
        )
      }, error=function(e){
        cat("❌ [Retention][semi_dormant] renderUI error:", e$message, "\n")
        return(div(class="alert alert-warning", paste("AI 區塊渲染錯誤:", e$message)))
      })
    }) %>% bindCache(dna_signature(), current_language())
    outputOptions(output, "ai_semi_dormant_customer", suspendWhenHidden = TRUE)

    # AI 瞌睡客分析
    output$ai_drowsy_customer <- renderUI({
      tryCatch({
        texts <- lang_texts(); if (is.null(texts)) return(div(class="alert alert-info","語系載入中…"))
        metrics <- retention_metrics(); if (is.null(metrics)) return(div(class="alert alert-info","資料準備中…"))
        dr <- dna_results(); if (is.null(dr) || is.null(dr$data_by_customer)) return(div(class="alert alert-warning","DNA 資料尚未就緒，請稍候"))
        data <- dr$data_by_customer

        cache_key <- get_ai_cache_key("drowsy", dr, current_language())
        drowsy_data <- data[data$nes_status == "S1", ]

        if(nrow(drowsy_data) > 0 && enable_gpt && !is.null(chat_api) && !is.null(prompts_df())) {
          prompts_now <- prompts_df(); if (is.null(prompts_now)) return(div(class="alert alert-info","AI 提示載入中…"))
          cached <- get_cached_ai(cache_key)
          if (!is.null(cached)) return(div(style="padding:15px; background:#fff3e0; border-radius:8px;", render_ai_result(cached)))

          tryCatch({
            result <- execute_gpt_request(
              var_id = "drowsy_customer_strategy",
              variables = list(
                drowsy_count = metrics$drowsy_count,
                drowsy_ratio = round((metrics$drowsy_count / nrow(data)) * 100, 1),
                avg_days = round(mean(drowsy_data$r_value, na.rm = TRUE), 0),
                avg_spend = round(mean(drowsy_data$total_spent, na.rm = TRUE), 0)
              ),
              chat_api_function = chat_api,
              model = cfg_ai_model,
              prompts_df = prompts_now
            )
            if(!is.null(result)) {
              set_cached_ai(cache_key, result)
              return(div(style="padding:15px; background:#fff3e0; border-radius:8px;", render_ai_result(result)))
            }
          }, error=function(e){
            cat(get_text("server.notifications.ai_analysis_failed", "AI 分析失敗"), ":", e$message, "\n")
            return(div(class="alert alert-danger", paste("AI 分析失敗:", e$message)))
          })
        }

        div(
          style = "padding: 15px; background: #fff3e0; border-radius: 8px;",
          h5(get_text("server.ai_analysis.drowsy.title", "😴 瞌睡客診斷"), style = "color: #f57c00;"),
          p(paste0(get_text("server.ai_analysis.drowsy.count_prefix", "共"), " ",
                   metrics$drowsy_count, " ",
                   get_text("server.ai_analysis.drowsy.count_suffix", "位瞌睡客戶"))),
          h5(get_text("server.ai_analysis.drowsy.strategy_title", "🔔 喚醒策略"), style = "color: #f57c00; margin-top: 15px;"),
          tags$ul(
            tags$li(get_text("server.ai_analysis.drowsy.suggestion1", "發送個人化關懷訊息")),
            tags$li(get_text("server.ai_analysis.drowsy.suggestion2", "提供限時獨家優惠")),
            tags$li(get_text("server.ai_analysis.drowsy.suggestion3", "推薦新品或熱銷商品"))
          )
        )
      }, error=function(e){
        cat("❌ [Retention][drowsy] renderUI error:", e$message, "\n")
        return(div(class="alert alert-warning", paste("AI 區塊渲染錯誤:", e$message)))
      })
    }) %>% bindCache(dna_signature(), current_language())
    outputOptions(output, "ai_drowsy_customer", suspendWhenHidden = TRUE)

    # AI 沉睡客分析
    output$ai_sleeping_customer <- renderUI({
      tryCatch({
        texts <- lang_texts(); if (is.null(texts)) return(div(class="alert alert-info","語系載入中…"))
        metrics <- retention_metrics(); if (is.null(metrics)) return(div(class="alert alert-info","資料準備中…"))
        dr <- dna_results(); if (is.null(dr) || is.null(dr$data_by_customer)) return(div(class="alert alert-warning","DNA 資料尚未就緒，請稍候"))
        data <- dr$data_by_customer

        cache_key <- get_ai_cache_key("sleeping", dr, current_language())
        sleeping_data <- data[data$nes_status == "S3", ]

        if(nrow(sleeping_data) > 0 && enable_gpt && !is.null(chat_api) && !is.null(prompts_df())) {
          prompts_now <- prompts_df(); if (is.null(prompts_now)) return(div(class="alert alert-info","AI 提示載入中…"))
          cached <- get_cached_ai(cache_key)
          if (!is.null(cached)) return(div(style="padding:15px; background:#ffebee; border-radius:8px;", render_ai_result(cached)))

          avg_sleep_days <- mean(sleeping_data$r_value, na.rm = TRUE)
          total_value <- sum(sleeping_data$total_spent, na.rm = TRUE)

          tryCatch({
            result <- execute_gpt_request(
              var_id = "sleeping_customer_strategy",
              variables = list(
                sleeping_count = metrics$sleeping_count,
                sleeping_ratio = round((metrics$sleeping_count / nrow(data)) * 100, 1),
                avg_sleep_days = round(avg_sleep_days, 0),
                total_value = round(total_value, 0)
              ),
              chat_api_function = chat_api,
              model = cfg_ai_model,
              prompts_df = prompts_now
            )
            if(!is.null(result)) {
              set_cached_ai(cache_key, result)
              return(div(style="padding:15px; background:#ffebee; border-radius:8px;", render_ai_result(result)))
            }
          }, error=function(e){
            cat(get_text("server.notifications.ai_analysis_failed", "AI 分析失敗"), ":", e$message, "\n")
            return(div(class="alert alert-danger", paste("AI 分析失敗:", e$message)))
          })
        }

        div(
          style = "padding: 15px; background: #ffebee; border-radius: 8px;",
          h5(get_text("server.ai_analysis.sleeping.title", "💤 沉睡客分析"), style = "color: #c62828;"),
          p(paste0(get_text("server.ai_analysis.sleeping.count_prefix", "共"), " ",
                   metrics$sleeping_count, " ",
                   get_text("server.ai_analysis.sleeping.count_suffix", "位沉睡客戶"))),
          h5(get_text("server.ai_analysis.sleeping.strategy_title", "♻️ 激活策略"), style = "color: #c62828; margin-top: 15px;"),
          tags$ul(
            tags$li(get_text("server.ai_analysis.sleeping.suggestion1", "發送「我們想念您」主題郵件")),
            tags$li(get_text("server.ai_analysis.sleeping.suggestion2", "提供大幅折扣或贈品")),
            tags$li(get_text("server.ai_analysis.sleeping.suggestion3", "重新介紹品牌價值與改進"))
          )
        )
      }, error=function(e){
        cat("❌ [Retention][sleeping] renderUI error:", e$message, "\n")
        return(div(class="alert alert-warning", paste("AI 區塊渲染錯誤:", e$message)))
      })
    }) %>% bindCache(dna_signature(), current_language())
    outputOptions(output, "ai_sleeping_customer", suspendWhenHidden = TRUE)

    # AI 結構分析
    output$ai_structure_analysis <- renderUI({
      tryCatch({
        texts <- lang_texts(); if (is.null(texts)) return(div(class="alert alert-info","語系載入中…"))
        metrics <- retention_metrics(); if (is.null(metrics)) return(div(class="alert alert-info","資料準備中…"))
        dr <- dna_results(); if (is.null(dr) || is.null(dr$data_by_customer)) return(div(class="alert alert-warning","DNA 資料尚未就緒，請稍候"))

        distribution <- metrics$status_distribution
        cache_key <- get_ai_cache_key("structure", dr, current_language())

        if(enable_gpt && !is.null(chat_api) && !is.null(prompts_df())) {
          prompts_now <- prompts_df(); if (is.null(prompts_now)) return(div(class="alert alert-info","AI 提示載入中…"))
          cached <- get_cached_ai(cache_key)
          if (!is.null(cached)) return(div(style="padding:15px; background:#f5f5f5; border-radius:8px;", render_ai_result(cached)))

          tryCatch({
            result <- execute_gpt_request(
              var_id = "customer_status_analysis",
              variables = list(
                n_count = distribution$count[distribution$nes_status == "N"],
                n_ratio = round(distribution$count[distribution$nes_status == "N"] / sum(distribution$count) * 100, 1),
                e0_count = distribution$count[distribution$nes_status == "E0"],
                e0_ratio = round(distribution$count[distribution$nes_status == "E0"] / sum(distribution$count) * 100, 1),
                s1_count = distribution$count[distribution$nes_status == "S1"],
                s1_ratio = round(distribution$count[distribution$nes_status == "S1"] / sum(distribution$count) * 100, 1),
                s2_count = distribution$count[distribution$nes_status == "S2"],
                s2_ratio = round(distribution$count[distribution$nes_status == "S2"] / sum(distribution$count) * 100, 1),
                s3_count = distribution$count[distribution$nes_status == "S3"],
                s3_ratio = round(distribution$count[distribution$nes_status == "S3"] / sum(distribution$count) * 100, 1)
              ),
              chat_api_function = chat_api,
              model = cfg_ai_model,
              prompts_df = prompts_now
            )
            if(!is.null(result)) {
              set_cached_ai(cache_key, result)
              return(div(style="padding:15px; background:#f5f5f5; border-radius:8px;", render_ai_result(result)))
            }
          }, error=function(e){
            cat(get_text("server.notifications.ai_analysis_failed", "AI 分析失敗"), ":", e$message, "\n")
            return(div(class="alert alert-danger", paste("AI 分析失敗:", e$message)))
          })
        }

        div(
          style = "padding: 15px; background: #f5f5f5; border-radius: 8px;",
          h5(get_text("server.ai_analysis.structure.title", "📊 結構健康度"), style = "color: #424242;"),
          p(get_text("server.ai_analysis.structure.description", "客戶結構分析顯示當前狀態分布")),
          h5(get_text("server.ai_analysis.structure.optimization_title", "🎯 優化目標"), style = "color: #424242; margin-top: 15px;"),
          tags$ul(
            tags$li(get_text("server.ai_analysis.structure.goal1", "提升主力客比率至 30% 以上")),
            tags$li(get_text("server.ai_analysis.structure.goal2", "降低沉睡客比率至 20% 以下")),
            tags$li(get_text("server.ai_analysis.structure.goal3", "維持新客獲取與流失的平衡"))
          )
        )
      }, error=function(e){
        cat("❌ [Retention][structure] renderUI error:", e$message, "\n")
        return(div(class="alert alert-warning", paste("AI 區塊渲染錯誤:", e$message)))
      })
    }) %>% bindCache(dna_signature(), current_language())
    outputOptions(output, "ai_structure_analysis", suspendWhenHidden = TRUE)

  })  # end moduleServer
}
