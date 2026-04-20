# 客戶增長模組 (Customer Acquisition & Coverage Module) - VitalSigns YAML Multi-Language Framework
# 監測客戶池擴張速度與結構健康
# Adapted to VitalSigns Framework - Supports Chinese/English/Japanese via YAML

library(shiny)
library(bs4Dash)
library(DT)
library(plotly)
library(dplyr)
library(lubridate)

# 載入提示和prompt系統
source("utils/hint_system.R")
source("utils/prompt_manager.R")

# Helper functions
`%+%` <- function(x, y) paste0(x, y)
`%||%` <- function(x, y) if (is.null(x)) y else x

# ============================================
# UI Function - Dynamic Rendering for Language Switching
# ============================================
acquisitionVitalsignsModuleUI <- function(id, module_config = NULL, lang_texts = NULL, enable_hints = TRUE) {
  ns <- NS(id)

  cat("🔍 [Acquisition Module UI] Called with id:", id, "\n")
  cat("  - Returning dynamic UI container for language switching support\n")

  # Return dynamic UI container that will be populated by server
  uiOutput(ns("dynamic_ui"))
}

# ============================================
# Server Function (VitalSigns Framework Pattern)
# ============================================
acquisitionVitalsignsModuleServer <- function(id, con = NULL, user_info = reactive(NULL),
                                               dna_module = NULL, time_series_data = NULL,
                                               module_config = NULL, lang_texts = NULL,
                                               enable_hints = TRUE, enable_gpt = TRUE, chat_api = NULL,
                                               api_config = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    cat("🚀 [Acquisition Server] Module initialized\n")

    # 從共用 API 設定讀取模型（fallback 到 gpt-5-nano）
    cfg_ai_model <- if (!is.null(api_config$default_model)) {
      api_config$default_model
    } else {
      "gpt-5-nano"
    }

    # ==============================================================
    # GPT 請求節流：避免同一份資料重複呼叫 API
    # 使用資料哈希 + 分析類型 做快取鍵，成功回傳後寫入緩存
    # (Following Retention module caching pattern)
    # ==============================================================
    ai_cache <- reactiveValues(data = list())

    get_ai_cache_key <- function(analysis_type, metrics_data, language = "zh_TW") {
      if (is.null(metrics_data)) return(NULL)

      # 產生穩定的資料哈希
      key_raw <- list(
        type = analysis_type,
        lang = language,
        data = digest::digest(metrics_data, algo = "sha256")
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
        cat("[Acquisition Cache] Hit:", substr(key, 1, 8), "\n")
        return(cached$content)
      }
      cat("[Acquisition Cache] Miss:", substr(key, 1, 8), "\n")
      NULL
    }

    set_cached_ai <- function(key, content) {
      if (is.null(key) || is.null(content)) return()
      ai_cache$data[[key]] <- list(content = content, cached_at = Sys.time(), used_at = Sys.time())
      cat("[Acquisition Cache] Stored:", substr(key, 1, 8), "\n")
    }

    # ===================================================================
    # Dynamic UI Rendering for Language Switching Support
    # ===================================================================
    output$dynamic_ui <- renderUI({
      req(lang_texts())  # Trigger re-render on language change

      # Helper function for reactive text retrieval
      get_text <- function(key, default = "") {
        texts <- lang_texts()
        if (is.null(texts)) return(default)

        # Navigate through nested structure
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

        fluidRow(
          # KPI 卡片
          column(3,
            bs4ValueBox(
              value = textOutput(ns("active_customers")),
              subtitle = tags$span(
                get_text("ui.kpis.active_customers", "顧客總數"),
                tags$i(
                  class = "fas fa-info-circle ml-1",
                  style = "font-size: 12px; color: #6c757d; cursor: help;",
                  title = get_hint("customer_acquisition_total", hints_df(), language = lang_texts()$language),
                  `data-toggle` = "tooltip",
                  `data-placement` = "top"
                )
              ),
              icon = icon("users"),
              color = "primary",
              width = 12
            )
          ),
          column(3,
            bs4ValueBox(
              value = textOutput(ns("cumulative_customers")),
              subtitle = tags$span(
                get_text("ui.kpis.cumulative_customers", "累積顧客數"),
                tags$i(
                  class = "fas fa-info-circle ml-1",
                  style = "font-size: 12px; color: #6c757d; cursor: help;",
                  title = get_hint("customer_acquisition_cumulative", hints_df(), language = lang_texts()$language),
                  `data-toggle` = "tooltip",
                  `data-placement` = "top"
                )
              ),
              icon = icon("user-plus"),
              color = "success",
              width = 12
            )
          ),
          column(3,
            bs4ValueBox(
              value = textOutput(ns("acquisition_rate")),
              subtitle = tags$span(
                get_text("ui.kpis.acquisition_rate", "顧客新增率"),
                tags$i(
                  class = "fas fa-info-circle ml-1",
                  style = "font-size: 12px; color: #6c757d; cursor: help;",
                  title = get_hint("customer_acquisition_rate", hints_df(), language = lang_texts()$language),
                  `data-toggle` = "tooltip",
                  `data-placement` = "top"
                )
              ),
              icon = icon("chart-line"),
              color = "info",
              width = 12
            )
          ),
          column(3,
            bs4ValueBox(
              value = textOutput(ns("net_change_rate")),
              subtitle = tags$span(
                get_text("ui.kpis.net_change_rate", "顧客變動率"),
                tags$i(
                  class = "fas fa-info-circle ml-1",
                  style = "font-size: 12px; color: #6c757d; cursor: help;",
                  title = get_hint("customer_change_rate", hints_df(), language = lang_texts()$language),
                  `data-toggle` = "tooltip",
                  `data-placement` = "top"
                )
              ),
              icon = icon("exchange-alt"),
              color = "warning",
              width = 12
            )
          )
        ),

        br(),

        fluidRow(
          # 客戶池結構分析
          column(6,
            bs4Card(
              title = get_text("ui.cards.customer_structure", "客戶池結構分析"),
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              plotlyOutput(ns("customer_structure"))
            )
          ),

          # 獲客漏斗分析
          column(6,
            bs4Card(
              title = get_text("ui.cards.acquisition_funnel", "獲客漏斗分析"),
              status = "info",
              solidHeader = TRUE,
              width = 12,
              plotlyOutput(ns("acquisition_funnel"))
            )
          )
        ),

        br(),

        fluidRow(
          # 客戶增長趨勢
          column(12,
            bs4Card(
              title = get_text("ui.cards.growth_trend", "客戶增長趨勢"),
              status = "success",
              solidHeader = TRUE,
              width = 12,
              helpText(get_text("ui.cards.growth_trend_notice", "注意：趨勢分析需要時間序列數據，目前顯示為靜態分析")),
              plotlyOutput(ns("growth_trend"))
            )
          )
        ),

        br(),

        fluidRow(
          # AI 分析建議
          column(12,
            bs4Card(
              title = get_text("ui.cards.ai_analysis", "🤖 AI 客戶增長分析"),
              status = "warning",
              solidHeader = TRUE,
              width = 12,
              h5(get_text("ui.cards.acquisition_analysis_title", "顧客新增率分析"), style = "color: #2c3e50; font-weight: bold;"),
              uiOutput(ns("ai_acquisition_analysis")),
              hr(),
              h5(get_text("ui.cards.change_rate_analysis_title", "顧客變動率分析"), style = "color: #2c3e50; font-weight: bold;"),
              uiOutput(ns("ai_change_rate_analysis"))
            )
          )
        ),

        br(),

        fluidRow(
          # 詳細數據表
          column(12,
            bs4Card(
              title = get_text("ui.cards.details", "客戶增長指標明細"),
              status = "secondary",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              width = 12,
              div(
                style = "margin-bottom: 10px;",
                downloadButton(ns("download_acquisition_details"), get_text("ui.buttons.download_details", "📥 下載完整客戶名單"),
                              class = "btn-success btn-sm")
              ),
              DTOutput(ns("acquisition_details"))
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
        cat("⚠️ [Acquisition Hints] Error getting language:", e$message, "\n")
        "zh_TW"  # fallback on error
      })

      # Load hints for current language from CSV
      cat("🔄 [Acquisition Hints] Loading hints from CSV for language:", current_lang, "\n")
      load_hints(language = current_lang, app_name = "vitalsigns")
    })

    # Module config validation (following Revenue Pulse pattern)
    if (is.null(module_config)) {
      cat("⚠️  [Acquisition] WARNING: module_config not provided, using empty config\n")
      module_config <- list()
    }

    if (is.null(lang_texts)) {
      cat("⚠️  [Acquisition] WARNING: lang_texts not provided\n")
    }

    # Helper function to safely get language text (reactive-aware)
    get_text <- function(key, default = "") {
      # If lang_texts is a function (reactive), call it; otherwise use it directly
      texts <- if (is.function(lang_texts)) {
        tryCatch(lang_texts(), error = function(e) lang_texts)
      } else {
        lang_texts
      }

      if (is.null(texts)) return(default)

      # Navigate through nested structure
      parts <- strsplit(key, "\\.")[[1]]
      value <- texts

      for (part in parts) {
        if (is.list(value) && part %in% names(value)) {
          value <- value[[part]]
        } else {
          return(default)
        }
      }

      # FIX: Don't convert lists to character - preserve structure for list-based content
      if (is.null(value)) {
        default
      } else if (is.list(value)) {
        value  # Return list as-is for array content (strategies, etc.)
      } else {
        as.character(value)  # Only convert scalar values
      }
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
        cat("⚠️ [Acquisition Prompts] Error getting language:", e$message, "\n")
        "zh_TW"
      })

      cat("📂 [Acquisition Prompts] Loading prompts for language:", current_lang, "\n")
      load_prompts(language = current_lang, app_name = "vitalsigns")
    })

    # NOTE: Hint system now uses inline YAML tooltips (no init_hint_system needed)
    # All hints come from lang_texts() via get_text("hints.*") calls

    # 取得 DNA 分析結果 - Defensive data access pattern (following Revenue Pulse pattern)
    dna_results <- reactive({
      req(dna_module)

      tryCatch({
        cat("🔍 [Acquisition] Accessing DNA module data...\n")

        # Step 1: Handle different types of dna_module input
        if (is.function(dna_module)) {
          # dna_module is a reactive function - call it
          result <- dna_module()
        } else if (is.list(dna_module) && "data" %in% names(dna_module)) {
          # dna_module is a list with a 'data' reactive - safely access and call
          data_source <- dna_module$data
          result <- if (is.reactive(data_source)) {
            data_source()
          } else if (is.function(data_source)) {
            data_source()
          } else {
            data_source
          }
        } else {
          # dna_module is the data itself
          result <- dna_module
        }

        # Step 2: Extract actual DNA results from result
        if (is.list(result) && "dna_results" %in% names(result)) {
          # Result has nested dna_results structure
          dna_data <- result$dna_results
        } else if (is.list(result) && "data_by_customer" %in% names(result)) {
          # Result is the dna_results object directly
          dna_data <- result
        } else {
          cat("❌ [Acquisition] ERROR: Unexpected data structure from DNA module\n")
          cat("  Result class:", class(result), "\n")
          if (is.list(result)) {
            cat("  Result names:", paste(names(result), collapse = ", "), "\n")
          }
          return(NULL)
        }

        # Step 3: Validate required structure
        if (is.null(dna_data$data_by_customer)) {
          cat("❌ [Acquisition] ERROR: data_by_customer not found in DNA results\n")
          return(NULL)
        }

        cat("✅ [Acquisition] Successfully loaded", nrow(dna_data$data_by_customer), "customer records\n")
        return(dna_data)

      }, error = function(e) {
        cat("❌ [Acquisition] ERROR in dna_results reactive:", e$message, "\n")
        return(NULL)
      })
    })
    
    # 計算客戶增長指標
    acquisition_metrics <- reactive({
      req(dna_results())
      
      data <- dna_results()$data_by_customer
      
      # 基本指標
      total_customers <- nrow(data)
      
      # 新客戶數（新客 N）
      new_customers <- sum(data$nes_status == "N", na.rm = TRUE)
      
      # 活躍客戶數（非沉睡客 S3）
      active_customers <- sum(data$nes_status != "S3", na.rm = TRUE)
      
      # 計算率
      metrics <- list(
        active_customers = active_customers,
        cumulative_customers = total_customers,
        new_customers = new_customers,
        acquisition_rate = (new_customers / total_customers) * 100,
        
        # 淨變動率（活躍客戶比例）
        net_change_rate = (active_customers / total_customers) * 100,
        
        # 客戶結構
        customer_structure = data %>%
          group_by(nes_status) %>%
          summarise(count = n()) %>%
          mutate(percentage = count / sum(count) * 100)
      )
      
      return(metrics)
    })

    # 如果 acquisition_metrics 結果更新則清除 AI 快取
    observeEvent(acquisition_metrics(), {
      ai_cache$data <- list()
      cat("🧹 [Acquisition] Metrics changed -> AI cache cleared\n")
    })

    # KPI 顯示
    output$active_customers <- renderText({
      metrics <- acquisition_metrics()
      format(metrics$active_customers, big.mark = ",")
    })
    
    output$cumulative_customers <- renderText({
      metrics <- acquisition_metrics()
      format(metrics$cumulative_customers, big.mark = ",")
    })
    
    output$acquisition_rate <- renderText({
      metrics <- acquisition_metrics()
      paste0(round(metrics$acquisition_rate, 1), "%")
    })
    
    output$net_change_rate <- renderText({
      metrics <- acquisition_metrics()
      paste0(round(metrics$net_change_rate, 1), "%")
    })
    
    # 客戶池結構圖
    output$customer_structure <- renderPlotly({
      req(acquisition_metrics())

      structure <- acquisition_metrics()$customer_structure

      # 定義顏色和標籤 (使用 YAML 語言鍵)
      structure <- structure %>%
        mutate(
          label = case_when(
            nes_status == "N" ~ get_text("server.customer_types.N", "新客戶"),
            nes_status == "E0" ~ get_text("server.customer_types.E0", "主力客戶"),
            nes_status == "S1" ~ get_text("server.customer_types.S1", "瞌睡客戶"),
            nes_status == "S2" ~ get_text("server.customer_types.S2", "半睡客戶"),
            nes_status == "S3" ~ get_text("server.customer_types.S3", "沉睡客戶"),
            TRUE ~ get_text("server.customer_types.other", "其他")
          ),
          color = case_when(
            nes_status == "N" ~ "#3498db",
            nes_status == "E0" ~ "#2ecc71",
            nes_status == "S1" ~ "#f39c12",
            nes_status == "S2" ~ "#e67e22",
            nes_status == "S3" ~ "#e74c3c",
            TRUE ~ "#95a5a6"
          )
        )

      plot_ly(structure,
              labels = ~label,
              values = ~count,
              type = 'pie',
              marker = list(colors = ~color),
              textposition = 'inside',
              textinfo = 'label+percent',
              hovertemplate = "%{label}<br>%{value}<br>%{percent}<extra></extra>") %>%
        layout(
          title = list(text = get_text("server.charts.structure.title", "客戶狀態分布"), font = list(size = 16)),
          showlegend = TRUE,
          legend = list(x = 1, y = 0.5)
        )
    })
    
    # 獲客漏斗分析（移除潛在客戶）
    output$acquisition_funnel <- renderPlotly({
      req(dna_results())
      data <- dna_results()$data_by_customer

      # 創建漏斗數據（不包含潛在客戶，使用 YAML 語言鍵）
      funnel_data <- data.frame(
        Stage = c(
          get_text("server.charts.funnel.stages.first_purchase", "首次購買"),
          get_text("server.charts.funnel.stages.repeat_purchase", "再次購買"),
          get_text("server.charts.funnel.stages.multiple_purchase", "多次購買"),
          get_text("server.charts.funnel.stages.core_customer", "主力客戶")
        ),
        Count = c(
          sum(data$times >= 1, na.rm = TRUE),  # 至少購買一次
          sum(data$times >= 2, na.rm = TRUE),  # 至少購買兩次
          sum(data$times >= 5, na.rm = TRUE),  # 至少購買五次
          sum(data$nes_status == "E0", na.rm = TRUE)  # 主力客戶
        )
      )

      funnel_data$Percentage <- round(funnel_data$Count / funnel_data$Count[1] * 100, 1)

      plot_ly(funnel_data,
              type = "funnel",
              y = ~Stage,
              x = ~Count,
              textposition = "inside",
              text = ~paste0(Count, " (", Percentage, "%)"),
              marker = list(color = c("#3498db", "#2ecc71", "#f39c12", "#e74c3c")),
              hovertemplate = "%{y}<br>%{x}<br>%{text}<extra></extra>") %>%
        layout(
          title = list(text = get_text("server.charts.funnel.title", "客戶轉化漏斗"), font = list(size = 16)),
          yaxis = list(categoryorder = "trace")
        )
    })
    
    # 客戶增長趨勢
    output$growth_trend <- renderPlotly({
      # 優先使用時間序列數據
      if (!is.null(time_series_data)) {
        monthly_data <- time_series_data$monthly_data()

        if (!is.null(monthly_data) && nrow(monthly_data) > 0) {
          # 顯示客戶數量的時間趨勢 (使用 YAML 語言鍵)
          plot_ly(monthly_data, x = ~period_date) %>%
            add_trace(
              y = ~customers,
              type = 'scatter',
              mode = 'lines+markers',
              name = get_text("server.charts.growth.series.monthly_customers", "月度客戶數"),
              line = list(color = '#3498db', width = 3),
              marker = list(size = 8),
              hovertemplate = "%{x|%Y-%m}<br>%{y:,.0f}<extra></extra>"
            ) %>%
            add_trace(
              y = ~customer_growth,
              type = 'bar',
              name = get_text("server.charts.growth.series.growth_rate", "成長率 (%)"),
              yaxis = 'y2',
              marker = list(
                color = ~ifelse(customer_growth >= 0, '#2ecc71', '#e74c3c')
              ),
              hovertemplate = "%{x|%Y-%m}<br>%{y:.1f}%<extra></extra>"
            ) %>%
            layout(
              title = list(text = get_text("server.charts.growth.title", "月度客戶增長趨勢"), font = list(size = 16)),
              xaxis = list(
                title = get_text("server.charts.growth.x_axis", "月份"),
                tickformat = "%Y-%m"
              ),
              yaxis = list(
                title = get_text("server.charts.growth.y_axis", "客戶數"),
                side = 'left'
              ),
              yaxis2 = list(
                title = get_text("server.charts.growth.y_axis_secondary", "成長率 (%)"),
                overlaying = 'y',
                side = 'right'
              ),
              legend = list(x = 0.1, y = 0.95),
              hovermode = 'x unified'
            )
        } else {
          # 沒有時間序列數據時顯示靜態分析
          display_static_analysis()
        }
      } else {
        # 沒有時間序列模組時顯示靜態分析
        display_static_analysis()
      }
    })
    
    # 輔助函數：顯示靜態分析
    display_static_analysis <- function() {
      req(dna_results())
      data <- dna_results()$data_by_customer

      # 按購買次數分組 (使用 YAML 語言鍵)
      growth_data <- data %>%
        mutate(purchase_group = case_when(
          times == 1 ~ get_text("server.charts.static.groups.once", "1次"),
          times == 2 ~ get_text("server.charts.static.groups.twice", "2次"),
          times >= 3 & times <= 5 ~ get_text("server.charts.static.groups.three_to_five", "3-5次"),
          times >= 6 & times <= 10 ~ get_text("server.charts.static.groups.six_to_ten", "6-10次"),
          times > 10 ~ get_text("server.charts.static.groups.more_than_ten", "10次以上")
        )) %>%
        group_by(purchase_group) %>%
        summarise(count = n()) %>%
        mutate(purchase_group = factor(purchase_group,
                                     levels = c(
                                       get_text("server.charts.static.groups.once", "1次"),
                                       get_text("server.charts.static.groups.twice", "2次"),
                                       get_text("server.charts.static.groups.three_to_five", "3-5次"),
                                       get_text("server.charts.static.groups.six_to_ten", "6-10次"),
                                       get_text("server.charts.static.groups.more_than_ten", "10次以上")
                                     )))

      plot_ly(growth_data,
              x = ~purchase_group,
              y = ~count,
              type = 'bar',
              marker = list(color = '#3498db'),
              text = ~count,
              textposition = "outside",
              hovertemplate = "%{x}<br>%{y}<extra></extra>") %>%
        layout(
          title = list(text = get_text("server.charts.static.title", "客戶購買頻次分布"), font = list(size = 16)),
          xaxis = list(title = get_text("server.charts.static.x_axis", "購買次數")),
          yaxis = list(title = get_text("server.charts.static.y_axis", "客戶數")),
          showlegend = FALSE
        )
    }
    
    # 詳細數據表
    output$acquisition_details <- renderDT({
      req(dna_results())
      data <- dna_results()$data_by_customer

      # 準備顯示數據 (使用 YAML 語言鍵)
      display_data <- data %>%
        select(customer_id, nes_status, times, total_spent, r_value) %>%
        mutate(
          customer_type = case_when(
            nes_status == "N" ~ get_text("server.customer_types.N", "新客戶"),
            nes_status == "E0" ~ get_text("server.customer_types.E0", "主力客戶"),
            nes_status == "S1" ~ get_text("server.customer_types.S1", "瞌睡客戶"),
            nes_status == "S2" ~ get_text("server.customer_types.S2", "半睡客戶"),
            nes_status == "S3" ~ get_text("server.customer_types.S3", "沉睡客戶"),
            TRUE ~ get_text("server.customer_types.other", "其他")
          ),
          days_since_last = round(r_value, 0)
        ) %>%
        select(
          customer_id,
          customer_type,
          times,
          total_spent,
          days_since_last
        ) %>%
        rename(
          !!get_text("tables.details.columns.customer_id", "客戶ID") := customer_id,
          !!get_text("tables.details.columns.customer_type", "客戶類型") := customer_type,
          !!get_text("tables.details.columns.times", "購買次數") := times,
          !!get_text("tables.details.columns.total_spent", "總消費金額") := total_spent,
          !!get_text("tables.details.columns.days_since_last", "距上次購買天數") := days_since_last
        )
      
      datatable(
        display_data,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          order = list(list(2, 'desc')),  # 按購買次數降序
          dom = 'frtip',  # 移除 B (buttons)
          columnDefs = list(
            list(className = 'dt-right', targets = c(2, 3, 4))
          )
        ),
        rownames = FALSE
      )

      # Apply formatting using the translated column names
      total_spent_col <- get_text("tables.details.columns.total_spent", "總消費金額")
      days_since_col <- get_text("tables.details.columns.days_since_last", "距上次購買天數")

      datatable_result <- datatable(
        display_data,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          order = list(list(2, 'desc')),
          dom = 'frtip',
          columnDefs = list(
            list(className = 'dt-right', targets = c(2, 3, 4))
          )
        ),
        rownames = FALSE
      )

      # Apply formatting if columns exist
      if (total_spent_col %in% names(display_data)) {
        datatable_result <- formatCurrency(datatable_result, total_spent_col, "$")
      }
      if (days_since_col %in% names(display_data)) {
        datatable_result <- formatRound(datatable_result, days_since_col, 0)
      }

      datatable_result
    })
    
    # 下載客戶詳細資料
    output$download_acquisition_details <- downloadHandler(
      filename = function() {
        paste0("customer_acquisition_details_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        req(dna_results())
        data <- dna_results()$data_by_customer

        # 準備完整資料 (使用 YAML 語言鍵)
        export_data <- data %>%
          select(customer_id, nes_status, times, total_spent, r_value, f_value, m_value) %>%
          mutate(
            customer_type = case_when(
              nes_status == "N" ~ get_text("server.customer_types.N", "新客戶"),
              nes_status == "E0" ~ get_text("server.customer_types.E0", "主力客戶"),
              nes_status == "S1" ~ get_text("server.customer_types.S1", "瞌睡客戶"),
              nes_status == "S2" ~ get_text("server.customer_types.S2", "半睡客戶"),
              nes_status == "S3" ~ get_text("server.customer_types.S3", "沉睡客戶"),
              TRUE ~ get_text("server.customer_types.other", "其他")
            ),
            days_since_last = round(r_value, 0)
          ) %>%
          select(
            customer_id,
            customer_type,
            times,
            total_spent,
            days_since_last,
            f_value,
            m_value
          ) %>%
          rename(
            !!get_text("tables.download.columns.customer_id", "客戶ID") := customer_id,
            !!get_text("tables.download.columns.customer_type", "客戶類型") := customer_type,
            !!get_text("tables.download.columns.times", "購買次數") := times,
            !!get_text("tables.download.columns.total_spent", "總消費金額") := total_spent,
            !!get_text("tables.download.columns.days_since_last", "距上次購買天數") := days_since_last,
            !!get_text("tables.download.columns.f_value", "購買頻率") := f_value,
            !!get_text("tables.download.columns.m_value", "平均單次消費") := m_value
          )

        write.csv(export_data, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )
    
    # =====================================================================
    # AI 分析功能區塊
    # Note: AI analysis text is generated dynamically using prompts from
    # the prompt_manager system. The fallback HTML templates below use
    # YAML language keys for all static text.
    # =====================================================================

    # AI 分析：顧客新增率
    ai_acquisition_analysis <- reactive({
      # 只依賴 acquisition_metrics，語言切換時不重新觸發 API
      req(acquisition_metrics())

      # 使用 isolate 獲取語言（不觸發反應式依賴）
      current_lang <- isolate({
        texts <- lang_texts()
        if (!is.null(texts) && !is.null(texts$language)) texts$language else "zh_TW"
      })

      if(enable_gpt && !is.null(chat_api) && !is.null(prompts_df())) {
        metrics <- acquisition_metrics()

        # 生成快取資料
        cache_data <- list(
          new_customers = metrics$new_customers,
          total_customers = metrics$cumulative_customers,
          acquisition_rate = round(metrics$acquisition_rate, 1),
          active_customers = metrics$active_customers
        )

        # 生成快取鍵
        cache_key <- get_ai_cache_key("acquisition_analysis", cache_data, current_lang)

        # 檢查快取
        cached_result <- get_cached_ai(cache_key)
        if (!is.null(cached_result)) {
          cat("[Acquisition AI] 使用快取的 AI 分析\n")
          return(cached_result)
        }

        # 準備 AI 分析數據 (使用 YAML 語言鍵)
        analysis_data <- sprintf(
          "%s%d
%s%d
%s%.1f%%
%s%d",
          get_text("server.ai.data_fields.new_customers_label", "新客戶數量："),
          metrics$new_customers,
          get_text("server.ai.data_fields.total_customers_label", "總客戶數："),
          metrics$cumulative_customers,
          get_text("server.ai.data_fields.acquisition_rate_label", "顧客新增率："),
          metrics$acquisition_rate,
          get_text("server.ai.data_fields.active_customers_label", "活躍客戶數："),
          metrics$active_customers
        )

        # 獲取 prompt
        prompts <- prompts_df()  # Call reactive
        prompt_template <- prompts[prompts$var_id == "customer_acquisition_rate_analysis", "prompt"][1]

        if(!is.na(prompt_template) && nchar(prompt_template) > 0) {
          # 替換模板變數
          prompt <- gsub("\\{new_customers\\}", as.character(metrics$new_customers), prompt_template)
          prompt <- gsub("\\{total_customers\\}", as.character(metrics$cumulative_customers), prompt)
          prompt <- gsub("\\{acquisition_rate\\}", sprintf("%.1f", metrics$acquisition_rate), prompt)
          prompt <- gsub("\\{active_customers\\}", as.character(metrics$active_customers), prompt)

          # 呼叫 AI API
          tryCatch({
            response <- chat_api(prompt)
            # 儲存快取
            set_cached_ai(cache_key, response)
            return(response)
          }, error = function(e) {
            error_msg <- get_text("server.ai.error", "AI 分析發生錯誤: {message}")
            return(gsub("\\{message\\}", e$message, error_msg))
          })
        }
      }

      # 預設分析（無 GPT 時） - 使用 YAML 語言鍵構建 HTML
      metrics <- acquisition_metrics()

      # 取得所有需要的文本
      title <- get_text("server.ai.acquisition_analysis.title", "📊 新客獲取效率評估")
      new_cust_label <- get_text("server.ai.acquisition_analysis.metrics.new_customers", "新客戶數量：<strong>{value} 位</strong>")
      acq_rate_label <- get_text("server.ai.acquisition_analysis.metrics.acquisition_rate", "顧客新增率：<strong>{value}%</strong>")
      benchmark <- get_text("server.ai.acquisition_analysis.metrics.industry_benchmark", "行業基準：一般電商新客率 15-25%")
      eval_prefix <- get_text("server.ai.acquisition_analysis.evaluation_prefix", "評估：")
      strategy_title <- get_text("server.ai.acquisition_analysis.strategy_title", "💡 策略建議")

      # 評估
      evaluation <- if (metrics$acquisition_rate < 15) {
        get_text("server.ai.acquisition_analysis.evaluation.low", "低於行業平均，需要加強獲客")
      } else if (metrics$acquisition_rate > 25) {
        get_text("server.ai.acquisition_analysis.evaluation.high", "高於行業平均，表現優異")
      } else {
        get_text("server.ai.acquisition_analysis.evaluation.normal", "符合行業水準")
      }

      # 策略建議 - 從 YAML 讀取（支持列表結構）
      # DEBUG: Check current language (使用 isolate 避免觸發反應式依賴)
      current_texts <- isolate(lang_texts())
      current_lang_debug <- if (!is.null(current_texts$language)) current_texts$language else "UNKNOWN"
      cat("🌍 [Acquisition AI] Current language:", current_lang_debug, "\n")

      strategies <- if (metrics$acquisition_rate < 15) {
        strat_list <- get_text("server.ai.acquisition_analysis.strategies.low_rate", NULL)
        cat("🔍 [Acquisition AI] low_rate strategies - class:", class(strat_list), "length:", length(strat_list), "\n")
        if (!is.null(strat_list) && length(strat_list) > 0) {
          cat("   First item:", strat_list[[1]], "\n")
        }
        # FIX: Accept both list and character vector (YAML can return either)
        if (!is.null(strat_list) && length(strat_list) >= 2) {
          # Convert to list if it's a vector
          if (is.character(strat_list) && !is.list(strat_list)) {
            cat("✅ [Acquisition AI] Using character vector from YAML (converting to list)\n")
            as.list(strat_list)
          } else {
            cat("✅ [Acquisition AI] Using list from YAML\n")
            strat_list
          }
        } else {
          cat("⚠️  [Acquisition AI] YAML strategies not found or insufficient length\n")
          fallback_list <- list(
            get_text("server.ai.acquisition_analysis.recommendations.increase_budget", "Consider increasing marketing budget"),
            get_text("server.ai.acquisition_analysis.recommendations.new_customer_offers", "Launch exclusive offers")
          )
          fallback_list
        }
      } else {
        strat_list <- get_text("server.ai.acquisition_analysis.strategies.normal_rate", NULL)
        cat("🔍 [Acquisition AI] normal_rate strategies - class:", class(strat_list), "length:", length(strat_list), "\n")
        # FIX: Accept both list and character vector
        if (!is.null(strat_list) && length(strat_list) >= 2) {
          if (is.character(strat_list) && !is.list(strat_list)) {
            cat("✅ [Acquisition AI] Using character vector from YAML (converting to list)\n")
            as.list(strat_list)
          } else {
            cat("✅ [Acquisition AI] Using list from YAML\n")
            strat_list
          }
        } else {
          cat("⚠️  [Acquisition AI] YAML strategies not found or insufficient length\n")
          fallback_list <- list(
            get_text("server.ai.acquisition_analysis.recommendations.maintain_strategy", "Maintain current strategy"),
            get_text("server.ai.acquisition_analysis.recommendations.focus_lifetime_value", "Focus on lifetime value")
          )
          fallback_list
        }
      }

      # 替換變數 (YAML 模板已包含單位，這裡只替換數值)
      new_cust_text <- gsub("\\{value\\}", as.character(metrics$new_customers), new_cust_label)
      acq_rate_text <- gsub("\\{value\\}", sprintf("%.1f", metrics$acquisition_rate), acq_rate_label)

      # 構建 HTML（完全使用 YAML 語言鍵）
      analysis <- sprintf(
        "<div style='padding: 15px; background: #f8f9fa; border-radius: 8px;'>
        <h6 style='color: #2c3e50; margin-bottom: 10px;'>%s</h6>
        <ul style='margin: 0; padding-left: 20px;'>
          <li>%s</li>
          <li>%s</li>
          <li>%s</li>
          <li>%s%s</li>
        </ul>
        <h6 style='color: #2c3e50; margin-top: 15px; margin-bottom: 10px;'>%s</h6>
        <ul style='margin: 0; padding-left: 20px;'>
          <li>%s</li>
          <li>%s</li>
        </ul>
        </div>",
        title,
        new_cust_text,
        acq_rate_text,
        benchmark,
        eval_prefix,
        evaluation,
        strategy_title,
        strategies[[1]],
        strategies[[2]]
      )
      
      return(HTML(analysis))
    })
    
    # AI 分析：顧客變動率
    ai_change_rate_analysis <- reactive({
      # 只依賴 acquisition_metrics，語言切換時不重新觸發 API
      req(acquisition_metrics())

      # 使用 isolate 獲取語言（不觸發反應式依賴）
      current_lang <- isolate({
        texts <- lang_texts()
        if (!is.null(texts) && !is.null(texts$language)) texts$language else "zh_TW"
      })

      if(enable_gpt && !is.null(chat_api) && !is.null(prompts_df())) {
        metrics <- acquisition_metrics()

        # 準備客戶結構數據 (使用 YAML 語言鍵)
        structure_text <- paste(
          sapply(1:nrow(metrics$customer_structure), function(i) {
            row <- metrics$customer_structure[i,]
            status_name <- switch(as.character(row$nes_status),
                                "N" = get_text("server.customer_types.N", "新客戶"),
                                "E0" = get_text("server.customer_types.E0", "主力客戶"),
                                "S1" = get_text("server.customer_types.S1", "瞌睡客戶"),
                                "S2" = get_text("server.customer_types.S2", "半睡客戶"),
                                "S3" = get_text("server.customer_types.S3", "沉睡客戶"),
                                get_text("server.customer_types.other", "其他"))
            status_format <- get_text("server.ai.data_fields.status_format", "%s: %d位 (%.1f%%)")
            sprintf(status_format, status_name, row$count, row$percentage)
          }),
          collapse = ", "
        )

        # 準備 AI 分析數據 (使用 YAML 語言鍵)
        analysis_data <- sprintf(
          "%s%d
%s%d
%s%.1f%%
%s%s",
          get_text("server.ai.data_fields.active_customers_label", "活躍客戶數："),
          metrics$active_customers,
          get_text("server.ai.data_fields.total_customers_label", "總客戶數："),
          metrics$cumulative_customers,
          get_text("server.ai.data_fields.net_change_rate_label", "顧客變動率："),
          metrics$net_change_rate,
          get_text("server.ai.data_fields.customer_structure_label", "客戶結構："),
          structure_text
        )

        # 獲取 prompt
        prompts <- prompts_df()  # Call reactive
        prompt_template <- prompts[prompts$var_id == "customer_change_rate_analysis", "prompt"][1]

        if(!is.na(prompt_template) && nchar(prompt_template) > 0) {
          # 替換模板變數
          prompt <- gsub("\\{active_customers\\}", as.character(metrics$active_customers), prompt_template)
          prompt <- gsub("\\{total_customers\\}", as.character(metrics$cumulative_customers), prompt)
          prompt <- gsub("\\{net_change_rate\\}", sprintf("%.1f", metrics$net_change_rate), prompt)
          prompt <- gsub("\\{customer_structure\\}", structure_text, prompt)

          # === 快取檢查 ===
          cache_data <- list(
            active_customers = metrics$active_customers,
            cumulative_customers = metrics$cumulative_customers,
            net_change_rate = round(metrics$net_change_rate, 2),
            customer_structure = as.character(structure_text)
          )
          cache_key <- get_ai_cache_key("change_rate_analysis", cache_data, current_lang)

          cached_result <- get_cached_ai(cache_key)
          if (!is.null(cached_result)) {
            cat("[Acquisition] 使用快取的顧客變動率 AI 分析\n")
            return(cached_result)
          }

          # 呼叫 AI API
          tryCatch({
            response <- chat_api(prompt)
            set_cached_ai(cache_key, response)
            return(response)
          }, error = function(e) {
            error_msg <- get_text("server.ai.error", "AI 分析發生錯誤: {message}")
            return(gsub("\\{message\\}", e$message, error_msg))
          })
        }
      }

      # 預設分析（無 GPT 時） - 使用 YAML 語言鍵構建 HTML
      metrics <- acquisition_metrics()

      # 取得所有需要的文本
      title <- get_text("server.ai.change_rate_analysis.title", "🔍 客戶池健康度診斷")
      active_rate_label <- get_text("server.ai.change_rate_analysis.metrics.active_rate", "活躍率：<strong>{value}%</strong>")
      health_status_label <- get_text("server.ai.change_rate_analysis.metrics.health_status", "健康狀態：<strong>{status}</strong>")
      dormant_rate_label <- get_text("server.ai.change_rate_analysis.metrics.dormant_rate", "沉睡客戶占比：<strong>{value}%</strong>")
      prevention_title <- get_text("server.ai.change_rate_analysis.prevention_title", "🛡️ 風險預防措施")

      # 健康狀態評估
      health_status <- if (metrics$net_change_rate > 70) {
        get_text("server.ai.change_rate_analysis.health_levels.healthy", "健康")
      } else if (metrics$net_change_rate > 50) {
        get_text("server.ai.change_rate_analysis.health_levels.warning", "警示")
      } else {
        get_text("server.ai.change_rate_analysis.health_levels.danger", "危險")
      }

      # 預防策略 - 從 YAML 讀取（支持列表結構）
      prevention_strategies <- if (metrics$net_change_rate < 50) {
        strat_list <- get_text("server.ai.change_rate_analysis.prevention_strategies.high_risk", NULL)
        cat("🔍 [Acquisition AI] high_risk prevention - class:", class(strat_list), "length:", length(strat_list), "\n")
        # FIX: Accept both list and character vector
        if (!is.null(strat_list) && length(strat_list) >= 3) {
          if (is.character(strat_list) && !is.list(strat_list)) {
            cat("✅ [Acquisition AI] Using character vector from YAML (converting to list)\n")
            as.list(strat_list)
          } else {
            cat("✅ [Acquisition AI] Using list from YAML\n")
            strat_list
          }
        } else {
          cat("⚠️  [Acquisition AI] YAML prevention strategies not found or insufficient length\n")
          fallback_list <- list(
            get_text("server.ai.change_rate_analysis.risk_prevention.wake_up_plan", "Launch reactivation campaign"),
            get_text("server.ai.change_rate_analysis.risk_prevention.exclusive_offers", "Offer exclusive deals"),
            get_text("server.ai.change_rate_analysis.risk_prevention.early_warning", "Establish early warning")
          )
          fallback_list
        }
      } else {
        strat_list <- get_text("server.ai.change_rate_analysis.prevention_strategies.low_risk", NULL)
        cat("🔍 [Acquisition AI] low_risk prevention - class:", class(strat_list), "length:", length(strat_list), "\n")
        # FIX: Accept both list and character vector
        if (!is.null(strat_list) && length(strat_list) >= 3) {
          if (is.character(strat_list) && !is.list(strat_list)) {
            cat("✅ [Acquisition AI] Using character vector from YAML (converting to list)\n")
            as.list(strat_list)
          } else {
            cat("✅ [Acquisition AI] Using list from YAML\n")
            strat_list
          }
        } else {
          cat("⚠️  [Acquisition AI] YAML prevention strategies not found or insufficient length\n")
          fallback_list <- list(
            get_text("server.ai.change_rate_analysis.risk_prevention.monitor_activity", "Monitor customer activity"),
            get_text("server.ai.change_rate_analysis.risk_prevention.strengthen_interaction", "Strengthen engagement"),
            get_text("server.ai.change_rate_analysis.risk_prevention.early_warning", "Establish early warning")
          )
          fallback_list
        }
      }

      # 替換變數
      active_rate_text <- gsub("\\{value\\}", sprintf("%.1f", metrics$net_change_rate), active_rate_label)
      health_status_text <- gsub("\\{status\\}", health_status, health_status_label)
      dormant_rate_text <- gsub("\\{value\\}", sprintf("%.1f", 100 - metrics$net_change_rate), dormant_rate_label)

      # 構建 HTML（完全使用 YAML 語言鍵）
      analysis <- sprintf(
        "<div style='padding: 15px; background: #fff3e0; border-radius: 8px;'>
        <h6 style='color: #e65100; margin-bottom: 10px;'>%s</h6>
        <ul style='margin: 0; padding-left: 20px;'>
          <li>%s</li>
          <li>%s</li>
          <li>%s</li>
        </ul>
        <h6 style='color: #e65100; margin-top: 15px; margin-bottom: 10px;'>%s</h6>
        <ul style='margin: 0; padding-left: 20px;'>
          <li>%s</li>
          <li>%s</li>
          <li>%s</li>
        </ul>
        </div>",
        title,
        active_rate_text,
        health_status_text,
        dormant_rate_text,
        prevention_title,
        prevention_strategies[[1]],
        prevention_strategies[[2]],
        prevention_strategies[[3]]
      )
      
      return(HTML(analysis))
    })
    
    # 輸出 AI 分析結果
    output$ai_acquisition_analysis <- renderUI({
      req(lang_texts())  # Force re-render on language change
      ai_acquisition_analysis()
    })

    output$ai_change_rate_analysis <- renderUI({
      req(lang_texts())  # Force re-render on language change
      ai_change_rate_analysis()
    })
    
  })
}