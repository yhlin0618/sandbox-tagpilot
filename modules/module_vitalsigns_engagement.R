################################################################################
# VitalSigns Engagement Module - YAML Multi-Language Framework
################################################################################
# 活躍轉化模組 (Engagement Flow Module)
# 掌握互動 → 再購 → 喚醒的節奏深度
#
# Version: 2.0.0 (YAML Framework)
# Last Updated: 2025-10-15
# Framework: VitalSigns + YAML Multi-Language + CSV Hints
################################################################################

library(shiny)
library(bs4Dash)
library(DT)
library(plotly)
library(dplyr)
library(markdown)

# Source utilities
source("utils/hint_system.R", encoding = "UTF-8")
source("utils/prompt_manager.R", encoding = "UTF-8")

# 載入 AI 載入管理器
if(file.exists("utils/ai_loading_manager.R")) {
  source("utils/ai_loading_manager.R", encoding = "UTF-8")
}

################################################################################
# UI Function
################################################################################

#' VitalSigns Engagement Module UI
#' @param id Module namespace ID
#' @param module_config Module configuration from YAML (optional)
#' @param lang_texts Static language texts (NOT used in new pattern)
#' @param enable_hints Enable hint system (default: TRUE)
#' @return Dynamic UI container for language switching support
engagementVitalsignsModuleUI <- function(id, module_config = NULL, lang_texts = NULL, enable_hints = TRUE) {
  ns <- NS(id)

  cat("🔍 [Engagement Module UI] Called with id:", id, "\n")
  cat("  - Returning dynamic UI container for language switching support\n")

  # Return dynamic UI container that will be populated by server
  uiOutput(ns("dynamic_ui"))
}

# Server Function
engagementVitalsignsModuleServer <- function(id, con = NULL, user_info = reactive(NULL),
                                       dna_module = NULL,
                                       module_config = NULL, lang_texts = NULL,
                                       enable_hints = TRUE, enable_gpt = FALSE,
                                       chat_api = NULL, api_config = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    cat("🚀 [Engagement Server] Module initialized\n")

    # 從共用 API 設定讀取模型（fallback 到 gpt-5-nano）
    cfg_ai_model <- if (!is.null(api_config$default_model)) {
      api_config$default_model
    } else {
      "gpt-5-nano"
    }

    # Make lang_texts reactive if it's not already
    lang_texts_reactive <- if (is.reactive(lang_texts)) {
      lang_texts
    } else {
      reactive(lang_texts)
    }

    # Reactive hints dataframe
    hints_df <- reactive({
      texts <- lang_texts_reactive()
      current_lang <- if (!is.null(texts) && !is.null(texts$language)) texts$language else "zh_TW"

      if (!enable_hints) return(NULL)

      tryCatch({
        hints <- load_hints(language = current_lang, app_name = "vitalsigns")
        cat("✅ [Engagement Server] Loaded", nrow(hints), "hints for", current_lang, "\n")
        hints
      }, error = function(e) {
        cat("⚠️ [Engagement Server] Failed to load hints:", e$message, "\n")
        NULL
      })
    })

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
        cat("[Engagement Cache] Hit:", substr(key, 1, 8), "\n")
        return(cached$content)
      }
      cat("[Engagement Cache] Miss:", substr(key, 1, 8), "\n")
      NULL
    }

    set_cached_ai <- function(key, content) {
      if (is.null(key) || is.null(content)) return()
      ai_cache$data[[key]] <- list(content = content, cached_at = Sys.time(), used_at = Sys.time())
      cat("[Engagement Cache] Stored:", substr(key, 1, 8), "\n")
    }

    # ===================================================================
    # Helper Function for Text Retrieval - MUST be outside renderUI!
    # This allows ALL render functions (renderText, renderPlotly, etc.) to access it
    # ===================================================================
    get_text <- function(key, default = "") {
      texts <- lang_texts_reactive()
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

    # ===================================================================
    # Dynamic UI Rendering for Language Switching Support
    # ===================================================================
    output$dynamic_ui <- renderUI({
      # 不使用 req()，避免語言檔尚未載入時 UI 整塊不渲染
      texts <- lang_texts_reactive()
      current_lang <- if (!is.null(texts) && !is.null(texts$language)) texts$language else "zh_TW"

      tagList(
        # 初始化載入管理器和提示系統
        if(exists("init_ai_loading_manager")) init_ai_loading_manager() else NULL,
        if(enable_hints) init_hint_system() else NULL,

        # KPI 指標卡片
        fluidRow(
          column(3,
            bs4ValueBox(
              value = textOutput(ns("cai_value")),
              subtitle = tags$span(
                get_text("ui.kpis.cai", "顧客活躍度 (CAI)"),
                tags$i(
                  class = "fas fa-info-circle ml-1",
                  style = "font-size: 12px; color: #6c757d; cursor: help;",
                  title = get_hint("activity_conversion_cai", hints_df(), language = current_lang),
                  `data-toggle` = "tooltip",
                  `data-placement` = "top"
                )
              ),
              icon = icon("chart-line"),
              color = "primary",
              width = 12
            )
          ),
          column(2,
            bs4ValueBox(
              value = textOutput(ns("conversion_rate")),
              subtitle = tags$span(
                get_text("ui.kpis.conversion_rate", "顧客轉化率"),
                tags$i(
                  class = "fas fa-info-circle ml-1",
                  style = "font-size: 12px; color: #6c757d; cursor: help;",
                  title = get_hint("activity_conversion_rate", hints_df(), language = current_lang),
                  `data-toggle` = "tooltip",
                  `data-placement` = "top"
                )
              ),
              icon = icon("percentage"),
              color = "success",
              width = 12
            )
          ),
          column(2,
            bs4ValueBox(
              value = textOutput(ns("purchase_frequency")),
              subtitle = tags$span(
                get_text("ui.kpis.purchase_frequency", "購買頻率"),
                tags$i(
                  class = "fas fa-info-circle ml-1",
                  style = "font-size: 12px; color: #6c757d; cursor: help;",
                  title = get_hint("activity_purchase_frequency", hints_df(), language = current_lang),
                  `data-toggle` = "tooltip",
                  `data-placement` = "top"
                )
              ),
              icon = icon("shopping-cart"),
              color = "info",
              width = 12
            )
          ),
          column(2,
            bs4ValueBox(
              value = textOutput(ns("avg_inter_purchase")),
              subtitle = tags$span(
                get_text("ui.kpis.inter_purchase_time", "平均再購時間"),
                tags$i(
                  class = "fas fa-info-circle ml-1",
                  style = "font-size: 12px; color: #6c757d; cursor: help;",
                  title = get_hint("activity_inter_purchase_time", hints_df(), language = current_lang),
                  `data-toggle` = "tooltip",
                  `data-placement` = "top"
                )
              ),
              icon = icon("clock"),
              color = "warning",
              width = 12
            )
          ),
          column(3,
            bs4ValueBox(
              value = textOutput(ns("reactivation_rate")),
              subtitle = if(enable_hints && !is.null(hints_df())) {
                tags$span(
                  get_text("ui.kpis.reactivation_rate", "喚醒率"),
                  tags$i(
                    class = "fas fa-info-circle",
                    style = "font-size: 12px; color: #6c757d;",
                    title = get_hint("activity_reactivation_rate", hints_df(), language = current_lang),
                    `data-toggle` = "tooltip",
                    `data-placement` = "top"
                  )
                )
              } else {
                get_text("ui.kpis.reactivation_rate", "喚醒率")
              },
              icon = icon("bell"),
              color = "purple",
              width = 12
            )
          )
        ),


        br(),

        # 圖表區域
        fluidRow(
          # 客戶活躍度分布散佈圖
          column(6,
            bs4Card(
              title = if(enable_hints && !is.null(hints_df())) {
                tags$span(
                  get_text("ui.cards.activity_distribution", "客戶活躍度分布"),
                  tags$i(
                    class = "fas fa-info-circle",
                    style = "font-size: 14px; color: #6c757d;",
                    title = get_hint("activity_distribution_chart", hints_df(), language = current_lang),
                    `data-toggle` = "tooltip",
                    `data-placement` = "top"
                  )
                )
              } else {
                get_text("ui.cards.activity_distribution", "客戶活躍度分布")
              },
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              plotlyOutput(ns("activity_distribution")),
              br(),
              div(
                style = "padding: 10px; background: #f8f9fa; border-radius: 5px; margin-top: 10px;",
                h6(get_text("ui.help.chart_explanation", "📊 圖表解讀說明"), style = "color: #2c3e50; font-weight: bold;"),
                p(get_text("ui.help.activity_distribution_desc", "此散佈圖展示客戶的活躍度與消費關係："), style = "margin-bottom: 5px;"),
                tags$ul(
                  tags$li(get_text("ui.help.activity_distribution_x", "橫軸：活躍度指數 (CAI)，正值表示活躍上升，負值表示活躍下降")),
                  tags$li(get_text("ui.help.activity_distribution_y", "縱軸：總消費金額")),
                  tags$li(get_text("ui.help.activity_distribution_color", "點的顏色：代表購買次數（綠色=高頻，紅色=低頻）")),
                  tags$li(get_text("ui.help.activity_distribution_size", "點的大小：也反映消費金額大小"))
                ),
                p(get_text("ui.help.activity_distribution_ideal", "理想狀態是多數客戶集中在右上角（高活躍度、高消費）"), style = "font-style: italic; color: #666;")
              )
            )
          ),

          # 轉化漏斗分析
          column(6,
            bs4Card(
              title = if(enable_hints && !is.null(hints_df())) {
                tags$span(
                  get_text("ui.cards.conversion_funnel", "顧客轉化漏斗"),
                  tags$i(
                    class = "fas fa-info-circle",
                    style = "font-size: 14px; color: #6c757d;",
                    title = get_hint("conversion_funnel_chart", hints_df(), language = current_lang),
                    `data-toggle` = "tooltip",
                    `data-placement` = "top"
                  )
                )
              } else {
                get_text("ui.cards.conversion_funnel", "顧客轉化漏斗")
              },
              status = "success",
              solidHeader = TRUE,
              width = 12,
              plotlyOutput(ns("conversion_funnel")),
              br(),
              div(
                style = "padding: 10px; background: #f8f9fa; border-radius: 5px; margin-top: 10px;",
                h6(get_text("ui.help.chart_explanation", "📊 圖表解讀說明"), style = "color: #2c3e50; font-weight: bold;"),
                p(get_text("ui.help.funnel_desc", "漏斗圖展示客戶從首購到忠誠的轉化過程："), style = "margin-bottom: 5px;"),
                tags$ul(
                  tags$li(get_text("ui.help.funnel_first", "1次購買：新客基數 (100%)")),
                  tags$li(get_text("ui.help.funnel_second", "2次購買：二購轉化率 = 2次購買客戶數 ÷ 總客戶數")),
                  tags$li(get_text("ui.help.funnel_regular", "3-4次：常客轉化率")),
                  tags$li(get_text("ui.help.funnel_loyal", "≥5次：忠誠客戶轉化率"))
                ),
                p(get_text("ui.help.funnel_note", "轉化率越高，表示客戶忠誠度經營越成功"), style = "font-style: italic; color: #666;")
              )
            )
          )
        ),

        br(),

        # 購買頻率與週期分析
        fluidRow(
          column(6,
            bs4Card(
              title = get_text("ui.cards.purchase_patterns", "購買頻率與週期分析"),
              status = "info",
              solidHeader = TRUE,
              width = 12,
              plotlyOutput(ns("purchase_patterns")),
              br(),
              div(
                style = "padding: 10px; background: #f8f9fa; border-radius: 5px; margin-top: 10px;",
                h6(get_text("ui.help.chart_explanation", "📊 圖表解讀說明"), style = "color: #2c3e50; font-weight: bold;"),
                p(get_text("ui.help.patterns_desc", "散佈圖展示每個客戶的購買頻率與週期關係："), style = "margin-bottom: 5px;"),
                tags$ul(
                  tags$li(get_text("ui.help.patterns_x", "橫軸：購買頻率（總購買次數）")),
                  tags$li(get_text("ui.help.patterns_y", "縱軸：平均購買週期（兩次購買間隔天數）")),
                  tags$li(get_text("ui.help.patterns_size", "點的大小：總消費金額")),
                  tags$li(get_text("ui.help.patterns_color", "顏色：客戶價值分群"))
                ),
                p(get_text("ui.help.patterns_ideal", "理想客戶位於右下角：高頻率、短週期、大消費"), style = "font-style: italic; color: #666;")
              )
            )
          ),

          # 客戶喚醒機會分析
          column(6,
            bs4Card(
              title = get_text("ui.cards.reactivation_opportunity", "客戶喚醒機會分析"),
              status = "warning",
              solidHeader = TRUE,
              width = 12,
              footer = downloadButton(ns("download_reactivation"), get_text("ui.buttons.download_reactivation", "下載需喚醒客戶清單"), class = "btn-sm"),
              plotlyOutput(ns("reactivation_opportunity")),
              br(),
              div(
                style = "padding: 10px; background: #f8f9fa; border-radius: 5px; margin-top: 10px;",
                h6(get_text("ui.help.chart_explanation", "📊 圖表解讀說明"), style = "color: #2c3e50; font-weight: bold;"),
                p(get_text("ui.help.reactivation_desc", "圖表列出沉睡客 (S1/S2/S3) 按未購天數的急迫程度："), style = "margin-bottom: 5px;"),
                tags$ul(
                  tags$li(get_text("ui.help.reactivation_x", "橫軸：急迫性（>180天 緊急，90-180天 重要，≤90天 一般）")),
                  tags$li(get_text("ui.help.reactivation_y", "縱軸：客戶數")),
                  tags$li(get_text("ui.help.reactivation_value", "氣泡文字：客戶數、累計消費金額、平均未購天數")),
                  tags$li(get_text("ui.help.reactivation_use", "用途：優先處理右側/高天數且金額大的群組，發送喚醒方案"))
                ),
                p(get_text("ui.help.reactivation_ideal", "理想狀態：緊急與重要客群數量下降，金額集中在已回購/活躍客群"), style = "font-style: italic; color: #666;")
              )
            )
          )
        ),

        br(),

        # 忠誠度階梯
        fluidRow(
          column(12,
            bs4Card(
              title = get_text("ui.cards.loyalty_ladder", "忠誠度階梯"),
              status = "purple",
              solidHeader = TRUE,
              width = 12,
              footer = downloadButton(ns("download_loyalty"), get_text("ui.buttons.download_loyalty", "下載忠誠度分析"), class = "btn-sm"),
              plotlyOutput(ns("loyalty_ladder"))
            )
          )
        ),

        br(),

        # AI 智能分析集中區
        fluidRow(
          column(12,
            bs4Card(
              title = get_text("ui.cards.ai_analysis", "🤖 AI 智能分析與建議"),
              status = "info",
              solidHeader = TRUE,
              width = 12,
              collapsible = TRUE,

              tabsetPanel(
                type = "tabs",

                tabPanel(
                  title = get_text("ui.tabs.frequency_analysis", "購買頻率分析"),
                  icon = icon("chart-bar"),
                  br(),
                  actionButton(ns("btn_analyze_frequency"),
                              get_text("ui.buttons.start_ai_analysis", "開始 AI 分析"),
                              icon = icon("robot"),
                              class = "btn-primary"),
                  br(), br(),
                  uiOutput(ns("ai_purchase_frequency"))
                ),

                tabPanel(
                  title = get_text("ui.tabs.reactivation_analysis", "喚醒機會分析"),
                  icon = icon("bell"),
                  br(),
                  actionButton(ns("btn_analyze_reactivation"),
                              get_text("ui.buttons.start_ai_analysis", "開始 AI 分析"),
                              icon = icon("robot"),
                              class = "btn-warning"),
                  br(), br(),
                  uiOutput(ns("ai_reactivation_opportunity"))
                ),

                tabPanel(
                  title = get_text("ui.tabs.loyalty_strategy", "忠誠度階梯策略"),
                  icon = icon("stairs"),
                  br(),
                  actionButton(ns("btn_analyze_loyalty"),
                              get_text("ui.buttons.start_ai_analysis", "開始 AI 分析"),
                              icon = icon("robot"),
                              class = "btn-success"),
                  br(), br(),
                  uiOutput(ns("ai_loyalty_ladder"))
                ),

                tabPanel(
                  title = get_text("ui.tabs.reactivation_list", "喚醒客戶清單"),
                  icon = icon("list"),
                  br(),
                  actionButton(ns("btn_analyze_list"),
                              get_text("ui.buttons.start_ai_analysis", "開始 AI 分析"),
                              icon = icon("robot"),
                              class = "btn-danger"),
                  br(), br(),
                  uiOutput(ns("ai_reactivation_list"))
                )
              )
            )
          )
        )
      )  # End tagList
    })  # End renderUI

    # ===================================================================
    # Server Logic Below (moved from old server function)
    # ===================================================================

    # Helper operator
    `%||%` <- function(x, y) if (is.null(x)) y else x

    # 載入 prompt 管理器
    prompts_df <- NULL
    if(enable_gpt) {
      prompts_df <- load_prompts()
    }
    
    # AI 分析狀態管理
    ai_analysis_state <- reactiveValues(
      frequency_analyzed = FALSE,
      reactivation_analyzed = FALSE,
      loyalty_analyzed = FALSE,
      list_analyzed = FALSE
    )
    
    # 輔助函數：處理 AI 結果的 Markdown 轉換
    render_ai_result <- function(result) {
      if(!is.null(result)) {
        if(grepl("\\*\\*|##|\\n\\n|^[0-9]+\\.|^-\\s", result)) {
          html_content <- markdownToHTML(text = result, fragment.only = TRUE)
          return(HTML(html_content))
        } else {
          return(HTML(result))
        }
      }
      return(NULL)
    }

    # 取得 DNA 分析結果 - Reactive Data Access Pattern (matching retention module)
    dna_results <- reactive({
      req(dna_module)

      cat("🔍 [Engagement] Accessing DNA module data...\n")

      result <- dna_module()
      if (!is.null(result) && !is.null(result$dna_results)) {
        dna_data <- result$dna_results

        # Backfill tolerant defaults similar to retention
        if (!is.null(dna_data$data_by_customer) && is.data.frame(dna_data$data_by_customer)) {
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
        }

        if (!is.null(dna_data$data_by_customer)) {
          cat("✅ [Engagement] DNA data loaded:", nrow(dna_data$data_by_customer), "customers\n")
        }
        return(dna_data)
      }

      cat("⚠️ [Engagement] DNA module returned NULL or no dna_results\n")
      return(NULL)
    })
    
    # 計算活躍度指標
    engagement_metrics <- reactive({
      req(dna_results())
      if (is.null(dna_results()$data_by_customer)) return(NULL)
      
      data <- dna_results()$data_by_customer
      
      # 檢查必要欄位是否存在
      if(!"cai" %in% names(data)) {
        # 優先使用 DNA 提供的 cai_value，否則設 0（不再推算）
        if ("cai_value" %in% names(data)) {
          data$cai <- data$cai_value
        } else {
          data$cai <- 0
        }
      }
      
      # 計算 CAI
      avg_cai <- mean(data$cai, na.rm = TRUE)
      if(is.na(avg_cai) || is.nan(avg_cai)) {
        avg_cai <- 0
      }
      
      # 計算轉化率
      total_customers <- nrow(data)
      converted_customers <- sum(data$times >= 5, na.rm = TRUE)
      conversion_rate <- if(total_customers > 0) {
        (converted_customers / total_customers) * 100
      } else {
        0
      }
      
      # 計算購買頻率
      avg_frequency <- mean(data$times, na.rm = TRUE)
      if(is.na(avg_frequency)) avg_frequency <- 0
      
      # 計算平均再購時間
      repurchase_customers <- data[data$times > 1, ]
      avg_inter_purchase <- if(nrow(repurchase_customers) > 0) {
        mean(repurchase_customers$r_value / (repurchase_customers$times - 1), na.rm = TRUE)
      } else {
        NA
      }
      
      # 計算喚醒率 - 使用 DNA 分析的 3 個月回購追蹤 (nrec 欄位)
      # nrec = "rec" 表示已回購, nrec = "nrec" 表示未回購
      if("nrec" %in% names(data) && sum(!is.na(data$nrec)) > 0) {
        # 計算有回購追蹤紀錄的客戶中，成功回購的比例
        tracked_customers <- data[!is.na(data$nrec), ]
        reactivation_rate <- mean(tracked_customers$nrec == "rec", na.rm = TRUE) * 100
      } else {
        # 資料不可用時設為 NA，UI 會顯示 "N/A"
        reactivation_rate <- NA
      }
      
      list(
        cai = avg_cai,
        conversion_rate = conversion_rate,
        purchase_frequency = avg_frequency,
        avg_inter_purchase = avg_inter_purchase,
        reactivation_rate = reactivation_rate,
        raw_data = data
      )
    })

    # 如果 engagement_metrics 結果更新則清除 AI 快取
    observeEvent(engagement_metrics(), {
      ai_cache$data <- list()
      cat("🧹 [Engagement] Metrics changed -> AI cache cleared\n")
    })

    # KPI 顯示 - 顯示平均 CAI
    output$cai_value <- renderText({
      metrics <- engagement_metrics()
      avg_cai <- metrics$cai
      if(!is.na(avg_cai) && !is.nan(avg_cai)) {
        formatted_cai <- sprintf("%.2f", avg_cai)
        if(as.numeric(formatted_cai) > 0) {
          paste0("+", formatted_cai, " (平均)")
        } else {
          paste0(formatted_cai, " (平均)")
        }
      } else {
        "0.00 (平均)"
      }
    })
    
    output$conversion_rate <- renderText({
      metrics <- engagement_metrics()
      paste0(sprintf("%.2f", metrics$conversion_rate), "%")
    })
    
    output$purchase_frequency <- renderText({
      metrics <- engagement_metrics()
      paste0(sprintf("%.2f", metrics$purchase_frequency), " ", get_text("ui.units.times"))
    })

    output$avg_inter_purchase <- renderText({
      metrics <- engagement_metrics()
      if(!is.na(metrics$avg_inter_purchase)) {
        paste0(sprintf("%.2f", metrics$avg_inter_purchase), " ", get_text("ui.units.days"))
      } else {
        "N/A"
      }
    })
    
    output$reactivation_rate <- renderText({
      metrics <- engagement_metrics()
      if(!is.na(metrics$reactivation_rate)) {
        paste0(sprintf("%.2f", metrics$reactivation_rate), "%")
      } else {
        "N/A"
      }
    })
    
    # 客戶活躍度分布（正確的散佈圖 - CAI vs 總消費）
    output$activity_distribution <- renderPlotly({
      req(engagement_metrics())
      data <- engagement_metrics()$raw_data
      
      # 移除 NA 值
      data <- data %>%
        filter(!is.na(cai) & !is.na(total_spent) & total_spent > 0)
      
      if(nrow(data) == 0) {
        return(plotly_empty() %>%
          layout(
            title = get_text("ui.charts.no_data"),
            annotations = list(
              list(
                text = get_text("ui.charts.no_activity_data"),
                showarrow = FALSE,
                font = list(size = 20)
              )
            )
          ))
      }

      # 準備散佈圖數據
      scatter_data <- data %>%
        mutate(
          hover_text = paste0(
            get_text("ui.charts.customer_id"), ": ", customer_id, "<br>",
            get_text("ui.charts.activity_index"), ": ", round(cai, 2), "<br>",
            get_text("ui.charts.purchase_count"), ": ", times, " ", get_text("ui.units.times"), "<br>",
            get_text("ui.charts.total_spending"), ": $", format(round(total_spent, 0), big.mark = ",")
          )
        )
      
      # 創建散佈圖
      plot_ly(scatter_data,
              x = ~cai,
              y = ~total_spent,
              color = ~times,
              colors = "RdYlGn",
              type = 'scatter',
              mode = 'markers',
              text = ~hover_text,
              hoverinfo = 'text',
              marker = list(
                size = ~sqrt(total_spent),
                sizemode = 'area',
                sizeref = 2 * max(sqrt(scatter_data$total_spent)) / (30^2),
                sizemin = 4,
                opacity = 0.7,
                line = list(color = 'white', width = 0.5)
              )) %>%
        layout(
          title = list(text = get_text("ui.charts.activity_vs_spending"), font = list(size = 14)),
          xaxis = list(
            title = get_text("ui.charts.activity_index"),
            zeroline = TRUE,
            zerolinecolor = 'rgba(0,0,0,0.3)',
            zerolinewidth = 2,
            gridcolor = 'rgba(0,0,0,0.1)'
          ),
          yaxis = list(
            title = paste0(get_text("ui.charts.total_spending"), " ($)"),
            type = if(min(scatter_data$total_spent) > 0 &&
                     max(scatter_data$total_spent) / min(scatter_data$total_spent) > 100) "log" else "linear",
            gridcolor = 'rgba(0,0,0,0.1)'
          ),
          showlegend = TRUE,
          legend = list(title = list(text = get_text("ui.charts.purchase_count"))),
          hovermode = 'closest'
        )
    })
    
    # 轉化漏斗分析
    output$conversion_funnel <- renderPlotly({
      req(engagement_metrics())
      data <- engagement_metrics()$raw_data

      # Define stage labels with proper fallbacks (avoid duplicate empty strings)
      stage_labels <- c(
        stage_1 = {
          label <- get_text("ui.charts.stage_1_purchase", "")
          if (nchar(label) == 0) get_text("loyalty_tiers.first_time", "1次購買") else label
        },
        stage_2 = {
          label <- get_text("ui.charts.stage_2_purchases", "")
          if (nchar(label) == 0) get_text("loyalty_tiers.occasional", "2次購買") else label
        },
        stage_3_4 = {
          label <- get_text("ui.charts.stage_3_4_purchases", "")
          if (nchar(label) == 0) get_text("loyalty_tiers.regular", "3-4次購買") else label
        },
        stage_5_plus = {
          label <- get_text("ui.charts.stage_5_plus_purchases", "")
          if (nchar(label) == 0) get_text("loyalty_tiers.loyal", "5次以上購買") else label
        }
      )

      # Ensure unique labels (prevent duplicate factor levels)
      if (length(unique(stage_labels)) != length(stage_labels)) {
        stage_labels <- c("1次購買", "2次購買", "3-4次購買", "5次以上購買")
      }

      # 準備漏斗數據
      funnel_data <- data %>%
        mutate(
          purchase_group = case_when(
            times == 1 ~ stage_labels[1],
            times == 2 ~ stage_labels[2],
            times >= 3 & times <= 4 ~ stage_labels[3],
            times >= 5 ~ stage_labels[4]
          )
        ) %>%
        group_by(purchase_group) %>%
        summarise(count = n(), .groups = 'drop')

      # 確保所有類別都存在
      all_groups <- data.frame(
        purchase_group = stage_labels,
        stringsAsFactors = FALSE
      )

      funnel_data <- all_groups %>%
        left_join(funnel_data, by = "purchase_group") %>%
        mutate(
          count = ifelse(is.na(count), 0, count),
          purchase_group = factor(purchase_group, levels = stage_labels)
        ) %>%
        arrange(purchase_group)
      
      if(sum(funnel_data$count) > 0) {
        plot_ly(funnel_data,
                type = "funnel",
                y = ~purchase_group,
                x = ~count,
                textposition = "inside",
                textinfo = "value+percent",
                marker = list(
                  color = c("#FF6B6B", "#FFA06B", "#FFD93D", "#6BCF7F")
                ),
                connector = list(line = list(color = "royalblue", width = 3)),
                hovertemplate = paste0("%{y}<br>", get_text("ui.charts.customer_count"), ": %{x}<br>", get_text("ui.charts.conversion_rate"), ": %{percentTotal}<extra></extra>")) %>%
          layout(
            title = list(text = get_text("ui.charts.funnel_title"), font = list(size = 14)),
            yaxis = list(title = get_text("ui.charts.funnel_stage")),
            xaxis = list(title = get_text("ui.charts.customer_count")),
            showlegend = FALSE
          )
      } else {
        plotly_empty()
      }
    })
    
    # 購買頻率與週期分析 - 散佈圖呈現
    output$purchase_patterns <- renderPlotly({
      req(engagement_metrics())
      data <- engagement_metrics()$raw_data
      
      # 過濾重複購買客戶
      pattern_data <- data %>%
        filter(times > 1 & !is.na(r_value) & !is.na(total_spent))
      
      if(nrow(pattern_data) > 0) {
        # 計算每個客戶的平均購買週期
        pattern_data <- pattern_data %>%
          mutate(
            avg_cycle = r_value / (times - 1),  # 平均購買週期
            purchase_frequency = times,          # 購買頻率（次數）
            hover_text = paste0(
              get_text("ui.charts.customer_id"), ": ", customer_id, "<br>",
              get_text("ui.charts.purchase_frequency_label"), ": ", times, " ", get_text("ui.units.times"), "<br>",
              get_text("ui.charts.avg_days_between_purchases"), ": ", round(avg_cycle, 1), " ", get_text("ui.units.days"), "<br>",
              get_text("ui.charts.total_spending"), ": $", format(round(total_spent, 0), big.mark = ","), "<br>",
              "Status: ", nes_status
            ),
            # 為顏色分組
            value_group = case_when(
              total_spent >= quantile(total_spent, 0.75, na.rm = TRUE) ~ get_text("ui.value_segments.high"),
              total_spent >= quantile(total_spent, 0.50, na.rm = TRUE) ~ get_text("ui.value_segments.medium_high"),
              total_spent >= quantile(total_spent, 0.25, na.rm = TRUE) ~ get_text("ui.value_segments.medium_low"),
              TRUE ~ get_text("ui.value_segments.low")
            )
          ) %>%
          filter(!is.na(avg_cycle) & avg_cycle > 0)  # 過濾異常值
        
        if(nrow(pattern_data) > 0) {
          # 創建顏色映射（基於動態文本）
          color_map <- setNames(
            c("#1f77b4", "#2ca02c", "#ff7f0e", "#d62728"),
            c(get_text("ui.value_segments.high"),
              get_text("ui.value_segments.medium_high"),
              get_text("ui.value_segments.medium_low"),
              get_text("ui.value_segments.low"))
          )

          # 創建散佈圖
          plot_ly(pattern_data,
                  x = ~purchase_frequency,
                  y = ~avg_cycle,
                  color = ~value_group,
                  colors = color_map,
                  type = 'scatter',
                  mode = 'markers',
                  text = ~hover_text,
                  hoverinfo = 'text',
                  marker = list(
                    size = ~sqrt(total_spent),
                    sizemode = 'area',
                    sizeref = 2 * max(sqrt(pattern_data$total_spent)) / (25^2),
                    sizemin = 3,
                    opacity = 0.6,
                    line = list(color = 'white', width = 0.5)
                  )) %>%
            layout(
              title = list(text = get_text("ui.charts.frequency_cycle_analysis"), font = list(size = 14)),
              xaxis = list(
                title = get_text("ui.charts.purchase_frequency_label"),
                zeroline = TRUE,
                gridcolor = 'rgba(0,0,0,0.1)',
                dtick = 5
              ),
              yaxis = list(
                title = paste0(get_text("ui.charts.avg_days_between_purchases"), " (", get_text("ui.units.days"), ")"),
                gridcolor = 'rgba(0,0,0,0.1)',
                type = if(max(pattern_data$avg_cycle) / min(pattern_data$avg_cycle) > 100) "log" else "linear"
              ),
              legend = list(
                title = list(text = get_text("ui.charts.customer_count")),
                orientation = "v",
                x = 1.02,
                y = 1
              ),
              hovermode = 'closest',
              margin = list(r = 120)
            )
        } else {
          plotly_empty()
        }
      } else {
        plotly_empty() %>%
          layout(
            title = get_text("ui.charts.no_data"),
            annotations = list(
              list(
                text = get_text("ui.charts.no_activity_data"),
                showarrow = FALSE,
                font = list(size = 20)
              )
            )
          )
      }
    })
    
    # 客戶喚醒機會分析
    output$reactivation_opportunity <- renderPlotly({
      req(engagement_metrics())
      data <- engagement_metrics()$raw_data
      
      # Define urgency levels with translated text
      urgent_text <- get_text("ui.urgency_levels.urgent")
      important_text <- get_text("ui.urgency_levels.important")
      normal_text <- get_text("ui.urgency_levels.normal")

      reactivation_data <- data %>%
        filter(nes_status %in% c("S1", "S2", "S3")) %>%
        mutate(
          urgency = case_when(
            r_value > 180 ~ urgent_text,
            r_value > 90 ~ important_text,
            TRUE ~ normal_text
          ),
          urgency_order = case_when(
            urgency == urgent_text ~ 1,
            urgency == important_text ~ 2,
            urgency == normal_text ~ 3
          )
        ) %>%
        group_by(urgency, urgency_order) %>%
        summarise(
          count = n(),
          total_value = sum(total_spent, na.rm = TRUE),
          avg_days = mean(r_value, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        arrange(urgency_order)

      # 確保三個急迫層級都顯示（即便為 0）
      all_levels <- data.frame(
        urgency = c(urgent_text, important_text, normal_text),
        urgency_order = c(1, 2, 3)
      )
      reactivation_data <- all_levels %>%
        left_join(reactivation_data, by = c("urgency", "urgency_order")) %>%
        mutate(
          count = coalesce(count, 0),
          total_value = coalesce(total_value, 0),
          avg_days = coalesce(avg_days, 0)
        )
      
      if(nrow(reactivation_data) > 0) {
        # Create color mapping for urgency levels
        urgency_colors <- setNames(
          c("#dc3545", "#ffc107", "#28a745"),
          c(urgent_text, important_text, normal_text)
        )

        plot_ly(reactivation_data,
                x = ~urgency,
                y = ~count,
                color = ~urgency,
                colors = urgency_colors,
                type = 'bar',
                text = ~paste0(
                  get_text("ui.charts.customer_count"), ": ", format(count, big.mark = ","), "<br>",
                  get_text("ui.charts.total_spending"), ": $", format(round(total_value,0), big.mark = ","), "<br>",
                  get_text("ui.charts.days_since_last_purchase"), ": ", sprintf('%.0f', avg_days), " ", get_text("ui.units.days")
                ),
                textposition = "outside",
                hoverinfo = 'text') %>%
          layout(
            title = list(text = get_text("ui.charts.reactivation_title"), font = list(size = 14)),
            xaxis = list(title = get_text("ui.charts.urgency_score")),
            yaxis = list(title = get_text("ui.charts.customer_count")),
            showlegend = FALSE
          )
      } else {
        plotly_empty() %>%
          layout(
            title = "無需喚醒的客戶",
            annotations = list(
              list(
                text = "所有客戶都處於活躍狀態",
                showarrow = FALSE,
                font = list(size = 20)
              )
            )
          )
      }
    })
    
    # 忠誠度階梯
    output$loyalty_ladder <- renderPlotly({
      req(engagement_metrics())
      data <- engagement_metrics()$raw_data

      # Define loyalty tiers with translated text
      first_time_text <- get_text("ui.loyalty_tiers.first_time")
      occasional_text <- get_text("ui.loyalty_tiers.occasional")
      regular_text <- get_text("ui.loyalty_tiers.regular")
      loyal_text <- get_text("ui.loyalty_tiers.loyal")
      ambassador_text <- get_text("ui.loyalty_tiers.ambassador")

      loyalty_data <- data %>%
        mutate(
          loyalty_level = factor(
            case_when(
              times == 1 ~ first_time_text,
              times >= 2 & times <= 3 ~ occasional_text,
              times >= 4 & times <= 6 ~ regular_text,
              times >= 7 & times <= 10 ~ loyal_text,
              times > 10 ~ ambassador_text
            ),
            levels = c(first_time_text, occasional_text, regular_text, loyal_text, ambassador_text)
          )
        ) %>%
        group_by(loyalty_level) %>%
        summarise(
          count = n(),
          avg_value = mean(total_spent, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        filter(!is.na(loyalty_level))
      
      if(nrow(loyalty_data) > 0) {
        plot_ly(loyalty_data,
                x = ~loyalty_level,
                y = ~count,
                type = 'bar',
                marker = list(
                  color = c("#e8e8e8", "#b3d9ff", "#66b3ff", "#0080ff", "#0040ff")
                ),
                text = ~paste0(count, " ", get_text("ui.units.customers"), "<br>",
                             get_text("ui.charts.average_purchase_value"), ": $", format(round(avg_value, 0), big.mark = ",")),
                textposition = "outside",
                hovertemplate = paste0("%{x}<br>", get_text("ui.charts.customer_count"), ": %{y}<br>%{text}<extra></extra>")) %>%
          layout(
            title = list(text = get_text("ui.charts.loyalty_ladder_title"), font = list(size = 14)),
            xaxis = list(title = get_text("ui.charts.loyalty_tier")),
            yaxis = list(title = get_text("ui.charts.customer_count")),
            showlegend = FALSE
          )
      } else {
        plotly_empty()
      }
    })
    
    # CSV 下載功能（保持不變）
    output$download_reactivation <- downloadHandler(
      filename = function() {
        paste0("reactivation_customers_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        data <- engagement_metrics()$raw_data
        
        reactivation_list <- data %>%
          filter(nes_status %in% c("S1", "S2", "S3")) %>%
          mutate(
            urgency = case_when(
              r_value > 180 ~ get_text("ui.urgency_levels.urgent"),
              r_value > 90 ~ get_text("ui.urgency_levels.important"),
              TRUE ~ get_text("ui.urgency_levels.normal")
            ),
            suggested_action = case_when(
              r_value > 180 ~ get_text("ui.actions.large_coupon"),
              r_value > 90 ~ get_text("ui.actions.product_promo"),
              TRUE ~ get_text("ui.actions.regular_care")
            )
          ) %>%
          select(customer_id, nes_status, r_value, total_spent, times, urgency, suggested_action) %>%
          arrange(desc(r_value))

        # 使用中文欄位名稱
        names(reactivation_list) <- c("客戶ID", "客戶狀態", "最近購買天數", "總消費金額", "購買次數", "急迫性", "建議行動")
        write.csv(reactivation_list, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )
    
    output$download_loyalty <- downloadHandler(
      filename = function() {
        paste0("loyalty_ladder_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        data <- engagement_metrics()$raw_data
        
        loyalty_export <- data %>%
          mutate(
            loyalty_level = case_when(
              times == 1 ~ get_text("ui.loyalty_tiers.first_time"),
              times >= 2 & times <= 3 ~ get_text("ui.loyalty_tiers.occasional"),
              times >= 4 & times <= 6 ~ get_text("ui.loyalty_tiers.regular"),
              times >= 7 & times <= 10 ~ get_text("ui.loyalty_tiers.loyal"),
              times > 10 ~ get_text("ui.loyalty_tiers.ambassador")
            ),
            next_level_target = case_when(
              times == 1 ~ get_text("ui.actions.upgrade_to_occasional"),
              times >= 2 & times <= 3 ~ paste0(get_text("ui.actions.upgrade_to_regular", default = "需再購"), 4 - times, get_text("ui.actions.upgrade_to_regular")),
              times >= 4 & times <= 6 ~ paste0(get_text("ui.actions.upgrade_to_loyal", default = "需再購"), 7 - times, get_text("ui.actions.upgrade_to_loyal")),
              times >= 7 & times <= 10 ~ paste0(get_text("ui.actions.upgrade_to_ambassador", default = "需再購"), 11 - times, get_text("ui.actions.upgrade_to_ambassador")),
              times > 10 ~ get_text("ui.actions.max_level")
            )
          ) %>%
          select(customer_id, loyalty_level, times, total_spent, m_value, next_level_target) %>%
          arrange(desc(times), desc(total_spent))

        # 使用中文欄位名稱
        names(loyalty_export) <- c("客戶ID", "忠誠度等級", "購買次數", "總消費金額", "平均單次金額", "升級目標")
        write.csv(loyalty_export, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )
    
    # AI 分析功能（加入按鈕觸發 + 確認 API 使用）
    
    # 購買頻率分析
    observeEvent(input$btn_analyze_frequency, {
      req(engagement_metrics())
      metrics <- engagement_metrics()
      
      output$ai_purchase_frequency <- renderUI({
        # 使用 withProgress 包裝整個分析過程
        withProgress(message = '🤖 正在進行 AI 分析...', value = 0.2, {
          setProgress(0.3, message = "📊 分析購買頻率數據...")
          
          # 檢查環境變數和 chat_api 函數
          api_key_exists <- nzchar(Sys.getenv("OPENAI_API_KEY"))
          chat_api_exists <- exists("chat_api") && is.function(chat_api)
          
          # 檢查是否啟用 GPT 並有 API
          api_status <- paste0("API狀態: ", 
                              ifelse(enable_gpt, "已啟用", "未啟用"), 
                              " | API Key: ", 
                              ifelse(api_key_exists, "已設定", "未設定"),
                              " | chat_api: ", 
                              ifelse(chat_api_exists, "可用", "不可用"))
          
          cat("[購買頻率分析]", api_status, "\n")
          
          # 檢查所有必要條件
          use_ai <- enable_gpt && 
                   api_key_exists && 
                   chat_api_exists && 
                   !is.null(prompts_df)
          
          if(use_ai) {
            tryCatch({
              # 準備數據
              freq_distribution <- metrics$raw_data %>%
                mutate(freq_group = cut(times, breaks = c(0, 1, 3, 5, 10, Inf),
                                       labels = c("1次", "2-3次", "4-5次", "6-10次", ">10次"))) %>%
                group_by(freq_group) %>%
                summarise(count = n(), .groups = 'drop')

              # 取得當前語言
              current_lang <- if (!is.null(lang_texts_reactive()) && !is.null(lang_texts_reactive()$language)) {
                lang_texts_reactive()$language
              } else {
                "zh_TW"
              }

              # 生成快取鍵
              cache_key <- get_ai_cache_key(
                "frequency_analysis",
                list(
                  avg_frequency = round(metrics$purchase_frequency, 1),
                  freq_dist = freq_distribution
                ),
                current_lang
              )

              # 檢查快取
              cached_result <- get_cached_ai(cache_key)
              if (!is.null(cached_result)) {
                cat("[購買頻率分析] 使用快取結果\n")
                ai_analysis_state$frequency_analyzed <- TRUE
                return(div(
                  style = "padding: 15px; background: #f0f8ff; border-radius: 8px;",
                  h5(get_text("ai_analysis.fallback_titles.ai_result"), style = "color: #0066cc; margin-bottom: 15px;"),
                  render_ai_result(cached_result)
                ))
              }

              cat("[購買頻率分析] 執行 execute_gpt_request...\n")

              result <- execute_gpt_request(
                var_id = "activity_purchase_frequency_analysis",
                variables = list(
                  avg_frequency = round(metrics$purchase_frequency, 1),
                  frequency_distribution = paste(freq_distribution$freq_group, ":",
                                               freq_distribution$count, "人",
                                               collapse = ", ")
                ),
                chat_api_function = chat_api,
                model = cfg_ai_model,
                prompts_df = prompts_df
              )

              if(!is.null(result)) {
                cat("[購買頻率分析] GPT API 成功返回結果\n")
                # 儲存快取
                set_cached_ai(cache_key, result)
                ai_analysis_state$frequency_analyzed <- TRUE
                return(div(
                  style = "padding: 15px; background: #f0f8ff; border-radius: 8px;",
                  h5(get_text("ai_analysis.fallback_titles.ai_result"), style = "color: #0066cc; margin-bottom: 15px;"),
                  render_ai_result(result)
                ))
              } else {
                cat("[購買頻率分析] GPT API 返回空結果\n")
              }
            }, error = function(e) {
              cat("[購買頻率分析] AI 分析失敗:", e$message, "\n")
            })
          } else {
            cat("[購買頻率分析] 使用預設分析（GPT未啟用或未設定）\n")
          }

          # 預設分析
          div(
            style = "padding: 15px; background: #f0f8ff; border-radius: 8px;",
            h5(get_text("ai_analysis.fallback_titles.frequency_insights"), style = "color: #2c3e50;"),
            tags$small(api_status, style = "color: #999; display: block; margin-bottom: 10px;"),
            p(paste0(get_text("ai_analysis.fallback_text.avg_frequency_label"), " ", round(metrics$purchase_frequency, 1), " ", get_text("ui.units.times"))),
            h5(get_text("ai_analysis.fallback_titles.marketing_suggestions"), style = "color: #2c3e50; margin-top: 15px;"),
            tags$ul(
              tags$li(get_text("ai_analysis.frequency_suggestions.low_frequency")),
              tags$li(get_text("ai_analysis.frequency_suggestions.medium_frequency")),
              tags$li(get_text("ai_analysis.frequency_suggestions.high_frequency"))
            )
          )
        })  # end withProgress
      })    # end renderUI
    })      # end observeEvent
    
    # 喚醒機會分析
    observeEvent(input$btn_analyze_reactivation, {
      req(engagement_metrics())
      data <- engagement_metrics()$raw_data
      
      output$ai_reactivation_opportunity <- renderUI({
        withProgress(message = '正在進行 AI 分析...', value = 0.5, {
          
          reactivation_stats <- data %>%
            filter(nes_status %in% c("S1", "S2", "S3")) %>%
            summarise(
              total = n(),
              total_value = sum(total_spent, na.rm = TRUE),
              avg_days = mean(r_value, na.rm = TRUE),
              .groups = 'drop'
            )
          
          # 檢查環境變數和 chat_api 函數
          api_key_exists <- nzchar(Sys.getenv("OPENAI_API_KEY"))
          chat_api_exists <- exists("chat_api") && is.function(chat_api)
          
          api_status <- paste0("API: ", 
                             ifelse(enable_gpt && api_key_exists && chat_api_exists, 
                                   "啟用", "未啟用"))
          cat("[喚醒機會分析]", api_status, "\n")
          
          # 檢查所有必要條件
          use_ai <- enable_gpt && 
                   api_key_exists && 
                   chat_api_exists && 
                   !is.null(prompts_df) && 
                   reactivation_stats$total > 0
          
          if(use_ai) {
            tryCatch({
              # 取得當前語言
              current_lang <- if (!is.null(lang_texts_reactive()) && !is.null(lang_texts_reactive()$language)) {
                lang_texts_reactive()$language
              } else {
                "zh_TW"
              }

              # 生成快取鍵
              cache_key <- get_ai_cache_key(
                "reactivation_analysis",
                reactivation_stats,
                current_lang
              )

              # 檢查快取
              cached_result <- get_cached_ai(cache_key)
              if (!is.null(cached_result)) {
                cat("[喚醒機會分析] 使用快取結果\n")
                ai_analysis_state$reactivation_analyzed <- TRUE
                return(div(
                  style = "padding: 15px; background: #fff5e6; border-radius: 8px;",
                  h5(get_text("ai_analysis.fallback_titles.ai_result"), style = "color: #ff9800; margin-bottom: 15px;"),
                  render_ai_result(cached_result)
                ))
              }

              cat("[喚醒機會分析] 執行 execute_gpt_request...\n")

              result <- execute_gpt_request(
                var_id = "activity_customer_reactivation_analysis",
                variables = list(
                  reactivation_count = reactivation_stats$total,
                  total_value = round(reactivation_stats$total_value, 0),
                  avg_dormant_days = round(reactivation_stats$avg_days, 0)
                ),
                chat_api_function = chat_api,
                model = cfg_ai_model,
                prompts_df = prompts_df
              )

              if(!is.null(result)) {
                cat("[喚醒機會分析] GPT API 成功返回結果\n")
                # 儲存快取
                set_cached_ai(cache_key, result)
                ai_analysis_state$reactivation_analyzed <- TRUE
                return(div(
                  style = "padding: 15px; background: #fff5e6; border-radius: 8px;",
                  h5(get_text("ai_analysis.fallback_titles.ai_result"), style = "color: #ff9800; margin-bottom: 15px;"),
                #  tags$small(api_status, style = "color: #999; display: block; margin-bottom: 10px;"),
                  render_ai_result(result)
                ))
              }
            }, error = function(e) {
              cat("[喚醒機會分析] AI 分析失敗:", e$message, "\n")
            })
          }
          
          # 預設分析
          div(
            style = "padding: 15px; background: #fff5e6; border-radius: 8px;",
            h5(get_text("ai_analysis.fallback_titles.reactivation_assessment"), style = "color: #d97706;"),
            tags$small(api_status, style = "color: #999; display: block; margin-bottom: 10px;"),
            p(paste0(get_text("ai_analysis.fallback_text.customer_count_label"), " ",
                     reactivation_stats$total, " ",
                     get_text("ai_analysis.fallback_text.customers_need_reactivation"))),
            p(paste0(get_text("ai_analysis.fallback_text.potential_value"), " $",
                     format(reactivation_stats$total_value, big.mark = ","))),
            h5(get_text("ai_analysis.fallback_titles.reactivation_strategy_title"), style = "color: #d97706; margin-top: 15px;"),
            tags$ul(
              tags$li(get_text("ai_analysis.reactivation_strategies.s1_sleepy")),
              tags$li(get_text("ai_analysis.reactivation_strategies.s2_half_asleep")),
              tags$li(get_text("ai_analysis.reactivation_strategies.s3_deep_sleep"))
            )
          )
        })
      })
    })
    
    # 忠誠度階梯分析（保持原有邏輯，加入 API 狀態顯示）
    observeEvent(input$btn_analyze_loyalty, {
      req(engagement_metrics())
      data <- engagement_metrics()$raw_data
      
      output$ai_loyalty_ladder <- renderUI({
        withProgress(message = '正在進行 AI 分析...', value = 0.5, {
          
          loyalty_dist <- data %>%
            mutate(
              loyalty_level = case_when(
                times == 1 ~ "初次購買",
                times >= 2 & times <= 3 ~ "偶爾購買",
                times >= 4 & times <= 6 ~ "常客",
                times >= 7 & times <= 10 ~ "忠誠客戶",
                times > 10 ~ "品牌大使"
              )
            ) %>%
            group_by(loyalty_level) %>%
            summarise(count = n(), .groups = 'drop')
          
          # 檢查環境變數和 chat_api 函數
          api_key_exists <- nzchar(Sys.getenv("OPENAI_API_KEY"))
          chat_api_exists <- exists("chat_api") && is.function(chat_api)
          
          api_status <- paste0("API: ", 
                             ifelse(enable_gpt && api_key_exists && chat_api_exists, 
                                   "啟用", "未啟用"))
          
          # 檢查所有必要條件
          use_ai <- enable_gpt && 
                   api_key_exists && 
                   chat_api_exists && 
                   !is.null(prompts_df)
          
          if(use_ai) {
            tryCatch({
              # 確保所有層級都有值
              get_count <- function(level) {
                val <- loyalty_dist$count[loyalty_dist$loyalty_level == level]
                if(length(val) == 0) return(0) else return(val)
              }

              # 取得當前語言
              current_lang <- if (!is.null(lang_texts_reactive()) && !is.null(lang_texts_reactive()$language)) {
                lang_texts_reactive()$language
              } else {
                "zh_TW"
              }

              # 生成快取鍵
              cache_key <- get_ai_cache_key(
                "loyalty_analysis",
                loyalty_dist,
                current_lang
              )

              # 檢查快取
              cached_result <- get_cached_ai(cache_key)
              if (!is.null(cached_result)) {
                cat("[忠誠度階梯分析] 使用快取結果\n")
                ai_analysis_state$loyalty_analyzed <- TRUE
                return(div(
                  style = "padding: 15px; background: #f5f0ff; border-radius: 8px;",
                  h5(get_text("ai_analysis.fallback_titles.ai_result"), style = "color: #7c3aed; margin-bottom: 15px;"),
                  render_ai_result(cached_result)
                ))
              }

              result <- execute_gpt_request(
                var_id = "activity_loyalty_ladder_strategy",
                variables = list(
                  first_time = get_count("初次購買"),
                  occasional = get_count("偶爾購買"),
                  regular = get_count("常客"),
                  loyal = get_count("忠誠客戶"),
                  ambassador = get_count("品牌大使")
                ),
                chat_api_function = chat_api,
                model = cfg_ai_model,
                prompts_df = prompts_df
              )

              if(!is.null(result)) {
                # 儲存快取
                set_cached_ai(cache_key, result)
                ai_analysis_state$loyalty_analyzed <- TRUE
                return(div(
                  style = "padding: 15px; background: #f5f0ff; border-radius: 8px;",
                  h5(get_text("ai_analysis.fallback_titles.ai_result"), style = "color: #7c3aed; margin-bottom: 15px;"),
                 # tags$small(api_status, style = "color: #999; display: block; margin-bottom: 10px;"),
                  render_ai_result(result)
                ))
              }
            }, error = function(e) {
              cat("忠誠度階梯 AI 分析失敗:", e$message, "\n")
            })
          }
          
          # 預設分析
          div(
            style = "padding: 15px; background: #f5f0ff; border-radius: 8px;",
            h5(get_text("ai_analysis.fallback_titles.loyalty_strategy"), style = "color: #7c3aed;"),
            p(get_text("ai_analysis.fallback_text.customer_distribution")),
            h5(get_text("ai_analysis.fallback_titles.upgrade_strategy_title"), style = "color: #7c3aed; margin-top: 15px;"),
            tags$ul(
              tags$li(get_text("ai_analysis.loyalty_upgrade_strategies.first_to_occasional")),
              tags$li(get_text("ai_analysis.loyalty_upgrade_strategies.occasional_to_regular")),
              tags$li(get_text("ai_analysis.loyalty_upgrade_strategies.regular_to_loyal")),
              tags$li(get_text("ai_analysis.loyalty_upgrade_strategies.loyal_to_ambassador"))
            )
          )
        })
      })
    })
    
    # 喚醒客戶清單分析
    observeEvent(input$btn_analyze_list, {
      req(engagement_metrics())
      data <- engagement_metrics()$raw_data
      
      output$ai_reactivation_list <- renderUI({
        withProgress(message = '正在進行 AI 分析...', value = 0.5, {
          
          reactivation_customers <- data %>%
            filter(nes_status %in% c("S1", "S2"))
          
          # 檢查環境變數和 chat_api 函數
          api_key_exists <- nzchar(Sys.getenv("OPENAI_API_KEY"))
          chat_api_exists <- exists("chat_api") && is.function(chat_api)
          
          api_status <- paste0("API: ", 
                             ifelse(enable_gpt && api_key_exists && chat_api_exists, 
                                   "啟用", "未啟用"))
          
          # 檢查所有必要條件
          use_ai <- enable_gpt && 
                   api_key_exists && 
                   chat_api_exists && 
                   !is.null(prompts_df) && 
                   nrow(reactivation_customers) > 0
          
          if(use_ai) {
            tryCatch({
              # 取得當前語言
              current_lang <- if (!is.null(lang_texts_reactive()) && !is.null(lang_texts_reactive()$language)) {
                lang_texts_reactive()$language
              } else {
                "zh_TW"
              }

              # 準備快取資料
              cache_data <- list(
                s1_count = sum(reactivation_customers$nes_status == "S1"),
                s2_count = sum(reactivation_customers$nes_status == "S2"),
                total_historical_value = round(sum(reactivation_customers$total_spent, na.rm = TRUE), 0),
                avg_order_value = round(mean(reactivation_customers$m_value, na.rm = TRUE), 0)
              )

              # 生成快取鍵
              cache_key <- get_ai_cache_key(
                "reactivation_list_analysis",
                cache_data,
                current_lang
              )

              # 檢查快取
              cached_result <- get_cached_ai(cache_key)
              if (!is.null(cached_result)) {
                cat("[喚醒清單分析] 使用快取結果\n")
                ai_analysis_state$list_analyzed <- TRUE
                return(div(
                  style = "padding: 15px; background: #ffebee; border-radius: 8px;",
                  h5("🤖 AI 智能分析", style = "color: #dc3545; margin-bottom: 15px;"),
                  render_ai_result(cached_result)
                ))
              }

              result <- execute_gpt_request(
                var_id = "activity_reactivation_marketing_plan",
                variables = cache_data,
                chat_api_function = chat_api,
                model = cfg_ai_model,
                prompts_df = prompts_df
              )

              if(!is.null(result)) {
                # 儲存快取
                set_cached_ai(cache_key, result)
                ai_analysis_state$list_analyzed <- TRUE
                return(div(
                  style = "padding: 15px; background: #ffebee; border-radius: 8px;",
                  h5("🤖 AI 智能分析", style = "color: #dc3545; margin-bottom: 15px;"),
                  render_ai_result(result)
                ))
              }
            }, error = function(e) {
              cat("喚醒清單 AI 分析失敗:", e$message, "\n")
            })
          }
          
          # 預設分析
          div(
            style = "padding: 15px; background: #ffebee; border-radius: 8px;",
            h5(get_text("ai_analysis.fallback_titles.reactivation_plan"), style = "color: #c62828;"),
            p(paste0(get_text("ai_analysis.fallback_text.reactivation_total"),
                     nrow(reactivation_customers), " ",
                     get_text("ui.units.customers"))),
            h5(get_text("ai_analysis.fallback_titles.execution_plan"), style = "color: #c62828; margin-top: 15px;"),
            tags$ul(
              tags$li(get_text("ai_analysis.reactivation_execution_plan.wave1")),
              tags$li(get_text("ai_analysis.reactivation_execution_plan.wave2")),
              tags$li(get_text("ai_analysis.reactivation_execution_plan.wave3")),
              tags$li(get_text("ai_analysis.reactivation_execution_plan.wave4"))
            ),
            p(get_text("ai_analysis.reactivation_execution_plan.timing_note"), style = "font-style: italic; color: #666;")
          )
        })
      })
    })
    
  })
}
