################################################################################
# 第三列：客戶價值分析模組
# Customer Value Analysis Module
#
# Purpose: 計算並展示 RFM 相關標籤
# - RFM 分數 (R/F/M 各維度)
# - RFM 總分與客戶分群
# - 價值等級分布
################################################################################

library(shiny)
library(dplyr)
library(DT)
library(plotly)

# Source tag calculation functions
if (file.exists("utils/calculate_customer_tags.R")) {
  source("utils/calculate_customer_tags.R")
}

#' Customer Value Analysis Module - UI
#'
#' @param id Module namespace ID
#' @return Shiny UI elements
customerValueAnalysisUI <- function(id) {
  ns <- NS(id)

  div(
    h3("客戶價值分析 (RFM)", style = "text-align: center; margin: 20px 0;"),

    # 狀態顯示（僅錯誤時顯示）
    conditionalPanel(
      condition = paste0("output['", ns("has_error"), "'] == true"),
      wellPanel(
        h4("⚠️ 分析狀態"),
        verbatimTextOutput(ns("status"))
      )
    ),

    # 資料品質警告
    uiOutput(ns("data_quality_warning")),

    # 資料準備就緒後顯示
    conditionalPanel(
      condition = paste0("output['", ns("data_ready"), "'] == true"),

      # === PDF需求 #3.1: 總覽指標卡片 ===
      h4("📊 整體資料總覽", style = "margin-top: 20px; margin-bottom: 20px; border-bottom: 2px solid #007bff; padding-bottom: 10px;"),

      fluidRow(
        column(3,
          bs4ValueBox(
            value = textOutput(ns("total_customers"), inline = TRUE),
            subtitle = "總顧客數",
            icon = icon("users"),
            color = "primary",
            width = 12
          )
        ),
        column(3,
          bs4ValueBox(
            value = textOutput(ns("avg_order_value"), inline = TRUE),
            subtitle = "平均客單價",
            icon = icon("dollar-sign"),
            color = "success",
            width = 12
          )
        ),
        column(3,
          bs4ValueBox(
            value = textOutput(ns("median_purchase_cycle"), inline = TRUE),
            subtitle = "平均購買週期",
            icon = icon("calendar-alt"),
            color = "warning",
            width = 12
          )
        ),
        column(3,
          bs4ValueBox(
            value = textOutput(ns("avg_transaction_count"), inline = TRUE),
            subtitle = "平均交易次數",
            icon = icon("shopping-cart"),
            color = "info",
            width = 12
          )
        )
      ),

      # RFM 關鍵指標卡片
      h4("📊 RFM 分析指標", style = "margin-top: 30px; margin-bottom: 20px; border-bottom: 2px solid #007bff; padding-bottom: 10px;"),

      fluidRow(
        column(3,
          bs4Card(
            title = "顧客平均最近購買日",
            status = "danger",
            solidHeader = TRUE,
            width = 12,
            div(style = "text-align: center; padding: 20px;",
              h2(textOutput(ns("avg_r_value")), style = "color: #dc3545; margin: 0;"),
              p("天", style = "color: #6c757d;")
            )
          )
        ),
        column(3,
          bs4Card(
            title = "顧客平均購買頻率",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            div(style = "text-align: center; padding: 20px;",
              h2(textOutput(ns("avg_f_value")), style = "color: #ffc107; margin: 0;"),
              p("次", style = "color: #6c757d;")
            )
          )
        ),
        column(3,
          bs4Card(
            title = "顧客平均購買金額",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            div(style = "text-align: center; padding: 20px;",
              h2(textOutput(ns("avg_m_value")), style = "color: #28a745; margin: 0;"),
              p("元", style = "color: #6c757d;")
            )
          )
        ),
        column(3,
          bs4Card(
            title = "平均 RFM 總分",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            div(style = "text-align: center; padding: 20px;",
              h2(textOutput(ns("avg_rfm_score")), style = "color: #17a2b8; margin: 0;"),
              p("分（3-15分）", style = "color: #6c757d; font-size: 0.9em;")
            )
          )
        )
      ),

      # === PDF需求：R/F/M 分群分析（圓餅圖） ===
      h4("📊 RFM 分群分析", style = "margin-top: 30px; margin-bottom: 20px; border-bottom: 2px solid #007bff; padding-bottom: 10px;"),

      # Row 1: R value 分群
      # PDF需求 #3.2.1: 改標題為「最近購買日」
      fluidRow(
        column(6,
          bs4Card(
            title = "最近購買日（R Value）",
            status = "danger",
            solidHeader = TRUE,
            width = 12,
            DTOutput(ns("r_segment_table"))
          )
        ),
        column(6,
          bs4Card(
            title = "最近購買日分群佔比",
            status = "danger",
            solidHeader = FALSE,
            width = 12,
            plotlyOutput(ns("r_segment_pie"), height = "300px")
          )
        )
      ),

      # Row 2: F value 分群
      fluidRow(
        column(6,
          bs4Card(
            title = "買家購買頻率分群（F Value）",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            DTOutput(ns("f_segment_table"))
          )
        ),
        column(6,
          bs4Card(
            title = "購買頻率分群佔比",
            status = "warning",
            solidHeader = FALSE,
            width = 12,
            plotlyOutput(ns("f_segment_pie"), height = "300px")
          )
        )
      ),

      # Row 3: M value 分群
      fluidRow(
        column(6,
          bs4Card(
            title = "買家購買金額分群（M Value）",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            DTOutput(ns("m_segment_table"))
          )
        ),
        column(6,
          bs4Card(
            title = "購買金額分群佔比",
            status = "success",
            solidHeader = FALSE,
            width = 12,
            plotlyOutput(ns("m_segment_pie"), height = "300px")
          )
        )
      ),

      # RFM 分數分布圖表
      fluidRow(
        column(4,
          bs4Card(
            title = "顧客平均最近購買日分布",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput(ns("r_distribution_plot"), height = "300px")
          )
        ),
        column(4,
          bs4Card(
            title = "顧客平均購買頻率分布",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput(ns("f_distribution_plot"), height = "300px")
          )
        ),
        column(4,
          bs4Card(
            title = "顧客平均購買金額分布",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput(ns("m_distribution_plot"), height = "300px")
          )
        )
      ),

      # RFM 總分與價值分群
      fluidRow(
        column(6,
          bs4Card(
            title = "RFM 總分分布",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput(ns("rfm_score_plot"), height = "350px")
          )
        ),
        column(6,
          bs4Card(
            title = "客戶價值分群",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput(ns("value_segment_plot"), height = "350px")
          )
        )
      ),

      # RFM 熱力圖（M vs F）
      # PDF需求 #3.3: 橫軸換購買金額(M)；縱軸的頻率 F 改成「次」
      fluidRow(
        column(12,
          bs4Card(
            title = "RFM 熱力圖：購買金額 vs 頻率（氣泡大小 = 最近購買日）",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput(ns("rfm_heatmap"), height = "450px")
          )
        )
      ),

      # 詳細資料表
      fluidRow(
        column(12,
          bs4Card(
            title = "客戶 RFM 詳細資料",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            fluidRow(
              column(6,
                p(style = "margin-top: 10px;", "顯示前 100 筆客戶資料，按 RFM 總分排序")
              ),
              column(6, style = "text-align: right;",
                # PDF需求 #3.4: 下載時顯示 UTF-8 BOM 提醒
                actionButton(ns("show_download_warning"), "下載說明", icon = icon("info-circle"), class = "btn-info btn-sm", style = "margin-right: 10px;"),
                downloadButton(ns("download_data"), "📥 下載完整 RFM 資料 (CSV)", class = "btn-success")
              )
            ),
            DTOutput(ns("customer_table"))
          )
        )
      )
    )
  )
}

#' Customer Value Analysis Module - Server
#'
#' @param id Module namespace ID
#' @param customer_data reactive - 包含客戶資料的 reactive 物件
#' @return reactive - 加入 RFM 標籤的客戶資料
customerValueAnalysisServer <- function(id, customer_data) {
  moduleServer(id, function(input, output, session) {

    # Reactive values
    values <- reactiveValues(
      processed_data = NULL,
      status_text = "等待資料...",
      data_quality_issues = list()
    )

    # 處理資料並計算 RFM 標籤
    observe({
      req(customer_data())

      tryCatch({
        values$status_text <- "📊 計算 RFM 客戶價值標籤中..."

        # 使用 utils 中的函數計算 RFM 標籤
        processed <- customer_data() %>%
          calculate_rfm_tags()

        values$processed_data <- processed

        # 檢查資料品質
        issues <- list()

        # 檢查 M 值變異度
        if ("tag_011_rfm_m" %in% names(processed)) {
          m_values <- processed$tag_011_rfm_m[!is.na(processed$tag_011_rfm_m)]
          if (length(m_values) > 0) {
            m_cv <- sd(m_values) / mean(m_values)  # Coefficient of variation
            m_median <- median(m_values)
            m_p20 <- quantile(m_values, 0.2)
            m_p80 <- quantile(m_values, 0.8)

            # 低變異度：CV < 0.15 或 P20/Median/P80 相同
            if (m_cv < 0.15 || (m_p20 == m_median && m_median == m_p80)) {
              issues$m_low_variance <- list(
                type = "warning",
                title = "⚠️ M 值（消費金額）變異度較低",
                message = sprintf(
                  "您的資料中有大量客戶的平均消費金額集中在 %s 元附近（變異係數: %.2f）。這可能導致「中消費買家」分群難以區分。\n\n建議：\n• 檢查資料是否包含多元價格帶的產品\n• 確認資料時間範圍是否足夠（建議 12-36 個月）\n• 若為固定價格商品，RFM 分析的 M 值參考性較低",
                  format(round(m_median, 0), big.mark = ","),
                  m_cv
                )
              )
            }
          }
        }

        # 檢查 F 值變異度
        if ("tag_010_rfm_f" %in% names(processed)) {
          f_values <- processed$tag_010_rfm_f[!is.na(processed$tag_010_rfm_f)]
          if (length(f_values) > 0) {
            single_purchase_pct <- mean(f_values < 1.5) * 100  # F < 1.5 視為單次購買
            if (single_purchase_pct > 90) {
              issues$f_high_single <- list(
                type = "info",
                title = "ℹ️ 高比例單次購買客戶",
                message = sprintf(
                  "您的資料中有 %.1f%% 的客戶僅購買過一次。在短時間範圍內這是正常的，但可能影響 F 值（購買頻率）的分析意義。\n\n建議：\n• 擴大資料時間範圍以觀察重複購買行為\n• 關注回購率提升策略",
                  single_purchase_pct
                )
              )
            }
          }
        }

        values$data_quality_issues <- issues
        # 成功時不顯示訊息
        values$status_text <- ""

      }, error = function(e) {
        values$status_text <- paste("❌ 錯誤:", e$message)
      })
    })

    # 控制 UI 顯示
    output$data_ready <- reactive({
      !is.null(values$processed_data)
    })
    outputOptions(output, "data_ready", suspendWhenHidden = FALSE)

    # 控制錯誤訊息顯示
    output$has_error <- reactive({
      !is.null(values$status_text) && values$status_text != "" && grepl("錯誤", values$status_text)
    })
    outputOptions(output, "has_error", suspendWhenHidden = FALSE)

    # 狀態輸出
    output$status <- renderText({
      values$status_text
    })

    # 資料品質警告輸出
    output$data_quality_warning <- renderUI({
      issues <- values$data_quality_issues

      if (length(issues) == 0) {
        return(NULL)
      }

      # 建立警告卡片
      warnings_ui <- lapply(names(issues), function(issue_name) {
        issue <- issues[[issue_name]]

        # 根據類型選擇顏色
        alert_class <- if (issue$type == "warning") {
          "alert-warning"
        } else if (issue$type == "info") {
          "alert-info"
        } else {
          "alert-danger"
        }

        div(
          class = paste("alert", alert_class, "alert-dismissible fade show"),
          role = "alert",
          style = "margin: 15px 0;",
          tags$strong(issue$title),
          tags$br(), tags$br(),
          tags$pre(
            style = "background-color: transparent; border: none; padding: 0; margin: 0; white-space: pre-wrap; font-family: inherit;",
            issue$message
          ),
          tags$button(
            type = "button",
            class = "close",
            `data-dismiss` = "alert",
            `aria-label` = "Close",
            tags$span(`aria-hidden` = "true", HTML("&times;"))
          )
        )
      })

      div(warnings_ui)
    })

    # ==========================================================================
    # PDF需求 #3.1: 總覽指標
    # ==========================================================================

    # 總顧客數
    output$total_customers <- renderText({
      req(values$processed_data)
      n_customers <- nrow(values$processed_data)
      format(n_customers, big.mark = ",")
    })

    # 平均客單價 (Average Order Value)
    output$avg_order_value <- renderText({
      req(values$processed_data)
      # 客單價 = 平均每次購買金額 (M value)
      avg_aov <- mean(values$processed_data$tag_011_rfm_m, na.rm = TRUE)
      paste0("$", format(round(avg_aov, 0), big.mark = ","))
    })

    # 中位購買週期 (Median Purchase Cycle)
    output$median_purchase_cycle <- renderText({
      req(values$processed_data)

      df <- values$processed_data

      # 必須同時具備 ipt (或 ipt_mean) 與 ni 欄位
      if (!("ni" %in% names(df)) ||
          (!("ipt" %in% names(df)) && !("ipt_mean" %in% names(df)))) {
        return("N/A")
      }

      # 將原始 ipt 欄位統一為 ipt_raw，便於後續計算
      df <- df %>%
        mutate(
          ipt_raw = if ("ipt" %in% names(df)) ipt else ipt_mean
        )

      # 平均購買間隔 = 總時間跨度 / (購買次數 - 1)
      ipt_values <- df %>%
        filter(
          ni >= 2,              # 排除單次購買顧客
          !is.na(ipt_raw),
          ipt_raw > 0
        ) %>%
        mutate(avg_ipt = ipt_raw / pmax(ni - 1, 1)) %>%
        pull(avg_ipt)

      if (length(ipt_values) > 0) {
        median_cycle <- median(ipt_values, na.rm = TRUE)

        # 根據數值大小選擇合適的顯示格式
        if (median_cycle >= 365) {
          # 顯示年
          years <- median_cycle / 365
          paste0(format(round(years, 1), big.mark = ","), " 年/次")
        } else if (median_cycle >= 30) {
          # 顯示月
          months <- median_cycle / 30
          paste0(format(round(months, 1), big.mark = ","), " 月/次")
        } else {
          # 顯示天
          paste0(format(round(median_cycle, 1), big.mark = ","), " 天/次")
        }
      } else {
        "N/A"
      }
    })

    # 平均交易次數
    output$avg_transaction_count <- renderText({
      req(values$processed_data)
      # F value 就是購買頻率（次數）
      avg_transactions <- mean(values$processed_data$tag_010_rfm_f, na.rm = TRUE)
      paste0(format(round(avg_transactions, 1), big.mark = ","), " 次")
    })

    # ==========================================================================
    # RFM 關鍵指標
    # ==========================================================================

    output$avg_r_value <- renderText({
      req(values$processed_data)
      avg_val <- mean(values$processed_data$tag_009_rfm_r, na.rm = TRUE)
      format(round(avg_val, 1), big.mark = ",")
    })

    output$avg_f_value <- renderText({
      req(values$processed_data)
      avg_val <- mean(values$processed_data$tag_010_rfm_f, na.rm = TRUE)
      format(round(avg_val, 2), big.mark = ",")
    })

    output$avg_m_value <- renderText({
      req(values$processed_data)
      avg_val <- mean(values$processed_data$tag_011_rfm_m, na.rm = TRUE)
      format(round(avg_val, 0), big.mark = ",")
    })

    output$avg_rfm_score <- renderText({
      req(values$processed_data)
      avg_val <- mean(values$processed_data$tag_012_rfm_score, na.rm = TRUE)
      format(round(avg_val, 1), big.mark = ",")
    })

    # ==========================================================================
    # PDF需求：R/F/M 分群分析（圓餅圖）
    # ==========================================================================

    # R value 分群 reactive
    r_segment_data <- reactive({
      req(values$processed_data)

      df <- values$processed_data %>%
        filter(!is.na(tag_009_rfm_r))

      # ✅ 需求 #2: 使用 P20/P80 (80/20法則) 統一分群標準
      p80 <- quantile(df$tag_009_rfm_r, 0.80, na.rm = TRUE)
      p20 <- quantile(df$tag_009_rfm_r, 0.20, na.rm = TRUE)

      df %>%
        mutate(
          r_segment = case_when(
            tag_009_rfm_r <= p20 ~ "最近買家",      # 底部 20%: 最近購買
            tag_009_rfm_r <= p80 ~ "中期買家",      # 中間 60%
            TRUE ~ "長期未購者"                     # 頂部 20%: 很久沒買
          )
        ) %>%
        group_by(r_segment) %>%
        summarise(
          客戶數量 = n(),
          百分比 = sprintf("%.1f%%", n() / nrow(df) * 100),
          平均R值天數 = round(mean(tag_009_rfm_r, na.rm = TRUE), 1),
          .groups = "drop"
        ) %>%
        mutate(
          r_segment = factor(r_segment, levels = c("最近買家", "中期買家", "長期未購者"))
        ) %>%
        arrange(r_segment)
    })

    # F value 分群 reactive
    f_segment_data <- reactive({
      req(values$processed_data)

      df <- values$processed_data %>%
        filter(!is.na(tag_010_rfm_f))

      # 智能分群邏輯：結合固定閾值和百分位數
      # 檢查單次購買比例
      single_purchase_pct <- mean(df$tag_010_rfm_f < 1.5, na.rm = TRUE)

      if (single_purchase_pct > 0.7) {
        # 如果70%以上只購買一次，使用固定閾值
        # 低頻：<= 1次，中頻：1-2次，高頻：>2次
        df %>%
          mutate(
            f_segment = case_when(
              tag_010_rfm_f > 2 ~ "高頻買家",
              tag_010_rfm_f > 1 ~ "中頻買家",
              TRUE ~ "低頻買家"
            )
          ) %>%
          group_by(f_segment) %>%
          summarise(
            客戶數量 = n(),
            百分比 = sprintf("%.1f%%", n() / nrow(df) * 100),
            平均購買次數 = round(mean(tag_010_rfm_f, na.rm = TRUE), 2),
            .groups = "drop"
          ) %>%
          mutate(
            f_segment = factor(f_segment, levels = c("高頻買家", "中頻買家", "低頻買家"))
          ) %>%
          arrange(desc(f_segment))
      } else {
        # ✅ 需求 #2: 使用 P20/P80 (80/20法則) 統一分群標準
        p80 <- quantile(df$tag_010_rfm_f, 0.80, na.rm = TRUE)
        p20 <- quantile(df$tag_010_rfm_f, 0.20, na.rm = TRUE)

        df %>%
          mutate(
            f_segment = case_when(
              tag_010_rfm_f >= p80 ~ "高頻買家",    # 頂部 20%: 購買頻率高
              tag_010_rfm_f >= p20 ~ "中頻買家",    # 中間 60%
              TRUE ~ "低頻買家"                     # 底部 20%: 購買頻率低
            )
          ) %>%
          group_by(f_segment) %>%
          summarise(
            客戶數量 = n(),
            百分比 = sprintf("%.1f%%", n() / nrow(df) * 100),
            平均購買次數 = round(mean(tag_010_rfm_f, na.rm = TRUE), 2),
            .groups = "drop"
          ) %>%
          mutate(
            f_segment = factor(f_segment, levels = c("高頻買家", "中頻買家", "低頻買家"))
          ) %>%
          arrange(desc(f_segment))
      }
    })

    # M value 分群 reactive
    m_segment_data <- reactive({
      req(values$processed_data)

      df <- values$processed_data %>%
        filter(!is.na(tag_011_rfm_m))

      # 智能分群邏輯：檢查變異度
      m_cv <- sd(df$tag_011_rfm_m, na.rm = TRUE) / mean(df$tag_011_rfm_m, na.rm = TRUE)
      m_median <- median(df$tag_011_rfm_m, na.rm = TRUE)
      m_p20 <- quantile(df$tag_011_rfm_m, 0.2, na.rm = TRUE)
      m_p80 <- quantile(df$tag_011_rfm_m, 0.8, na.rm = TRUE)

      # 如果變異度太低（CV < 0.2）或 P20/Median/P80 太接近，使用平均值±標準差切分
      if (m_cv < 0.2 || (m_p80 - m_p20) / m_median < 0.3) {
        # 低變異度：使用均值±0.5倍標準差切分
        m_mean <- mean(df$tag_011_rfm_m, na.rm = TRUE)
        m_sd <- sd(df$tag_011_rfm_m, na.rm = TRUE)
        threshold_low <- m_mean - 0.5 * m_sd
        threshold_high <- m_mean + 0.5 * m_sd

        df %>%
          mutate(
            m_segment = case_when(
              tag_011_rfm_m > threshold_high ~ "高消費買家",
              tag_011_rfm_m >= threshold_low ~ "中消費買家",
              TRUE ~ "低消費買家"
            )
          ) %>%
          group_by(m_segment) %>%
          summarise(
            客戶數量 = n(),
            百分比 = sprintf("%.1f%%", n() / nrow(df) * 100),
            平均消費金額 = round(mean(tag_011_rfm_m, na.rm = TRUE), 0),
            .groups = "drop"
          ) %>%
          mutate(
            m_segment = factor(m_segment, levels = c("高消費買家", "中消費買家", "低消費買家"))
          ) %>%
          arrange(desc(m_segment))
      } else {
        # ✅ 需求 #2: 使用 P20/P80 (80/20法則) 統一分群標準
        p80 <- quantile(df$tag_011_rfm_m, 0.80, na.rm = TRUE)
        p20 <- quantile(df$tag_011_rfm_m, 0.20, na.rm = TRUE)

        df %>%
          mutate(
            m_segment = case_when(
              tag_011_rfm_m >= p80 ~ "高消費買家",    # 頂部 20%: 高消費
              tag_011_rfm_m >= p20 ~ "中消費買家",    # 中間 60%
              TRUE ~ "低消費買家"                     # 底部 20%: 低消費
            )
          ) %>%
          group_by(m_segment) %>%
          summarise(
            客戶數量 = n(),
            百分比 = sprintf("%.1f%%", n() / nrow(df) * 100),
            平均消費金額 = round(mean(tag_011_rfm_m, na.rm = TRUE), 0),
            .groups = "drop"
          ) %>%
          mutate(
            m_segment = factor(m_segment, levels = c("高消費買家", "中消費買家", "低消費買家"))
          ) %>%
          arrange(desc(m_segment))
      }
    })

    # R 分群表格
    output$r_segment_table <- renderDT({
      req(r_segment_data())

      datatable(
        r_segment_data() %>% rename(分群 = r_segment),
        options = list(
          dom = 't',
          ordering = FALSE,
          pageLength = 10
        ),
        rownames = FALSE,
        class = 'cell-border stripe'
      ) %>%
        formatStyle(
          '分群',
          backgroundColor = styleEqual(
            c("最近買家", "中期買家", "長期未購者"),
            c("#d4edda", "#fff3cd", "#f8d7da")
          )
        )
    })

    # R 分群圓餅圖
    output$r_segment_pie <- renderPlotly({
      req(r_segment_data())

      plot_ly(
        data = r_segment_data(),
        labels = ~r_segment,
        values = ~客戶數量,
        type = 'pie',
        marker = list(
          colors = c("#28a745", "#ffc107", "#dc3545"),
          line = list(color = '#FFFFFF', width = 2)
        ),
        textinfo = 'label+percent',
        textposition = 'inside',
        hoverinfo = 'label+value+percent'
      ) %>%
        layout(
          showlegend = TRUE,
          legend = list(orientation = 'v', x = 1.1, y = 0.5)
        )
    })

    # F 分群表格
    output$f_segment_table <- renderDT({
      req(f_segment_data())

      datatable(
        f_segment_data() %>% rename(分群 = f_segment),
        options = list(
          dom = 't',
          ordering = FALSE,
          pageLength = 10
        ),
        rownames = FALSE,
        class = 'cell-border stripe'
      ) %>%
        formatStyle(
          '分群',
          backgroundColor = styleEqual(
            c("高頻買家", "中頻買家", "低頻買家"),
            c("#d4edda", "#fff3cd", "#f8d7da")
          )
        )
    })

    # F 分群圓餅圖
    output$f_segment_pie <- renderPlotly({
      req(f_segment_data())

      plot_ly(
        data = f_segment_data(),
        labels = ~f_segment,
        values = ~客戶數量,
        type = 'pie',
        marker = list(
          colors = c("#28a745", "#ffc107", "#dc3545"),
          line = list(color = '#FFFFFF', width = 2)
        ),
        textinfo = 'label+percent',
        textposition = 'inside',
        hoverinfo = 'label+value+percent'
      ) %>%
        layout(
          showlegend = TRUE,
          legend = list(orientation = 'v', x = 1.1, y = 0.5)
        )
    })

    # M 分群表格
    output$m_segment_table <- renderDT({
      req(m_segment_data())

      datatable(
        m_segment_data() %>% rename(分群 = m_segment),
        options = list(
          dom = 't',
          ordering = FALSE,
          pageLength = 10
        ),
        rownames = FALSE,
        class = 'cell-border stripe'
      ) %>%
        formatStyle(
          '分群',
          backgroundColor = styleEqual(
            c("高消費買家", "中消費買家", "低消費買家"),
            c("#d4edda", "#fff3cd", "#f8d7da")
          )
        )
    })

    # M 分群圓餅圖
    output$m_segment_pie <- renderPlotly({
      req(m_segment_data())

      plot_ly(
        data = m_segment_data(),
        labels = ~m_segment,
        values = ~客戶數量,
        type = 'pie',
        marker = list(
          colors = c("#28a745", "#ffc107", "#dc3545"),
          line = list(color = '#FFFFFF', width = 2)
        ),
        textinfo = 'label+percent',
        textposition = 'inside',
        hoverinfo = 'label+value+percent'
      ) %>%
        layout(
          showlegend = TRUE,
          legend = list(orientation = 'v', x = 1.1, y = 0.5)
        )
    })

    # ==========================================================================
    # RFM 分數分布圖表
    # ==========================================================================

    output$r_distribution_plot <- renderPlotly({
      req(values$processed_data)

      plot_ly(
        x = values$processed_data$tag_009_rfm_r,
        type = "histogram",
        marker = list(color = "#dc3545"),
        nbinsx = 30
      ) %>%
        layout(
          xaxis = list(title = "R 值（天）"),
          yaxis = list(title = "客戶數"),
          title = ""
        )
    })

    output$f_distribution_plot <- renderPlotly({
      req(values$processed_data)

      plot_ly(
        x = values$processed_data$tag_010_rfm_f,
        type = "histogram",
        marker = list(color = "#ffc107"),
        nbinsx = 30
      ) %>%
        layout(
          xaxis = list(title = "F 值（次）"),
          yaxis = list(title = "客戶數"),
          title = ""
        )
    })

    output$m_distribution_plot <- renderPlotly({
      req(values$processed_data)

      plot_ly(
        x = values$processed_data$tag_011_rfm_m,
        type = "histogram",
        marker = list(color = "#28a745"),
        nbinsx = 30
      ) %>%
        layout(
          xaxis = list(title = "M 值（元）"),
          yaxis = list(title = "客戶數"),
          title = ""
        )
    })

    # ==========================================================================
    # RFM 總分與價值分群圖表
    # ==========================================================================

    output$rfm_score_plot <- renderPlotly({
      req(values$processed_data)

      # 只顯示有 RFM 總分的客戶
      data_with_score <- values$processed_data %>%
        filter(!is.na(tag_012_rfm_score))

      plot_ly(
        data = data_with_score,
        x = ~tag_012_rfm_score,
        type = "histogram",
        marker = list(color = "#17a2b8"),
        nbinsx = 13
      ) %>%
        layout(
          xaxis = list(title = "RFM 總分（3-15分）", dtick = 1),
          yaxis = list(title = "客戶數"),
          title = ""
        )
    })

    output$value_segment_plot <- renderPlotly({
      req(values$processed_data)

      # 計算價值分群數量
      segment_counts <- values$processed_data %>%
        count(tag_013_value_segment) %>%
        arrange(desc(n))

      # 定義顏色映射
      color_map <- c(
        "高" = "#28a745",
        "中" = "#ffc107",
        "低" = "#dc3545"
      )

      plot_ly(
        data = segment_counts,
        x = ~tag_013_value_segment,
        y = ~n,
        type = "bar",
        marker = list(color = ~color_map[tag_013_value_segment])
      ) %>%
        layout(
          xaxis = list(title = "價值分群"),
          yaxis = list(title = "客戶數"),
          title = ""
        )
    })

    # ==========================================================================
    # RFM 熱力圖（R vs F，氣泡大小 = M）
    # ==========================================================================

    output$rfm_heatmap <- renderPlotly({
      req(values$processed_data)

      # 只顯示有完整 RFM 資料的客戶
      data_complete <- values$processed_data %>%
        filter(!is.na(tag_009_rfm_r), !is.na(tag_010_rfm_f), !is.na(tag_011_rfm_m))

      # 如果資料太多，採樣顯示
      if (nrow(data_complete) > 500) {
        data_complete <- data_complete %>%
          sample_n(500)
      }

      # PDF需求 #3.3: 橫軸=購買金額(M), 縱軸=頻率(F), 氣泡大小=新近度(R)
      plot_ly(
        data = data_complete,
        x = ~tag_011_rfm_m,        # 橫軸：購買金額 M
        y = ~tag_010_rfm_f,        # 縱軸：頻率 F
        size = ~tag_009_rfm_r,     # 氣泡大小：新近度 R
        color = ~tag_013_value_segment,
        colors = c("高" = "#28a745", "中" = "#ffc107", "低" = "#dc3545"),
        type = "scatter",
        mode = "markers",
        marker = list(
          sizemode = "diameter",
          sizeref = 2,
          opacity = 0.6
        ),
        text = ~paste0(
          "客戶: ", customer_id, "<br>",
          "M: ", format(round(tag_011_rfm_m, 0), big.mark = ","), " 元<br>",
          "F: ", round(tag_010_rfm_f, 2), " 次<br>",
          "R: ", round(tag_009_rfm_r, 1), " 天<br>",
          "價值: ", tag_013_value_segment
        ),
        hoverinfo = "text"
      ) %>%
        layout(
          xaxis = list(title = "購買金額 M（元）"),
          yaxis = list(title = "頻率 F（次/月）"),
          title = "",
          showlegend = TRUE,
          legend = list(title = list(text = "價值分群"))
        )
    })

    # ==========================================================================
    # 資料表
    # ==========================================================================

    output$customer_table <- renderDT({
      req(values$processed_data)

      # 選擇要顯示的欄位
      display_data <- values$processed_data %>%
        select(
          customer_id,
          購買次數 = ni,
          R值_新近度 = tag_009_rfm_r,
          F值_頻率 = tag_010_rfm_f,
          M值_金額 = tag_011_rfm_m,
          RFM總分 = tag_012_rfm_score,
          價值分群 = tag_013_value_segment
        ) %>%
        arrange(desc(RFM總分)) %>%
        head(100)  # 只顯示前 100 筆

      # 格式化數值
      display_data$R值_新近度 <- round(display_data$R值_新近度, 1)
      display_data$F值_頻率 <- round(display_data$F值_頻率, 2)
      display_data$M值_金額 <- format(round(display_data$M值_金額, 0), big.mark = ",")
      display_data$RFM總分 <- ifelse(is.na(display_data$RFM總分), "-", as.character(display_data$RFM總分))

      datatable(
        display_data,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Chinese-traditional.json')
        ),
        rownames = FALSE
      )
    })

    # ==========================================================================
    # CSV 下載 + UTF-8 BOM 提醒
    # PDF需求 #3.4
    # ==========================================================================

    # 顯示下載說明 Modal
    observeEvent(input$show_download_warning, {
      showModal(modalDialog(
        title = tags$h4(icon("info-circle"), " CSV 檔案下載說明"),
        tags$div(
          style = "font-size: 15px; line-height: 1.8;",
          tags$p(
            tags$strong("📥 如何正確開啟下載的 CSV 檔案：")
          ),
          tags$div(
            style = "background-color: #f8f9fa; padding: 15px; border-left: 4px solid #17a2b8; margin: 15px 0;",
            tags$p(
              style = "margin: 0;",
              "若使用 EXCEL 開啟檔案出現", tags$strong(style = "color: #dc3545;", "亂碼"),
              "，請依照以下步驟處理："
            )
          ),
          tags$ol(
            style = "margin-left: 20px;",
            tags$li("用", tags$strong("記事本（Notepad）"), "重新開啟下載的 CSV 檔案"),
            tags$li("點選「", tags$strong("另存新檔"), "」"),
            tags$li("編碼選擇：", tags$strong(style = "color: #28a745;", "使用 BOM 的 UTF-8"), ""),
            tags$li("儲存後，再用 EXCEL 重新開啟檔案")
          ),
          tags$div(
            style = "background-color: #d4edda; padding: 10px; border-radius: 5px; margin-top: 15px;",
            tags$p(
              style = "margin: 0; color: #155724;",
              icon("check-circle"), " 這樣就能正確顯示中文字了！"
            )
          )
        ),
        footer = tagList(
          modalButton("關閉")
        ),
        easyClose = TRUE,
        size = "m"
      ))
    })

    output$download_data <- downloadHandler(
      filename = function() {
        paste0("customer_rfm_analysis_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(values$processed_data)

        export_data <- values$processed_data %>%
          select(
            customer_id,
            ni,
            tag_009_rfm_r,
            tag_010_rfm_f,
            tag_011_rfm_m,
            tag_012_rfm_score,
            tag_013_value_segment
          ) %>%
          rename(
            `購買次數` = ni,
            `R值_新近度` = tag_009_rfm_r,
            `F值_頻率` = tag_010_rfm_f,
            `M值_金額` = tag_011_rfm_m,
            `RFM總分` = tag_012_rfm_score,
            `價值分群` = tag_013_value_segment
          )

        write.csv(export_data, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )

    # ==========================================================================
    # 回傳處理後的資料（供下一個模組使用）
    # ==========================================================================

    return(reactive({ values$processed_data }))
  })
}

################################################################################
# 使用範例（在 app.R 中）
################################################################################
#
# # UI
# customerValueAnalysisUI("rfm_analysis")
#
# # Server
# rfm_data <- customerValueAnalysisServer("rfm_analysis", base_value_data)
#
################################################################################
