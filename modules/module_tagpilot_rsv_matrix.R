# ==============================================================================
# R/S/V Matrix Module (Risk-Stability-Value)
# ==============================================================================
# Purpose: Customer vitality analysis using three dimensions:
#   - R (Risk): Dormancy risk / 靜止風險
#   - S (Stability): Transaction regularity / 交易穩定度
#   - V (Value): Customer lifetime value / 顧客終身價值
#
# RSV Framework v3.0 - 使用 MAMBA 真實 RSV 變數:
# =====================================================================
#   R (Risk): 優先使用 nrec_prob（邏輯回歸預測流失機率 0-1）
#     - nrec_prob > 0.7 → 高靜止戶（即將或已經流失）
#     - nrec_prob 0.3-0.7 → 中靜止戶（互動減少但仍有潛力）
#     - nrec_prob < 0.3 → 低靜止戶（穩定活躍群）
#     - Fallback: customer_dynamics → r_value 分位數
#
#   S (Stability): 優先使用 cri（Customer Regularity Index 0-1）
#     - cri 接近 0 → 高穩定顧客（固定頻率與金額、行為一致）
#     - cri 中等 → 中穩定顧客（有規律但偶爾波動）
#     - cri 接近 1 → 低穩定顧客（購買間隔不固定、交易起伏大）
#     - Fallback: ni 分位數
#
#   V (Value): 優先使用 clv（預測未來 10 年價值）
#     - clv 前 20% → 高價值顧客（高消費力、高忠誠度）
#     - clv 中間 60% → 中價值顧客（穩定貢獻、可升級）
#     - clv 後 20% → 低價值顧客（消費低或不穩）
#     - Fallback: total_spent → m_value * ni
#
# Reference:
#   - documents/03_requirements/2025-12-03_rsv_framework_feasibility_analysis.md
#   - scripts/global_scripts/04_utils/fn_analysis_dna.R
#
# Author: Claude AI Assistant
# Date: 2025-10-25
# Last Updated: 2025-12-03
# Version: 3.0 - Using MAMBA's actual RSV variables (nrec_prob, cri, clv)
# ==============================================================================

library(shiny)
library(bs4Dash)
library(dplyr)
library(DT)
library(plotly)
library(purrr)

# ==============================================================================
# UI Function
# ==============================================================================

rsvMatrixUI <- function(id) {
  ns <- NS(id)

  div(
    h3("R/S/V 顧客生命力矩陣"),
    p(class = "text-muted",
      "整合三維度分析：R (靜止風險) × S (交易穩定度) × V (終生價值)，提供 27 種客戶類型與策略建議"),

    # Status panel
    wellPanel(
      style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; border: none;",
      h4("📊 分析狀態", style = "color: white; margin-top: 0;"),
      verbatimTextOutput(ns("status"))
    ),

    # Key metrics cards
    fluidRow(
      column(4,
        bs4Card(
          title = "🔴 高風險客戶",
          status = "danger",
          solidHeader = TRUE,
          width = 12,
          div(
            h2(textOutput(ns("high_risk_count")), style = "margin: 0;"),
            p(textOutput(ns("high_risk_pct")), class = "text-muted")
          )
        )
      ),
      column(4,
        bs4Card(
          title = "⭐ 高穩定客戶",
          status = "success",
          solidHeader = TRUE,
          width = 12,
          div(
            h2(textOutput(ns("high_stability_count")), style = "margin: 0;"),
            p(textOutput(ns("high_stability_pct")), class = "text-muted")
          )
        )
      ),
      column(4,
        bs4Card(
          title = "💎 高價值客戶",
          status = "warning",
          solidHeader = TRUE,
          width = 12,
          div(
            h2(textOutput(ns("high_value_count")), style = "margin: 0;"),
            p(textOutput(ns("high_value_pct")), class = "text-muted")
          )
        )
      )
    ),

    # R/S/V Distribution Charts
    fluidRow(
      column(4,
        bs4Card(
          title = "R - 靜止風險分布",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          plotlyOutput(ns("risk_distribution"))
        )
      ),
      column(4,
        bs4Card(
          title = "S - 交易穩定度分布",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          plotlyOutput(ns("stability_distribution"))
        )
      ),
      column(4,
        bs4Card(
          title = "V - 終生價值分布",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          plotlyOutput(ns("value_distribution"))
        )
      )
    ),

    # Issue #11: 移除 3D 散點圖（與下方表格資訊重疊）
    # 3D Matrix Visualization
    # bs4Card(
    #   title = "R × S × V 生命力矩陣（三維可視化）",
    #   status = "primary",
    #   solidHeader = TRUE,
    #   width = 12,
    #   p(class = "text-muted", "點擊任一格查看該類客戶名單與策略建議"),
    #   plotlyOutput(ns("rsv_3d_scatter"), height = "600px")
    # ),

    # Matrix Heatmap (R × S with V as color)
    bs4Card(
      title = "R × S 矩陣（V 作為顏色深度）",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      plotlyOutput(ns("rsv_heatmap"), height = "500px")
    ),

    # Strategy Table
    # 2025-12-26: Comment out - 訂閱版暫不顯示
    # bs4Card(
    #   title = "客戶類型與策略對應表",
    #   status = "success",
    #   solidHeader = TRUE,
    #   width = 12,
    #   DTOutput(ns("strategy_table"))
    # ),

    # Customer Detail Table
    bs4Card(
      title = "客戶明細（含 R/S/V 標籤）",
      status = "info",
      solidHeader = TRUE,
      width = 12,
      DTOutput(ns("customer_table")),
      downloadButton(ns("download_data"), "📥 下載完整資料 (CSV)", class = "btn-primary mt-3")
    )
  )
}

# ==============================================================================
# Server Function
# ==============================================================================

rsvMatrixServer <- function(id, customer_data) {
  moduleServer(id, function(input, output, session) {

    # Reactive values
    values <- reactiveValues(
      processed_data = NULL,
      strategy_summary = NULL
    )

    # ===========================================================================
    # Data Processing
    # ===========================================================================

    observe({
      req(customer_data())

      tryCatch({
        df <- customer_data()

        # Calculate R/S/V metrics
        # =====================================================================
        # RSV Framework v3.0 - Using MAMBA's actual RSV variables
        # =====================================================================
        # R (Risk): 優先使用 nrec_prob（流失機率 0-1）
        #   - nrec_prob > 0.7 → 高靜止戶
        #   - nrec_prob 0.3-0.7 → 中靜止戶
        #   - nrec_prob < 0.3 → 低靜止戶
        #   - Fallback: customer_dynamics → r_value 分位數
        #
        # S (Stability): 優先使用 cri（Customer Regularity Index）
        #   - cri 接近 0 → 高穩定（使用 cri_ecdf 分位數）
        #   - cri 接近 1 → 低穩定
        #   - Fallback: ni 分位數
        #
        # V (Value): 優先使用 clv（預測未來 10 年價值）
        #   - 保留 analysis_dna() 計算的真實 clv
        #   - Fallback: total_spent → m_value * ni
        # =====================================================================

        df_rsv <- df %>%
          # R (Risk): 優先使用 nrec_prob，否則使用 customer_dynamics，再 fallback 到 r_value
          mutate(
            # 保留原始 nrec_prob 用於顯示
            risk_prob = if ("nrec_prob" %in% names(.)) {
              nrec_prob
            } else {
              NA_real_
            },
            # 計算 r_level
            r_level = if ("nrec_prob" %in% names(.) && !all(is.na(nrec_prob))) {
              # 使用 MAMBA 的 nrec_prob（邏輯回歸預測流失機率）
              case_when(
                is.na(nrec_prob) ~ "中",  # 無法預測時設為中
                nrec_prob > 0.7 ~ "高",   # 高流失風險
                nrec_prob > 0.3 ~ "中",   # 中流失風險
                TRUE ~ "低"               # 低流失風險（穩定活躍）
              )
            } else if ("customer_dynamics" %in% names(.)) {
              # Fallback 1: 使用 customer_dynamics
              case_when(
                customer_dynamics %in% c("dormant", "half_sleepy") ~ "高",
                customer_dynamics == "sleepy" ~ "中",
                customer_dynamics %in% c("active", "newbie") ~ "低",
                TRUE ~ "中"
              )
            } else {
              # Fallback 2: 使用 r_value 分位數
              r_p80 <- quantile(r_value, 0.8, na.rm = TRUE)
              r_p20 <- quantile(r_value, 0.2, na.rm = TRUE)
              case_when(
                r_value >= r_p80 ~ "高",
                r_value >= r_p20 ~ "中",
                TRUE ~ "低"
              )
            },
            tag_032_dormancy_risk = case_when(
              r_level == "高" ~ "高靜止戶",
              r_level == "中" ~ "中靜止戶",
              TRUE ~ "低靜止戶"
            )
          ) %>%
          # S (Stability): 優先使用 cri（Customer Regularity Index）
          # CRI 邏輯：cri 接近 0 = 高穩定, cri 接近 1 = 低穩定
          # Fallback: ni（交易次數，更多交易 = 更穩定）
          mutate(
            # 選擇穩定度指標：優先 cri, fallback 到 ni
            stability_metric = if ("cri" %in% names(.) && !all(is.na(cri))) {
              # 使用 CRI（注意：CRI 越低越穩定，需反轉以保持一致性）
              cri
            } else {
              # Fallback: 使用 ni
              ni
            },
            stability_metric = ifelse(is.na(stability_metric), 0, stability_metric),
            # 標記使用的指標來源
            stability_source = if ("cri" %in% names(.) && !all(is.na(cri))) "cri" else "ni"
          ) %>%
          # Calculate S level with edge case handling
          {
            df <- .
            use_cri <- df$stability_source[1] == "cri"

            if (use_cri) {
              # CRI-based: 使用 cri_ecdf 分位數或直接用 cri 閾值
              # CRI 接近 0 = 高穩定, CRI 接近 1 = 低穩定
              if ("cri_ecdf" %in% names(df) && !all(is.na(df$cri_ecdf))) {
                # 使用 cri_ecdf（已是 0-1 的 ECDF 值）
                df <- df %>%
                  mutate(
                    s_level = case_when(
                      is.na(cri_ecdf) ~ "中",
                      cri_ecdf < 0.33 ~ "高",  # CRI 較低 = 高穩定
                      cri_ecdf < 0.67 ~ "中",
                      TRUE ~ "低"              # CRI 較高 = 低穩定
                    )
                  )
              } else {
                # 使用 cri 分位數
                cri_p33 <- quantile(df$stability_metric, 0.33, na.rm = TRUE)
                cri_p67 <- quantile(df$stability_metric, 0.67, na.rm = TRUE)
                df <- df %>%
                  mutate(
                    s_level = case_when(
                      is.na(stability_metric) ~ "中",
                      stability_metric < cri_p33 ~ "高",  # CRI 較低 = 高穩定
                      stability_metric < cri_p67 ~ "中",
                      TRUE ~ "低"                         # CRI 較高 = 低穩定
                    )
                  )
              }
            } else {
              # ni-based fallback: higher ni = higher stability
              s_p20 <- quantile(df$stability_metric, 0.2, na.rm = TRUE)
              s_p80 <- quantile(df$stability_metric, 0.8, na.rm = TRUE)
              s_min <- min(df$stability_metric, na.rm = TRUE)
              s_max <- max(df$stability_metric, na.rm = TRUE)

              if (s_p20 == s_min) {
                df <- df %>%
                  arrange(stability_metric) %>%
                  mutate(
                    rank_val = row_number(),
                    s_level = case_when(
                      rank_val <= ceiling(n() * 0.2) ~ "低",
                      stability_metric >= s_p80 ~ "高",
                      TRUE ~ "中"
                    )
                  ) %>%
                  select(-rank_val)
              } else if (s_p80 == s_max) {
                df <- df %>%
                  arrange(desc(stability_metric)) %>%
                  mutate(
                    rank_val = row_number(),
                    s_level = case_when(
                      rank_val <= ceiling(n() * 0.2) ~ "高",
                      stability_metric < s_p20 ~ "低",
                      TRUE ~ "中"
                    )
                  ) %>%
                  select(-rank_val)
              } else if (abs(s_max - s_min) < 0.01 || s_p20 == s_p80) {
                df <- df %>%
                  arrange(desc(stability_metric)) %>%
                  mutate(
                    rank_val = row_number(),
                    s_level = case_when(
                      rank_val <= ceiling(n() * 0.2) ~ "高",
                      rank_val <= ceiling(n() * 0.8) ~ "中",
                      TRUE ~ "低"
                    )
                  ) %>%
                  select(-rank_val)
              } else {
                df <- df %>%
                  mutate(
                    s_level = case_when(
                      stability_metric >= s_p80 ~ "高",
                      stability_metric >= s_p20 ~ "中",
                      TRUE ~ "低"
                    )
                  )
              }
            }
            df
          } %>%
          mutate(
            tag_033_transaction_stability = case_when(
              s_level == "高" ~ "高穩定",
              s_level == "中" ~ "中穩定",
              TRUE ~ "低穩定"
            ),
            # Store the actual metric for display
            stability_cv = stability_metric
          ) %>%
          # V (Value): 優先使用 clv（來自 analysis_dna() 的真實 CLV 預測）
          # Fallback: total_spent → m_value * ni
          mutate(
            # ✅ RSV v3.0: 優先使用 MAMBA 的真實 CLV
            # clv = analysis_dna() 計算的預測未來 10 年顧客終身價值
            # 只有在 clv 不存在時才使用 fallback
            clv_value = if ("clv" %in% names(.) && !all(is.na(clv))) {
              # 使用 MAMBA 的真實 CLV（預測未來 10 年價值）
              clv
            } else if ("total_spent" %in% names(.)) {
              # Fallback 1: 使用歷史總消費金額
              total_spent
            } else if ("m_value" %in% names(.) && "ni" %in% names(.)) {
              # Fallback 2: AOV * 交易次數 = 總消費
              m_value * ni
            } else {
              # Last resort fallback
              m_value
            },
            # 標記使用的指標來源
            value_source = if ("clv" %in% names(.) && !all(is.na(clv))) "clv" else "total_spent"
          ) %>%
          # Calculate V level with edge case handling
          {
            df <- .
            v_p20 <- quantile(df$clv_value, 0.2, na.rm = TRUE)
            v_p80 <- quantile(df$clv_value, 0.8, na.rm = TRUE)
            v_min <- min(df$clv_value, na.rm = TRUE)
            v_max <- max(df$clv_value, na.rm = TRUE)

            # Edge case handling for V level
            if (v_p20 == v_min) {
              # Force bottom 20% as "低價值"
              df <- df %>%
                arrange(clv_value) %>%
                mutate(
                  rank_val = row_number(),
                  v_level = case_when(
                    rank_val <= ceiling(n() * 0.2) ~ "低",
                    clv_value >= v_p80 ~ "高",
                    TRUE ~ "中"
                  )
                ) %>%
                select(-rank_val)
            } else if (v_p80 == v_max) {
              # Force top 20% as "高價值"
              df <- df %>%
                arrange(desc(clv_value)) %>%
                mutate(
                  rank_val = row_number(),
                  v_level = case_when(
                    rank_val <= ceiling(n() * 0.2) ~ "高",
                    clv_value < v_p20 ~ "低",
                    TRUE ~ "中"
                  )
                ) %>%
                select(-rank_val)
            } else if (abs(v_max - v_min) < 0.01 || v_p20 == v_p80) {
              # All values similar: force equal split
              df <- df %>%
                arrange(clv_value) %>%
                mutate(
                  rank_val = row_number(),
                  v_level = case_when(
                    rank_val <= ceiling(n() * 0.2) ~ "低",
                    rank_val <= ceiling(n() * 0.8) ~ "中",
                    TRUE ~ "高"
                  )
                ) %>%
                select(-rank_val)
            } else {
              # Normal case
              df <- df %>%
                mutate(
                  v_level = case_when(
                    clv_value >= v_p80 ~ "高",
                    clv_value >= v_p20 ~ "中",
                    TRUE ~ "低"
                  )
                )
            }
            df
          } %>%
          mutate(
            tag_034_customer_lifetime_value = case_when(
              v_level == "高" ~ "高價值",
              v_level == "中" ~ "中價值",
              TRUE ~ "低價值"
            )
          ) %>%
          # Combine R/S/V for customer type
          mutate(
            rsv_key = paste0(r_level, s_level, v_level),
            customer_type = map_chr(rsv_key, get_customer_type),
            strategy = map_chr(rsv_key, get_strategy_text),
            action = map_chr(rsv_key, get_action_text)
          )

        values$processed_data <- df_rsv

        # Generate strategy summary
        strategy_summary <- df_rsv %>%
          group_by(customer_type, r_level, s_level, v_level, strategy, action) %>%
          summarise(
            customer_count = n(),
            percentage = round(n() / nrow(df_rsv) * 100, 1),
            avg_clv = round(mean(clv, na.rm = TRUE), 0),
            .groups = "drop"
          ) %>%
          arrange(desc(customer_count))

        values$strategy_summary <- strategy_summary

      }, error = function(e) {
        showNotification(
          paste("R/S/V 矩陣計算錯誤:", e$message),
          type = "error",
          duration = 10
        )
      })
    })

    # ===========================================================================
    # Status Output
    # ===========================================================================

    output$status <- renderText({
      if (is.null(values$processed_data)) {
        return("⏳ 等待資料...")
      }

      df <- values$processed_data
      n_total <- nrow(df)
      n_types <- length(unique(df$customer_type))

      # 偵測使用的 RSV 變數來源
      r_source <- if ("risk_prob" %in% names(df) && !all(is.na(df$risk_prob))) {
        "nrec_prob (邏輯回歸預測)"
      } else if ("customer_dynamics" %in% names(df)) {
        "customer_dynamics (DNA 分析)"
      } else {
        "r_value (分位數)"
      }

      s_source <- if ("stability_source" %in% names(df)) {
        if (df$stability_source[1] == "cri") "cri (經驗貝氏 CRI)" else "ni (交易次數)"
      } else {
        "ni (交易次數)"
      }

      v_source <- if ("value_source" %in% names(df)) {
        if (df$value_source[1] == "clv") "clv (預測 10 年價值)" else "total_spent (歷史總消費)"
      } else {
        "total_spent (歷史總消費)"
      }

      paste0(
        "✅ RSV 生命力矩陣 v3.0 計算完成\n",
        "總客戶數：", n_total, " 人\n",
        "客戶類型數：", n_types, " 種\n",
        "━━━━━━━━━━━━━━━━━━━━━━━━\n",
        "📊 RSV 變數來源：\n",
        "  R (風險): ", r_source, "\n",
        "  S (穩定): ", s_source, "\n",
        "  V (價值): ", v_source
      )
    })

    # ===========================================================================
    # Key Metrics
    # ===========================================================================

    output$high_risk_count <- renderText({
      req(values$processed_data)
      df <- values$processed_data
      high_risk <- sum(df$r_level == "高", na.rm = TRUE)
      paste0(high_risk, " 人")
    })

    output$high_risk_pct <- renderText({
      req(values$processed_data)
      df <- values$processed_data
      pct <- round(sum(df$r_level == "高", na.rm = TRUE) / nrow(df) * 100, 1)
      paste0(pct, "% 客戶屬於高風險客戶")
    })

    output$high_stability_count <- renderText({
      req(values$processed_data)
      df <- values$processed_data
      high_stable <- sum(df$s_level == "高", na.rm = TRUE)
      paste0(high_stable, " 人")
    })

    output$high_stability_pct <- renderText({
      req(values$processed_data)
      df <- values$processed_data
      pct <- round(sum(df$s_level == "高", na.rm = TRUE) / nrow(df) * 100, 1)
      paste0(pct, "% 客戶交易穩定")
    })

    output$high_value_count <- renderText({
      req(values$processed_data)
      df <- values$processed_data
      high_value <- sum(df$v_level == "高", na.rm = TRUE)
      paste0(high_value, " 人")
    })

    output$high_value_pct <- renderText({
      req(values$processed_data)
      df <- values$processed_data
      pct <- round(sum(df$v_level == "高", na.rm = TRUE) / nrow(df) * 100, 1)
      paste0(pct, "% 客戶為高終生價值")
    })

    # ===========================================================================
    # Distribution Charts
    # ===========================================================================

    output$risk_distribution <- renderPlotly({
      req(values$processed_data)
      df <- values$processed_data

      dist_data <- df %>%
        group_by(tag_032_dormancy_risk) %>%
        summarise(count = n(), .groups = "drop") %>%
        mutate(
          percentage = round(count / sum(count) * 100, 1),
          label = paste0(tag_032_dormancy_risk, "\n", count, " 人 (", percentage, "%)")
        )

      plot_ly(dist_data,
              labels = ~tag_032_dormancy_risk,
              values = ~count,
              type = 'pie',
              marker = list(colors = c("高靜止戶" = "#ef4444", "中靜止戶" = "#f59e0b", "低靜止戶" = "#10b981")),
              textinfo = 'label+percent',
              hoverinfo = 'text',
              text = ~label) %>%
        layout(showlegend = TRUE,
               legend = list(orientation = "h", y = -0.1))
    })

    output$stability_distribution <- renderPlotly({
      req(values$processed_data)
      df <- values$processed_data

      dist_data <- df %>%
        group_by(tag_033_transaction_stability) %>%
        summarise(count = n(), .groups = "drop") %>%
        mutate(
          percentage = round(count / sum(count) * 100, 1),
          label = paste0(tag_033_transaction_stability, "\n", count, " 人 (", percentage, "%)")
        )

      plot_ly(dist_data,
              labels = ~tag_033_transaction_stability,
              values = ~count,
              type = 'pie',
              marker = list(colors = c("高穩定" = "#10b981", "中穩定" = "#f59e0b", "低穩定" = "#ef4444")),
              textinfo = 'label+percent',
              hoverinfo = 'text',
              text = ~label) %>%
        layout(showlegend = TRUE,
               legend = list(orientation = "h", y = -0.1))
    })

    output$value_distribution <- renderPlotly({
      req(values$processed_data)
      df <- values$processed_data

      dist_data <- df %>%
        group_by(tag_034_customer_lifetime_value) %>%
        summarise(count = n(), .groups = "drop") %>%
        mutate(
          percentage = round(count / sum(count) * 100, 1),
          label = paste0(tag_034_customer_lifetime_value, "\n", count, " 人 (", percentage, "%)")
        )

      plot_ly(dist_data,
              labels = ~tag_034_customer_lifetime_value,
              values = ~count,
              type = 'pie',
              marker = list(colors = c("高價值" = "#8b5cf6", "中價值" = "#3b82f6", "低價值" = "#6b7280")),
              textinfo = 'label+percent',
              hoverinfo = 'text',
              text = ~label) %>%
        layout(showlegend = TRUE,
               legend = list(orientation = "h", y = -0.1))
    })

    # ===========================================================================
    # Issue #11: 移除 3D 散點圖（與下方表格資訊重疊）
    # 3D Scatter Plot (R × S × V)
    # ===========================================================================

    # output$rsv_3d_scatter <- renderPlotly({
    #   req(values$processed_data)
    #   df <- values$processed_data
    #
    #   # Map levels to numeric values for 3D plot
    #   df_plot <- df %>%
    #     mutate(
    #       r_numeric = case_when(r_level == "低" ~ 1, r_level == "中" ~ 2, r_level == "高" ~ 3),
    #       s_numeric = case_when(s_level == "低" ~ 1, s_level == "中" ~ 2, s_level == "高" ~ 3),
    #       v_numeric = case_when(v_level == "低" ~ 1, v_level == "中" ~ 2, v_level == "高" ~ 3),
    #       hover_text = paste0(
    #         "客戶類型：", customer_type, "<br>",
    #         "R (風險)：", tag_032_dormancy_risk, "<br>",
    #         "S (穩定)：", tag_033_transaction_stability, "<br>",
    #         "V (價值)：", tag_034_customer_lifetime_value, "<br>",
    #         "策略：", strategy
    #       )
    #     )
    #
    #   plot_ly(df_plot,
    #           x = ~r_numeric,
    #           y = ~s_numeric,
    #           z = ~v_numeric,
    #           color = ~customer_type,
    #           type = 'scatter3d',
    #           mode = 'markers',
    #           marker = list(size = 5, opacity = 0.7),
    #           hovertext = ~hover_text,
    #           hoverinfo = 'text') %>%
    #     layout(
    #       scene = list(
    #         xaxis = list(title = "R - 靜止風險", tickvals = c(1, 2, 3), ticktext = c("低", "中", "高")),
    #         yaxis = list(title = "S - 交易穩定度", tickvals = c(1, 2, 3), ticktext = c("低", "中", "高")),
    #         zaxis = list(title = "V - 終生價值", tickvals = c(1, 2, 3), ticktext = c("低", "中", "高"))
    #       ),
    #       legend = list(orientation = "v", x = 1.05, y = 1)
    #     )
    # })

    # ===========================================================================
    # Heatmap (R × S with V as color)
    # ===========================================================================

    output$rsv_heatmap <- renderPlotly({
      req(values$processed_data)
      df <- values$processed_data

      # Create heatmap data
      heatmap_data <- df %>%
        group_by(r_level, s_level, v_level) %>%
        summarise(count = n(), .groups = "drop") %>%
        group_by(r_level, s_level) %>%
        summarise(
          total_count = sum(count),
          high_value_pct = round(sum(count[v_level == "高"]) / sum(count) * 100, 1),
          .groups = "drop"
        )

      # Create matrix
      matrix_data <- heatmap_data %>%
        tidyr::pivot_wider(
          names_from = s_level,
          values_from = total_count,
          values_fill = 0
        ) %>%
        arrange(desc(r_level))

      plot_ly(
        x = c("低", "中", "高"),
        y = c("高", "中", "低"),
        z = as.matrix(matrix_data[, -1]),
        type = "heatmap",
        colorscale = "Viridis",
        hovertemplate = "R風險: %{y}<br>S穩定: %{x}<br>客戶數: %{z}<extra></extra>"
      ) %>%
        layout(
          xaxis = list(title = "S - 交易穩定度"),
          yaxis = list(title = "R - 靜止風險"),
          title = "客戶分布熱力圖（顏色深度代表客戶數量）"
        )
    })

    # ===========================================================================
    # Strategy Table
    # 2025-12-26: Comment out - 訂閱版暫不顯示
    # ===========================================================================

    # output$strategy_table <- renderDT({
    #   req(values$strategy_summary)
    #
    #   datatable(
    #     values$strategy_summary %>%
    #       select(
    #         `客戶類型` = customer_type,
    #         `R風險` = r_level,
    #         `S穩定` = s_level,
    #         `V價值` = v_level,
    #         `客戶數` = customer_count,
    #         `占比(%)` = percentage,
    #         `平均CLV` = avg_clv,
    #         `建議策略` = strategy,
    #         `行動方案` = action
    #       ),
    #     options = list(
    #       pageLength = 10,
    #       scrollX = TRUE,
    #       order = list(list(4, 'desc'))  # Sort by customer count
    #     ),
    #     rownames = FALSE,
    #     class = 'cell-border stripe hover'
    #   )
    # })

    # ===========================================================================
    # Customer Detail Table
    # ===========================================================================

    output$customer_table <- renderDT({
      req(values$processed_data)

      # ✅ RSV v2.0：更新顯示欄位
      # - R值：顯示 customer_dynamics（如有）或 r_value
      # - S值：顯示 ni（交易次數）
      # - V值：顯示 total_spent 或 clv
      display_df <- values$processed_data %>%
        mutate(
          # 動態來源欄位
          customer_dynamics_display = if ("customer_dynamics" %in% names(.)) {
            customer_dynamics
          } else {
            NA_character_
          },
          total_spent_display = if ("total_spent" %in% names(.)) {
            total_spent
          } else {
            clv
          }
        ) %>%
        select(
          `客戶ID` = customer_id,
          `R風險` = tag_032_dormancy_risk,
          `S穩定` = tag_033_transaction_stability,
          `V價值` = tag_034_customer_lifetime_value,
          `客戶類型` = customer_type,
          `建議策略` = strategy,
          `最近購買(天)` = r_value,
          `顧客動態` = customer_dynamics_display,
          `交易次數` = ni,
          `總消費金額` = total_spent_display
        ) %>%
        mutate(
          `最近購買(天)` = round(`最近購買(天)`, 1),
          `總消費金額` = round(`總消費金額`, 0)
        )

      datatable(
        display_df,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          order = list(list(9, 'desc'))  # Sort by 總消費金額
        ),
        rownames = FALSE,
        class = 'cell-border stripe hover',
        filter = 'top'
      )
    })

    # ===========================================================================
    # Download Handler
    # ===========================================================================

    output$download_data <- downloadHandler(
      filename = function() {
        paste0("rsv_matrix_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        req(values$processed_data)

        # ✅ RSV v2.0：更新下載欄位
        # 使用正確的 DNA 輸出變數
        export_df <- values$processed_data %>%
          mutate(
            # 確保 total_spent 存在
            total_spent_export = if ("total_spent" %in% names(.)) {
              total_spent
            } else {
              clv
            },
            # 確保 customer_dynamics 存在
            customer_dynamics_export = if ("customer_dynamics" %in% names(.)) {
              customer_dynamics
            } else {
              NA_character_
            }
          ) %>%
          select(
            客戶ID = customer_id,
            靜止風險 = tag_032_dormancy_risk,
            交易穩定度 = tag_033_transaction_stability,
            終生價值 = tag_034_customer_lifetime_value,
            客戶類型 = customer_type,
            策略建議 = strategy,
            行動方案 = action,
            最近購買天數 = r_value,
            顧客動態 = customer_dynamics_export,
            交易次數 = ni,
            總消費金額 = total_spent_export,
            # 使用 any_of 處理可能不存在的欄位
            any_of(c(
              "平均購買間隔" = "ipt"  # DNA 提供的是總時間跨度
            ))
          ) %>%
          mutate(
            across(where(is.numeric), ~round(.x, 2))
          )

        # ✅ 使用 UTF-8 BOM 確保 Excel 正確顯示中文
        con <- file(file, open = "wb", encoding = "UTF-8")
        writeBin(charToRaw('\ufeff'), con)  # UTF-8 BOM
        write.csv(export_df, con, row.names = FALSE, fileEncoding = "UTF-8")
        close(con)
      }
    )

    # ===========================================================================
    # Return processed data for downstream modules
    # ===========================================================================

    return(reactive({ values$processed_data }))
  })
}

# ==============================================================================
# Helper Functions: Customer Type & Strategy Mapping
# ==============================================================================

get_customer_type <- function(rsv_key) {
  # ✅ Issue #18: NEW 9-Type Customer Classification System
  # Map R×S×V combinations to 9 core customer types with emoji
  # Based on revision requirements 20251108

  type_map <- list(
    # 核心 9 種分類 (Primary 9 Types)
    "低高高" = "💎 金鑽客",                    # 活躍 × 穩定 × 高價值
    "低高中" = "🌱 成長型忠誠客",              # 穩定但中價值
    "中中高" = "⚠️ 預警高值客",               # 有價值但活躍度下降
    "高低高" = "💔 流失高值客",               # 高價值但流失風險
    "高低低" = "💤 沉睡客",                   # 不穩定且價值低
    "中中中" = "📊 潛力客群",                 # 中間族群
    "低低低" = "👁️ 邊緣客/觀望客",            # 活動少、價值低
    "中高高" = "💡 沉靜貴客",                 # 穩定高值但互動下降
    "高高中" = "⚠️ 風險主力客",               # 穩定中值但流失中

    # 其他 18 種組合 (Other 18 combinations) - 映射到最接近的核心類型
    "低高低" = "🌱 成長型忠誠客",              # 穩定低值 → 成長型
    "低中高" = "💎 金鑽客",                    # 活躍高值 → 金鑽客
    "低中中" = "📊 潛力客群",                 # 活躍中值中穩 → 潛力
    "低中低" = "📊 潛力客群",                 # 活躍低值中穩 → 潛力
    "低低高" = "⚠️ 預警高值客",               # 活躍高值低穩 → 預警
    "低低中" = "👁️ 邊緣客/觀望客",            # 活躍中值低穩 → 邊緣

    "中高中" = "🌱 成長型忠誠客",              # 中活躍高穩中值 → 成長型
    "中高低" = "📊 潛力客群",                 # 中活躍高穩低值 → 潛力
    "中中低" = "📊 潛力客群",                 # 中間低值 → 潛力
    "中低高" = "⚠️ 預警高值客",               # 中活躍低穩高值 → 預警
    "中低中" = "📊 潛力客群",                 # 中活躍低穩中值 → 潛力
    "中低低" = "💤 沉睡客",                   # 中活躍低穩低值 → 沉睡

    "高高高" = "💔 流失高值客",               # 高活躍（風險）高穩高值 → 流失高值
    "高高低" = "⚠️ 風險主力客",               # 高風險高穩低值 → 風險主力
    "高中高" = "💔 流失高值客",               # 高風險中穩高值 → 流失高值
    "高中中" = "⚠️ 風險主力客",               # 高風險中穩中值 → 風險主力
    "高中低" = "💤 沉睡客",                   # 高風險中穩低值 → 沉睡
    "高低中" = "💤 沉睡客"                    # 高風險低穩中值 → 沉睡
  )

  type_map[[rsv_key]] %||% "📊 潛力客群"  # Default to 潛力客群
}

get_strategy_text <- function(rsv_key) {
  # ✅ Issue #18: NEW Strategy Mapping for 9-Type System
  # Based on revision requirements 20251108

  strategy_map <- list(
    # 核心 9 種分類策略
    "低高高" = "VIP 體驗 + 品牌共創",                    # 💎 金鑽客
    "低高中" = "升級誘因",                              # 🌱 成長型忠誠客
    "中中高" = "早期挽回",                              # ⚠️ 預警高值客
    "高低高" = "挽回行銷",                              # 💔 流失高值客
    "高低低" = "低成本回流/冷啟策略",                   # 💤 沉睡客
    "中中中" = "分群培育",                              # 📊 潛力客群
    "低低低" = "輕促銷策略",                            # 👁️ 邊緣客/觀望客
    "中高高" = "情感維繫",                              # 💡 沉靜貴客
    "高高中" = "挽留提醒",                              # ⚠️ 風險主力客

    # 其他 18 種組合 - 映射到對應核心類型策略
    "低高低" = "升級誘因",                              # → 成長型
    "低中高" = "VIP 體驗 + 品牌共創",                    # → 金鑽客
    "低中中" = "分群培育",                              # → 潛力客群
    "低中低" = "分群培育",                              # → 潛力客群
    "低低高" = "早期挽回",                              # → 預警高值客
    "低低中" = "輕促銷策略",                            # → 邊緣客

    "中高中" = "升級誘因",                              # → 成長型
    "中高低" = "分群培育",                              # → 潛力客群
    "中中低" = "分群培育",                              # → 潛力客群
    "中低高" = "早期挽回",                              # → 預警高值客
    "中低中" = "分群培育",                              # → 潛力客群
    "中低低" = "低成本回流/冷啟策略",                   # → 沉睡客

    "高高高" = "挽回行銷",                              # → 流失高值客
    "高高低" = "挽留提醒",                              # → 風險主力客
    "高中高" = "挽回行銷",                              # → 流失高值客
    "高中中" = "挽留提醒",                              # → 風險主力客
    "高中低" = "低成本回流/冷啟策略",                   # → 沉睡客
    "高低中" = "低成本回流/冷啟策略"                    # → 沉睡客
  )

  strategy_map[[rsv_key]] %||% "分群培育"  # Default
}

get_action_text <- function(rsv_key) {
  # ✅ Issue #18: NEW Action Plans for 9-Type System
  # Based on revision requirements 20251108

  action_map <- list(
    # 核心 9 種分類的具體行動方案
    "低高高" = "專屬客服 / 新品搶先體驗 / 會員大使",                    # 💎 金鑽客
    "低高中" = "搭售組合、滿額升級、會員積分任務",                      # 🌱 成長型忠誠客
    "中中高" = "回購提醒、VIP 喚醒禮、定向廣告再觸及",                  # ⚠️ 預警高值客
    "高低高" = "再行銷廣告、專屬優惠、客服致電喚回",                    # 💔 流失高值客
    "高低低" = "廣告再曝光 + 再註冊誘因",                              # 💤 沉睡客
    "中中中" = "推薦新品、品牌故事內容推播、任務制獎勵",                # 📊 潛力客群
    "低低低" = "試用 / 入門優惠券 / 交叉品牌聯合活動",                  # 👁️ 邊緣客/觀望客
    "中高高" = "品牌關懷訊息、生日禮、活動邀請",                        # 💡 沉靜貴客
    "高高中" = "優惠截止倒數、客服問候 / 滿意度問卷",                   # ⚠️ 風險主力客

    # 其他 18 種組合 - 映射到對應核心類型行動
    "低高低" = "搭售組合、滿額升級、會員積分任務",                      # → 成長型
    "低中高" = "專屬客服 / 新品搶先體驗 / 會員大使",                    # → 金鑽客
    "低中中" = "推薦新品、品牌故事內容推播、任務制獎勵",                # → 潛力客群
    "低中低" = "推薦新品、品牌故事內容推播、任務制獎勵",                # → 潛力客群
    "低低高" = "回購提醒、VIP 喚醒禮、定向廣告再觸及",                  # → 預警高值客
    "低低中" = "試用 / 入門優惠券 / 交叉品牌聯合活動",                  # → 邊緣客

    "中高中" = "搭售組合、滿額升級、會員積分任務",                      # → 成長型
    "中高低" = "推薦新品、品牌故事內容推播、任務制獎勵",                # → 潛力客群
    "中中低" = "推薦新品、品牌故事內容推播、任務制獎勵",                # → 潛力客群
    "中低高" = "回購提醒、VIP 喚醒禮、定向廣告再觸及",                  # → 預警高值客
    "中低中" = "推薦新品、品牌故事內容推播、任務制獎勵",                # → 潛力客群
    "中低低" = "廣告再曝光 + 再註冊誘因",                              # → 沉睡客

    "高高高" = "再行銷廣告、專屬優惠、客服致電喚回",                    # → 流失高值客
    "高高低" = "優惠截止倒數、客服問候 / 滿意度問卷",                   # → 風險主力客
    "高中高" = "再行銷廣告、專屬優惠、客服致電喚回",                    # → 流失高值客
    "高中中" = "優惠截止倒數、客服問候 / 滿意度問卷",                   # → 風險主力客
    "高中低" = "廣告再曝光 + 再註冊誘因",                              # → 沉睡客
    "高低中" = "廣告再曝光 + 再註冊誘因"                               # → 沉睡客
  )

  action_map[[rsv_key]] %||% "推薦新品、品牌故事內容推播、任務制獎勵"  # Default
}
