################################################################################
# Module: Customer Base Value Analysis
# Description: 顧客基礎價值分析模組
# Features:
#   1. 購買週期分群 (Purchase Cycle Segmentation)
#   2. 過去價值分群 (Past Value Segmentation)
#   3. 客單價分析 (Average Order Value Analysis)
# Version: 1.0
# Date: 2025-10-25
################################################################################

library(shiny)
library(bs4Dash)
library(dplyr)
library(plotly)
library(DT)

# ============================================================================
# UI Function
# ============================================================================

customerBaseValueUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("顧客基礎價值分析", style = "text-align: center; margin: 20px 0;"),
    
    # 說明文字
    wellPanel(
      style = "background-color: #e7f3ff; border-left: 4px solid #007bff;",
      h4("📊 模組說明"),
      p("本模組分析顧客的基礎價值指標，包含："),
      tags$ul(
        tags$li(tags$strong("購買週期分群"), " - 根據平均購買間隔（IPT）將顧客分為高/中/低購買週期"),
        tags$li(tags$strong("過去價值分群"), " - 根據歷史總消費金額（M值）將顧客分為高/中/低價值"),
        tags$li(tags$strong("客單價分析"), " - 比較新客與主力客的平均訂單金額")
      ),
      p(style = "margin-bottom: 0; color: #6c757d; font-size: 0.9em;",
        "💡 所有分群均採用 80/20 法則（P20/P80 百分位數）")
    ),
    
    # 檢查資料狀態
    conditionalPanel(
      condition = paste0("output['", ns("has_data"), "'] == false"),
      wellPanel(
        style = "background-color: #fff3cd; border-left: 4px solid #ffc107;",
        h4("⚠️ 需要先完成 DNA 分析"),
        p("請先在「DNA 分析」模組上傳資料並完成分析，才能使用此模組。")
      )
    ),
    
    # 主要內容區（有資料時顯示）
    conditionalPanel(
      condition = paste0("output['", ns("has_data"), "'] == true"),
      
      # 1. 購買週期分群
      fluidRow(
        column(12,
          bs4Card(
            title = "購買週期分群 (Purchase Cycle)",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            
            fluidRow(
              column(6,
                h5("📈 分群統計"),
                DTOutput(ns("purchase_cycle_table"))
              ),
              column(6,
                h5("📊 視覺化"),
                plotlyOutput(ns("purchase_cycle_plot"), height = "300px")
              )
            ),
            
            hr(),
            
            div(
              style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px;",
              h6(tags$strong("💡 指標說明")),
              p(style = "margin-bottom: 0; font-size: 0.9em;",
                tags$strong("購買週期 (IPT)"), ": Inter-Purchase Time，平均兩次購買間隔天數。",
                tags$br(),
                "• ", tags$strong("高購買週期"), ": IPT ≥ P80（購買間隔長，不常買）",
                tags$br(),
                "• ", tags$strong("中購買週期"), ": P20 ≤ IPT < P80（正常購買頻率）",
                tags$br(),
                "• ", tags$strong("低購買週期"), ": IPT < P20（購買間隔短，常買）"
              )
            )
          )
        )
      ),
      
      # 2. 過去價值分群
      fluidRow(
        column(12,
          bs4Card(
            title = "過去價值分群 (Past Value)",
            width = 12,
            status = "success",
            solidHeader = TRUE,
            collapsible = TRUE,
            
            fluidRow(
              column(6,
                h5("📈 分群統計"),
                DTOutput(ns("past_value_table"))
              ),
              column(6,
                h5("📊 視覺化"),
                plotlyOutput(ns("past_value_plot"), height = "300px")
              )
            ),
            
            hr(),
            
            div(
              style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px;",
              h6(tags$strong("💡 指標說明")),
              p(style = "margin-bottom: 0; font-size: 0.9em;",
                tags$strong("過去價值 (M值)"), ": 歷史總消費金額，代表顧客的累積貢獻。",
                tags$br(),
                "• ", tags$strong("高價值顧客"), ": M ≥ P80（前20%高消費客戶）",
                tags$br(),
                "• ", tags$strong("中價值顧客"), ": P20 ≤ M < P80（中間60%客戶）",
                tags$br(),
                "• ", tags$strong("低價值顧客"), ": M < P20（後20%低消費客戶）"
              )
            )
          )
        )
      ),
      
      # 3. 客單價分析
      fluidRow(
        column(12,
          bs4Card(
            title = "客單價分析 (Average Order Value)",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            
            fluidRow(
              column(6,
                h5("📈 客單價比較"),
                DTOutput(ns("aov_table"))
              ),
              column(6,
                h5("📊 視覺化"),
                plotlyOutput(ns("aov_plot"), height = "300px")
              )
            ),
            
            hr(),
            
            div(
              style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px;",
              h6(tags$strong("💡 指標說明")),
              p(style = "margin-bottom: 0; font-size: 0.9em;",
                tags$strong("客單價 (AOV)"), ": Average Order Value，平均訂單金額 = 總消費金額 ÷ 交易次數。",
                tags$br(),
                "• ", tags$strong("新客客單價"), ": 新客群體的平均訂單金額",
                tags$br(),
                "• ", tags$strong("主力客客單價"), ": 主力客群體的平均訂單金額",
                tags$br(),
                "• 比較兩者差異可了解客戶成長潛力"
              )
            )
          )
        )
      )
    )
  )
}

# ============================================================================
# Server Function
# ============================================================================

customerBaseValueServer <- function(id, dna_results) {
  moduleServer(id, function(input, output, session) {
    
    # 檢查是否有資料
    output$has_data <- reactive({
      !is.null(dna_results())
    })
    outputOptions(output, "has_data", suspendWhenHidden = FALSE)
    
    # ========================================================================
    # 1. 購買週期分群
    # ========================================================================
    
    purchase_cycle_data <- reactive({
      req(dna_results())
      df <- dna_results()
      
      # 使用 ipt_mean (平均購買間隔)
      # 只分析有 IPT 資料的客戶（ni >= 2）
      df_with_ipt <- df %>%
        filter(!is.na(ipt_mean), ni >= 2)
      
      if (nrow(df_with_ipt) == 0) {
        return(NULL)
      }
      
      # 計算百分位數
      p80 <- quantile(df_with_ipt$ipt_mean, 0.8, na.rm = TRUE)
      p20 <- quantile(df_with_ipt$ipt_mean, 0.2, na.rm = TRUE)
      ipt_min <- min(df_with_ipt$ipt_mean, na.rm = TRUE)
      ipt_max <- max(df_with_ipt$ipt_mean, na.rm = TRUE)

      # 分群（處理邊界情況以確保三群都存在）
      if (p20 == ipt_min) {
        # P20 = 最小值：會缺少「低」群，需要特殊處理
        # 強制最底部 20% 為「低」
        df_with_ipt <- df_with_ipt %>%
          arrange(ipt_mean) %>%
          mutate(
            rank_val = row_number(),
            purchase_cycle_level = case_when(
              rank_val <= ceiling(n() * 0.2) ~ "低購買週期",
              ipt_mean >= p80 ~ "高購買週期",
              TRUE ~ "中購買週期"
            )
          ) %>%
          select(-rank_val)
      } else if (p80 == ipt_max) {
        # P80 = 最大值：會缺少「高」群，需要特殊處理
        # 強制最頂部 20% 為「高」
        df_with_ipt <- df_with_ipt %>%
          arrange(desc(ipt_mean)) %>%
          mutate(
            rank_val = row_number(),
            purchase_cycle_level = case_when(
              rank_val <= ceiling(n() * 0.2) ~ "高購買週期",
              ipt_mean < p20 ~ "低購買週期",
              TRUE ~ "中購買週期"
            )
          ) %>%
          select(-rank_val)
      } else if (abs(ipt_max - ipt_min) < 0.01 || p20 == p80) {
        # 所有值相同或太接近，強制分成三等份
        df_with_ipt <- df_with_ipt %>%
          arrange(ipt_mean) %>%
          mutate(
            rank_val = row_number(),
            purchase_cycle_level = case_when(
              rank_val <= ceiling(n() * 0.2) ~ "低購買週期",
              rank_val <= ceiling(n() * 0.8) ~ "中購買週期",
              TRUE ~ "高購買週期"
            )
          ) %>%
          select(-rank_val)
      } else {
        # 正常情況：使用 P20/P80 分位數
        df_with_ipt <- df_with_ipt %>%
          mutate(
            purchase_cycle_level = case_when(
              ipt_mean >= p80 ~ "高購買週期",
              ipt_mean >= p20 ~ "中購買週期",
              TRUE ~ "低購買週期"
            )
          )
      }
      
      # 統計
      summary_data <- df_with_ipt %>%
        group_by(purchase_cycle_level) %>%
        summarise(
          customer_count = n(),
          percentage = round(n() / nrow(df_with_ipt) * 100, 1),
          avg_ipt = round(mean(ipt_mean, na.rm = TRUE), 1),
          .groups = "drop"
        ) %>%
        arrange(desc(purchase_cycle_level))
      
      return(list(
        data = df_with_ipt,
        summary = summary_data,
        p80 = p80,
        p20 = p20
      ))
    })
    
    # 購買週期表格
    output$purchase_cycle_table <- renderDT({
      req(purchase_cycle_data())
      
      summary_data <- purchase_cycle_data()$summary
      
      # 格式化表格
      summary_data <- summary_data %>%
        mutate(
          購買週期等級 = purchase_cycle_level,
          客戶數 = paste0(customer_count, " 人"),
          佔比 = paste0(percentage, "%"),
          平均購買週期 = paste0(avg_ipt, " 天")
        ) %>%
        select(購買週期等級, 客戶數, 佔比, 平均購買週期)
      
      datatable(
        summary_data,
        options = list(
          dom = 't',
          ordering = FALSE,
          pageLength = 3
        ),
        rownames = FALSE,
        class = "display nowrap"
      )
    })
    
    # 購買週期圖表
    output$purchase_cycle_plot <- renderPlotly({
      req(purchase_cycle_data())
      
      summary_data <- purchase_cycle_data()$summary
      
      # 圓餅圖
      plot_ly(
        data = summary_data,
        labels = ~purchase_cycle_level,
        values = ~customer_count,
        type = 'pie',
        textinfo = 'label+percent',
        textposition = 'inside',
        marker = list(
          colors = c("高購買週期" = "#dc3545", "中購買週期" = "#ffc107", "低購買週期" = "#28a745"),
          line = list(color = '#FFFFFF', width = 2)
        ),
        hovertemplate = paste0(
          '<b>%{label}</b><br>',
          '客戶數: %{value}<br>',
          '佔比: %{percent}<br>',
          '<extra></extra>'
        )
      ) %>%
        layout(
          showlegend = TRUE,
          legend = list(orientation = "v", x = 1, y = 0.5)
        )
    })
    
    # ========================================================================
    # 2. 過去價值分群
    # ========================================================================
    
    past_value_data <- reactive({
      req(dna_results())
      df <- dna_results()
      
      # 使用 m_value (總消費金額)
      df_with_m <- df %>%
        filter(!is.na(m_value), m_value > 0)
      
      if (nrow(df_with_m) == 0) {
        return(NULL)
      }
      
      # 計算百分位數
      p80 <- quantile(df_with_m$m_value, 0.8, na.rm = TRUE)
      p20 <- quantile(df_with_m$m_value, 0.2, na.rm = TRUE)
      m_min <- min(df_with_m$m_value, na.rm = TRUE)
      m_max <- max(df_with_m$m_value, na.rm = TRUE)

      # 分群（處理邊界情況以確保三群都存在）
      if (p20 == m_min) {
        # P20 = 最小值：會缺少「低」群，需要特殊處理
        # 強制最底部 20% 為「低」
        df_with_m <- df_with_m %>%
          arrange(m_value) %>%
          mutate(
            rank_val = row_number(),
            past_value_level = case_when(
              rank_val <= ceiling(n() * 0.2) ~ "低價值顧客",
              m_value >= p80 ~ "高價值顧客",
              TRUE ~ "中價值顧客"
            )
          ) %>%
          select(-rank_val)
      } else if (p80 == m_max) {
        # P80 = 最大值：會缺少「高」群，需要特殊處理
        # 強制最頂部 20% 為「高」
        df_with_m <- df_with_m %>%
          arrange(desc(m_value)) %>%
          mutate(
            rank_val = row_number(),
            past_value_level = case_when(
              rank_val <= ceiling(n() * 0.2) ~ "高價值顧客",
              m_value < p20 ~ "低價值顧客",
              TRUE ~ "中價值顧客"
            )
          ) %>%
          select(-rank_val)
      } else if (abs(m_max - m_min) < 0.01 || p20 == p80) {
        # 所有值相同或太接近，強制分成三等份
        df_with_m <- df_with_m %>%
          arrange(m_value) %>%
          mutate(
            rank_val = row_number(),
            past_value_level = case_when(
              rank_val <= ceiling(n() * 0.2) ~ "低價值顧客",
              rank_val <= ceiling(n() * 0.8) ~ "中價值顧客",
              TRUE ~ "高價值顧客"
            )
          ) %>%
          select(-rank_val)
      } else {
        # 正常情況：使用 P20/P80 分位數
        df_with_m <- df_with_m %>%
          mutate(
            past_value_level = case_when(
              m_value >= p80 ~ "高價值顧客",
              m_value >= p20 ~ "中價值顧客",
              TRUE ~ "低價值顧客"
            )
          )
      }
      
      # 統計
      summary_data <- df_with_m %>%
        group_by(past_value_level) %>%
        summarise(
          customer_count = n(),
          percentage = round(n() / nrow(df_with_m) * 100, 1),
          avg_value = round(mean(m_value, na.rm = TRUE), 0),
          total_value = round(sum(m_value, na.rm = TRUE), 0),
          .groups = "drop"
        ) %>%
        arrange(desc(past_value_level))
      
      return(list(
        data = df_with_m,
        summary = summary_data,
        p80 = p80,
        p20 = p20
      ))
    })
    
    # 過去價值表格
    output$past_value_table <- renderDT({
      req(past_value_data())
      
      summary_data <- past_value_data()$summary
      
      # 格式化表格
      summary_data <- summary_data %>%
        mutate(
          價值等級 = past_value_level,
          客戶數 = paste0(customer_count, " 人"),
          佔比 = paste0(percentage, "%"),
          平均消費 = paste0("$", format(avg_value, big.mark = ",")),
          總貢獻 = paste0("$", format(total_value, big.mark = ","))
        ) %>%
        select(價值等級, 客戶數, 佔比, 平均消費, 總貢獻)
      
      datatable(
        summary_data,
        options = list(
          dom = 't',
          ordering = FALSE,
          pageLength = 3
        ),
        rownames = FALSE,
        class = "display nowrap"
      )
    })
    
    # 過去價值圖表
    output$past_value_plot <- renderPlotly({
      req(past_value_data())
      
      summary_data <- past_value_data()$summary
      
      # 圓餅圖
      plot_ly(
        data = summary_data,
        labels = ~past_value_level,
        values = ~customer_count,
        type = 'pie',
        textinfo = 'label+percent',
        textposition = 'inside',
        marker = list(
          colors = c("高價值顧客" = "#28a745", "中價值顧客" = "#ffc107", "低價值顧客" = "#dc3545"),
          line = list(color = '#FFFFFF', width = 2)
        ),
        hovertemplate = paste0(
          '<b>%{label}</b><br>',
          '客戶數: %{value}<br>',
          '佔比: %{percent}<br>',
          '<extra></extra>'
        )
      ) %>%
        layout(
          showlegend = TRUE,
          legend = list(orientation = "v", x = 1, y = 0.5)
        )
    })
    
    # ========================================================================
    # 3. 客單價分析
    # ========================================================================
    
    aov_data <- reactive({
      req(dna_results())
      df <- dna_results()
      
      # 計算客單價 (AOV = m_value / ni)
      df <- df %>%
        filter(!is.na(m_value), !is.na(ni), ni > 0, !is.na(customer_dynamics)) %>%
        mutate(
          aov = m_value / ni
        )

      # 只分析新客和主力客
      df_filtered <- df %>%
        filter(customer_dynamics %in% c("newbie", "active"))

      if (nrow(df_filtered) == 0) {
        return(NULL)
      }

      # 統計
      summary_data <- df_filtered %>%
        group_by(customer_dynamics) %>%
        summarise(
          customer_count = n(),
          avg_aov = round(mean(aov, na.rm = TRUE), 0),
          median_aov = round(median(aov, na.rm = TRUE), 0),
          total_sales = round(sum(m_value, na.rm = TRUE), 0),
          .groups = "drop"
        ) %>%
        mutate(
          stage_name = case_when(
            customer_dynamics == "newbie" ~ "新客",
            customer_dynamics == "active" ~ "主力客",
            TRUE ~ customer_dynamics
          )
        )
      
      # 計算差異
      if (nrow(summary_data) == 2) {
        newbie_aov <- summary_data %>% filter(stage_name == "新客") %>% pull(avg_aov)
        active_aov <- summary_data %>% filter(stage_name == "主力客") %>% pull(avg_aov)
        
        if (length(newbie_aov) > 0 && length(active_aov) > 0) {
          diff_amount <- active_aov - newbie_aov
          diff_percent <- round((diff_amount / newbie_aov) * 100, 1)
        } else {
          diff_amount <- NA
          diff_percent <- NA
        }
      } else {
        diff_amount <- NA
        diff_percent <- NA
      }
      
      return(list(
        data = df_filtered,
        summary = summary_data,
        diff_amount = diff_amount,
        diff_percent = diff_percent
      ))
    })
    
    # 客單價表格
    output$aov_table <- renderDT({
      req(aov_data())
      
      summary_data <- aov_data()$summary
      
      # 格式化表格
      summary_data <- summary_data %>%
        mutate(
          生命週期 = stage_name,
          客戶數 = paste0(customer_count, " 人"),
          平均客單價 = paste0("$", format(avg_aov, big.mark = ",")),
          中位數客單價 = paste0("$", format(median_aov, big.mark = ",")),
          總銷售額 = paste0("$", format(total_sales, big.mark = ","))
        ) %>%
        select(生命週期, 客戶數, 平均客單價, 中位數客單價, 總銷售額)
      
      # 添加差異說明
      if (!is.na(aov_data()$diff_percent)) {
        diff_text <- if (aov_data()$diff_amount > 0) {
          paste0("主力客客單價比新客高 $", format(abs(aov_data()$diff_amount), big.mark = ","),
                 " (", aov_data()$diff_percent, "%)")
        } else {
          paste0("新客客單價比主力客高 $", format(abs(aov_data()$diff_amount), big.mark = ","),
                 " (", abs(aov_data()$diff_percent), "%)")
        }
      } else {
        diff_text <- ""
      }
      
      datatable(
        summary_data,
        options = list(
          dom = 't',
          ordering = FALSE,
          pageLength = 2
        ),
        rownames = FALSE,
        class = "display nowrap",
        caption = htmltools::tags$caption(
          style = "caption-side: bottom; text-align: left; padding: 10px; color: #28a745; font-weight: bold;",
          diff_text
        )
      )
    })
    
    # 客單價圖表
    output$aov_plot <- renderPlotly({
      req(aov_data())
      
      summary_data <- aov_data()$summary
      
      # 長條圖
      plot_ly(
        data = summary_data,
        x = ~stage_name,
        y = ~avg_aov,
        type = 'bar',
        text = ~paste0("$", format(avg_aov, big.mark = ",")),
        textposition = 'outside',
        marker = list(
          color = c("新客" = "#17a2b8", "主力客" = "#28a745"),
          line = list(color = '#FFFFFF', width = 1.5)
        ),
        hovertemplate = paste0(
          '<b>%{x}</b><br>',
          '平均客單價: $%{y:,.0f}<br>',
          '客戶數: %{customdata}<br>',
          '<extra></extra>'
        ),
        customdata = ~customer_count
      ) %>%
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = "平均客單價 ($)", tickformat = ","),
          showlegend = FALSE
        )
    })

    # 返回客戶資料供下一個模組使用
    return(dna_results)

  })
}
