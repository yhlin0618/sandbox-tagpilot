################################################################################
# 第六列：生命週期預測模組
# Lifecycle Prediction Module
#
# Purpose: 計算並展示生命週期預測相關標籤
# - 下次購買金額預測
# - 下次購買日期預測
# - 預測信心度
################################################################################

library(shiny)
library(dplyr)
library(DT)
library(plotly)

# Source tag calculation functions
if (file.exists("utils/calculate_customer_tags.R")) {
  source("utils/calculate_customer_tags.R")
}

#' Lifecycle Prediction Module - UI
#'
#' @param id Module namespace ID
#' @return Shiny UI elements
lifecyclePredictionUI <- function(id) {
  ns <- NS(id)

  div(
    h3("生命週期預測分析", style = "text-align: center; margin: 20px 0;"),

    # 狀態顯示
    wellPanel(
      h4("📊 分析狀態"),
      verbatimTextOutput(ns("status"))
    ),

    # 資料準備就緒後顯示
    conditionalPanel(
      condition = paste0("output['", ns("data_ready"), "'] == true"),

      # 預測關鍵指標 (Issue #8: 移除平均預測天數)
      fluidRow(
        column(6,
          bs4Card(
            title = "平均預測購買金額",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            div(style = "text-align: center; padding: 20px;",
              h2(textOutput(ns("avg_predicted_amount")), style = "color: #28a745; margin: 0;"),
              p("元", style = "color: #6c757d;")
            )
          )
        ),
        # Issue #8: 移除「平均預測天數」(邏輯不好解釋)
        # column(4,
        #   bs4Card(
        #     title = "平均預測天數",
        #     status = "info",
        #     solidHeader = TRUE,
        #     width = 12,
        #     div(style = "text-align: center; padding: 20px;",
        #       h2(textOutput(ns("avg_predicted_days")), style = "color: #17a2b8; margin: 0;"),
        #       p("天後", style = "color: #6c757d;")
        #     )
        #   )
        # ),
        column(6,
          bs4Card(
            title = "高信心度預測客戶",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            div(style = "text-align: center; padding: 20px;",
              h2(textOutput(ns("high_confidence_count")), style = "color: #ffc107; margin: 0;"),
              p("位（購買次數 ≥ 4）", style = "color: #6c757d; font-size: 0.9em;")
            )
          )
        )
      ),

      # 預測分布圖表
      fluidRow(
        column(6,
          bs4Card(
            title = "預測購買金額分布",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput(ns("predicted_amount_plot"), height = "350px")
          )
        ),
        column(6,
          bs4Card(
            title = "預測購買日期分布",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput(ns("predicted_date_plot"), height = "350px")
          )
        )
      ),

      # 預測時間軸
      fluidRow(
        column(12,
          bs4Card(
            title = "未來 90 天預測購買時間軸",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput(ns("prediction_timeline"), height = "400px")
          )
        )
      ),

      # 預測金額 vs 歷史金額散點圖
      fluidRow(
        column(12,
          bs4Card(
            title = "預測購買金額 vs 歷史平均金額（氣泡大小 = 購買次數）",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput(ns("prediction_vs_history"), height = "450px"),
            footer = div(
              style = "padding: 10px; background-color: #f8f9fa; border-top: 1px solid #dee2e6;",
              tags$small(
                icon("info-circle"),
                " ",
                strong("圖表說明："),
                br(),
                "• ",
                strong("X軸："),
                "歷史平均消費金額（M值）｜",
                strong("Y軸："),
                "預測下次購買金額｜",
                strong("氣泡大小："),
                "購買次數（ni）",
                br(),
                "• ",
                strong("顏色代表預測信心度："),
                tags$span(style = "color: #28a745; font-weight: bold;", "■ 綠色"),
                "（≥4次購買，高信心度）、",
                tags$span(style = "color: #ffc107; font-weight: bold;", "■ 黃色"),
                "（2-3次購買，中信心度）、",
                tags$span(style = "color: #dc3545; font-weight: bold;", "■ 紅色"),
                "（1次購買，低信心度）",
                br(),
                "• ",
                strong("篩選條件："),
                "顯示所有有預測金額且歷史消費金額 > 0 的客戶（包含僅購買 1 次的新客）",
                br(),
                "• ",
                strong("採樣策略："),
                "若客戶數 > 2000，優先保留所有重複購買客戶（ni≥2），再從新客中隨機採樣至 2000 筆；若客戶數介於 500-2000，隨機採樣 500 筆"
              )
            )
          )
        )
      ),

      # 詳細資料表
      fluidRow(
        column(12,
          bs4Card(
            title = "客戶生命週期預測詳細資料",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            fluidRow(
              column(6,
                p(style = "margin-top: 10px;", "顯示前 100 筆客戶資料，按預測購買日期排序")
              ),
              column(6, style = "text-align: right;",
                downloadButton(ns("download_data"), "📥 下載完整預測資料 (CSV)", class = "btn-success")
              )
            ),
            DTOutput(ns("customer_table"))
          )
        )
      )
    )
  )
}

#' Lifecycle Prediction Module - Server
#'
#' @param id Module namespace ID
#' @param customer_data reactive - 包含客戶資料的 reactive 物件
#' @return reactive - 加入預測標籤的客戶資料
lifecyclePredictionServer <- function(id, customer_data) {
  moduleServer(id, function(input, output, session) {

    # Reactive values
    values <- reactiveValues(
      processed_data = NULL,
      status_text = "等待資料..."
    )

    # 處理資料並計算預測標籤
    observe({
      req(customer_data())

      tryCatch({
        values$status_text <- "📊 計算生命週期預測標籤中..."

        # 使用 utils 中的函數計算預測標籤
        processed <- customer_data() %>%
          calculate_prediction_tags()

        values$processed_data <- processed

        # 計算統計數字
        high_confidence <- sum(processed$ni >= 4, na.rm = TRUE)
        avg_pred_amount <- mean(processed$tag_030_next_purchase_amount, na.rm = TRUE)

        values$status_text <- sprintf(
          "✅ 完成！預測 %d 位客戶的未來行為\n• 高信心度客戶：%d 位（購買次數 ≥ 4）\n• 平均預測金額：%s 元",
          nrow(processed),
          high_confidence,
          format(round(avg_pred_amount, 0), big.mark = ",")
        )

      }, error = function(e) {
        values$status_text <- paste("❌ 錯誤:", e$message)
      })
    })

    # 控制 UI 顯示
    output$data_ready <- reactive({
      !is.null(values$processed_data)
    })
    outputOptions(output, "data_ready", suspendWhenHidden = FALSE)

    # 狀態輸出
    output$status <- renderText({
      values$status_text
    })

    # ==========================================================================
    # 關鍵指標
    # ==========================================================================

    output$avg_predicted_amount <- renderText({
      req(values$processed_data)
      avg_val <- mean(values$processed_data$tag_030_next_purchase_amount, na.rm = TRUE)
      format(round(avg_val, 0), big.mark = ",")
    })

    # Issue #8: 移除平均預測天數（邏輯不好解釋）
    # output$avg_predicted_days <- renderText({
    #   req(values$processed_data)
    #   # 計算預測日期與今天的天數差
    #   today <- Sys.Date()
    #   avg_days <- mean(
    #     as.numeric(difftime(values$processed_data$tag_031_next_purchase_date, today, units = "days")),
    #     na.rm = TRUE
    #   )
    #   format(round(avg_days, 1), big.mark = ",")
    # })

    output$high_confidence_count <- renderText({
      req(values$processed_data)
      count <- sum(values$processed_data$ni >= 4, na.rm = TRUE)
      format(count, big.mark = ",")
    })

    # ==========================================================================
    # 預測金額分布圖
    # ==========================================================================

    output$predicted_amount_plot <- renderPlotly({
      req(values$processed_data)

      plot_ly(
        x = values$processed_data$tag_030_next_purchase_amount,
        type = "histogram",
        marker = list(color = "#28a745"),
        nbinsx = 30
      ) %>%
        layout(
          xaxis = list(title = "預測購買金額（元）"),
          yaxis = list(title = "客戶數"),
          title = ""
        )
    })

    # ==========================================================================
    # 預測日期分布圖
    # ==========================================================================

    output$predicted_date_plot <- renderPlotly({
      req(values$processed_data)

      # 計算距今天數
      today <- Sys.Date()
      days_until <- as.numeric(difftime(
        values$processed_data$tag_031_next_purchase_date,
        today,
        units = "days"
      ))

      plot_ly(
        x = days_until,
        type = "histogram",
        marker = list(color = "#17a2b8"),
        nbinsx = 30
      ) %>%
        layout(
          xaxis = list(title = "預測天數（天後）"),
          yaxis = list(title = "客戶數"),
          title = ""
        )
    })

    # ==========================================================================
    # 預測時間軸（未來 90 天）
    # ==========================================================================

    output$prediction_timeline <- renderPlotly({
      req(values$processed_data)

      today <- Sys.Date()
      future_90 <- today + 90

      # 篩選未來 90 天內的預測
      timeline_data <- values$processed_data %>%
        filter(
          tag_031_next_purchase_date >= today,
          tag_031_next_purchase_date <= future_90
        ) %>%
        count(tag_031_next_purchase_date) %>%
        arrange(tag_031_next_purchase_date)

      # 如果沒有資料，創建空圖表
      if (nrow(timeline_data) == 0) {
        plot_ly() %>%
          layout(
            title = "未來 90 天內暫無預測購買",
            xaxis = list(title = "日期"),
            yaxis = list(title = "預測購買客戶數")
          )
      } else {
        plot_ly(
          data = timeline_data,
          x = ~tag_031_next_purchase_date,
          y = ~n,
          type = "scatter",
          mode = "lines+markers",
          marker = list(color = "#17a2b8", size = 8),
          line = list(color = "#17a2b8", width = 2),
          hovertemplate = "日期: %{x|%Y-%m-%d}<br>預測客戶數: %{y}<extra></extra>"
        ) %>%
          layout(
            xaxis = list(title = "預測購買日期"),
            yaxis = list(title = "預測購買客戶數"),
            title = ""
          )
      }
    })

    # ==========================================================================
    # 預測 vs 歷史散點圖
    # ==========================================================================

    output$prediction_vs_history <- renderPlotly({
      req(values$processed_data)

      # ✅ 修正：準備資料，顯示所有有預測金額的客戶
      # 放寬過濾條件，只要有預測金額和歷史消費金額即可
      scatter_data <- values$processed_data %>%
        filter(
          !is.na(tag_030_next_purchase_amount),  # 必須有預測金額
          !is.na(tag_011_rfm_m),                 # 必須有歷史消費金額
          tag_030_next_purchase_amount > 0,      # 預測金額必須 > 0（避免顯示異常值）
          tag_011_rfm_m > 0                      # 歷史金額必須 > 0（避免顯示異常值）
        )

      # 詳細診斷輸出
      total_customers <- nrow(values$processed_data)
      has_prediction <- sum(!is.na(values$processed_data$tag_030_next_purchase_amount))
      has_both <- sum(!is.na(values$processed_data$tag_030_next_purchase_amount) &
                      !is.na(values$processed_data$tag_011_rfm_m))
      final_count <- nrow(scatter_data)

      cat(sprintf("預測金額散點圖資料檢查:\n"))
      cat(sprintf("  原始客戶總數: %d 筆\n", total_customers))
      cat(sprintf("  有預測金額: %d 筆 (%.1f%%)\n",
                  has_prediction, has_prediction/total_customers*100))
      cat(sprintf("  同時有預測金額和歷史金額: %d 筆 (%.1f%%)\n",
                  has_both, has_both/total_customers*100))
      cat(sprintf("  過濾異常值後: %d 筆 (%.1f%%)\n",
                  final_count, final_count/total_customers*100))

      # 分析 ni 分布
      if (final_count > 0) {
        ni_dist <- scatter_data %>%
          count(ni) %>%
          arrange(desc(n))
        cat(sprintf("  購買次數分布（前5名）:\n"))
        for (i in 1:min(5, nrow(ni_dist))) {
          cat(sprintf("    ni=%d: %d 筆 (%.1f%%)\n",
                      ni_dist$ni[i], ni_dist$n[i], ni_dist$n[i]/final_count*100))
        }
      }

      # 如果沒有資料，顯示提示
      if (nrow(scatter_data) == 0) {
        return(plotly_empty() %>%
          layout(
            title = "無可用預測資料",
            annotations = list(
              x = 0.5, y = 0.5,
              text = "所有客戶的預測金額資料不足\n請確認客戶有足夠的購買記錄",
              showarrow = FALSE,
              font = list(size = 16)
            )
          ))
      }

      # 建立信心度分類（在採樣之前，用於分層採樣）
      scatter_data <- scatter_data %>%
        mutate(
          confidence = case_when(
            ni >= 4 ~ "高（≥4次）",
            ni >= 2 ~ "中（2-3次）",
            TRUE ~ "低（1次）"
          )
        )

      # 如果資料太多，使用分層採樣確保各信心度層級都有代表
      original_count <- nrow(scatter_data)
      if (original_count > 2000) {
        # 對於超大數據集，使用分層採樣
        # 確保高價值客戶（ni>=2）都被包含，然後從 ni=1 中隨機採樣
        high_value <- scatter_data %>% filter(ni >= 2)
        low_value <- scatter_data %>% filter(ni == 1)

        # 如果高價值客戶超過 1000，採樣到 1000
        if (nrow(high_value) > 1000) {
          high_value <- high_value %>% sample_n(1000)
        }

        # 從低價值客戶中採樣補足到 2000
        remaining_slots <- 2000 - nrow(high_value)
        if (nrow(low_value) > remaining_slots) {
          low_value <- low_value %>% sample_n(remaining_slots)
        }

        scatter_data <- bind_rows(high_value, low_value)
        cat(sprintf("  分層採樣顯示: %d 筆（原始 %s 筆）\n",
                    nrow(scatter_data), format(original_count, big.mark = ",")))
        cat(sprintf("    - ni≥2: %d 筆\n", nrow(high_value)))
        cat(sprintf("    - ni=1: %d 筆\n", nrow(low_value)))
      } else if (original_count > 500) {
        # 中等數據集，簡單隨機採樣
        scatter_data <- scatter_data %>%
          sample_n(500)
        cat(sprintf("  隨機採樣顯示: 500 筆（原始 %d 筆）\n", original_count))
      }

      # 顏色映射
      color_map <- c(
        "高（≥4次）" = "#28a745",
        "中（2-3次）" = "#ffc107",
        "低（1次）" = "#dc3545"
      )

      # 計算參考線範圍
      max_value <- max(scatter_data$tag_011_rfm_m, na.rm = TRUE)

      # 建立參考線資料
      reference_line <- data.frame(
        x = c(0, max_value),
        y = c(0, max_value)
      )

      plot_ly() %>%
        # 添加散點圖
        add_trace(
          data = scatter_data,
          x = ~tag_011_rfm_m,
          y = ~tag_030_next_purchase_amount,
          size = ~ni,
          color = ~confidence,
          colors = color_map,
          type = "scatter",
          mode = "markers",
          marker = list(
            sizemode = "diameter",
            sizeref = 2,
            opacity = 0.6
          ),
          text = ~paste0(
            "客戶: ", customer_id, "<br>",
            "歷史平均: ", format(round(tag_011_rfm_m, 0), big.mark = ","), " 元<br>",
            "預測金額: ", format(round(tag_030_next_purchase_amount, 0), big.mark = ","), " 元<br>",
            "購買次數: ", ni, " 次<br>",
            "預測信心度: ", confidence
          ),
          hoverinfo = "text"
        ) %>%
        # 添加 y=x 參考線（使用獨立的資料框）
        add_trace(
          data = reference_line,
          x = ~x,
          y = ~y,
          type = "scatter",
          mode = "lines",
          line = list(color = "gray", dash = "dash"),
          name = "參考線 (y=x)",
          showlegend = TRUE,
          hoverinfo = "skip",
          inherit = FALSE  # 不繼承主圖的屬性
        ) %>%
        layout(
          xaxis = list(title = "歷史平均購買金額（元）"),
          yaxis = list(title = "預測購買金額（元）"),
          title = "",
          showlegend = TRUE,
          legend = list(title = list(text = "預測信心度"))
        )
    })

    # ==========================================================================
    # 資料表
    # ==========================================================================

    output$customer_table <- renderDT({
      req(values$processed_data)

      # 計算距今天數
      today <- Sys.Date()

      # 選擇要顯示的欄位
      display_data <- values$processed_data %>%
        mutate(
          days_until = as.numeric(difftime(tag_031_next_purchase_date, today, units = "days")),
          confidence = case_when(
            ni >= 4 ~ "高",
            ni >= 2 ~ "中",
            TRUE ~ "低"
          )
        ) %>%
        select(
          customer_id,
          購買次數 = ni,
          預測金額 = tag_030_next_purchase_amount,
          預測日期 = tag_031_next_purchase_date,
          距今天數 = days_until,
          預測信心度 = confidence
        ) %>%
        arrange(距今天數) %>%
        head(100)  # 只顯示前 100 筆

      # 格式化數值
      display_data$預測金額 <- format(round(display_data$預測金額, 0), big.mark = ",")
      display_data$預測日期 <- as.character(display_data$預測日期)
      display_data$距今天數 <- round(display_data$距今天數, 1)

      datatable(
        display_data,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Chinese-traditional.json')
        ),
        rownames = FALSE
      ) %>%
        formatStyle(
          '預測信心度',
          backgroundColor = styleEqual(
            c("低", "中", "高"),
            c("#f8d7da", "#fff3cd", "#d4edda")
          )
        ) %>%
        formatStyle(
          '距今天數',
          background = styleColorBar(range(display_data$距今天數, na.rm = TRUE), '#17a2b8'),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
    })

    # ==========================================================================
    # CSV 下載
    # ==========================================================================

    output$download_data <- downloadHandler(
      filename = function() {
        paste0("customer_lifecycle_prediction_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(values$processed_data)

        export_data <- values$processed_data %>%
          select(
            customer_id,
            ni,
            tag_030_next_purchase_amount,
            tag_031_next_purchase_date
          )

        write.csv(export_data, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )

    # ==========================================================================
    # 回傳處理後的資料（供其他模組使用）
    # ==========================================================================

    return(reactive({ values$processed_data }))
  })
}

################################################################################
# 使用範例（在 app.R 中）
################################################################################
#
# # UI
# lifecyclePredictionUI("lifecycle_pred")
#
# # Server
# prediction_data <- lifecyclePredictionServer("lifecycle_pred", status_data)
#
################################################################################
