################################################################################
# 第四列：顧客動態模組
# Customer Dynamics Module
#
# Purpose: 計算並展示顧客動態相關標籤
# - 顧客動態（生命週期階段）
# - 流失風險
# - 預估流失天數
################################################################################

library(shiny)
library(dplyr)
library(DT)
library(plotly)

# Source tag calculation functions
if (file.exists("utils/calculate_customer_tags.R")) {
  source("utils/calculate_customer_tags.R")
}

#' Customer Status Module - UI
#'
#' @param id Module namespace ID
#' @return Shiny UI elements
customerStatusUI <- function(id) {
  ns <- NS(id)

  div(
    h3("顧客動態分析", style = "text-align: center; margin: 20px 0;"),

    # 狀態顯示
    wellPanel(
      h4("📊 分析狀態"),
      verbatimTextOutput(ns("status"))
    ),

    # 資料準備就緒後顯示
    conditionalPanel(
      condition = paste0("output['", ns("data_ready"), "'] == true"),

      # PDF需求 #5.2.1: 顧客動態分布
      fluidRow(
        column(6,
          bs4Card(
            title = "顧客動態分布",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput(ns("lifecycle_pie"), height = "350px")
          )
        ),
        column(6,
          bs4Card(
            title = "流失風險分布",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            # PDF需求 #5.4: 高風險客戶匯出功能
            div(style = "margin-bottom: 10px;",
              downloadButton(ns("download_high_risk"), "匯出高風險客戶", class = "btn-danger btn-sm", style = "margin-right: 10px;"),
              actionButton(ns("show_export_warning"), "匯出說明", icon = icon("info-circle"), class = "btn-info btn-sm")
            ),
            plotlyOutput(ns("churn_risk_bar"), height = "350px")
          )
        )
      ),

      # ============================================================================
      # [COMMENTED OUT - 2025-12-26] 顧客動態 × 流失風險矩陣
      # 原因：簡化分析內容
      # ============================================================================
      # # PDF需求 #5.2.2: 顧客動態 × 流失風險矩陣
      # fluidRow(
      #   column(12,
      #     bs4Card(
      #       title = "顧客動態 × 流失風險矩陣",
      #       status = "info",
      #       solidHeader = TRUE,
      #       width = 12,
      #       plotlyOutput(ns("lifecycle_churn_heatmap"), height = "400px")
      #     )
      #   )
      # ),

      # PDF需求 #5.6: 預估流失天數分布（改為兩張圖）
      fluidRow(
        column(6,
          bs4Card(
            title = "流失狀態分布",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput(ns("churn_status_pie"), height = "350px")
          )
        ),
        column(6,
          bs4Card(
            title = "預估流失天數分布（未流失客戶）",
            status = "danger",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput(ns("days_to_churn_hist"), height = "350px")
          )
        )
      ),

      # 統計指標區塊
      fluidRow(
        column(6,
          bs4Card(
            title = "關鍵統計指標",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            div(style = "padding: 15px;",
              # PDF需求 #5.7: 更新統計指標
              # Row 1: 新客數 + 主力客數
              fluidRow(
                column(6,
                  div(style = "text-align: center; margin-bottom: 15px;",
                    h5("新客數", style = "color: #17a2b8; margin-bottom: 5px;"),
                    h3(textOutput(ns("newbie_count")), style = "color: #17a2b8; margin: 5px 0;"),
                    p("位", style = "color: #6c757d; font-size: 0.9em;")
                  )
                ),
                column(6,
                  div(style = "text-align: center; margin-bottom: 15px;",
                    h5("主力客數", style = "color: #28a745; margin-bottom: 5px;"),
                    h3(textOutput(ns("active_count")), style = "color: #28a745; margin: 5px 0;"),
                    p("位", style = "color: #6c757d; font-size: 0.9em;")
                  )
                )
              ),
              # Row 2: 睡眠客數 + 半睡客數
              fluidRow(
                column(6,
                  div(style = "text-align: center; margin-bottom: 15px;",
                    h5("睡眠客數", style = "color: #ffc107; margin-bottom: 5px;"),
                    h3(textOutput(ns("sleepy_count")), style = "color: #ffc107; margin: 5px 0;"),
                    p("位", style = "color: #6c757d; font-size: 0.9em;")
                  )
                ),
                column(6,
                  div(style = "text-align: center; margin-bottom: 15px;",
                    h5("半睡客數", style = "color: #fd7e14; margin-bottom: 5px;"),
                    h3(textOutput(ns("half_sleepy_count")), style = "color: #fd7e14; margin: 5px 0;"),
                    p("位", style = "color: #6c757d; font-size: 0.9em;")
                  )
                )
              ),
              # Row 3: 沉睡客數 + 高風險客數
              fluidRow(
                column(6,
                  div(style = "text-align: center; margin-bottom: 15px;",
                    h5("沉睡客數", style = "color: #6c757d; margin-bottom: 5px;"),
                    h3(textOutput(ns("dormant_count")), style = "color: #6c757d; margin: 5px 0;"),
                    p("位", style = "color: #6c757d; font-size: 0.9em;")
                  )
                ),
                column(6,
                  div(style = "text-align: center; margin-bottom: 15px;",
                    h5("高風險客數", style = "color: #dc3545; margin-bottom: 5px;"),
                    h3(textOutput(ns("high_risk_count")), style = "color: #dc3545; margin: 5px 0;"),
                    p("位", style = "color: #6c757d; font-size: 0.9em;")
                  )
                )
              ),
              # Row 4: 平均流失風險天數（改進說明）
              fluidRow(
                column(12,
                  div(style = "text-align: center; border-top: 1px solid #dee2e6; padding-top: 15px;",
                    h5("平均預估流失天數", style = "color: #e83e8c; margin-bottom: 5px;"),
                    h3(textOutput(ns("avg_days_to_churn")), style = "color: #e83e8c; margin: 5px 0;"),
                    p("天", style = "color: #6c757d; font-size: 0.9em;"),
                    p(style = "color: #6c757d; font-size: 0.8em; margin-top: 5px;",
                      "說明：平均多少天後客戶可能流失",
                      tags$br(),
                      "0 天 = 已超過預期回購時間（高風險）"
                    )
                  )
                )
              )
            )
          )
        )
      ),

      # 詳細資料表 (PDF需求 #5.8.2: 統一用詞)
      fluidRow(
        column(12,
          bs4Card(
            title = "顧客動態詳細資料",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            fluidRow(
              column(6,
                p(style = "margin-top: 10px;", "顯示前 100 筆高風險客戶，按預估流失天數排序")
              ),
              column(6, style = "text-align: right;",
                # PDF需求 #5.10: 下載時顯示 UTF-8 BOM 提醒
                actionButton(ns("show_download_warning"), "下載說明", icon = icon("info-circle"), class = "btn-info btn-sm", style = "margin-right: 10px;"),
                downloadButton(ns("download_data"), "📥 下載完整狀態資料 (CSV)", class = "btn-success")
              )
            ),
            DTOutput(ns("customer_table"))
          )
        )
      )
    )
  )
}

#' Customer Status Module - Server
#'
#' @param id Module namespace ID
#' @param customer_data reactive - 包含客戶資料的 reactive 物件
#' @return reactive - 加入狀態標籤的客戶資料
customerStatusServer <- function(id, customer_data) {
  moduleServer(id, function(input, output, session) {

    # Reactive values
    values <- reactiveValues(
      processed_data = NULL,
      status_text = "等待資料..."
    )

    # ✅ FIX: Use processed data from DNA module (already has all tags)
    observe({
      req(customer_data())

      tryCatch({
        values$status_text <- "📊 載入客戶狀態資料中..."

        # ✅ CRITICAL: DNA module already calculated ALL tags including tag_017_customer_dynamics
        # We should NOT recalculate - just use the data
        # customer_data() from DNA module has tag_017_customer_dynamics with Chinese values
        processed <- customer_data()

        values$processed_data <- processed

        # ✅ FIX: tag_017_customer_dynamics contains CHINESE values (新客, 主力客, 沉睡客, etc.)
        # NOT English values ("active", "dormant")
        # 計算統計數字 - use correct Chinese labels
        high_risk <- sum(processed$tag_018_churn_risk == "高風險", na.rm = TRUE)
        active <- sum(processed$tag_017_customer_dynamics == "主力客", na.rm = TRUE)
        dormant <- sum(processed$tag_017_customer_dynamics == "沉睡客", na.rm = TRUE)

        values$status_text <- sprintf(
          "✅ 完成！分析 %d 位客戶的狀態\n• 高風險客戶：%d 位\n• 主力客戶：%d 位\n• 沉睡客戶：%d 位",
          nrow(processed), high_risk, active, dormant
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

    output$high_risk_count <- renderText({
      req(values$processed_data)
      count <- sum(values$processed_data$tag_018_churn_risk == "高風險", na.rm = TRUE)
      format(count, big.mark = ",")
    })

    output$active_count <- renderText({
      req(values$processed_data)
      # ✅ FIX: Use Chinese value "主力客" not English "active"
      count <- sum(values$processed_data$tag_017_customer_dynamics == "主力客", na.rm = TRUE)
      format(count, big.mark = ",")
    })

    output$dormant_count <- renderText({
      req(values$processed_data)
      # ✅ FIX: Use Chinese value "沉睡客" not English "dormant"
      count <- sum(values$processed_data$tag_017_customer_dynamics == "沉睡客", na.rm = TRUE)
      format(count, big.mark = ",")
    })

    output$avg_days_to_churn <- renderText({
      req(values$processed_data)

      # ✅ 修正：只計算有效資料（排除 NA 和負值）
      valid_days <- values$processed_data$tag_019_days_to_churn
      valid_days <- valid_days[!is.na(valid_days) & valid_days >= 0]

      if (length(valid_days) == 0) {
        return("N/A")
      }

      avg_val <- mean(valid_days)
      format(round(avg_val, 1), big.mark = ",")
    })

    # PDF需求 #5.7: 新增統計指標
    output$newbie_count <- renderText({
      req(values$processed_data)
      count <- sum(values$processed_data$tag_017_customer_dynamics == "新客", na.rm = TRUE)
      format(count, big.mark = ",")
    })

    output$sleepy_count <- renderText({
      req(values$processed_data)
      count <- sum(values$processed_data$tag_017_customer_dynamics == "睡眠客", na.rm = TRUE)
      format(count, big.mark = ",")
    })

    output$half_sleepy_count <- renderText({
      req(values$processed_data)
      count <- sum(values$processed_data$tag_017_customer_dynamics == "半睡客", na.rm = TRUE)
      format(count, big.mark = ",")
    })

    # ==========================================================================
    # 顧客動態分布圖
    # ==========================================================================

    output$lifecycle_pie <- renderPlotly({
      req(values$processed_data)

      # 計算各階段數量
      lifecycle_counts <- values$processed_data %>%
        count(tag_017_customer_dynamics) %>%
        arrange(desc(n))

      # ✅ FIX: tag_017_customer_dynamics contains CHINESE values (新客, 主力客, etc.)
      # NOT English values, so we use Chinese keys in the maps
      color_map <- c(
        "新客" = "#17a2b8",
        "主力客" = "#28a745",
        "睡眠客" = "#ffc107",
        "半睡客" = "#fd7e14",
        "沉睡客" = "#6c757d",
        "未知" = "#e9ecef"
      )

      # Label map for display (tag values are already in Chinese, so we use them directly)
      # But we can map to more user-friendly display names if needed
      label_map <- c(
        "新客" = "新客",
        "主力客" = "主力客",
        "睡眠客" = "睡眠客",
        "半睡客" = "半睡客",
        "沉睡客" = "沉睡客",
        "未知" = "未知"
      )

      lifecycle_counts$label_zh <- label_map[lifecycle_counts$tag_017_customer_dynamics]
      lifecycle_counts$color <- color_map[lifecycle_counts$tag_017_customer_dynamics]

      plot_ly(
        data = lifecycle_counts,
        labels = ~label_zh,
        values = ~n,
        type = "pie",
        marker = list(colors = ~color),
        textinfo = "label+percent+value",
        hovertemplate = "%{label}<br>數量: %{value}<br>比例: %{percent}<extra></extra>"
      ) %>%
        layout(
          title = "",
          showlegend = TRUE
        )
    })

    # ==========================================================================
    # 流失風險分布圖
    # ==========================================================================

    output$churn_risk_bar <- renderPlotly({
      req(values$processed_data)

      # 計算各風險等級數量
      risk_counts <- values$processed_data %>%
        count(tag_018_churn_risk) %>%
        arrange(match(tag_018_churn_risk, c("低", "中", "高")))

      # 定義顏色
      color_map <- c(
        "低" = "#28a745",
        "中" = "#ffc107",
        "高" = "#dc3545"
      )

      plot_ly(
        data = risk_counts,
        x = ~tag_018_churn_risk,
        y = ~n,
        type = "bar",
        marker = list(color = ~color_map[tag_018_churn_risk]),
        text = ~n,
        textposition = "outside",
        hovertemplate = "風險等級: %{x}<br>客戶數: %{y}<extra></extra>"
      ) %>%
        layout(
          xaxis = list(title = "流失風險等級", categoryorder = "array",
                      categoryarray = c("低", "中", "高")),
          yaxis = list(title = "客戶數"),
          title = ""
        )
    })

    # ==========================================================================
    # [COMMENTED OUT - 2025-12-26] 生命週期 × 流失風險熱力圖
    # 原因：簡化分析內容
    # ==========================================================================
    # output$lifecycle_churn_heatmap <- renderPlotly({
    #   req(values$processed_data)
    #
    #   # ✅ 修正：過濾有效資料並計算交叉矩陣
    #   heatmap_data <- values$processed_data %>%
    #     filter(!is.na(tag_017_customer_dynamics), !is.na(tag_018_churn_risk)) %>%
    #     count(tag_017_customer_dynamics, tag_018_churn_risk) %>%
    #     tidyr::pivot_wider(
    #       names_from = tag_018_churn_risk,
    #       values_from = n,
    #       values_fill = 0
    #     )
    #
    #   # 如果沒有資料，顯示提示
    #   if (nrow(heatmap_data) == 0) {
    #     return(plotly_empty() %>% layout(title = "無有效資料"))
    #   }
    #
    #   # 確保有所有風險等級欄位
    #   for (risk in c("低", "中", "高")) {
    #     if (!risk %in% names(heatmap_data)) {
    #       heatmap_data[[risk]] <- 0
    #     }
    #   }
    #
    #   # ✅ 新增：只顯示有資料的列（移除全為0的顧客動態）
    #   heatmap_data <- heatmap_data %>%
    #     mutate(row_total = `低` + `中` + `高`) %>%
    #     filter(row_total > 0) %>%
    #     select(-row_total)
    #
    #   if (nrow(heatmap_data) == 0) {
    #     return(plotly_empty() %>% layout(title = "所有客戶動態資料為空"))
    #   }
    #
    #   # 按照預定順序排列（只保留有資料的）
    #   lifecycle_order <- c("新客", "主力客", "睡眠客", "半睡客", "沉睡客", "未知")
    #   heatmap_data <- heatmap_data %>%
    #     filter(tag_017_customer_dynamics %in% lifecycle_order) %>%
    #     arrange(match(tag_017_customer_dynamics, lifecycle_order))
    #
    #   # 建立風險等級順序
    #   risk_order <- c("低", "中", "高")
    #
    #   # 將資料轉換為矩陣格式
    #   z_matrix <- as.matrix(heatmap_data[, risk_order, drop = FALSE])
    #
    #   # 建立熱圖
    #   plot_ly(
    #     x = risk_order,
    #     y = heatmap_data$tag_017_customer_dynamics,
    #     z = z_matrix,
    #     type = "heatmap",
    #     colorscale = list(
    #       c(0, "rgb(255, 255, 255)"),
    #       c(0.5, "rgb(255, 193, 7)"),
    #       c(1, "rgb(220, 53, 69)")
    #     ),
    #     text = z_matrix,
    #     texttemplate = "%{text}",
    #     textfont = list(size = 14, color = "black"),
    #     hovertemplate = "顧客動態: %{y}<br>流失風險: %{x}<br>客戶數: %{z}<extra></extra>",
    #     showscale = TRUE
    #   ) %>%
    #     layout(
    #       xaxis = list(title = "流失風險等級", side = "bottom"),
    #       yaxis = list(title = "顧客動態", autorange = "reversed"),
    #       title = ""
    #     )
    # })

    # ==========================================================================
    # 流失狀態圓餅圖（新增）
    # ==========================================================================

    output$churn_status_pie <- renderPlotly({
      req(values$processed_data)

      # ✅ 計算已流失 vs 未流失客戶數
      churn_status <- values$processed_data %>%
        filter(!is.na(tag_019_days_to_churn)) %>%
        mutate(
          status = case_when(
            tag_019_days_to_churn == 0 ~ "已流失（0天）",
            tag_019_days_to_churn > 0 ~ "未流失",
            TRUE ~ "未知"
          )
        ) %>%
        count(status) %>%
        mutate(pct = round(n / sum(n) * 100, 1))

      if (nrow(churn_status) == 0) {
        return(plotly_empty() %>% layout(title = "無有效資料"))
      }

      # 定義顏色
      colors <- c(
        "已流失（0天）" = "#dc3545",
        "未流失" = "#28a745",
        "未知" = "#6c757d"
      )

      plot_ly(
        data = churn_status,
        labels = ~status,
        values = ~n,
        type = "pie",
        marker = list(colors = ~colors[status]),
        textinfo = "label+percent",
        hovertemplate = "%{label}<br>客戶數: %{value}<br>佔比: %{percent}<extra></extra>"
      ) %>%
        layout(
          showlegend = TRUE,
          title = ""
        )
    })

    # ==========================================================================
    # 預估流失天數分布圖（只顯示未流失客戶）
    # ==========================================================================

    output$days_to_churn_hist <- renderPlotly({
      req(values$processed_data)

      # ✅ 修正：只顯示未流失客戶（天數 > 0）
      valid_data <- values$processed_data %>%
        filter(!is.na(tag_019_days_to_churn), tag_019_days_to_churn > 0)

      # 如果沒有有效資料，顯示提示
      if (nrow(valid_data) == 0) {
        plotly_empty() %>%
          layout(title = "無未流失客戶資料")
      } else {
        plot_ly(
          x = valid_data$tag_019_days_to_churn,
          type = "histogram",
          marker = list(color = "#ffc107"),
          nbinsx = 30,
          hovertemplate = "天數: %{x}<br>客戶數: %{y}<extra></extra>"
        ) %>%
          layout(
            xaxis = list(
              title = "預估多少天後會流失",
              tickformat = "d"
            ),
            yaxis = list(title = "客戶數"),
            title = "",
            showlegend = FALSE
          )
      }
    })

    # ==========================================================================
    # 資料表
    # ==========================================================================

    output$customer_table <- renderDT({
      req(values$processed_data)

      # ✅ FIX: tag_017_customer_dynamics already contains CHINESE values
      # No mapping needed - use values directly
      # PDF需求 #5.8.2: 統一用詞「顧客動態」

      # 選擇要顯示的欄位
      display_data <- values$processed_data %>%
        select(
          customer_id,
          購買次數 = ni,
          顧客動態 = tag_017_customer_dynamics,
          流失風險 = tag_018_churn_risk,
          預估流失天數 = tag_019_days_to_churn
        ) %>%
        arrange(預估流失天數) %>%
        head(100)  # 只顯示前 100 筆

      # ✅ 修正：將 0 天顯示為「已流失」
      display_data <- display_data %>%
        mutate(
          預估流失天數 = case_when(
            is.na(預估流失天數) ~ "N/A",
            預估流失天數 == 0 ~ "已流失",
            TRUE ~ as.character(round(預估流失天數, 1))
          )
        )

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
          '流失風險',
          backgroundColor = styleEqual(
            c("低", "中", "高"),
            c("#d4edda", "#fff3cd", "#f8d7da")
          )
        )
    })

    # ==========================================================================
    # CSV 下載 + UTF-8 BOM 提醒
    # PDF需求 #5.10
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

    # PDF需求 #5.9: 顧客狀態 CSV 下載欄位名稱優化
    output$download_data <- downloadHandler(
      filename = function() {
        paste0("customer_dynamics_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(values$processed_data)

        # 使用中文欄位名稱，更容易理解
        export_data <- values$processed_data %>%
          select(
            customer_id,
            購買次數 = ni,
            顧客動態 = tag_017_customer_dynamics,
            流失風險 = tag_018_churn_risk,
            預估流失天數 = tag_019_days_to_churn
          )

        write.csv(export_data, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )

    # ==========================================================================
    # PDF需求 #5.4: 高風險客戶匯出
    # ==========================================================================

    # 匯出說明 Modal
    observeEvent(input$show_export_warning, {
      showModal(modalDialog(
        title = "📥 高風險客戶匯出說明",
        size = "m",
        easyClose = TRUE,
        footer = modalButton("關閉"),

        div(
          h4("匯出內容", style = "color: #dc3545; margin-bottom: 15px;"),
          p("此功能會匯出", tags$strong("高風險"), "和", tags$strong("中風險"), "的客戶資料。"),

          tags$hr(),

          h4("匯出欄位", style = "margin-bottom: 15px;"),
          tags$ul(
            tags$li(tags$strong("customer_id"), " - 客戶識別碼"),
            tags$li(tags$strong("顧客動態"), " - 客戶生命週期階段（新客/主力客/睡眠客等）"),
            tags$li(tags$strong("流失風險"), " - 風險等級（高風險/中風險）"),
            tags$li(tags$strong("預估流失天數"), " - 預測多少天後可能流失")
          ),

          tags$hr(),

          h4("💡 使用提醒", style = "margin-bottom: 15px;"),
          tags$ol(
            tags$li(tags$strong("檔案格式："), "CSV 檔案，使用 UTF-8 編碼"),
            tags$li(tags$strong("Excel 開啟："), "若出現亂碼，請用記事本開啟後另存為「使用 BOM 的 UTF-8」格式"),
            tags$li(tags$strong("篩選條件："), "僅包含高風險與中風險客戶"),
            tags$li(tags$strong("建議用途："), "優先關懷高風險客戶，制定挽留策略")
          ),

          tags$hr(),

          p(style = "color: #6c757d; font-size: 0.9em; margin-top: 15px;",
            "💡 提示：建議定期匯出高風險客戶名單，並制定相應的客戶關懷計劃，以降低流失率。")
        )
      ))
    })

    # 高風險客戶下載處理
    output$download_high_risk <- downloadHandler(
      filename = function() {
        paste0("high_risk_customers_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(values$processed_data)

        # 篩選高風險和中風險客戶
        export_data <- values$processed_data %>%
          filter(tag_018_churn_risk %in% c("高風險", "中風險")) %>%
          select(
            customer_id,
            顧客動態 = tag_017_customer_dynamics,
            流失風險 = tag_018_churn_risk
          ) %>%
          arrange(desc(流失風險 == "高風險"))  # 高風險優先

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
# customerStatusUI("customer_status")
#
# # Server
# status_data <- customerStatusServer("customer_status", rfm_data)
#
################################################################################
