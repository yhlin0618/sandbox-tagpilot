# ============================================================================
# 第三列：顧客活躍度模組 (Customer Activity Index Module)
# ============================================================================
# Req #4.1: 新增顧客活躍度分析頁面
# CAI (Customer Activity Index) = (mle - wmle) / mle
#   mle = 最後購買間隔 (mean of last intervals)
#   wmle = 加權平均購買間隔 (weighted mean of intervals)
# 用於評估客戶購買間隔的變化趨勢（活躍度）
# 僅針對交易次數 >= 4 的顧客計算（需要足夠的間隔數據）

library(shiny)
library(bs4Dash)
library(DT)
library(plotly)
library(dplyr)
library(tidyr)  # For pivot_wider in lifecycle × CAI matrix

#' 顧客活躍度模組 UI
#'
#' @param id Module ID
#' @return UI elements
customerActivityUI <- function(id) {
  ns <- NS(id)

  tagList(
    # === 第一列：平均 CAI 值卡片 ===
    h4("📊 顧客活躍度總覽", style = "margin-bottom: 20px;"),

    fluidRow(
      column(3,
        bs4ValueBox(
          value = textOutput(ns("avg_cai"), inline = TRUE),
          subtitle = "平均顧客活躍度",
          icon = icon("chart-line"),
          color = "info",
          width = 12
        )
      ),
      column(3,
        bs4ValueBox(
          value = textOutput(ns("active_pct"), inline = TRUE),
          subtitle = "漸趨活躍消費客戶比例",
          icon = icon("users"),
          color = "success",
          width = 12
        )
      ),
      column(3,
        bs4ValueBox(
          value = textOutput(ns("stable_pct"), inline = TRUE),
          subtitle = "穩定消費客戶比率",
          icon = icon("hand-point-right"),
          color = "warning",
          width = 12
        )
      ),
      column(3,
        bs4ValueBox(
          value = textOutput(ns("inactive_pct"), inline = TRUE),
          subtitle = "漸趨靜止消費客戶比例",
          icon = icon("bed"),
          color = "danger",
          width = 12
        )
      )
    ),

    # === 第二列：活躍度分群表格與圓餅圖 ===
    h4("📋 活躍度分群分析", style = "margin: 30px 0 20px 0;"),

    fluidRow(
      column(6,
        bs4Card(
          title = "活躍度分群統計",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          DTOutput(ns("activity_segment_table"))
        )
      ),
      column(6,
        bs4Card(
          title = "活躍度分群佔比",
          status = "info",
          solidHeader = FALSE,
          width = 12,
          plotlyOutput(ns("activity_segment_pie"), height = "300px")
        )
      )
    ),

    # ============================================================================
    # [COMMENTED OUT - 2025-12-26] 生命週期 × CAI 交叉分析
    # 原因：簡化分析內容，避免廠商覺得太複雜
    # ============================================================================
    # # === 第三列：生命週期 × CAI 交叉矩陣圖 ===
    # h4("🔄 生命週期 × CAI 交叉分析", style = "margin: 30px 0 20px 0;"),
    #
    # fluidRow(
    #   column(12,
    #     bs4Card(
    #       title = "生命週期 × CAI 活躍度矩陣",
    #       status = "primary",
    #       solidHeader = FALSE,
    #       width = 12,
    #       plotlyOutput(ns("lifecycle_cai_matrix"), height = "400px")
    #     )
    #   )
    # ),

    # === 第四列：CAI 分布圖表（兩張圖）===
    h4("📈 CAI 分布分析", style = "margin: 30px 0 20px 0;"),

    fluidRow(
      column(6,
        bs4Card(
          title = "CAI 數值分布",
          status = "primary",
          solidHeader = FALSE,
          width = 12,
          plotlyOutput(ns("cai_distribution"), height = "350px")
        )
      ),
      column(6,
        bs4Card(
          title = "CAI × 購買金額關係",
          status = "primary",
          solidHeader = FALSE,
          width = 12,
          plotlyOutput(ns("cai_vs_monetary"), height = "350px")
        )
      )
    ),

    # === 第四列：客戶 CAI 詳細資料表 ===
    h4("📄 客戶 CAI 詳細資料", style = "margin: 30px 0 20px 0;"),

    fluidRow(
      column(12,
        bs4Card(
          title = "客戶活躍度詳細列表（前 100 筆）",
          status = "success",
          solidHeader = TRUE,
          width = 12,
          div(style = "margin-bottom: 10px;",
            downloadButton(ns("download_cai"), "下載完整資料",
                           class = "btn-success btn-sm", style = "margin-right: 10px;"),
            actionButton(ns("show_download_warning"), "下載說明",
                         icon = icon("info-circle"), class = "btn-info btn-sm")
          ),
          DTOutput(ns("cai_detail_table"))
        )
      )
    )
  )
}

#' 顧客活躍度模組 Server
#'
#' @param id Module ID
#' @param processed_data Reactive containing processed customer data
customerActivityServer <- function(id, processed_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values
    values <- reactiveValues(
      cai_data = NULL,
      segment_data = NULL
    )

    # ==========================================================================
    # 使用來自 DNA 分析的 CAI 資料並進行分群
    # ==========================================================================
    observe({
      req(processed_data())

      tryCatch({
        # 使用 DNA 分析已經計算好的 CAI
        # cai_value = (mle - wmle) / mle （來自 fn_analysis_dna.R）
        # cai_ecdf = CAI 的累積分布函數值（百分位數）

        # 檢查是否有 cai_value 欄位
        if (!"cai_value" %in% names(processed_data()) && !"cai" %in% names(processed_data())) {
          showNotification(
            "錯誤：找不到 CAI 資料。請確認已執行 DNA 分析。",
            type = "error",
            duration = 10
          )
          return(NULL)
        }

        # 標準化欄位名稱（處理 cai 或 cai_value）
        cai_df <- processed_data() %>%
          mutate(
            cai = if("cai_value" %in% names(.)) cai_value else cai,
            cai_ecdf = if("cai_ecdf" %in% names(.)) cai_ecdf else NA_real_
          )

        # 過濾有效的 CAI 資料（僅保留 ni >= 4 的客戶）
        # CAI 只對交易次數 >= 4 的客戶計算，其他為 NA
        cai_df <- cai_df %>%
          filter(!is.na(cai))

        # 根據 CAI 原始值進行分群（2025-12-26 修改）
        # 理論：
        # - CAI >= 0.2：購買間隔縮短 → 漸趨活躍
        # - -0.2 < CAI < 0.2：購買間隔穩定 → 穩定消費
        # - CAI <= -0.2：購買間隔拉長 → 漸趨靜止
        # 使用 CAI 原始值 ±0.2 閾值分群
        if (nrow(cai_df) > 0 && sum(!is.na(cai_df$cai)) > 0) {
          cai_df <- cai_df %>%
            mutate(
              activity_segment = case_when(
                is.na(cai) ~ "未知",
                cai >= 0.2 ~ "漸趨活躍消費客戶",    # CAI >= 0.2
                cai > -0.2 ~ "穩定消費客戶",        # -0.2 < CAI < 0.2
                TRUE ~ "漸趨靜止消費客戶"           # CAI <= -0.2
              )
            )

          values$cai_data <- cai_df

          # 計算分群統計
          values$segment_data <- cai_df %>%
            group_by(activity_segment) %>%
            summarise(
              客戶數量 = n(),
              百分比 = sprintf("%.1f%%", n() / nrow(cai_df) * 100),
              平均顧客活躍度 = round(mean(cai, na.rm = TRUE), 2),  # 改名：平均 CAI 值 → 平均顧客活躍度
              .groups = "drop"
            ) %>%
            mutate(
              activity_segment = factor(activity_segment,
                                        levels = c("漸趨活躍消費客戶", "穩定消費客戶", "漸趨靜止消費客戶", "未知"))
            ) %>%
            arrange(activity_segment)
        }
      }, error = function(e) {
        showNotification(
          paste("讀取 CAI 資料時發生錯誤:", e$message),
          type = "error",
          duration = 5
        )
      })
    })

    # ==========================================================================
    # 第一列：總覽指標
    # ==========================================================================

    output$avg_cai <- renderText({
      req(values$cai_data)
      avg <- mean(values$cai_data$cai, na.rm = TRUE)
      format(round(avg, 2), big.mark = ",")
    })

    output$active_pct <- renderText({
      req(values$segment_data)
      seg <- values$segment_data %>% filter(activity_segment == "漸趨活躍消費客戶")
      if (nrow(seg) > 0) {
        seg$百分比[1]
      } else {
        "0%"
      }
    })

    output$stable_pct <- renderText({
      req(values$segment_data)
      seg <- values$segment_data %>% filter(activity_segment == "穩定消費客戶")
      if (nrow(seg) > 0) {
        seg$百分比[1]
      } else {
        "0%"
      }
    })

    output$inactive_pct <- renderText({
      req(values$segment_data)
      seg <- values$segment_data %>% filter(activity_segment == "漸趨靜止消費客戶")
      if (nrow(seg) > 0) {
        seg$百分比[1]
      } else {
        "0%"
      }
    })

    # ==========================================================================
    # 第二列：活躍度分群表格與圓餅圖
    # ==========================================================================

    output$activity_segment_table <- renderDT({
      req(values$segment_data)

      datatable(
        values$segment_data %>% rename(分群 = activity_segment),
        options = list(
          dom = 't',
          ordering = FALSE,
          pageLength = 10
        ),
        rownames = FALSE
      ) %>%
        formatStyle(
          '分群',
          backgroundColor = styleEqual(
            c("漸趨活躍消費客戶", "穩定消費客戶", "漸趨靜止消費客戶"),
            c("#d4edda", "#fff3cd", "#f8d7da")
          )
        )
    })

    output$activity_segment_pie <- renderPlotly({
      req(values$segment_data)

      plot_data <- values$segment_data %>%
        filter(activity_segment != "未知")

      plot_ly(
        data = plot_data,
        labels = ~activity_segment,
        values = ~客戶數量,
        type = 'pie',
        marker = list(
          colors = c("#28a745", "#ffc107", "#dc3545"),
          line = list(color = '#FFFFFF', width = 2)
        ),
        textinfo = 'label+percent',
        hovertemplate = paste0(
          "<b>%{label}</b><br>",
          "客戶數：%{value}<br>",
          "佔比：%{percent}<br>",
          "<extra></extra>"
        )
      ) %>%
        layout(
          showlegend = TRUE,
          legend = list(orientation = "v", x = 1, y = 0.5)
        )
    })

    # ==========================================================================
    # [COMMENTED OUT - 2025-12-26] 生命週期 × CAI 交叉矩陣
    # 原因：簡化分析內容，避免廠商覺得太複雜
    # ==========================================================================
    # output$lifecycle_cai_matrix <- renderPlotly({
    #   req(values$cai_data)
    #
    #   # ✅ FIX Issue #2: Implement Lifecycle × CAI cross-tabulation heatmap
    #   # Check if tag_017_customer_dynamics exists in the data
    #   if (!"tag_017_customer_dynamics" %in% names(values$cai_data)) {
    #     # Show error message if lifecycle data is missing
    #     plot_ly() %>%
    #       layout(
    #         title = "錯誤：找不到生命週期資料（tag_017_customer_dynamics）",
    #         xaxis = list(visible = FALSE),
    #         yaxis = list(visible = FALSE)
    #       )
    #   } else {
    #     # Create cross-tabulation: lifecycle (rows) × CAI activity segment (columns)
    #     cross_tab_data <- values$cai_data %>%
    #       group_by(tag_017_customer_dynamics, activity_segment) %>%
    #       summarise(count = n(), .groups = "drop") %>%
    #       tidyr::pivot_wider(
    #         names_from = activity_segment,
    #         values_from = count,
    #         values_fill = 0
    #       )
    #
    #     # Define lifecycle order (Chinese labels)
    #     lifecycle_order <- c("新客", "主力客", "半睡客", "睡眠客", "沉睡客")
    #
    #     # Filter and arrange by lifecycle order
    #     cross_tab_data <- cross_tab_data %>%
    #       filter(tag_017_customer_dynamics %in% lifecycle_order) %>%
    #       arrange(match(tag_017_customer_dynamics, lifecycle_order))
    #
    #     # Ensure all CAI activity segments exist
    #     activity_cols <- c("漸趨活躍消費客戶", "穩定消費客戶", "漸趨靜止消費客戶")
    #     for (col in activity_cols) {
    #       if (!col %in% names(cross_tab_data)) {
    #         cross_tab_data[[col]] <- 0
    #       }
    #     }
    #
    #     # Select and order columns
    #     cross_tab_data <- cross_tab_data %>%
    #       select(tag_017_customer_dynamics, all_of(activity_cols))
    #
    #     # Convert to matrix for heatmap (exclude lifecycle column)
    #     matrix_data <- as.matrix(cross_tab_data[, -1])
    #
    #     # ✅ FIX: Use proper hovertemplate to show actual customer counts
    #     plot_ly(
    #       x = activity_cols,
    #       y = cross_tab_data$tag_017_customer_dynamics,
    #       z = matrix_data,
    #       type = "heatmap",
    #       colorscale = list(
    #         c(0, "rgb(255, 255, 255)"),
    #         c(0.3, "rgb(173, 216, 230)"),
    #         c(0.6, "rgb(65, 105, 225)"),
    #         c(1, "rgb(25, 25, 112)")
    #       ),
    #       hovertemplate = paste0(
    #         "<b>生命週期：%{y}</b><br>",
    #         "活躍度分群：%{x}<br>",
    #         "客戶數：<b>%{z}</b><br>",
    #         "<extra></extra>"
    #       ),
    #       showscale = TRUE,
    #       colorbar = list(title = "客戶數")
    #     ) %>%
    #       layout(
    #         xaxis = list(
    #           title = "CAI 活躍度分群",
    #           tickangle = 0
    #         ),
    #         yaxis = list(
    #           title = "生命週期階段",
    #           autorange = "reversed"
    #         ),
    #         title = "",
    #         margin = list(l = 100, b = 100)
    #       )
    #   }
    # })

    # ==========================================================================
    # 第四列：CAI 分布圖表
    # ==========================================================================

    output$cai_distribution <- renderPlotly({
      req(values$cai_data)

      plot_ly(
        data = values$cai_data,
        x = ~cai,
        type = "histogram",
        marker = list(color = "#17a2b8"),
        nbinsx = 30
      ) %>%
        layout(
          xaxis = list(title = "CAI 數值"),
          yaxis = list(title = "客戶數"),
          title = ""
        )
    })

    output$cai_vs_monetary <- renderPlotly({
      req(values$cai_data)

      plot_ly(
        data = values$cai_data,
        x = ~tag_011_rfm_m,
        y = ~cai,
        color = ~activity_segment,
        colors = c("漸趨活躍消費客戶" = "#28a745", "穩定消費客戶" = "#ffc107", "漸趨靜止消費客戶" = "#dc3545"),
        type = "scatter",
        mode = "markers",
        marker = list(size = 8, opacity = 0.6),
        text = ~paste0(
          "客戶ID: ", customer_id, "<br>",
          "購買金額: $", format(round(tag_011_rfm_m, 0), big.mark = ","), "<br>",
          "CAI: ", round(cai, 2), "<br>",
          "分群: ", activity_segment
        ),
        hoverinfo = "text"
      ) %>%
        layout(
          xaxis = list(title = "購買金額 M（元）"),
          yaxis = list(title = "CAI 值"),
          title = "",
          showlegend = TRUE
        )
    })

    # ==========================================================================
    # 第四列：客戶 CAI 詳細資料表
    # ==========================================================================

    output$cai_detail_table <- renderDT({
      req(values$cai_data)

      display_data <- values$cai_data %>%
        select(
          客戶ID = customer_id,
          顧客活躍度係數 = cai,
          顧客活躍度分群 = activity_segment,
          購買次數 = tag_010_rfm_f,
          最近購買天數 = tag_009_rfm_r,
          購買金額 = tag_011_rfm_m
        ) %>%
        arrange(desc(顧客活躍度係數)) %>%
        head(100)

      # 格式化數值
      display_data <- display_data %>%
        mutate(
          顧客活躍度係數 = round(顧客活躍度係數, 2),  # 改為小數點後 2 位
          購買次數 = round(購買次數, 1),
          最近購買天數 = round(最近購買天數, 0),
          購買金額 = round(購買金額, 0)
        )

      datatable(
        display_data,
        options = list(
          pageLength = 20,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        rownames = FALSE
      ) %>%
        formatStyle(
          '顧客活躍度分群',
          backgroundColor = styleEqual(
            c("漸趨活躍消費客戶", "穩定消費客戶", "漸趨靜止消費客戶"),
            c("#d4edda", "#fff3cd", "#f8d7da")
          )
        ) %>%
        formatCurrency('購買金額', currency = "$", digits = 0)
    })

    # ==========================================================================
    # 下載功能
    # ==========================================================================

    # 下載說明 Modal
    observeEvent(input$show_download_warning, {
      showModal(modalDialog(
        title = "📥 下載說明",
        HTML("
          <p><strong>下載提醒：</strong></p>
          <p>若使用 EXCEL 開啟檔案出現亂碼，請用記事本重新開啟檔案，並點選：<strong>使用 BOM 的 UTF-8 儲存檔案</strong>後，再用 EXCEL 重新開啟檔案。</p>
          <hr>
          <p><strong>檔案內容：</strong></p>
          <ul>
            <li>客戶ID</li>
            <li>顧客活躍度係數</li>
            <li>顧客活躍度分群（漸趨活躍消費客戶/穩定消費客戶/漸趨靜止消費客戶）</li>
            <li>購買次數</li>
            <li>最近購買天數</li>
            <li>購買金額</li>
          </ul>
        "),
        easyClose = TRUE,
        footer = modalButton("關閉")
      ))
    })

    # 下載 CAI 資料
    output$download_cai <- downloadHandler(
      filename = function() {
        paste0("customer_activity_index_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(values$cai_data)

        export_data <- values$cai_data %>%
          select(
            客戶ID = customer_id,
            顧客活躍度係數 = cai,
            顧客活躍度分群 = activity_segment,
            購買次數 = tag_010_rfm_f,
            最近購買天數 = tag_009_rfm_r,
            購買金額 = tag_011_rfm_m
          ) %>%
          arrange(desc(顧客活躍度係數))

        write.csv(export_data, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )
  })
}
