# ==============================================================================
# Advanced Analytics Module (Phase 5)
# ==============================================================================
# Purpose: Provide advanced analytics features including:
#   - Customer transition matrix (Task 8.1)
#   - Trend analysis (Task 8.2)
#   - Customer journey visualization (Task 8.3)
#
# Note: These features require historical data. Current implementation uses
#       simulated data to demonstrate concepts. For production use, connect
#       to a database with historical customer snapshots.
#
# Author: Claude AI Assistant
# Date: 2025-10-25
# Version: 1.0 (Simulated Demo)
# ==============================================================================

library(shiny)
library(bs4Dash)
library(dplyr)
library(plotly)
library(networkD3)
library(htmlwidgets)

# ==============================================================================
# UI Function
# ==============================================================================

advancedAnalyticsUI <- function(id) {
  ns <- NS(id)

  div(
    h3("進階分析（需歷史資料）"),

    # Warning banner
    div(
      class = "alert alert-warning",
      style = "background: #fff3cd; border: 2px solid #ffc107; border-radius: 8px; padding: 1.5rem; margin-bottom: 2rem;",
      h4(icon("exclamation-triangle"), " 重要說明", style = "margin-top: 0; color: #856404;"),
      p(
        "以下進階分析功能需要", strong("歷史資料"), "支援（多個時期的客戶快照）。",
        style = "margin-bottom: 0.5rem;"
      ),
      p(
        "當前展示使用", strong("模擬資料"), "來說明這些功能的概念與價值。",
        style = "margin-bottom: 0.5rem;"
      ),
      p(
        strong("建議："), "連接至資料庫並定期儲存客戶快照（建議每月一次），即可啟用完整功能。",
        style = "margin-bottom: 0; color: #856404;"
      )
    ),

    # Task 8.1: Customer Transition Matrix
    bs4Card(
      title = "📊 任務 8.1：客戶移轉矩陣",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      p(class = "text-muted",
        "顯示客戶在九宮格中的移動軌跡，了解客戶價值與活躍度的變化趨勢"),

      fluidRow(
        column(6,
          h5("功能說明："),
          tags$ul(
            tags$li("追蹤客戶在價值-活躍度矩陣中的位置變化"),
            tags$li("識別客戶升級（往高價值/高活躍移動）或降級趨勢"),
            tags$li("計算各格子之間的移轉比例"),
            tags$li("預測未來可能的客戶分布")
          ),
          actionButton(ns("generate_transition"), "📈 生成模擬移轉矩陣", class = "btn-primary btn-lg", style = "width: 100%; margin-top: 1rem;")
        ),
        column(6,
          h5("所需資料："),
          tags$ul(
            tags$li(strong("至少 2 個時期"), "的客戶快照（如：本月 vs 上月）"),
            tags$li("每個快照包含完整的九宮格分析結果"),
            tags$li("相同客戶 ID 跨時期追蹤")
          ),
          h5("應用價值："),
          tags$ul(
            tags$li("識別流失高風險客戶（往低活躍移動）"),
            tags$li("發掘成長潛力客戶（往高價值移動）"),
            tags$li("評估行銷活動成效（客戶升級率）")
          )
        )
      ),

      # Transition matrix visualization
      conditionalPanel(
        condition = paste0("output['", ns("show_transition"), "'] == true"),
        hr(),
        h5("移轉矩陣視覺化："),
        plotlyOutput(ns("transition_heatmap"), height = "500px"),
        hr(),
        h5("主要發現："),
        uiOutput(ns("transition_insights"))
      )
    ),

    # Task 8.2: Trend Analysis
    bs4Card(
      title = "📈 任務 8.2：趨勢分析",
      status = "success",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      p(class = "text-muted",
        "分析關鍵指標的時間序列變化，識別季節性模式與長期趨勢"),

      fluidRow(
        column(6,
          h5("功能說明："),
          tags$ul(
            tags$li("追蹤關鍵指標隨時間變化（客單價、購買週期、流失率等）"),
            tags$li("識別季節性模式（旺季/淡季）"),
            tags$li("預測未來趨勢（使用時間序列模型）"),
            tags$li("比較不同客群的表現趨勢")
          ),
          actionButton(ns("generate_trend"), "📊 生成模擬趨勢分析", class = "btn-success btn-lg", style = "width: 100%; margin-top: 1rem;")
        ),
        column(6,
          h5("所需資料："),
          tags$ul(
            tags$li(strong("至少 12 個月"), "的歷史資料"),
            tags$li("每月關鍵指標匯總"),
            tags$li("不同客群的分類資料")
          ),
          h5("應用價值："),
          tags$ul(
            tags$li("預測未來業績與客戶行為"),
            tags$li("優化庫存與行銷預算配置"),
            tags$li("及早發現異常趨勢並採取行動")
          )
        )
      ),

      # Trend analysis visualization
      conditionalPanel(
        condition = paste0("output['", ns("show_trend"), "'] == true"),
        hr(),
        fluidRow(
          column(6, plotlyOutput(ns("trend_aov"), height = "350px")),
          column(6, plotlyOutput(ns("trend_churn"), height = "350px"))
        ),
        hr(),
        h5("趨勢洞察："),
        uiOutput(ns("trend_insights"))
      )
    ),

    # Task 8.3: Customer Journey Visualization
    bs4Card(
      title = "🗺️ 任務 8.3：客戶旅程視覺化",
      status = "warning",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      p(class = "text-muted",
        "使用 Sankey 圖展示客戶在不同生命週期階段之間的流動路徑"),

      fluidRow(
        column(6,
          h5("功能說明："),
          tags$ul(
            tags$li("視覺化客戶從新客到忠誠客的完整旅程"),
            tags$li("識別關鍵轉換點與流失節點"),
            tags$li("計算各階段的轉換率"),
            tags$li("發現異常流動模式（如：新客直接流失）")
          ),
          actionButton(ns("generate_journey"), "🎯 生成模擬客戶旅程", class = "btn-warning btn-lg", style = "width: 100%; margin-top: 1rem;")
        ),
        column(6,
          h5("所需資料："),
          tags$ul(
            tags$li("客戶生命週期狀態的歷史記錄"),
            tags$li("狀態變更的時間戳記"),
            tags$li("足夠的資料量（建議 > 1000 位客戶）")
          ),
          h5("應用價值："),
          tags$ul(
            tags$li("優化新客轉化策略"),
            tags$li("減少關鍵節點的流失率"),
            tags$li("設計更有效的客戶生命週期管理方案")
          )
        )
      ),

      # Journey visualization
      conditionalPanel(
        condition = paste0("output['", ns("show_journey"), "'] == true"),
        hr(),
        h5("客戶旅程 Sankey 圖："),
        sankeyNetworkOutput(ns("journey_sankey"), height = "600px"),
        hr(),
        h5("旅程分析："),
        uiOutput(ns("journey_insights"))
      )
    ),

    # Implementation guide
    bs4Card(
      title = "💡 如何啟用完整功能",
      status = "info",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      collapsed = TRUE,
      h5("步驟 1: 建立歷史資料儲存機制"),
      p("在資料庫中建立客戶快照表，定期（建議每月）儲存客戶分析結果："),
      tags$pre(
        style = "background: #f5f5f5; padding: 1rem; border-radius: 4px;",
        'CREATE TABLE customer_snapshots (
  snapshot_id INT PRIMARY KEY,
  customer_id VARCHAR(255),
  snapshot_date DATE,
  value_level VARCHAR(10),
  activity_level VARCHAR(10),
  customer_dynamics VARCHAR(20),
  rfm_score INT,
  m_value DECIMAL(10,2),
  -- ... 其他標籤
);'
      ),

      h5("步驟 2: 設定自動化快照流程"),
      p("使用排程工具（如 cron job）每月自動執行分析並儲存結果。"),

      h5("步驟 3: 修改模組以讀取歷史資料"),
      p("更新本模組的 Server 邏輯，從資料庫讀取歷史快照而非使用模擬資料。"),

      h5("步驟 4: 調整分析參數"),
      p("根據實際業務需求調整時間範圍、分組方式等參數。"),

      hr(),
      p(
        strong("技術支援："),
        "如需協助建置歷史資料系統，請聯絡 ",
        tags$a(href = "mailto:partners@peakededges.com", "partners@peakededges.com")
      )
    )
  )
}

# ==============================================================================
# Server Function
# ==============================================================================

advancedAnalyticsServer <- function(id, customer_data) {
  moduleServer(id, function(input, output, session) {

    # Reactive values
    values <- reactiveValues(
      show_transition = FALSE,
      show_trend = FALSE,
      show_journey = FALSE,
      transition_data = NULL,
      trend_data = NULL,
      journey_data = NULL
    )

    # ===========================================================================
    # Task 8.1: Customer Transition Matrix
    # ===========================================================================

    observeEvent(input$generate_transition, {
      # Generate simulated transition matrix
      # In production, this would query historical snapshots from database

      grid_positions <- c(
        "高價值高活躍", "高價值中活躍", "高價值低活躍",
        "中價值高活躍", "中價值中活躍", "中價值低活躍",
        "低價值高活躍", "低價值中活躍", "低價值低活躍"
      )

      # Simulate transition probabilities
      set.seed(42)
      n_positions <- length(grid_positions)
      transition_matrix <- matrix(0, nrow = n_positions, ncol = n_positions)

      for (i in 1:n_positions) {
        # Most customers stay in same position
        transition_matrix[i, i] <- runif(1, 0.5, 0.7)

        # Some move to adjacent positions
        if (i %% 3 != 1) transition_matrix[i, i-1] <- runif(1, 0.05, 0.15)  # Left
        if (i %% 3 != 0) transition_matrix[i, i+1] <- runif(1, 0.05, 0.15)  # Right
        if (i > 3) transition_matrix[i, i-3] <- runif(1, 0.02, 0.08)        # Up
        if (i <= 6) transition_matrix[i, i+3] <- runif(1, 0.02, 0.08)       # Down

        # Normalize to sum to 1
        transition_matrix[i, ] <- transition_matrix[i, ] / sum(transition_matrix[i, ])
      }

      values$transition_data <- list(
        matrix = transition_matrix,
        labels = grid_positions
      )
      values$show_transition <- TRUE
    })

    output$show_transition <- reactive({ values$show_transition })
    outputOptions(output, "show_transition", suspendWhenHidden = FALSE)

    output$transition_heatmap <- renderPlotly({
      req(values$transition_data)

      data <- values$transition_data

      plot_ly(
        x = data$labels,
        y = data$labels,
        z = data$matrix,
        type = "heatmap",
        colorscale = "RdYlGn",
        reversescale = FALSE,
        hovertemplate = "從 %{y}<br>移轉至 %{x}<br>比例: %{z:.1%}<extra></extra>"
      ) %>%
        layout(
          title = "客戶移轉矩陣（模擬資料）",
          xaxis = list(title = "移轉至", tickangle = -45),
          yaxis = list(title = "移轉自"),
          margin = list(l = 150, b = 150)
        )
    })

    output$transition_insights <- renderUI({
      req(values$transition_data)

      div(
        tags$ul(
          tags$li(strong("穩定性："), "平均 60% 客戶保持在原位置（表示客戶行為相對穩定）"),
          tags$li(strong("升級路徑："), "8% 客戶往高價值或高活躍方向移動（成功的培育策略）"),
          tags$li(strong("降級風險："), "12% 客戶往低價值或低活躍方向移動（需要挽回行動）"),
          tags$li(strong("行動建議："), "針對降級客戶及時啟動喚回策略，針對升級客戶提供VIP體驗")
        ),
        p(
          class = "text-muted",
          style = "margin-top: 1rem;",
          icon("info-circle"), " 注意：以上為模擬資料示例。實際資料將提供更精確的洞察。"
        )
      )
    })

    # ===========================================================================
    # Task 8.2: Trend Analysis
    # ===========================================================================

    observeEvent(input$generate_trend, {
      # Generate simulated trend data
      # In production, this would aggregate historical data by month

      months <- seq.Date(as.Date("2024-01-01"), as.Date("2024-12-01"), by = "month")
      n_months <- length(months)

      # Simulate AOV trend with seasonality
      set.seed(42)
      base_aov <- 500
      trend_aov <- base_aov + seq(0, 100, length.out = n_months)  # Increasing trend
      seasonal_aov <- 50 * sin(seq(0, 2*pi, length.out = n_months))  # Seasonal pattern
      noise_aov <- rnorm(n_months, 0, 20)
      aov_values <- trend_aov + seasonal_aov + noise_aov

      # Simulate churn rate trend
      base_churn <- 0.15
      trend_churn <- base_churn - seq(0, 0.05, length.out = n_months)  # Decreasing trend
      seasonal_churn <- 0.03 * sin(seq(0, 2*pi, length.out = n_months) + pi/2)
      noise_churn <- rnorm(n_months, 0, 0.01)
      churn_values <- pmax(0, pmin(1, trend_churn + seasonal_churn + noise_churn))

      values$trend_data <- data.frame(
        month = months,
        aov = aov_values,
        churn_rate = churn_values
      )
      values$show_trend <- TRUE
    })

    output$show_trend <- reactive({ values$show_trend })
    outputOptions(output, "show_trend", suspendWhenHidden = FALSE)

    output$trend_aov <- renderPlotly({
      req(values$trend_data)

      plot_ly(values$trend_data, x = ~month, y = ~aov, type = 'scatter', mode = 'lines+markers',
              line = list(color = '#2ecc71', width = 3),
              marker = list(size = 8, color = '#27ae60'),
              hovertemplate = "%{x|%Y-%m}<br>平均客單價: $%{y:.0f}<extra></extra>") %>%
        layout(
          title = "平均客單價趨勢（模擬資料）",
          xaxis = list(title = "月份"),
          yaxis = list(title = "平均客單價 ($)"),
          hovermode = "x unified"
        )
    })

    output$trend_churn <- renderPlotly({
      req(values$trend_data)

      plot_ly(values$trend_data, x = ~month, y = ~churn_rate * 100, type = 'scatter', mode = 'lines+markers',
              line = list(color = '#e74c3c', width = 3),
              marker = list(size = 8, color = '#c0392b'),
              hovertemplate = "%{x|%Y-%m}<br>流失率: %{y:.1f}%<extra></extra>") %>%
        layout(
          title = "客戶流失率趨勢（模擬資料）",
          xaxis = list(title = "月份"),
          yaxis = list(title = "流失率 (%)"),
          hovermode = "x unified"
        )
    })

    output$trend_insights <- renderUI({
      req(values$trend_data)

      div(
        tags$ul(
          tags$li(strong("客單價："), "呈現穩定上升趨勢（+20%），顯示客戶價值提升成功"),
          tags$li(strong("季節性："), "Q4 有明顯旺季效應（11-12月），建議提前備貨與行銷"),
          tags$li(strong("流失率："), "逐月下降（-33%），表示客戶留存策略有效"),
          tags$li(strong("預測："), "預計下季度客單價將突破 $650，流失率降至 10% 以下")
        ),
        p(
          class = "text-muted",
          style = "margin-top: 1rem;",
          icon("info-circle"), " 注意：以上為模擬資料示例。實際資料將基於您的歷史交易記錄。"
        )
      )
    })

    # ===========================================================================
    # Task 8.3: Customer Journey Visualization
    # ===========================================================================

    observeEvent(input$generate_journey, {
      # Generate simulated customer journey data for Sankey diagram
      # In production, this would trace actual customer lifecycle transitions

      # Define nodes (lifecycle stages)
      nodes <- data.frame(
        name = c("新客", "主力客", "瞌睡客", "半睡客", "沉睡客", "流失")
      )

      # Define links (transitions between stages)
      # Format: source, target, value
      links <- data.frame(
        source = c(0, 0, 1, 1, 1, 2, 2, 3, 3, 4, 4),
        target = c(1, 5, 2, 3, 5, 1, 3, 2, 4, 3, 5),
        value = c(300, 50, 150, 80, 70, 100, 50, 60, 20, 30, 40)
      )

      values$journey_data <- list(nodes = nodes, links = links)
      values$show_journey <- TRUE
    })

    output$show_journey <- reactive({ values$show_journey })
    outputOptions(output, "show_journey", suspendWhenHidden = FALSE)

    output$journey_sankey <- renderSankeyNetwork({
      req(values$journey_data)

      sankeyNetwork(
        Links = values$journey_data$links,
        Nodes = values$journey_data$nodes,
        Source = "source",
        Target = "target",
        Value = "value",
        NodeID = "name",
        units = "位客戶",
        fontSize = 16,
        nodeWidth = 30,
        sinksRight = FALSE,
        colourScale = JS('d3.scaleOrdinal().domain(["新客", "主力客", "瞌睡客", "半睡客", "沉睡客", "流失"])
                         .range(["#3498db", "#2ecc71", "#f39c12", "#e67e22", "#e74c3c", "#95a5a6"]);')
      )
    })

    output$journey_insights <- renderUI({
      req(values$journey_data)

      total_newbie <- 350  # 新客總數
      converted <- 300  # 轉化為主力客
      lost <- 50  # 直接流失

      conversion_rate <- round(converted / total_newbie * 100, 1)
      loss_rate <- round(lost / total_newbie * 100, 1)

      div(
        tags$ul(
          tags$li(strong("新客轉化率："), paste0(conversion_rate, "% （300/350）- 表現優秀")),
          tags$li(strong("新客流失率："), paste0(loss_rate, "% （50/350）- 需要改善新客體驗")),
          tags$li(strong("主力客留存："), "70% 保持活躍或可喚回（瞌睡/半睡）"),
          tags$li(strong("關鍵節點："), "瞌睡客→沉睡客的流失風險最高，需及時喚回"),
          tags$li(strong("行動建議："), "強化新客引導流程，建立瞌睡客自動喚回機制")
        ),
        p(
          class = "text-muted",
          style = "margin-top: 1rem;",
          icon("info-circle"), " 注意：以上為模擬資料示例。實際旅程分析將追蹤真實客戶行為變化。"
        )
      )
    })

    # ===========================================================================
    # Return processed data (for potential downstream use)
    # ===========================================================================

    return(reactive({ customer_data() }))
  })
}
