# ==============================================================================
# Customer Export Module (客戶標籤輸出表)
# ==============================================================================
# Purpose: 提供完整客戶標籤輸出，可下載 CSV
#
# Output Fields:
#   - 基礎資料: 客戶 ID
#   - RFM: RFM 分數、R、F、M
#   - 活躍度: CAI/顧客活躍度係數、NES
#   - 風險: 流失風險
#   - 交易: 交易次數、平均購買間隔天數、回購時間
#   - 價值: 顧客價值、顧客終身價值 (CLV)
#   - RSV: R風險、S穩定、V價值、客戶類型
#   - 行銷: 行銷方案建議（來自決策表）
#
# Author: Claude AI Assistant
# Date: 2025-12-26
# Version: 1.0
# ==============================================================================

library(shiny)
library(bs4Dash)
library(dplyr)
library(DT)

# ==============================================================================
# UI Function
# ==============================================================================

customerExportUI <- function(id) {
  ns <- NS(id)

  div(
    h3("客戶標籤輸出表"),
    p(class = "text-muted",
      "整合所有客戶分析標籤，提供完整資料輸出。可依需求篩選並下載 CSV 檔案。"),

    # Status panel
    wellPanel(
      style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; border: none;",
      h4("📊 資料狀態", style = "color: white; margin-top: 0;"),
      verbatimTextOutput(ns("status"))
    ),

    # Summary cards
    fluidRow(
      column(3,
        bs4Card(
          title = "📊 總客戶數",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          h2(textOutput(ns("total_customers")), style = "margin: 0;")
        )
      ),
      column(3,
        bs4Card(
          title = "📋 可用標籤數",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          h2(textOutput(ns("total_tags")), style = "margin: 0;")
        )
      ),
      column(3,
        bs4Card(
          title = "💰 平均 CLV",
          status = "success",
          solidHeader = TRUE,
          width = 12,
          h2(textOutput(ns("avg_clv")), style = "margin: 0;")
        )
      ),
      column(3,
        bs4Card(
          title = "⭐ 平均 RFM",
          status = "warning",
          solidHeader = TRUE,
          width = 12,
          h2(textOutput(ns("avg_rfm")), style = "margin: 0;")
        )
      )
    ),

    # Filter and export section
    bs4Card(
      title = "篩選與輸出",
      status = "secondary",
      solidHeader = TRUE,
      width = 12,
      fluidRow(
        column(3,
          selectInput(ns("filter_strategy"), "行銷策略：",
            choices = c("全部" = "all"),
            selected = "all"
          )
        ),
        column(3,
          selectInput(ns("filter_dynamics"), "顧客動態：",
            choices = c("全部" = "all"),
            selected = "all"
          )
        ),
        column(3,
          selectInput(ns("filter_risk"), "靜止風險：",
            choices = c("全部" = "all", "高" = "高", "中" = "中", "低" = "低"),
            selected = "all"
          )
        ),
        column(3,
          div(style = "margin-top: 25px;",
            downloadButton(ns("download_full"), "📥 下載完整標籤表 (CSV)",
              class = "btn-primary btn-block")
          )
        )
      )
    ),

    # Data preview table
    h4("📋 客戶標籤預覽", style = "margin: 30px 0 20px 0;"),
    bs4Card(
      title = NULL,
      status = "white",
      solidHeader = FALSE,
      width = 12,
      p(class = "text-muted", "顯示前 1000 筆資料預覽，完整資料請下載 CSV"),
      DTOutput(ns("export_table"))
    ),

    # Field description
    h4("📖 欄位說明", style = "margin: 30px 0 20px 0;"),
    bs4Card(
      title = NULL,
      status = "secondary",
      solidHeader = FALSE,
      width = 12,
      div(style = "max-height: 300px; overflow-y: auto;",
        tags$table(class = "table table-sm table-striped",
          tags$thead(
            tags$tr(
              tags$th("類別"),
              tags$th("欄位"),
              tags$th("說明")
            )
          ),
          tags$tbody(
            tags$tr(tags$td("基礎"), tags$td("客戶ID"), tags$td("客戶唯一識別碼")),
            tags$tr(tags$td("RFM"), tags$td("顧客價值分數"), tags$td("RFM 綜合分數 (3-15)")),
            tags$tr(tags$td("RFM"), tags$td("最近購買日"), tags$td("最近購買天數分數")),
            tags$tr(tags$td("RFM"), tags$td("購買頻率"), tags$td("購買頻率分數")),
            tags$tr(tags$td("RFM"), tags$td("購買金額"), tags$td("購買金額分數")),
            tags$tr(tags$td("活躍度"), tags$td("顧客活躍度"), tags$td("漸趨靜止戶/穩定消費戶/漸趨活躍戶（ni<4 時顯示 NA）")),
            tags$tr(tags$td("活躍度"), tags$td("活躍度分群"), tags$td("漸趨活躍/穩定/漸趨靜止")),
            tags$tr(tags$td("動態"), tags$td("顧客動態"), tags$td("新客/主力客/瞌睡客/半睡客/沉睡客")),
            tags$tr(tags$td("風險"), tags$td("流失機率"), tags$td("預測流失機率 (0-1)")),
            tags$tr(tags$td("交易"), tags$td("交易次數"), tags$td("歷史購買次數")),
            tags$tr(tags$td("交易"), tags$td("平均購買週期"), tags$td("購買間隔天數（F=1 時顯示 NA）")),
            tags$tr(tags$td("價值"), tags$td("歷史消費"), tags$td("歷史總消費金額")),
            tags$tr(tags$td("價值"), tags$td("顧客終生價值"), tags$td("顧客終身價值（預測未來10年）")),
            tags$tr(tags$td("RSV"), tags$td("顧客風險"), tags$td("靜止風險等級（高/中/低）")),
            tags$tr(tags$td("RSV"), tags$td("顧客交易穩定度"), tags$td("交易穩定度等級（高/中/低）")),
            tags$tr(tags$td("RSV"), tags$td("顧客終身價值等級"), tags$td("顧客價值等級（高/中/低）")),
            tags$tr(tags$td("RSV"), tags$td("客戶類型"), tags$td("RSV 組合分類")),
            tags$tr(tags$td("行銷"), tags$td("主策略"), tags$td("行銷主策略")),
            tags$tr(tags$td("行銷"), tags$td("行銷目的"), tags$td("策略目的")),
            tags$tr(tags$td("行銷"), tags$td("行銷建議"), tags$td("具體行動建議"))
          )
        )
      )
    )
  )
}

# ==============================================================================
# Server Function
# ==============================================================================

customerExportServer <- function(id, marketing_data) {
  moduleServer(id, function(input, output, session) {

    # Reactive values
    values <- reactiveValues(
      export_data = NULL
    )

    # ===========================================================================
    # Process data for export
    # ===========================================================================

    observe({
      req(marketing_data())

      tryCatch({
        df <- marketing_data()

        if (is.null(df) || nrow(df) == 0) {
          values$export_data <- NULL
          return()
        }

        # Prepare export data with all available fields (v2 規格更新)
        export_df <- df %>%
          mutate(
            # Ensure all fields exist
            customer_id = customer_id,

            # RFM fields (欄位名稱更新：RFM分數→顧客價值分數)
            rfm_score = if ("tag_012_rfm_score" %in% names(.)) tag_012_rfm_score else NA,
            r_score = if ("tag_007_r_score" %in% names(.)) tag_007_r_score else
              if ("r_value" %in% names(.)) r_value else NA,
            f_score = if ("tag_008_f_score" %in% names(.)) tag_008_f_score else
              if ("f_value" %in% names(.)) f_value else NA,
            m_score = if ("tag_009_m_score" %in% names(.)) tag_009_m_score else
              if ("m_value" %in% names(.)) m_value else NA,

            # Activity fields (項目 5: ni >= 4 才顯示活躍度)
            ni_val = if ("ni" %in% names(.)) ni else NA,
            cai_raw = if ("cai" %in% names(.)) cai else NA,
            # 顧客活躍度轉為文字描述，且 ni < 4 時顯示 NA
            cai_value = case_when(
              is.na(ni_val) | ni_val < 4 ~ NA_character_,
              !is.na(cai_raw) & cai_raw < -0.2 ~ "漸趨靜止戶",
              !is.na(cai_raw) & cai_raw >= -0.2 & cai_raw <= 0.2 ~ "穩定消費戶",
              !is.na(cai_raw) & cai_raw > 0.2 ~ "漸趨活躍戶",
              TRUE ~ NA_character_
            ),
            activity_segment = if ("activity_segment" %in% names(.)) activity_segment else NA,

            # Customer dynamics
            customer_dynamics = if ("tag_017_customer_dynamics" %in% names(.)) {
              tag_017_customer_dynamics
            } else if ("customer_dynamics" %in% names(.)) {
              customer_dynamics
            } else NA,

            # Risk fields
            churn_prob = if ("nrec_prob" %in% names(.)) nrec_prob else NA,

            # Transaction fields (項目 1: F=1 時平均購買週期顯示 NA)
            transaction_count = if ("ni" %in% names(.)) ni else NA,
            ipt_raw = if ("ipt_mean" %in% names(.)) ipt_mean else
              if ("ipt" %in% names(.)) ipt else NA,
            # F=1（購買次數=1）時，平均購買週期顯示 NA
            avg_purchase_interval = case_when(
              is.na(ni_val) | ni_val <= 1 ~ NA_real_,
              TRUE ~ ipt_raw
            ),

            # Value fields
            total_spent = if ("total_spent" %in% names(.)) total_spent else
              if ("m_value" %in% names(.) && "ni" %in% names(.)) m_value * ni else NA,
            clv = if ("clv" %in% names(.)) clv else
              if ("clv_value" %in% names(.)) clv_value else NA,

            # RSV fields
            r_level = if ("r_level" %in% names(.)) r_level else NA,
            s_level = if ("s_level" %in% names(.)) s_level else NA,
            v_level = if ("v_level" %in% names(.)) v_level else NA,
            customer_type = if ("customer_type" %in% names(.)) customer_type else NA,

            # Marketing fields (from decision module)
            marketing_strategy = if ("marketing_strategy" %in% names(.)) marketing_strategy else NA,
            marketing_purpose = if ("marketing_purpose" %in% names(.)) marketing_purpose else NA,
            marketing_recommendation = if ("marketing_recommendation" %in% names(.)) {
              marketing_recommendation
            } else NA
          ) %>%
          select(
            customer_id,
            rfm_score, r_score, f_score, m_score,
            cai_value, activity_segment,
            customer_dynamics,
            churn_prob,
            transaction_count, avg_purchase_interval,
            total_spent, clv,
            r_level, s_level, v_level, customer_type,
            marketing_strategy, marketing_purpose, marketing_recommendation
          )

        values$export_data <- export_df

      }, error = function(e) {
        message("Customer export error: ", e$message)
        values$export_data <- NULL
      })
    })

    # ===========================================================================
    # Status output
    # ===========================================================================

    output$status <- renderText({
      if (is.null(values$export_data)) {
        return("⏳ 等待資料載入...")
      }

      n_total <- nrow(values$export_data)
      n_cols <- ncol(values$export_data)
      n_with_marketing <- sum(!is.na(values$export_data$marketing_strategy))

      paste0("✅ 已載入 ", format(n_total, big.mark = ","), " 位客戶資料\n",
             "📋 共 ", n_cols, " 個標籤欄位\n",
             "🎯 ", format(n_with_marketing, big.mark = ","), " 位客戶已分配行銷策略")
    })

    # ===========================================================================
    # Summary cards
    # ===========================================================================

    output$total_customers <- renderText({
      req(values$export_data)
      format(nrow(values$export_data), big.mark = ",")
    })

    output$total_tags <- renderText({
      req(values$export_data)
      as.character(ncol(values$export_data))
    })

    output$avg_clv <- renderText({
      req(values$export_data)
      avg <- mean(values$export_data$clv, na.rm = TRUE)
      if (is.na(avg)) return("N/A")
      paste0("$", format(round(avg, 0), big.mark = ","))
    })

    output$avg_rfm <- renderText({
      req(values$export_data)
      avg <- mean(values$export_data$rfm_score, na.rm = TRUE)
      if (is.na(avg)) return("N/A")
      round(avg, 1)
    })

    # ===========================================================================
    # Update filter choices
    # ===========================================================================

    observe({
      req(values$export_data)

      # Marketing strategy choices
      strategies <- unique(values$export_data$marketing_strategy)
      strategies <- strategies[!is.na(strategies)]
      strategy_choices <- c("全部" = "all", setNames(strategies, strategies))
      updateSelectInput(session, "filter_strategy", choices = strategy_choices)

      # Customer dynamics choices
      dynamics <- unique(values$export_data$customer_dynamics)
      dynamics <- dynamics[!is.na(dynamics)]
      dynamics_choices <- c("全部" = "all", setNames(dynamics, dynamics))
      updateSelectInput(session, "filter_dynamics", choices = dynamics_choices)
    })

    # ===========================================================================
    # Filtered data
    # ===========================================================================

    filtered_data <- reactive({
      req(values$export_data)

      df <- values$export_data

      # Apply strategy filter
      if (!is.null(input$filter_strategy) && input$filter_strategy != "all") {
        df <- df %>% filter(marketing_strategy == input$filter_strategy)
      }

      # Apply dynamics filter
      if (!is.null(input$filter_dynamics) && input$filter_dynamics != "all") {
        df <- df %>% filter(customer_dynamics == input$filter_dynamics)
      }

      # Apply risk filter
      if (!is.null(input$filter_risk) && input$filter_risk != "all") {
        df <- df %>% filter(r_level == input$filter_risk)
      }

      df
    })

    # ===========================================================================
    # Data preview table
    # ===========================================================================

    output$export_table <- renderDT({
      req(filtered_data())

      # Limit to 1000 rows for preview
      preview_df <- head(filtered_data(), 1000)

      # Rename columns for display (v2 規格欄位名稱)
      display_df <- preview_df %>%
        select(
          `客戶ID` = customer_id,
          `顧客價值分數` = rfm_score,
          `最近購買日` = r_score,
          `購買頻率` = f_score,
          `購買金額` = m_score,
          `顧客活躍度` = cai_value,
          `活躍度分群` = activity_segment,
          `顧客動態` = customer_dynamics,
          `流失機率` = churn_prob,
          `交易次數` = transaction_count,
          `平均購買週期` = avg_purchase_interval,
          `歷史消費` = total_spent,
          `顧客終生價值` = clv,
          `顧客風險` = r_level,
          `顧客交易穩定度` = s_level,
          `顧客終身價值等級` = v_level,
          `客戶類型` = customer_type,
          `主策略` = marketing_strategy,
          `行銷目的` = marketing_purpose
        )

      datatable(
        display_df,
        options = list(
          pageLength = 20,
          scrollX = TRUE,
          language = list(
            url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Chinese-traditional.json'
          )
        ),
        rownames = FALSE,
        class = 'cell-border stripe hover'
      )
    })

    # ===========================================================================
    # Download handler
    # ===========================================================================

    output$download_full <- downloadHandler(
      filename = function() {
        paste0("客戶標籤完整輸出_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(filtered_data())

        export_df <- filtered_data() %>%
          select(
            `客戶ID` = customer_id,
            `顧客價值分數` = rfm_score,
            `最近購買日` = r_score,
            `購買頻率` = f_score,
            `購買金額` = m_score,
            `顧客活躍度` = cai_value,
            `活躍度分群` = activity_segment,
            `顧客動態` = customer_dynamics,
            `流失機率` = churn_prob,
            `交易次數` = transaction_count,
            `平均購買週期` = avg_purchase_interval,
            `歷史消費金額` = total_spent,
            `顧客終生價值` = clv,
            `顧客風險` = r_level,
            `顧客交易穩定度` = s_level,
            `顧客終身價值等級` = v_level,
            `客戶類型` = customer_type,
            `行銷主策略` = marketing_strategy,
            `行銷目的` = marketing_purpose,
            `行銷建議` = marketing_recommendation
          )

        # UTF-8 BOM for Excel
        con <- file(file, open = "wb", encoding = "UTF-8")
        writeBin(charToRaw('\ufeff'), con)
        write.csv(export_df, con, row.names = FALSE, fileEncoding = "UTF-8")
        close(con)
      }
    )

    # ===========================================================================
    # Return export data
    # ===========================================================================

    return(reactive({ values$export_data }))
  })
}
