# ==============================================================================
# Marketing Decision Module (客戶行銷決策表)
# ==============================================================================
# Purpose: 根據多個標籤組合，為每位客戶提供具體行銷策略建議
#
# Decision Logic (按順序判斷，先符合者優先):
#   ① NES ∈ {S1,S2,S3} → 喚醒/回流（防流失）
#   ② CAI 低 + 靜止戶=中/高 → 關係修復（降反感）
#   ③ 靜止戶=高 + CLV 分流 → 成本控管（避免浪費）
#   ④ NES = N（新客）→ Onboarding（建立信任）
#   ⑤ RFM ≤ 5 + CLV 低 → 低成本培養（不放棄）
#   ⑥ 5 < RFM ≤ 10 → 標準培養（依穩定度分級：保守/核心/進階）
#   ⑦ RFM 高 → VIP 維繫（依穩定度分級）
#   ⑧ NES = E0 + CLV 高 → 尊榮維繫（穩定關係）
#   ⑨ 其他/資料不足 → 基礎維繫（周延覆蓋）
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

marketingDecisionUI <- function(id) {
  ns <- NS(id)

  div(
    h3("客戶行銷決策表"),
    p(class = "text-muted",
      "根據顧客標籤組合，自動分配最適行銷策略。決策順序：沉睡喚醒 → 關係修復 → 成本控管 → 新客培養 → 標準/VIP維繫"),

    # Status panel
    wellPanel(
      style = "background: linear-gradient(135deg, #11998e 0%, #38ef7d 100%); color: white; border: none;",
      h4("📊 分析狀態", style = "color: white; margin-top: 0;"),
      verbatimTextOutput(ns("status"))
    ),

    # Summary cards
    fluidRow(
      column(3,
        bs4Card(
          title = "🔔 喚醒/回流",
          status = "danger",
          solidHeader = TRUE,
          width = 12,
          div(
            h2(textOutput(ns("awakening_count")), style = "margin: 0;"),
            p(textOutput(ns("awakening_pct")), class = "text-muted")
          )
        )
      ),
      column(3,
        bs4Card(
          title = "🌱 新客培養",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          div(
            h2(textOutput(ns("onboarding_count")), style = "margin: 0;"),
            p(textOutput(ns("onboarding_pct")), class = "text-muted")
          )
        )
      ),
      column(3,
        bs4Card(
          title = "📈 標準培養",
          status = "warning",
          solidHeader = TRUE,
          width = 12,
          div(
            h2(textOutput(ns("standard_count")), style = "margin: 0;"),
            p(textOutput(ns("standard_pct")), class = "text-muted")
          )
        )
      ),
      column(3,
        bs4Card(
          title = "👑 VIP/尊榮",
          status = "success",
          solidHeader = TRUE,
          width = 12,
          div(
            h2(textOutput(ns("vip_count")), style = "margin: 0;"),
            p(textOutput(ns("vip_pct")), class = "text-muted")
          )
        )
      )
    ),

    # Strategy distribution chart (full width after removing decision logic explanation)
    fluidRow(
      column(12,
        bs4Card(
          title = "行銷策略分布",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          plotlyOutput(ns("strategy_distribution"), height = "400px")
        )
      )
    ),

    # Customer detail table
    h4("📋 客戶行銷策略明細", style = "margin: 30px 0 20px 0;"),
    bs4Card(
      title = NULL,
      status = "white",
      solidHeader = FALSE,
      width = 12,
      fluidRow(
        column(4,
          selectInput(ns("filter_strategy"), "篩選策略：",
            choices = c("全部" = "all"),
            selected = "all"
          )
        ),
        column(4,
          downloadButton(ns("download_decision"), "📥 下載決策表 (CSV)",
            class = "btn-primary", style = "margin-top: 25px;")
        )
      ),
      DTOutput(ns("decision_table"))
    )
  )
}

# ==============================================================================
# Server Function
# ==============================================================================

marketingDecisionServer <- function(id, rsv_data) {
  moduleServer(id, function(input, output, session) {

    # Reactive values
    values <- reactiveValues(
      decision_data = NULL,
      strategy_summary = NULL
    )

    # ===========================================================================
    # Process data and assign strategies
    # ===========================================================================

    observe({
      req(rsv_data())

      tryCatch({
        df <- rsv_data()

        if (is.null(df) || nrow(df) == 0) {
          values$decision_data <- NULL
          return()
        }

        # Apply marketing decision logic
        decision_df <- assign_marketing_strategy(df)
        values$decision_data <- decision_df

        # Calculate strategy summary
        values$strategy_summary <- decision_df %>%
          group_by(marketing_strategy, marketing_purpose) %>%
          summarise(
            customer_count = n(),
            .groups = "drop"
          ) %>%
          mutate(
            percentage = round(customer_count / sum(customer_count) * 100, 1)
          ) %>%
          arrange(desc(customer_count))

      }, error = function(e) {
        message("Marketing decision error: ", e$message)
        values$decision_data <- NULL
      })
    })

    # ===========================================================================
    # Status output
    # ===========================================================================

    output$status <- renderText({
      if (is.null(values$decision_data)) {
        return("⏳ 等待資料載入...")
      }

      n_total <- nrow(values$decision_data)
      n_strategies <- length(unique(values$decision_data$marketing_strategy))

      paste0("✅ 已完成 ", format(n_total, big.mark = ","), " 位客戶的行銷策略分配\n",
             "📊 共使用 ", n_strategies, " 種策略類型")
    })

    # ===========================================================================
    # Summary cards
    # ===========================================================================

    # Awakening/Return (喚醒/回流)
    output$awakening_count <- renderText({
      req(values$decision_data)
      count <- sum(values$decision_data$marketing_strategy == "喚醒/回流", na.rm = TRUE)
      format(count, big.mark = ",")
    })

    output$awakening_pct <- renderText({
      req(values$decision_data)
      count <- sum(values$decision_data$marketing_strategy == "喚醒/回流", na.rm = TRUE)
      pct <- round(count / nrow(values$decision_data) * 100, 1)
      paste0(pct, "% 客戶需要喚醒")
    })

    # Onboarding (新客培養)
    output$onboarding_count <- renderText({
      req(values$decision_data)
      count <- sum(values$decision_data$marketing_strategy == "Onboarding", na.rm = TRUE)
      format(count, big.mark = ",")
    })

    output$onboarding_pct <- renderText({
      req(values$decision_data)
      count <- sum(values$decision_data$marketing_strategy == "Onboarding", na.rm = TRUE)
      pct <- round(count / nrow(values$decision_data) * 100, 1)
      paste0(pct, "% 為新客")
    })

    # Standard nurturing (標準培養)
    output$standard_count <- renderText({
      req(values$decision_data)
      count <- sum(grepl("標準培養|低成本培養", values$decision_data$marketing_strategy), na.rm = TRUE)
      format(count, big.mark = ",")
    })

    output$standard_pct <- renderText({
      req(values$decision_data)
      count <- sum(grepl("標準培養|低成本培養", values$decision_data$marketing_strategy), na.rm = TRUE)
      pct <- round(count / nrow(values$decision_data) * 100, 1)
      paste0(pct, "% 需培養")
    })

    # VIP (VIP/尊榮維繫)
    output$vip_count <- renderText({
      req(values$decision_data)
      count <- sum(grepl("VIP|尊榮", values$decision_data$marketing_strategy), na.rm = TRUE)
      format(count, big.mark = ",")
    })

    output$vip_pct <- renderText({
      req(values$decision_data)
      count <- sum(grepl("VIP|尊榮", values$decision_data$marketing_strategy), na.rm = TRUE)
      pct <- round(count / nrow(values$decision_data) * 100, 1)
      paste0(pct, "% 高價值客戶")
    })

    # ===========================================================================
    # Strategy distribution chart
    # ===========================================================================

    output$strategy_distribution <- renderPlotly({
      req(values$strategy_summary)

      # Define color palette
      color_map <- c(
        "喚醒/回流" = "#dc3545",
        "關係修復" = "#fd7e14",
        "成本控管" = "#6c757d",
        "Onboarding" = "#17a2b8",
        "低成本培養" = "#20c997",
        "標準培養（保守）" = "#ffc107",
        "標準培養（核心）" = "#e0a800",
        "標準培養（進階）" = "#c69500",
        "VIP維繫（穩定度低）" = "#28a745",
        "VIP維繫（穩定度中）" = "#218838",
        "VIP維繫（穩定度高）" = "#1e7e34",
        "尊榮維繫" = "#6f42c1",
        "基礎維繫" = "#adb5bd"
      )

      df <- values$strategy_summary %>%
        mutate(
          color = ifelse(marketing_strategy %in% names(color_map),
                        color_map[marketing_strategy],
                        "#adb5bd")
        )

      plot_ly(df,
              x = ~reorder(marketing_strategy, -customer_count),
              y = ~customer_count,
              type = "bar",
              marker = list(color = ~color),
              text = ~paste0(customer_count, " (", percentage, "%)"),
              textposition = "outside",
              hoverinfo = "text",
              hovertext = ~paste0(
                "策略: ", marketing_strategy, "<br>",
                "目的: ", marketing_purpose, "<br>",
                "客戶數: ", customer_count, "<br>",
                "佔比: ", percentage, "%"
              )) %>%
        layout(
          xaxis = list(title = "", tickangle = -45),
          yaxis = list(title = "客戶數"),
          showlegend = FALSE,
          margin = list(b = 120)
        )
    })

    # ===========================================================================
    # Update filter choices
    # ===========================================================================

    observe({
      req(values$decision_data)

      strategies <- unique(values$decision_data$marketing_strategy)
      choices <- c("全部" = "all", setNames(strategies, strategies))

      updateSelectInput(session, "filter_strategy", choices = choices)
    })

    # ===========================================================================
    # Customer detail table
    # ===========================================================================

    output$decision_table <- renderDT({
      req(values$decision_data)

      df <- values$decision_data

      # Apply filter
      if (!is.null(input$filter_strategy) && input$filter_strategy != "all") {
        df <- df %>% filter(marketing_strategy == input$filter_strategy)
      }

      # Select and rename columns for display (欄位名稱已更新依 v2 規格)
      display_df <- df %>%
        mutate(
          # 顧客活躍度轉為文字描述
          activity_text = case_when(
            !is.na(cai) & cai < -0.2 ~ "漸趨靜止戶",
            !is.na(cai) & cai >= -0.2 & cai <= 0.2 ~ "穩定消費戶",
            !is.na(cai) & cai > 0.2 ~ "漸趨活躍戶",
            TRUE ~ NA_character_
          )
        ) %>%
        select(
          `客戶ID` = customer_id,
          `顧客價值分數` = any_of(c("tag_012_rfm_score", "rfm_score")),
          `顧客動態` = any_of(c("tag_017_customer_dynamics", "customer_dynamics")),
          `顧客活躍度` = activity_text,
          `顧客風險` = any_of(c("r_level")),
          `顧客交易穩定度` = any_of(c("s_level")),
          `顧客價值等級` = any_of(c("v_level")),
          `顧客終生價值` = clv_level,           # 等級（高/中/低）- 基於 P20/P80
          `顧客終生價值分數` = clv_actual,      # 數值 - MAMBA 預測值或 clv_value
          `主策略` = marketing_strategy,
          `行銷目的` = marketing_purpose,
          `行銷建議` = marketing_recommendation
        )

      datatable(
        display_df,
        escape = FALSE,  # 允許 HTML 渲染（行銷建議的換行和粗體）
        options = list(
          pageLength = 20,
          scrollX = TRUE,
          order = list(list(8, 'asc')),  # Sort by strategy
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Chinese-traditional.json')
        ),
        rownames = FALSE,
        class = 'cell-border stripe hover'
      )
    })

    # ===========================================================================
    # Download handler
    # ===========================================================================

    output$download_decision <- downloadHandler(
      filename = function() {
        paste0("客戶行銷決策表_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(values$decision_data)

        export_df <- values$decision_data %>%
          mutate(
            # 顧客活躍度轉為文字描述
            activity_text = case_when(
              !is.na(cai) & cai < -0.2 ~ "漸趨靜止戶",
              !is.na(cai) & cai >= -0.2 & cai <= 0.2 ~ "穩定消費戶",
              !is.na(cai) & cai > 0.2 ~ "漸趨活躍戶",
              TRUE ~ NA_character_
            )
          ) %>%
          select(
            `客戶ID` = customer_id,
            `顧客價值分數` = any_of(c("tag_012_rfm_score", "rfm_score")),
            `顧客動態` = any_of(c("tag_017_customer_dynamics", "customer_dynamics")),
            `顧客活躍度` = activity_text,
            `顧客風險` = any_of(c("r_level")),
            `顧客交易穩定度` = any_of(c("s_level")),
            `顧客價值等級` = any_of(c("v_level")),
            `顧客終生價值` = clv_level,           # 等級（高/中/低）- 基於 P20/P80
            `顧客終生價值分數` = clv_actual,      # 數值 - MAMBA 預測值或 clv_value
            `主策略` = marketing_strategy,
            `行銷目的` = marketing_purpose,
            `行銷建議` = marketing_recommendation
          ) %>%
          mutate(
            # 移除 HTML 標籤，保留純文字（CSV 下載用）
            `行銷建議` = gsub("<br>", "\n", `行銷建議`),           # 將 <br> 替換為換行符
            `行銷建議` = gsub("</?strong>", "", `行銷建議`)        # 移除 <strong> 和 </strong>
          )

        # UTF-8 BOM for Excel
        con <- file(file, open = "wb", encoding = "UTF-8")
        writeBin(charToRaw('\ufeff'), con)
        write.csv(export_df, con, row.names = FALSE, fileEncoding = "UTF-8")
        close(con)
      }
    )

    # ===========================================================================
    # Return processed data
    # ===========================================================================

    return(reactive({ values$decision_data }))
  })
}

# ==============================================================================
# Marketing Strategy Decision Engine
# ==============================================================================

assign_marketing_strategy <- function(df) {
  # First, ensure all required columns exist with default values
  if (!"tag_017_customer_dynamics" %in% names(df)) {
    df$tag_017_customer_dynamics <- NA_character_
  }
  if (!"cai" %in% names(df)) {
    df$cai <- NA_real_
  }
  if (!"activity_segment" %in% names(df)) {
    df$activity_segment <- NA_character_
  }
  if (!"r_level" %in% names(df)) {
    df$r_level <- "中"
  }
  if (!"s_level" %in% names(df)) {
    df$s_level <- "中"
  }
  if (!"v_level" %in% names(df)) {
    df$v_level <- "中"
  }
  if (!"tag_012_rfm_score" %in% names(df)) {
    df$tag_012_rfm_score <- NA_real_
  }

  # Step 1: Calculate clv_actual (numeric CLV value)
  df <- df %>%
    mutate(
      # NES status (from customer_dynamics)
      # 注意：tag_017_customer_dynamics 的值來自 calculate_customer_tags.R
      # 標準中文值：新客、主力客、睡眠客、半睡客、沉睡客
      nes_status = case_when(
        tag_017_customer_dynamics %in% c("沉睡客", "dormant") ~ "S3",
        tag_017_customer_dynamics %in% c("半睡客", "half_sleepy", "half_sleep") ~ "S2",
        tag_017_customer_dynamics %in% c("睡眠客", "瞌睡客", "sleepy", "dozing") ~ "S1",  # 修正：加入「睡眠客」
        tag_017_customer_dynamics %in% c("新客", "newbie") ~ "N",
        tag_017_customer_dynamics %in% c("主力客", "active") ~ "E0",
        TRUE ~ "Unknown"
      ),

      # CAI status (low if CAI < -0.2 or activity_segment is 漸趨靜止)
      cai_low = case_when(
        !is.na(cai) & cai < -0.2 ~ TRUE,
        !is.na(activity_segment) & activity_segment == "漸趨靜止消費客戶" ~ TRUE,
        TRUE ~ FALSE
      ),

      # RFM score
      rfm_score_val = tag_012_rfm_score,

      # CLV actual value (優先使用 MAMBA 預測值 clv，其次 clv_value)
      clv_actual = case_when(
        !is.na(clv) ~ clv,
        !is.na(clv_value) ~ clv_value,
        TRUE ~ NA_real_
      )
    )

  # Step 2: Calculate CLV level using P20/P80 quantiles (80/20 rule)
  # If all clv_actual are NA, fallback to v_level
  if (all(is.na(df$clv_actual))) {
    # Fallback: use v_level if no CLV data available
    df <- df %>%
      mutate(
        clv_level = case_when(
          v_level == "高" ~ "高",
          v_level == "低" ~ "低",
          TRUE ~ "中"
        )
      )
  } else {
    # Dynamic calculation: P20/P80 quantiles
    clv_p20 <- quantile(df$clv_actual, 0.2, na.rm = TRUE)
    clv_p80 <- quantile(df$clv_actual, 0.8, na.rm = TRUE)

    df <- df %>%
      mutate(
        clv_level = case_when(
          is.na(clv_actual) ~ "中",           # NA values → 中
          clv_actual >= clv_p80 ~ "高",        # Top 20% → 高
          clv_actual >= clv_p20 ~ "中",        # Middle 60% → 中
          TRUE ~ "低"                          # Bottom 20% → 低
        )
      )
  }

  # Apply decision logic in priority order
  df <- df %>%
    mutate(
      # Decision logic - sequential evaluation
      marketing_strategy = case_when(
        # ① NES ∈ {S1,S2,S3} → 喚醒/回流
        nes_status %in% c("S1", "S2", "S3") ~ "喚醒/回流",

        # ② CAI 低 + 靜止戶=中/高 → 關係修復
        cai_low & r_level %in% c("中", "高") ~ "關係修復",

        # ③ 靜止戶=高 + CLV 分流 → 成本控管
        r_level == "高" ~ "成本控管",

        # ④ NES = N（新客）→ Onboarding
        nes_status == "N" ~ "Onboarding",

        # ⑤ RFM ≤ 5 + CLV 低 → 低成本培養
        !is.na(rfm_score_val) & rfm_score_val <= 5 & clv_level == "低" ~ "低成本培養",

        # ⑥ 5 < RFM ≤ 10 → 標準培養（依穩定度分級）
        !is.na(rfm_score_val) & rfm_score_val > 5 & rfm_score_val <= 10 & s_level == "低" ~ "標準培養（保守）",
        !is.na(rfm_score_val) & rfm_score_val > 5 & rfm_score_val <= 10 & s_level == "中" ~ "標準培養（核心）",
        !is.na(rfm_score_val) & rfm_score_val > 5 & rfm_score_val <= 10 & s_level == "高" ~ "標準培養（進階）",

        # ⑦ RFM > 10 → VIP 維繫（依穩定度分級）
        !is.na(rfm_score_val) & rfm_score_val > 10 & s_level == "低" ~ "VIP維繫（穩定度低）",
        !is.na(rfm_score_val) & rfm_score_val > 10 & s_level == "中" ~ "VIP維繫（穩定度中）",
        !is.na(rfm_score_val) & rfm_score_val > 10 & s_level == "高" ~ "VIP維繫（穩定度高）",

        # ⑧ NES = E0 + CLV 高 → 尊榮維繫
        nes_status == "E0" & clv_level == "高" ~ "尊榮維繫",

        # ⑨ 其他/資料不足 → 基礎維繫
        TRUE ~ "基礎維繫"
      ),

      # Marketing purpose
      marketing_purpose = case_when(
        marketing_strategy == "喚醒/回流" ~ "防流失",
        marketing_strategy == "關係修復" ~ "降反感",
        marketing_strategy == "成本控管" ~ "避免浪費",
        marketing_strategy == "Onboarding" ~ "建立信任",
        marketing_strategy == "低成本培養" ~ "不放棄",
        grepl("標準培養（保守）", marketing_strategy) ~ "建立穩定節奏",
        grepl("標準培養（核心）", marketing_strategy) ~ "擴展需求",
        grepl("標準培養（進階）", marketing_strategy) ~ "提升客單",
        grepl("VIP維繫（穩定度低）", marketing_strategy) ~ "提升穩定度",
        grepl("VIP維繫（穩定度中）", marketing_strategy) ~ "維持滿意度",
        grepl("VIP維繫（穩定度高）", marketing_strategy) ~ "深化忠誠",
        marketing_strategy == "尊榮維繫" ~ "穩定關係",
        TRUE ~ "周延覆蓋"
      ),

      # Marketing recommendation (詳細版 - HTML 格式 - 來源：PDF 第 2-6 頁 - 2025-12-29 更新)
      marketing_recommendation = case_when(
        # ① 喚醒/回流 - 防流失
        marketing_strategy == "喚醒/回流" ~ "1. 定期發送關懷訊息，附上個人化優惠<br>2. S1 給小額折扣<br>3. S3 提供更大折扣或專屬好禮，增加誘因<br>4. 利用廣告再行銷觸及長期未互動者<br>5. 在社群/網路上重新進入他們視野，引導回購",

        # ② 關係修復 - 降反感
        marketing_strategy == "關係修復" ~ "1. 根據過往喜好推薦新品資訊或限時折扣券，引發興趣<br>2. 透過 EDM、APP 推播等自動化工具發送專屬優惠<br>3. 根據最近瀏覽的品類提供優惠，促進再購<br>4. 主動調查滿意度或收集回饋，找出活躍下滑原因並對症優化，讓顧客感受到重視而繼續與品牌互動",

        # ③ 成本控管 - 避免浪費
        marketing_strategy == "成本控管" ~ "1. <strong>資深客服或經理</strong>以個人名義主動聯繫（如 Email 致謝），表達對其長期支持的感謝與關懷，同時提供量身定制的專屬優惠（如根據之前購買的高價產品提供升級配套折扣、延長保固或會員升等禮遇），讓顧客感到被重視而願意回流<br>2. 如有必要可安排<strong>線上一對一服務或諮詢</strong>，傾聽其需求變化，以消除停止購買的障礙",

        # ④ Onboarding - 建立信任
        marketing_strategy == "Onboarding" ~ "1. <strong>歡迎新客計畫</strong>：顧客首購後立即跟進，透過 Email 或簡訊感謝購買，並詢問使用體驗是否滿意，邀請給予建議，以表重視<br>2. 提供<strong>新手專屬禮遇</strong>，如下次購物折扣券、免運優惠或新會員積分禮，鼓勵再購<br>3. 搭配品牌故事、熱銷商品介紹、使用教學等內容推送，增進新客對產品了解<br>4. 同時建立 FAQ、自動客服機器人等支援，隨時解答新客疑問，降低其購買門檻並增強信任感",

        # ⑤ 低成本培養 - 不放棄
        marketing_strategy == "低成本培養" ~ "1. 善用<strong>行銷自動化工具</strong>對這些客戶進行批量且精準的溝通，如定期發送促銷電子報，重點宣傳折扣商品、清倉特賣優惠資訊<br>2. 提供<strong>小額累積獎勵機制</strong>，如每次購買可累積積分兌換小禮物，誘導他們多次消費以獲得實惠<br>3. 對於一段時間內毫無互動的，可以透過溫馨提醒訊息或特惠券激發其興趣（如「回來看看新品，享下單優惠」），但控制頻率，避免過度打擾<br>4. 針對其中購買反應熱絡者，後續可升級為重點經營對象，加大投入<br>5. 反之，對長期無回響者逐漸減少行銷資源投入，把重心放在更有價值客群",

        # ⑥-1 標準培養（保守）- 建立穩定節奏
        grepl("標準培養（保守）", marketing_strategy) ~ "1. <strong>建立定期溝通機制</strong>：在每次購買後的預估用畢日期前，透過 Email/簡訊提醒可能需要再次購買，附上相關產品推薦或優惠（如「您上次購買的商品可能快用完，現在續購享 95 折」）<br>2. <strong>提供預訂/訂閱選項</strong>：若產品屬於消耗品，可提議加入定期訂購計畫，以優惠價格按周期自動配送，讓顧客省心又不間斷使用品牌產品<br>3. 當發現該客戶已超過平均購買周期未回購，及早給予關懷，例如發送專屬折扣券或邀請參與會員活動，提醒其回歸<br>4. 在日常經營中強調產品品質與服務一致性，消除其顧慮，以增加每次購買後繼續選擇本品牌的信心",

        # ⑥-2 標準培養（核心）- 擴展需求
        grepl("標準培養（核心）", marketing_strategy) ~ "1. <strong>需求預測與提醒</strong>：分析此客群購買週期和季節性趨勢，在預計其將產生需求時主動發送提醒訊息或再行銷廣告，如「您可能快沒 XX 了，現在補貨享優惠」<br>2. <strong>優惠驅動</strong>：提供限時折扣或免運券來降低其購買猶豫（如季節性需求旺季前的早鳥優惠）。同時透過內容行銷強調產品價值和使用好處，減少純比價心態<br>3. 對於尚未購買的新產品線，根據他們已購產品的使用場景，推薦相關產品以滿足更多需求，增加額外銷售機會",

        # ⑥-3 標準培養（進階）- 提升客單
        grepl("標準培養（進階）", marketing_strategy) ~ "1. <strong>VIP 升級誘因</strong>：設計分級會員制度，向這些穩定熟客宣傳升級好處，如成為高級會員即可享有專屬折扣、生日禮、優先客服<br>2. 透過提示其距離升級僅差幾次購買或金額，激發其追求更高會員等級的動機<br>3. <strong>產品/服務加值</strong>：推薦他們體驗更高價值的產品或服務，如高階版本、延伸保固、配套課程等，並提供試用或優惠價以降低嘗試門檻<br>4. 對經常購買同類產品的，建議加入訂閱計畫（如每月自動寄送）並給予訂閱折扣，鎖定其長期消費<br>5. 在交流中強調「您是我們的忠實顧客，值得更尊榮待遇」，以情感連結促進升級意願，讓顧客感受到升級不只是消費更多，而是成為品牌圈子中的核心成員",

        # ⑦-1 VIP維繫（穩定度低）- 提升穩定度
        grepl("VIP維繫（穩定度低）", marketing_strategy) ~ "1. <strong>建立專屬會員制</strong>：為該客群打造高級會員或 VIP 俱樂部，提供差異化待遇（如更高現金回饋比例、專屬折扣碼、獨享新品預購權）來提高黏著度<br>2. <strong>個性化再行銷</strong>：根據他們過去購買的大額品項，定期推薦相關的高端新品或配套服務，透過 EDM、社群精準廣告保持品牌曝光，讓顧客在需要時首先想到您<br>3. <strong>體驗關懷</strong>：不定期對該客群進行服務回訪，瞭解上次購物體驗是否滿意，提供問題解決通道，消除他們不穩定的原因。同時在重大節日或其購物周年送上個性化祝福或小禮，強化情感連結，促使其長期留存",

        # ⑦-2 VIP維繫（穩定度中）- 維持滿意度
        grepl("VIP維繫（穩定度中）", marketing_strategy) ~ "1. <strong>專屬關懷</strong>：針對這些新晉的高價值顧客，提供特別的關注與禮遇。如首次達到高消費額度後，由客服經理發出感謝信或致電致謝（可用 Email 避免電話打擾），並提供專屬折扣券或邀請其加入高端會員計畫<br>2. <strong>體驗式行銷</strong>：邀請其參與品牌舉辦的體驗活動或新品發表會，讓他們感受成為貴賓的榮耀，深化情感連結。同時，定期追蹤其購買趨勢，適時給予激勵（如「再購買一次即可升級 VIP」或累積滿額贈），促使其保持頻繁購買",

        # ⑦-3 VIP維繫（穩定度高）- 深化忠誠
        grepl("VIP維繫（穩定度高）", marketing_strategy) ~ "1. <strong>VIP 尊享服務</strong>：為核心忠誠客提供高規格的專屬服務，如專屬客服熱線、生日/節日的定制禮品或驚喜、會員日雙倍積分等<br>2. <strong>參與感計畫</strong>：邀請此客群參與品牌內部的諮詢委員會或新品試用小組，讓他們對品牌發展有發言權。如提前試用新品並反饋意見，或參與限定的粉絲見面活動，增強他們的歸屬感與榮譽感<br>3. <strong>鼓勵口碑</strong>：透過會員推薦獎勵計畫激勵他們介紹朋友（如成功推薦新客可獲高額積分或現金券），利用他們對品牌的熱愛帶動口碑擴散。同時定期在公開場合（社群媒體、官網）感謝並表揚這些忠誠客戶，如故事分享或會員榜單，強化他們與品牌共同成就的認同感",

        # ⑧ 尊榮維繫 - 穩定關係
        marketing_strategy == "尊榮維繫" ~ "1. <strong>專屬會員維護</strong>：為主力客提供定制化的頂級會員福利，如免運永久有效、專屬客服經理、優先知曉/預購新品等，讓他們享受便利與尊貴，但避免頻繁的促銷信息轟炸<br>2. <strong>定期滿意度回訪</strong>：透過問卷或 VIP 專線定期了解他們的需求變化和滿意度，並收集 NPS（淨推薦值）反饋，一方面快速響應其問題提升服務，另方面衡量他們願意推薦的程度以判斷忠誠健康度<br>3. <strong>節奏式驚喜</strong>：不定期在重要時刻（如會員周年、生日）給予驚喜回饋（優惠券或禮品），既表心意又不破壞其正常消費節奏。同時，在新品上市等需要推廣時，採取 soft sell 方式（例如邀請試用而非直接促銷），確保主力客群感受到尊重與體貼，持續留在品牌懷抱",

        # ⑨ 基礎維繫 - 周延覆蓋
        TRUE ~ "1. 定期品牌內容／更新<br>2. <strong>數據補強</strong>：利用站內外活動鼓勵此類顧客提供資訊，如社群投票、有獎調查問卷、官網會員檔案完善（完成可獲小優惠）等方式蒐集偏好與聯絡資料<br>3. <strong>廣泛觸及</strong>：在資料不足階段，採取內容導向的廣泛溝通，如定期發送電子報介紹品牌故事、新品資訊，或在社群推送有趣內容，觀察其點擊、瀏覽行為，以獲知興趣方向<br>4. <strong>行為誘導</strong>：設計新客體驗活動，如第一次購物送禮或免運，以降低嘗試門檻，誘導未購買者下單，從購買記錄中獲取關鍵 RFM 資訊。總之，先透過多管道互動把資料「餵足」、看清客質，再將其導入適合的精準行銷路徑，實現從觀察期到經營期的平滑過渡"
      )
    )

  return(df)
}
