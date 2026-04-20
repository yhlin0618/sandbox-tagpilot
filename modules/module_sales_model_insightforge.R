################################################################################
# InsightForge 銷售模型模組 - Poisson 回歸分析
################################################################################

library(DT)
library(dplyr)
library(plotly)

# NULL 合併運算子
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

#' Sales Model Module - UI
sales_model_insightforgeModuleUI <- function(id, module_config = NULL, lang_texts = NULL, brand_choices = NULL) {
  ns <- NS(id)

  # 從 lang_texts 參數獲取語言，而非從全域環境
  # 這確保 UI 使用與傳入參數一致的語言
  current_language <- if (!is.null(lang_texts) && !is.null(lang_texts$language)) {
    lang_texts$language
  } else if (exists("get_current_language") && is.function(get_current_language)) {
    get_current_language()
  } else {
    "zh_TW"
  }

  # 獲取模組語言內容（優先使用傳入的 lang_texts）
  module_lang_content <- lang_texts %||% list(language = current_language)

  cat("🌍 [Sales Model UI] 當前語言:", current_language, "\n")
  cat("  📋 [Sales Model UI] Received brand_choices:", if(is.null(brand_choices)) "NULL" else paste(brand_choices, collapse = ", "), "\n")

  # 獲取當前 hints - 使用與 lang_texts 一致的語言
  hints_df <- if (exists("hints_df") && is.function(hints_df)) {
    # 如果 hints_df 是 reactive，取其值
    tryCatch({
      isolate(hints_df())
    }, error = function(e) {
      cat("❌ [Sales Model UI] hints_df reactive 讀取失敗:", e$message, "\n")
      NULL
    })
  } else if (exists("load_hints") && is.function(load_hints)) {
    tryCatch({
      load_hints(language = current_language)
    }, error = function(e) {
      cat("❌ [Sales Model UI] hints 載入失敗:", e$message, "\n")
      NULL
    })
  } else {
    NULL
  }

  # 使用安全文字取得器
  safe_get_text_func <- if (exists("safe_get_text") && is.function(safe_get_text)) {
    safe_get_text
  } else {
    function(content, path, default) default
  }

  tab1_title <- safe_get_text_func(module_lang_content, "tabs.mean_scores", "評分均值")
  tab2_title <- safe_get_text_func(module_lang_content, "tabs.sales_model", "銷售模型")
  tab3_title <- safe_get_text_func(module_lang_content, "tabs.marketing_strategy", "個性化行銷策略")

  cat("🎨 [Sales Model UI] Tab 标题:", tab1_title, "|", tab2_title, "|", tab3_title, "\n")

  tabBox(
    id = ns("sales_tabs"),
    width = 12,

    # Tab 1: 評分均值
    tabPanel(
      tab1_title,
      icon = icon("chart-bar"),
      br(),
      h5(safe_get_text_func(module_lang_content, "mean_scores.title", "各 Variation 的屬性評分平均值"),
         if (exists("add_info_icon")) add_info_icon("variation_mean_tbl", hints_df)),
      DTOutput(ns("variation_mean_tbl"))
    ),

    # Tab 2: 銷售模型
    tabPanel(
      tab2_title,
      icon = icon("chart-line"),
      br(),
      h5(safe_get_text_func(module_lang_content, "sales_model.title", "銷售迴歸分析")),

      # 合併資料預覽
      h6(safe_get_text_func(module_lang_content, "sales_model.merged_data_preview", "合併後的資料預覽：")),
      DTOutput(ns("merged_data_tbl")),
      br(),

      # 執行按鈕
      if (exists("add_hint")) {
        add_hint(
          actionButton(ns("run_poisson"),
                      safe_get_text_func(module_lang_content, "buttons.run_analysis", "執行分析"),
                      class = "btn-warning"),
          "run_poisson", hints_df
        )
      } else {
        actionButton(ns("run_poisson"),
                    safe_get_text_func(module_lang_content, "buttons.run_analysis", "執行分析"),
                    class = "btn-warning")
      },
      br(), br(),

      # 分析結果
      h5(safe_get_text_func(module_lang_content, "results.title", "📊 銷售模型分析結果"),
         if (exists("add_info_icon")) add_info_icon("result_interpretation", hints_df)),
      DTOutput(ns("poisson_results")),
      br(),
      h5(safe_get_text_func(module_lang_content, "results.visualization", "📈 邊際效應視覺化"),
         if (exists("add_info_icon")) add_info_icon("marginal_effect", hints_df)),
      plotlyOutput(ns("poisson_plot"), height = "400px")
    ),

    # Tab 3: 個性化行銷策略
    tabPanel(
      tab3_title,
      icon = icon("user-tag"),
      br(),
      h5(safe_get_text_func(module_lang_content, "strategy.title", "根據分析結果產生行銷策略")),

      fluidRow(
        column(6,
          selectInput(ns("strategy_brand"),
                     safe_get_text_func(module_lang_content, "strategy.select_brand", "選擇品牌："),
                     choices = brand_choices)
        ),
        column(6,
          actionButton(ns("generate_strategy"),
                      safe_get_text_func(module_lang_content, "buttons.generate_strategy", "產生行銷策略"),
                      class = "btn-success",
                      style = "margin-top: 25px;")
        )
      ),

      br(),
      uiOutput(ns("marketing_strategy"))
    )
  )
}

#' Sales Model Module - Server
salesModelModuleServer <- function(id, scored_data = NULL, sales_data = NULL, prompts_df = NULL, module_config = NULL, lang_texts = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ============================================
    # 🌍 統一語言管理
    # ============================================
    # 使用全域語言管理系統（已在主 app 中載入）

    # 創建模組語言更新器
    if (exists("create_module_language_updater") && is.function(create_module_language_updater)) {
      create_module_language_updater("sales_model_insightforge", function(language, content) {
        cat("✅ [Sales Model Module] 語言更新:", language, "\n")
        # 更新模組級別的語言文字
        lang_texts <<- content

        # 強制重新渲染 UI 元素
        if (exists("session") && !is.null(session)) {
          # 觸發 UI 更新
          session$sendCustomMessage("module_language_updated", list(
            module = "sales_model_insightforge",
            language = language
          ))
        }
      })
    } else {
      # 後備語言變化偵測
      observe({
        if (exists("global_lang_content") && is.function(global_lang_content)) {
          current_lang_content <- global_lang_content()
          if (!is.null(current_lang_content) &&
              !is.null(current_lang_content$content$modules$sales_model)) {
            lang_texts <<- current_lang_content$content$modules$sales_model
            lang_texts$language <<- current_lang_content$language

            cat("🔄 [Sales Model Module] 後備語言更新:", current_lang_content$language, "\n")
          }
        }
      })
    }

    # 獲取當前語言
    current_language <- reactive({
      # 直接使用全域語言狀態
      if (exists("global_lang_content", envir = .GlobalEnv) && is.function(get("global_lang_content", envir = .GlobalEnv))) {
        lang_state <- get("global_lang_content", envir = .GlobalEnv)()
        if (!is.null(lang_state) && !is.null(lang_state$language)) {
          return(lang_state$language)
        }
      }

      # 備用方式
      if (exists("get_current_language", envir = .GlobalEnv) && is.function(get("get_current_language", envir = .GlobalEnv))) {
        return(get("get_current_language", envir = .GlobalEnv)())
      }

      return("zh_TW")  # 最終備用
    })

    # 載入 Prompt 管理系統（使用全域功能）
    prompts_df <- reactive({
      if (exists("load_prompts", envir = .GlobalEnv) && is.function(get("load_prompts", envir = .GlobalEnv))) {
        current_lang <- current_language()
        return(get("load_prompts", envir = .GlobalEnv)(language = current_lang))
      } else {
        return(NULL)
      }
    })

    # 取得模組語言內容（使用全域功能）
    module_lang_content <- reactive({
      if (exists("get_module_language_content", envir = .GlobalEnv) && is.function(get("get_module_language_content", envir = .GlobalEnv))) {
        return(get("get_module_language_content", envir = .GlobalEnv)("sales_model_insightforge", current_language()))
      } else {
        return(NULL)
      }
    })

    # 載入 Poisson regression 函數
    source("scripts/global_scripts/07_models/Poisson_Regression.R")

    # 反應式變數
    mean_data_rv <- reactiveVal(NULL)
    merged_data_rv <- reactiveVal(NULL)
    poisson_results_rv <- reactiveVal(NULL)

    # 計算評分均值
    observe({
      # Listen to UI update trigger to re-populate dropdown after UI recreation
      if (exists("module_ui_update_trigger") && is.function(module_ui_update_trigger)) {
        trigger_val <- module_ui_update_trigger()
        cat("🔍 [Sales Model] observe triggered (UI trigger:", trigger_val, ")\n")
      } else {
        cat("🔍 [Sales Model] observe triggered\n")
      }

      score_data <- scored_data()
      cat("  📊 [Sales Model] scored_data() returned:", if(is.null(score_data)) "NULL" else paste(nrow(score_data), "rows"), "\n")

      req(score_data)

      if (!"Variation" %in% names(score_data)) {
        cat("  ❌ [Sales Model] ERROR: No 'Variation' column in scored_data\n")
        cat("  📋 [Sales Model] Available columns:", paste(names(score_data), collapse = ", "), "\n")
        return()
      }

      cat("  ✅ [Sales Model] Processing scored data with", nrow(score_data), "rows\n")

      # 獲取評分欄位（除了 Variation 外的所有數值欄位）
      score_cols <- names(score_data)[!names(score_data) %in% "Variation"]
      score_cols <- score_cols[sapply(score_data[score_cols], is.numeric)]

      if (length(score_cols) > 0) {
        mean_data <- score_data %>%
          select(Variation, all_of(score_cols)) %>%
          group_by(Variation) %>%
          summarise(across(all_of(score_cols), \(x) mean(x, na.rm = TRUE))) %>%
          ungroup()

        mean_data_rv(mean_data)

        # 顯示評分均值表
        output$variation_mean_tbl <- renderDT(
          mean_data,
          selection = "none",
          options = list(
            pageLength = 10,
            scrollX = TRUE
          )
        )

        # 更新品牌選擇
        brand_choices <- unique(mean_data$Variation)
        cat("  🔄 [Sales Model] 更新 strategy_brand 選項:", paste(brand_choices, collapse = ", "), "\n")
        updateSelectInput(session, "strategy_brand",
                         choices = brand_choices)
      }
    })

    # 合併評分與銷售資料
    observe({
      req(mean_data_rv(), sales_data())

      mean_data <- mean_data_rv()
      sales_df <- sales_data()

      # 智能合併評分與銷售資料
      eval_variations <- unique(mean_data$Variation)
      sales_variations <- unique(sales_df$Variation)
      common_vars <- intersect(eval_variations, sales_variations)

      # 使用安全的语言文本获取
      texts <- module_lang_content()
      safe_get_text_func <- if (exists("safe_get_text") && is.function(safe_get_text)) {
        safe_get_text
      } else {
        function(content, path, default) default
      }

      cat(safe_get_text_func(texts, "console.data_check", "📊 資料對應檢查："), "\n")

      score_var_text <- safe_get_text_func(texts, "console.score_variations", "評分變體數量: {count}")
      cat(gsub("\\{count\\}", length(eval_variations), score_var_text), "\n")

      sales_var_text <- safe_get_text_func(texts, "console.sales_variations", "銷售變體數量: {count}")
      cat(gsub("\\{count\\}", length(sales_variations), sales_var_text), "\n")

      direct_match_text <- safe_get_text_func(texts, "console.direct_match", "直接匹配數量: {count}")
      cat(gsub("\\{count\\}", length(common_vars), direct_match_text), "\n")

      if (length(common_vars) > 0) {
        # 直接匹配的情況
        cat(safe_get_text_func(texts, "console.using_direct_match", "✅ 使用直接匹配策略"), "\n")
        merged_data <- sales_df %>%
          filter(Variation %in% common_vars) %>%
          left_join(mean_data, by = "Variation") %>%
          filter(!is.na(Sales))

        # 不需要添加匹配資訊，直接使用合併後的資料
        merged_data_with_info <- merged_data

      } else {
        # 智能對應策略
        cat(safe_get_text_func(texts, "console.using_smart_mapping", "🔄 使用智能對應策略"), "\n")

        if (length(eval_variations) == 0 || length(sales_variations) == 0) {
          error_msg <- safe_get_text_func(texts, "console.no_data_error", "❌ 評分或銷售資料為空")
          showNotification(error_msg, type = "error")
          return()
        }

        # 取較小的數量進行對應
        min_length <- min(length(eval_variations), length(sales_variations))

        if (length(eval_variations) <= length(sales_variations)) {
          mapping_table <- data.frame(
            eval_variation = eval_variations,
            sales_variation = sales_variations[1:length(eval_variations)],
            stringsAsFactors = FALSE
          )
        } else {
          mapping_table <- data.frame(
            eval_variation = eval_variations[1:length(sales_variations)],
            sales_variation = sales_variations,
            stringsAsFactors = FALSE
          )
        }

        merged_data <- sales_df %>%
          left_join(mapping_table, by = c("Variation" = "sales_variation")) %>%
          mutate(
            Variation = coalesce(eval_variation, Variation)
          ) %>%
          select(-eval_variation) %>%
          left_join(mean_data, by = "Variation") %>%
          filter(!is.na(Sales))

        # 不需要顯示對應關係資訊
        merged_data_with_info <- merged_data
      }

      merged_data_rv(merged_data_with_info)

      # 顯示合併後的資料
      output$merged_data_tbl <- renderDT(
        merged_data_with_info,
        selection = "none",
        options = list(
          pageLength = 5,
          scrollX = TRUE
        )
      )
    })

    # 執行 Poisson 回歸分析
    observeEvent(input$run_poisson, {
      req(merged_data_rv())

      texts <- module_lang_content()
      safe_get_text_func <- if (exists("safe_get_text") && is.function(safe_get_text)) {
        safe_get_text
      } else {
        function(content, path, default) default
      }

      notification_msg <- safe_get_text_func(texts, "messages.info.analyzing", "🔄 正在執行 Poisson 回歸分析...")
      showNotification(notification_msg, type = "message", duration = 3)

      tryCatch({
        analysis_data <- merged_data_rv()

        # 檢查是否有 Sales 欄位（大寫 S）
        if ("Sales" %in% names(analysis_data) && !"sales" %in% names(analysis_data)) {
          analysis_data$sales <- analysis_data$Sales
        }

        # 只使用評分產生的屬性（從 mean_data 取得，排除 Variation）
        score_cols <- names(mean_data_rv())
        score_cols <- score_cols[score_cols != "Variation"]

        # 確認這些欄位都存在於 analysis_data 中
        score_cols <- score_cols[score_cols %in% names(analysis_data)]

        # 再次確認都是數值型欄位
        score_cols <- score_cols[sapply(analysis_data[score_cols], is.numeric)]

        # 輸出 Poisson 分析訊息
        cat(safe_get_text_func(texts, "console.poisson_attributes", "📊 使用評分屬性進行 Poisson 回歸:"), "\n")

        attrs_list_text <- safe_get_text_func(texts, "console.attributes_list", "  屬性列表: {attrs}")
        cat(gsub("\\{attrs\\}", paste(score_cols, collapse = ", "), attrs_list_text), "\n")

        attrs_count_text <- safe_get_text_func(texts, "console.attributes_count", "  屬性數量: {count}")
        cat(gsub("\\{count\\}", length(score_cols), attrs_count_text), "\n")

        if (length(score_cols) == 0) {
          error_msg <- safe_get_text_func(texts, "messages.error.no_sales_column", "❌ 沒有找到評分欄位")
          showNotification(error_msg, type = "error")
          return()
        }

        # Get column names from YAML - fully language-agnostic
        attr_col <- safe_get_text_func(texts, "columns.attribute", "Attribute")
        coef_col <- safe_get_text_func(texts, "columns.coefficient", "Coefficient")
        effect_col <- safe_get_text_func(texts, "columns.marginal_effect", "Marginal Effect")
        multiplier_col <- safe_get_text_func(texts, "columns.multiplier", "Multiplier")
        interp_col <- safe_get_text_func(texts, "columns.interpretation", "Interpretation")
        meaning_col <- safe_get_text_func(texts, "columns.business_significance", "Business Significance")

        # Default messages from YAML - fully language-agnostic
        fail_msg <- safe_get_text_func(texts, "defaults.analysis_failed", "Analysis failed")
        no_analysis_msg <- safe_get_text_func(texts, "defaults.unable_to_analyze", "Unable to analyze")

        # 對每個評分屬性執行 Poisson 回歸
        results_list <- lapply(score_cols, function(col) {
          result <- tryCatch({
            poisson_regression(col, analysis_data)
          }, error = function(e) {
            attr_error_text <- safe_get_text_func(texts, "console.attribute_error", "⚠️ 無法分析屬性 {attr}: {error}")
            error_msg <- gsub("\\{attr\\}", col, gsub("\\{error\\}", e$message, attr_error_text))
            message(error_msg)
            list(
              marginal_effect_pct = NA_real_,
              coefficient = NA_real_,
              track_multiplier = NA_real_,
              interpretation = fail_msg,
              practical_meaning = no_analysis_msg
            )
          })

          # Ensure all required fields exist with proper types and lengths
          if (is.null(result$marginal_effect_pct)) result$marginal_effect_pct <- NA_real_
          if (is.null(result$coefficient)) result$coefficient <- NA_real_
          if (is.null(result$track_multiplier)) result$track_multiplier <- NA_real_
          if (is.null(result$interpretation)) result$interpretation <- ""
          if (is.null(result$practical_meaning)) result$practical_meaning <- ""

          # Ensure scalar values (length 1)
          result$marginal_effect_pct <- result$marginal_effect_pct[1]
          result$coefficient <- result$coefficient[1]
          result$track_multiplier <- result$track_multiplier[1]
          result$interpretation <- as.character(result$interpretation)[1]
          result$practical_meaning <- as.character(result$practical_meaning)[1]

          # Translate Chinese content from Poisson function to current language using YAML mappings
          # Get translation maps from YAML (no hardcoded patterns!)
          interp_map <- texts$translation_map$interpretation
          meaning_map <- texts$translation_map$meaning

          # Translate interpretation
          interp_translated <- result$interpretation
          if (!is.null(interp_map) && !is.na(interp_translated) && nchar(interp_translated) > 0) {
            for (pattern in names(interp_map)) {
              if (grepl(pattern, interp_translated)) {
                interp_translated <- interp_map[[pattern]]
                break
              }
            }
          }
          # Fallback to default if still not translated
          if (interp_translated == result$interpretation && (is.na(interp_translated) || nchar(interp_translated) == 0)) {
            interp_translated <- fail_msg
          }

          # Translate practical meaning
          meaning_translated <- result$practical_meaning
          if (!is.null(meaning_map) && !is.na(meaning_translated) && nchar(meaning_translated) > 0) {
            for (pattern in names(meaning_map)) {
              if (grepl(pattern, meaning_translated)) {
                meaning_translated <- meaning_map[[pattern]]
                break
              }
            }
          }
          # Fallback to default if still not translated
          if (meaning_translated == result$practical_meaning && (is.na(meaning_translated) || nchar(meaning_translated) == 0)) {
            meaning_translated <- no_analysis_msg
          }

          # Create dataframe with standard column names first, then rename
          temp_df <- data.frame(
            attr = col,
            coef = result$coefficient,
            effect = if (is.na(result$marginal_effect_pct)) NA_character_ else paste0(result$marginal_effect_pct, "%"),
            multiplier = result$track_multiplier,
            interp = interp_translated,
            meaning = meaning_translated,
            stringsAsFactors = FALSE
          )

          # Rename columns to language-aware names
          names(temp_df) <- c(attr_col, coef_col, effect_col, multiplier_col, interp_col, meaning_col)

          temp_df
        })

        results_df <- do.call(rbind, results_list)

        # 按賽道倍數排序（影響力從大到小）- 處理 NA 值
        # Create temporary column for sorting
        multiplier_temp <- paste0(multiplier_col, "_num")
        results_df[[multiplier_temp]] <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", results_df[[multiplier_col]])))
        results_df <- results_df %>%
          arrange(desc(abs(ifelse(is.na(.data[[multiplier_temp]]), 0, .data[[multiplier_temp]])))) %>%
          select(-all_of(multiplier_temp))

        poisson_results_rv(results_df)

        # 顯示結果表
        output$poisson_results <- renderDT(
          results_df,
          selection = "none",
          options = list(
            pageLength = 15,
            scrollX = TRUE
          )
        )

        # 繪製視覺化圖表 - 處理 NA 值
        # Use language-aware column names for plot
        effect_value_col <- paste0(effect_col, "_value")

        # First create the numeric value column
        plot_data <- results_df
        plot_data[[effect_value_col]] <- suppressWarnings(as.numeric(gsub("%", "", plot_data[[effect_col]])))
        plot_data[[effect_value_col]] <- ifelse(is.na(plot_data[[effect_value_col]]), 0, plot_data[[effect_value_col]])
        plot_data[[attr_col]] <- factor(plot_data[[attr_col]], levels = rev(plot_data[[attr_col]]))

        # Get hover text labels from YAML - fully language-agnostic
        meaning_label <- safe_get_text_func(texts, "results.business_significance_label", "Business Significance:")
        effect_label <- safe_get_text_func(texts, "results.marginal_effect_label", "Marginal Effect:")

        output$poisson_plot <- renderPlotly({
          p <- plot_ly(
            data = plot_data,
            x = as.formula(paste0("~`", effect_value_col, "`")),
            y = as.formula(paste0("~`", attr_col, "`")),
            type = 'bar',
            orientation = 'h',
            text = as.formula(paste0("~paste('", meaning_label, "', `", meaning_col, "`)")),
            hovertemplate = paste(
              "%{y}<br>",
              effect_label, " %{x:.1f}%<br>",
              "%{text}<br>",
              "<extra></extra>"
            ),
            marker = list(
              color = as.formula(paste0("~ifelse(`", effect_value_col, "` > 0, '#28a745', '#dc3545')")),
              line = list(color = 'white', width = 1)
            )
          )

          p <- p %>%
            layout(
              title = safe_get_text_func(texts, "chart.title", "屬性對銷售的邊際效應"),
              xaxis = list(
                title = safe_get_text_func(texts, "chart.xaxis_title", "邊際效應 (%)"),
                gridcolor = '#f0f0f0',
                zeroline = TRUE,
                zerolinecolor = '#888',
                zerolinewidth = 2
              ),
              yaxis = list(
                title = "",
                gridcolor = '#f0f0f0'
              ),
              margin = list(l = 150),
              plot_bgcolor = '#fafafa',
              paper_bgcolor = 'white'
            )

          p
        })

        success_msg <- safe_get_text_func(texts, "messages.success.analysis_complete", "✅ Poisson 回歸分析完成！")
        showNotification(success_msg, type = "message", duration = 5)

      }, error = function(e) {
        error_template <- safe_get_text_func(texts, "messages.error.analysis_failed", "❌ 分析失敗: {error}")
        error_msg <- gsub("\\{error\\}", e$message, error_template)
        showNotification(error_msg, type = "error", duration = 10)
      })
    })

    # 產生個性化行銷策略（品牌特定分析 - 參考 MAMBA 架構）
    observeEvent(input$generate_strategy, {
      # 確保必要資料已就緒
      if (is.null(poisson_results_rv()) || nrow(poisson_results_rv()) == 0) {
        showNotification("⚠️ 請先在『銷售模型』分頁點擊「執行分析」，產生模型結果後再生成策略。", type = "warning", duration = 5)
        return()
      }
      if (is.null(mean_data_rv()) || nrow(mean_data_rv()) == 0) {
        showNotification("⚠️ 尚未取得品牌屬性平均值，請檢查資料上傳。", type = "warning", duration = 5)
        return()
      }
      req(input$strategy_brand)

      selected_brand <- input$strategy_brand
      results <- poisson_results_rv()  # 市場層級的屬性重要性
      mean_data <- mean_data_rv()       # 各品牌的屬性評分

      cat("\n🎯 [Strategy] 產生", selected_brand, "的行銷策略\n")

      # 找出最重要的屬性（前3個）
      top_attrs <- head(results, 3)

      # 获取当前语言文本
      texts <- module_lang_content()
      safe_get_text_func <- if (exists("safe_get_text") && is.function(safe_get_text)) {
        safe_get_text
      } else {
        function(content, path, default) default
      }

      # 產生策略內容
      # Get attribute column names from YAML - fully language-agnostic
      attr_col <- safe_get_text_func(texts, "columns.attribute", "Attribute")
      meaning_col <- safe_get_text_func(texts, "columns.business_significance", "Business Significance")
      effect_col <- safe_get_text_func(texts, "columns.marginal_effect", "Marginal Effect")
      multiplier_col <- safe_get_text_func(texts, "columns.multiplier", "Multiplier")

      # === 品牌定位分析（新增）===
      # 取得選定品牌的資料
      brand_data <- mean_data %>% filter(Variation == selected_brand)

      if (nrow(brand_data) == 0) {
        showNotification(paste("❌ 找不到品牌:", selected_brand), type = "error")
        return()
      }

      # 計算市場平均（排除 Ideal 如果存在）
      market_avg <- mean_data %>%
        filter(!Variation %in% c("Ideal", selected_brand)) %>%
        summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))

      # 取得重要屬性的名稱
      top_attr_names <- if (attr_col %in% names(top_attrs)) {
        top_attrs[[attr_col]]
      } else {
        top_attrs[[1]]
      }

      cat("  📊 重要屬性:", paste(top_attr_names, collapse = ", "), "\n")

      # 比較品牌 vs 市場在重要屬性上的表現
      brand_vs_market <- lapply(top_attr_names, function(attr_name) {
        # 確認屬性存在於資料中
        if (!attr_name %in% names(brand_data) || !attr_name %in% names(market_avg)) {
          return(list(
            attr = attr_name,
            brand_val = NA,
            market_val = NA,
            diff = 0,
            diff_pct = 0,
            status = "資料不足"
          ))
        }

        brand_val <- as.numeric(brand_data[[attr_name]])
        market_val <- as.numeric(market_avg[[attr_name]])
        diff <- brand_val - market_val
        diff_pct <- if (market_val != 0) round((diff / market_val) * 100, 1) else 0

        status <- if (is.na(diff)) {
          "資料不足"
        } else if (diff > 0.1) {
          "優勢"
        } else if (diff < -0.1) {
          "待改善"
        } else {
          "持平"
        }

        cat("    -", attr_name, ": 品牌=", round(brand_val, 2), ", 市場=", round(market_val, 2), ", 差異=", round(diff, 2), "(", status, ")\n")

        list(
          attr = attr_name,
          brand_val = round(brand_val, 2),
          market_val = round(market_val, 2),
          diff = round(diff, 2),
          diff_pct = diff_pct,
          status = status
        )
      })

      # 分類優勢和劣勢
      advantages <- Filter(function(x) x$status == "優勢", brand_vs_market)
      improvements <- Filter(function(x) x$status == "待改善", brand_vs_market)

      strategy_html <- tags$div(
        class = "marketing-strategy",
        style = "background: #f8f9fa; padding: 20px; border-radius: 8px;",

        h4(paste("🎯", selected_brand, safe_get_text_func(texts, "strategy.recommendation_title", "行銷策略建議"))),
        br(),

        # === 品牌定位分析（新增區塊）===
        h5("📈 品牌定位分析："),
        tags$div(
          style = "margin-bottom: 15px; padding: 10px; background: #e9ecef; border-radius: 5px;",
          tags$p(style = "margin-bottom: 5px;", tags$strong("與市場平均相比：")),
          tags$ul(
            lapply(brand_vs_market, function(item) {
              status_color <- switch(item$status,
                "優勢" = "#28a745",
                "待改善" = "#dc3545",
                "持平" = "#6c757d",
                "#6c757d"
              )
              status_icon <- switch(item$status,
                "優勢" = "↑",
                "待改善" = "↓",
                "持平" = "→",
                "?"
              )
              tags$li(
                tags$strong(item$attr, ": "),
                tags$span(
                  style = paste0("color:", status_color, "; font-weight: bold;"),
                  paste0(status_icon, " ", item$status)
                ),
                if (!is.na(item$brand_val) && !is.na(item$market_val)) {
                  paste0(" (品牌: ", item$brand_val, " vs 市場: ", item$market_val, ")")
                }
              )
            })
          )
        ),

        h5(safe_get_text_func(texts, "strategy.key_attributes", "📊 關鍵屬性分析：")),
        tags$ul(
          lapply(1:nrow(top_attrs), function(i) {
            attr <- top_attrs[i, ]
            # Get values using either English or Chinese column names
            attr_name <- if (attr_col %in% names(attr)) attr[[attr_col]] else attr[[1]]
            attr_meaning <- if (meaning_col %in% names(attr)) attr[[meaning_col]] else attr[[2]]
            attr_effect <- if (effect_col %in% names(attr)) attr[[effect_col]] else attr[[3]]
            attr_mult <- if (multiplier_col %in% names(attr)) attr[[multiplier_col]] else attr[[4]]

            tags$li(
              tags$strong(attr_name, ": "),
              paste(attr_meaning,
                   sprintf("(%s: %s, %s: %sx)",
                          safe_get_text_func(texts, "results.marginal_effect", "邊際效應"),
                          attr_effect,
                          safe_get_text_func(texts, "results.multiplier", "賽道倍數"),
                          attr_mult))
            )
          })
        ),

        h5(safe_get_text_func(texts, "strategy.marketing_suggestions", "💡 行銷建議：")),
        tags$ol(
          # 根據品牌定位生成針對性建議
          if (length(advantages) > 0) {
            adv_attrs <- paste(sapply(advantages, function(x) x$attr), collapse = "、")
            tags$li(paste0("✅ 優勢強化：您在「", adv_attrs, "」表現優於市場，建議在行銷文案中重點強調這些優勢"))
          },
          if (length(improvements) > 0) {
            imp_attrs <- paste(sapply(improvements, function(x) x$attr), collapse = "、")
            tags$li(paste0("⚠️ 待改善：您在「", imp_attrs, "」低於市場平均，建議優化產品在這些面向的表現"))
          },
          tags$li({
            top_attr_name <- if (attr_col %in% names(top_attrs)) top_attrs[[attr_col]][1] else top_attrs[[1]][1]
            paste0("🎯 核心因素：「", top_attr_name, "」對銷售影響最大，應列為首要關注重點")
          }),
          tags$li(safe_get_text_func(texts, "strategy.suggestion_4", "定期監控這些屬性的市場反饋並優化"))
        ),

        h5(safe_get_text_func(texts, "strategy.execution_strategy", "🚀 執行策略：")),
        tags$div(
          class = "alert alert-info",
          safe_get_text_func(texts, "strategy.ab_test_recommendation", "建議先進行小規模 A/B 測試，驗證屬性強調的效果，再逐步擴大投放規模。")
        )
      )

      output$marketing_strategy <- renderUI(strategy_html)

      success_template <- safe_get_text_func(texts, "messages.success.strategy_generated", "✅ 已產生 {brand} 的行銷策略！")
      success_msg <- gsub("\\{brand\\}", selected_brand, success_template)
      showNotification(success_msg, type = "message", duration = 5)
    })

    # 返回模組輸出
    list(
      mean_data = reactive(mean_data_rv()),
      merged_data = reactive(merged_data_rv()),
      poisson_results = reactive(poisson_results_rv())
    )
  })
}
