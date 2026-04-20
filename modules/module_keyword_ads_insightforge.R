################################################################################
# 關鍵字廣告投放建議模組
################################################################################

# NULL 合併運算子
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

#' 關鍵字廣告建議模組 UI
keyword_ads_insightforgeModuleUI <- function(id, module_config = NULL, lang_texts = NULL, brand_choices = NULL) {
  ns <- NS(id)

  # 使用統一語言管理器取得當前語言內容（從全域環境）
  current_language <- if (exists("get_current_language") && is.function(get_current_language)) {
    get_current_language()
  } else {
    "zh_TW"
  }

  # 取得模組語言內容
  module_lang_content <- if (exists("get_module_language_content") && is.function(get_module_language_content)) {
    get_module_language_content("keyword_ads_insightforge", current_language)
  } else {
    lang_texts %||% list(language = current_language)
  }

  cat("🌍 [Keyword Ads UI] 當前語言:", current_language, "\n")
  cat("  📋 [Keyword Ads UI] Received brand_choices:", if(is.null(brand_choices)) "NULL" else paste(brand_choices, collapse = ", "), "\n")

  # 載入提示系統
  hints_df <- if (exists("get_current_hints") && is.function(get_current_hints)) {
    get_current_hints()
  } else if (exists("load_hints") && is.function(load_hints)) {
    tryCatch({
      load_hints(language = current_language)
    }, error = function(e) {
      cat("❌ [Keyword Ads UI] hints 載入失敗:", e$message, "\n")
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
  
  fluidRow(
    bs4Card(
      title = safe_get_text_func(module_lang_content, "title", "🔍 關鍵字廣告投放建議"),
      status = "info",
      width = 12,
      solidHeader = TRUE,
      elevation = 3,

      fluidRow(
        column(12,
          p(if (!is.null(lang_texts) && !is.null(lang_texts$subtitle)) lang_texts$subtitle else "根據產品屬性分析結果，建議最佳關鍵字廣告策略", style = "color: #666; margin-bottom: 20px;"),
          
          # 分析控制區
          div(
            style = "background: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 20px;",
            h5(if (!is.null(lang_texts) && !is.null(lang_texts$settings$title)) lang_texts$settings$title else "📋 分析設定"),
            fluidRow(
              column(4,
                selectInput(ns("keyword_brand"),
                           if (!is.null(lang_texts) && !is.null(lang_texts$settings$brand_selection)) lang_texts$settings$brand_selection else "選擇品牌/產品：",
                           choices = brand_choices)
              ),
              column(4,
                numericInput(ns("keyword_count"),
                            if (!is.null(lang_texts) && !is.null(lang_texts$settings$keyword_count)) lang_texts$settings$keyword_count else "建議關鍵字數量：",
                           value = 10, min = 5, max = 20, step = 1)
              ),
              column(4,
                selectInput(ns("keyword_platform"),
                           if (!is.null(lang_texts) && !is.null(lang_texts$settings$platform)) lang_texts$settings$platform else "廣告平台：",
                           choices = if (!is.null(lang_texts) && !is.null(lang_texts$platforms)) {
                             c(lang_texts$platforms$google, lang_texts$platforms$facebook, lang_texts$platforms$all)
                           } else {
                             c("Google Ads" = "google",
                               "Facebook Ads" = "facebook",
                               "全平台" = "all")
                           },
                           selected = "google")
              )
            ),
            actionButton(ns("generate_keywords"),
                        if (!is.null(lang_texts) && !is.null(lang_texts$generation$button)) lang_texts$generation$button else "生成關鍵字建議",
                        class = "btn-primary", icon = icon("search"))
          ),
          
          # 結果顯示區
          uiOutput(ns("keyword_results")),
          
          # 下載報告
          div(
            style = "margin-top: 20px;",
            downloadButton(ns("download_keywords"),
                          if (!is.null(lang_texts) && !is.null(lang_texts$export$button)) lang_texts$export$button else "下載關鍵字報告",
                          class = "btn-secondary")
          )
        )
      )
    )
  )
}

#' 關鍵字廣告建議模組 Server
keyword_ads_insightforgeModuleServer <- function(id, scored_data = NULL, prompts_df = NULL, module_config = NULL, lang_texts = NULL, api_config = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 從共用 API 設定讀取模型（fallback 到 gpt-5-nano）
    cfg_ai_model <- if (!is.null(api_config$default_model)) {
      api_config$default_model
    } else {
      "gpt-5-nano"
    }

    # ============================================
    # 🌍 語言管理狀態
    # ============================================
    # 追蹤當前語言和內容
    current_language <- reactiveVal("zh_TW")
    current_hints <- reactiveVal(NULL)
    current_prompts <- reactiveVal(NULL)
    module_lang_texts <- reactiveVal(NULL)

    # 輔助函數：安全取得語言文字
    get_lang_text <- function(path, default = "") {
      texts <- module_lang_texts()
      if (is.null(texts)) return(default)

      # 解析路徑 (e.g., "generation$processing")
      parts <- strsplit(path, "\\$")[[1]]
      result <- texts
      for (part in parts) {
        if (is.null(result) || !is.list(result)) return(default)
        result <- result[[part]]
        if (is.null(result)) return(default)
      }
      return(result)
    }

    # ============================================
    # 初始化模組語言內容
    # ============================================
    initial_lang <- if (exists("get_module_language_content")) {
      lang_content <- get_module_language_content("keyword_ads_insightforge", "zh_TW")
      module_lang_texts(lang_content)
      "zh_TW"
    } else {
      "zh_TW"
    }

    # ============================================
    # 🔄 語言變更觀察器 - 核心修復
    # ============================================
    observe({
      # 監聽全局語言變更
      if (exists("module_ui_update_trigger") && is.function(module_ui_update_trigger)) {
        module_ui_update_trigger()  # 觸發更新
      }

      # 獲取當前語言內容
      current_lang_content <- if (exists("global_lang_content") && is.function(global_lang_content)) {
        tryCatch(global_lang_content(), error = function(e) NULL)
      } else {
        NULL
      }

      if (!is.null(current_lang_content)) {
        new_language <- current_lang_content$language

        # 只在語言真正改變時更新
        if (new_language != isolate(current_language())) {
          cat("\n📢 [Keyword Ads Module] 偵測到語言變更:", isolate(current_language()), "->", new_language, "\n")

          # 更新語言狀態
          current_language(new_language)

          # 1. 重新載入 hints
          new_hints <- tryCatch({
            if (exists("load_hints") && is.function(load_hints)) {
              cat("📝 [Keyword Ads Module] 重新載入 hints，語言:", new_language, "\n")
              load_hints(language = new_language)
            } else {
              NULL
            }
          }, error = function(e) {
            cat("❌ [Keyword Ads Module] Hints 載入失敗:", e$message, "\n")
            NULL
          })
          current_hints(new_hints)

          # 2. 重新載入 prompts
          new_prompts <- tryCatch({
            if (exists("load_prompts") && is.function(load_prompts)) {
              cat("📝 [Keyword Ads Module] 重新載入 prompts，語言:", new_language, "\n")
              load_prompts(language = new_language)
            } else {
              NULL
            }
          }, error = function(e) {
            cat("❌ [Keyword Ads Module] Prompts 載入失敗:", e$message, "\n")
            NULL
          })
          current_prompts(new_prompts)

          # 3. 更新模組特定的語言文字
          new_lang_texts <- if (exists("get_module_language_content")) {
            get_module_language_content("keyword_ads_insightforge", new_language)
          } else if (!is.null(current_lang_content$content$modules$keyword_ads)) {
            current_lang_content$content$modules$keyword_ads
          } else {
            NULL
          }

          if (!is.null(new_lang_texts)) {
            module_lang_texts(new_lang_texts)
            cat("✅ [Keyword Ads Module] 模組語言文字已更新\n")
          }

          cat("✅ [Keyword Ads Module] 語言切換完成\n")
        }
      }
    })

    # 儲存關鍵字建議結果
    keyword_suggestions <- reactiveVal(NULL)
    
    # 更新品牌選擇列表
    observe({
      # 檢查 scored_data 是否可用
      if (is.null(scored_data) || !is.function(scored_data)) {
        cat("  ⚠️  [Keyword Ads] scored_data dependency not available, skipping update\n")
        return()
      }

      # Listen to UI update trigger to re-populate dropdown after UI recreation
      if (exists("module_ui_update_trigger") && is.function(module_ui_update_trigger)) {
        trigger_val <- module_ui_update_trigger()
        cat("🔍 [Keyword Ads] observe triggered (UI trigger:", trigger_val, ")\n")
      } else {
        cat("🔍 [Keyword Ads] observe triggered\n")
      }

      data <- tryCatch({
        scored_data()
      }, error = function(e) {
        cat("  ⚠️  [Keyword Ads] scored_data() error:", e$message, "\n")
        NULL
      })
      cat("  📊 [Keyword Ads] scored_data() returned:", if(is.null(data)) "NULL" else paste(nrow(data), "rows"), "\n")

      req(data)

      if (!"Variation" %in% names(data)) {
        cat("  ❌ [Keyword Ads] ERROR: No 'Variation' column in scored_data\n")
        cat("  📋 [Keyword Ads] Available columns:", paste(names(data), collapse = ", "), "\n")
        return()
      }

      variations <- unique(data$Variation)
      cat("  ✅ [Keyword Ads] Found variations:", paste(variations, collapse = ", "), "\n")
      cat("  🔄 [Keyword Ads] Updating keyword_brand selectInput with", length(variations), "choices\n")
      updateSelectInput(session, "keyword_brand", choices = variations)
      cat("  ✅ [Keyword Ads] keyword_brand updated successfully\n")
    })
    
    # 生成關鍵字建議
    observeEvent(input$generate_keywords, {
      # 檢查依賴是否可用
      if (is.null(scored_data) || !is.function(scored_data)) {
        showNotification("⚠️ 評分資料未載入，請先執行評分分析", type = "warning", duration = 5)
        return()
      }

      data <- tryCatch({
        scored_data()
      }, error = function(e) {
        cat("  ⚠️  [Keyword Ads] scored_data() error:", e$message, "\n")
        NULL
      })

      req(input$keyword_brand, data)

      notification_msg <- get_lang_text("generation$processing", "🔄 正在分析關鍵字...")
      showNotification(notification_msg, type = "message", duration = 3)

      tryCatch({
        # 取得品牌評分資料
        brand_data <- data %>%
          filter(Variation == input$keyword_brand)
        
        # 計算各屬性平均分
        attr_cols <- names(brand_data)[!names(brand_data) %in% "Variation"]
        attr_cols <- attr_cols[sapply(brand_data[attr_cols], is.numeric)]
        
        brand_mean <- brand_data %>%
          summarise(across(all_of(attr_cols), \(x) mean(x, na.rm = TRUE)))
        
        # 排序屬性（由高到低）
        attr_scores <- sort(unlist(brand_mean), decreasing = TRUE)
        top_attrs <- names(head(attr_scores, 5))
        
        # 使用集中式 Prompt 管理生成關鍵字
        if (!is.null(prompts_df) && exists("prepare_gpt_messages")) {
          # 如果 prompts_df 是 reactive，取其值
          prompts_data <- if (is.function(prompts_df)) prompts_df() else prompts_df

          # 檢查 prompts_df 是否包含所需的 prompt
          if (is.null(prompts_data) || !"keyword_generation" %in% prompts_data$var_id) {
            # 如果沒有找到 prompt，使用備用方案
            basic_keywords <- generate_basic_keywords(input$keyword_brand, top_attrs)
            keyword_suggestions(basic_keywords)
            output$keyword_results <- renderUI({
              div(
                style = "background: white; border: 1px solid #dee2e6; border-radius: 8px; padding: 20px;",
                h5(paste(get_lang_text("results$title", "📌 關鍵字建議"), "（基本版）"), style = "color: #2c3e50; margin-bottom: 15px;"),
                div(
                  style = "background: #f8f9fa; padding: 15px; border-radius: 5px;",
                  HTML(gsub("\n", "<br>", basic_keywords))
                )
              )
            })
            basic_msg <- get_lang_text("messages$basic_generation", "使用基本關鍵字生成")
            showNotification(basic_msg, type = "message", duration = 3)
            return()
          }
          
          messages <- prepare_gpt_messages(
            var_id = "keyword_generation",
            variables = list(
              brand_name = input$keyword_brand,
              top_attributes = paste(top_attrs, collapse = ", "),
              keyword_count = input$keyword_count,
              platform = input$keyword_platform,
              avg_score = round(mean(attr_scores), 2)
            ),
            prompts_df = prompts_data
          )
          
          # 呼叫 GPT API
          if (exists("chat_api")) {
            keyword_text <- chat_api(messages)
            keyword_suggestions(keyword_text)
          } else {
            # 備用方案：生成基本關鍵字
            basic_keywords <- generate_basic_keywords(input$keyword_brand, top_attrs)
            keyword_suggestions(basic_keywords)
          }
        } else {
          # 沒有 Prompt 管理的備用方案
          basic_keywords <- generate_basic_keywords(input$keyword_brand, top_attrs)
          keyword_suggestions(basic_keywords)
        }
        
        # 顯示結果
        output$keyword_results <- renderUI({
          req(keyword_suggestions())

          div(
            style = "background: white; border: 1px solid #dee2e6; border-radius: 8px; padding: 20px;",
            h5(get_lang_text("results$title", "📌 關鍵字廣告建議"), style = "color: #2c3e50; margin-bottom: 15px;"),

            # 關鍵屬性
            div(
              style = "background: #e8f4f8; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
              strong(get_lang_text("results$core_attributes", "核心優勢屬性：")),
              p(paste(top_attrs, collapse = " | "), style = "margin-top: 5px;")
            ),

            # 關鍵字建議
            div(
              style = "background: #f8f9fa; padding: 15px; border-radius: 5px;",
              HTML(markdown::markdownToHTML(text = keyword_suggestions(), fragment.only = TRUE))
            )
          )
        })

        success_msg <- get_lang_text("generation$complete", "✅ 關鍵字建議生成完成！")
        showNotification(success_msg, type = "message", duration = 3)

      }, error = function(e) {
        error_msg_template <- get_lang_text("messages$generation_failed", "❌ 生成關鍵字失敗：{error}")
        error_msg <- gsub("\\{error\\}", e$message, error_msg_template)
        showNotification(error_msg, type = "error", duration = 5)
        output$keyword_results <- renderUI({
          div(class = "alert alert-danger", error_msg)
        })
      })
    })
    
    # 下載功能
    output$download_keywords <- downloadHandler(
      filename = function() {
        paste0("keyword_ads_", input$keyword_brand, "_", Sys.Date(), ".txt")
      },
      content = function(file) {
        no_suggestions_text <- get_lang_text("messages$no_suggestions", "尚無關鍵字建議")
        writeLines(keyword_suggestions() %||% no_suggestions_text, file)
      }
    )
    
    # 輔助函數：生成基本關鍵字
    generate_basic_keywords <- function(brand, attrs) {
      keywords <- c(
        paste(brand, "評價"),
        paste(brand, "推薦"),
        paste(brand, "比較"),
        paste(attrs, brand),
        paste("最佳", attrs),
        paste(attrs, "產品")
      )
      suggestion_title <- get_lang_text("results$keyword_list", "建議關鍵字")
      paste(paste0(suggestion_title, "：\n"), paste("• ", keywords, collapse = "\n"))
    }
  })
}

# 為了與 app_insightforge.R 保持兼容，創建 Server 函數別名
keywordAdsModuleServer <- keyword_ads_insightforgeModuleServer