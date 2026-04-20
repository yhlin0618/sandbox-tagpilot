################################################################################
# 新品開發建議模組
################################################################################

# NULL 合併運算子
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

#' 新品開發建議模組 UI
product_dev_insightforgeModuleUI <- function(id, module_config = NULL, lang_texts = NULL) {
  ns <- NS(id)

  cat("🚀 [Product Dev Module UI] 初始化，語言:",
      if(!is.null(lang_texts) && !is.null(lang_texts$language)) lang_texts$language else "未知", "\n")

  # Get current language
  current_language_val <- if (!is.null(lang_texts) && !is.null(lang_texts$language)) {
    lang_texts$language
  } else {
    "zh_TW"
  }

  # Load hints for current language
  hints_df <- tryCatch({
    if (exists("load_hints") && is.function(load_hints)) {
      load_hints(language = current_language_val)
    } else {
      NULL
    }
  }, error = function(e) {
    NULL
  })

  # Helper function to get text from lang_texts
  get_lang_value <- function(path, default) {
    if (is.null(lang_texts)) return(default)
    parts <- strsplit(path, "\\.")[[1]]
    current <- lang_texts
    for (part in parts) {
      if (is.list(current) && part %in% names(current)) {
        current <- current[[part]]
      } else {
        return(default)
      }
    }
    if (is.null(current)) default else current
  }

  # Get translated texts
  title_text <- get_lang_value("title", "🚀 New Product Development Recommendations")
  subtitle_text <- get_lang_value("subtitle", "Based on market attribute analysis, provide new product development direction recommendations")
  strategy_title_text <- get_lang_value("strategy.title", "🎯 Development Strategy Settings")
  strategy_approach_text <- get_lang_value("strategy.approach", "Development Strategy:")
  priority_count_text <- get_lang_value("strategy.priority_count", "Priority Development Count:")
  analyze_button_text <- get_lang_value("analysis.button", "Analyze Development Opportunities")

  # Strategy choices (removed: innovation, compete)
  strategy_choices <- setNames(
    c("gap", "strength"),
    c(get_lang_value("strategies.gap", "Fill Market Gap"),
      get_lang_value("strategies.strength", "Strengthen Advantage Areas"))
  )

  # Segment choices removed - target segment handled in BrandEdge

  # 📌 Return static UI (will be rebuilt by outer renderUI on language change)
  fluidRow(
    bs4Card(
      title = title_text,
      status = "success",
      width = 12,
      solidHeader = TRUE,
      elevation = 3,

      fluidRow(
        column(12,
          p(subtitle_text, style = "color: #666; margin-bottom: 20px;"),

          # Analysis settings
          div(
            style = "background: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 20px;",
            h5(strategy_title_text),
            fluidRow(
              column(6,
                selectInput(ns("dev_strategy"),
                           strategy_approach_text,
                           choices = strategy_choices,
                           selected = "gap")
              ),
              column(6,
                numericInput(ns("dev_priority"),
                            priority_count_text,
                           value = 3, min = 1, max = 5, step = 1)
              )
            ),
            actionButton(ns("analyze_opportunities"),
                        analyze_button_text,
                        class = "btn-success", icon = icon("lightbulb"))
          ),

          # Market gaps analysis
          uiOutput(ns("market_gaps")),

          # Product suggestions
          uiOutput(ns("product_suggestions")),

          # Development roadmap
          uiOutput(ns("development_roadmap"))
        )
      )
    )
  )
}

#' 新品開發建議模組 Server
product_dev_insightforgeModuleServer <- function(id, scored_data = NULL, prompts_df = NULL, module_config = NULL, lang_texts = NULL, api_config = NULL) {
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
    # 追蹤當前語言和內容 - 初始化為全局語言
    initial_lang <- if (exists("global_lang_content") && is.function(global_lang_content)) {
      tryCatch({
        lang_content <- global_lang_content()
        if (!is.null(lang_content) && !is.null(lang_content$language)) {
          lang_content$language
        } else {
          "zh_TW"
        }
      }, error = function(e) "zh_TW")
    } else {
      "zh_TW"
    }
    current_language <- reactiveVal(initial_lang)
    current_hints <- reactiveVal(NULL)
    current_prompts <- reactiveVal(NULL)
    module_lang_texts <- reactiveVal(NULL)

    cat("📝 [Product Dev Module] 初始化語言:", initial_lang, "\n")

    # 輔助函數：安全取得語言文字
    get_lang_text <- function(path, default = "") {
      texts <- module_lang_texts()
      if (is.null(texts)) return(default)

      # 解析路徑 (e.g., "market_gaps$title")
      parts <- strsplit(path, "\\$")[[1]]
      result <- texts
      for (part in parts) {
        if (is.null(result) || !is.list(result)) return(default)
        result <- result[[part]]
        if (is.null(result)) return(default)
      }
      return(result)
    }

    # 初始化模組語言內容
    if (exists("get_module_language_content")) {
      lang_content <- get_module_language_content("product_dev_insightforge", initial_lang)
      module_lang_texts(lang_content)
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
          cat("\n📢 [Product Dev Module] 偵測到語言變更:", isolate(current_language()), "->", new_language, "\n")

          # 更新語言狀態
          current_language(new_language)

          # 1. 重新載入 hints
          new_hints <- tryCatch({
            if (exists("load_hints") && is.function(load_hints)) {
              cat("📝 [Product Dev Module] 重新載入 hints，語言:", new_language, "\n")
              load_hints(language = new_language)
            } else {
              NULL
            }
          }, error = function(e) {
            cat("❌ [Product Dev Module] Hints 載入失敗:", e$message, "\n")
            NULL
          })
          current_hints(new_hints)

          # 2. 重新載入 prompts
          new_prompts <- tryCatch({
            if (exists("load_prompts") && is.function(load_prompts)) {
              cat("📝 [Product Dev Module] 重新載入 prompts，語言:", new_language, "\n")
              load_prompts(language = new_language)
            } else {
              NULL
            }
          }, error = function(e) {
            cat("❌ [Product Dev Module] Prompts 載入失敗:", e$message, "\n")
            NULL
          })
          current_prompts(new_prompts)

          # 3. 更新模組特定的語言文字
          new_lang_texts <- if (exists("get_module_language_content")) {
            get_module_language_content("product_dev_insightforge", new_language)
          } else if (!is.null(current_lang_content$content$modules$product_dev)) {
            current_lang_content$content$modules$product_dev
          } else {
            NULL
          }

          if (!is.null(new_lang_texts)) {
            module_lang_texts(new_lang_texts)
            cat("✅ [Product Dev Module] 模組語言文字已更新\n")
          }

          cat("✅ [Product Dev Module] 語言切換完成\n")
        }
      }
    })

    # ── 📌 修復的輔助函數：智能處理多種 lang_texts 格式 ──────────────────────────────
    get_lang_value <- function(path, default = NULL) {
      cat("🔍 Product Dev get_lang_value: 開始取得路徑 '", path, "' 的內容\n", sep = "")

      if (is.null(lang_texts)) {
        error_msg <- paste0(
          "❌ [Product Dev Module] 語言內容錯誤:\n",
          "  - 錯誤類型: lang_texts 為 NULL\n",
          "  - 請求路徑: ", path, "\n",
          "  - 使用預設: ", default
        )
        cat(error_msg, "\n")
        warning(error_msg)
        return(default)
      }

      # 獲取實際的 lang_texts 值
      texts <- if (is.function(lang_texts)) {
        tryCatch({
          result <- lang_texts()  # 如果是 reactive，調用它
          cat("📌 Product Dev get_lang_value: 獲取 reactive lang_texts 成功，語言:", result$language %||% "未知", "\n")
          result
        }, error = function(e) {
          error_msg <- paste0(
            "❌ [Product Dev Module] Reactive 語言內容錯誤:\n",
            "  - 錯誤類型: reactive 調用失敗\n",
            "  - 請求路徑: ", path, "\n",
            "  - 錯誤訊息: ", e$message, "\n",
            "  - 使用預設: ", default
          )
          cat(error_msg, "\n")
          warning(error_msg)
          NULL
        })
      } else {
        cat("📌 Product Dev get_lang_value: 使用靜態 lang_texts，語言:", lang_texts$language %||% "未知", "\n")
        lang_texts  # 如果不是，直接使用
      }

      if (is.null(texts)) {
        error_msg <- paste0(
          "❌ [Product Dev Module] 語言內容載入錯誤:\n",
          "  - 錯誤類型: texts 為 NULL\n",
          "  - 請求路徑: ", path, "\n",
          "  - 使用預設: ", default
        )
        cat(error_msg, "\n")
        warning(error_msg)
        return(default)
      }

      # 🔍 智能檢測 lang_texts 的格式
      module_content <- NULL

      # 情況 1: 完整語言對象 (有 content.modules 結構)
      if (!is.null(texts$content) && !is.null(texts$content$modules)) {
        cat("📋 Product Dev get_lang_value: 檢測到完整語言對象格式\n")
        if (!is.null(texts$content$modules$product_dev)) {
          module_content <- texts$content$modules$product_dev
          cat("✅ Product Dev get_lang_value: 找到模組內容 - product_dev\n")
        } else {
          error_msg <- paste0(
            "❌ [Product Dev Module] 模組查找錯誤:\n",
            "  - 錯誤類型: 找不到 product_dev 模組\n",
            "  - 請求路徑: ", path, "\n",
            "  - 可用模組: ", paste(names(texts$content$modules), collapse = ", "), "\n",
            "  - 使用預設: ", default
          )
          cat(error_msg, "\n")
          warning(error_msg)
          return(default)
        }
      }
      # 情況 2: 直接的模組內容 (主應用已提取過的模組特定內容)
      else if (!is.null(texts$title) || !is.null(texts$strategy) || !is.null(texts$analysis)) {
        cat("📋 Product Dev get_lang_value: 檢測到直接模組內容格式\n")
        module_content <- texts
      }
      # 情況 3: 可能的其他結構
      else {
        cat("📋 Product Dev get_lang_value: 未知格式，嘗試直接使用\n")
        module_content <- texts
      }

      if (!is.null(module_content)) {
        # 處理嵌套路徑 (e.g., "strategy.title" or "strategy$title")
        paths <- if (grepl("[$]", path)) {
          unlist(strsplit(path, "[$]", perl = TRUE))
        } else {
          unlist(strsplit(path, "[.]", perl = TRUE))
        }

        value <- module_content
        cat("    - 嘗試路徑:", paste(paths, collapse = " -> "), "\n")

        for (i in seq_along(paths)) {
          p <- paths[i]
          if (!is.null(value) && is.list(value) && p %in% names(value)) {
            value <- value[[p]]
            cat("      ✅ 步驟", i, ":", p, "成功\n")
          } else {
            available_keys <- if(!is.null(value) && is.list(value)) names(value) else NULL
            error_msg <- paste0(
              "❌ [Product Dev Module] 路徑解析錯誤:\n",
              "  - 錯誤類型: 找不到路徑元素\n",
              "  - 完整路徑: ", path, "\n",
              "  - 失敗步驟: ", i, " (", p, ")\n",
              "  - 可用鍵值: ",
              if(!is.null(available_keys)) paste(available_keys, collapse = ", ") else "無(非列表或NULL)", "\n",
              "  - 使用預設: ", default
            )
            cat(error_msg, "\n")
            warning(error_msg)
            return(default)
          }
        }

        cat("    ✅ 最終值:", if(!is.null(value)) value else "NULL", "\n")

        if (is.null(value)) {
          error_msg <- paste0(
            "❌ [Product Dev Module] 值為 NULL 錯誤:\n",
            "  - 錯誤類型: 路徑存在但值為 NULL\n",
            "  - 完整路徑: ", path, "\n",
            "  - 使用預設: ", default
          )
          cat(error_msg, "\n")
          warning(error_msg)
          return(default)
        }

        return(value)
      }

      error_msg <- paste0(
        "❌ [Product Dev Module] 最終錯誤:\n",
        "  - 錯誤類型: 無法找到模組內容\n",
        "  - 請求路徑: ", path, "\n",
        "  - 使用預設: ", default
      )
      cat(error_msg, "\n")
      warning(error_msg)
      return(default)
    }

    # ── 🔄 Reactive: 動態 UI 生成 - 主要的 renderUI ──────────────────────────────
    # ⚠️ DISABLED: UI is now static and rebuilt by outer renderUI in app_dynamic.R
    # This eliminates double-nesting issue that broke language switching
    if (FALSE) {
    output$product_dev_ui_container <- renderUI({
      # ⚠️ CRITICAL: Read ALL reactive dependencies at the TOP to establish proper reactive chain
      # This MUST be before any conditionals or they won't trigger re-execution

      # 1. Read trigger value (this forces re-execution when language changes)
      trigger_val <- if (exists("module_ui_update_trigger") && is.function(module_ui_update_trigger)) {
        module_ui_update_trigger()
      } else {
        0
      }

      # 2. Read lang_texts reactive (this also triggers re-execution)
      texts <- if (is.function(lang_texts)) {
        tryCatch({
          lang_texts()  # Read reactive WITHOUT isolate()
        }, error = function(e) {
          NULL
        })
      } else {
        NULL
      }

      cat("\n🎯 [Product Dev Module renderUI] 開始渲染 UI (trigger:", trigger_val, ")\n")

      # 3. Extract language from texts
      current_language_val <- if (!is.null(texts) && !is.null(texts$language)) {
        texts$language
      } else {
        "zh_TW"
      }

      cat("  📖 [Product Dev renderUI] lang_texts 語言:", current_language_val, "\n")
      cat("🌍 Product Dev renderUI: 當前語言 =", current_language_val, "\n")

      # 載入對應語言的 hints
      hints_df <- tryCatch({
        if (exists("load_hints") && is.function(load_hints)) {
          result <- load_hints(language = current_language_val)
          cat("📊 Product Dev renderUI - 載入hints語言:", current_language_val, " - 提示數量:", if(!is.null(result)) nrow(result) else 0, "\n")
          result
        } else {
          cat("⚠️ Product Dev renderUI - load_hints 函數不存在\n")
          NULL
        }
      }, error = function(e) {
        cat("❌ Product Dev renderUI - hints載入失敗:", e$message, "\n")
        NULL
      })

      # 使用語言內容（如果有提供）- 使用點號分隔符以相容 YAML 結構
      title_text <- get_lang_value("title", "🚀 New Product Development Recommendations")
      subtitle_text <- get_lang_value("subtitle", "Based on market attribute analysis, provide new product development direction recommendations")
      strategy_title_text <- get_lang_value("strategy.title", "🎯 Development Strategy Settings")
      strategy_approach_text <- get_lang_value("strategy.approach", "Development Strategy:")
      priority_count_text <- get_lang_value("strategy.priority_count", "Priority Development Count:")
      analyze_button_text <- get_lang_value("analysis.button", "Analyze Development Opportunities")

      # Strategy choices with English fallbacks (removed: innovation, compete)
      strategy_choices <- setNames(
        c("gap", "strength"),
        c(get_lang_value("strategies.gap", "Fill Market Gap"),
          get_lang_value("strategies.strength", "Strengthen Advantage Areas"))
      )

      # Segment choices removed - target segment handled in BrandEdge

      cat("\n📝 [Product Dev Module renderUI] 語言內容檢查:\n")
      cat("  ✅ 標題:", title_text, "\n")
      cat("  ✅ 子標題:", subtitle_text, "\n")
      cat("  ✅ 分析按鈕:", analyze_button_text, "\n")
      cat("\n🎨 [Product Dev Module renderUI] 生成完整 UI 結構\n")

      fluidRow(
        bs4Card(
          title = title_text,
          status = "success",
          width = 12,
          solidHeader = TRUE,
          elevation = 3,

          fluidRow(
            column(12,
              p(subtitle_text, style = "color: #666; margin-bottom: 20px;"),

              # 分析設定
              div(
                style = "background: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 20px;",
                h5(strategy_title_text),
                fluidRow(
                  column(6,
                    selectInput(ns("dev_strategy"),
                               strategy_approach_text,
                               choices = strategy_choices,
                               selected = "gap")
                  ),
                  column(6,
                    numericInput(ns("dev_priority"),
                                priority_count_text,
                               value = 3, min = 1, max = 5, step = 1)
                  )
                ),
                actionButton(ns("analyze_opportunities"),
                            analyze_button_text,
                            class = "btn-success", icon = icon("lightbulb"))
              ),

              # 市場缺口分析
              uiOutput(ns("market_gaps")),

              # 新品建議
              uiOutput(ns("product_suggestions")),

              # 開發路線圖
              uiOutput(ns("development_roadmap"))
            )
          )
        )
      )
    })  # END of renderUI
    }  # END of if (FALSE) - disabled inner renderUI

    # 儲存分析結果
    market_analysis <- reactiveVal(NULL)
    dev_suggestions <- reactiveVal(NULL)
    
    # 分析開發機會
    observeEvent(input$analyze_opportunities, {
      # 檢查依賴是否可用
      if (is.null(scored_data) || !is.function(scored_data)) {
        showNotification("⚠️ 評分資料未載入，請先執行評分分析", type = "warning", duration = 5)
        return()
      }

      data <- tryCatch({
        scored_data()
      }, error = function(e) {
        cat("  ⚠️  [Product Dev] scored_data() error:", e$message, "\n")
        NULL
      })

      req(data)

      notification_msg <- get_lang_value("analysis.processing", "🔄 Analyzing market opportunities...")
      showNotification(notification_msg, type = "message", duration = 3)

      tryCatch({
        # 計算所有品牌的屬性平均分
        attr_cols <- names(data)[!names(data) %in% "Variation"]
        attr_cols <- attr_cols[sapply(data[attr_cols], is.numeric)]

        cat("  📊 [Product Dev] Found", length(attr_cols), "numeric attribute columns\n")

        overall_mean <- data %>%
          summarise(across(all_of(attr_cols), \(x) mean(x, na.rm = TRUE)))

        # 找出低分屬性（市場缺口）
        attr_scores <- unlist(overall_mean)

        cat("  📊 [Product Dev] Calculated scores for", length(attr_scores), "attributes\n")
        cat("  📊 [Product Dev] Valid scores:", sum(!is.na(attr_scores) & !is.nan(attr_scores)), "\n")

        # 過濾掉 NA/NaN 的屬性分數
        attr_scores <- attr_scores[!is.na(attr_scores) & !is.nan(attr_scores)]

        # 如果沒有有效的屬性分數，顯示錯誤
        if (length(attr_scores) == 0) {
          dev_suggestions(get_lang_value("messages.no_valid_scores", "⚠️ 無法分析：所有屬性資料不完整，請檢查評分資料"))
          return()
        }

        low_score_attrs <- names(attr_scores)[attr_scores < median(attr_scores)]
        high_score_attrs <- names(attr_scores)[attr_scores >= median(attr_scores)]
        
        # 根據策略生成建議 (只保留 gap 和 strength)
        if (input$dev_strategy == "gap") {
          focus_attrs <- low_score_attrs
          strategy_desc <- get_lang_value("strategies.gap", "Fill Market Gap")
        } else if (input$dev_strategy == "strength") {
          focus_attrs <- high_score_attrs
          strategy_desc <- get_lang_value("strategies.strength", "Strengthen Advantage Areas")
        } else {
          # 預設使用 gap 策略
          focus_attrs <- low_score_attrs
          strategy_desc <- get_lang_value("strategies.gap", "Fill Market Gap")
        }
        
        # 使用集中式 Prompt 管理
        if (!is.null(prompts_df) && exists("prepare_gpt_messages")) {
          # 如果 prompts_df 是 reactive，取其值
          prompts_data <- if (is.function(prompts_df)) prompts_df() else prompts_df

          # 檢查 prompts_df 是否包含所需的 prompt
          if (is.null(prompts_data) || !"product_development" %in% prompts_data$var_id) {
            # 使用備用方案
            dev_suggestions(get_lang_value("messages.analysis_failed", "Based on analysis, suggested development focus:\n1. Improve low-score attributes\n2. Strengthen advantage areas\n3. Innovation breakthrough"))
          } else {
            messages <- prepare_gpt_messages(
              var_id = "product_development",
              variables = list(
                strategy = strategy_desc,
                focus_attributes = paste(head(focus_attrs, 5), collapse = ", "),
                market_gaps = paste(low_score_attrs, collapse = ", "),
                strength_areas = paste(high_score_attrs, collapse = ", "),
                priority_count = input$dev_priority
              ),
              prompts_df = prompts_data
            )
            
            if (exists("chat_api")) {
              suggestions <- chat_api(messages)
              dev_suggestions(suggestions)
            } else {
              dev_suggestions(get_lang_value("console.analysis_done", "GPT API not configured, using basic recommendations"))
            }
          }
        } else {
          # 沒有 prompt 管理系統的備用方案
          basic_suggestions <- paste(
            get_lang_value("console.identifying_gaps", "Recommended development directions:"),
            "• Fill market gaps",
            "• Strengthen competitive advantages",
            "• Innovate product features",
            sep = "\n"
          )
          dev_suggestions(basic_suggestions)
        }
        
        # 顯示市場缺口分析（根據策略調整顯示內容）
        output$market_gaps <- renderUI({
          # 根據策略決定顯示重點
          # 根據策略決定顯示重點
          default_titles <- list(
            gap = "📊 市場缺口分析（重點開發）",
            strength = "📊 競爭優勢分析（強化領域）"
          )

          title_text <- get_lang_text("market_gaps$title",
                                      default_titles[[input$dev_strategy]] %||% "📊 市場機會分析")
          
          # 根據策略調整顯示順序和內容
          if (input$dev_strategy == "gap") {
            # 缺口策略：優先顯示低分屬性
            content <- div(
              p(strong(get_lang_value("market_gaps.focus_improve", "🎯 重點改進屬性（市場缺口）："))),
              tags$ul(
                lapply(head(low_score_attrs, min(5, length(low_score_attrs))), function(attr) {
                  tags$li(
                    style = "color: #dc3545;",
                    paste(attr, "-", get_lang_value("market_gaps.avg_score", "平均分"), ":", round(attr_scores[attr], 2), get_lang_value("market_gaps.urgent_improve", "（急需改進）"))
                  )
                })
              ),
              if (length(high_score_attrs) > 0) {
                tagList(
                  p(strong(get_lang_value("market_gaps.existing_advantages", "✅ 現有優勢（保持）："))),
                  tags$ul(
                    lapply(head(high_score_attrs, 2), function(attr) {
                      tags$li(paste(attr, "-", get_lang_value("market_gaps.avg_score", "平均分"), ":", round(attr_scores[attr], 2)))
                    })
                  )
                )
              }
            )
          } else if (input$dev_strategy == "strength") {
            # 優勢策略：優先顯示高分屬性
            content <- div(
              p(strong(get_lang_value("market_gaps.core_strengths", "⭐ 核心優勢（繼續強化）："))),
              tags$ul(
                lapply(head(high_score_attrs, min(5, length(high_score_attrs))), function(attr) {
                  tags$li(
                    style = "color: #28a745;",
                    paste(attr, "-", get_lang_value("market_gaps.avg_score", "平均分"), ":", round(attr_scores[attr], 2), get_lang_value("market_gaps.competitive_advantage", "（競爭優勢）"))
                  )
                })
              ),
              if (length(low_score_attrs) > 0) {
                tagList(
                  p(strong(get_lang_value("market_gaps.areas_to_improve", "⚠️ 待改進領域："))),
                  tags$ul(
                    lapply(head(low_score_attrs, 2), function(attr) {
                      tags$li(paste(attr, "-", get_lang_value("market_gaps.avg_score", "平均分"), ":", round(attr_scores[attr], 2)))
                    })
                  )
                )
              }
            )
          } else {
            # 其他策略：平衡顯示
            content <- div(
              p(strong(get_lang_value("market_gaps.market_gaps_low", "市場缺口（低分屬性）："))),
              tags$ul(
                lapply(head(low_score_attrs, 3), function(attr) {
                  tags$li(paste(attr, "-", get_lang_value("market_gaps.avg_score", "平均分"), ":", round(attr_scores[attr], 2)))
                })
              ),
              p(strong(get_lang_value("market_gaps.competitive_high", "競爭優勢（高分屬性）："))),
              tags$ul(
                lapply(head(high_score_attrs, 3), function(attr) {
                  tags$li(paste(attr, "-", get_lang_value("market_gaps.avg_score", "平均分"), ":", round(attr_scores[attr], 2)))
                })
              )
            )
          }

          div(
            style = "background: #fff3cd; border: 1px solid #ffc107; border-radius: 8px; padding: 15px; margin-bottom: 20px;",
            h5(title_text, style = "color: #856404;"),
            div(style = "margin-top: 10px;", content),
            # 加入策略說明
            tags$hr(),
            p(
              style = "font-size: 0.9em; color: #666;",
              strong(get_lang_value("market_gaps.current_strategy", "當前策略：")),
              get_lang_value(paste0("strategy_descriptions.", input$dev_strategy),
                switch(input$dev_strategy,
                  "gap" = "填補市場空缺 - 優先改進低分屬性",
                  "strength" = "強化優勢領域 - 繼續提升高分屬性",
                  input$dev_strategy
                ))
            )
          )
        })
        
        # 顯示新品建議
        output$product_suggestions <- renderUI({
          req(dev_suggestions())
          
          div(
            style = "background: #d4edda; border: 1px solid #28a745; border-radius: 8px; padding: 15px; margin-bottom: 20px;",
            h5(get_lang_text("suggestions$title", "💡 新品開發建議"), style = "color: #155724;"),
            div(
              style = "background: white; padding: 10px; border-radius: 5px; margin-top: 10px;",
              HTML(markdown::markdownToHTML(text = dev_suggestions(), fragment.only = TRUE))
            )
          )
        })
        
        # 顯示開發路線圖（根據策略和選擇動態調整）
        output$development_roadmap <- renderUI({
          # 根據策略決定行動方向 - 使用語言管理的文字
          action_verb <- switch(input$dev_strategy,
            "gap" = get_lang_value("action_verbs.gap", "Improve"),
            "strength" = get_lang_value("action_verbs.strength", "Strengthen"),
            get_lang_value("console.generating_concepts", "Develop")
          )

          div(
            style = "background: #e7f3ff; border: 1px solid #007bff; border-radius: 8px; padding: 15px;",
            h5(paste("🗓️", get_lang_value("roadmap.title", "Development Priority")), style = "color: #004085;"),
            div(
              style = "margin-top: 10px;",
              tags$ol(
                lapply(1:min(input$dev_priority, length(focus_attrs)), function(i) {
                  # 根據策略設定不同的提升潛力範圍
                  potential_range <- switch(input$dev_strategy,
                    "gap" = c(30, 60),      # 缺口策略：高潛力
                    "strength" = c(20, 40),  # 優勢策略：穩定提升
                    c(20, 50)
                  )
                  
                  # 設定不同優先級的顏色
                  priority_color <- if(i == 1) "#dc3545" else if(i == 2) "#ffc107" else "#28a745"
                  
                  tags$li(
                    style = paste0("margin-bottom: 10px; padding: 8px; background: rgba(0,123,255,0.05); border-left: 3px solid ", priority_color, ";"),
                    strong(paste(get_lang_value("roadmap.priority", "Priority"), i, ":", action_verb, focus_attrs[i])),
                    tags$br(),
                    tags$small(
                      style = "color: #666;",
                      paste("•", get_lang_value("console.evaluating_opportunities", "Expected Enhancement Potential"), ":",
                           round(runif(1, potential_range[1], potential_range[2])), "%"),
                      tags$br(),
                      paste("•", get_lang_value("roadmap.timeline", "Recommended Timeline"), ":",
                           if(i == 1) get_lang_value("console.concepts_generated", "3 months") else if(i == 2) "6 months" else "9 months")
                    )
                  )
                })
              ),
              # 加入總體時程建議
              tags$hr(),
              p(
                style = "font-size: 0.9em; color: #666; margin-top: 10px;",
                strong(paste("💡", get_lang_value("console.generating_concepts", "Development Recommendations"), ":")), " ",
                switch(input$dev_strategy,
                  "gap" = get_lang_value("strategy_details.gap", "Prioritize filling market gaps, rapidly respond to customer needs"),
                  "strength" = get_lang_value("strategy_details.strength", "Continuously strengthen competitive advantages, consolidate market position"),
                  get_lang_value("strategy_details.default", "Formulate clear product development strategy")
                )
              )
            )
          )
        })
        
        success_msg <- get_lang_value("analysis.complete", "✅ New product development recommendations generated!")
        showNotification(success_msg, type = "message", duration = 3)
        
      }, error = function(e) {
        error_msg <- get_lang_value("messages.analysis_failed", "Analysis failed: {error}")
        error_msg <- gsub("\\{error\\}", e$message, error_msg)
        showNotification(error_msg, type = "error")
      })
    })

    # 🔄 語言切換監聽器 - 當語言改變時重新織染 UI
    observe({
      if (is.function(lang_texts)) {
        # 監聽語言變更
        current_lang <- lang_texts()
        if (!is.null(current_lang) && !is.null(current_lang$language)) {
          cat("🔄 Product Dev 語言切換監聽器: 語言已變更為", current_lang$language, "\n")
          # 觸發 renderUI 重新織染（直接調用 observe 就會觸發）
        }
      }
    })
  })
}

# 為了與 app_insightforge.R 保持兼容，創建 Server 函數別名
productDevModuleServer <- product_dev_insightforgeModuleServer
