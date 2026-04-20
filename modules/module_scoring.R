################################################################################
# InsightForge 評分模組 v2 - 使用集中式 Prompt 管理
# 更新：2025-01-08 移除平行評分（CLOUD_P001 - Shiny Cloud 不支援）
################################################################################

# Note: future/furrr removed per CLOUD_P001 - parallel processing prohibited in Shiny Cloud

# 注意：add_info_icon 和 add_hint 函數應該從 hint_system.R 載入
# 如果這些函數不存在，模組仍可運作但不會顯示提示

#' Attribute Generation & Scoring Module – UI
scoreModuleUI <- function(id, module_config = NULL, lang_texts = NULL) {
  ns <- NS(id)

  cat("🚀 [Score Module UI] 初始化，語言:",
      if(!is.null(lang_texts) && !is.null(lang_texts$language)) lang_texts$language else "未知", "\n")

  # Get current language for hints
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

  # Get module config
  module_config <- get_score_module_config(module_config)
  attr_config <- module_config$attribute_extraction$num_attributes %||% list(min = 10, max = 30, default = 15, step = 1)
  scoring_config <- module_config$scoring$review_sampling %||% list(min = 2, max = 500, default = 50, step = 10)

  # Helper function to get text from lang_texts
  get_lang_value <- function(path, default) {
    if (is.null(lang_texts)) return(default)
    parts <- strsplit(path, "\\$|\\.")[[1]]
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
  title_text <- get_lang_value("title", "📊 屬性評分分析")
  subtitle_text <- get_lang_value("subtitle", "分析產品屬性並計算評分")
  gen_attr_text <- get_lang_value("buttons$generate_attributes", "產生屬性")
  start_scoring_text <- get_lang_value("buttons$start_scoring", "開始評分")
  next_button_text <- get_lang_value("buttons$proceed", "下一步 ➡️")

  # Section titles
  attributes_section_text <- get_lang_value("settings$attributes", "1. 選擇屬性數量")
  generated_attrs_text <- get_lang_value("settings$generated_attributes", "2. 生成的屬性")
  scoring_section_text <- get_lang_value("settings$scoring_title", "3. 評分")
  sample_size_text <- get_lang_value("settings$sample_size", "每個品牌評論抽樣數量：")

  # 📌 Return static UI (will be rebuilt by outer renderUI on language change)
  div(id = ns("step2_box"),
    h4(title_text),
    p(subtitle_text, style = "color: #666; margin-bottom: 20px;"),

    # 1. Attribute count selection
    h5(attributes_section_text,
       if (exists("add_info_icon")) add_info_icon("num_attributes", hints_df)),
    if (exists("add_hint")) {
      add_hint(
        sliderInput(ns("num_attributes"), NULL,
                    min = attr_config$min %||% 10,
                    max = attr_config$max %||% 30,
                    value = attr_config$default %||% 15,
                    step = attr_config$step %||% 1,
                    ticks = FALSE),
        "num_attributes", hints_df
      )
    } else {
      sliderInput(ns("num_attributes"), NULL,
                  min = attr_config$min %||% 10,
                  max = attr_config$max %||% 30,
                  value = attr_config$default %||% 15,
                  step = attr_config$step %||% 1,
                  ticks = FALSE)
    },

    if (exists("add_hint")) {
      add_hint(
        actionButton(ns("gen_facets"), gen_attr_text, class = "btn-secondary"),
        "gen_facets", hints_df
      )
    } else {
      actionButton(ns("gen_facets"), gen_attr_text, class = "btn-secondary")
    },

    br(), br(),

    # 2. Generated attributes display
    h5(generated_attrs_text),
    verbatimTextOutput(ns("facet_msg")),
    br(),

    # 3. Scoring section
    h5(scoring_section_text,
       if (exists("add_info_icon")) add_info_icon("scoring_section", hints_df)),
    if (exists("add_hint")) {
      add_hint(
        sliderInput(ns("nrows"), sample_size_text,
                    min = scoring_config$min %||% 2,
                    max = scoring_config$max %||% 500,
                    value = scoring_config$default %||% 50,
                    step = scoring_config$step %||% 10,
                    ticks = FALSE),
        "nrows", hints_df
      )
    } else {
      sliderInput(ns("nrows"), sample_size_text,
                  min = scoring_config$min %||% 2,
                  max = scoring_config$max %||% 500,
                  value = scoring_config$default %||% 50,
                  step = scoring_config$step %||% 10,
                  ticks = FALSE)
    },

    if (exists("add_hint")) {
      add_hint(
        actionButton(ns("score"), start_scoring_text, class = "btn-primary"),
        "score_button", hints_df
      )
    } else {
      actionButton(ns("score"), start_scoring_text, class = "btn-primary")
    },
    br(), br(),

    # Score results table
    DTOutput(ns("score_tbl")),
    br(),

    # Proceed button
    actionButton(ns("to_step3"), next_button_text, class = "btn-info"),

    # Help markdown
    br(), br(),
    div(
      style = "border-top: 1px solid #ddd; margin-top: 20px; padding-top: 15px;",
      uiOutput(ns("score_help_markdown"))
    )
  )
}

#' Score Module 的靜態配置獲取函數
get_score_module_config <- function(module_config = NULL) {
  # 載入模組配置（如果沒有傳入，嘗試從全域載入）
  if (is.null(module_config)) {
    if (exists("app_config") && !is.null(app_config$module_configs$scoring)) {
      module_config <- app_config$module_configs$scoring
    } else {
      # 使用預設值
      module_config <- list(
        attribute_extraction = list(
          num_attributes = list(min = 10, max = 30, default = 15, step = 1)
        ),
        scoring = list(
          review_sampling = list(min = 2, max = 500, default = 50, step = 10)
        )
      )
    }
  }
  return(module_config)
}

# 📌 這些輔助函數將移動到 Server 端

# 📌 UI 內容已移動到 Server 端的 renderUI

# NULL 合併運算子
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

#' Attribute Generation & Scoring Module – Server
#' 使用集中式 Prompt 管理系統
scoreModuleServer <- function(id, con, user_info, raw_data, module_config = NULL, lang_texts = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    cat("🎬 [Score Module Server] 開始初始化...\n")

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
    lang_texts_loaded <- reactiveVal(FALSE)  # 追蹤 lang_texts 是否已載入

    cat("📝 [Score Module] 初始化語言:", initial_lang, "\n")

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
        current_lang <- isolate(current_language())
        is_loaded <- isolate(lang_texts_loaded())

        # 更新條件：語言改變 OR 首次載入（確保初始化時也更新一次）
        should_update <- (new_language != current_lang) || !is_loaded

        if (should_update) {
          if (!is_loaded) {
            cat("\n🔧 [Score Module] 首次載入語言內容:", new_language, "\n")
          } else {
            cat("\n📢 [Score Module] 偵測到語言變更:", current_lang, "->", new_language, "\n")
          }

          # 更新語言狀態
          current_language(new_language)

          # 1. 重新載入 hints
          new_hints <- tryCatch({
            if (exists("load_hints") && is.function(load_hints)) {
              cat("📝 [Score Module] 重新載入 hints，語言:", new_language, "\n")
              load_hints(language = new_language)
            } else {
              NULL
            }
          }, error = function(e) {
            cat("❌ [Score Module] Hints 載入失敗:", e$message, "\n")
            NULL
          })
          current_hints(new_hints)

          # 2. 重新載入 prompts
          new_prompts <- tryCatch({
            if (exists("load_prompts") && is.function(load_prompts)) {
              cat("📝 [Score Module] 重新載入 prompts，語言:", new_language, "\n")
              load_prompts(language = new_language)
            } else {
              NULL
            }
          }, error = function(e) {
            cat("❌ [Score Module] Prompts 載入失敗:", e$message, "\n")
            NULL
          })
          current_prompts(new_prompts)

          # 3. 更新模組特定的語言文字
          # 使用 get_module_language_content 函數直接從 global_language_state 獲取
          if (exists("get_module_language_content") && is.function(get_module_language_content)) {
            module_content <- tryCatch({
              get_module_language_content("scoring", new_language)
            }, error = function(e) {
              cat("⚠️ [Score Module] get_module_language_content 失敗:", e$message, "\n")
              NULL
            })

            if (!is.null(module_content)) {
              lang_texts <<- module_content
              lang_texts$language <<- new_language
              lang_texts_loaded(TRUE)
              cat("✅ [Score Module] 模組語言文字已更新 (使用 get_module_language_content)\n")
            }
          } else {
            # 備用方案：嘗試多個可能的模組名稱
            possible_names <- c("scoring", "scoring", "score")
            for (name in possible_names) {
              if (!is.null(current_lang_content$content$modules[[name]])) {
                lang_texts <<- current_lang_content$content$modules[[name]]
                lang_texts$language <<- new_language
                lang_texts_loaded(TRUE)
                cat("✅ [Score Module] 模組語言文字已更新 (使用:", name, ")\n")
                break
              }
            }
          }

          cat("✅ [Score Module] 語言切換完成\n")
        }
      }
    })

    # ============================================
    # 🔧 強制初始化語言內容（在 observe 之外執行一次）
    # ============================================
    # 立即檢查並載入語言內容，不等待 reactive 觸發
    cat("🔍 [Score Module] 檢查 global_lang_content 是否存在...\n")
    cat("  - exists('global_lang_content'):", exists("global_lang_content"), "\n")
    if (exists("global_lang_content")) {
      cat("  - is.function(global_lang_content):", is.function(get("global_lang_content", envir = .GlobalEnv)), "\n")
    }

    if (exists("global_lang_content") && is.function(global_lang_content)) {
      current_lang_content <- tryCatch(global_lang_content(), error = function(e) {
        cat("⚠️ [Score Module] 初始化時無法取得 global_lang_content:", e$message, "\n")
        NULL
      })

      # 優先使用 get_module_language_content 函數
      if (exists("get_module_language_content") && is.function(get_module_language_content)) {
        cat("🔧 [Score Module] 使用 get_module_language_content 初始化...\n")
        module_content <- tryCatch({
          get_module_language_content("scoring", initial_lang)
        }, error = function(e) {
          cat("⚠️ [Score Module] get_module_language_content 初始化失敗:", e$message, "\n")
          NULL
        })

        if (!is.null(module_content)) {
          lang_texts <<- module_content
          lang_texts$language <<- initial_lang
          lang_texts_loaded(TRUE)
          cat("✅ [Score Module] 初始化語言內容完成\n")
        }
      } else if (!is.null(current_lang_content) && !is.null(current_lang_content$content)) {
        # 備用方案：從 global_lang_content 獲取
        cat("🔧 [Score Module] 從 global_lang_content 初始化...\n")
        possible_names <- c("scoring", "scoring", "score")
        for (name in possible_names) {
          if (!is.null(current_lang_content$content$modules[[name]])) {
            cat("  📦 找到模組內容:", name, "\n")
            lang_texts <<- current_lang_content$content$modules[[name]]
            lang_texts$language <<- current_lang_content$language
            lang_texts_loaded(TRUE)
            cat("  ✅ 初始化語言內容載入完成\n")
            break
          }
        }
      } else {
        cat("⚠️ [Score Module] global_lang_content 尚未就緒，將等待 observer 觸發\n")
      }
    }

    # ── 載入模組配置 ──────────────────────────────────────────────
    module_config <- get_score_module_config(module_config)

    # 從配置取得參數
    attr_config <- module_config$attribute_extraction$num_attributes
    review_config <- module_config$scoring$review_sampling

    # ── 📌 修復的輔助函數：智能處理多種 lang_texts 格式 ──────────────────────────────
    get_lang_value <- function(path, default = NULL) {
      cat("🔍 Score get_lang_value: 開始取得路徑 '", path, "' 的內容\n", sep = "")

      # 建立錯誤報告
      error_details <- list()
      error_details$requested_path <- path
      error_details$attempted_steps <- character()

      if (is.null(lang_texts)) {
        error_msg <- paste0(
          "❌ [Score Module] 語言內容錯誤:\n",
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
          cat("📌 Score get_lang_value: 獲取 reactive lang_texts 成功，語言:", result$language %||% "未知", "\n")
          result
        }, error = function(e) {
          error_msg <- paste0(
            "❌ [Score Module] Reactive 語言內容錯誤:\n",
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
        cat("📌 Score get_lang_value: 使用靜態 lang_texts，語言:", lang_texts$language %||% "未知", "\n")
        lang_texts  # 如果不是，直接使用
      }

      if (is.null(texts)) {
        error_msg <- paste0(
          "❌ [Score Module] 語言內容載入錯誤:\n",
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
        cat("📋 Score get_lang_value: 檢測到完整語言對象格式\n")
        error_details$format_type <- "完整語言對象"
        error_details$available_modules <- names(texts$content$modules)

        # 嘗試多個可能的模組名稱
        possible_names <- c("scoring", "scoring", "score")
        for (name in possible_names) {
          if (name %in% names(texts$content$modules)) {
            module_content <- texts$content$modules[[name]]
            cat("✅ Score get_lang_value: 找到模組內容 -", name, "\n")
            error_details$matched_module <- name
            break
          }
        }

        if (is.null(module_content)) {
          error_msg <- paste0(
            "❌ [Score Module] 模組查找錯誤:\n",
            "  - 錯誤類型: 找不到匹配的模組\n",
            "  - 請求路徑: ", path, "\n",
            "  - 嘗試模組: ", paste(possible_names, collapse = ", "), "\n",
            "  - 可用模組: ", paste(names(texts$content$modules), collapse = ", "), "\n",
            "  - 使用預設: ", default
          )
          cat(error_msg, "\n")
          warning(error_msg)
          return(default)
        }
      }
      # 情況 2: 直接的模組內容 (主應用已提取過的模組特定內容)
      else if (!is.null(texts$title) || !is.null(texts$buttons) || !is.null(texts$settings)) {
        cat("📋 Score get_lang_value: 檢測到直接模組內容格式\n")
        error_details$format_type <- "直接模組內容"
        module_content <- texts
      }
      # 情況 3: 可能的其他結構
      else {
        cat("📋 Score get_lang_value: 未知格式，嘗試直接使用\n")
        error_details$format_type <- "未知格式"
        error_details$available_keys <- names(texts)
        module_content <- texts
      }

      if (!is.null(module_content)) {
        # 處理嵌套路徑 (e.g., "buttons$generate_attributes")
        # 同時支援 $ 和 . 作為分隔符
        paths <- if (grepl("[$]", path)) {
          unlist(strsplit(path, "[$]", perl = TRUE))
        } else {
          unlist(strsplit(path, "[.]", perl = TRUE))
        }

        value <- module_content
        cat("    - 嘗試路徑:", paste(paths, collapse = " -> "), "\n")
        error_details$path_components <- paths

        for (i in seq_along(paths)) {
          p <- paths[i]
          error_details$attempted_steps <- c(error_details$attempted_steps, p)

          if (!is.null(value) && is.list(value) && p %in% names(value)) {
            value <- value[[p]]
            cat("      ✅ 步驟", i, ":", p, "成功\n")
          } else {
            available_keys <- if(!is.null(value) && is.list(value)) names(value) else NULL

            error_msg <- paste0(
              "❌ [Score Module] 路徑解析錯誤:\n",
              "  - 錯誤類型: 找不到路徑元素\n",
              "  - 完整路徑: ", path, "\n",
              "  - 失敗步驟: ", i, " (", p, ")\n",
              "  - 已嘗試: ", paste(error_details$attempted_steps, collapse = " -> "), "\n",
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
            "❌ [Score Module] 值為 NULL 錯誤:\n",
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
        "❌ [Score Module] 最終錯誤:\n",
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
    output$score_ui_container <- renderUI({
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

      cat("\n🎯 [Score Module renderUI] 開始渲染 UI (trigger:", trigger_val, ")\n")

      # 3. Extract language from texts
      current_language_val <- if (!is.null(texts) && !is.null(texts$language)) {
        texts$language
      } else {
        "zh_TW"
      }

      cat("  📖 [Score renderUI] lang_texts 語言:", current_language_val, "\n")
      cat("🌍 Score renderUI: 當前語言 =", current_language_val, "\n")

      # 載入對應語言的 hints
      hints_df <- tryCatch({
        if (exists("load_hints") && is.function(load_hints)) {
          result <- load_hints(language = current_language_val)
          cat("📊 Score renderUI - 載入hints語言:", current_language_val, " - 提示數量:", if(!is.null(result)) nrow(result) else 0, "\n")
          result
        } else {
          cat("⚠️ Score renderUI - load_hints 函數不存在\n")
          NULL
        }
      }, error = function(e) {
        cat("❌ Score renderUI - hints載入失敗:", e$message, "\n")
        NULL
      })

      # 使用語言內容（如果有提供）- 使用點號分隔符以相容 YAML 結構
      title_text <- get_lang_value("title", "📊 屬性評分分析")
      subtitle_text <- get_lang_value("subtitle", "分析產品屬性並計算評分")
      gen_attr_text <- get_lang_value("buttons.generate_attributes", "產生屬性")
      start_scoring_text <- get_lang_value("buttons.start_scoring", "開始評分")
      next_button_text <- get_lang_value("buttons.proceed", "下一步 ➡️")

      cat("\n📝 [Score Module renderUI] 語言內容檢查:\n")
      cat("  ✅ 標題:", title_text, "\n")
      cat("  ✅ 子標題:", subtitle_text, "\n")
      cat("  ✅ 產生屬性按鈕:", gen_attr_text, "\n")
      cat("  ✅ 開始評分按鈕:", start_scoring_text, "\n")
      cat("  ✅ 下一步按鈕:", next_button_text, "\n")
      cat("\n🎨 [Score Module renderUI] 生成完整 UI 結構\n")

      div(id = ns("step2_box"),
        h4(title_text),
        p(subtitle_text, style = "color: #666; margin-bottom: 20px;"),

        # 1. 屬性數量選擇
        h5(get_lang_value("settings$attributes", "1. 選擇屬性數量"),
           if (exists("add_info_icon")) add_info_icon("num_attributes", hints_df)),
        if (exists("add_hint")) {
          add_hint(
            sliderInput(ns("num_attributes"), NULL,
                        min = attr_config$min %||% 10,
                        max = attr_config$max %||% 30,
                        value = attr_config$default %||% 15,
                        step = attr_config$step %||% 1,
                        ticks = FALSE),
            "num_attributes", hints_df
          )
        } else {
          sliderInput(ns("num_attributes"), NULL,
                      min = attr_config$min %||% 10,
                      max = attr_config$max %||% 30,
                      value = attr_config$default %||% 15,
                      step = attr_config$step %||% 1,
                      ticks = FALSE)
        },

        if (exists("add_hint")) {
          add_hint(
            actionButton(ns("gen_facets"), gen_attr_text, class = "btn-secondary"),
            "gen_facets", hints_df
          )
        } else {
          actionButton(ns("gen_facets"), gen_attr_text, class = "btn-secondary")
        },
        verbatimTextOutput(ns("facet_msg")),
        br(),

        # 2. 評論數量選擇
        h5(get_lang_value("settings$review_count", "2. 選擇要分析的顧客評論則數"),
           if (exists("add_info_icon")) add_info_icon("nrows", hints_df)),
        if (exists("add_hint")) {
          add_hint(
            sliderInput(ns("nrows"), NULL,
                        min = review_config$min %||% 2,
                        max = review_config$max %||% 500,
                        value = review_config$default %||% 50,
                        step = review_config$step %||% 10,
                        ticks = FALSE),
            "nrows", hints_df
          )
        } else {
          sliderInput(ns("nrows"), NULL,
                      min = review_config$min %||% 2,
                      max = review_config$max %||% 500,
                      value = review_config$default %||% 50,
                      step = review_config$step %||% 10,
                      ticks = FALSE)
        },

        # 3. 開始評分（加入提示）
        h5(get_lang_value("settings$start_scoring", "3. 請點擊 [開始評分]")),
        if (exists("add_hint")) {
          add_hint(
            actionButton(ns("score"), start_scoring_text, class = "btn-primary"),
            "score_button", hints_df
          )
        } else {
          actionButton(ns("score"), start_scoring_text, class = "btn-primary")
        },
        br(), br(),

        DTOutput(ns("score_tbl")),
        br(),
        actionButton(ns("to_step3"), next_button_text, class = "btn-info"),

        # 📝 添加動態 markdown 內容載入示例
        br(), br(),
        div(
          style = "border-top: 1px solid #ddd; margin-top: 20px; padding-top: 15px;",
          uiOutput(ns("score_help_markdown"))
        )
      )
    })  # END of renderUI
    }  # END of if (FALSE) - disabled inner renderUI

    # 📝 動態載入 Score 模組的幫助 markdown
    output$score_help_markdown <- renderUI({
      cat("📝 Score 模組: 載入幫助 markdown\n")

      # 獲取當前語言
      current_language <- if (is.function(lang_texts)) {
        tryCatch({
          texts <- lang_texts()
          if (!is.null(texts) && !is.null(texts$language)) texts$language else "zh_TW"
        }, error = function(e) "zh_TW")
      } else if (!is.null(lang_texts) && !is.null(lang_texts$language)) {
        lang_texts$language
      } else {
        "zh_TW"
      }

      # 獲取全域語言內容（如果可用）
      current_lang_content <- if (exists("global_lang_content") && is.function(global_lang_content)) {
        try(global_lang_content(), silent = TRUE)
      } else {
        NULL
      }

      if (!inherits(current_lang_content, "try-error") && !is.null(current_lang_content)) {
        cat("📝 Score markdown: 使用全域語言內容，語言 =", current_lang_content$language, "\n")

        # 使用語言管理器的 markdown 載入功能
        if (exists("includeLanguageMarkdown")) {
          tryCatch({
            includeLanguageMarkdown("brandscore_info.md", current_lang_content, fallback_path = "md/brandscore_info.md")
          }, error = function(e) {
            cat("❌ Score markdown 載入失敗:", e$message, "\n")
            div(
              class = "alert alert-info",
              style = "margin: 10px 0; font-size: 14px;",
              h6("💡 評分模組說明"),
              p("此模組使用 AI 技術分析顧客評論，為不同品牌變體計算屬性評分，幫助您了解各品牌在不同維度的表現。")
            )
          })
        } else {
          # 備用方案：使用 get_language_dir_from_config 動態獲取目錄
          language_folder <- if (exists("get_language_dir_from_config")) {
            get_language_dir_from_config(current_lang_content$language)
          } else {
            if (current_lang_content$language == "en_US") "english"
            else if (current_lang_content$language == "ja_JP") "japanese"
            else "chinese"
          }
          markdown_path <- file.path("database", "content", language_folder, "markdown", "brandscore_info.md")

          if (file.exists(markdown_path)) {
            includeMarkdown(markdown_path)
          } else if (file.exists("md/brandscore_info.md")) {
            includeMarkdown("md/brandscore_info.md")
          } else {
            div(
              class = "alert alert-info",
              style = "margin: 10px 0; font-size: 14px;",
              h6("💡 評分模組說明"),
              p("此模組使用 AI 技術分析顧客評論，為不同品牌變體計算屬性評分，幫助您了解各品牌在不同維度的表現。")
            )
          }
        }
      } else {
        # 無語言內容時的備用方案
        cat("📝 Score markdown: 無語言內容，使用預設\n")
        if (file.exists("md/brandscore_info.md")) {
          includeMarkdown("md/brandscore_info.md")
        } else {
          div(
            class = "alert alert-info",
            style = "margin: 10px 0; font-size: 14px;",
            h6("💡 評分模組說明"),
            p("此模組使用 AI 技術分析顧客評論，為不同品牌變體計算屬性評分，幫助您了解各品牌在不同維度的表現。")
          )
        }
      }
    })

    # 🔄 語言切換監聽器 - 當語言改變時重新織染 UI
    observe({
      if (is.function(lang_texts)) {
        # 監聽語言變更
        current_lang <- lang_texts()
        if (!is.null(current_lang) && !is.null(current_lang$language)) {
          cat("🔄 Score 語言切換監聽器: 語言已變更為", current_lang$language, "\n")
          # 觸發 renderUI 重新織染（直接調用 observe 就會觸發）
        }
      }
    })

    # ── Reactive: 載入 Prompts (在 reactive context 中) ─────────────
    prompts_reactive <- reactive({
      # 現在在 reactive context 中，可以安全調用 get_lang_value
      current_language <- if (is.function(lang_texts)) {
        tryCatch({
          texts <- lang_texts()
          if (!is.null(texts) && !is.null(texts$language)) texts$language else "zh_TW"
        }, error = function(e) "zh_TW")
      } else if (!is.null(lang_texts) && !is.null(lang_texts$language)) {
        lang_texts$language
      } else {
        "zh_TW"
      }
      cat("📋 Score module - 載入prompts語言:", current_language, "\n")

      if (exists("load_prompts") && is.function(load_prompts)) {
        load_prompts(language = current_language)
      } else {
        NULL
      }
    })

    # ── 反應式變數 ────────────────────────────────────────────────────────
    facets_rv    <- reactiveVal(NULL)
    working_data <- reactiveVal(NULL)   # scored data output
    
    # 取得全域 regression_trigger
    regression_trigger <- session$userData$regression_trigger
    
    # ── 輔助函數 ──────────────────────────────────────────────────────────
    safe_value <- function(txt) {
      txt <- trimws(txt)
      num <- stringr::str_extract(txt, "[1-5]")
      if (!is.na(num)) return(as.numeric(num))
      val <- suppressWarnings(as.numeric(txt))
      if (!is.na(val) && val >= 1 && val <= 5) return(val)
      NA_real_
    }
    
    # ── 產生屬性 ──────────────────────────────────────────────────────────
    observeEvent(input$gen_facets, {
      dat <- raw_data()
      req(nrow(dat) > 0)
      
      # 顯示 progress bar
      progress_title <- get_lang_value("messages$info$analyzing_attributes", "Analyzing attributes...")
      shinyWidgets::progressSweetAlert(
        session = session,
        id = "facet_progress",
        title = progress_title,
        display_pct = FALSE,
        value = 0
      )
      
      # 抽樣評論資料 - 每個 Variation 各抽樣（從配置讀取）
      sample_size <- module_config$attribute_extraction$sampling_for_extraction$records_per_variation %||% 30
      dat_sample <- dplyr::bind_rows(
        lapply(split(dat, dat$Variation), function(df) {
          df[sample(seq_len(nrow(df)), min(sample_size, nrow(df))), , drop=FALSE]
        })
      )
      
      # 準備評論資料為 JSON
      sample_txt <- jsonlite::toJSON(dat_sample, dataframe = "rows", auto_unbox = TRUE)
      
      # 使用集中式 Prompt 管理系統
      messages <- tryCatch({
        prepare_gpt_messages(
          var_id = "extract_attributes",
          variables = list(
            num_attributes = input$num_attributes,
            sample_reviews = sample_txt
          ),
          prompts_df = prompts_reactive()
        )
      }, error = function(e) {
        # 使用語言內容或預設文字
        prompt_failed_msg <- get_lang_value("console$prompt_failed", "❌ Prompt 準備失敗:")
        error_msg <- paste(prompt_failed_msg, e$message)
        cat(error_msg, "\n")
        NULL
      })
      
      if (is.null(messages)) {
        shinyWidgets::closeSweetAlert(session = session)
        # 使用語言內容或預設文字
        error_msg <- get_lang_value("messages$prompt_failed", "❌ 無法準備 prompt")
        showNotification(error_msg, type = "error")
        return()
      }
      
      # 呼叫 chat API
      cat("📞 [Score Module] 準備呼叫 chat_api...\n")
      txt <- try(chat_api(messages), silent = TRUE)
      cat("📞 [Score Module] chat_api 呼叫完成\n")

      if (inherits(txt, "try-error")) {
        cat("❌ [Score Module] chat_api 失敗!\n")
        shinyWidgets::closeSweetAlert(session = session)

        # 提取實際錯誤訊息
        actual_error <- as.character(txt)
        cat("🔍 [Score Module] 原始錯誤對象:\n")
        print(txt)
        cat("🔍 [Score Module] 轉換後錯誤訊息:", actual_error, "\n")

        if (length(actual_error) > 0) {
          actual_error <- actual_error[1]
        } else {
          actual_error <- "Unknown error"
        }

        # 使用語言內容或預設文字
        attr_gen_failed_msg <- get_lang_value("messages$error$attribute_generation_failed", "❌ 產生屬性失敗：{error}")
        error_msg <- gsub("\\{error\\}", actual_error, attr_gen_failed_msg)

        cat("📢 [Score Module] 最終錯誤訊息:", error_msg, "\n")

        showNotification(error_msg, type = "error", duration = 6)
        output$facet_msg <- renderText(error_msg)
        shinyjs::disable(ns("score"))
        return()
      }

      cat("✅ [Score Module] chat_api 成功，開始解析屬性...\n")
      cat("📝 [Score Module] API 返回內容:\n", txt, "\n")

      # 解析屬性 - 簡單按逗號分割（Prompt 已要求只返回逗號分隔格式）
      # 預期格式: attr1, attr2, attr3, attr4, attr5
      attrs <- unlist(strsplit(txt, ",")) |>
               trimws() |>
               unique()

      # 過濾掉空字串
      attrs <- attrs[nchar(attrs) > 0]

      cat("🔍 [Score Module] 解析結果:\n")
      cat("  - 解析出的屬性數量:", length(attrs), "\n")
      cat("  - 需要的屬性數量:", input$num_attributes, "\n")
      cat("  - 屬性列表:", paste(attrs, collapse = " | "), "\n")

      if (length(attrs) < input$num_attributes - 1) {
        cat("❌ [Score Module] 屬性數量不足!\n")
        shinyWidgets::closeSweetAlert(session = session)
        shinyjs::disable(ns("score"))

        # 使用專門的解析錯誤訊息並替換佔位符
        parse_error_template <- get_lang_value(
          "messages$error$attribute_parse_failed",
          "⚠️ 無法解析足夠的屬性（需要 {required} 個，僅解析出 {found} 個）"
        )

        parse_error_msg <- gsub("\\{required\\}", input$num_attributes,
                               gsub("\\{found\\}", length(attrs), parse_error_template))

        cat("📢 [Score Module] 解析錯誤訊息:", parse_error_msg, "\n")
        output$facet_msg <- renderText(parse_error_msg)

        # 顯示詳細通知 - 使用語言化訊息
        showNotification(
          parse_error_msg,
          type = "error",
          duration = 10
        )
        return()
      }
      
      # 限制屬性數量為用戶選擇的數量
      attrs <- head(attrs, input$num_attributes)

      # 儲存屬性
      facets_rv(attrs)
      shinyjs::enable(ns("score"))

      # 使用語言管理的訊息
      attr_generated_msg <- get_lang_value("messages$info$attributes_generated", "✅ Generated {count} attributes: {attrs}")
      facet_display_msg <- gsub("\\{count\\}", length(attrs),
                               gsub("\\{attrs\\}", paste(attrs, collapse = ", "), attr_generated_msg))
      output$facet_msg <- renderText(facet_display_msg)

      shinyWidgets::closeSweetAlert(session = session)

      # 使用語言管理的通知訊息
      attr_notify_msg <- get_lang_value("messages$info$attributes_generated_notify", "✅ Generated {count} attributes!")
      notification_msg <- gsub("\\{count\\}", length(attrs), attr_notify_msg)
      showNotification(notification_msg, type = "message", duration = 3)
    })
    
    # ── 開始評分 ──────────────────────────────────────────────────────────
    observeEvent(input$score, {
      shinyjs::disable(ns("score"))
      
      attrs <- facets_rv()
      req(length(attrs) > 0)
      
      df0 <- raw_data()
      req(!is.null(df0))
      
      # 抽樣評論 - 每個 Variation 各抽 input$nrows 筆
      df <- dplyr::bind_rows(
        lapply(split(df0, df0$Variation), function(d) {
          n <- min(input$nrows, nrow(d))
          if (n == 0) return(NULL)
          d[sample(seq_len(nrow(d)), n), , drop=FALSE]
        })
      )
      
      total <- nrow(df)

      # ========================================================================
      # 循序評分處理 + 批次評分（移除 furrr 平行處理 - CLOUD_P001）
      # - Shiny Cloud 環境不支援 furrr（closure 環境過大導致序列化失敗）
      # - 改用 for loop + 即時進度更新
      # - 使用 score_attributes (複數) 一次評所有屬性，減少 API 呼叫
      # ========================================================================

      message(sprintf("🚀 [Sequential Scoring] 開始循序評分 %d 則評論 × %d 個屬性", total, length(attrs)))

      # 預先準備 prompts_df
      prompts_df_local <- prompts_reactive()

      # 定義批次評分函數（一次評所有屬性）
      score_single_review <- function(row_idx, df_row, attrs_list, prompts_df_cache) {
        tryCatch({
          # 合併 Title 和 Body 為評論文字
          review_text <- paste(
            ifelse(is.na(df_row$Title) || df_row$Title == "", "", df_row$Title),
            ifelse(is.na(df_row$Body) || df_row$Body == "", "", df_row$Body),
            sep = " "
          )

          # 使用批次評分 prompt (score_attributes 複數)
          prompt_messages <- prepare_gpt_messages(
            var_id = "score_attributes",
            variables = list(
              attributes = paste(attrs_list, collapse = ", "),
              review_text = review_text
            ),
            prompts_df = prompts_df_cache
          )

          # 呼叫 API（一次評所有屬性）
          response <- chat_api(prompt_messages, timeout_sec = 60)

          # 解析 JSON 回應
          scores <- tryCatch({
            # 嘗試從回應中提取 JSON
            json_match <- regmatches(response, regexpr("\\{[^{}]*\\}", response))
            if (length(json_match) > 0) {
              parsed <- jsonlite::fromJSON(json_match)
              if (!is.null(parsed$scores)) {
                # 按照 attrs_list 順序提取分數
                sapply(attrs_list, function(attr) {
                  val <- parsed$scores[[attr]]
                  if (is.null(val) || is.na(val)) NA_real_ else as.numeric(val)
                })
              } else {
                # Fallback: 從回應中提取數字
                nums <- as.numeric(stringr::str_extract_all(response, "[1-5]")[[1]])
                if (length(nums) >= length(attrs_list)) nums[1:length(attrs_list)] else c(nums, rep(NA_real_, length(attrs_list) - length(nums)))
              }
            } else {
              # Fallback: 從回應中提取數字
              nums <- as.numeric(stringr::str_extract_all(response, "[1-5]")[[1]])
              if (length(nums) >= length(attrs_list)) nums[1:length(attrs_list)] else c(nums, rep(NA_real_, length(attrs_list) - length(nums)))
            }
          }, error = function(e) {
            rep(NA_real_, length(attrs_list))
          })

          # 確保長度正確
          if (length(scores) < length(attrs_list)) {
            scores <- c(scores, rep(NA_real_, length(attrs_list) - length(scores)))
          } else if (length(scores) > length(attrs_list)) {
            scores <- scores[1:length(attrs_list)]
          }

          scores_df <- as.data.frame(setNames(as.list(scores), attrs_list), check.names = FALSE)
          scores_df <- cbind(Variation = df_row$Variation, scores_df)
          list(success = TRUE, data = scores_df)
        }, error = function(e) {
          message(sprintf("⚠️ 評分錯誤 [%d]: %s", row_idx, e$message))
          na_vals <- rep(NA_real_, length(attrs_list))
          scores_df <- as.data.frame(setNames(as.list(na_vals), attrs_list), check.names = FALSE)
          scores_df <- cbind(Variation = df_row$Variation, scores_df)
          list(success = FALSE, data = scores_df)
        })
      }

      # 循序評分 + 即時進度更新
      progress_msg <- get_lang_value("messages$info$scoring_in_progress", "評分中，請稍候...")
      results_list <- withProgress(message = progress_msg, value = 0.1, {

        results <- list()
        for (i in seq_len(total)) {
          # 即時更新進度（讓用戶知道不是卡住）
          setProgress(
            value = 0.1 + 0.8 * (i / total),
            detail = sprintf("評分中... %d/%d (%s)", i, total, df$Variation[i])
          )

          results[[i]] <- score_single_review(i, df[i, ], attrs, prompts_df_local)
        }

        setProgress(value = 0.9, detail = "整理評分結果...")
        results
      })

      # 統計錯誤數
      error_count <- sum(!sapply(results_list, function(x) x$success))
      if (error_count > 0) {
        message(sprintf("⚠️ [Sequential Scoring] %d/%d 則評論評分失敗", error_count, total))
      }
      message(sprintf("✅ [Sequential Scoring] 完成！成功評分 %d/%d 則評論（API 呼叫數: %d）", total - error_count, total, total))

      # 提取結果 data.frame
      results_list <- lapply(results_list, function(x) x$data)
      
      # 合併結果
      result_df <- dplyr::bind_rows(results_list)

      # 顯示錯誤統計（如果有錯誤）
      if (error_count > 0) {
        # 使用語言內容或預設文字
        scoring_warning_msg <- get_lang_value("messages$warning$scoring_warning", "⚠️ 評分完成，但有 {error_count}/{total} 筆資料因 API 錯誤而跳過")
        warning_msg <- gsub("\\{error_count\\}", error_count, gsub("\\{total\\}", total, scoring_warning_msg))
        showNotification(warning_msg, type = "warning", duration = 10)
      } else {
        # 使用語言內容或預設文字
        scoring_success_msg <- get_lang_value("messages$info$scoring_success", "✅ 評分完成！成功處理 {total} 筆資料")
        success_msg <- gsub("\\{total\\}", total, scoring_success_msg)
        showNotification(success_msg, type = "message", duration = 5
        )
      }

      # 顯示評分結果
      show_df <- result_df[, c("Variation", attrs), drop = FALSE]

      # 記錄結果資訊
      message(sprintf("📊 評分結果: %d 筆資料, %d 個屬性", nrow(show_df), length(attrs)))

      # 儲存到 working_data 供其他模組使用
      working_data(result_df)
      cat("📦 [Score Module] working_data 已更新:", nrow(result_df), "筆資料\n")
      cat("  - Variations:", paste(unique(result_df$Variation), collapse = ", "), "\n")

      output$score_tbl <- DT::renderDT(show_df, selection = "none",
                                      options = list(pageLength = 10, scrollX = TRUE))
      
      # 啟用下一步
      shinyjs::enable(ns("to_step3"))
      
      # 使用 prompt 產生總結（選擇性）
      if (nrow(result_df) > 0) {
        # 計算統計資料
        score_distribution <- sapply(result_df[attrs], mean, na.rm = TRUE)
        
        # 產生評分總結
        summary_messages <- prepare_gpt_messages(
          var_id = "scoring_summary",
          variables = list(
            total_reviews = nrow(result_df),
            num_variations = length(unique(result_df$Variation)),
            attributes = paste(attrs, collapse = ", "),
            score_distribution = paste(
              names(score_distribution), ":", 
              round(score_distribution, 2), 
              collapse = "; "
            )
          ),
          prompts_df = prompts_reactive()
        )
        
        # 可選：呼叫 API 產生總結
        # summary_text <- try(chat_api(summary_messages), silent = TRUE)
        # if (!inherits(summary_text, "try-error")) {
        #   showNotification(summary_text, type = "message", duration = 10)
        # }
      }
      
      # 使用語言內容或預設文字
      success_msg <- get_lang_value("messages$info$scoring_complete", "✅ 評分完成並已存入 processed_data")
      showNotification(success_msg, type = "message")
      working_data(result_df)
    })
    
    # ── 下一步按鈕 ────────────────────────────────────────────────────────
    observeEvent(input$to_step3, {
      # 使用語言內容或預設文字
      merge_msg <- get_lang_value("messages$info$merging_data", "➡️ 進入資料合併！")
      showNotification(merge_msg, type = "message")
      if (!is.null(regression_trigger)) {
        regression_trigger(isolate(regression_trigger()) + 1)
      }
    })
    
    # ── 輸出 ──────────────────────────────────────────────────────────────
    list(
      scored_data = reactive(working_data()),
      proceed_step = reactive({ input$to_step3 })
    )
  })
}
