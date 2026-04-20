# BrandEdge Advanced Attribute Module
# 進階品牌屬性評價模組（10-30個屬性）
# Version: 2.0
# Last Updated: 2025-10-10
#
# Features:
#   • Generate 10-30 product attributes from reviews
#   • Score reviews against extracted attributes
#   • Support multi-language interface
#   • Integrate with centralized prompt system

# Load shared functions if not already loaded
if (!exists("get_lang_text")) {
  source("modules/module_brandedge_shared.R")
}

# ========== UI Function ==========

advancedAttributeModuleUI <- function(id, module_config = NULL, lang_texts = NULL) {
  ns <- NS(id)

  # Get language content - support both reactive and static lang_texts
  module_lang_content <- tryCatch(
    if (is.function(lang_texts)) lang_texts() else lang_texts,
    error = function(e) NULL
  )

  # Helper function to get translated text
  get_text <- function(path, default) {
    if (is.null(module_lang_content)) return(default)

    # Navigate nested structure using dot notation
    keys <- strsplit(path, "\\.")[[1]]
    value <- module_lang_content

    for (key in keys) {
      if (is.list(value) && key %in% names(value)) {
        value <- value[[key]]
      } else {
        return(default)
      }
    }

    if (is.character(value)) return(value) else return(default)
  }

  # UI Layout
  tagList(
    # Info box
    box(
      title = get_text("modules.advanced_attribute.title", "進階品牌屬性評價"),
      status = "info",
      solidHeader = TRUE,
      width = 12,
      p(get_text("modules.advanced_attribute.description", "從評論中萃取10-30個產品屬性並進行評分")),

      fluidRow(
        column(6,
          numericInput(
            ns("num_attributes"),
            get_text("analysis.num_attributes", "屬性數量"),
            value = 15,
            min = 10,
            max = 30
          )
        ),
        column(6,
          br(),
          actionButton(
            ns("generate_attributes"),
            get_text("buttons.generate_attributes", "產生屬性"),
            class = "btn-primary",
            icon = icon("wand-magic-sparkles")
          )
        )
      )
    ),

    # Attributes display
    box(
      title = get_text("analysis.attributes", "產品屬性"),
      status = "success",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      uiOutput(ns("attributes_list"))
    ),

    # Scoring control
    box(
      title = get_text("analysis.attribute_scoring", "屬性評分"),
      status = "warning",
      solidHeader = TRUE,
      width = 12,
      actionButton(
        ns("start_scoring"),
        get_text("buttons.start_scoring", "開始評分"),
        class = "btn-success",
        icon = icon("star")
      ),
      br(),
      br(),
      DTOutput(ns("scoring_results"))
    )
  )
}

# ========== Server Function ==========

advancedAttributeModuleServer <- function(id, review_data, lang_texts = reactive(NULL), module_config = NULL, api_config = NULL) {
  moduleServer(id, function(input, output, session) {

    # 從共用 API 設定讀取模型（fallback 到 gpt-5-nano）
    cfg_ai_model <- if (!is.null(api_config$default_model)) {
      api_config$default_model
    } else {
      "gpt-5-nano"
    }

    # Reactive values
    rv <- reactiveValues(
      attributes = NULL,
      scores = NULL
    )

    # Helper function to get message text
    get_msg <- function(path, default) {
      lang_content <- tryCatch(lang_texts(), error = function(e) NULL)
      if (is.null(lang_content)) return(default)

      # Navigate nested structure
      keys <- strsplit(path, "\\.")[[1]]
      value <- lang_content

      for (key in keys) {
        if (is.list(value) && key %in% names(value)) {
          value <- value[[key]]
        } else {
          return(default)
        }
      }

      if (is.character(value)) return(value) else return(default)
    }

    # Note: chat_api() is sourced from module_brandedge_shared.R
    # which loads scripts/global_scripts/08_ai/fn_chat_api.R
    # This correctly handles GPT-5 Responses API format

    # ========== Generate Attributes Observer ==========
    observeEvent(input$generate_attributes, {
      req(review_data())

      num_attrs <- input$num_attributes
      progress_msg <- get_msg("system.messages.info.analyzing_reviews", "正在分析評論內容，萃取產品屬性中...")

      withProgress(message = progress_msg, value = 0, {
        incProgress(0.3)

        # Sample reviews for analysis
        sample_reviews <- review_data() %>%
          sample_n(min(50, nrow(.))) %>%
          pull(Body) %>%
          paste(collapse = " ")

        # Get current language
        current_lang <- tryCatch({
          lang_content <- lang_texts()
          lang_content$language %||% "chinese"
        }, error = function(e) "chinese")

        message("🌐 [Advanced Attribute Extraction] Using language: ", current_lang)

        # Prepare GPT messages using prompt_manager
        prompt_messages <- tryCatch({
          prepare_gpt_messages(
            var_id = "extract_attributes",
            language = current_lang,
            variables = list(
              num_attributes = num_attrs,
              sample_reviews = substr(sample_reviews, 1, 3000)
            )
          )
        }, error = function(e) {
          message("⚠️ Prompt loading failed, using fallback: ", e$message)
          list(
            list(role = "system", content = "You are a product attribute extraction expert."),
            list(role = "user", content = sprintf("Extract %d key product attributes from these reviews: %s\nReturn as JSON array: {\"attributes\": [\"attr1\", \"attr2\", ...]}", num_attrs, substr(sample_reviews, 1, 1000)))
          )
        })

        incProgress(0.6)

        # Call OpenAI API（使用共用 API 設定的模型）
        response <- tryCatch(
          chat_api(prompt_messages, model = cfg_ai_model),
          error = function(e) {
            message("❌ API error: ", e$message)
            # Fallback default attributes
            paste0('{"attributes": ["品質", "價格", "功能", "外觀", "耐用性", "使用便利性", ',
                   '"包裝", "客服", "配送速度", "性價比", "創新性", "安全性", ',
                   '"環保", "品牌信譽", "售後服務"]}')
          }
        )

        # Parse attributes
        attributes_parsed <- tryCatch({
          json_response <- jsonlite::fromJSON(response)
          if ("attributes" %in% names(json_response)) {
            json_response$attributes
          } else {
            # Try extracting from text
            attr_matches <- stringr::str_extract_all(response, '"([^"]+)"')[[1]]
            gsub('"', '', attr_matches)
          }
        }, error = function(e) {
          message("⚠️ JSON parse error: ", e$message)
          c("品質", "價格", "功能", "外觀", "耐用性", "使用便利性",
            "包裝", "客服", "配送速度", "性價比", "創新性", "安全性",
            "環保", "品牌信譽", "售後服務")
        })

        # Ensure we have the right number of attributes
        if (length(attributes_parsed) < num_attrs) {
          default_attrs <- c("材質", "重量", "尺寸", "顏色", "保固",
                            "配件", "說明書", "認證", "產地", "保存",
                            "設計", "便攜性", "相容性", "效能", "穩定性")
          attributes_parsed <- c(attributes_parsed,
                                default_attrs[1:(num_attrs - length(attributes_parsed))])
        }

        rv$attributes <- head(attributes_parsed, num_attrs)

        success_msg <- get_msg("system.messages.success.attributes_extracted", "已成功萃取")
        showNotification(
          paste(success_msg, length(rv$attributes), get_msg("system.messages.labels.product_attributes", "個產品屬性")),
          type = "message"
        )

        incProgress(1)
      })
    })

    # ========== Display Attributes List ==========
    output$attributes_list <- renderUI({
      req(rv$attributes)

      attr_count_text <- get_msg("modules.advanced_attribute.attr_count", "已產生的屬性（共{count}個）")
      attr_count_text <- gsub("\\{count\\}", length(rv$attributes), attr_count_text)

      HTML(paste0(
        "<div class='attributes-list'>",
        "<h5>", attr_count_text, "</h5>",
        "<div class='row'>",
        paste0(
          "<div class='col-md-3' style='margin-bottom: 10px;'><span class='badge badge-info' style='font-size: 14px; padding: 8px 12px;'>",
          rv$attributes, "</span></div>",
          collapse = ""
        ),
        "</div>",
        "</div>"
      ))
    })

    # ========== Start Scoring Observer ==========
    observeEvent(input$start_scoring, {
      req(rv$attributes, review_data())

      progress_msg <- get_msg("system.messages.info.scoring_in_progress", "正在評分中，請稍候...")

      withProgress(message = progress_msg, value = 0, {
        reviews <- review_data()
        n_reviews <- min(100, nrow(reviews))  # Maximum 100 reviews

        # Get current language
        current_lang <- tryCatch({
          lang_content <- lang_texts()
          lang_content$language %||% "chinese"
        }, error = function(e) "chinese")

        scores_matrix <- matrix(NA, nrow = n_reviews, ncol = length(rv$attributes))
        colnames(scores_matrix) <- rv$attributes

        error_count <- 0

        for (i in 1:n_reviews) {
          detail_msg <- get_msg("system.messages.info.processing_item", "正在處理第 {current}/{total} 筆")
          detail_msg <- gsub("\\{current\\}", i, gsub("\\{total\\}", n_reviews, detail_msg))
          incProgress(1/n_reviews, detail = detail_msg)

          review_text <- reviews$Body[i]
          brand_name <- reviews$Variation[i]

          # Prepare scoring prompt
          prompt_messages <- tryCatch({
            prepare_gpt_messages(
              var_id = "score_attributes",
              language = current_lang,
              variables = list(
                attributes = paste(rv$attributes, collapse = ", "),
                review_text = substr(review_text, 1, 1000)
              )
            )
          }, error = function(e) {
            list(
              list(role = "system", content = "You are a product review scoring expert."),
              list(role = "user", content = sprintf("Score these attributes 1-5 based on review:\nAttributes: %s\nReview: %s\nReturn JSON: {\"scores\": {\"attr1\": 4, \"attr2\": 3, ...}}",
                                                    paste(rv$attributes, collapse = ", "), substr(review_text, 1, 500)))
            )
          })

          # Get scores from API（使用共用 API 設定的模型）
          response <- tryCatch({
            chat_api(prompt_messages, model = cfg_ai_model)
          }, error = function(e) {
            error_count <<- error_count + 1
            message("⚠️ Scoring error (item ", i, ", brand: ", brand_name, "): ", e$message)
            NULL
          })

          if (!is.null(response)) {
            # Parse scores
            scores_parsed <- tryCatch({
              json_response <- jsonlite::fromJSON(response)
              if ("scores" %in% names(json_response)) {
                json_response$scores
              } else {
                NULL
              }
            }, error = function(e) NULL)

            # Fill score matrix
            if (!is.null(scores_parsed)) {
              for (attr in rv$attributes) {
                if (!is.null(scores_parsed[[attr]])) {
                  scores_matrix[i, attr] <- as.numeric(scores_parsed[[attr]])
                }
              }
            }
          }
        }

        # Calculate average scores by brand
        rv$scores <- data.frame(
          Variation = reviews$Variation[1:n_reviews],
          scores_matrix,
          check.names = FALSE
        ) %>%
          group_by(Variation) %>%
          summarise(across(everything(), ~mean(., na.rm = TRUE)), .groups = 'drop')

        # Show completion notification
        if (error_count == 0) {
          success_msg <- get_msg("system.messages.success.scoring_complete", "✅ 評分完成！成功處理 {total} 筆資料")
          success_msg <- gsub("\\{total\\}", n_reviews, success_msg)
          showNotification(success_msg, type = "message", duration = 5)
        } else {
          warning_msg <- get_msg("system.messages.warning.scoring_warning", "⚠️ 評分完成，但有 {error_count}/{total} 筆資料因 API 錯誤而跳過")
          warning_msg <- gsub("\\{error_count\\}", error_count, gsub("\\{total\\}", n_reviews, warning_msg))
          showNotification(warning_msg, type = "warning", duration = 10)
        }
      })
    })

    # ========== Display Scoring Results ==========
    output$scoring_results <- renderDT({
      req(rv$scores)

      caption_text <- get_msg("system.ui.tables.avg_scores_caption", "各品牌屬性平均分數")

      DT::datatable(
        rv$scores,
        caption = paste(caption_text, "（", ncol(rv$scores) - 1, get_msg("analysis.attributes", "個屬性"), "）"),
        rownames = FALSE,
        options = list(
          pageLength = 20,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        extensions = 'Buttons'
      ) %>%
        DT::formatRound(columns = 2:ncol(rv$scores), digits = 2) %>%
        DT::formatStyle(
          columns = 2:ncol(rv$scores),
          background = DT::styleColorBar(range(rv$scores[, -1], na.rm = TRUE), 'lightblue'),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
    })

    # Return reactive values for use by other modules
    return(reactive({
      list(
        attributes = rv$attributes,
        scores = rv$scores
      )
    }))
  })
}

message("✅ BrandEdge Advanced Attribute Module loaded")
