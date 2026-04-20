# BrandEdge Legacy Modules
# 保留原有模組（向下相容）
# Includes: PCA, Ideal Point, Strategy, DNA modules
# Version: 1.0
# Last Updated: 2025-10-06

# Load shared functions
if (!exists("get_lang_text")) {
  source("modules/module_brandedge_shared.R")
}

# ========== 保留原有模組（向下相容）==========

# PCA Module
pcaModuleUI <- function(id, lang_texts = NULL) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("hint")),
    plotlyOutput(ns("pca_plot"))
  )
}

pcaModuleServer <- function(id, data, lang_texts = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {

    # Helper function to get translated text
    get_text <- function(path, default) {
      lang_content <- tryCatch(lang_texts(), error = function(e) NULL)
      if (is.null(lang_content)) return(default)

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
    
    # 預處理數據以避免常數列問題
    processed_data <- reactive({
      full_data <- data()
      req(ncol(full_data) >= 3)  # 至少需要 Variation + 2個數值列
      
      # 分離品牌名稱和數值數據
      brand_names <- full_data$Variation
      nums <- full_data %>% select(-Variation)
      
      # 轉換為data.frame以避免tibble警告
      nums_df <- as.data.frame(nums)
      
      # 檢查並移除常數列（變異數為0的列）
      vars <- apply(nums_df, 2, var, na.rm = TRUE)
      valid_cols <- !is.na(vars) & vars > 1e-8  # 允許極小的變異
      
      if (sum(valid_cols) < 2) {
        return(NULL)  # 沒有足夠的變數進行PCA
      }
      
      # 返回處理後的數據和品牌名稱
      list(
        data = nums_df[, valid_cols, drop = FALSE],
        brands = brand_names,
        removed_cols = sum(!valid_cols)
      )
    })
    
    pca_res <- reactive({
      processed <- processed_data()
      req(!is.null(processed))
      prcomp(processed$data, center = TRUE, scale. = TRUE, rank. = 3)
    })
    
    output$hint <- renderUI({
      full_data <- data()
      processed <- processed_data()
      
      if (ncol(full_data) < 3) {  # Variation + 至少2個數值列
        tags$div(style="color:#888", get_text("legacy.pca.need_data", "請先完成屬性評分，並且至少需要2個數值屬性才能進行PCA分析。"))
      } else if (is.null(processed)) {
        tags$div(style="color:#orange", get_text("legacy.pca.constant_cols", "⚠️ 數據中存在常數列（變異為0），無法進行PCA分析。請確認評分數據的變異性。"))
      } else if (processed$removed_cols > 0) {
        removed_msg <- get_text("legacy.pca.removed_cols", "ℹ️ 已自動移除 {removed} 個常數列，使用 {used} 個變數進行PCA分析。")
        removed_msg <- gsub("\\{removed\\}", processed$removed_cols, gsub("\\{used\\}", ncol(processed$data), removed_msg))
        tags$div(style="color:#blue", removed_msg)
      } else {
        using_msg <- get_text("legacy.pca.using_attrs", "✅ 使用 {count} 個屬性進行PCA分析")
        using_msg <- gsub("\\{count\\}", ncol(processed$data), using_msg)
        tags$div(style="color:#green", using_msg)
      }
    })
    
    output$pca_plot <- renderPlotly({
      processed <- processed_data()
      req(!is.null(processed), ncol(processed$data) >= 2)
      
      tryCatch({
        pc <- pca_res()
        scores   <- as.data.frame(pc$x)
        loadings <- as.data.frame(pc$rotation) * 5
        
        # 使用實際的品牌名稱
        scores$Brand <- processed$brands[1:nrow(scores)]
        
        load_long <- bind_rows(
          loadings %>% rownames_to_column("var") %>% mutate(PC1=0, PC2=0, PC3=0),
          loadings %>% rownames_to_column("var")
        )
        
        # 計算解釋變異量
        var_explained <- summary(pc)$importance[2, 1:3] * 100
        
        plot_ly() %>%
          add_markers(data = scores, 
                     x = ~PC1, y = ~PC2, z = ~PC3, 
                     text = ~paste(get_text("legacy.pca.brand_label", "品牌:"), Brand, "<br>PC1:", round(PC1, 2), "<br>PC2:", round(PC2, 2), "<br>PC3:", round(PC3, 2)),
                     hoverinfo = "text",
                     marker = list(size = 10, color = "steelblue", opacity = 0.8)) %>%
          add_lines(data = load_long, 
                   x = ~PC1, y = ~PC2, z = ~PC3, 
                   color = ~var,
                   line = list(width = 2),
                   hoverinfo = "text",
                   text = ~paste(get_text("legacy.pca.attribute_label", "屬性:"), var)) %>%
          layout(
            title = get_text("legacy.pca.map_title", "PCA 品牌定位地圖"),
            scene = list(
              xaxis = list(title = paste("PC1 (", round(var_explained[1], 1), "%)")),
              yaxis = list(title = paste("PC2 (", round(var_explained[2], 1), "%)")),
              zaxis = list(title = paste("PC3 (", round(var_explained[3], 1), "%)"))
            ),
            margin = list(l = 0, r = 0, t = 50, b = 0)
          )
      }, error = function(e) {
        plot_ly() %>%
          add_annotations(
            text = paste(get_text("legacy.pca.error", "PCA 分析錯誤:"), e$message, "<br>", get_text("legacy.pca.check_data", "請確認數據品質和變異性")),
            xref = "paper", yref = "paper",
            x = 0.5, y = 0.5, xanchor = "center", yanchor = "center",
            showarrow = FALSE,
            font = list(size = 16, color = "red")
          ) %>%
          layout(title = get_text("legacy.pca.analysis_title", "PCA 分析"))
      })
    })
  })
}

# Ideal Analysis Module
idealModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    textOutput(ns("key_factors")),
    br(),
    br(),
    DTOutput(ns("ideal_rank"))
  )
}

idealModuleServer <- function(id, data, raw, indicator, key_vars, lang_texts = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {

    # Helper function to get translated text
    get_text <- function(path, default) {
      lang_content <- tryCatch(lang_texts(), error = function(e) NULL)
      if (is.null(lang_content)) return(default)

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

    ideal_row <- reactive({
      df <- data()
      req("Ideal" %in% df$Variation)
      df %>%
        filter(Variation == "Ideal") %>%
        select(where(~ is.numeric(.x) && !any(is.na(.x))))
    })
    
    output$key_factors <- renderText({
      paste(get_text("legacy.ideal.key_factors_label", "關鍵因素："), paste(key_vars(), collapse = ", "))
    })
    
    output$ideal_rank <- renderDT({
      ind <- indicator()
      df  <- raw() %>% select(Variation)
      df$Score <- ind %>% 
        select(-any_of(c("Variation","sales","rating"))) %>%  
        select(any_of(key_vars())) %>% 
        rowSums()
      df <- df %>% filter(Variation != "Ideal") %>% arrange(desc(Score))
      
      DT::datatable(df,
                    rownames = FALSE,
                    options = list(pageLength = 10, searching = TRUE))
    })
  })
}

# Strategy Analysis Module
strategyModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("select_Variation"),
                label = get_lang_text("ui.analysis.select_variation",
                                     default = "Select Variation"),
                choices = NULL),
    actionButton(ns("run_strategy"),
                label = get_lang_text("ui.analysis.run_strategy",
                                     default = "Run Strategy Analysis"),
                class = "btn btn-primary mb-3"),
    plotlyOutput(ns("strategy_plot"), height = "800px"),
    withSpinner(
      htmlOutput(ns("strategy_summary")),
      type  = 6,
      color = "#0d6efd"
    )
  )
}

strategyModuleServer <- function(id, data, key_vars, lang_texts = reactive(NULL), api_config = NULL) {
  moduleServer(id, function(input, output, session) {
    rv <- reactiveValues(cache = list())

    # 從共用 API 設定讀取模型（fallback 到 gpt-5-nano）
    cfg_ai_model <- if (!is.null(api_config$default_model)) {
      api_config$default_model
    } else {
      "gpt-5-nano"
    }

    # Helper function to get translated text
    get_text <- function(path, default) {
      lang_content <- tryCatch(lang_texts(), error = function(e) NULL)
      if (is.null(lang_content)) return(default)

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
    
    observe({
      req(data())
      updateSelectInput(session, "select_Variation",
                        choices = unique(data()$Variation))
    })
    
    output$strategy_plot <- renderPlotly({
      req(input$select_Variation)
      
      ind       <- dplyr::filter(data(), Variation == input$select_Variation)
      req(nrow(ind) > 0)
      
      key       <- key_vars()
      feats_key <- key
      feats_non <- setdiff(names(ind), c(key, "Variation"))
      
      sums_key <- colSums(ind[feats_key, drop = FALSE])
      sums_non <- colSums(ind[feats_non, drop = FALSE])
      
      # Get translated quadrant labels
      label_appeal <- get_text("legacy.strategy.appeal", "訴求")
      label_change <- get_text("legacy.strategy.change", "改變")
      label_improve <- get_text("legacy.strategy.improve", "改善")
      label_weakness <- get_text("legacy.strategy.weakness", "劣勢")

      quad_feats <- list()
      quad_feats[[label_appeal]] <- feats_key[sums_key >  mean(sums_key)]
      quad_feats[[label_change]] <- feats_key[sums_key <= mean(sums_key)]
      quad_feats[[label_improve]] <- feats_non[sums_non >  mean(sums_non)]
      quad_feats[[label_weakness]] <- feats_non[sums_non <= mean(sums_non)]

      make_multi_col <- function(feats, x_center, y_title, y_step = -1.5) {
        if (length(feats) == 0) return(NULL)
        cols <- ceiling(length(feats) / 6)
        dfs  <- lapply(seq_along(feats), function(i) {
          data.frame(text = feats[i],
                     x    = x_center + ((i-1) %/% 6 - (cols-1)/2) * 3,
                     y    = y_title + ((i-1) %% 6) * y_step)
        })
        do.call(rbind, dfs)
      }

      specs <- list()
      specs[[label_appeal]] <- list(x =  5, y = 10)
      specs[[label_change]] <- list(x =  5, y = -1)
      specs[[label_improve]] <- list(x = -5, y = 10)
      specs[[label_weakness]] <- list(x = -5, y = -1)
      
      p <- plotly::plot_ly() |>
        layout(
          shapes = list(
            list(type = 'line', x0 = 0, x1 = 0, y0 = -11, y1 = 11, line = list(width = 2)),
            list(type = 'line', x0 = -11, x1 = 11, y0 = 0,  y1 = 0,  line = list(width = 2))
          ),
          xaxis = list(showgrid = FALSE, zeroline = FALSE,
                       showticklabels = FALSE, range = c(-12, 12)),
          yaxis = list(showgrid = FALSE, zeroline = FALSE,
                       showticklabels = FALSE, range = c(-12, 12)),
          showlegend = FALSE
        )
      
      titles_df <- data.frame(
        text = names(specs),
        x    = vapply(specs, `[[`, numeric(1), "x"),
        y    = vapply(specs, `[[`, numeric(1), "y")
      )
      p <- p |> add_trace(
        data = titles_df, type = "scatter", mode = "text",
        x = ~x, y = ~y, text = ~text,
        textfont = list(size = 24, color = "black")
      )
      
      for (nm in names(quad_feats)) {
        df_c <- make_multi_col(
          quad_feats[[nm]], specs[[nm]]$x, specs[[nm]]$y - 2
        )
        if (!is.null(df_c)) {
          p <- p |> add_trace(
            data = df_c, type = "scatter", mode = "text",
            x = ~x, y = ~y, text = ~text,
            textfont = list(size = 18, color = "blue")
          )
        }
      }
      
      p |> layout(margin = list(l = 60, r = 60, t = 60, b = 60),
                  font   = list(size = 10))
    })
    
    observeEvent(input$run_strategy, {
      var_now <- req(input$select_Variation)
      
      progress_msg <- get_text("legacy.strategy.analyzing", "策略分析中…")
      withProgress(message = progress_msg, value = 0, {
        incProgress(0.2)
        
        ind       <- dplyr::filter(data(), Variation == input$select_Variation)
        key       <- key_vars()
        feats_key <- key
        feats_non <- setdiff(names(ind), c(key, "Variation"))
        
        sums_key <- colSums(ind[feats_key, drop = FALSE])
        sums_non <- colSums(ind[feats_non, drop = FALSE])
        
        # Get translated quadrant labels
        label_appeal <- get_text("legacy.strategy.appeal", "訴求")
        label_change <- get_text("legacy.strategy.change", "改變")
        label_improve <- get_text("legacy.strategy.improve", "改善")
        label_weakness <- get_text("legacy.strategy.weakness", "劣勢")

        quad_feats <- list(
          Variation = input$select_Variation
        )
        quad_feats[[label_appeal]] <- feats_key[sums_key >  mean(sums_key)]
        quad_feats[[label_change]] <- feats_key[sums_key <= mean(sums_key)]
        quad_feats[[label_improve]] <- feats_non[sums_non >  mean(sums_non)]
        quad_feats[[label_weakness]] <- feats_non[sums_non <= mean(sums_non)]
        features <- jsonlite::toJSON(quad_feats, auto_unbox = TRUE)
        incProgress(0.4)
        
        # 使用集中管理的 prompt 生成定位策略
        messages <- prepare_gpt_messages(
          var_id = "positioning_strategy",
          variables = list(
            features = features
          ),
          prompts_df = prompts_df
        )
        
        txt <- tryCatch(
          chat_api(messages, model = cfg_ai_model),
          error = function(e) paste("❌ GPT 失敗：", e$message)
        )
        rv$cache[[var_now]] <- txt
        incProgress(0.9)
      })
    })
    
    output$strategy_summary <- renderUI({
      var_now <- req(input$select_Variation)
      txt     <- rv$cache[[var_now]]
      
      if (is.null(txt)) {
        return(HTML(paste0("<i style='color:gray'>", get_text("legacy.strategy.not_generated", "尚未產生策略分析，請點擊「策略探索」。"), "</i>")))
      }
      
      res <- strip_code_fence(txt)
      html <- markdownToHTML(text = res, fragment.only = TRUE)
      HTML(html)
    })
  })
}

# Brand DNA Module
dnaModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("dna_plot"), height = "600px")
  )
}

dnaModuleServer <- function(id, data_full) {
  moduleServer(id, function(input, output, session) {
    output$dna_plot <- renderPlotly({
      
      dta_Brand_log <- data_full() %>% 
        pivot_longer(
          cols = -c("Variation"),
          names_to = "attribute",
          values_to = "score"
        ) %>%
        arrange(Variation, attribute)
      
      Variation_groups <- split(dta_Brand_log, dta_Brand_log$Variation)
      dta_Brand_log$attribute <- as.factor(dta_Brand_log$attribute)
      fac_lis <-  dta_Brand_log$attribute
      
      p <- plot_ly()
      
      for (Variation_data in Variation_groups) {
        Variation <- unique(Variation_data$Variation)
        p <- add_trace(p, data=Variation_data, x = ~levels(fac_lis), y = ~score, 
                       color=~Variation, type = 'scatter', mode = 'lines+markers', name = Variation,
                       text = ~Variation, hoverinfo = 'text',
                       marker = list(size = 10),
                       line = list(width = 2),
                       visible = "legendonly"
        )
      }
      p <- layout(p,
                  xaxis = list(
                    title = list(
                      text = "Attribute",
                      font = list(size = 20)
                    ),
                    tickangle = -45
                  ),
                  yaxis = list(
                    title = "Score",font = list(size = 20)
                  )
      )
      p
    })
  })
}
