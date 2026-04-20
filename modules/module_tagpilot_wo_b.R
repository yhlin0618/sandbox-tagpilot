# Source global chat_api for GPT-5 Responses API support
source("scripts/global_scripts/08_ai/fn_chat_api.R")

strip_code_fence <- function(txt) {
  str_replace_all(
    txt,
    regex("^```[A-Za-z0-9]*[ \\t]*\\r?\\n|\\r?\\n```[ \\t]*$", multiline = TRUE),
    ""
  )
}

# Note: chat_api() is sourced from scripts/global_scripts/08_ai/fn_chat_api.R
# which correctly handles GPT-5 Responses API format

# 1. PCA Module --------------------------------------------------------------
pcaModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("hint")),       # 顯示提示
    plotlyOutput(ns("pca_plot"))
  )
}

pcaModuleServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    pca_res <- reactive({
      nums <- data()
      prcomp(nums, center = TRUE, scale. = TRUE, rank. = 3)
    })
    
    output$hint <- renderUI({
      df <- data()
      if (ncol(df) < 2) {
        tags$div(style="color:#888", "請先從左側選好「分類／品牌／Variation」，並且該組資料至少有兩個數值欄，才能看到 PCA 結果。")
      }
    })
    
    output$pca_plot <- renderPlotly({
      nums <- data()
      req(ncol(nums) >= 2)     # 只有夠欄才繪圖
      pc <- pca_res()
      scores   <- as.data.frame(pc$x)
      loadings <- as.data.frame(pc$rotation) * 5
      load_long <- bind_rows(
        loadings %>% rownames_to_column("var") %>% mutate(PC1=0,PC2=0,PC3=0),
        loadings %>% rownames_to_column("var")
      )
      plot_ly() %>%
        add_markers(data = scores, x=~PC1, y=~PC2, z=~PC3, color=I("gray")) %>%
        add_lines  (data = load_long, x=~PC1, y=~PC2, z=~PC3, color=~var) %>%
        layout(scene = list(
          xaxis=list(title="PC1"),
          yaxis=list(title="PC2"),
          zaxis=list(title="PC3")
        ))
    })
  })
}




# 2. Ideal Analysis Module --------------------------------------------------
idealModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    textOutput(ns("key_factors")),
    br(),
    br(),
    DTOutput(ns("ideal_rank"))
    
  )
  
  
  
}
idealModuleServer <- function(id, data, raw,indicator,key_vars) {
  moduleServer(id, function(input, output, session) {
    # ... 保留 ideal_row, indicator, key_fac 定義 ...
    
    
    # 1. 抓出「理想點」那一 row，只取數值欄
    ideal_row <- reactive({
      df <- data()
      req("Ideal" %in% df$Variation)
      df %>%
        filter(Variation == "Ideal") %>%
        select(where(~ is.numeric(.x) && !any(is.na(.x))))
    })
    
    
    #  print(key_vars())
    output$key_factors <- renderText({
      paste("關鍵因素：", paste(key_vars(), collapse = ", "))
    })
    
    output$ideal_rank <- renderDT({
      ind <- indicator()
      df  <- raw() %>% select(Variation)
      # 計算 Score
      #ind %>% select(-c("Variation","sales","rating")) %>%  rowSums() %>% print()
      df$Score <-   ind %>% select(-any_of(c("Variation","sales","rating"))) %>%  
        select(any_of(key_vars())) %>% 
        rowSums()
      df <- df %>% filter(Variation != "Ideal") %>% arrange(desc(Score))
      # 用 DT::datatable 才會出現 Score 欄
      DT::datatable(df,
                    rownames = FALSE,
                    options = list(pageLength = 10, searching = TRUE))
    })
    output$debug_keys <- renderPrint({
      key_vars()  # 看看挑出来的关键字段名
    })
    
   # 
   # md_text <- reactive({
   #   req(input$refresh)  # 按鈕才會更新
   #   position_txt <- toJSON(position_dta, dataframe = "rows", auto_unbox = TRUE)
   #   
   #   sys <- list(role = "system", content = "你是一位數據分析師，請用繁體中文回答。")
   #   usr <- list(
   #     role = "user",
   #     content = paste0(
   #       "以下為儀表板的資料。請總結此儀表板上的關鍵因素有哪些？不要自己加關鍵因素。並根據前面儀表板各Variation的缺點，未來要強化的關鍵因素是甚麼，並提出廣告建議，字數限200字內",
   #       "請回覆為markdown的格式。資料:",
   #       position_txt
   #     )
   #   )
   #   txt <- chat_api(list(sys, usr))   # ← 一次完成
   #   attrs <- str_extract_all(txt, "[^{},，\\s]+")[[1]]
   #   attrs <- unique(trimws(attrs))
   #   
   # })
   
   # output$brand_ideal_summary <- renderUI({
   #   # markdownToHTML 會回傳一個 HTML 字串
   #   html <- markdownToHTML(text = md_text(), fragment.only = TRUE)
   #   HTML(html)
   # })
   
   
    
  })
  
  
  
  
}


# 3. Strategy Analysis Module ------------------------------------------------
# ── UI ──────────────────────────────────────────────────────────────────
strategyModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("select_Variation"), "選擇 Variation", choices = NULL),
    
    actionButton(ns("run_strategy"), "策略探索",
                 class = "btn btn-primary mb-3"),
    
    plotlyOutput(ns("strategy_plot"), height = "800px"),
    
    ## ⭐ 正確的 withSpinner 用法 → tag 放第一個參數
    withSpinner(
      htmlOutput(ns("strategy_summary")),
      type  = 6,
      color = "#0d6efd"
    )
  )
}

# ── Server ─────────────────────────────────────────────────────────────
strategyModuleServer <- function(id, data, key_vars) {
  moduleServer(id, function(input, output, session) {
    rv <- reactiveValues(cache = list())   # key = Variation, value = GPT 文字
    
    # 1. 動態更新下拉選單
    observe({
      req(data())
      updateSelectInput(session, "select_Variation",
                        choices = unique(data()$Variation))
    })
    
    # 2. 四象限圖：只要 Variation 改變就更新
    output$strategy_plot <- renderPlotly({
      req(input$select_Variation)
      
      ind       <- dplyr::filter(data(), Variation == input$select_Variation)
      req(nrow(ind) > 0)                           # 若沒資料先擋掉
      
      key       <- key_vars()
      feats_key <- key
      feats_non <- setdiff(names(ind), c(key, "Variation"))
      
      sums_key <- colSums(ind[feats_key, drop = FALSE])
      sums_non <- colSums(ind[feats_non, drop = FALSE])
      
      quad_feats <- list(
        訴求 = feats_key[sums_key >  mean(sums_key)],
        改變 = feats_key[sums_key <= mean(sums_key)],
        改善 = feats_non[sums_non >  mean(sums_non)],
        劣勢 = feats_non[sums_non <= mean(sums_non)]
      )
      
      #── 產生座標用的小函式（略） ───────────────────────────
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
      
      specs <- list(
        訴求 = list(x =  5, y = 10),
        改變 = list(x =  5, y = -1),
        改善 = list(x = -5, y = 10),
        劣勢 = list(x = -5, y = -1)
      )
      
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
      
      # 象限標題
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
      
      # 每象限特徵文字
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
    
    # 3. GPT：只有按鈕被按時才發送
    
    ## 按鈕觸發後，把結果存進快取 -------------------------------
    observeEvent(input$run_strategy, {
      var_now <- req(input$select_Variation)
      
      withProgress(message = "策略分析中…", value = 0, {
        incProgress(0.2)
    
      req(input$select_Variation)
   #   cat(">>> trigger at", Sys.time(), "\n")   # ← 若不印，表示沒觸發
  
        
        ind       <- dplyr::filter(data(), Variation == input$select_Variation)
        key       <- key_vars()
        feats_key <- key
        feats_non <- setdiff(names(ind), c(key, "Variation"))
        
        sums_key <- colSums(ind[feats_key, drop = FALSE])
        sums_non <- colSums(ind[feats_non, drop = FALSE])
        
        quad_feats <- list(
          Variation = input$select_Variation,
          訴求  = feats_key[sums_key >  mean(sums_key)],
          改變  = feats_key[sums_key <= mean(sums_key)],
          改善  = feats_non[sums_non >  mean(sums_non)],
          劣勢  = feats_non[sums_non <= mean(sums_non)]
        )
        features <- jsonlite::toJSON(quad_feats, auto_unbox = TRUE)
        incProgress(0.4)
        
        sys <- list(role = "system",
                    content = "你是一位數據分析師，請用繁體中文回答。")
        usr <- list(
          role = "user",
          content = paste0(
            "以下為特定Variation在品牌定位上，可訴求的屬性、可改變的屬性、可改善的屬性、品牌的劣勢屬性等，請針對此四個分析面向，提出相應的行銷策略、廣告文案建議。如果四個分析面向都沒有半個屬性，則完全不要呈現，也不要提供任何行銷建議。字數300字內。",
            "請回覆為markdown的格式。資料:",
            features
          )
        )
        txt <- tryCatch(
          chat_api(list(sys, usr)),
          error = function(e) paste("❌ GPT 失敗：", e$message)
        )
        rv$cache[[var_now]] <- txt
        incProgress(0.9)
        # cat(txt)
        # txt
      })
    })
    
    # 4. Markdown → HTML（有 spinner 包住）
    
    output$strategy_summary <- renderUI({
      var_now <- req(input$select_Variation)
      txt     <- rv$cache[[var_now]]
      
      if (is.null(txt)) {
        return(HTML("<i style='color:gray'>尚未產生策略分析，請點擊「策略探索」。</i>"))
      }
      

      
      
      # req(md_text_strategy())
      # res <- md_text_strategy()
      res <-   strip_code_fence(txt)
      #   print(res)
      html <- markdownToHTML(text = res, fragment.only = TRUE)
      HTML(html)
    })

  })
}


## 5. Brand DNA Module -------------------------------------------------------
dnaModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("dna_plot"), height = "600px")
  )
}

dnaModuleServer <- function(id, data_full) {
  moduleServer(id, function(input, output, session) {
    output$dna_plot <- renderPlotly({
      
      
      # pivot 成长表
      dta_Brand_log <- data_full() %>% 
        pivot_longer(
          cols = -c("Variation"),
          names_to = "attribute",
          values_to = "score"
        ) %>%
        arrange(Variation, attribute)
      
      # # 保持属性顺序
      # dta_Brand_log$attribute <- factor(
      #   dta_Brand_log$attribute,
      #   levels = unique(dta_Brand_log$attribute)
      # )
      # 
      Variation_groups <- split(dta_Brand_log, dta_Brand_log$Variation)
      dta_Brand_log$attribute <- as.factor(dta_Brand_log$attribute)
      fac_lis <-  dta_Brand_log$attribute
      
      p <- plot_ly()
      
      # 为每个Brand添加一条线
      for (Variation_data in Variation_groups) {
        
        Variation <- unique(Variation_data$Variation)
        p <- add_trace(p,data=Variation_data, x = ~levels(fac_lis), y = ~score, 
                       color=~Variation, type = 'scatter', mode = 'lines+markers', name = Variation,
                       text = ~Variation, hoverinfo = 'text',
                       marker = list(size = 10),  # 设置点的大小
                       line = list(width = 2),     # 设置线的宽度
                       visible = "legendonly"  # 默认隐藏所有内容
        )
      }
      p <- layout(p,
                  #title = "Brand DNA",
                  xaxis = list(
                    title = list(
                      text = "Attribute",
                      font = list(size = 20)  # 设置x轴标题字体大小
                    ),
                    tickangle = -45  # 旋转x轴文本
                  ),
                  yaxis = list(
                    title = "Score",font = list(size = 20)
                  )
                  #           legend = list(
                  #   itemclick = 'toggleothers', # 点击图例时，切换其他图例
                  #   traceorder = 'normal'       # 图例的显示顺序
                  # )
      )
      p
    })
  })
}


