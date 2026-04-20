#' @principle P15 Debug Efficiency Exception
#' @principle P81 Tidyverse-Shiny Terminology Alignment
#' @principle R76 Module Data Connection Rule
#' @principle R88 Shiny Module ID Handling
#' @principle R89 Integer ID Type Conversion
#' @principle R90 ID Relationship Validation
#' @r21_exception This file contains the UI-server-defaults triple for microCustomer module
#' @justification These components are frequently debugged together during UI development and data integration
#' @refactor_plan To be refactored back into separate files once micro-level component design is finalized (est. Q3 2025)

####microCustomerDefaults####

#' Default Values for Micro Customer Component
#'
#' This function provides standard default values for the micro customer component.
#' These defaults ensure that all UI outputs have appropriate values even when
#' data is unavailable or invalid, implementing the UI-Server Pairing Rule.
#'
#' @return Named list of output IDs and their default values
#' @export
microCustomerDefaults <- function() {
  list(
    # Customer history metrics
    dna_time_first = "N/A",
    dna_time_first_to_now = "0",
    
    # RFM metrics
    dna_r_label = "N/A",
    dna_r_value = "0",
    dna_f_label = "N/A",
    dna_f_value = "0",
    dna_m_label = "N/A",
    dna_m_value = "0.00",
    
    # Customer activity metrics
    dna_cai_label = "N/A",
    dna_cai = "0.00",
    dna_ipt_mean = "0.0",
    
    # Value metrics
    dna_pcv = "0.00",
    dna_clv = "0.00",
    dna_cri = "0.00",
    
    # Prediction metrics
    dna_nrec = "N/A",
    dna_nrec_prob = "0%",
    
    # Status metrics
    dna_nes_status = "N/A",
    
    # Transaction metrics
    dna_nt = "0.00",
    dna_e0t = "0.00"
  )
}

####microCustomerFilterUI####

#' Micro Customer Filter UI Component
#'
#' This component provides a filter interface for selecting customers
#' to be displayed in the micro-level customer analysis view.
#' Follows P75: Search Input Optimization pattern.
#'
#' @param id The module ID
#' @param translate 翻譯函數，默認為identity函數
#'
#' @return A UI component for filtering customers
#' @export
microCustomerFilterUI <- function(id, translate = function(x) x) {
  ns <- NS(id)
  
  div(
    class = "customer-filter-container",
    style = "padding: 15px; background-color: #f8f9fa; border-radius: 5px; margin-bottom: 20px;",
    fluidRow(
      column(
        width = 12,
        h4(translate("客戶篩選"), style = "margin-bottom: 15px;"),
        # 提供初始佔位符選項以改善用戶體驗 - 遵循P81原則命名
        selectizeInput(
          inputId = ns("customer_filter"),
          label = translate("Select Customer:"),
          choices = c("-- 正在載入客戶資料 --" = ""), # 提供初始選項
          selected = NULL,
          options = list(
            placeholder = translate("開始輸入以搜尋客戶..."),
            onInitialize = I('function() { this.setValue(""); }')
          )
        ),
        div(
          style = "margin-top: 10px;",
          actionButton(
            inputId = ns("clear_filter"),
            label = translate("清除篩選"),
            icon = icon("times"),
            class = "btn-outline-secondary",
            width = "100%"
          )
        )
      )
    )
  )
}

####microCustomerUI####

#' Micro Customer UI Component
#'
#' This component provides the UI elements for displaying detailed customer analytics
#' in the micro-level view of the application.
#'
#' IMPORTANT: According to the UI-Server Pairing Rule, this UI component MUST be used with
#' its corresponding server component microCustomerServer(). All outputs defined here
#' must be fulfilled by the server component to avoid broken displays.
#'
#' @param id The module ID
#'
#' @return A UI component
#' @export
microCustomerUI <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    title = "微觀",
    # 使用基本的shiny元素替代bs4Dash專有元素
    div(
      class = "card",
      style = "width: 100%; margin-bottom: 20px;",
      div(
        class = "card-body",
        # Header: Customer Name and Email
        fluidRow(
          column(12,
                 div(
                   class = "customer-profile-header",
                   style = "margin-bottom: 15px; padding-bottom: 15px; border-bottom: 1px solid #eee; text-align: center;",
                   div(
                     style = "font-size: 1.4rem; font-weight: 600;",
                     htmlOutput(ns("customer_name"))
                   ),
                   div(
                     style = "font-size: 1rem; color: #666;",
                     htmlOutput(ns("customer_email"))
                   )
                 )
          )
        ),
        # Row 1: 顧客資歷、最近購買日(R)、購買頻率(F)
        fluidRow(
          column(4, bs4Dash::valueBoxOutput(ns("dna_time_first"), width = 12)),
          column(4, bs4Dash::valueBoxOutput(ns("dna_recency"), width = 12)),
          column(4, bs4Dash::valueBoxOutput(ns("dna_frequency"), width = 12))
        ),
        # Row 2: 購買金額(M)、顧客活躍度(CAI)、顧客平均購買週期(IPT)
        fluidRow(
          column(4, bs4Dash::valueBoxOutput(ns("dna_monetary"), width = 12)),
          column(4, bs4Dash::valueBoxOutput(ns("dna_cai"), width = 12)),
          column(4, bs4Dash::valueBoxOutput(ns("dna_ipt"), width = 12))
        ),
        # Row 3: 過去價值(PCV)、顧客終身價值(CLV)、顧客交易穩定度 (CRI)
        fluidRow(
          column(4, bs4Dash::valueBoxOutput(ns("dna_pcv"), width = 12)),
          column(4, bs4Dash::valueBoxOutput(ns("dna_clv"), width = 12)),
          column(4, bs4Dash::valueBoxOutput(ns("dna_cri"), width = 12))
        ),
        # Row 4: 顧客狀態(NES)、新客單價、主力客單價
        fluidRow(
          column(4, bs4Dash::valueBoxOutput(ns("dna_nes"), width = 12)),
          column(4, bs4Dash::valueBoxOutput(ns("dna_nt"), width = 12)),
          column(4, bs4Dash::valueBoxOutput(ns("dna_e0t"), width = 12))
        )
      )
    )
  )
}

####microCustomerServer####

#' Unified Micro Customer Server Component (R76 Compliant)
#'
#' This component integrates both filtering and display logic for the micro customer
#' analytics view. It handles customer search, filtering, and data visualization.
#' Follows R76: Module Data Connection Rule - receives data connection instead of pre-filtered data.
#'
#' @param id The module ID
#' @param app_data_connection App data connection object providing access to data sources
#' @param session 當前Shiny會話，用於registerDataObj
#'
#' @return Reactive expression that returns filtered data
#' @export
microCustomerServer <- function(id, 
                               app_data_connection = NULL,
                               session = getDefaultReactiveDomain()) {
  # 根據R88 (Shiny Module ID Handling)移除多餘的ID參數
  # 讓Shiny自行處理命名空間，避免雙重命名空間問題
  
  # 添加調試信息，記錄ID設置
  cat("DEBUG: microCustomerServer ID -", 
      "id:", id, "\n")
  
  # 創建整合的模組伺服器
  moduleServer(id, function(input, output, session) {
    # 使用session$ns而不是NS(id)以避免雙重命名空間問題
    # 重要：moduleServer已經創建了正確的命名空間環境
    
    # 定義常量
    textRlabel <- c("極近", "近期", "一般", "久遠", "非常久遠")
    textFlabel <- c("極低", "低", "一般", "高", "非常高")
    textMlabel <- c("極低", "低", "一般", "高", "非常高")
    textCAIlabel <- c("不活躍", "低度活躍", "一般活躍", "活躍", "非常活躍")
    
    # 獲取默認值
    defaults <- microCustomerDefaults()
    
    # 創建篩選數據的反應值
    filtered_data <- reactiveVal(NULL)
    
    # === R76 Data Connection: Getting data from the connection ===
    df_dna_by_customer <- reactive({
      # Make sure we have a data connection
      req(app_data_connection)
      
      # Get DNA data from connection
      if (is.function(app_data_connection$get_dna_data)) {
        # If it's a function style connection
        app_data_connection$get_dna_data()
      } else if (is.list(app_data_connection) && "df_dna_by_customer" %in% names(app_data_connection)) {
        # If it's a list style connection
        app_data_connection$df_dna_by_customer
      } else {
        # No data available
        NULL
      }
    })
    
    df_customer_profile <- reactive({
      # Make sure we have a data connection
      req(app_data_connection)
      
      # Get customer profile data from connection
      if (is.function(app_data_connection$get_customer_profiles)) {
        # If it's a function style connection
        app_data_connection$get_customer_profiles()
      } else if (is.list(app_data_connection) && "df_customer_profile" %in% names(app_data_connection)) {
        # If it's a list style connection
        app_data_connection$df_customer_profile
      } else {
        # No data available
        NULL
      }
    })
    
    # 預處理客戶搜索資料
    customer_search_data <- reactive({
      # 確保有客戶資料
      profiles <- df_customer_profile()
      if (is.null(profiles) || !is.data.frame(profiles) || nrow(profiles) == 0) {
        return(data.frame())
      }
      
      # 確保有所需的欄位
      if (!all(c("customer_id", "buyer_name", "email") %in% colnames(profiles))) {
        return(data.frame())
      }
      
      # 確保此客戶存在DNA資料 (R76 filtering)
      dna_data <- df_dna_by_customer()
      if (!is.null(dna_data) && nrow(dna_data) > 0) {
        dna_customer_ids <- unique(dna_data$customer_id)
        profiles <- profiles %>% filter(customer_id %in% dna_customer_ids)
      }
      
      # 創建搜尋資料框
      search_data <- data.frame(
        id = profiles$customer_id,
        name = profiles$buyer_name,
        email = profiles$email,
        # 建立標籤但限制長度避免溢出
        label = sapply(1:nrow(profiles), function(i) {
          name <- profiles$buyer_name[i]
          email <- profiles$email[i]
          
          # 如果名稱太長則截斷
          if (nchar(name) > 20) {
            name <- paste0(substr(name, 1, 17), "...")
          }
          
          # 如果電子郵件太長則截斷
          if (nchar(email) > 25) {
            email <- paste0(substr(email, 1, 22), "...")
          }
          
          paste0(name, " (", email, ")")
        }),
        stringsAsFactors = FALSE
      )
      
      return(search_data)
    })
    
    # NSQL data flow documentation using MP27 v2:
    # DATA_FLOW(component: customer_filter) {
    #   SOURCE: dna_data, customer_profiles
    #   INITIALIZE: {
    #     EXTRACT(dna_data → DISTINCT customer_id → valid_ids)
    #     FILTER(customer_profiles → WHERE customer_id IN valid_ids → dropdown_options)
    #   }
    #   ON_SELECT: {
    #     value = customer_filter.selected
    #     FILTER(customer_profiles → WHERE customer_id = value → customer_detail)
    #     FILTER(dna_data → WHERE customer_id = value → customer_metrics)
    #   }
    # }
    
    # 設置搜尋處理函數 - 遵循R88 (Shiny Module ID Handling Rule) + P81 (Tidyverse-Shiny Terminology)
    observe({
      # 添加調試信息
      cat("DEBUG: Loading customer filter options\n")
      cat("DEBUG: filter_id =", filter_id, "\n")
      
      profiles <- df_customer_profile()
      dna_data <- df_dna_by_customer()
      
      # 獲取正確的輸入ID - 使用moduleServer中的session$ns而非外部定義的ns函數
      # 正確獲取命名空間後的ID - 基於R88 Shiny Module ID Handling
      ns <- session$ns
      customer_filter_id <- "customer_filter" # 依照P81: filter用於過濾行的操作
      
      # 確保customer profiles存在
      if (is.null(profiles)) {
        cat("DEBUG: df_customer_profile is NULL\n")
        # 設置一個默認選項，避免空下拉選單
        updateSelectizeInput(
          session = session,
          inputId = customer_filter_id,
          choices = c("-- 無客戶資料 --" = "")
        )
        return()
      }
      
      # 獲取客戶資料直接用於下拉選單
      if (!is.data.frame(profiles) || 
          nrow(profiles) == 0 || 
          !all(c("customer_id", "buyer_name", "email") %in% colnames(profiles))) {
        cat("DEBUG: df_customer_profile validation failed\n")
        updateSelectizeInput(
          session = session,
          inputId = customer_filter_id,
          choices = c("-- 客戶資料格式無效 --" = "")
        )
        return()
      }
      
      # 確保有DNA資料客戶，按照R76進行過濾
      if (!is.null(dna_data) && nrow(dna_data) > 0) {
        dna_customer_ids <- unique(dna_data$customer_id)
        profiles <- profiles %>% filter(customer_id %in% dna_customer_ids)
      }
      
      # 只與DNA資料的客戶進行配對
      choices <- setNames(
        as.character(profiles$customer_id),
        paste0(profiles$buyer_name, " (", profiles$email, ")")
      )
      
      cat("DEBUG: Created", length(choices), "customer choices\n")
      
      # 使用updateSelectizeInput而不是updateSelectInput
      updateSelectizeInput(
        session = session,
        inputId = customer_filter_id,
        choices = c("-- 請選擇客戶 --" = "", choices)
      )
    })
    
    # 當選擇客戶時 - 適用R89 (Integer ID Type Conversion) + P81 (Tidyverse-Shiny Terminology)
    observeEvent(input$customer_filter, {
      # 添加調試信息
      cat("DEBUG: customer_filter changed to", input$customer_filter, "\n")
      cat("DEBUG: session namespace =", session$ns(""), "\n")
      cat("DEBUG: all input values:", paste(names(input), collapse=", "), "\n")
      
      # 確保有資料連接
      req(app_data_connection)
      
      # 確保有必要的數據
      profiles <- df_customer_profile()
      dna_data <- df_dna_by_customer()
      
      if (is.null(profiles) || is.null(dna_data)) {
        cat("DEBUG: Data sources are NULL\n")
        return()
      }
      
      # 只在有選擇時進行篩選
      if (!is.null(input$customer_filter) && input$customer_filter != "") {
        # 獲取選定的客戶ID - 遵循R89，確保使用整數ID
        selected_customer_id <- as.integer(input$customer_filter)
        cat("DEBUG: Selected customer ID:", selected_customer_id, "\n")
        
        # 適用R90 (ID Relationship Validation)
        if (!selected_customer_id %in% profiles$customer_id) {
          cat("DEBUG: Invalid customer ID - not found in profiles\n")
          return()
        }
        
        # 篩選客戶資料
        filtered_customer <- profiles %>%
          dplyr::filter(customer_id == selected_customer_id)
        
        # 篩選DNA資料
        filtered_dna <- dna_data %>%
          dplyr::filter(customer_id == selected_customer_id)
        
        # 適用R90確認關聯存在
        if (nrow(filtered_dna) == 0) {
          cat("DEBUG: Customer has no DNA data\n")
          # 為遵循P80顯示統一錯誤信息
          showNotification("所選客戶無DNA資料", type = "warning")
        }
        
        # 合併資料
        if (nrow(filtered_dna) > 0 && nrow(filtered_customer) > 0) {
          result <- dplyr::left_join(filtered_dna, filtered_customer, by = "customer_id")
          filtered_data(result)
        } else {
          # 若找不到匹配的資料，返回空資料框
          filtered_data(data.frame())
        }
      } else if (!is.null(dna_data) && nrow(dna_data) > 0 &&
                 !is.null(profiles) && nrow(profiles) > 0) {
        # 若無選擇但有數據，顯示第一位客戶的資料（必須是有DNA的客戶）
        dna_customer_ids <- unique(dna_data$customer_id)
        filtered_profiles <- profiles %>% filter(customer_id %in% dna_customer_ids)
        
        if (nrow(filtered_profiles) > 0) {
          # 依照P80，確保使用整數ID
          first_customer_id <- as.integer(filtered_profiles$customer_id[1])
          
          filtered_dna <- dna_data %>% 
            dplyr::filter(customer_id == first_customer_id)
          filtered_customer <- profiles %>% 
            dplyr::filter(customer_id == first_customer_id)
          
          result <- dplyr::left_join(filtered_dna, filtered_customer, by = "customer_id")
          filtered_data(result)
        } else {
          # 若無DNA客戶，返回空資料框
          filtered_data(data.frame())
        }
      } else {
        # 若無選擇且無數據，返回空資料框
        filtered_data(data.frame())
      }
    })
    
    # 當按下清除篩選按鈕時 - 適用R88 (Shiny Module ID Handling)
    observeEvent(input$clear_filter, {
      # 添加調試信息
      cat("DEBUG: clear_filter button clicked\n")
      cat("DEBUG: session namespace =", session$ns(""), "\n")
      cat("DEBUG: all input values:", paste(names(input), collapse=", "), "\n")
      
      # 確保有資料連接
      req(app_data_connection)
      
      # 確保有必要的數據
      profiles <- df_customer_profile()
      dna_data <- df_dna_by_customer()
      
      if (is.null(profiles) || is.null(dna_data)) {
        cat("DEBUG: Data sources are NULL\n")
        return()
      }
      
      # 清空選擇 - 遵循R88正確處理模組ID
      updateSelectizeInput(session, "customer_filter", selected = "")
      
      # 若有數據，顯示第一位客戶的資料（必須是有DNA的客戶）
      if (!is.null(dna_data) && nrow(dna_data) > 0 &&
          !is.null(profiles) && nrow(profiles) > 0) {
        # 獲取擁有DNA資料的客戶ID
        dna_customer_ids <- unique(dna_data$customer_id)
        filtered_profiles <- profiles %>% filter(customer_id %in% dna_customer_ids)
        
        if (nrow(filtered_profiles) > 0) {
          # 依照P80/R89確保使用整數ID
          first_customer_id <- as.integer(filtered_profiles$customer_id[1])
          
          filtered_dna <- dna_data %>% 
            dplyr::filter(customer_id == first_customer_id)
          filtered_customer <- profiles %>% 
            dplyr::filter(customer_id == first_customer_id)
          
          result <- dplyr::left_join(filtered_dna, filtered_customer, by = "customer_id")
          filtered_data(result)
        } else {
          # 若無DNA客戶，返回空資料框
          filtered_data(data.frame())
        }
      } else {
        # 若無數據，返回空資料框
        filtered_data(data.frame())
      }
    })
    
    # 初始化過濾結果 - 顯示第一筆資料
    observe({
      # 確保有資料連接 - 遵循R76 (Module Data Connection)
      req(app_data_connection)
      
      # 確保有必要的數據
      profiles <- df_customer_profile()
      dna_data <- df_dna_by_customer()
      
      if (is.null(profiles) || is.null(dna_data)) {
        return()
      }
      
      if (is.null(filtered_data())) {
        # 只顯示第一筆資料（避免初始載入過多）
        if (!is.null(dna_data) && nrow(dna_data) > 0 &&
            !is.null(profiles) && nrow(profiles) > 0) {
          # 獲取擁有DNA資料的客戶ID
          dna_customer_ids <- unique(dna_data$customer_id)
          filtered_profiles <- profiles %>% filter(customer_id %in% dna_customer_ids)
          
          if (nrow(filtered_profiles) > 0) {
            # 依照P80/R89，確保使用整數ID
            first_customer_id <- as.integer(filtered_profiles$customer_id[1])
            
            # 適用R90 (ID Relationship Validation)
            if (!first_customer_id %in% dna_data$customer_id) {
              cat("DEBUG: First customer has no DNA data\n")
              filtered_data(data.frame())
              return()
            }
            
            filtered_dna <- dna_data %>% 
              dplyr::filter(customer_id == first_customer_id)
            filtered_customer <- profiles %>% 
              dplyr::filter(customer_id == first_customer_id)
            
            # 遵循MP52 (Unidirectional Data Flow)，保持數據單向流動
            result <- dplyr::left_join(filtered_dna, filtered_customer, by = "customer_id")
            filtered_data(result)
          } else {
            # 若無DNA客戶，返回空資料框
            filtered_data(data.frame())
          }
        } else {
          filtered_data(data.frame())
        }
      }
    })
    
    # === 顯示功能 ===
    
    # 選定的客戶資料
    selected_customer_data <- reactive({
      data <- filtered_data()
      if (is.null(data) || nrow(data) == 0) {
        return(NULL)
      }
      return(data)
    })
    
    # 安全值存取輔助函數
    safeValue <- function(data, field, default = NA) {
      if (is.null(data) || nrow(data) == 0 || !field %in% names(data)) {
        return(default)
      }
      value <- data[[field]][1]
      if (is.null(value) || is.na(value)) {
        return(default)
      }
      return(value)
    }
    
    # 顯示客戶名稱和電子郵件
    output$customer_name <- renderUI({
      customer <- selected_customer_data()
      if (is.null(customer)) return(HTML("<span>未選擇客戶</span>"))
      
      name <- safeValue(customer, "buyer_name", default = "未知客戶")
      HTML(paste0("<span>", name, "</span>"))
    })
    
    output$customer_email <- renderUI({
      customer <- selected_customer_data()
      if (is.null(customer)) return(HTML("<span>無電子郵件</span>"))
      
      email <- safeValue(customer, "email", default = "無電子郵件")
      HTML(paste0("<span>", email, "</span>"))
    })
    
    # 渲染所有valueBoxes
    output$dna_time_first <- renderValueBox({
      customer <- selected_customer_data()
      
      if (is.null(customer)) {
        value <- defaults$dna_time_first
        subtitle <- "客戶資歷"
      } else {
        time_first <- safeValue(customer, "time_first", default = NA)
        time_first_to_now <- safeValue(customer, "time_first_to_now", default = NA)
        
        if (is.na(time_first)) {
          value <- defaults$dna_time_first
        } else {
          value <- format(time_first, "%Y-%m-%d")
        }
        
        if (is.na(time_first_to_now)) {
          subtitle <- paste("客戶資歷:", defaults$dna_time_first_to_now, "天")
        } else {
          subtitle <- paste("客戶資歷:", time_first_to_now, "天")
        }
      }
      
      bs4Dash::valueBox(
        value = value,
        subtitle = subtitle,
        icon = icon("calendar"),
        color = "primary"
      )
    })
    
    output$dna_recency <- renderValueBox({
      customer <- selected_customer_data()
      
      if (is.null(customer)) {
        label <- defaults$dna_r_label
        value <- defaults$dna_r_value
      } else {
        rlabel <- safeValue(customer, "r_label", default = NA)
        rvalue <- safeValue(customer, "r_value", default = NA)
        
        if (is.na(rlabel) || (!is.factor(rlabel) && !is.character(rlabel))) {
          label <- defaults$dna_r_label
        } else {
          label <- as.character(rlabel)
        }
        
        if (is.na(rvalue)) {
          value <- defaults$dna_r_value
        } else {
          value <- rvalue
        }
      }
      
      bs4Dash::valueBox(
        value = label,
        subtitle = paste("最近購買日(R):", round(as.numeric(value), 2), "天"),
        icon = icon("clock"),
        color = "danger"
      )
    })
    
    output$dna_frequency <- renderValueBox({
      customer <- selected_customer_data()
      
      if (is.null(customer)) {
        label <- defaults$dna_f_label
        value <- defaults$dna_f_value
      } else {
        flabel <- safeValue(customer, "f_label", default = NA)
        fvalue <- safeValue(customer, "f_value", default = NA)
        
        if (is.na(flabel) || (!is.factor(flabel) && !is.character(flabel))) {
          label <- defaults$dna_f_label
        } else {
          label <- as.character(flabel)
        }
        
        if (is.na(fvalue)) {
          value <- defaults$dna_f_value
        } else {
          value <- fvalue
        }
      }
      
      bs4Dash::valueBox(
        value = label,
        subtitle = paste("購買頻率(F):", value, "次"),
        icon = icon("chart-line"),
        color = "warning"
      )
    })
    
    output$dna_monetary <- renderValueBox({
      customer <- selected_customer_data()
      
      if (is.null(customer)) {
        label <- defaults$dna_m_label
        value <- defaults$dna_m_value
      } else {
        mlabel <- safeValue(customer, "m_label", default = NA)
        mvalue <- safeValue(customer, "m_value", default = NA)
        
        if (is.na(mlabel) || (!is.factor(mlabel) && !is.character(mlabel))) {
          label <- defaults$dna_m_label
        } else {
          label <- as.character(mlabel)
        }
        
        if (is.na(mvalue)) {
          value <- defaults$dna_m_value
        } else {
          value <- round(mvalue, 2)
        }
      }
      
      bs4Dash::valueBox(
        value = label,
        subtitle = paste("購買金額(M):", value),
        icon = icon("dollar-sign"),
        color = "success"
      )
    })
    
    output$dna_cai <- renderValueBox({
      customer <- selected_customer_data()
      
      if (is.null(customer)) {
        label <- defaults$dna_cai_label
        value <- defaults$dna_cai
      } else {
        cailabel <- safeValue(customer, "cai_label", default = NA)
        cai <- safeValue(customer, "cai", default = NA)
        
        if (is.na(cailabel) || (!is.factor(cailabel) && !is.character(cailabel))) {
          label <- defaults$dna_cai_label
        } else {
          label <- as.character(cailabel)
        }
        
        if (is.na(cai)) {
          value <- defaults$dna_cai
        } else {
          value <- round(cai, 2)
        }
      }
      
      bs4Dash::valueBox(
        value = label,
        subtitle = paste("顧客活躍度(CAI):", value),
        icon = icon("user-check"),
        color = "info"
      )
    })
    
    output$dna_ipt <- renderValueBox({
      customer <- selected_customer_data()
      
      if (is.null(customer)) {
        value <- defaults$dna_ipt_mean
      } else {
        ipt_mean <- safeValue(customer, "ipt_mean", default = NA)
        
        if (is.na(ipt_mean)) {
          value <- defaults$dna_ipt_mean
        } else {
          value <- round(ipt_mean, 1)
        }
      }
      
      bs4Dash::valueBox(
        value = value,
        subtitle = "顧客平均購買週期(IPT)",
        icon = icon("calendar-check"),
        color = "secondary"
      )
    })
    
    output$dna_pcv <- renderValueBox({
      customer <- selected_customer_data()
      
      if (is.null(customer)) {
        value <- defaults$dna_pcv
      } else {
        pcv <- safeValue(customer, "pcv", default = NA)
        
        if (is.na(pcv)) {
          value <- defaults$dna_pcv
        } else {
          value <- round(pcv, 2)
        }
      }
      
      bs4Dash::valueBox(
        value = value,
        subtitle = "過去價值(PCV)",
        icon = icon("history"),
        color = "primary"
      )
    })
    
    output$dna_clv <- renderValueBox({
      customer <- selected_customer_data()
      
      if (is.null(customer)) {
        value <- defaults$dna_clv
      } else {
        clv <- safeValue(customer, "clv", default = NA)
        
        if (is.na(clv)) {
          value <- defaults$dna_clv
        } else {
          value <- round(clv, 2)
        }
      }
      
      bs4Dash::valueBox(
        value = value,
        subtitle = "顧客終身價值(CLV)",
        icon = icon("gem"),
        color = "success"
      )
    })
    
    output$dna_cri <- renderValueBox({
      customer <- selected_customer_data()
      
      if (is.null(customer)) {
        value <- defaults$dna_cri
      } else {
        cri <- safeValue(customer, "cri", default = NA)
        
        if (is.na(cri)) {
          value <- defaults$dna_cri
        } else {
          value <- round(cri, 2)
        }
      }
      
      bs4Dash::valueBox(
        value = value,
        subtitle = "顧客交易穩定度(CRI)",
        icon = icon("balance-scale"),
        color = "warning"
      )
    })
    
    output$dna_nes <- renderValueBox({
      customer <- selected_customer_data()
      
      if (is.null(customer)) {
        value <- defaults$dna_nes_status
      } else {
        nesstatus <- safeValue(customer, "nes_status", default = NA)
        
        if (is.na(nesstatus)) {
          value <- defaults$dna_nes_status
        } else {
          value <- as.character(nesstatus)
        }
      }
      
      bs4Dash::valueBox(
        value = value,
        subtitle = "顧客狀態(NES)",
        icon = icon("user-tag"),
        color = "danger"
      )
    })
    
    output$dna_nt <- renderValueBox({
      customer <- selected_customer_data()
      
      if (is.null(customer)) {
        value <- defaults$dna_nt
      } else {
        nt <- safeValue(customer, "nt", default = NA)
        
        if (is.na(nt)) {
          value <- defaults$dna_nt
        } else {
          value <- round(nt, 2)
        }
      }
      
      bs4Dash::valueBox(
        value = value,
        subtitle = "新客單價",
        icon = icon("user-plus"),
        color = "info"
      )
    })
    
    output$dna_e0t <- renderValueBox({
      customer <- selected_customer_data()
      
      if (is.null(customer)) {
        value <- defaults$dna_e0t
      } else {
        e0t <- safeValue(customer, "e0t", default = NA)
        
        if (is.na(e0t)) {
          value <- defaults$dna_e0t
        } else {
          value <- round(e0t, 2)
        }
      }
      
      bs4Dash::valueBox(
        value = value,
        subtitle = "主力客單價",
        icon = icon("star"),
        color = "secondary"
      )
    })
    
    # 返回篩選後的資料
    return(filtered_data)
  })
}

####microCustomerInitialize####

#' Initialize the Micro Customer components (R76 Compliant)
#'
#' Integrates the filter and display components
#'
#' @param id Module ID
#' @param app_data_connection App data connection (NOT pre-filtered data)
#' @param config Optional configuration parameters
#'
#' @return A list containing UI elements and server function
#' @export
microCustomerInitialize <- function(id, app_data_connection = NULL, config = NULL) {
  # 不使用filter_ns和display_ns函數，而是使用原始id
  # 這樣可以避免Shiny模組ID的雙重命名空間問題，符合R88原則
  
  # Return the components
  list(
    ui = list(
      filter = microCustomerFilterUI(id),
      display = microCustomerUI(id)
    ),
    server = function(input, output, session) {
      # 使用原始ID而非後綴版本，讓Shiny處理命名空間
      filtered_data <- microCustomerServer(
        id, 
        app_data_connection
        # 不傳遞filter_id和display_id，使用原始id
      )
      
      # Return the filtered data for potential outside use
      return(filtered_data)
    }
  )
}