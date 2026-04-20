#' @principle P15 Debug Efficiency Exception
#' @principle P76 Error Handling Patterns
#' @principle P77 Performance Optimization
#' @principle P80 Integer ID Consistency
#' @principle P81 Tidyverse-Shiny Terminology Alignment
#' @principle R76 Module Data Connection Rule
#' @principle R88 Shiny Module ID Handling
#' @principle R89 Integer ID Type Conversion
#' @principle R90 ID Relationship Validation
#' @principle MP52 Unidirectional Data Flow
#' @principle MP53 Feedback Loop
#' @principle MP54 UI-Server Correspondence
#' @principle MP55 Computation Allocation
#' @principle MP56 Connected Component Principle
#' @principle R91 Universal Data Access Pattern
#' @r21_exception This file contains the UI-server-defaults triple for microCustomer2 module with configurable field selection
#' @justification These components are frequently debugged together during UI development and data integration
#' @refactor_plan To be refactored back into separate files once micro-level component design is finalized (est. Q3 2025)

# Load the universal data accessor if not already loaded
if (!exists("universal_data_accessor")) {
  source("update_scripts/global_scripts/00_principles/02_db_utils/fn_universal_data_accessor.R")
}

# NSQL Data Flow Documentation
# DATA_FLOW(component: customer_filter) {
#   SOURCE: app_data_connection
#   INITIALIZE: {
#     EXTRACT(app_data_connection → GET dna_data → dna_data)
#     EXTRACT(app_data_connection → GET customer_profiles → profiles)
#     EXTRACT(dna_data → DISTINCT customer_id → valid_ids)
#     FILTER(profiles → WHERE customer_id IN valid_ids → dropdown_options)
#   }
#   ON_SELECT: {
#     value = customer_filter.selected
#     FILTER(profiles → WHERE customer_id = as.integer(value) → customer_detail)
#     FILTER(dna_data → WHERE customer_id = as.integer(value) → customer_metrics)
#     JOIN(customer_detail → WITH customer_metrics ON customer_id → display_data)
#   }
# }

####microCustomer2Defaults####

#' Default Values for Micro Customer Component with configurable fields
#'
#' This function provides standard default values for the micro customer component.
#' These defaults ensure that all UI outputs have appropriate values even when
#' data is unavailable or invalid, implementing the UI-Server Pairing Rule.
#'
#' @return Named list of output IDs and their default values
#' @export
microCustomer2Defaults <- function() {
  list(
    # Customer header information
    customer_name = "未選擇客戶",
    customer_email = "無電子郵件",
    
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

####microCustomer2FilterUI####

#' Micro Customer2 Filter UI Component
#'
#' This component provides a filter interface for selecting customers
#' to be displayed in the micro-level customer analysis view.
#' Follows P75: Search Input Optimization pattern and P81: Tidyverse-Shiny Terminology Alignment.
#'
#' @param id The module ID
#' @param translate Translation function, defaults to identity function
#' @param filter_options Optional configuration for filter appearance
#'
#' @return A UI component for filtering customers
#' @export
microCustomer2FilterUI <- function(id, translate = function(x) x, filter_options = NULL) {
  ns <- NS(id)
  
  # Set default options if not provided
  if (is.null(filter_options)) {
    filter_options <- list(
      title = translate("客戶篩選"),
      placeholder = translate("開始輸入以搜尋客戶..."),
      clear_button = TRUE,
      clear_label = translate("清除篩選"),
      background_color = "#f8f9fa",
      search_min_chars = 2
    )
  }
  
  div(
    class = "customer-filter-container",
    style = paste0("padding: 15px; background-color: ", 
                  filter_options$background_color, 
                  "; border-radius: 5px; margin-bottom: 20px;"),
    fluidRow(
      column(
        width = 12,
        h4(filter_options$title, style = "margin-bottom: 15px;"),
        # 提供初始佔位符選項以改善用戶體驗
        selectizeInput(
          inputId = ns("customer_filter"), # P81: filter aligns with dplyr::filter
          label = translate("Select Customer:"),
          choices = c("-- 正在載入客戶資料 --" = ""), # 提供初始選項
          selected = NULL,
          options = list(
            placeholder = filter_options$placeholder,
            onInitialize = I('function() { this.setValue(""); }')
          )
        ),
        if (filter_options$clear_button) {
          div(
            style = "margin-top: 10px;",
            actionButton(
              inputId = ns("clear_filter"),
              label = filter_options$clear_label,
              icon = icon("times"),
              class = "btn-outline-secondary",
              width = "100%"
            )
          )
        }
      )
    )
  )
}

####microCustomer2UI####

#' Micro Customer2 UI Component with Configurable Fields
#'
#' This component provides the UI elements for displaying detailed customer analytics
#' in the micro-level view of the application, with the ability to include/exclude fields.
#'
#' @param id The module ID
#' @param include_fields List or vector of field names to include (NULL = all fields)
#' @param exclude_fields List or vector of field names to exclude (takes precedence over include)
#' @param translate Translation function, defaults to identity function
#'
#' @return A UI component
#' @export
microCustomer2UI <- function(id, 
                            include_fields = NULL, 
                            exclude_fields = NULL,
                            translate = function(x) x) {
  ns <- NS(id)
  
  # Define all available fields with their information
  all_fields <- list(
    # Row 1: Customer history, Recency, Frequency
    history = list(
      id = "dna_time_first",
      title = translate("顧客資歷"),
      icon = "calendar",
      color = "primary",
      width = 4,
      row = 1
    ),
    recency = list(
      id = "dna_recency",
      title = translate("最近購買日(R)"),
      icon = "clock",
      color = "danger",
      width = 4,
      row = 1
    ),
    frequency = list(
      id = "dna_frequency",
      title = translate("購買頻率(F)"),
      icon = "chart-line",
      color = "warning",
      width = 4,
      row = 1
    ),
    
    # Row 2: Monetary, CAI, IPT
    monetary = list(
      id = "dna_monetary",
      title = translate("購買金額(M)"),
      icon = "dollar-sign",
      color = "success",
      width = 4,
      row = 2
    ),
    cai = list(
      id = "dna_cai",
      title = translate("顧客活躍度(CAI)"),
      icon = "user-check",
      color = "info",
      width = 4,
      row = 2
    ),
    ipt = list(
      id = "dna_ipt",
      title = translate("顧客平均購買週期(IPT)"),
      icon = "calendar-check",
      color = "secondary",
      width = 4,
      row = 2
    ),
    
    # Row 3: PCV, CLV, CRI
    pcv = list(
      id = "dna_pcv",
      title = translate("過去價值(PCV)"),
      icon = "history",
      color = "primary",
      width = 4,
      row = 3
    ),
    clv = list(
      id = "dna_clv",
      title = translate("顧客終身價值(CLV)"),
      icon = "gem",
      color = "success",
      width = 4,
      row = 3
    ),
    cri = list(
      id = "dna_cri",
      title = translate("顧客交易穩定度(CRI)"),
      icon = "balance-scale",
      color = "warning",
      width = 4,
      row = 3
    ),
    
    # Row 4: NES, NT, E0T
    nes = list(
      id = "dna_nes",
      title = translate("顧客狀態(NES)"),
      icon = "user-tag",
      color = "danger",
      width = 4,
      row = 4
    ),
    nt = list(
      id = "dna_nt",
      title = translate("新客單價"),
      icon = "user-plus",
      color = "info",
      width = 4,
      row = 4
    ),
    e0t = list(
      id = "dna_e0t",
      title = translate("主力客單價"),
      icon = "star",
      color = "secondary",
      width = 4,
      row = 4
    )
  )
  
  # Filter the fields based on include/exclude
  selected_fields <- names(all_fields)
  
  # If specific fields are included, filter to only those fields
  if (!is.null(include_fields)) {
    selected_fields <- intersect(include_fields, selected_fields)
  }
  
  # If fields should be excluded, remove them
  if (!is.null(exclude_fields)) {
    selected_fields <- setdiff(selected_fields, exclude_fields)
  }
  
  # Get the list of fields to display, grouped by row
  display_fields <- all_fields[selected_fields]
  row_numbers <- sapply(display_fields, function(field) field$row)
  unique_rows <- sort(unique(row_numbers))
  
  # Create a tab panel with the dynamic content
  nav_panel(
    title = translate("微觀"),
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
        
        # Generate dynamic rows based on selected fields
        lapply(unique_rows, function(row_num) {
          # Get fields for this row
          row_fields <- display_fields[row_numbers == row_num]
          
          # Create a fluid row with the fields
          fluidRow(
            lapply(row_fields, function(field) {
              column(
                width = field$width,
                bs4Dash::valueBoxOutput(ns(field$id), width = 12)
              )
            })
          )
        })
      )
    )
  )
}

####microCustomer2Server####

#' Unified Micro Customer2 Server Component with Universal Data Access
#'
#' This component integrates both filtering and display logic for the micro customer
#' analytics view with configurable fields. It handles customer search, filtering, 
#' and data visualization.
#' Implements R91 Universal Data Access Pattern for compatibility with any connection type.
#'
#' @param id The module ID
#' @param app_data_connection App data connection object providing access to data sources.
#'        This can be any of: DBI connection, list with data frames, list with functions,
#'        reactive expression, or direct data frame.
#' @param include_fields List or vector of field names to include (NULL = all fields)
#' @param exclude_fields List or vector of field names to exclude (takes precedence over include)
#' @param session Current Shiny session
#'
#' @return Reactive expression that returns filtered data
#' @export
#' @implements R91 Universal Data Access Pattern
microCustomer2Server <- function(id, 
                               app_data_connection = NULL,
                               include_fields = NULL,
                               exclude_fields = NULL,
                               session = getDefaultReactiveDomain()) {
  # MP27 Data Flow Documentation:
  # REACTIVE(dna_data) {
  #   DEPENDS_ON: [app_data_connection]
  #   SOURCE: app_data_connection
  #   TRANSFORM: EXTRACT(app_data_connection → dna_data)
  # }
  
  # 創建整合的模組伺服器
  moduleServer(id, function(input, output, session) {
    # Log module initialization
    cat("INFO: Initializing microCustomer2 module with ID:", id, "\n")
    
    # Define all field ids for verification
    all_field_ids <- c(
      "dna_time_first", "dna_recency", "dna_frequency", 
      "dna_monetary", "dna_cai", "dna_ipt",
      "dna_pcv", "dna_clv", "dna_cri",
      "dna_nes", "dna_nt", "dna_e0t"
    )
    
    # Map between field names and IDs
    field_mapping <- list(
      history = "dna_time_first",
      recency = "dna_recency",
      frequency = "dna_frequency",
      monetary = "dna_monetary",
      cai = "dna_cai",
      ipt = "dna_ipt",
      pcv = "dna_pcv",
      clv = "dna_clv",
      cri = "dna_cri",
      nes = "dna_nes",
      nt = "dna_nt",
      e0t = "dna_e0t"
    )
    
    # Process field selections
    selected_field_names <- names(field_mapping)
    
    # If specific fields are included, filter to only those fields
    if (!is.null(include_fields)) {
      selected_field_names <- intersect(include_fields, selected_field_names)
    }
    
    # If fields should be excluded, remove them
    if (!is.null(exclude_fields)) {
      selected_field_names <- setdiff(selected_field_names, exclude_fields)
    }
    
    # Get the IDs of the selected fields
    selected_field_ids <- unlist(field_mapping[selected_field_names])
    cat("INFO: Selected field IDs:", paste(selected_field_ids, collapse = ", "), "\n")
    
    # == 定義常量 ==
    textRlabel <- c("極近", "近期", "一般", "久遠", "非常久遠")
    textFlabel <- c("極低", "低", "一般", "高", "非常高")
    textMlabel <- c("極低", "低", "一般", "高", "非常高")
    textCAIlabel <- c("不活躍", "低度活躍", "一般活躍", "活躍", "非常活躍")
    
    # == 默認值設置 ==
    defaults <- microCustomer2Defaults()
    
    # == 反應式數據來源 - 使用R91 Universal Data Access Pattern ==
    
    # MP55: Apply pre-computation for frequently accessed data
    # 從連接獲取DNA數據
    df_dna_by_customer <- reactive({
      # P76: 錯誤處理前先確認資料連接存在
      req(app_data_connection)
      
      # R91: 使用通用數據存取模式來處理不同類型的連接
      data <- universal_data_accessor(
        data_connection = app_data_connection,
        data_name = "dna_by_customer",
        log_level = 3
      )
      
      if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
        cat("WARNING: No DNA data available or returned empty dataset\n")
        return(NULL)
      }
      
      return(data)
    })
    
    # 從連接獲取客戶資料
    df_customer_profile <- reactive({
      # P76: 錯誤處理前先確認資料連接存在
      req(app_data_connection)
      
      # R91: 使用通用數據存取模式來處理不同類型的連接
      data <- universal_data_accessor(
        data_connection = app_data_connection, 
        data_name = "customer_profile",
        log_level = 3
      )
      
      if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
        cat("WARNING: No customer profile data available or returned empty dataset\n")
        return(NULL)
      }
      
      return(data)
    })
    
    # 取得有效的客戶ID列表 - 只包含有DNA數據的客戶
    valid_customer_ids <- reactive({
      # P76: 錯誤處理模式
      dna_data <- df_dna_by_customer()
      if (is.null(dna_data) || !is.data.frame(dna_data) || nrow(dna_data) == 0) {
        cat("DEBUG: No DNA data available for extracting customer IDs\n")
        return(integer(0))
      }
      
      # R89: 確保客戶ID為整數類型
      as.integer(unique(dna_data$customer_id))
    })
    
    # 存儲篩選後的客戶數據（選擇結果）
    filtered_data <- reactiveVal(NULL)
    
    # == 下拉選單初始化 - 為客戶篩選填充選項 ==
    observe({
      # P77: 性能優化 - 高效準備下拉選單
      profiles <- df_customer_profile()
      valid_ids <- valid_customer_ids()
      
      # P76: 錯誤處理 - 確保客戶資料存在
      if (is.null(profiles) || !is.data.frame(profiles) || nrow(profiles) == 0) {
        cat("DEBUG: Customer profiles data not available\n")
        updateSelectizeInput(
          session = session,
          inputId = "customer_filter",
          choices = c("-- 無客戶資料 --" = "")
        )
        return()
      }
      
      # P76: 錯誤處理 - 確保資料有正確的欄位
      if (!all(c("customer_id", "buyer_name", "email") %in% colnames(profiles))) {
        cat("DEBUG: Required fields missing in customer profiles\n")
        cat("DEBUG: Available fields:", paste(colnames(profiles), collapse=", "), "\n")
        
        # Attempt to map common field name variations
        id_field <- NULL
        if ("customer_id" %in% colnames(profiles)) {
          id_field <- "customer_id"
        } else if ("id" %in% colnames(profiles)) {
          id_field <- "id"
        }
        
        name_field <- NULL
        if ("buyer_name" %in% colnames(profiles)) {
          name_field <- "buyer_name"
        } else if ("name" %in% colnames(profiles)) {
          name_field <- "name"
        } else if ("customer_name" %in% colnames(profiles)) {
          name_field <- "customer_name"
        }
        
        email_field <- NULL
        if ("email" %in% colnames(profiles)) {
          email_field <- "email"
        } else if ("customer_email" %in% colnames(profiles)) {
          email_field <- "customer_email"
        }
        
        # If we can map all required fields, use them
        if (!is.null(id_field) && !is.null(name_field)) {
          cat("DEBUG: Using mapped fields: id=", id_field, ", name=", name_field, 
              if (!is.null(email_field)) paste(", email=", email_field, sep="") else "", "\n")
        } else {
          # Otherwise, let the user know fields are missing
          updateSelectizeInput(
            session = session,
            inputId = "customer_filter",
            choices = c("-- 客戶資料格式無效 --" = "")
          )
          return()
        }
      } else {
        # Standard field names
        id_field <- "customer_id"
        name_field <- "buyer_name"
        email_field <- "email"
      }
      
      # R90: ID關係驗證 - 只顯示有DNA數據的客戶
      if (length(valid_ids) > 0) {
        # MP52: 單向數據流 - 過濾用戶設置檔的數據
        filtered_profiles <- profiles %>% 
          dplyr::filter(.data[[id_field]] %in% valid_ids)
        
        if (nrow(filtered_profiles) == 0) {
          cat("DEBUG: No customers matched between profiles and DNA data\n")
          updateSelectizeInput(
            session = session,
            inputId = "customer_filter",
            choices = c("-- 無匹配客戶 --" = "")
          )
          return()
        }
        
        # P77: 性能優化 - 為UI準備高效的選項
        if (!is.null(email_field) && email_field %in% colnames(filtered_profiles)) {
          choices <- setNames(
            as.character(filtered_profiles[[id_field]]), # R89: 保留文本格式以正確顯示在UI中
            paste0(filtered_profiles[[name_field]], " (", filtered_profiles[[email_field]], ")")
          )
        } else {
          choices <- setNames(
            as.character(filtered_profiles[[id_field]]),
            filtered_profiles[[name_field]]
          )
        }
        
        cat("DEBUG: Created", length(choices), "customer choices\n")
        
        # MP53: 回饋循環 - 更新UI以反映可用的客戶
        updateSelectizeInput(
          session = session,
          inputId = "customer_filter", 
          choices = c("-- 請選擇客戶 --" = "", choices)
        )
      } else {
        # MP53: 回饋循環 - 沒有有效選項時通知用戶
        updateSelectizeInput(
          session = session,
          inputId = "customer_filter",
          choices = c("-- 無DNA客戶資料 --" = "")
        )
      }
    })
    
    # == 客戶選擇響應 - 當用戶選擇客戶時更新數據 ==
    observeEvent(input$customer_filter, {
      selected_value <- input$customer_filter
      cat("DEBUG: customer_filter changed to", selected_value, "\n")
      
      # P76: 錯誤處理
      profiles <- df_customer_profile()
      dna_data <- df_dna_by_customer()
      
      if (is.null(profiles) || is.null(dna_data)) {
        cat("DEBUG: Data sources are NULL\n")
        return()
      }
      
      # Identify field names in the datasets
      id_field <- if ("customer_id" %in% colnames(profiles)) "customer_id" else "id"
      
      # 只在有選擇時進行篩選
      if (!is.null(selected_value) && selected_value != "") {
        # R89: ID類型轉換規則 - 確保ID為整數
        selected_customer_id <- as.integer(selected_value)
        cat("DEBUG: Selected customer ID:", selected_customer_id, "\n")
        
        # R90: ID關係驗證 - 確認客戶存在
        if (!selected_customer_id %in% profiles[[id_field]]) {
          cat("DEBUG: Invalid customer ID - not found in profiles\n")
          # MP53: 回饋循環 - 通知用戶無效選擇
          showNotification("選擇的客戶ID無效", type = "warning")
          return()
        }
        
        # MP52: 單向數據流 - 按ID過濾兩個數據源
        filtered_customer <- profiles %>%
          dplyr::filter(.data[[id_field]] == selected_customer_id)
        
        filtered_dna <- dna_data %>%
          dplyr::filter(customer_id == selected_customer_id)
        
        # R90: 關係驗證 - 確認客戶有DNA數據
        if (nrow(filtered_dna) == 0) {
          cat("DEBUG: Customer has no DNA data\n")
          # MP53: 回饋循環 - 通知用戶沒有數據
          showNotification("所選客戶無DNA資料", type = "warning")
          filtered_data(data.frame())
          return()
        }
        
        # MP52: 單向數據流 - 合併資料以供顯示
        if (nrow(filtered_dna) > 0 && nrow(filtered_customer) > 0) {
          # Find common join field
          if (id_field == "customer_id") {
            result <- dplyr::left_join(filtered_dna, filtered_customer, by = "customer_id")
          } else {
            result <- dplyr::left_join(filtered_dna, filtered_customer, 
                                       by = c("customer_id" = id_field))
          }
          filtered_data(result)
        } else {
          # 若找不到匹配的資料，返回空資料框
          filtered_data(data.frame())
        }
      } else if (!is.null(dna_data) && nrow(dna_data) > 0 &&
                 !is.null(profiles) && nrow(profiles) > 0) {
        # 若無選擇但有數據，顯示第一位有DNA數據的客戶
        valid_ids <- valid_customer_ids()
        
        if (length(valid_ids) > 0) {
          # R89: ID類型轉換 - 確保ID為整數
          first_customer_id <- as.integer(valid_ids[1])
          
          filtered_dna <- dna_data %>% 
            dplyr::filter(customer_id == first_customer_id)
          
          if (id_field == "customer_id") {
            filtered_customer <- profiles %>% 
              dplyr::filter(customer_id == first_customer_id)
            result <- dplyr::left_join(filtered_dna, filtered_customer, by = "customer_id")
          } else {
            filtered_customer <- profiles %>% 
              dplyr::filter(.data[[id_field]] == first_customer_id)
            result <- dplyr::left_join(filtered_dna, filtered_customer, 
                                      by = c("customer_id" = id_field))
          }
          
          filtered_data(result)
        } else {
          # 若無有效客戶，返回空資料框
          filtered_data(data.frame())
        }
      } else {
        # 若無選擇且無數據，返回空資料框
        filtered_data(data.frame())
      }
    })
    
    # == 清除篩選響應 ==
    observeEvent(input$clear_filter, {
      cat("DEBUG: clear_filter button clicked\n")
      
      # R88: Shiny模組ID處理 - 正確更新UI元素
      updateSelectizeInput(session, "customer_filter", selected = "")
      
      # P76: 錯誤處理
      profiles <- df_customer_profile()
      dna_data <- df_dna_by_customer()
      
      if (is.null(profiles) || is.null(dna_data)) {
        cat("DEBUG: Data sources are NULL\n")
        return()
      }
      
      # Identify field names
      id_field <- if ("customer_id" %in% colnames(profiles)) "customer_id" else "id"
      
      # 自動選擇第一位客戶
      valid_ids <- valid_customer_ids()
      if (length(valid_ids) > 0) {
        # R89: ID類型轉換 - 確保ID為整數
        first_customer_id <- as.integer(valid_ids[1])
        
        filtered_dna <- dna_data %>% 
          dplyr::filter(customer_id == first_customer_id)
        
        if (id_field == "customer_id") {
          filtered_customer <- profiles %>% 
            dplyr::filter(customer_id == first_customer_id)
          result <- dplyr::left_join(filtered_dna, filtered_customer, by = "customer_id")
        } else {
          filtered_customer <- profiles %>% 
            dplyr::filter(.data[[id_field]] == first_customer_id)
          result <- dplyr::left_join(filtered_dna, filtered_customer, 
                                    by = c("customer_id" = id_field))
        }
        
        filtered_data(result)
      } else {
        # 若無有效客戶，返回空資料框
        filtered_data(data.frame())
      }
    })
    
    # == 模組初始化 - 自動選擇第一位客戶 ==
    observe({
      # 此反應式表達式處理模組初始化
      
      # P76: 錯誤處理
      profiles <- df_customer_profile()
      dna_data <- df_dna_by_customer()
      
      if (is.null(profiles) || is.null(dna_data)) {
        return()
      }
      
      # Identify field names
      id_field <- if ("customer_id" %in% colnames(profiles)) "customer_id" else "id"
      
      # 只在還沒有選擇數據時進行初始化
      if (is.null(filtered_data())) {
        valid_ids <- valid_customer_ids()
        
        if (length(valid_ids) > 0) {
          # R89: ID類型轉換 - 確保ID為整數
          first_customer_id <- as.integer(valid_ids[1])
          
          # R90: 關係驗證 - 確認客戶存在於兩個數據集
          if (!first_customer_id %in% profiles[[id_field]] || 
              !first_customer_id %in% dna_data$customer_id) {
            cat("DEBUG: First customer ID is invalid\n")
            filtered_data(data.frame())
            return()
          }
          
          filtered_dna <- dna_data %>% 
            dplyr::filter(customer_id == first_customer_id)
          
          if (id_field == "customer_id") {
            filtered_customer <- profiles %>% 
              dplyr::filter(customer_id == first_customer_id)
            result <- dplyr::left_join(filtered_dna, filtered_customer, by = "customer_id")
          } else {
            filtered_customer <- profiles %>% 
              dplyr::filter(.data[[id_field]] == first_customer_id)
            result <- dplyr::left_join(filtered_dna, filtered_customer, 
                                      by = c("customer_id" = id_field))
          }
          
          # MP52: 單向數據流 - 合併資料
          filtered_data(result)
        } else {
          # 若無有效客戶，返回空資料框
          filtered_data(data.frame())
        }
      }
    })
    
    # == 顯示功能 - 準備數據以供UI顯示 ==
    
    # 選定的客戶資料 - 添加錯誤處理
    selected_customer_data <- reactive({
      data <- filtered_data()
      if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
        return(NULL)
      }
      return(data)
    })
    
    # 安全值存取輔助函數 - 提高強健性
    safeValue <- function(data, field, default = NA) {
      # P76: 錯誤處理 - 更強健的值獲取函數
      if (is.null(data) || !is.data.frame(data) || nrow(data) == 0 || !field %in% names(data)) {
        return(default)
      }
      value <- data[[field]][1]
      if (is.null(value) || is.na(value)) {
        return(default)
      }
      return(value)
    }
    
    # == UI輸出渲染 - 將數據呈現在UI中 ==
    
    # MP56: Connected Component - Define the output elements only for selected fields
    
    # 顯示客戶名稱和電子郵件
    output$customer_name <- renderUI({
      # MP54: UI-Server對應 - 確保UI元素有伺服器功能
      customer <- selected_customer_data()
      if (is.null(customer)) return(HTML("<span>未選擇客戶</span>"))
      
      # Find the name field
      name_field <- NULL
      if ("buyer_name" %in% colnames(customer)) {
        name_field <- "buyer_name"
      } else if ("name" %in% colnames(customer)) {
        name_field <- "name"
      } else if ("customer_name" %in% colnames(customer)) {
        name_field <- "customer_name"
      }
      
      if (is.null(name_field)) {
        return(HTML("<span>未知客戶</span>"))
      }
      
      name <- safeValue(customer, name_field, default = "未知客戶")
      HTML(paste0("<span>", name, "</span>"))
    })
    
    output$customer_email <- renderUI({
      # MP54: UI-Server對應 - 確保UI元素有伺服器功能
      customer <- selected_customer_data()
      if (is.null(customer)) return(HTML("<span>無電子郵件</span>"))
      
      # Find the email field
      email_field <- NULL
      if ("email" %in% colnames(customer)) {
        email_field <- "email"
      } else if ("customer_email" %in% colnames(customer)) {
        email_field <- "customer_email"
      }
      
      if (is.null(email_field)) {
        return(HTML("<span>無電子郵件</span>"))
      }
      
      email <- safeValue(customer, email_field, default = "無電子郵件")
      HTML(paste0("<span>", email, "</span>"))
    })
    
    # MP56: Only create output renderers for fields that were selected
    
    # Customer history metrics - Time first
    if ("dna_time_first" %in% selected_field_ids) {
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
    }
    
    # RFM metrics - Recency
    if ("dna_recency" %in% selected_field_ids) {
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
    }
    
    # RFM metrics - Frequency
    if ("dna_frequency" %in% selected_field_ids) {
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
    }
    
    # RFM metrics - Monetary
    if ("dna_monetary" %in% selected_field_ids) {
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
    }
    
    # Customer activity metrics - CAI
    if ("dna_cai" %in% selected_field_ids) {
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
    }
    
    # Customer activity metrics - IPT
    if ("dna_ipt" %in% selected_field_ids) {
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
    }
    
    # Value metrics - PCV
    if ("dna_pcv" %in% selected_field_ids) {
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
    }
    
    # Value metrics - CLV
    if ("dna_clv" %in% selected_field_ids) {
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
    }
    
    # Value metrics - CRI
    if ("dna_cri" %in% selected_field_ids) {
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
    }
    
    # Status metrics - NES
    if ("dna_nes" %in% selected_field_ids) {
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
    }
    
    # Transaction metrics - NT
    if ("dna_nt" %in% selected_field_ids) {
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
    }
    
    # Transaction metrics - E0T
    if ("dna_e0t" %in% selected_field_ids) {
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
    }
    
    # 返回篩選後的資料
    return(filtered_data)
  })
}

####microCustomer2Component####

#' Create a Micro Customer2 component with configurable fields
#'
#' Integrates the filter and display components following R91 Universal Data Access Pattern.
#' This function returns a component that can be used as a whole or by individual parts
#' within union components.
#'
#' @param id Module ID
#' @param app_data_connection App data connection (any supported connection type)
#' @param include_fields List or vector of field names to include (NULL = all fields)
#' @param exclude_fields List or vector of field names to exclude (takes precedence over include)
#' @param filter_options Optional configuration for filter appearance
#' @param translate Translation function, defaults to identity function
#'
#' @return A list containing UI elements and server function
#' @export
#' @principle MP56 Connected Component Principle
microCustomer2Component <- function(id, 
                                   app_data_connection = NULL, 
                                   include_fields = NULL,
                                   exclude_fields = NULL,
                                   filter_options = NULL,
                                   translate = function(x) x) {
  # R88: 不使用自定义namespace后缀，防止双重命名空间问题
  # 使用Shiny内置的命名空间机制
  
  # Return the components as a structured list for flexible component access
  list(
    ui = list(
      filter = microCustomer2FilterUI(id, translate, filter_options),
      display = microCustomer2UI(id, include_fields, exclude_fields, translate)
    ),
    server = function(input, output, session) {
      # Pass field parameters to server function
      filtered_data <- microCustomer2Server(
        id, 
        app_data_connection,
        include_fields,
        exclude_fields,
        session = session
      )
      
      # Return the filtered data for potential outside use
      return(filtered_data)
    }
  )
}

#' @rdname microCustomer2Component
#' @export
#' @principle R12 Minimal Modification
#' @deprecated Please use microCustomer2Component instead
microCustomer2Initialize <- function(id, 
                                   app_data_connection = NULL, 
                                   include_fields = NULL,
                                   exclude_fields = NULL,
                                   filter_options = NULL,
                                   translate = function(x) x) {
  # Create alias for backward compatibility
  microCustomer2Component(id, app_data_connection, include_fields, exclude_fields, filter_options, translate)
}