# =============================================================
# union_production_test.R
# 2025‑04‑19 – 更新為使用 tbl2 進行數據存取 (R116 Enhanced Data Access)
# 2025‑04‑17 – 清理結構 + 動態 Filter 區塊 (dynamic_filter)
# 2025-04-24 – 修正 Platform 選項處理邏輯 (按照 MP081 原則)
# 2025-05-16 – 新增 positionTable 組件 (按照 MP56、MP081 原則)
# 2025-05-22 – 新增 positionDNAPlotly、positionCSAPlotly、positionKFE、positionIdealRate、positionStrategy、microMacroKPI 組件
# =============================================================

# IMPORTANT CODE MAINTENANCE NOTES:
# 1. radioButtons parameters MUST be named explicitly (inputId, label, choices, selected) per MP081
# 2. Platform switch handling MUST use consistent variable names throughout the observer
# 3. Any variables used in notification must also be updated in the logging section
# 4. Currently supported platforms: Amazon (ID: 2) and All Platforms (ID: 0)
# 5. Position components include table, DNA plotly, CSA plotly, and KFE analysis
# =============================================================

# ---- 0. 初始化 ------------------------------------------------------------
# MP47: Functional Programming - Safe project root detection for deployment
find_and_set_project_root <- function(){
  p <- getwd()
  
  # Check if we're already in the right directory (deployment case)
  if (file.exists(file.path(p, "app.R")) && 
      dir.exists(file.path(p, "update_scripts", "global_scripts"))) {
    message("工作目錄已設定為 (deployment): ", p)
    return(TRUE)
  }
  
  # Original logic for local development
  while(!endsWith(p,"precision_marketing_app") && !file.exists(file.path(p,"precision_marketing_app"))){
    nxt <- dirname(p)
    if(nxt==p){ 
      warning("找不到 precision_marketing_app 目錄，使用當前目錄")
      return(TRUE)  # Continue with current directory in deployment
    }
    p <- nxt 
  }
  
  if(!endsWith(p,"precision_marketing_app")) p <- file.path(p,"precision_marketing_app")
  if(dir.exists(p)){ 
    setwd(p)
    message("工作目錄已設定為: ", p)
    return(TRUE)
  } else {
    warning("無法設定專案根目錄，使用當前目錄")
    return(TRUE)
  }
}

# R68: Object Initialization - Load Rprofile to get autoinit function
rprofile_path <- file.path("update_scripts", "global_scripts", "22_initializations", "sc_Rprofile.R")
if (file.exists(rprofile_path)) {
  source(rprofile_path)
  message(">> 已載入 sc_Rprofile.R")
} else {
  warning("找不到 sc_Rprofile.R，創建備用 autoinit 函數")
  # Fallback autoinit function if sc_Rprofile.R not found
  autoinit <- function() {
    message(">> 執行備用初始化")
    assign("PACKAGES_INITIALIZED", list(status = "minimal"), envir = .GlobalEnv)
    assign("INITIALIZATION_COMPLETED", TRUE, envir = .GlobalEnv)
    assign("translate", function(x) x, envir = .GlobalEnv)
    invisible(NULL)
  }
}

find_and_set_project_root()
autoinit()

# R68: Object Initialization - Ensure required variables exist
if (!exists("PACKAGES_INITIALIZED")) {
  PACKAGES_INITIALIZED <- list(status = "minimal")
}

# ---- 1. 套件 --------------------------------------------------------------
library(shiny); library(bs4Dash); library(plotly); library(dplyr); library(shinyjs); library(DBI); library(DT); library(tidyr); library(MASS); library(stringr); library(httr2); library(jsonlite)

# ---- 2. 連線 --------------------------------------------------------------
# Connect to the app database using the Enhanced Data Access approach (R116)
connect_to_app_database <- function(db_path = "app_data/app_data.duckdb") {
  tryCatch({
    if (!requireNamespace("DBI", quietly = TRUE)) {
      message("Installing DBI package...")
      install.packages("DBI")
    }
    
    if (!requireNamespace("duckdb", quietly = TRUE)) {
      message("Installing duckdb package...")
      install.packages("duckdb")
    }
    
    # Check if file exists
    if (!file.exists(db_path)) {
      stop("Database file does not exist at path: ", db_path)
    }
    
    # 1. 關閉所有先前的連線（如果有）
    #    並且釋放掉該 driver instance  
    if (exists("conn")) {
      DBI::dbDisconnect(conn, shutdown = TRUE)
    }
    
    # 2. 建立一個新的 DuckDB driver，並在這裡指定 dbdir 與 read_only = TRUE
    drv_ro <- duckdb::duckdb(
      dbdir     = db_path,     # 你的 .duckdb 檔案路徑
      read_only = TRUE         # 唯讀模式
    )
    
    # 3. 用這個唯讀 driver 建立連線
    conn_ro <- DBI::dbConnect(drv_ro)
    
    # 4. 驗證
    conn_ro
    # 應該會看到類似：
    # <duckdb_connection ... read_only=TRUE ...>
    
    return(conn_ro)
  }, error = function(e) {
    message("Error connecting to database: ", e$message)
    return(NULL)
  })
}

app_connection <- connect_to_app_database()

# ---- 3. Component 載入 ----------------------------------------------------
# 確保載入 position 相關元件
source(file.path("update_scripts", "global_scripts", "10_rshinyapp_components", "position", "positionTable", "positionTable.R"))
source(file.path("update_scripts", "global_scripts", "10_rshinyapp_components", "position", "positionDNAPlotly", "positionDNAPlotly.R"))
source(file.path("update_scripts", "global_scripts", "10_rshinyapp_components", "position", "positionMSPlotly", "positionMSPlotly.R"))
source(file.path("update_scripts", "global_scripts", "10_rshinyapp_components", "position", "positionKFE", "positionKFE.R"))
source(file.path("update_scripts", "global_scripts", "10_rshinyapp_components", "position", "positionIdealRate", "positionIdealRate.R"))
source(file.path("update_scripts", "global_scripts", "10_rshinyapp_components", "position", "positionStrategy", "positionStrategy.R"))
source(file.path("update_scripts", "global_scripts", "10_rshinyapp_components", "micro", "microMacroKPI", "microMacroKPI.R"))

# 載入 Poisson 分析相關元件
source(file.path("update_scripts", "global_scripts", "10_rshinyapp_components", "poisson", "poissonTimeAnalysis", "poissonTimeAnalysis.R"))
source(file.path("update_scripts", "global_scripts", "10_rshinyapp_components", "poisson", "poissonFeatureAnalysis", "poissonFeatureAnalysis.R"))
source(file.path("update_scripts", "global_scripts", "10_rshinyapp_components", "poisson", "poissonCommentAnalysis", "poissonCommentAnalysis.R"))

# 翻譯函數
translate <- function(x) x   # 佔位翻譯函數

# ---- 4. CSS ---------------------------------------------------------------
css_deps <- tags$head(tags$style(HTML(
  ".sidebar .selectize-dropdown{width:auto!important;min-width:100%!important}.sidebar .selectize-dropdown-content{min-width:100%}
  /* 確保 DataTables 的滾動條始終顯示 */
  .dataTables_scrollBody { overflow-x: scroll !important; }
  /* macOS 滾動條始終顯示的全域設定 */
  .dataTables_scrollBody::-webkit-scrollbar { -webkit-appearance: none; }
  .dataTables_scrollBody::-webkit-scrollbar:horizontal { height: 10px; }
  .dataTables_scrollBody::-webkit-scrollbar-thumb { border-radius: 5px; background-color: rgba(0,0,0,.3); }
  .dataTables_scrollBody::-webkit-scrollbar-track { background-color: rgba(0,0,0,.1); border-radius: 5px; }
  /* 確保 filter panel 中的按鈕有適當的左右邊距 */
  .well .btn-block { margin-left: 0 !important; margin-right: 0 !important; width: 100% !important; }
  .wellPanel .btn-block { margin-left: 0 !important; margin-right: 0 !important; width: 100% !important; }
  /* 調整 sidebar accordion 樣式 */
  .sidebar-section .accordion { margin: 0 -15px; }
  .sidebar-section .accordion .card { 
    border: none; 
    background-color: transparent;
    margin-bottom: 0.5rem;
  }
  .sidebar-section .accordion .card-header { 
    background-color: var(--primary); 
    border: 1px solid var(--primary);
    padding: 0.5rem 1rem;
    color: #fff !important;
  }
  .sidebar-section .accordion .card-header:hover { 
    background-color: var(--primary); 
    filter: brightness(1.1);
  }
  /* 確保 accordion 標題文字可見 */
  .sidebar-section .accordion .card-header .btn { 
    color: #fff !important;
    font-size: 1.1rem;
  }
  .sidebar-section .accordion .card-header h2 { 
    margin: 0; 
    font-size: 1rem;
  }
  .sidebar-section .accordion .btn-link { 
    color: #fff !important; 
    text-decoration: none;
    font-weight: 500;
    width: 100%;
    text-align: left;
    padding: 0;
    display: flex;
    justify-content: space-between;
    align-items: center;
  }
  .sidebar-section .accordion .btn-link:hover { 
    color: #fff !important; 
    text-decoration: none;
  }
  .sidebar-section .accordion .btn-link::after {
    content: '';
    display: inline-block;
    margin-left: auto;
    transition: transform 0.2s;
  }
  .sidebar-section .accordion .card-body { 
    padding: 0.75rem; 
    background-color: transparent;
    border-left: 1px solid var(--primary);
    border-right: 1px solid var(--primary);
    border-bottom: 1px solid var(--primary);
  }
  .sidebar-section .radio { margin-top: 0.25rem; margin-bottom: 0.25rem; }
  .sidebar-section .shiny-options-group { margin-top: 0; }"
)))

# ---- 5. UI ---------------------------------------------------------------
ui <- bs4DashPage(
  title = translate("AI Marketing Platform"), fullscreen = TRUE,
  header = bs4DashNavbar(title = bs4DashBrand(translate("AI Marketing Platform")), skin="light", status="primary"),
  sidebar = bs4DashSidebar(status="primary", width="300px", elevation = 3,minified  = FALSE,
                           sidebarMenu(id="sidebar_menu",
                                       bs4SidebarHeader("Application Settings"),
                                       bs4SidebarMenuItem("Customer DNA",   tabName="microCustomer", icon=icon("users")),
                                       bs4SidebarMenuItem("Macro KPI", tabName="microMacroKPI", icon=icon("tachometer-alt")),
                                       bs4SidebarMenuItem("DNA Distribution", tabName="dna",          icon=icon("chart-bar")),
                                       bs4SidebarMenuItem("Position Table", tabName="position",      icon=icon("table")),
                                       bs4SidebarMenuItem("Position DNA Plot", tabName="positionDNA", icon=icon("chart-line")),
                                       bs4SidebarMenuItem("Market Segmentation", tabName="positionMS", icon=icon("crosshairs")),
                                       bs4SidebarMenuItem("Key Factors", tabName="positionKFE", icon=icon("key")),
                                       bs4SidebarMenuItem("Ideal Rate Analysis", tabName="positionIdealRate", icon=icon("star")),
                                       bs4SidebarMenuItem("Strategy Analysis", tabName="positionStrategy", icon=icon("compass")),
                                       bs4SidebarMenuItem("時間區段分析", tabName="poissonTime", icon=icon("clock")),
                                       bs4SidebarMenuItem("精準模型", tabName="poissonFeature", icon=icon("bullseye")),
                                       bs4SidebarMenuItem("產品賽道分析", tabName="poissonComment", icon=icon("trophy")) ),
                           # 共用 Platform Filter with collapsible sections
                           div(class="sidebar-section p-3 mt-2",
                               bs4Accordion(
                                 id = "sidebar_accordion",
                                 bs4AccordionItem(
                                   title = "Platform",
                                   status = "primary",
                                   collapsed = FALSE,  # 預設展開
                                   # Following MP081: Explicit Parameter Specification Metaprinciple
                                   radioButtons(
                                     inputId = "platform",
                                     label = NULL,
                                     choices = df_platform %>%
                                       filter(platform_id %in% c("all", app_configs$platform)) %>%
                                       { setNames(.$platform_id, .$platform_name_english) },
                                     selected = "all"
                                   )
                                 ),
                                 bs4AccordionItem(
                                   title = "Product line",
                                   status = "primary",
                                   collapsed = FALSE,  # 預設展開
                                   radioButtons(
                                     inputId = "product_line",
                                     label = NULL,
                                     choices = df_product_line %>%
                                       filter(included) %>%
                                       { setNames(.$product_line_id, .$product_line_name_english) },
                                     selected = "all"
                                   )
                                 )
                               )),
                           # 動態 filter 容器
                           uiOutput("dynamic_filter") ),
  
  body = bs4DashBody(css_deps, useShinyjs(),
                     bs4TabItems(
                       bs4TabItem(tabName="microCustomer", fluidRow(column(12, bs4Card(title="Customer DNA Analysis", status="primary", width=12, solidHeader=TRUE, elevation=3, uiOutput("cust_display"))))),
                       bs4TabItem(tabName="microMacroKPI", fluidRow(column(12, bs4Card(title="Key Performance Indicators", status="info", width=12, solidHeader=TRUE, elevation=3, uiOutput("micro_macro_kpi_display"))))),
                       bs4TabItem(tabName="dna",          fluidRow(column(12, bs4Card(title="DNA Distribution",     status="primary", width=12, solidHeader=TRUE, elevation=3, uiOutput("dna_display"))))),
                       bs4TabItem(tabName="position",      fluidRow(column(12, bs4Card(title="Position Analysis",    status="primary", width=12, solidHeader=TRUE, elevation=3, uiOutput("position_display"))))),
                       bs4TabItem(tabName="positionDNA",   fluidRow(column(12, bs4Card(title="Position DNA Visualization", status="primary", width=12, solidHeader=TRUE, elevation=3, uiOutput("position_dna_display"))))),
                       bs4TabItem(tabName="positionMS",    fluidRow(
                         column(12, bs4Card(title="Market Segmentation Analysis", status="primary", width=12, solidHeader=TRUE, elevation=3, uiOutput("position_ms_display"))),
                         column(12, bs4Card(title="Key Factor Evaluation", status="info", width=12, solidHeader=TRUE, elevation=3, uiOutput("position_kfe_display")))
                       )),
                       bs4TabItem(tabName="positionKFE",   fluidRow(column(12, bs4Card(title="Key Factor Analysis", status="success", width=12, solidHeader=TRUE, elevation=3, uiOutput("position_kfe_full_display"))))),
                       bs4TabItem(tabName="positionIdealRate", fluidRow(column(12, bs4Card(title="Ideal Rate Analysis", status="warning", width=12, solidHeader=TRUE, elevation=3, uiOutput("position_ideal_rate_display"))))),
                       bs4TabItem(tabName="positionStrategy", fluidRow(column(12, bs4Card(title="Strategic Position Analysis", status="danger", width=12, solidHeader=TRUE, elevation=3, uiOutput("position_strategy_display"))))),
                       bs4TabItem(tabName="poissonTime", fluidRow(column(12, bs4Card(title="時間區段分析", status="primary", width=12, solidHeader=TRUE, elevation=3, uiOutput("poisson_time_display"))))),
                       bs4TabItem(tabName="poissonFeature", fluidRow(column(12, bs4Card(title="精準模型分析", status="success", width=12, solidHeader=TRUE, elevation=3, uiOutput("poisson_feature_display"))))),
                       bs4TabItem(tabName="poissonComment", fluidRow(column(12, bs4Card(title="產品賽道分析", status="info", width=12, solidHeader=TRUE, elevation=3, uiOutput("poisson_comment_display"))))) ) ),
  
  footer = dashboardFooter(fixed=TRUE, right="Version 1.0.0 | 2025") )

# ---- 6. Server -----------------------------------------------------------
server <- function(input, output, session){
  # ---- 6.1 共享設定 ------------------------------------------------------
  comp_config <- reactive({
    list(
      filters    = list(platform_id = input$platform,
                        product_line_id = input$product_line),
      active_tab = input$sidebar_menu        # ← 新增
    )
  })
  
  # ---- 6.2 Component instances -----------------------------------------
  customer_comp <- microCustomerComponent("cust", app_connection, comp_config, translate)
  dna_comp      <- microDNADistributionComponent("dna", app_connection, comp_config, translate)
  position_comp <- positionTableComponent("position", app_connection, comp_config, translate)
  position_dna_comp <- positionDNAPlotlyComponent("position_dna", app_connection, comp_config, translate)
  position_ms_comp <- positionMSPlotlyComponent("position_ms", app_connection, comp_config, translate)
  position_kfe_comp <- positionKFEComponent("position_kfe", app_connection, comp_config, translate, display_mode = "compact")
  position_kfe_full_comp <- positionKFEComponent("position_kfe_full", app_connection, comp_config, translate, display_mode = "full")
  position_ideal_rate_comp <- positionIdealRateComponent("position_ideal_rate", app_connection, comp_config, translate)
  position_strategy_comp <- positionStrategyComponent("position_strategy", app_connection, comp_config, translate)
  micro_macro_kpi_comp <- microMacroKPIComponent("micro_macro_kpi", app_connection, comp_config, translate)
  
  # Poisson analysis components
  poisson_time_comp <- poissonTimeAnalysisComponent("poisson_time", app_connection, comp_config, translate)
  poisson_feature_comp <- poissonFeatureAnalysisComponent("poisson_feature", app_connection, comp_config, translate)
  poisson_comment_comp <- poissonCommentAnalysisComponent("poisson_comment", app_connection, comp_config, translate)
  
  # ---- 6.3 動態 Filter 注入 --------------------------------------------
  output$dynamic_filter <- renderUI({ switch(input$sidebar_menu,
                                             "microCustomer" = customer_comp$ui$filter,
                                             "dna"           = dna_comp$ui$filter,
                                             "position"      = position_comp$ui$filter,
                                             "positionDNA"   = position_dna_comp$ui$filter,
                                             "positionMS"    = position_ms_comp$ui$filter,
                                             "positionKFE"   = position_kfe_full_comp$ui$filter,
                                             "positionIdealRate" = position_ideal_rate_comp$ui$filter,
                                             "positionStrategy" = position_strategy_comp$ui$filter,
                                             "microMacroKPI" = micro_macro_kpi_comp$ui$filter,
                                             "poissonTime"   = poisson_time_comp$ui$filter,
                                             "poissonFeature" = poisson_feature_comp$ui$filter,
                                             "poissonComment" = poisson_comment_comp$ui$filter, NULL) })
  
  # ---- 6.4 Component UI (using renderUI2 for conditional rendering) -------
  output$cust_display     <- renderUI2(
    current_tab = input$sidebar_menu,
    target_tab = "microCustomer", 
    ui_component = customer_comp$ui$display
  )
  
  output$dna_display      <- renderUI2(
    current_tab = input$sidebar_menu,
    target_tab = "dna", 
    ui_component = dna_comp$ui$display
  )
  
  output$position_display <- renderUI2(
    current_tab = input$sidebar_menu,
    target_tab = "position", 
    ui_component = position_comp$ui$display
  )
  
  output$position_dna_display <- renderUI2(
    current_tab = input$sidebar_menu,
    target_tab = "positionDNA", 
    ui_component = position_dna_comp$ui$display
  )
  
  output$position_ms_display <- renderUI2(
    current_tab = input$sidebar_menu,
    target_tab = "positionMS", 
    ui_component = position_ms_comp$ui$display,
    loading_icon = "crosshairs"
  )
  
  output$position_kfe_display <- renderUI2(
    current_tab = input$sidebar_menu,
    target_tab = "positionMS",  # KFE is shown on positionMS tab
    ui_component = position_kfe_comp$ui$display,
    loading_icon = "key"
  )
  
  output$position_kfe_full_display <- renderUI2(
    current_tab = input$sidebar_menu,
    target_tab = "positionKFE", 
    ui_component = position_kfe_full_comp$ui$display,
    loading_icon = "key"
  )
  
  output$position_ideal_rate_display <- renderUI2(
    current_tab = input$sidebar_menu,
    target_tab = "positionIdealRate", 
    ui_component = position_ideal_rate_comp$ui$display,
    loading_icon = "star"
  )
  
  output$position_strategy_display <- renderUI2(
    current_tab = input$sidebar_menu,
    target_tab = "positionStrategy", 
    ui_component = position_strategy_comp$ui$display,
    loading_icon = "compass"
  )
  
  output$micro_macro_kpi_display <- renderUI2(
    current_tab = input$sidebar_menu,
    target_tab = "microMacroKPI", 
    ui_component = micro_macro_kpi_comp$ui$display,
    loading_icon = "tachometer-alt"
  )
  
  # Poisson component UI outputs
  output$poisson_time_display <- renderUI2(
    current_tab = input$sidebar_menu,
    target_tab = "poissonTime", 
    ui_component = poisson_time_comp$ui$display,
    loading_icon = "clock"
  )
  
  output$poisson_feature_display <- renderUI2(
    current_tab = input$sidebar_menu,
    target_tab = "poissonFeature", 
    ui_component = poisson_feature_comp$ui$display,
    loading_icon = "bullseye"
  )
  
  output$poisson_comment_display <- renderUI2(
    current_tab = input$sidebar_menu,
    target_tab = "poissonComment", 
    ui_component = poisson_comment_comp$ui$display,
    loading_icon = "trophy"
  )
  
  # ---- 6.5 啟動 Component server ---------------------------------------
  # Initialize all component servers immediately (for proper reactive behavior)
  cust_res     <- customer_comp$server(input, output, session)
  dna_res      <- dna_comp$server(input, output, session)
  position_res <- position_comp$server(input, output, session)
  position_dna_res <- position_dna_comp$server(input, output, session)
  position_ms_res <- position_ms_comp$server(input, output, session)
  
  # For conditional execution, we need to call the server function directly with active_tab
  # This overwrites the above call with conditional logic
  position_ms_res <- positionMSPlotlyServer("position_ms", app_connection, comp_config, session, active_tab = reactive(input$sidebar_menu))
  position_kfe_res <- position_kfe_comp$server(input, output, session)
  position_kfe_full_res <- position_kfe_full_comp$server(input, output, session)
  position_ideal_rate_res <- position_ideal_rate_comp$server(input, output, session)
  position_strategy_res <- position_strategy_comp$server(input, output, session)
  micro_macro_kpi_res <- micro_macro_kpi_comp$server(input, output, session)
  
  # Initialize Poisson component servers
  poisson_time_res <- poisson_time_comp$server(input, output, session)
  poisson_feature_res <- poisson_feature_comp$server(input, output, session)
  poisson_comment_res <- poisson_comment_comp$server(input, output, session)
  
  # ---- 6.55  切換分頁時的跨元件協調 ---------------------------
  observeEvent(input$sidebar_menu, {
    # 切到 DNA 或 Position 分頁時重置 microCustomer
    if (input$sidebar_menu %in% c("dna", "position", "positionDNA", "positionMS", "positionKFE", "positionIdealRate", "positionStrategy", "microMacroKPI", "poissonTime", "poissonFeature", "poissonComment")) {
      # 透過自訂訊息「點擊」 cust 模組裡的 clear_filter 按鈕
      session$sendCustomMessage(
        "shiny.button.click",
        list(id = "cust-clear_filter")  # cust = 模組 ID
      )
    }
    
    # 當切換到 Position 相關分頁時，顯示通知訊息
    if (input$sidebar_menu == "position") {
      showNotification("Position data shows competitive keyword ranking performance", 
                       type = "message", duration = 5)
    }
    
    # 當切換到 Position DNA 分頁時，顯示通知訊息
    if (input$sidebar_menu == "positionDNA") {
      showNotification("Interactive DNA visualization shows multi-dimensional brand positioning", 
                       type = "message", duration = 5)
    }
    
    # 當切換到 Position MS (Market Segmentation) 分頁時，顯示通知訊息
    if (input$sidebar_menu == "positionMS") {
      showNotification("Market Segmentation Analysis provides MDS-based positioning insights", 
                       type = "message", duration = 5)
    }
    
    # 當切換到 Position KFE 分頁時，顯示通知訊息
    if (input$sidebar_menu == "positionKFE") {
      showNotification("Key Factor Evaluation identifies critical success factors", 
                       type = "message", duration = 5)
    }
    
    # 當切換到 Position Ideal Rate 分頁時，顯示通知訊息
    if (input$sidebar_menu == "positionIdealRate") {
      showNotification("Ideal Rate Analysis provides product ranking based on key factor performance", 
                       type = "message", duration = 5)
    }
    
    # 當切換到 Position Strategy 分頁時，顯示通知訊息
    if (input$sidebar_menu == "positionStrategy") {
      showNotification("Strategy Analysis provides four-quadrant strategic positioning insights", 
                       type = "message", duration = 5)
    }
    
    # 當切換到 Micro/Macro KPI 分頁時，顯示通知訊息
    if (input$sidebar_menu == "microMacroKPI") {
      showNotification("KPI dashboard displays summary statistics and performance metrics", 
                       type = "message", duration = 5)
    }
    
    # 當切換到 Poisson 時間分析分頁時，顯示通知訊息
    if (input$sidebar_menu == "poissonTime") {
      showNotification("時間區段分析：分析時間因素對銷售的影響", 
                       type = "message", duration = 5)
    }
    
    # 當切換到 Poisson 精準模型分頁時，顯示通知訊息
    if (input$sidebar_menu == "poissonFeature") {
      showNotification("精準模型：全面分析商品屬性對銷售的影響", 
                       type = "message", duration = 5)
    }
    
    # 當切換到 Poisson 產品賽道分析分頁時，顯示通知訊息
    if (input$sidebar_menu == "poissonComment") {
      showNotification("產品賽道分析：基於評分和評論的競爭力分析", 
                       type = "message", duration = 5)
    }
  }, ignoreInit = TRUE)
  
  # ---- 6.6 通用通知 / 狀態 ---------------------------------------------
  observeEvent(input$platform, {
    # Map platform ID to platform name
    platform_map <- c(
      "0" = "All Platforms",
      "2" = "Amazon"
    )
    
    # Get the platform name using the platform ID as key
    platform_id <- as.character(input$platform)
    platform_name <- platform_map[platform_id]
    
    # Show notification with the platform name
    showNotification(paste("Switched to", platform_name), type="message", duration=3)
    
    # Log platform switch using tbl2-compatible approach
    tryCatch({
      if (!is.null(app_connection)) {
        # Example of using tbl2 for logging (if log table exists)
        if (any(grepl("system_log", DBI::dbListTables(app_connection)))) {
          log_data <- data.frame(
            timestamp = Sys.time(),
            event = "platform_switch",
            details = platform_name,
            user_id = session$user
          )
          tbl2(log_data) # Would typically be collected and saved to db
        }
      }
    }, error = function(e) {
      # Silently handle any logging errors
    })
  }, ignoreInit = TRUE)
  
  session$onSessionEnded(function(){ if(!is.null(app_connection)&&inherits(app_connection,"DBIConnection")) try(DBI::dbDisconnect(app_connection), silent=TRUE) })
}

# ---- 7. Run --------------------------------------------------------------
shinyApp(ui, server)
