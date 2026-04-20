###############################################################################
# TagPilot 顧客標籤行銷引擎 - 主應用程式                                          #
# 版本: v18 (bs4Dash)                                                         #
# 更新: 2024-06-23                                                            #
###############################################################################

# RSV 框架對應 (v3.0 - 2025-12-03 更新)：
# =====================================================================
# TagPilot Premium 現在使用與 MAMBA (L4) 相同的 RSV 真實變數！
#
# analysis_dna() 輸出的 RSV 變數（優先使用）：
#   R (Risk/靜止風險): nrec_prob（邏輯回歸預測流失機率 0-1）
#     - nrec_prob > 0.7 → 高靜止戶（即將或已經流失）
#     - nrec_prob 0.3-0.7 → 中靜止戶（互動減少但仍有潛力）
#     - nrec_prob < 0.3 → 低靜止戶（穩定活躍群）
#
#   S (Stability/交易穩定度): cri, cri_ecdf（經驗貝氏 Customer Regularity Index）
#     - cri 接近 0 → 高穩定顧客（固定頻率與金額）
#     - cri 中等 → 中穩定顧客（有規律但偶爾波動）
#     - cri 接近 1 → 低穩定顧客（購買間隔不固定）
#
#   V (Value/顧客終身價值): clv（PIF 函數預測未來 10 年價值）
#     - 使用 P20/P80 分位數分群
#
# Fallback 變數（當 RSV 真實變數不存在時）：
#   R: customer_dynamics → r_value 分位數
#   S: ni（交易次數）
#   V: total_spent → m_value * ni
#
# 參考文件：
#   - documents/03_requirements/2025-12-03_rsv_framework_feasibility_analysis.md
#   - scripts/global_scripts/04_utils/fn_analysis_dna.R
# =====================================================================

# ── 系統初始化 ──────────────────────────────────────────────────────────────
source("config/packages.R")    # 載入套件管理
source("config/config.R")      # 載入配置設定

# 初始化套件環境
initialize_packages()

# 驗證配置
validate_config()

# ── 全域 CSS 樣式 ────────────────────────────────────────────────────────
css_deps <- tags$head(tags$style(HTML("
  /* 確保 DataTables 的滾動條始終顯示 */
  .dataTables_scrollBody { overflow-x: scroll !important; }
  /* macOS 滾動條始終顯示的全域設定 */
  .dataTables_scrollBody::-webkit-scrollbar { -webkit-appearance: none; }
  .dataTables_scrollBody::-webkit-scrollbar:horizontal { height: 10px; }
  .dataTables_scrollBody::-webkit-scrollbar-thumb { border-radius: 5px; background-color: rgba(0,0,0,.3); }
  .dataTables_scrollBody::-webkit-scrollbar-track { background-color: rgba(0,0,0,.1); border-radius: 5px; }

  /* 登入頁面樣式 */
  .login-container {
    max-width: 400px;
    margin: 2rem auto;
    padding: 2rem;
    background: white;
    border-radius: 10px;
    box-shadow: 0 4px 6px rgba(0,0,0,0.1);
  }
  .login-icon { text-align: center; margin-bottom: 2rem; }

  /* 歡迎訊息樣式 */
  .welcome-banner {
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    color: white;
    padding: 1.5rem;
    border-radius: 10px;
    margin-bottom: 2rem;
    text-align: center;
  }
")))

# ── 載入模組 ────────────────────────────────────────────────────────────────
source("database/db_connection.R")    # 資料庫連接模組
source("utils/tagpilot_data_access.R")         # 資料存取工具（整合 tbl2）
source("modules/module_tagpilot_wo_b.R")       # 主要分析模組

# ==========================================
# 載入 Supabase 登入模組
# 模組位於 global_scripts，可透過 git subrepo 在各專案共用
# ==========================================
# 認證工具在 11_rshinyapp_utils（工具類）
supabase_auth_path <- "scripts/global_scripts/11_rshinyapp_utils/supabase_auth"
# 登入元件在 10_rshinyapp_components（元件類）
login_component_path <- "scripts/global_scripts/10_rshinyapp_components/login"

if (file.exists(file.path(supabase_auth_path, "module_supabase_auth.R")) &&
    file.exists(file.path(login_component_path, "module_login_supabase.R"))) {
  source(file.path(supabase_auth_path, "module_supabase_auth.R"))
  source(file.path(login_component_path, "module_login_supabase.R"))
  message("✅ 已載入 Supabase 登入模組 (from global_scripts)")
} else {
  stop("❌ 找不到 Supabase 登入模組")
}

source("modules/module_tagpilot_upload.R")     # 上傳模組
source("modules/module_tagpilot_dna.R")  # DNA 分析模組 Premium V2 with Z-Score Customer Dynamics

# ── 載入六列分析模組 ─────────────────────────────────────────────────────
source("modules/module_tagpilot_customer_base_value.R")      # 客戶基數價值
source("modules/module_tagpilot_customer_value.R")  # RFM 價值分析
source("modules/module_tagpilot_customer_activity.R")        # 顧客活躍度分析 (CAI)
source("modules/module_tagpilot_customer_status.R")          # 客戶狀態分析
source("modules/module_tagpilot_rsv_matrix.R")               # R/S/V 生命力矩陣
source("modules/module_tagpilot_marketing.R")       # 客戶行銷決策表（2025-12-26 新增）
source("modules/module_tagpilot_customer_export.R")          # 客戶標籤輸出表（2025-12-26 新增）
source("modules/module_tagpilot_lifecycle.R")     # 生命週期預測
# source("modules/module_tagpilot_advanced_analytics.R")       # 進階分析（需歷史資料）- Issue #20: 暫時隱藏

# ── reactive values -----------------------------------------------------------
facets_rv   <- reactiveVal(NULL)  # 目前 LLM 產出的 10 個屬性 (字串向量)
progress_info <- reactiveVal(list(start=NULL, done=0, total=0))
safe_value <- function(txt) {
  txt <- trimws(txt)
  num <- str_extract(txt, "[1-5]")
  if (!is.na(num)) return(as.numeric(num))
  val <- suppressWarnings(as.numeric(txt))
  if (!is.na(val) && val >= 1 && val <= 5) return(val)
  NA_real_
}

# 全域 tab 切換 trigger
regression_trigger <- reactiveVal(0)

# ── 登入頁面 UI ──────────────────────────────────────────────────────────
# 使用新的參數化登入模組

# ── 主要應用 UI (bs4Dash) ───────────────────────────────────────────────
main_app_ui <- bs4DashPage(
  title = "TagPilot 顧客標籤行銷引擎",
  fullscreen = TRUE,

  # 頁首
  header = bs4DashNavbar(
    title = bs4DashBrand(
      title = "TagPilot",
      color = "primary",
      image = "assets/icons/app_icon.png"
    ),
    skin = "light",
    status = "primary",
    fixed = TRUE,
    rightUi = tagList(
      uiOutput("db_status"),
      uiOutput("user_menu")
    )
  ),

  # 側邊欄
  sidebar = bs4DashSidebar(
    status = "primary",
    width = "280px",
    elevation = 3,
    minified = FALSE,

    # 歡迎訊息
    div(class = "welcome-banner",
        h5("🎉 歡迎！", style = "margin: 0;"),
        textOutput("welcome_user", inline = TRUE)
    ),

    # 選單
    sidebarMenu(
      id = "sidebar_menu",
      bs4SidebarHeader("分析流程"),
      # (1) 資料上傳
      bs4SidebarMenuItem(
        text = "資料上傳",
        tabName = "upload",
        icon = icon("upload")
      ),
      # (2) 顧客價值 - 最近購買日、購買頻率、購買金額、RFM 分析
      bs4SidebarMenuItem(
        text = "顧客價值",
        tabName = "rfm_analysis",
        icon = icon("chart-pie")
      ),
      # (3) 顧客活躍度 - CAI 分析 (待開發)
      bs4SidebarMenuItem(
        text = "顧客活躍度",
        tabName = "cai_analysis",
        icon = icon("chart-line")
      ),
      # (4) 顧客狀態 - 顧客狀態、顧客流失風險、顧客入店資歷（原：顧客動態）
      bs4SidebarMenuItem(
        text = "顧客狀態",
        tabName = "customer_status",
        icon = icon("heartbeat")
      ),
      # (5) 顧客市場區隔分析 (Issue #4: 縮短標題)
      # 2025-12-26: Comment out - 訂閱版暫不顯示
      # bs4SidebarMenuItem(
      #   text = "顧客市場區隔分析",
      #   tabName = "dna_analysis",
      #   icon = icon("th")
      # ),
      # (6) 顧客結構 - 顧客購買週期、過去價值、客單價（原：顧客基礎價值）
      bs4SidebarMenuItem(
        text = "顧客結構",
        tabName = "base_value",
        icon = icon("coins")
      ),
      # (7) 顧客回購預測 (Issue #8: 改名)
      bs4SidebarMenuItem(
        text = "顧客回購預測",
        tabName = "lifecycle_pred",
        icon = icon("clock")
      ),
      # 保留：R/S/V 生命力矩陣
      bs4SidebarMenuItem(
        text = "R/S/V 生命力矩陣",
        tabName = "rsv_matrix",
        icon = icon("cube")
      ),
      # 客戶行銷決策表（2025-12-26 新增）
      bs4SidebarMenuItem(
        text = "客戶行銷決策表",
        tabName = "marketing_decision",
        icon = icon("bullseye")
      ),
      # 客戶標籤輸出表（2025-12-26 新增）
      bs4SidebarMenuItem(
        text = "客戶標籤輸出",
        tabName = "customer_export",
        icon = icon("file-export")
      ),
      # 進階分析（暫時隱藏 - Issue #20）
      # bs4SidebarMenuItem(
      #   text = "進階分析（需歷史資料）",
      #   tabName = "advanced_analytics",
      #   icon = icon("chart-line")
      # ),
      bs4SidebarHeader("平台資訊"),
      bs4SidebarMenuItem(
        text = "關於我們",
        tabName = "about",
        icon = icon("info-circle")
      ),

      # 登出按鈕（放在選單最底部）
      div(style = "margin-top: 20px; padding: 0 15px;",
        actionButton("logout", "登出", class = "btn-secondary btn-block", icon = icon("sign-out-alt"),
                     style = "width: 100%;")
      )
    )
  ),

  # 主要內容
  body = bs4DashBody(
    css_deps,
    useShinyjs(),

    bs4TabItems(
      # 上傳資料頁面
      bs4TabItem(
        tabName = "upload",
        fluidRow(
          bs4Card(
            title = "步驟 1：上傳資料 (支援多檔案)",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            elevation = 3,
            uploadModuleUI("upload1")
          )
        )
      ),



      # 顧客市場區隔分析（Issue #4: 縮短標題）
      bs4TabItem(
        tabName = "dna_analysis",
        fluidRow(
          bs4Card(
            title = "顧客市場區隔分析",
            status = "success",
            width = 12,
            solidHeader = TRUE,
            elevation = 3,
            dnaMultiPremiumModuleUI("dna_multi1")
          )
        )
      ),

      # 客戶基數價值
      bs4TabItem(
        tabName = "base_value",
        fluidRow(
          bs4Card(
            title = NULL,
            status = "info",
            width = 12,
            solidHeader = FALSE,
            elevation = 3,
            customerBaseValueUI("base_value_module")
          )
        )
      ),

      # RFM 價值分析
      bs4TabItem(
        tabName = "rfm_analysis",
        fluidRow(
          bs4Card(
            title = NULL,
            status = "primary",
            width = 12,
            solidHeader = FALSE,
            elevation = 3,
            customerValueAnalysisUI("rfm_module")
          )
        )
      ),

      # 顧客活躍度 (CAI)
      bs4TabItem(
        tabName = "cai_analysis",
        customerActivityUI("customer_activity")
      ),

      # 客戶狀態
      bs4TabItem(
        tabName = "customer_status",
        fluidRow(
          bs4Card(
            title = NULL,
            status = "warning",
            width = 12,
            solidHeader = FALSE,
            elevation = 3,
            customerStatusUI("status_module")
          )
        )
      ),

      # R/S/V 生命力矩陣
      bs4TabItem(
        tabName = "rsv_matrix",
        fluidRow(
          bs4Card(
            title = NULL,
            status = "primary",
            width = 12,
            solidHeader = FALSE,
            elevation = 3,
            rsvMatrixUI("rsv_module")
          )
        )
      ),

      # 客戶行銷決策表（2025-12-26 新增）
      bs4TabItem(
        tabName = "marketing_decision",
        fluidRow(
          bs4Card(
            title = NULL,
            status = "success",
            width = 12,
            solidHeader = FALSE,
            elevation = 3,
            marketingDecisionUI("marketing_module")
          )
        )
      ),

      # 客戶標籤輸出表（2025-12-26 新增）
      bs4TabItem(
        tabName = "customer_export",
        fluidRow(
          bs4Card(
            title = NULL,
            status = "info",
            width = 12,
            solidHeader = FALSE,
            elevation = 3,
            customerExportUI("export_module")
          )
        )
      ),

      # 顧客回購預測 (Issue #8)
      bs4TabItem(
        tabName = "lifecycle_pred",
        fluidRow(
          bs4Card(
            title = NULL,
            status = "success",
            width = 12,
            solidHeader = FALSE,
            elevation = 3,
            lifecyclePredictionUI("prediction_module")
          )
        )
      ),

      # Phase 5：進階分析 - Issue #20: 暫時隱藏
      # bs4TabItem(
      #   tabName = "advanced_analytics",
      #   fluidRow(
      #     bs4Card(
      #       title = NULL,
      #       status = "info",
      #       width = 12,
      #       solidHeader = FALSE,
      #       elevation = 3,
      #       advancedAnalyticsUI("advanced_module")
      #     )
      #   )
      # ),

      # 關於我們頁面
      bs4TabItem(
        tabName = "about",
        fluidRow(
          bs4Card(
            title = NULL,
            status = "info",
            width = 12,
            solidHeader = FALSE,
            elevation = 3,
            div(
              style = "text-align: center; margin-bottom: 2rem;",
              img(src = "assets/icons/app_icon.png", width = "320px", alt = "TagPilot Logo")
            ),
            h1("顧客標籤行銷引擎", style = "text-align: center; color: #007bff; margin-bottom: 2rem;"),

            h2("🎯 服務描述", style = "color: #343a40; border-bottom: 2px solid #007bff; padding-bottom: 0.5rem;"),
            p(
              "我們是一套由 AI 驅動的精準行銷平台，協助品牌根據客戶特徵與行為數據，制定個人化行銷策略。我們整合 NLP、推薦系統與自動化分析以及統計和行銷理論，提供高效且可擴展的解決方案，協助行銷團隊更快達成轉換與黏著目標。",
              style = "font-size: 1.1rem; line-height: 1.6; margin-bottom: 1.5rem;"),
            p("本平台除了能提供企業上針對過去資料的洞見外，也能進一步協助新產品開發。",
              style = "font-size: 1.1rem; line-height: 1.6; margin-bottom: 2rem;"),

            h2("🛠️ 提供服務", style = "color: #343a40; border-bottom: 2px solid #007bff; padding-bottom: 0.5rem;"),
            div(
              style = "background: #f8f9fa; padding: 1.5rem; border-radius: 8px; margin-bottom: 2rem;",
              tags$ul(
                style = "list-style: none; padding: 0; margin: 0;",
                tags$li(
                  icon("circle"),
                  " 客群分群建模（Segmentation Modeling）",
                  style = "margin-bottom: 0.8rem; font-size: 1.1rem;"
                ),
                tags$li(
                  icon("circle"),
                  " 意圖辨識與推薦系統（Intent Detection & Recommendation）",
                  style = "margin-bottom: 0.8rem; font-size: 1.1rem;"
                ),
                tags$li(
                  icon("circle"),
                  " 評論內容語意分析（Sentiment & Aspect Analysis）",
                  style = "margin-bottom: 0.8rem; font-size: 1.1rem;"
                ),
                tags$li(
                  icon("circle"),
                  " 行銷活動預測與績效追蹤（Campaign Forecasting & Tracking）",
                  style = "margin-bottom: 0.8rem; font-size: 1.1rem;"
                ),
                tags$li(
                  icon("circle"),
                  " 多管道數據整合與儀表板（Omni-channel Dashboard & ETL）",
                  style = "font-size: 1.1rem;"
                )
              )
            ),

            h2(
              "📞 聯絡方式",
              style = "color: #343a40; border-bottom: 2px solid #007bff; padding-bottom: 0.5rem;"
            ),
            div(
              style = paste(
                "background: #fff3cd; padding: 1.5rem; border-radius: 8px;",
                "border-left: 4px solid #ffc107;"
              ),
              p(
                strong("公司: "),
                "祈鋒行銷科技有限公司",
                style = "margin-bottom: 1rem; font-size: 1.1rem;"
              ),
              p(
                strong("聯絡資訊: "),
                tags$a(
                  href = "mailto:mr.no.one01@gmail.com",
                  "mr.no.one01@gmail.com",
                  style = "color: #007bff; text-decoration: none;"
                ),
                style = "margin-bottom: 1rem; font-size: 1.1rem;"
              ),
              p(
                "如需商業合作或平台體驗，請聯絡資料分析團隊。",
                style = "font-style: italic; color: #6c757d; margin-bottom: 0;"
              )
            )
          )
        )
      )
    )
  ),

  # 頁尾
  footer = bs4DashFooter(
    fixed = TRUE,
    left = "TagPilot v18 - 顧客標籤行銷引擎",
    right = "© 2024 All Rights Reserved"
  )
)

# ── 資源路徑設定 (在 UI 定義前) ──────────────────────────────────────────
# 設定 global_scripts 資源路徑
addResourcePath("assets", "scripts/global_scripts/24_assets")
addResourcePath("global_assets", "scripts/global_scripts")  # 供 login.css 等 CSS 使用

# ── 條件式 UI ─────────────────────────────────────────────────────────
ui <- fluidPage(
  useShinyjs(),

  # 資源路徑設定
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "https://use.fontawesome.com/releases/v5.15.4/css/all.css"),
    # Modern Login Page CSS (from global_scripts/19_CSS/login.css)
    tags$link(rel = "stylesheet", type = "text/css", href = "global_assets/19_CSS/login.css"),
    tags$style("#icon_bar { display:flex; gap:12px; align-items:center; margin-bottom:16px; } #icon_bar img { max-height:60px; }")
  ),

  # 根據登入狀態顯示不同UI
  conditionalPanel(
    condition = "output.user_logged_in == false",
    # 登入頁面 - 使用 login.css 的 login-page-wrapper 樣式
    loginSupabaseUI("login1",
                    title = "TagPilot",
                    subtitle = "顧客標籤行銷引擎",
                    app_icon = "global_assets/24_assets/icons/app_icon.png",
                    show_language_selector = FALSE)
  ),

  conditionalPanel(
    condition = "output.user_logged_in == true",
    main_app_ui
  )
)

# ── Server ------------------------------------------------------------------
server <- function(input, output, session) {
  # 設定檔案上傳大小限制為 200MB
  options(shiny.maxRequestSize = 200*1024^2)

  # 資料庫連接（連接但不初始化表格）
  con_global <- get_con()
  # 正確關閉連接：Pool 用 poolClose()，一般連接用 dbDisconnect()
  onStop(function() {
    if (inherits(con_global, "Pool")) {
      pool::poolClose(con_global)
    } else {
      DBI::dbDisconnect(con_global)
    }
  })

  # 反應式變數
  user_info    <- reactiveVal(NULL)   # 登入後的 user row
  sales_data   <- reactiveVal(NULL)   # 銷售資料
  db_initialized <- reactiveVal(FALSE)  # 資料表初始化狀態

  # 設定資源路徑
  images_path <- if (dir.exists("www/images")) "www/images" else "www"
  addResourcePath("images", images_path)

  # 登入模組 - 使用 Supabase（已移除舊版 bcrypt 登入）
  message("🔐 使用 Supabase 登入模組")
  login_mod <- loginSupabaseServer("login1", app_name = "tagpilot")

  observe({
    user_info(login_mod$user_info())

    # 登入成功後初始化資料表
    # 注意：init_tables() 不接受參數，它使用全域的 db_pool
    if (!is.null(user_info()) && !db_initialized()) {
      tryCatch({
        init_tables()  # 移除 con_global 參數
        db_initialized(TRUE)
        showNotification("✅ 資料庫初始化完成", type = "message")
      }, error = function(e) {
        showNotification(paste("資料庫初始化失敗:", e$message), type = "error")
      })
    }
  })

  # 上傳模組
  upload_mod <- uploadModuleServer("upload1", con_global, user_info)
  observe({
    sales_data(upload_mod$dna_data())  # 將DNA資料傳遞給sales_data以供DNA模組使用
  })

  # 按「下一步」自動切換到顧客價值頁面
  observeEvent(upload_mod$proceed_step(), {
    if (!is.null(upload_mod$proceed_step()) && upload_mod$proceed_step() > 0 && !is.null(upload_mod$dna_data()) && nrow(upload_mod$dna_data()) > 0) {
      updateTabItems(session, "sidebar_menu", "rfm_analysis")
    }
  }, ignoreInit = TRUE)

  # DNA 分析模組 Premium with IPT T-Series Insight
  dna_mod <- dnaMultiPremiumModuleServer("dna_multi1", con_global, user_info, upload_mod$dna_data)

  # ─────────────────────────────────────────────────────────────────────────
  # 六列詳細分析模組（依序串接）
  # ─────────────────────────────────────────────────────────────────────────
  # 客戶基數價值（接收DNA模組的輸出）
  base_value_data <- customerBaseValueServer("base_value_module", dna_mod)

  # RFM 價值分析（接收客戶基數價值輸出）
  rfm_data <- customerValueAnalysisServer("rfm_module", base_value_data)

  # 顧客活躍度分析 (CAI)
  customerActivityServer("customer_activity", rfm_data)

  # 客戶狀態（接收RFM分析輸出）
  status_data <- customerStatusServer("status_module", rfm_data)

  # R/S/V 生命力矩陣（接收客戶狀態輸出）
  rsv_data <- rsvMatrixServer("rsv_module", status_data)

  # 客戶行銷決策表（2025-12-26 新增 - 接收R/S/V矩陣輸出）
  marketing_data <- marketingDecisionServer("marketing_module", rsv_data)

  # 客戶標籤輸出表（2025-12-26 新增 - 接收行銷決策輸出）
  export_data <- customerExportServer("export_module", marketing_data)

  # 顧客回購預測（Issue #8 - 接收R/S/V矩陣輸出）
  prediction_data <- lifecyclePredictionServer("prediction_module", rsv_data)

  # 進階分析（接收生命週期預測輸出）- Issue #20: 暫時隱藏
  # advanced_data <- advancedAnalyticsServer("advanced_module", prediction_data)

  # 登入狀態輸出
  output$user_logged_in <- reactive({
    !is.null(user_info())
  })
  outputOptions(output, "user_logged_in", suspendWhenHidden = FALSE)

  # 歡迎訊息
  output$welcome_user <- renderText({
    if (!is.null(user_info())) {
      sprintf("%s (%s)", user_info()$username, user_info()$role)
    } else {
      ""
    }
  })

  # 用戶選單
  output$user_menu <- renderUI({
    req(user_info())
    div(
      style = "display: flex; align-items: center;",
      span(
        icon("user"), user_info()$username,
        style = "margin-right: 15px;"
      )
    )
  })

  # 資料庫狀態顯示
  output$db_status <- renderUI({
    if (dbIsValid(con_global)) {
      span(
        icon("database"), "資料庫已連接",
        style = "color: #28a745; margin-right: 15px;"
      )
    } else {
      span(
        icon("database"), "資料庫未連接",
        style = "color: #dc3545; margin-right: 15px;"
      )
    }
  })

  # ✅ Task 7.5: 文檔按鈕點擊處理（使用 Modal 彈窗）

  # 九宮格分析邏輯文檔
  observeEvent(input$show_logic_doc, {
    logic_content <- tryCatch({
      readLines("documents/logic.md", encoding = "UTF-8") %>% paste(collapse = "\n")
    }, error = function(e) {
      "📄 文檔載入失敗。請確認 documents/logic.md 檔案存在。"
    })

    showModal(modalDialog(
      title = "📖 九宮格分析邏輯（45種策略組合）",
      HTML(markdown::markdownToHTML(text = logic_content, fragment.only = TRUE)),
      easyClose = TRUE,
      size = "xl",
      footer = modalButton("關閉")
    ))
  })

  # 技術警告文檔
  observeEvent(input$show_warnings_doc, {
    warnings_content <- tryCatch({
      readLines("documents/warnings.md", encoding = "UTF-8") %>% paste(collapse = "\n")
    }, error = function(e) {
      "⚠️ 文檔載入失敗。請確認 documents/warnings.md 檔案存在。"
    })

    showModal(modalDialog(
      title = "⚠️ 技術警告與限制說明",
      HTML(markdown::markdownToHTML(text = warnings_content, fragment.only = TRUE)),
      easyClose = TRUE,
      size = "xl",
      footer = modalButton("關閉")
    ))
  })

  # 系統架構文檔
  observeEvent(input$show_architecture_doc, {
    arch_content <- tryCatch({
      readLines("documents/TagPilot_Premium_App_Architecture_Documentation.md", encoding = "UTF-8") %>% paste(collapse = "\n")
    }, error = function(e) {
      "🏗️ 文檔載入失敗。請確認 documents/TagPilot_Premium_App_Architecture_Documentation.md 檔案存在。"
    })

    showModal(modalDialog(
      title = "🏗️ TagPilot Premium 系統架構文檔",
      HTML(markdown::markdownToHTML(text = arch_content, fragment.only = TRUE)),
      easyClose = TRUE,
      size = "xl",
      footer = modalButton("關閉")
    ))
  })

  # 功能開發計劃
  observeEvent(input$show_work_plan, {
    plan_content <- tryCatch({
      readLines("documents/Work_Plan_TagPilot_Premium_Enhancement.md", encoding = "UTF-8") %>% paste(collapse = "\n")
    }, error = function(e) {
      "📋 文檔載入失敗。請確認 documents/Work_Plan_TagPilot_Premium_Enhancement.md 檔案存在。"
    })

    showModal(modalDialog(
      title = "📋 TagPilot Premium 功能開發計劃",
      HTML(markdown::markdownToHTML(text = plan_content, fragment.only = TRUE)),
      easyClose = TRUE,
      size = "xl",
      footer = modalButton("關閉")
    ))
  })

  # 登出按鈕
  observeEvent(input$logout, {
    user_info(NULL); sales_data(NULL)
    # 也可呼叫 login_mod$logout() 來重置 module 狀態
    login_mod$logout()
  })


}

# ---- Run --------------------------------------------------------------------
shinyApp(ui, server)
