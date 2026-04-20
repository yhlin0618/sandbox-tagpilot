# Multi-File DNA Analysis Module - Version 2.0
# Implements Z-Score Based Customer Dynamics (顧客動態)
# Based on: logic_revised.md + 顧客動態計算方式調整_20251025.md
# Updated: 2025-11-01
#
# MAJOR CHANGES FROM V1:
# 1. Z-score based lifecycle classification (replaces fixed 7/14/21 thresholds)
# 2. Removed activity degradation strategy for ni < 4 (now strictly NA)
# 3. New newbie definition: ni == 1 AND customer_age_days <= μ_ind
# 4. Terminology: lifecycle_stage → customer_dynamics

library(shiny)
library(shinyjs)
library(dplyr)
library(DT)
library(plotly)
library(readxl)
library(later)

# Helper functions
`%+%` <- function(x, y) paste0(x, y)
`%||%` <- function(x, y) if (is.null(x)) y else x
nrow2 <- function(x) {
  if (is.null(x)) return(0)
  if (!is.data.frame(x) && !is.matrix(x)) return(0)
  return(nrow(x))
}

# Source DNA analysis function and helper functions
if (file.exists("scripts/global_scripts/04_utils/fn_analysis_dna.R")) {
  source("scripts/global_scripts/04_utils/fn_left_join_remove_duplicate2.R")
  source("scripts/global_scripts/04_utils/fn_fct_na_value_to_level.R")
  source("scripts/global_scripts/04_utils/fn_analysis_dna.R")
} else {
  stop("❌ Cannot find fn_analysis_dna.R - required for DNA analysis")
}

# ✅ Source new z-score customer dynamics implementation
if (file.exists("utils/analyze_customer_dynamics_new.R")) {
  source("utils/analyze_customer_dynamics_new.R")
} else {
  warning("⚠️  analyze_customer_dynamics_new.R not found - z-score method unavailable")
}

# ✅ Source customer tags calculation functions
if (file.exists("utils/calculate_customer_tags.R")) {
  source("utils/calculate_customer_tags.R")
} else {
  warning("⚠️  calculate_customer_tags.R not found - tag calculations may fail")
}

# ✅ Source configuration if available
if (file.exists("config/customer_dynamics_config.R")) {
  source("config/customer_dynamics_config.R")
}

# ============================================================================
# UI Function (Minimal changes - mostly terminology updates)
# ============================================================================

dnaMultiPremiumModuleUI <- function(id) {
  ns <- NS(id)

  div(
    # Status display (hidden by default, shown during/after analysis)
    conditionalPanel(
      condition = paste0("output['", ns("show_status"), "'] == true"),
      wellPanel(
        h4("📊 處理狀態"),
        verbatimTextOutput(ns("status"))
      )
    ),

    # === 分析結果顯示區（九宮格分析）===
    conditionalPanel(
      condition = paste0("output['", ns("show_results"), "'] == true"),

      # Z-score metadata display
      fluidRow(
        column(12,
          wellPanel(
            style = "background-color: #e7f3ff; border-left: 4px solid #007bff;",
            h5(icon("chart-line"), " 分析方法資訊", style = "margin-top: 0;"),
            uiOutput(ns("zscore_metadata"))
          )
        )
      ),

      # Customer Dynamics selector
      fluidRow(
        column(12,
          wellPanel(
            uiOutput(ns("customer_dynamics_selector"))
          )
        )
      ),

      # Grid Matrix - Value × Activity (九宮格)
      h3("💎 顧客價值 × 活躍度九宮格", style = "text-align: center; margin: 30px 0;"),
      uiOutput(ns("grid_matrix")),

      # Customer Detail Table
      h3("📊 客戶明細資料", style = "text-align: center; margin: 50px 0 20px 0;"),
      fluidRow(
        column(12,
          bs4Card(
            title = "客戶列表",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            footer = downloadButton(ns("download_customer_data"), "下載客戶資料 CSV", class = "btn-primary"),
            DTOutput(ns("customer_table"))
          )
        )
      )
    )
  )
}

# ============================================================================
# SERVER Function - MAJOR REVISIONS for Z-Score Method
# ============================================================================

dnaMultiPremiumModuleServer <- function(id, con, user_info, dna_data_reactive) {
  moduleServer(id, function(input, output, session) {

    # Reactive values storage
    values <- reactiveValues(
      dna_results = NULL,
      transaction_data = NULL,
      zscore_metadata = NULL,
      data_validation = NULL
    )

    # Check if data uploaded
    output$has_uploaded_data <- reactive({
      !is.null(dna_data_reactive()) && nrow(dna_data_reactive()) > 0
    })
    outputOptions(output, "has_uploaded_data", suspendWhenHidden = FALSE)

    # ============================================================================
    # NEW: Data Validation Check
    # ============================================================================

    observe({
      req(dna_data_reactive())

      # Store transaction data and standardize column names
      # Upload module creates: customer_id, payment_time, lineitem_price
      # We need: customer_id, transaction_date, transaction_amount
      raw_data <- dna_data_reactive()

      transaction_data <- raw_data %>%
        rename(
          transaction_date = payment_time,
          transaction_amount = lineitem_price
        )

      values$transaction_data <- transaction_data

      # Validate data requirements
      validation <- tryCatch({

        transaction_data <- values$transaction_data

        # Calculate metrics
        cap_days <- as.numeric(
          max(transaction_data$transaction_date, na.rm = TRUE) -
          min(transaction_data$transaction_date, na.rm = TRUE)
        ) + 1

        n_customers <- n_distinct(transaction_data$customer_id)
        n_transactions <- nrow(transaction_data)

        customer_counts <- transaction_data %>%
          group_by(customer_id) %>%
          summarise(ni = n(), .groups = "drop")

        n_with_repeat <- sum(customer_counts$ni >= 2)

        # Check requirements
        checks <- list(
          observation_period = list(
            value = cap_days,
            threshold = 365,
            passed = cap_days >= 365,
            message = paste0("觀察期: ", round(cap_days), " 天 (建議 ≥ 365 天)")
          ),
          total_customers = list(
            value = n_customers,
            threshold = 100,
            passed = n_customers >= 100,
            message = paste0("總客戶數: ", n_customers, " (建議 ≥ 100)")
          ),
          repeat_customers = list(
            value = n_with_repeat,
            threshold = 30,
            passed = n_with_repeat >= 30,
            message = paste0("回購客戶: ", n_with_repeat, " (需要 ≥ 30 才能計算可靠的 μ_ind)")
          ),
          total_transactions = list(
            value = n_transactions,
            threshold = 500,
            passed = n_transactions >= 500,
            message = paste0("總交易數: ", n_transactions, " (建議 ≥ 500)")
          )
        )

        all_passed <- all(sapply(checks, function(x) x$passed))

        list(
          checks = checks,
          all_passed = all_passed
        )

      }, error = function(e) {
        list(
          checks = list(),
          all_passed = FALSE,
          error = e$message
        )
      })

      values$data_validation <- validation
    })

    # Output validation status
    output$data_validation_passed <- reactive({
      req(values$data_validation)
      values$data_validation$all_passed
    })
    outputOptions(output, "data_validation_passed", suspendWhenHidden = FALSE)

    # Output validation message
    output$data_validation_message <- renderUI({
      req(values$data_validation)

      validation <- values$data_validation

      if (!validation$all_passed) {
        checks <- validation$checks

        messages <- lapply(names(checks), function(check_name) {
          check <- checks[[check_name]]
          status <- if (check$passed) "✅" else "❌"
          tags$li(HTML(paste(status, check$message)))
        })

        tagList(
          tags$ul(messages),
          tags$p(
            style = "margin-top: 10px;",
            strong("注意："),
            "部分資料要求未達標，系統將使用固定閾值方法作為備用。"
          )
        )
      }
    })

    # ============================================================================
    # MAIN ANALYSIS LOGIC - Z-Score Method (AUTO-TRIGGERED)
    # ============================================================================

    # Output show_status flag
    output$show_status <- reactive({
      !is.null(dna_data_reactive()) && nrow(dna_data_reactive()) > 0
    })
    outputOptions(output, "show_status", suspendWhenHidden = FALSE)

    # Auto-trigger analysis when data is uploaded
    observe({

      req(dna_data_reactive())

      # Only run once per data upload
      if (!is.null(values$dna_results)) return()

      output$status <- renderText({
        "⏳ 自動分析中，請稍候..."
      })

      tryCatch({

        message("\n========================================")
        message("開始分析顧客動態（使用 analysis_dna）")
        message("========================================\n")

        tryCatch({
          transaction_data <- values$transaction_data
          message("[DEBUG] ✓ Step 1: Got transaction_data, rows: ", nrow(transaction_data))
        }, error = function(e) {
          message("[ERROR] Step 1 FAILED: Getting transaction_data - ", e$message)
          stop("無法取得交易資料: ", e$message)
        })

        # Prepare df_sales_by_customer_by_date
        tryCatch({
          message("[DEBUG] Step 2: Preparing sales_by_customer_by_date...")
          sales_by_customer_by_date <- transaction_data %>%
            mutate(date = as.Date(transaction_date)) %>%
            group_by(customer_id, date) %>%
            summarise(
              payment_time = min(transaction_date),
              sum_spent_by_date = sum(transaction_amount, na.rm = TRUE),
              count_transactions_by_date = n(),
              .groups = "drop"
            )
          message("[DEBUG] ✓ Step 2: sales_by_customer_by_date prepared, rows: ", nrow(sales_by_customer_by_date))
        }, error = function(e) {
          message("[ERROR] Step 2 FAILED: Preparing sales_by_customer_by_date - ", e$message)
          stop("準備每日銷售資料失敗: ", e$message)
        })

        # Prepare df_sales_by_customer
        tryCatch({
          message("[DEBUG] Step 3: Preparing sales_by_customer...")
          sales_by_customer <- transaction_data %>%
            group_by(customer_id) %>%
            summarise(
              total_spent = sum(transaction_amount, na.rm = TRUE),
              times = n(),
              ni = n(),
              first_purchase = min(transaction_date),
              last_purchase = max(transaction_date),
              .groups = "drop"
            ) %>%
            mutate(
              ipt = pmax(as.numeric(difftime(last_purchase, first_purchase, units = "days")), 1)
            )
          message("[DEBUG] ✓ Step 3: sales_by_customer prepared, rows: ", nrow(sales_by_customer))
        }, error = function(e) {
          message("[ERROR] Step 3 FAILED: Preparing sales_by_customer - ", e$message)
          stop("準備客戶銷售資料失敗: ", e$message)
        })

        # Call analysis_dna() with skip_within_subject = TRUE
        tryCatch({
          message("[DEBUG] Step 4: Calling analysis_dna()...")
          dna_result <- analysis_dna(
            df_sales_by_customer = sales_by_customer,
            df_sales_by_customer_by_date = sales_by_customer_by_date,
            skip_within_subject = TRUE,
            verbose = FALSE
          )
          message("[DEBUG] ✓ Step 4: analysis_dna() completed")
        }, error = function(e) {
          message("[ERROR] Step 4 FAILED: analysis_dna() - ", e$message)
          stop("DNA分析失敗: ", e$message)
        })

        tryCatch({
          message("[DEBUG] Step 5: Converting DNA result to tibble...")
          customer_data <- dna_result$data_by_customer %>%
            as_tibble()
          message("[DEBUG] ✓ Step 5: Converted to tibble, rows: ", nrow(customer_data))
          message("[DEBUG]   Columns from DNA: ", paste(names(customer_data), collapse=", "))
        }, error = function(e) {
          message("[ERROR] Step 5 FAILED: Converting to tibble - ", e$message)
          stop("轉換DNA結果失敗: ", e$message)
        })

        # Add customer_dynamics using analyze_customer_dynamics_new for classification only
        tryCatch({
          message("[DEBUG] Step 6: Calling analyze_customer_dynamics_new()...")
          zscore_results <- analyze_customer_dynamics_new(
            transaction_data,
            method = "auto",
            k = 2.5,
            min_window = 90,
            use_recency_guardrail = TRUE
          )
          message("[DEBUG] ✓ Step 6: analyze_customer_dynamics_new() completed")
          message("[DEBUG]   Method used: ", zscore_results$validation$method_used)
        }, error = function(e) {
          message("[ERROR] Step 6 FAILED: analyze_customer_dynamics_new() - ", e$message)
          stop("客戶動態分析失敗: ", e$message)
        })

        # Merge customer_dynamics from zscore_results
        tryCatch({
          message("[DEBUG] Step 7: Merging customer_dynamics and value_level...")
          customer_data <- customer_data %>%
            left_join(
              zscore_results$customer_data %>% select(customer_id, customer_dynamics, value_level),
              by = "customer_id"
            )
          message("[DEBUG] ✓ Step 7: Merge completed, rows: ", nrow(customer_data))
        }, error = function(e) {
          message("[ERROR] Step 7 FAILED: Merging customer_dynamics - ", e$message)
          stop("合併客戶動態資料失敗: ", e$message)
        })

        use_zscore_method <- (zscore_results$validation$method_used == "z_score")

        # Store metadata
        tryCatch({
          message("[DEBUG] Step 8: Storing metadata...")
          if (use_zscore_method) {
            values$zscore_metadata <- list(
              method = "z_score",
              message = paste0("使用 Z-Score 統計方法（μ_ind=", round(zscore_results$parameters$mu_ind, 1), "天，W=", zscore_results$parameters$W, "天）"),
              mu_ind = zscore_results$parameters$mu_ind,
              W = zscore_results$parameters$W,
              lambda_w = zscore_results$parameters$lambda_w,
              sigma_w = zscore_results$parameters$sigma_w,
              n_customers = nrow(customer_data),
              n_newbies = sum(customer_data$customer_dynamics == "newbie", na.rm = TRUE)
            )
          } else {
            values$zscore_metadata <- list(
              method = "fixed_threshold",
              message = "使用固定閾值方法（資料不足以使用 Z-Score）"
            )
          }
          message("[DEBUG] ✓ Step 8: Metadata stored")
        }, error = function(e) {
          message("[ERROR] Step 8 FAILED: Storing metadata - ", e$message)
          stop("儲存分析資訊失敗: ", e$message)
        })

        # ============================================================================
        # Common Processing: Value Level & Activity Level
        # ============================================================================

        # Rename cai_value to cai for consistency (analysis_dna uses cai_value)
        tryCatch({
          message("[DEBUG] Step 9: Checking cai_value field...")
          message("[DEBUG]   Columns: ", paste(head(names(customer_data), 20), collapse=", "))
          if ("cai_value" %in% names(customer_data) && !"cai" %in% names(customer_data)) {
            customer_data <- customer_data %>%
              rename(cai = cai_value)
            message("[DEBUG] ✓ Step 9: Renamed cai_value to cai")
          } else {
            message("[DEBUG] ✓ Step 9: cai already exists or cai_value not found")
          }
        }, error = function(e) {
          message("[ERROR] Step 9 FAILED: Renaming cai - ", e$message)
          stop("重新命名CAI欄位失敗: ", e$message)
        })

        # Ensure critical fields are correct types before calculations
        tryCatch({
          message("[DEBUG] Step 10: Converting data types...")
          message("[DEBUG]   m_value class before: ", class(customer_data$m_value))
          message("[DEBUG]   m_value sample: ", paste(head(customer_data$m_value, 3), collapse=", "))

          customer_data <- customer_data %>%
            mutate(
              m_value = as.numeric(m_value),
              r_value = as.numeric(r_value),
              f_value = as.numeric(f_value),
              cai = as.numeric(cai),
              cai_ecdf = as.numeric(cai_ecdf),
              ni = as.integer(ni)
            )

          message("[DEBUG] ✓ Step 10: Data types converted")
          message("[DEBUG]   m_value class after: ", class(customer_data$m_value))
          message("[DEBUG]   m_value has NA: ", sum(is.na(customer_data$m_value)))
        }, error = function(e) {
          message("[ERROR] Step 10 FAILED: Converting data types - ", e$message)
          stop("資料類型轉換失敗: ", e$message)
        })

        # ✅ IMPORTANT: Use value_level from analyze_customer_dynamics_new()
        # DON'T recalculate it here - it's already calculated with proper edge case handling
        tryCatch({
          message("[DEBUG] Step 11-12: Using value_level from analyze_customer_dynamics_new()...")

          # Check if value_level exists from zscore_results
          if ("value_level" %in% names(customer_data)) {
            message("[DEBUG]   ✓ value_level already exists from analyze_customer_dynamics_new()")
            value_dist <- table(customer_data$value_level)
            message("[DEBUG]   Distribution: ", paste(names(value_dist), "=", value_dist, collapse = ", "))
          } else {
            # Fallback: shouldn't happen if analyze_customer_dynamics_new() works correctly
            message("[WARN]   value_level not found, using simple P20/P80 method as fallback")
            m_q80 <- quantile(customer_data$m_value, 0.8, na.rm = TRUE)
            m_q20 <- quantile(customer_data$m_value, 0.2, na.rm = TRUE)

            customer_data <- customer_data %>%
              mutate(
                value_level = case_when(
                  is.na(m_value) ~ "未知",
                  m_value >= m_q80 ~ "高",
                  m_value >= m_q20 ~ "中",
                  TRUE ~ "低"
                )
              )
          }

          message("[DEBUG] ✓ Step 11-12: value_level ready")
        }, error = function(e) {
          message("[ERROR] Step 11-12 FAILED: value_level handling - ", e$message)
          stop("處理價值等級失敗: ", e$message)
        })

        # ✅ MAJOR CHANGE: Activity level strictly ni >= 4 only (no degradation)
        # Split customers by ni >= 4
        tryCatch({
          message("[DEBUG] Step 13: Splitting customers by ni >= 4...")
          customers_sufficient <- customer_data %>% filter(ni >= 4)
          customers_insufficient <- customer_data %>% filter(ni < 4)
          message("[DEBUG] ✓ Step 13: Split completed")
          message("[DEBUG]   ni >= 4: ", nrow(customers_sufficient))
          message("[DEBUG]   ni < 4: ", nrow(customers_insufficient))
        }, error = function(e) {
          message("[ERROR] Step 13 FAILED: Splitting customers - ", e$message)
          stop("分割客戶群失敗: ", e$message)
        })

        # Calculate activity_level ONLY for ni >= 4
        if (nrow(customers_sufficient) > 0) {
          tryCatch({
            message("[DEBUG] Step 14: Calculating activity_level for ni >= 4...")
            message("[DEBUG]   cai_ecdf class: ", class(customers_sufficient$cai_ecdf))
            message("[DEBUG]   cai_ecdf sample: ", paste(head(customers_sufficient$cai_ecdf, 3), collapse=", "))

            customers_sufficient <- customers_sufficient %>%
              mutate(
                activity_level = case_when(
                  !is.na(cai_ecdf) ~ case_when(
                    cai_ecdf >= 0.8 ~ "高",  # 漸趨活躍戶
                    cai_ecdf >= 0.2 ~ "中",  # 穩定消費
                    TRUE ~ "低"              # 漸趨靜止戶
                  ),
                  TRUE ~ NA_character_
                )
              )
            message("[DEBUG] ✓ Step 14: activity_level calculated for ni >= 4")
          }, error = function(e) {
            message("[ERROR] Step 14 FAILED: Calculating activity_level - ", e$message)
            stop("計算活躍度等級失敗: ", e$message)
          })
        }

        # ✅ NEW: ni < 4 get NA (no degradation strategy)
        if (nrow(customers_insufficient) > 0) {
          tryCatch({
            message("[DEBUG] Step 15: Setting activity_level to NA for ni < 4...")
            customers_insufficient <- customers_insufficient %>%
              mutate(
                activity_level = NA_character_
              )
            message("[DEBUG] ✓ Step 15: activity_level set to NA")
          }, error = function(e) {
            message("[ERROR] Step 15 FAILED: Setting NA activity_level - ", e$message)
            stop("設定低交易客戶活躍度失敗: ", e$message)
          })
        }

        # Combine back
        tryCatch({
          message("[DEBUG] Step 16: Combining customer groups...")
          customer_data <- bind_rows(customers_sufficient, customers_insufficient) %>%
            arrange(customer_id)
          message("[DEBUG] ✓ Step 16: Combined, total rows: ", nrow(customer_data))
        }, error = function(e) {
          message("[ERROR] Step 16 FAILED: Combining customers - ", e$message)
          stop("合併客戶群失敗: ", e$message)
        })

        # Calculate grid position
        tryCatch({
          message("[DEBUG] Step 17: Calculating grid_position...")
          customer_data <- customer_data %>%
            mutate(
              # First calculate base grid position (A1-C3)
              grid_base = case_when(
                is.na(activity_level) ~ "無",  # ni < 4
                value_level == "高" & activity_level == "高" ~ "A1",
                value_level == "高" & activity_level == "中" ~ "A2",
                value_level == "高" & activity_level == "低" ~ "A3",
                value_level == "中" & activity_level == "高" ~ "B1",
                value_level == "中" & activity_level == "中" ~ "B2",
                value_level == "中" & activity_level == "低" ~ "B3",
                value_level == "低" & activity_level == "高" ~ "C1",
                value_level == "低" & activity_level == "中" ~ "C2",
                value_level == "低" & activity_level == "低" ~ "C3",
                TRUE ~ "其他"
              ),
              # Then add lifecycle suffix based on customer_dynamics
              # Note: customer_dynamics from DNA analysis is in English
              lifecycle_suffix = case_when(
                customer_dynamics == "newbie" ~ "N",
                customer_dynamics == "active" ~ "C",
                customer_dynamics == "sleepy" ~ "S",
                customer_dynamics == "half_sleepy" ~ "H",
                customer_dynamics == "dormant" ~ "D",
                TRUE ~ ""
              ),
              # Combine to create full grid_position (e.g., "A1C", "B2N")
              grid_position = if_else(
                grid_base == "無" | grid_base == "其他",
                grid_base,
                paste0(grid_base, lifecycle_suffix)
              )
            ) %>%
            select(-grid_base, -lifecycle_suffix)  # Remove temporary columns
          message("[DEBUG] ✓ Step 17: grid_position calculated")
        }, error = function(e) {
          message("[ERROR] Step 17 FAILED: Calculating grid_position - ", e$message)
          stop("計算九宮格位置失敗: ", e$message)
        })

        # ✅ Calculate all customer tags (base value, RFM, status, prediction)
        # This populates tag_017_customer_dynamics and other tags needed by downstream modules
        tryCatch({
          message("[DEBUG] Step 18: Calculating customer tags...")
          customer_data <- calculate_all_customer_tags(customer_data)
          message("[DEBUG] ✓ Step 18: Customer tags calculated")
        }, error = function(e) {
          message("[ERROR] Step 18 FAILED: Calculating tags - ", e$message)
          stop("計算客戶標籤失敗: ", e$message)
        })

        # Store results
        values$dna_results <- list(
          data_by_customer = customer_data,
          method = if(use_zscore_method) "z_score" else "fixed_threshold"
        )

        # ✅ CRITICAL FIX: Set values$processed_data for customer status module
        # Customer status module expects values$processed_data with tag_017_customer_dynamics
        values$processed_data <- customer_data

        output$status <- renderText({
          method_name <- if(use_zscore_method) "Z-Score 統計方法" else "固定閾值方法"
          paste0("✅ 分析完成！使用 ", method_name, "\n",
                 "總客戶數: ", nrow(customer_data), "\n",
                 "ni ≥ 4 (有活躍度): ", sum(!is.na(customer_data$activity_level)), "\n",
                 "ni < 4 (無活躍度): ", sum(is.na(customer_data$activity_level)))
        })

      }, error = function(e) {
        output$status <- renderText({
          paste0("❌ 分析失敗：", e$message)
        })
      })
    })

    # ============================================================================
    # UI OUTPUTS
    # ============================================================================

    # Show results flag
    output$show_results <- reactive({
      !is.null(values$dna_results)
    })
    outputOptions(output, "show_results", suspendWhenHidden = FALSE)

    # ✅ DYNAMIC: Customer Dynamics selector based on available data
    output$customer_dynamics_selector <- renderUI({
      req(values$dna_results)

      df <- values$dna_results$data_by_customer

      # Count customers by dynamics stage
      stage_counts <- df %>%
        group_by(customer_dynamics) %>%
        summarise(count = n(), .groups = "drop") %>%
        filter(count > 0)  # Only include stages with data

      # Create dynamic choices
      all_choices <- c("全部" = "all")

      stage_labels <- c(
        "newbie" = "新客 (Newbie)",
        "active" = "主力客 (Active)",
        "sleepy" = "瞌睡客 (Sleepy)",
        "half_sleepy" = "半睡客 (Half-Sleep)",
        "dormant" = "沉睡客 (Dormant)"
      )

      for (stage in stage_counts$customer_dynamics) {
        if (stage %in% names(stage_labels)) {
          count <- stage_counts$count[stage_counts$customer_dynamics == stage]
          all_choices[[paste0(stage_labels[stage], " (", count, ")")]] <- stage
        }
      }

      selectInput(
        session$ns("customer_dynamics"),
        "選擇顧客動態階段：",
        choices = all_choices,
        selected = "all"
      )
    })

    # ✅ NEW: Z-Score metadata display
    output$zscore_metadata <- renderUI({
      req(values$zscore_metadata)

      meta <- values$zscore_metadata

      if (meta$method == "fixed_threshold") {
        tags$div(
          class = "alert alert-warning",
          icon("info-circle"),
          " ", meta$message
        )
      } else {
        tags$ul(
          tags$li(paste0("中位購買間隔 (μ_ind): ", round(meta$mu_ind, 1), " 天")),
          tags$li(paste0("活躍觀察窗 (W): ", meta$W, " 天 (", round(meta$W/7), " 週)")),
          tags$li(paste0("平均窗口購買次數 (λ_w): ", round(meta$lambda_w, 2))),
          tags$li(paste0("標準差 (σ_w): ", round(meta$sigma_w, 2))),
          tags$li(paste0("分析客戶數: ", meta$n_customers, " (新客: ", meta$n_newbies, ")"))
        )
      }
    })

    # Summary metrics
    output$total_customers <- renderUI({
      req(values$dna_results)
      h2(nrow(values$dna_results$data_by_customer), style = "margin: 0;")
    })

    output$avg_order_value <- renderUI({
      req(values$dna_results)
      # Use m_value (monetary value per transaction) instead of aov
      avg_m <- mean(values$dna_results$data_by_customer$m_value, na.rm = TRUE)
      h2(paste0("$", round(avg_m, 0)), style = "margin: 0;")
    })

    output$avg_purchase_cycle <- renderUI({
      req(values$dna_results, values$zscore_metadata)

      # ✅ 使用多次購買顧客的平均購買間隔（排除單次購買）
      # 資料不足時仍優先使用 z-score 計算出的 μ_ind
      if (!is.null(values$zscore_metadata$mu_ind) &&
          is.finite(values$zscore_metadata$mu_ind)) {
        return(h2(round(values$zscore_metadata$mu_ind, 1), style = "margin: 0;"))
      }

      df <- values$dna_results$data_by_customer

      if (!all(c("ipt", "ni") %in% names(df))) {
        return(h2("N/A", style = "margin: 0;"))
      }

      cycle_values <- df %>%
        filter(!is.na(ipt), ipt > 0, !is.na(ni), ni >= 2) %>%
        mutate(avg_cycle = ipt / pmax(ni - 1, 1)) %>%
        pull(avg_cycle)

      if (length(cycle_values) == 0 || all(is.na(cycle_values))) {
        h2("N/A", style = "margin: 0;")
      } else {
        h2(round(median(cycle_values, na.rm = TRUE), 1), style = "margin: 0;")
      }
    })

    output$avg_transaction <- renderUI({
      req(values$dna_results)
      avg_ni <- mean(values$dna_results$data_by_customer$ni, na.rm = TRUE)
      h2(round(avg_ni, 1), style = "margin: 0;")
    })

    # ========================================================================
    # Grid Matrix Visualization - Updated for Z-Score Method
    # ========================================================================

    # Calculate nine grid data
    nine_grid_data <- reactive({
      req(values$dna_results, input$customer_dynamics)

      df <- values$dna_results$data_by_customer

      # Filter selected customer dynamics stage
      df <- df[df$customer_dynamics == input$customer_dynamics, ]

      if (nrow(df) == 0) return(NULL)

      return(df)
    })

    # Generate grid content function
    generate_grid_content <- function(value_level, activity_level, df, customer_dynamics) {
      if (is.null(df)) {
        return(HTML('<div style="text-align: center; padding: 15px;">無此顧客動態的客戶</div>'))
      }

      # Calculate customer count in this segment
      customers <- df[df$value_level == value_level & df$activity_level == activity_level, ]
      count <- nrow(customers)

      # Calculate total and percentage
      total_customers <- nrow(df)
      percentage <- if (total_customers > 0) round((count / total_customers) * 100, 1) else 0

      if (count == 0) {
        return(HTML('<div style="text-align: center; padding: 15px; color: #999;">無此類型客戶</div>'))
      }

      # Calculate averages
      avg_m <- round(mean(customers$m_value, na.rm = TRUE), 0)
      avg_f <- round(mean(customers$f_value, na.rm = TRUE), 2)

      # Define grid position
      grid_position <- paste0(
        switch(value_level, "高" = "A", "中" = "B", "低" = "C"),
        switch(activity_level, "高" = "1", "中" = "2", "低" = "3"),
        switch(customer_dynamics,
          "newbie" = "N",
          "active" = "C",
          "sleepy" = "D",
          "half_sleepy" = "H",
          "dormant" = "S"
        )
      )

      # Get strategy
      strategy <- get_strategy(grid_position)

      # If strategy is NULL (hidden combination), return placeholder
      if (is.null(strategy)) {
        return(HTML('<div style="text-align: center; padding: 15px; color: #ccc; font-style: italic;">（新客不適用）</div>'))
      }

      # Color coding
      value_intensity <- switch(value_level,
        "高" = 1.0,
        "中" = 0.6,
        "低" = 0.3
      )

      activity_color <- switch(activity_level,
        "高" = "#10b981",
        "中" = "#f59e0b",
        "低" = "#ef4444"
      )

      stage_color <- switch(customer_dynamics,
        "newbie" = "#4CAF50",
        "active" = "#2196F3",
        "sleepy" = "#FFC107",
        "half_sleepy" = "#FF9800",
        "dormant" = "#F44336"
      )

      bg_color <- sprintf("rgba(%d, %d, %d, %.2f)",
        col2rgb(activity_color)[1],
        col2rgb(activity_color)[2],
        col2rgb(activity_color)[3],
        0.1 * value_intensity)

      # Generate HTML
      HTML(sprintf('
        <div style="text-align: center; padding: 20px; border-left: 5px solid %s; background: %s; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.05);">
          <div style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;">
            <span style="font-size: 14px; font-weight: bold; color: #666; background: white; padding: 4px 8px; border-radius: 4px;">%s</span>
            <span style="font-size: 12px; color: %s; background: white; padding: 4px 8px; border-radius: 4px; font-weight: bold;">%s占比</span>
          </div>

          <h4 style="margin: 10px 0; color: #333;">
            <i class="fas fa-%s" style="margin-right: 8px; color: %s;"></i>
            %s
          </h4>

          <div style="font-size: 28px; font-weight: bold; margin: 15px 0; color: #2c3e50;">
            %d <span style="font-size: 16px; color: #7f8c8d;">位</span>
          </div>

          <div style="background: white; padding: 12px; border-radius: 6px; margin: 12px 0;">
            <div style="display: flex; justify-content: space-around; text-align: center;">
              <div>
                <div style="color: #888; font-size: 11px; margin-bottom: 4px;">平均金額</div>
                <div style="color: #2c3e50; font-weight: bold; font-size: 15px;">%s</div>
              </div>
              <div style="border-left: 1px solid #ddd;"></div>
              <div>
                <div style="color: #888; font-size: 11px; margin-bottom: 4px;">購買頻率</div>
                <div style="color: #2c3e50; font-weight: bold; font-size: 15px;">%.2f</div>
              </div>
            </div>
          </div>

          <div style="background: linear-gradient(135deg, #667eea 0%%, #764ba2 100%%); color: white; padding: 12px; border-radius: 6px; margin: 10px 0;">
            <div style="font-size: 11px; margin-bottom: 4px; opacity: 0.9;">建議策略</div>
            <div style="font-weight: bold; font-size: 13px;">%s</div>
          </div>

          <div style="color: #888; font-size: 11px; margin-top: 8px; font-style: italic;">
            KPI: %s
          </div>
        </div>
      ', stage_color, bg_color, grid_position, activity_color, paste0(percentage, "%%"),
         strategy$icon, activity_color, strategy$title, count,
         format(avg_m, big.mark=","), avg_f, strategy$action, strategy$kpi))
    }

    # Dynamic grid generation
    output$grid_matrix <- renderUI({
      df <- nine_grid_data()

      if (is.null(df)) {
        return(
          div(style = "text-align: center; padding: 50px;",
              h4("請先完成資料上傳並進行分析"))
        )
      }

      current_stage <- input$customer_dynamics

      # Check for insufficient non-newbie customers (ni < 4)
      insufficient_non_newbie <- df %>%
        filter(ni < 4, customer_dynamics != "newbie")

      if (nrow(insufficient_non_newbie) > 0 & current_stage != "newbie") {
        insufficient_message <- div(
          bs4Card(
            title = "⚠️ 交易次數不足",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            p(paste0("目前選擇的顧客動態中，有 ", nrow(insufficient_non_newbie),
                     " 位客戶的交易次數少於 4 次，無法可靠計算活躍度。")),
            p("這些客戶不會顯示在下方的九宮格分析中。"),
            p(strong("建議："), "持續觀察這些客戶，待交易次數達到 4 次以上後再進行完整分析。")
          )
        )
      } else {
        insufficient_message <- NULL
      }

      # Filter: only show ni >= 4 or newbie in grid
      df_for_grid <- df %>%
        filter(ni >= 4 | customer_dynamics == "newbie")

      # Newbie special handling
      if (current_stage == "newbie") {
        newbie_data <- df %>% filter(customer_dynamics == "newbie")
        newbie_count <- nrow(newbie_data)

        newbie_by_value <- newbie_data %>%
          group_by(value_level) %>%
          summarise(
            count = n(),
            avg_value = mean(m_value, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          arrange(desc(avg_value))

        return(div(
          h4("顧客動態：新客", style = "text-align: center; margin: 20px 0;"),

          bs4Card(
            title = "📊 新客概況",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            fluidRow(
              column(4,
                bs4ValueBox(
                  value = newbie_count,
                  subtitle = "新客總數",
                  icon = icon("user-plus"),
                  color = "info",
                  width = 12
                )
              ),
              column(4,
                bs4ValueBox(
                  value = sprintf("$%.0f", mean(newbie_data$m_value, na.rm = TRUE)),
                  subtitle = "平均首單價值",
                  icon = icon("dollar-sign"),
                  color = "success",
                  width = 12
                )
              ),
              column(4,
                bs4ValueBox(
                  value = sprintf("%.0f天", mean(newbie_data$r_value, na.rm = TRUE)),
                  subtitle = "平均首購距今",
                  icon = icon("clock"),
                  color = "warning",
                  width = 12
                )
              )
            )
          ),

          h4("💡 新客行銷策略", style = "text-align: center; margin: 30px 0 20px 0;"),

          fluidRow(
            column(4,
              bs4Card(
                title = "A3N：王者休眠-N",
                status = "success",
                solidHeader = TRUE,
                width = 12,
                div(
                  h3(style = "color: #28a745; margin: 0;",
                     newbie_by_value %>% filter(value_level == "高") %>% pull(count) %>% {if(length(.) > 0) . else 0}),
                  p(class = "text-muted", "位客戶"),
                  hr(),
                  div(style = "padding: 10px; background: #f8f9fa; border-radius: 5px; margin-bottom: 10px;",
                    strong("指標："), "高V 低(無)A 新客"
                  ),
                  div(style = "padding: 10px; background: #e7f3ff; border-radius: 5px;",
                    strong("🎯 行銷方案："),
                    tags$ul(style = "margin: 10px 0 0 0; padding-left: 20px;",
                      tags$li("首購後 48h 無互動 → 專屬客服問候")
                    )
                  )
                )
              )
            ),
            column(4,
              bs4Card(
                title = "B3N：成長停滯-N",
                status = "warning",
                solidHeader = TRUE,
                width = 12,
                div(
                  h3(style = "color: #ffc107; margin: 0;",
                     newbie_by_value %>% filter(value_level == "中") %>% pull(count) %>% {if(length(.) > 0) . else 0}),
                  p(class = "text-muted", "位客戶"),
                  hr(),
                  div(style = "padding: 10px; background: #f8f9fa; border-radius: 5px; margin-bottom: 10px;",
                    strong("指標："), "中V 低(無)A 新客"
                  ),
                  div(style = "padding: 10px; background: #fff3cd; border-radius: 5px;",
                    strong("🎯 行銷方案："),
                    tags$ul(style = "margin: 10px 0 0 0; padding-left: 20px;",
                      tags$li("首購加碼券（限 72h）")
                    )
                  )
                )
              )
            ),
            column(4,
              bs4Card(
                title = "C3N：清倉邊緣-N",
                status = "danger",
                solidHeader = TRUE,
                width = 12,
                div(
                  h3(style = "color: #dc3545; margin: 0;",
                     newbie_by_value %>% filter(value_level == "低") %>% pull(count) %>% {if(length(.) > 0) . else 0}),
                  p(class = "text-muted", "位客戶"),
                  hr(),
                  div(style = "padding: 10px; background: #f8f9fa; border-radius: 5px; margin-bottom: 10px;",
                    strong("指標："), "低V 低(無)A 新客"
                  ),
                  div(style = "padding: 10px; background: #f8d7da; border-radius: 5px;",
                    strong("🎯 行銷方案："),
                    tags$ul(style = "margin: 10px 0 0 0; padding-left: 20px;",
                      tags$li("取消後續推播、只留月度新品 EDM")
                    )
                  )
                )
              )
            )
          ),

          wellPanel(
            style = "background-color: #fff3cd; border-left: 4px solid #ffc107;",
            h5(icon("info-circle"), " 說明", style = "margin-top: 0;"),
            p("新客（交易次數 = 1）無法計算活躍度（需 ≥4 筆交易），因此不顯示九宮格。"),
            p("建議針對不同價值層級的新客，採用差異化的培育策略，促進二次購買。")
          )
        ))
      }

      # Other customer dynamics: show grid
      div(
        insufficient_message,

        h4(paste("顧客動態:",
                 switch(current_stage,
                        "active" = "主力客",
                        "sleepy" = "瞌睡客",
                        "half_sleepy" = "半睡客",
                        "dormant" = "沉睡客")),
            style = "text-align: center; margin: 20px 0;"),

        # High value customers
        fluidRow(
          column(4, bs4Card(title = "高價值 × 高活躍度", status = "success", width = 12, solidHeader = TRUE,
                          generate_grid_content("高", "高", df_for_grid, current_stage))),
          column(4, bs4Card(title = "高價值 × 中活躍度", status = "success", width = 12, solidHeader = TRUE,
                          generate_grid_content("高", "中", df_for_grid, current_stage))),
          column(4, bs4Card(title = "高價值 × 低活躍度", status = "success", width = 12, solidHeader = TRUE,
                          generate_grid_content("高", "低", df_for_grid, current_stage)))
        ),

        # Medium value customers
        fluidRow(
          column(4, bs4Card(title = "中價值 × 高活躍度", status = "warning", width = 12, solidHeader = TRUE,
                          generate_grid_content("中", "高", df_for_grid, current_stage))),
          column(4, bs4Card(title = "中價值 × 中活躍度", status = "warning", width = 12, solidHeader = TRUE,
                          generate_grid_content("中", "中", df_for_grid, current_stage))),
          column(4, bs4Card(title = "中價值 × 低活躍度", status = "warning", width = 12, solidHeader = TRUE,
                          generate_grid_content("中", "低", df_for_grid, current_stage)))
        ),

        # Low value customers
        fluidRow(
          column(4, bs4Card(title = "低價值 × 高活躍度", status = "danger", width = 12, solidHeader = TRUE,
                          generate_grid_content("低", "高", df_for_grid, current_stage))),
          column(4, bs4Card(title = "低價值 × 中活躍度", status = "danger", width = 12, solidHeader = TRUE,
                          generate_grid_content("低", "中", df_for_grid, current_stage))),
          column(4, bs4Card(title = "低價值 × 低活躍度", status = "danger", width = 12, solidHeader = TRUE,
                          generate_grid_content("低", "低", df_for_grid, current_stage)))
        )
      )
    })

    # Customer table
    output$customer_table <- renderDT({
      req(values$dna_results)

      df <- values$dna_results$data_by_customer

      # Filter by customer_dynamics if selected
      if (!is.null(input$customer_dynamics) && input$customer_dynamics != "all") {
        df <- df %>% filter(customer_dynamics == input$customer_dynamics)
      }

      # ✅ FIX Issue #4: Add customer type labels and strategies
      # Use get_strategy() to extract title (customer type) and action (strategy)
      df <- df %>%
        rowwise() %>%
        mutate(
          strategy_data = list(get_strategy(grid_position)),
          customer_type = if(!is.null(strategy_data)) strategy_data$title else NA_character_,
          strategy = if(!is.null(strategy_data)) strategy_data$action else NA_character_
        ) %>%
        ungroup() %>%
        select(-strategy_data)

      # ✅ FIX Issue #4: Chinese column names and add type/strategy columns
      display_df <- df %>%
        select(
          客戶ID = customer_id,
          客戶類型標籤 = customer_type,           # NEW: Customer type label (e.g., "潛力新芽")
          建議策略 = strategy,                    # NEW: Strategy recommendation
          生命週期階段 = customer_dynamics,
          價值等級 = value_level,
          活躍度等級 = activity_level,
          九宮格位置 = grid_position,
          交易次數 = ni,
          最近購買天數 = r_value,
          購買金額 = m_value,
          any_of(c(
            "平均購買間隔" = "ipt",
            "Z分數" = "z_i",
            "加權頻率" = "F_i_w"
          ))
        )

      datatable(
        display_df,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = 'frtip'  # ✅ 移除 buttons，改用自訂下載按鈕
        ),
        rownames = FALSE
      ) %>%
        formatRound(c('最近購買天數', '購買金額'), digits = 1)
    })

    # ============================================================================
    # DOWNLOAD HANDLER - Customer Data CSV with UTF-8 BOM
    # ============================================================================

    output$download_customer_data <- downloadHandler(
      filename = function() {
        paste0("customer_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        req(values$dna_results)

        df <- values$dna_results$data_by_customer

        # Get strategy data for each customer
        df <- df %>%
          rowwise() %>%
          mutate(
            strategy_data = list(get_strategy(grid_position)),
            customer_type = if(!is.null(strategy_data)) strategy_data$title else NA_character_,
            strategy = if(!is.null(strategy_data)) strategy_data$action else NA_character_
          ) %>%
          ungroup() %>%
          select(-strategy_data)

        # Prepare export data with Chinese column names
        export_df <- df %>%
          select(
            客戶ID = customer_id,
            客戶類型標籤 = customer_type,
            建議策略 = strategy,
            生命週期階段 = customer_dynamics,
            價值等級 = value_level,
            活躍度等級 = activity_level,
            九宮格位置 = grid_position,
            交易次數 = ni,
            最近購買天數 = r_value,
            購買金額 = m_value,
            any_of(c(
              "平均購買間隔" = "ipt",
              "Z分數" = "z_i",
              "加權頻率" = "F_i_w"
            ))
          )

        # Format numeric columns
        export_df <- export_df %>%
          mutate(
            最近購買天數 = round(最近購買天數, 1),
            購買金額 = round(購買金額, 1)
          )

        # ✅ Write with UTF-8 BOM for Excel compatibility
        con <- file(file, open = "wb", encoding = "UTF-8")
        writeBin(charToRaw('\ufeff'), con)  # UTF-8 BOM
        write.csv(export_df, con, row.names = FALSE, fileEncoding = "UTF-8")
        close(con)
      }
    )

    # ============================================================================
    # RETURN REACTIVE DATA (Critical for downstream modules)
    # ============================================================================

    return(reactive({
      req(values$dna_results)
      values$dna_results$data_by_customer
    }))

  })
}

# ============================================================================
# STRATEGY DEFINITION FUNCTION
# ============================================================================

get_strategy <- function(grid_position) {
  # Hidden combinations (newbie high/medium activity - not applicable)
  hidden_segments <- c("A1N", "A2N", "B1N", "B2N", "C1N", "C2N")

  # If hidden combination, return NULL
  if (grid_position %in% hidden_segments) {
    return(NULL)
  }

  # Define strategies for 45 different combinations
  strategies <- list(
    # Newbie strategies (N) - Only A3N, B3N, C3N
    "A3N" = list(
      title = "王者休眠-N",
      action = "首購後 48h 無互動 → 專屬客服問候",
      icon = "user-clock",
      kpi = "高V 低A 新客"
    ),
    "B3N" = list(
      title = "成長停滯-N",
      action = "首購加碼券 (限 72h)",
      icon = "pause",
      kpi = "中V 低A 新客"
    ),
    "C3N" = list(
      title = "清倉邊緣-N",
      action = "取消後續推播、只留月度新品 EDM",
      icon = "trash",
      kpi = "低V 低A 新客"
    ),

    # Active customer strategies (C)
    "A1C" = list(
      title = "王者引擎-C",
      action = "VIP 社群 + 新品搶先權",
      icon = "crown",
      kpi = "高V 高A 主力"
    ),
    "A2C" = list(
      title = "王者穩健-C",
      action = "階梯折扣券 (高門檻)",
      icon = "star",
      kpi = "高V 中A 主力"
    ),
    "A3C" = list(
      title = "王者休眠-C",
      action = "高值客深度訪談 + 專屬客服",
      icon = "user-clock",
      kpi = "高V 低A 主力"
    ),
    "B1C" = list(
      title = "成長火箭-C",
      action = "訂閱制試用 + 個性化推薦",
      icon = "rocket",
      kpi = "中V 高A 主力"
    ),
    "B2C" = list(
      title = "成長常規-C",
      action = "點數倍數日/會員日",
      icon = "chart-line",
      kpi = "中V 中A 主力"
    ),
    "B3C" = list(
      title = "成長停滯-C",
      action = "再購提醒 + 小樣包",
      icon = "pause",
      kpi = "中V 低A 主力"
    ),
    "C1C" = list(
      title = "潛力新芽-C",
      action = "引導升級高單價品",
      icon = "seedling",
      kpi = "低V 高A 主力"
    ),
    "C2C" = list(
      title = "潛力維持-C",
      action = "補貨提醒 + 省運方案",
      icon = "balance-scale",
      kpi = "低V 中A 主力"
    ),
    "C3C" = list(
      title = "清倉邊緣-C",
      action = "低成本關懷：避免過度促銷",
      icon = "trash",
      kpi = "低V 低A 主力"
    ),

    # Sleepy customer strategies (D)
    "A1D" = list(
      title = "王者引擎-D",
      action = "專屬醒修券 (8 折上限)",
      icon = "crown",
      kpi = "高V 高A 瞌睡"
    ),
    "A2D" = list(
      title = "王者穩健-D",
      action = "客服致電關懷 + NPS 調查",
      icon = "star",
      kpi = "高V 中A 瞌睡"
    ),
    "A3D" = list(
      title = "王者休眠-D",
      action = "Win-Back 套餐 + VIP 續會禮",
      icon = "user-clock",
      kpi = "高V 低A 瞌睡"
    ),
    "B1D" = list(
      title = "成長火箭-D",
      action = "小遊戲抽獎 + 回購券",
      icon = "rocket",
      kpi = "中V 高A 瞌睡"
    ),
    "B2D" = list(
      title = "成長常規-D",
      action = "品類換血建議 + 搭售優惠",
      icon = "chart-line",
      kpi = "中V 中A 瞌睡"
    ),
    "B3D" = list(
      title = "成長停滯-D",
      action = "Push+SMS 雙管齊下",
      icon = "pause",
      kpi = "中V 低A 瞌睡"
    ),
    "C1D" = list(
      title = "潛力新芽-D",
      action = "低價快速回購品推薦",
      icon = "seedling",
      kpi = "低V 高A 瞌睡"
    ),
    "C2D" = list(
      title = "潛力維持-D",
      action = "簡訊喚醒 + 滿額贈",
      icon = "balance-scale",
      kpi = "低V 中A 瞌睡"
    ),
    "C3D" = list(
      title = "清倉邊緣-D",
      action = "清庫存閃購一天",
      icon = "trash",
      kpi = "低V 低A 瞌睡"
    ),

    # Half-sleepy customer strategies (H)
    "A1H" = list(
      title = "王者引擎-H",
      action = "專屬客服 + 差異化補貼",
      icon = "crown",
      kpi = "高V 高A 半睡"
    ),
    "A2H" = list(
      title = "王者穩健-H",
      action = "兩步式「問卷→優惠」",
      icon = "star",
      kpi = "高V 中A 半睡"
    ),
    "A3H" = list(
      title = "王者休眠-H",
      action = "VIP 醒修券...滿額升等",
      icon = "user-clock",
      kpi = "高V 低A 半睡"
    ),
    "B1H" = list(
      title = "成長火箭-H",
      action = "會員日兌換券",
      icon = "rocket",
      kpi = "中V 高A 半睡"
    ),
    "B2H" = list(
      title = "成長常規-H",
      action = "價格敏品小額試用",
      icon = "chart-line",
      kpi = "中V 中A 半睡"
    ),
    "B3H" = list(
      title = "成長停滯-H",
      action = "封存前最後折扣",
      icon = "pause",
      kpi = "中V 低A 半睡"
    ),
    "C1H" = list(
      title = "潛力新芽-H",
      action = "爆款低價促購",
      icon = "seedling",
      kpi = "低V 高A 半睡"
    ),
    "C2H" = list(
      title = "潛力維持-H",
      action = "免運券 + 再購提醒",
      icon = "balance-scale",
      kpi = "低V 中A 半睡"
    ),
    "C3H" = list(
      title = "清倉邊緣-H",
      action = "月度 EDM；不再 Push",
      icon = "trash",
      kpi = "低V 低A 半睡"
    ),

    # Dormant customer strategies (S)
    "A1S" = list(
      title = "王者引擎-S",
      action = "客服電話 + 專屬復活禮盒",
      icon = "crown",
      kpi = "高V 高A 沉睡"
    ),
    "A2S" = list(
      title = "王者穩健-S",
      action = "高值客流失調查 + 買一送一",
      icon = "star",
      kpi = "高V 中A 沉睡"
    ),
    "A3S" = list(
      title = "王者休眠-S",
      action = "只做客情維繫，勿頻促",
      icon = "user-clock",
      kpi = "高V 低A 沉睡"
    ),
    "B1S" = list(
      title = "成長火箭-S",
      action = "不定期驚喜包",
      icon = "rocket",
      kpi = "中V 高A 沉睡"
    ),
    "B2S" = list(
      title = "成長常規-S",
      action = "庫存清倉先行名單",
      icon = "chart-line",
      kpi = "中V 中A 沉睡"
    ),
    "B3S" = list(
      title = "成長停滯-S",
      action = "定向廣告 retarget + SMS",
      icon = "pause",
      kpi = "中V 低A 沉睡"
    ),
    "C1S" = list(
      title = "潛力新芽-S",
      action = "簡訊一次 + 退訂選項",
      icon = "seedling",
      kpi = "低V 高A 沉睡"
    ),
    "C2S" = list(
      title = "潛力維持-S",
      action = "只保留月報 EDM",
      icon = "balance-scale",
      kpi = "低V 中A 沉睡"
    ),
    "C3S" = list(
      title = "清倉邊緣-S",
      action = "名單除重/不再接觸",
      icon = "trash",
      kpi = "低V 低A 沉睡"
    )
  )

  # If strategy not found, return default
  default_strategy <- list(
    title = paste("分類", grid_position),
    action = "一般性行銷活動",
    icon = "users",
    kpi = "基礎指標追蹤"
  )

  return(strategies[[grid_position]] %||% default_strategy)
}
