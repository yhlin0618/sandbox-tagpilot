#LOCK FILE
#
# microDNADistribution.R
#
# Following principles:
# - MP56: Connected Component Principle (component structure)
# - MP55: Computation Allocation Principle (pre-computation)
# - MP73: Interactive Visualization Preference (plotly for visualizations)
# - MP81: Explicit Parameter Specification (function arguments)
# - R116: Enhanced Data Access with tbl2 (data access)
# - R91: Universal Data Access Pattern (data access)
# - R09: UI-Server-Defaults Triple (component organization)
# - P006: Data Visualization (visualization standards)
#

# LOCK FILE
#
# 重新設計：
#   • 拔除 MP55「預先計算」邏輯 → 不再有 input$use_precomputed
#   • 空資料改用 plotly::plotly_empty()，避免連續警告
#   • 改用 tbl2 取代 universal_data_accessor (R116原則)
#   • 保留 Connected Component 架構
# -----------------------------------------------------------------------------

# helper ----------------------------------------------------------------------
#' Paste operator for string concatenation
#' @param x Character string. First string to concatenate.
#' @param y Character string. Second string to concatenate.
#' @return Character string. The concatenated result of x and y.
`%+%` <- function(x, y) paste0(x, y)

# Filter UI -------------------------------------------------------------------
#' microDNADistributionFilterUI
#' @param id Character string. The module ID used for namespacing inputs and outputs.
#' @param translate Function. Translation function for UI text elements (defaults to identity function).
#'        Should accept a string and return a translated string.
#' @return shiny.tag. A Shiny UI component containing the filter controls for the DNA distribution component.
microDNADistributionFilterUI <- function(id, translate = identity) {
  ns <- NS(id)
  wellPanel(
    style = "padding:15px;",
    h4(translate("Metric Selection")),
    actionButton(ns("m_ecdf"),  translate("Purchase Amount (M)"), class = "btn-block btn-info mb-2"),
    actionButton(ns("r_ecdf"),  translate("Recency (R)"),          class = "btn-block btn-info mb-2"),
    actionButton(ns("f_ecdf"),  translate("Frequency (F)"),        class = "btn-block btn-info mb-2"),
    actionButton(ns("ipt_ecdf"),translate("Inter‑purchase Time"),   class = "btn-block btn-info mb-2"),
    hr(), h4(translate("Visualization Type")),
    actionButton(ns("f_barplot"),  translate("Frequency Histogram"), class = "btn-block btn-secondary mb-2"),
    actionButton(ns("nes_barplot"),translate("NES Distribution"),    class = "btn-block btn-secondary mb-2"),
    hr(),
    textOutput(ns("component_status"))
  )
}

# Display UI ------------------------------------------------------------------
#' microDNADistributionDisplayUI
#' @param id Character string. The module ID used for namespacing inputs and outputs.
#' @return shiny.tag. A Shiny UI component containing the display elements for the DNA distribution visualizations.
microDNADistributionDisplayUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "component-header mb-3 text-center",
        h3("Customer DNA Distribution Analysis"),
        p("Visualize key customer metrics distribution patterns")),
    div(class = "component-output",
        plotlyOutput(ns("dna_distribution_plot"), height = "450px"),
        uiOutput(ns("metric_stats")))
  )
}

# Server ----------------------------------------------------------------------
#' microDNADistributionServer
#' @param id Character string. The module ID used for namespacing inputs and outputs.
#' @param app_data_connection Database connection object or list. Any connection type supported by tbl2.
#'        Can be a DBI connection, a list with getter functions, a file path, or NULL if no database access is needed.
#' @param config List or reactive expression. Optional configuration settings that can customize behavior.
#'        If reactive, will be re-evaluated when dependencies change.
#' @param session Shiny session object. The current Shiny session (defaults to getDefaultReactiveDomain()).
#' @return list. A list of reactive values providing access to component state and data:
#'        - current_visualization: reactive value indicating the current visualization type
#'        - component_status: reactive value indicating the current status (idle, loading, ready, etc.)
#'        - df_dna_by_customer: reactive containing the fetched customer DNA data
microDNADistributionServer <- function(id, app_data_connection = NULL, config = NULL,
                                       session = getDefaultReactiveDomain()) {
  moduleServer(id, function(input, output, session) {
    
    # ------------ 狀態 ---------------------------------------------
    #' Reactive value for tracking current visualization
    #' @description Tracks which visualization is currently active
    #' @return Character string. The ID of the current visualization button
    current_visualization <- reactiveVal("none")
    
    #' Reactive value for tracking component status
    #' @description Tracks the current status of the component (loading, ready, etc.)
    #' @return Character string. One of "idle", "loading", "ready", "computing", or "error"
    component_status      <- reactiveVal("idle")
    
    # ------------ 解析 platform_id ----------------------------------
    #' Extract platform_id from configuration
    #' 
    #' Parses the provided configuration (reactive or static) to extract platform_id.
    #' Handles both direct platform_id and nested platform_id in filters.
    #' 
    #' @return Numeric or NULL. The extracted platform_id or NULL if not found.
    platform_id <- reactive({
      # Handle both reactive and non-reactive config
      cfg <- if (is.function(config) && "reactive" %in% class(config)) config() else config
      
      # Extract platform_id from config if available
      if (!is.null(cfg)) {
        # Check for direct platform_id
        if (!is.null(cfg[["platform_id"]])) {
          return(cfg[["platform_id"]])
        }
        # Check for nested platform_id in filters
        if (!is.null(cfg[["filters"]]) && !is.null(cfg[["filters"]][["platform_id"]])) {
          return(cfg[["filters"]][["platform_id"]])
        }
      }
      
      # Return NULL if no platform_id found
      NULL
    })
    
    # ------------ 資料存取 (R116) ------------------------------------
    #' Reactive data accessor for customer DNA data
    #' 
    #' Retrieves customer DNA data from the database or data source using the Enhanced Data Access
    #' Pattern (R116). Applies platform filtering if specified in the configuration.
    #' 
    #' @return data.frame. A reactive data frame containing customer DNA metrics.
    #'         Contains columns for customer_id, M, R, F, IPT_mean, and NES.
    df_dna_by_customer <- reactive({
      # Update component status to show data loading
      component_status("loading")
      
      # Get platform ID from config
      plat <- platform_id()
      
      # Access data directly using tbl2 for enhanced data access (R116)
      if (!is.null(plat) && !is.na(plat) && as.numeric(plat) > 0) {
        # Apply platform filter when a valid platform ID is provided
        numeric_plat <- as.integer(plat)
        # SLN08: Use !! to force evaluation of variables before SQL translation
        # This prevents "Cannot translate a shiny reactive to SQL" errors
        res <- tbl2(app_data_connection, "df_dna_by_customer") %>%
          dplyr::filter(platform_id == !!numeric_plat)
      } else {
        # Use tbl2 without filter
        res <- tbl2(app_data_connection, "df_dna_by_customer")
      }
      
      # Update component status and return results
      component_status("ready")
      res %>% collect()
    })
    
    # ------------ helper ------------------------------------------------------
    #' Compute distribution for ECDF plotting
    #' @param v Numeric vector. The values to compute distribution for.
    #' @return data.frame with columns x (values), y (cumulative probabilities), and count (total count).
    compute_distribution <- function(v) {
      v <- v[!is.na(v)]; if (!length(v)) return(NULL)
      fn <- ecdf(v); x <- sort(unique(v)); data.frame(x = x, y = fn(x), count = length(v))
    }
    
    #' Calculate summary statistics for a numeric vector
    #' @param v Numeric vector. The values to calculate statistics for.
    #' @return List containing various statistics: n, mean, median, sd, min, max, q1, q3.
    calc_stats <- function(v) {
      v <- v[!is.na(v)]; if (!length(v)) return(list(n = 0))
      q <- quantile(v, c(.25, .75)); list(n = length(v), mean = mean(v), median = median(v), sd = sd(v),
                                          min = min(v), max = max(v), q1 = q[1], q3 = q[2])
    }
    
    #' Create a UI element showing statistics as a formatted box
    #' @param st List. Statistics list as returned by calc_stats().
    #' @param name Character string. The name of the metric being displayed.
    #' @return shiny.tag. A UI element showing the statistics in a formatted box.
    stats_box <- function(st, name) {
      if (is.null(st) || st[["n"]] == 0) return(p("No data available"))
      
      # Create a named list for statistics
      stats_list <- list(
        Count = st[["n"]], 
        Mean = round(st[["mean"]], 2), 
        Median = round(st[["median"]], 2),
        `Std Dev` = round(st[["sd"]], 2)
      )
      
      # Create UI elements using mapply instead of lapply to get both value and name
      stats_ui <- mapply(
        function(value, stat_name) {
          div(class = "stat-box", 
              h5(stat_name), 
              p(format(value, big.mark = ",")))
        },
        stats_list,
        names(stats_list),
        SIMPLIFY = FALSE
      )
      
      tagList(
        h4(name %+% " Summary Statistics"),
        div(class = "stats-container", stats_ui)
      )
    }
    
    #' Create a plotly ECDF plot for the given data
    #' @param dat data.frame. The raw data containing the field to plot.
    #' @param field Character string. The field name in dat to visualize.
    #' @param title Character string. The title for the plot.
    #' @param btn Character string. The button ID that triggered this plot.
    #' @return plotly object. A plotly visualization of the ECDF.
    ecdf_plot <- function(dat, field, title, btn) {
      # Handle empty or invalid data
      if (is.null(dat) || nrow2(dat) == 0 || !field %in% names(dat)) {
        return(plotly::plotly_empty(type = "scatter") %>%
                 plotly::add_annotations(text = "No data available", showarrow = FALSE))
      }
      
      # Compute the distribution directly within the function
      dist_data <- compute_distribution(dat[[field]])
      
      # Handle empty distribution data
      if (is.null(dist_data) || nrow2(dist_data) == 0) {
        return(plotly::plotly_empty(type = "scatter") %>%
                 plotly::add_annotations(text = "No valid distribution data", showarrow = FALSE))
      }
      
      # Create the plot
      plotly::plot_ly(dist_data, x = ~x, y = ~y, type = "scatter", mode = "lines",
                      line = list(color = "#1F77B4", width = 2), hoverinfo = "text",
                      text = ~paste0(title, ": ", format(x, big.mark = ","),
                                     "<br>Percentage: ", format(y * 100, digits = 2), "%")) %>%
        plotly::layout(title = list(text = title %+% " - CDF", font = list(size = 16)),
                       xaxis = list(title = title),
                       yaxis = list(title = "Cumulative %", tickformat = ".0%"))
    }
    
    #' Create a plotly bar plot for the given data
    #' @param dat data.frame. The raw data containing the field to plot.
    #' @param field Character string. The field name in dat to visualize.
    #' @param title Character string. The title for the plot.
    #' @param levels Character vector. Optional levels for factor variables.
    #' @return plotly object. A plotly visualization of the bar chart.
    bar_plot <- function(dat, field, title, levels = NULL) {
      # Handle empty or invalid data
      if (is.null(dat) || nrow2(dat) == 0 || !field %in% names(dat)) {
        return(plotly::plotly_empty(type = "bar") %>%
                 plotly::add_annotations(text = "No data available", showarrow = FALSE))
      }
      
      # Create the table/counts based on the field
      if (!is.null(levels) && field %in% names(dat)) {
        # Use specified levels
        cnt <- table(factor(dat[[field]], levels = levels))
      } else {
        # Default table
        cnt <- table(dat[[field]])
      }
      
      # Handle empty counts
      if (is.null(cnt) || !length(cnt)) {
        return(plotly::plotly_empty(type = "bar") %>%
                 plotly::add_annotations(text = "No valid count data", showarrow = FALSE))
      }
      
      # Convert to data frame for plotting
      df <- data.frame(x = names(cnt), y = as.numeric(cnt))
      total <- sum(df[["y"]])
      
      # Create the plot
      plotly::plot_ly(df, x = ~x, y = ~y, type = "bar", marker = list(color = "#1F77B4"), hoverinfo = "text",
                      text = ~paste0("Value: ", x, "<br>Count: ", format(y, big.mark = ","),
                                     "<br>Percentage: ", round(y / total * 100, 1), "%")) %>%
        plotly::layout(title = list(text = title, font = list(size = 16)),
                       xaxis = list(title = "Value"), yaxis = list(title = "Count"))
    }
    
    # ------------ 資料存取輔助 ----------------------------------------------
    #' Debug logging for data verification
    #' 
    #' Prints sample data for debugging and monitoring
    observe({
      # Get base data
      dat <- df_dna_by_customer()
      if (!is.null(dat) && nrow2(dat) > 0) {
        # Print sample data for debugging
        print(paste("Loaded DNA data with", nrow2(dat), "records"))
        print(head(dat))
      }
    })
    
    # ------------ observer 工具 ----------------------------------------------
    #' Create an observer for rendering a metric visualization
    #' 
    #' Factory function that creates an observer for a specific metric button.
    #' When the button is clicked, this creates the appropriate ECDF visualization.
    #' 
    #' @param field Character string. The field name in the data to visualize (e.g., "M", "R", "F").
    #' @param btn Character string. The input ID of the button that triggers this visualization.
    #' @param ttl Character string. The title to display for this visualization.
    #' @return Observer. A Shiny observer that handles visualization when the button is clicked.
    render_metric <- function(field, btn, ttl) {
      observeEvent(input[[btn]], {
        # Update current visualization tracking
        current_visualization(btn)
        
        # Get data
        dat <- df_dna_by_customer()
        
        # Render the visualization with access to the field and button
        output$dna_distribution_plot <- renderPlotly({
          ecdf_plot(dat, field, ttl, btn)
        })
        
        # Render statistics
        output$metric_stats <- renderUI({
          if (!is.null(dat) && nrow2(dat) > 0 && field %in% names(dat)) {
            stats_box(calc_stats(dat[[field]]), ttl)
          } else {
            p("No data available")
          }
        })
      })
    }
    
    render_metric("m_value", "m_ecdf",  "Purchase Amount (M)")
    render_metric("r_value", "r_ecdf",  "Recency (R)")
    render_metric("f_value", "f_ecdf",  "Frequency (F)")
    render_metric("ipt_mean", "ipt_ecdf", "Inter‑purchase Time")
    
    # barplots
    observeEvent(input$f_barplot, {
      current_visualization("f_barplot")
      dat <- df_dna_by_customer()
      
      output$dna_distribution_plot <- renderPlotly({
        bar_plot(dat, "f_value", "Purchase Frequency Distribution")
      })
      
      output$metric_stats <- renderUI({
        if (!is.null(dat) && nrow2(dat) > 0 && "f_value" %in% names(dat)) {
          stats_box(calc_stats(dat[["f_value"]]), "Frequency (F)")
        } else {
          p("No data available")
        }
      })
    })
    
    observeEvent(input$nes_barplot, {
      current_visualization("nes_barplot")
      dat <- df_dna_by_customer()
      
      output$dna_distribution_plot <- renderPlotly({
        bar_plot(dat, "nes_status", "NES Status Distribution", levels = c("N", "E0", "S1", "S2", "S3"))
      })
      
      output$metric_stats <- renderUI({
        if (is.null(dat) || nrow2(dat) == 0 || !"nes_status" %in% names(dat)) {
          p("No data available")
        } else {
          NULL  # No stats for NES distribution
        }
      })
    })
    
    output$component_status <- renderText({
      switch(component_status(),
             idle = "Select a metric", loading = "Loading data...",
             ready = "Ready", computing = "Computing...", error = "Error", component_status())
    })
    
    # 預設載入 M
    observe({ isolate({ if (current_visualization() == "none" && !is.null(df_dna_by_customer()) && nrow2(df_dna_by_customer()) > 0)
      session$sendCustomMessage("shiny.button.click", list(id = session$ns("m_ecdf"))) }) })
    
    list(current_visualization = current_visualization,
         component_status      = component_status,
         df_dna_by_customer    = df_dna_by_customer)
  })
}

# Component wrapper -----------------------------------------------------------
#' microDNADistributionComponent
#' 
#' Implements a visualization component for customer DNA distributions (M, R, F, IPT, NES)
#' following the Connected Component principle and using plotly for visualizations.
#' 
#' @param id Character string. The module ID used for namespacing inputs and outputs.
#' @param app_data_connection Database connection object or list. The data connection supporting Enhanced Data Access pattern (R116).
#'        Can be a DBI connection, a list with getter functions, a file path, or NULL if no database access is needed.
#' @param config List or reactive expression. Configuration parameters for customizing component behavior (optional).
#'        If reactive, will be re-evaluated when dependencies change.
#' @param translate Function. Translation function for UI text elements (defaults to identity function).
#'        Should accept a string and return a translated string.
#' @return A list containing UI and server functions structured according to the Connected Component Principle (MP56).
#'         The UI element contains 'filter' and 'display' components, and the server function initializes component functionality.
#' @examples
#' # Basic usage with default settings
#' dnaComponent <- microDNADistributionComponent("dna_viz")
#' 
#' # Usage with database connection
#' dnaComponent <- microDNADistributionComponent(
#'   id = "dna_viz",
#'   app_data_connection = app_conn, 
#'   config = list(platform_id = 1)
#' )
#'
#' # Usage with file path (tbl2 enhanced access)
#' dnaComponent <- microDNADistributionComponent(
#'   id = "dna_viz",
#'   app_data_connection = "path/to/data.csv",
#'   config = list(filters = list(platform_id = 1))
#' )
#' @export
microDNADistributionComponent <- function(id, app_data_connection = NULL, config = NULL, translate = identity) {
  list(
    ui = list(filter = microDNADistributionFilterUI(id, translate),
              display = microDNADistributionDisplayUI(id)),
    server = function(input, output, session) {
      microDNADistributionServer(id, app_data_connection, config, session)
    }
  )
}

# For backwards compatibility - assigns the component function to the old name
#' @rdname microDNADistributionComponent
#' @usage microDNADistribution(id, app_data_connection = NULL, config = NULL, translate = identity)
#' @description Alias for microDNADistributionComponent. Provided for backward compatibility.
microDNADistribution <- microDNADistributionComponent