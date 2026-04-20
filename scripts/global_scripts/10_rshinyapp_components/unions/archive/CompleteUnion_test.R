# @principle MP55 Computation Allocation
# @principle MP56 Connected Component Principle
# @principle R91 Universal Data Access Pattern
# @principle P22 CSS Component Display Controls
# @principle MP52 Unidirectional Data Flow
# @principle MP24 Natural SQL Language
# @principle MP27 Specialized Natural SQL Language

# Comprehensive example demonstrating the full potential of union components
# This example creates a complete dashboard with:
# 1. Sidebar with UnionFiltersUI (filter components) 
# 2. Main UI with unionUI (content components)
# 3. Server with synchronized components
# 4. Navbar-based navigation

# NSQL DATA FLOW DESCRIPTION:
# 
# FROM INPUT.navbarMenu
# SELECT tab_selection
# STORE IN reactive::active_tab;
# 
# FROM reactive::active_tab
# JOIN WITH (SELECT all_tabs FROM components::unionUI)
# WHERE tab_name = reactive::active_tab
# UPDATE visibility = TRUE
# UPDATE visibility = FALSE WHERE tab_name != reactive::active_tab;
# 
# FROM INPUT.navbarMenu
# SELECT tab_selection 
# CALL filter_union_state.toggle_component
# WITH (tab = tab_selection, show = TRUE);
# 
# FROM INPUT.navbarMenu
# SELECT tab_selection
# FOR EACH IN enum_tabs
# WHERE tab_name = tab_selection
#   UPDATE DOM.sidebar_filters.tab_name.style.display = "block"
# ELSE
#   UPDATE DOM.sidebar_filters.tab_name.style.display = "none"
# END FOR;
# 
# FROM reactive::active_tab
# STORE IN reactive::current_page
# FOR status_tracking;

init_script_path <- file.path("update_scripts", "global_scripts", "00_principles", 
                              "sc_initialization_app_mode.R")
source(init_script_path)


library(shiny)
library(bs4Dash)
library(shinyjs)
library(dplyr)
library(ggplot2)

# Source required components
source("update_scripts/global_scripts/10_rshinyapp_components/unions/ComponentsUnion.R")
source("update_scripts/global_scripts/10_rshinyapp_components/unions/unionUI.R")
source("update_scripts/global_scripts/10_rshinyapp_components/micro/microCustomer/microCustomer2.R")

# Main example function
CompleteUnionExample <- function() {
  
  # === PREPARE MOCK DATA ===
  # Create sample data for microCustomer2
  sample_dna_data <- data.frame(
    customer_id = 1:10,
    time_first = Sys.Date() - sample(30:500, 10),
    time_first_to_now = sample(30:500, 10),
    r_label = sample(c("極近", "近期", "一般", "久遠", "非常久遠"), 10, replace = TRUE),
    r_value = sample(1:100, 10),
    f_label = sample(c("極低", "低", "一般", "高", "非常高"), 10, replace = TRUE),
    f_value = sample(1:20, 10),
    m_label = sample(c("極低", "低", "一般", "高", "非常高"), 10, replace = TRUE),
    m_value = round(runif(10, 100, 10000), 2),
    cai_label = sample(c("不活躍", "低度活躍", "一般活躍", "活躍", "非常活躍"), 10, replace = TRUE),
    cai = round(runif(10, 0, 1), 2),
    ipt_mean = round(runif(10, 5, 60), 1),
    pcv = round(runif(10, 100, 5000), 2),
    clv = round(runif(10, 1000, 20000), 2),
    cri = round(runif(10, 0, 1), 2),
    nes_status = sample(c("新客", "主力", "休眠", "流失"), 10, replace = TRUE),
    nt = round(runif(10, 50, 500), 2),
    e0t = round(runif(10, 100, 1000), 2)
  )
  
  sample_customer_profiles <- data.frame(
    customer_id = 1:10,
    buyer_name = paste0("Customer ", LETTERS[1:10]),
    email = paste0("customer", 1:10, "@example.com"),
    phone = paste0("0912345", sprintf("%03d", 1:10)),
    address = paste0("Address ", 1:10)
  )
  
  sample_products <- data.frame(
    product_id = 1:20,
    product_name = paste0("Product ", 1:20),
    category = sample(c("Electronics", "Clothing", "Food", "Home"), 20, replace = TRUE),
    price = round(runif(20, 10, 1000), 2),
    stock = sample(0:100, 20, replace = TRUE)
  )
  
  sample_sales <- data.frame(
    sale_id = 1:100,
    customer_id = sample(1:10, 100, replace = TRUE),
    product_id = sample(1:20, 100, replace = TRUE),
    quantity = sample(1:5, 100, replace = TRUE),
    date = sample(seq(Sys.Date() - 365, Sys.Date(), by = "day"), 100, replace = TRUE)
  )
  
  # Create a mock app_data_connection
  app_data_connection <- list(
    dna_by_customer = function() { sample_dna_data },
    customer_profile = function() { sample_customer_profiles },
    products = function() { sample_products },
    sales = function() { sample_sales }
  )
  
  # === CREATE FILTER COMPONENTS FOR SIDEBAR ===
  
  # 1. RFM Customer Filter
  rfm_component <- microCustomer2Initialize(
    id = "rfm_customer",
    app_data_connection = app_data_connection,
    include_fields = c("recency", "frequency", "monetary")
  )
  
  # 2. Value Metrics Customer Filter
  value_component <- microCustomer2Initialize(
    id = "value_customer",
    app_data_connection = app_data_connection,
    include_fields = c("pcv", "clv", "cri")
  )
  
  # 3. Date Range Filter
  date_filter <- div(
    h4("Date Range", style = "color: #3c8dbc;"),
    dateRangeInput(
      "date_range", 
      "Select Period:", 
      start = Sys.Date() - 30, 
      end = Sys.Date()
    ),
    checkboxInput(
      "compare_previous",
      "Compare with previous period",
      value = FALSE
    )
  )
  
  # 4. Product Filter
  product_filter <- div(
    h4("Product Filter", style = "color: #3c8dbc;"),
    selectInput(
      "product_category", 
      "Product Category:", 
      choices = c("All Categories" = "all", 
                 "Electronics" = "Electronics", 
                 "Clothing" = "Clothing", 
                 "Food" = "Food",
                 "Home" = "Home"),
      selected = "all"
    ),
    selectizeInput(
      "product_ids",
      "Products:",
      choices = NULL,
      multiple = TRUE,
      options = list(
        placeholder = "Select products...",
        onInitialize = I('function() { this.setValue(""); }')
      )
    )
  )
  
  # 5. Region Filter
  region_filter <- div(
    h4("Region Filter", style = "color: #3c8dbc;"),
    selectInput(
      "region", 
      "Select Region:", 
      choices = c("All Regions" = "all", "North America" = "na", "Europe" = "eu", "Asia" = "as"),
      selected = "all"
    ),
    checkboxGroupInput(
      "sub_regions",
      "Sub-regions:",
      choices = c("East", "West", "North", "South"),
      selected = character(0)
    )
  )
  
  # Common filter settings - always visible
  common_filters <- div(
    h4("Global Settings", style = "color: #666; border-bottom: 1px solid #eee; padding-bottom: 5px;"),
    radioButtons(
      "time_granularity",
      "Time Granularity:",
      choices = c("Daily", "Weekly", "Monthly"),
      selected = "Weekly"
    ),
    sliderInput(
      "value_threshold",
      "Value Threshold:",
      min = 0,
      max = 10000,
      value = 1000,
      step = 100
    )
  )
  
  # === CREATE MAIN CONTENT COMPONENTS ===
  
  # 1. RFM Analysis Content
  rfm_content <- div(
    h2("RFM Analysis Dashboard"),
    fluidRow(
      column(
        width = 12,
        # RFM microCustomer2 display
        rfm_component$ui$display
      )
    ),
    fluidRow(
      column(
        width = 6,
        bs4Dash::box(
          title = "RFM Distribution",
          width = NULL,
          status = "primary",
          solidHeader = TRUE,
          plotOutput("rfm_distribution_plot", height = "300px")
        )
      ),
      column(
        width = 6,
        bs4Dash::box(
          title = "RFM Segments",
          width = NULL,
          status = "info",
          solidHeader = TRUE,
          plotOutput("rfm_segments_plot", height = "300px")
        )
      )
    )
  )
  
  # 2. Value Analysis Content
  value_content <- div(
    h2("Customer Value Analysis"),
    fluidRow(
      column(
        width = 12,
        # Value microCustomer2 display
        value_component$ui$display
      )
    ),
    fluidRow(
      column(
        width = 6,
        bs4Dash::box(
          title = "CLV Distribution",
          width = NULL,
          status = "success",
          solidHeader = TRUE,
          plotOutput("clv_distribution_plot", height = "300px")
        )
      ),
      column(
        width = 6,
        bs4Dash::box(
          title = "Value Segments",
          width = NULL,
          status = "warning",
          solidHeader = TRUE,
          plotOutput("value_segments_plot", height = "300px")
        )
      )
    )
  )
  
  # 3. Time Series Analysis Content
  time_series_content <- div(
    h2("Time Series Analysis"),
    fluidRow(
      column(
        width = 12,
        bs4Dash::box(
          title = "Sales Trend",
          width = NULL,
          status = "info",
          solidHeader = TRUE,
          plotOutput("time_series_plot", height = "250px")
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        bs4Dash::box(
          title = "Seasonality",
          width = NULL,
          status = "primary",
          solidHeader = TRUE,
          plotOutput("seasonality_plot", height = "250px")
        )
      ),
      column(
        width = 6,
        bs4Dash::box(
          title = "Comparative Analysis",
          width = NULL,
          status = "warning",
          solidHeader = TRUE,
          plotOutput("comparative_plot", height = "250px")
        )
      )
    )
  )
  
  # 4. Product Analysis Content
  product_content <- div(
    h2("Product Analysis"),
    fluidRow(
      column(
        width = 12,
        bs4Dash::box(
          title = "Product Performance",
          width = NULL,
          status = "success",
          solidHeader = TRUE,
          plotOutput("product_performance_plot", height = "250px")
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        bs4Dash::box(
          title = "Category Breakdown",
          width = NULL,
          status = "primary",
          solidHeader = TRUE,
          plotOutput("category_breakdown_plot", height = "250px")
        )
      ),
      column(
        width = 6,
        bs4Dash::box(
          title = "Product Associations",
          width = NULL,
          status = "danger",
          solidHeader = TRUE,
          plotOutput("product_associations_plot", height = "250px")
        )
      )
    )
  )
  
  # 5. Region Analysis Content
  region_content <- div(
    h2("Regional Analysis"),
    fluidRow(
      column(
        width = 12,
        box(
          title = "Regional Sales Map",
          width = NULL,
          status = "danger",
          solidHeader = TRUE,
          plotOutput("region_map_plot", height = "400px")
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        box(
          title = "Regional Comparison",
          width = NULL,
          status = "info",
          solidHeader = TRUE,
          plotOutput("region_comparison_plot", height = "250px")
        )
      ),
      column(
        width = 6,
        box(
          title = "Regional Trends",
          width = NULL,
          status = "success", 
          solidHeader = TRUE,
          plotOutput("region_trends_plot", height = "250px")
        )
      )
    )
  )
  
  # === INITIALIZE UNIONS ===
  
  # 1. Filter Union for Sidebar
  filters_union <- UnionComponentsInitialize(
    id = "sidebar_filters",
    filters = list(
      rfm = rfm_component$ui$filter,
      value = value_component$ui$filter,
      date = date_filter,
      product = product_filter,
      region = region_filter
    ),
    initial_tab = "rfm",
    container_type = "sidebar",
    container_style = NULL,  # Use default
    title = "Analysis Filters",
    common_filters = common_filters
  )
  
  # 2. Content Union for Main UI
  main_content_components <- list(
    rfm = rfm_content,
    value = value_content,
    time = time_series_content,
    product = product_content,
    region = region_content
  )
  
  main_content_initial_visibility <- list(
    rfm = TRUE,
    value = FALSE,
    time = FALSE,
    product = FALSE,
    region = FALSE
  )
  
  # === CREATE THE UI ===
  ui <- bs4Dash::dashboardPage(
    title = "Comprehensive Union Example",
    dark = FALSE,
    
    # Header
    header = bs4Dash::dashboardHeader(
      title = bs4Dash::dashboardBrand(
        title = "Union Components Demo",
        color = "primary"
      ),
      # Add navbar menu for navigation
      bs4Dash::navbarMenu(
        id = "navbar_menu",
        bs4Dash::navbarTab(tabName = "rfm", text = "RFM Analysis"),
        bs4Dash::navbarTab(tabName = "value", text = "Value Analysis"),
        bs4Dash::navbarTab(tabName = "time", text = "Time Series"),
        bs4Dash::navbarTab(tabName = "product", text = "Product Analysis"),
        bs4Dash::navbarTab(tabName = "region", text = "Regional Analysis")
      ),
      # Default sidebar toggle will be included automatically
      leftUi = NULL
    ),
    
    # Sidebar with filter union
    sidebar = filters_union$ui,
    
    # Body content with UI union
    body = bs4Dash::dashboardBody(
      shinyjs::useShinyjs(),
      
      # Main content union using the unionUI component
      unionUI(
        rfm = main_content_components$rfm,
        value = main_content_components$value,
        time = main_content_components$time, 
        product = main_content_components$product,
        region = main_content_components$region,
        id = "main_content",
        initial_visibility = main_content_initial_visibility
      ),
      
      # Status footer
      fluidRow(
        column(
          width = 12,
          bs4Dash::box(
            title = "System Status",
            width = 12,
            status = "secondary",
            collapsed = TRUE,
            collapsible = TRUE,
            
            # Show active components
            fluidRow(
              column(
                width = 6,
                h4("Active Filters"),
                verbatimTextOutput("active_filters")
              ),
              column(
                width = 6,
                h4("Active Content"),
                verbatimTextOutput("active_content")
              )
            )
          )
        )
      )
    )
  )
  
  # === SERVER LOGIC ===
  server <- function(input, output, session) {
    # ======== UNION SERVER COMPONENTS ========
    
    # 1. Initialize Filter Union Server
    filter_union_state <- filters_union$server(input, output, session)
    
    # 2. Create reactive values to control content visibility
    active_tab <- reactiveVal("rfm")
    
    # Dynamic visibility conditions based on active tab
    content_visibility_conditions <- list(
      rfm = reactive({ active_tab() == "rfm" }),
      value = reactive({ active_tab() == "value" }),
      time = reactive({ active_tab() == "time" }),
      product = reactive({ active_tab() == "product" }),
      region = reactive({ active_tab() == "region" })
    )
    
    # Use unionServer (from unionUI.R) for main content
    active_content <- unionServer(
      "main_content",
      visibility_conditions = content_visibility_conditions
    )
    
    # ======== COMPONENT SERVERS ========
    
    # Initialize microCustomer2 components
    rfm_data <- rfm_component$server(input, output, session)
    value_data <- value_component$server(input, output, session)
    
    # ======== SYNCHRONIZE FILTERS AND CONTENT ========
    
    # Create a reactive expression to track which content is currently active
    current_page <- reactiveVal("rfm")
    
    # ======== NAVIGATION OBSERVERS ========
    
    # Navbar menu observer - single point of navigation control
    observeEvent(input$navbar_menu, {
      selected_tab <- input$navbar_menu
      
      # Print for debugging
      cat("Navbar selection changed to:", selected_tab, "\n")
      
      # 1. Update the active tab for content visibility
      active_tab(selected_tab)
      
      # 2. Update the current page tracked in the app
      current_page(selected_tab)
      
      # 3. Show the corresponding filter in the sidebar
      result <- filter_union_state$toggle_component(selected_tab, show = TRUE)
      cat("Filter toggle result:", result, "\n")
      
      # 4. Force CSS visibility for the component
      shinyjs::runjs(sprintf('$("#sidebar_filters-filter_%s").css("display", "block");', selected_tab))
      
      # 5. Hide all other filters
      for (name in c("rfm", "value", "date", "product", "region")) {
        if (name != selected_tab) {
          shinyjs::runjs(sprintf('$("#sidebar_filters-filter_%s").css("display", "none");', name))
        }
      }
      
      # 6. Update the navbar selected state if needed
      bs4Dash::updateNavbarTabs(session, "navbar_menu", selected = selected_tab)
    })
    
    # ======== PLOTS AND OUTPUTS ========
    
    # 1. RFM PLOTS
    output$rfm_distribution_plot <- renderPlot({
      # Generate a histogram of RFM scores
      ggplot(sample_dna_data, aes(x = m_value)) +
        geom_histogram(fill = "steelblue", bins = 30) +
        labs(title = "Distribution of Monetary Values", x = "Value", y = "Count") +
        theme_minimal()
    })
    
    output$rfm_segments_plot <- renderPlot({
      # Create a sample RFM segment plot
      segments <- data.frame(
        segment = c("Champions", "Loyal", "Potential", "New", "Promising", "Need Attention", "Dormant", "Lost"),
        count = sample(5:50, 8)
      )
      
      ggplot(segments, aes(x = reorder(segment, -count), y = count)) +
        geom_bar(stat = "identity", fill = "darkblue") +
        labs(title = "Customer Segments by RFM", x = "", y = "Customer Count") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    # 2. VALUE PLOTS
    output$clv_distribution_plot <- renderPlot({
      # Generate a histogram of CLV values
      ggplot(sample_dna_data, aes(x = clv)) +
        geom_histogram(fill = "darkgreen", bins = 20) +
        labs(title = "Customer Lifetime Value Distribution", x = "CLV", y = "Count") +
        theme_minimal()
    })
    
    output$value_segments_plot <- renderPlot({
      # Create a sample value segment plot
      value_segments <- data.frame(
        segment = c("High Value", "Medium Value", "Low Value"),
        count = c(15, 35, 50)
      )
      
      ggplot(value_segments, aes(x = "", y = count, fill = segment)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        labs(title = "Customer Value Segments", fill = "Segment") +
        theme_minimal() +
        theme(axis.text = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank())
    })
    
    # 3. TIME SERIES PLOTS
    output$time_series_plot <- renderPlot({
      # Create a sample time series plot
      dates <- seq(as.Date(input$date_range[1]), as.Date(input$date_range[2]), by = "day")
      values <- cumsum(rnorm(length(dates), mean = 5, sd = 10))
      ts_data <- data.frame(date = dates, value = values)
      
      ggplot(ts_data, aes(x = date, y = value)) +
        geom_line(color = "steelblue") +
        geom_smooth(method = "loess", se = FALSE, color = "red") +
        labs(title = "Sales Trend Over Time", x = "Date", y = "Sales Value") +
        theme_minimal()
    })
    
    output$seasonality_plot <- renderPlot({
      # Create a sample seasonality plot
      months <- factor(month.abb, levels = month.abb)
      seasonal_values <- c(20, 25, 30, 35, 45, 60, 80, 75, 60, 45, 30, 25)
      season_data <- data.frame(month = months, value = seasonal_values)
      
      ggplot(season_data, aes(x = month, y = value, group = 1)) +
        geom_line(color = "darkblue") +
        geom_point(color = "darkblue", size = 3) +
        labs(title = "Monthly Seasonality", x = "", y = "Average Sales") +
        theme_minimal()
    })
    
    output$comparative_plot <- renderPlot({
      # Create a sample comparative plot
      if (input$compare_previous) {
        # Show comparison data
        months <- rep(factor(month.abb[1:6], levels = month.abb[1:6]), 2)
        period <- rep(c("Current", "Previous"), each = 6)
        values <- c(sample(30:80, 6), sample(20:60, 6))
        comp_data <- data.frame(month = months, period = period, value = values)
        
        ggplot(comp_data, aes(x = month, y = value, fill = period)) +
          geom_bar(stat = "identity", position = "dodge") +
          labs(title = "Period Comparison", x = "", y = "Value") +
          scale_fill_manual(values = c("Current" = "steelblue", "Previous" = "lightblue")) +
          theme_minimal()
      } else {
        # Show single period data
        months <- factor(month.abb[1:6], levels = month.abb[1:6])
        values <- sample(30:80, 6)
        comp_data <- data.frame(month = months, value = values)
        
        ggplot(comp_data, aes(x = month, y = value)) +
          geom_bar(stat = "identity", fill = "steelblue") +
          labs(title = "Current Period Only", x = "", y = "Value") +
          theme_minimal() +
          annotate("text", x = 3, y = 40, 
                  label = "Enable 'Compare with previous period'\nto see comparison", 
                  color = "darkgray")
      }
    })
    
    # 4. PRODUCT PLOTS
    output$product_performance_plot <- renderPlot({
      # Filter products based on selection
      if (!is.null(input$product_ids) && length(input$product_ids) > 0) {
        # Filter to selected products
        filtered_products <- sample_products %>%
          filter(product_id %in% as.numeric(input$product_ids))
      } else if (input$product_category != "all") {
        # Filter by category
        filtered_products <- sample_products %>%
          filter(category == input$product_category)
      } else {
        # Use all products but limit to top 10 by price
        filtered_products <- sample_products %>%
          arrange(desc(price)) %>%
          head(10)
      }
      
      # Create product performance plot
      ggplot(filtered_products, aes(x = reorder(product_name, price), y = price)) +
        geom_bar(stat = "identity", fill = "orange") +
        coord_flip() +
        labs(title = "Product Price Comparison", x = "", y = "Price") +
        theme_minimal()
    })
    
    output$category_breakdown_plot <- renderPlot({
      # Create category breakdown plot
      category_data <- sample_products %>%
        group_by(category) %>%
        summarize(count = n(), average_price = mean(price))
      
      ggplot(category_data, aes(x = "", y = count, fill = category)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        labs(title = "Products by Category", fill = "Category") +
        theme_minimal() +
        theme(axis.text = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank())
    })
    
    output$product_associations_plot <- renderPlot({
      # Create a simple association plot (simplified for demo)
      set.seed(123)
      products <- sample_products$product_name[1:5]
      associations <- expand.grid(product1 = products, product2 = products)
      associations <- associations %>% 
        filter(product1 != product2) %>%
        mutate(strength = runif(n(), 0, 1))
      
      ggplot(associations, aes(x = product1, y = product2, fill = strength)) +
        geom_tile() +
        scale_fill_gradient(low = "white", high = "darkred") +
        labs(title = "Product Association Strength", x = "", y = "") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    # 5. REGION PLOTS
    output$region_map_plot <- renderPlot({
      # Simple placeholder for a map visualization
      regions <- c("North America", "Europe", "Asia", "South America", "Africa", "Australia")
      values <- c(45, 30, 35, 20, 15, 10)
      map_data <- data.frame(region = regions, value = values)
      
      ggplot(map_data, aes(x = reorder(region, -value), y = value, fill = value)) +
        geom_bar(stat = "identity") +
        scale_fill_gradient(low = "lightblue", high = "darkblue") +
        labs(title = "Sales by Region (Map Placeholder)", x = "", y = "Sales Value") +
        theme_minimal() +
        theme(legend.position = "none")
    })
    
    output$region_comparison_plot <- renderPlot({
      # Create region comparison plot based on selection
      if (input$region != "all") {
        # Show breakdown for selected region
        subregions <- c("East", "West", "North", "South")
        values <- sample(10:50, 4)
        region_data <- data.frame(subregion = subregions, value = values)
        
        ggplot(region_data, aes(x = subregion, y = value, fill = subregion)) +
          geom_bar(stat = "identity") +
          labs(title = paste0(input$region, " Subregions"), x = "", y = "Value") +
          theme_minimal() +
          theme(legend.position = "none")
      } else {
        # Show all regions
        regions <- c("North America", "Europe", "Asia")
        values <- c(45, 30, 35)
        region_data <- data.frame(region = regions, value = values)
        
        ggplot(region_data, aes(x = region, y = value, fill = region)) +
          geom_bar(stat = "identity") +
          labs(title = "Region Comparison", x = "", y = "Value") +
          theme_minimal() +
          theme(legend.position = "none")
      }
    })
    
    output$region_trends_plot <- renderPlot({
      # Create region trends plot
      months <- factor(month.abb[1:6], levels = month.abb[1:6])
      regions <- if (input$region != "all") {
        # Use subregions if a region is selected
        selected_subregions <- input$sub_regions
        if (length(selected_subregions) == 0) {
          c("East", "West") # Default if none selected
        } else {
          selected_subregions
        }
      } else {
        c("North America", "Europe", "Asia")
      }
      
      # Create trend data
      trend_data <- expand.grid(month = months, region = regions)
      trend_data$value <- runif(nrow(trend_data), 10, 50)
      
      ggplot(trend_data, aes(x = month, y = value, color = region, group = region)) +
        geom_line() +
        geom_point() +
        labs(title = "Regional Trends Over Time", x = "", y = "Value") +
        theme_minimal()
    })
    
    # ======== PRODUCT SELECTION DYNAMIC UPDATES ========
    
    # Update product selection based on category
    observe({
      selected_category <- input$product_category
      
      products <- if (selected_category == "all") {
        sample_products
      } else {
        sample_products %>% filter(category == selected_category)
      }
      
      # Create choices for product IDs
      product_choices <- setNames(
        as.character(products$product_id),
        paste0(products$product_name, " ($", products$price, ")")
      )
      
      # Update the selectize input
      updateSelectizeInput(
        session,
        "product_ids",
        choices = product_choices,
        selected = character(0),
        server = TRUE
      )
    })
    
    # ======== STATUS OUTPUTS ========
    
    # Display active filters
    output$active_filters <- renderPrint({
      list(
        visible_filters = filter_union_state$visible_components(),
        common_settings = list(
          time_granularity = input$time_granularity,
          value_threshold = input$value_threshold
        )
      )
    })
    
    # Display active content
    output$active_content <- renderPrint({
      list(
        active_page = current_page(),
        content_state = active_content()
      )
    })
  }
  
  # Start the Shiny app
  shinyApp(ui, server)
}

# Execute the example application when this script is sourced
if (interactive()) {
  CompleteUnionExample()
}