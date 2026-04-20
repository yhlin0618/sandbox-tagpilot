# Test syntax detection for tbl2 enhanced functionality
# This tests the logic that detects database.table syntax

# Initialize environment
library(DBI)
library(duckdb)
library(dplyr)

# Set working directory
setwd("/Users/che/Library/CloudStorage/Dropbox/che_workspace/projects/ai_martech/l4_enterprise/WISER")

# Initialize
needgoogledrive <- TRUE
source(".Rprofile")
autoinit()

cat("=== Testing tbl2 Syntax Detection ===\n\n")

# Create temporary databases for testing
temp_dir <- tempdir()
main_db_path <- file.path(temp_dir, "main_test.duckdb")
attached_db_path <- file.path(temp_dir, "attached_test.duckdb")

tryCatch({
  # Create main database
  main_con <- dbConnect(duckdb(), main_db_path)
  
  # Create attached database with test data
  attached_con <- dbConnect(duckdb(), attached_db_path)
  
  # Create test data in attached database
  test_data <- data.frame(
    id = 1:3,
    name = c("Alice", "Bob", "Charlie"),
    value = c(10, 20, 30)
  )
  
  dbWriteTable(attached_con, "test_table", test_data)
  dbDisconnect(attached_con)
  
  # Attach the database
  dbExecute(main_con, paste0("ATTACH '", attached_db_path, "' AS attached_db"))
  
  cat("Test 1: Verify database attachment\n")
  # Check if attached database is available
  databases <- dbGetQuery(main_con, "PRAGMA database_list")
  attached_db_found <- "attached_db" %in% databases$name
  cat("✓ Attached database found:", attached_db_found, "\n")
  
  cat("\nTest 2: Test direct SQL with attached database\n")
  # Test direct SQL access
  direct_sql_result <- dbGetQuery(main_con, "SELECT * FROM attached_db.test_table LIMIT 1")
  cat("✓ Direct SQL works:", nrow(direct_sql_result), "rows\n")
  
  cat("\nTest 3: Test tbl2 syntax detection logic\n")
  
  # Test the syntax detection logic
  test_names <- c(
    "regular_table",          # Should use regular syntax
    "attached_db.test_table", # Should use attached syntax
    "db.schema.table",        # Should use attached syntax
    "table_with_underscores"  # Should use regular syntax
  )
  
  for (test_name in test_names) {
    has_dot <- grepl("\\.", test_name)
    cat("  '", test_name, "' -> ", ifelse(has_dot, "attached syntax", "regular syntax"), "\n")
  }
  
  cat("\nTest 4: Test tbl2 with attached database syntax\n")
  
  # Test our enhanced tbl2 function
  tbl2_result <- tbl2(main_con, "attached_db.test_table") %>%
    head(2) %>%
    collect()
  
  cat("✓ tbl2 with attached syntax works:", nrow(tbl2_result), "rows\n")
  cat("  Columns:", paste(names(tbl2_result), collapse = ", "), "\n")
  
  cat("\nTest 5: Test with dplyr operations\n")
  
  # Test filtering
  filtered_result <- tbl2(main_con, "attached_db.test_table") %>%
    filter(value > 15) %>%
    collect()
  
  cat("✓ Filtering works:", nrow(filtered_result), "rows with value > 15\n")
  
  # Test selection
  selected_result <- tbl2(main_con, "attached_db.test_table") %>%
    select(name, value) %>%
    collect()
  
  cat("✓ Selection works:", ncol(selected_result), "columns selected\n")
  
  # Clean up
  dbDisconnect(main_con)
  unlink(main_db_path)
  unlink(attached_db_path)
  
  cat("\n=== All syntax detection tests passed! ===\n")
  cat("✓ tbl2 successfully supports database.table syntax\n")
  
}, error = function(e) {
  cat("❌ ERROR:", e$message, "\n")
  
  # Clean up on error
  if (exists("main_con")) {
    try(dbDisconnect(main_con), silent = TRUE)
  }
  if (exists("attached_con")) {
    try(dbDisconnect(attached_con), silent = TRUE)
  }
  try(unlink(main_db_path), silent = TRUE)
  try(unlink(attached_db_path), silent = TRUE)
})