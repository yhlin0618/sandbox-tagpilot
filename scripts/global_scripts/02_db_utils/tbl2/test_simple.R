# Simple test for tbl2 attached database functionality
# Creates test databases to verify the enhanced tbl2 function

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

cat("=== Simple tbl2 Attached Database Test ===\n\n")

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
    id = 1:5,
    name = c("Alice", "Bob", "Charlie", "David", "Eve"),
    product_line_id = c("jew", "sop", "jew", "sop", "jew"),
    included_competiter = c(TRUE, FALSE, TRUE, FALSE, TRUE)
  )
  
  dbWriteTable(attached_con, "test_table", test_data)
  dbDisconnect(attached_con)
  
  # Attach the database
  dbExecute(main_con, paste0("ATTACH '", attached_db_path, "' AS attached_db"))
  
  cat("Test 1: Verify database attachment\n")
  attached_tables <- dbGetQuery(main_con, "PRAGMA table_list('attached_db')")
  cat("✓ Found", nrow(attached_tables), "tables in attached_db\n")
  
  cat("\nTest 2: Test regular tbl2 functionality\n")
  # First, copy table to main database for regular test
  dbExecute(main_con, "CREATE TABLE main_table AS SELECT * FROM attached_db.test_table")
  
  regular_result <- tbl2(main_con, "main_table") %>%
    head(3) %>%
    collect()
  
  cat("✓ Regular tbl2 works:", nrow(regular_result), "rows\n")
  
  cat("\nTest 3: Test enhanced tbl2 with attached database syntax\n")
  
  # Test the enhanced tbl2 with attached database
  attached_result <- tbl2(main_con, "attached_db.test_table") %>%
    head(3) %>%
    collect()
  
  cat("✓ Enhanced tbl2 works:", nrow(attached_result), "rows\n")
  
  cat("\nTest 4: Test with dplyr operations\n")
  
  # Test filtering with attached database
  filtered_result <- tbl2(main_con, "attached_db.test_table") %>%
    filter(included_competiter == TRUE) %>%
    collect()
  
  cat("✓ Filtering works:", nrow(filtered_result), "competitor records\n")
  
  # Test grouping
  grouped_result <- tbl2(main_con, "attached_db.test_table") %>%
    group_by(product_line_id) %>%
    summarise(count = n()) %>%
    collect()
  
  cat("✓ Grouping works:\n")
  for (i in seq_len(nrow(grouped_result))) {
    cat("  ", grouped_result$product_line_id[i], ":", grouped_result$count[i], "\n")
  }
  
  cat("\nTest 5: Test syntax detection\n")
  
  # Test that our detection works correctly
  test_strings <- c(
    "regular_table",
    "database.table",
    "db.schema.table",
    "table_with_underscores"
  )
  
  for (test_str in test_strings) {
    has_dot <- grepl("\\.", test_str)
    cat("  '", test_str, "' -> ", ifelse(has_dot, "attached syntax", "regular syntax"), "\n")
  }
  
  # Clean up
  dbDisconnect(main_con)
  unlink(main_db_path)
  unlink(attached_db_path)
  
  cat("\n=== All tests passed successfully! ===\n")
  
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