# ====================================================================
# sc_create_mock_duckdb.R
# 
# Script to initialize a DuckDB database file in 'global_scripts/30_global_data'
# with sample tables for testing and development.
#
# NOTES:
# - This script creates a mock database with test data for development
# - The database is placed in the 30_global_data directory for shared access
# - Contains example tables: customer_profile, orders
# - Can be run directly: Rscript sc_create_mock_duckdb.R
# - Or sourced from another script: source("sc_create_mock_duckdb.R")
# ====================================================================

# --- Install and load dependencies ---
if (!requireNamespace("DBI", quietly = TRUE)) {
  install.packages("DBI")
}
if (!requireNamespace("duckdb", quietly = TRUE)) {
  install.packages("duckdb")
}
library(DBI)
library(duckdb)

# 1. 專案根目錄 (這裡假設就是 getwd())
ROOT_DIR <- getwd()

# 2. 想要放 db 的子資料夾
db_dir <- file.path(ROOT_DIR,
                    "scripts",
                    "global_scripts",
                    "30_global_data")

# 3. 如果資料夾不存在，就遞迴建立
if (!dir.exists(db_dir)) {
  dir.create(db_dir, recursive = TRUE, showWarnings = FALSE)
  message("Created directory: ", db_dir)
}

# 4. 最終的 duckdb 檔案路徑
db_path <- file.path(db_dir, "mock_data.duckdb")

# 檢查
message("DuckDB will be created at: ", db_path)
if (!dir.exists(db_dir)) {
  dir.create(db_dir, recursive = TRUE)
  message("Created directory: ", db_dir)
}

# --- Notes for Developers ---
cat("\n======== MOCK DUCKDB CREATION UTILITY ========\n")
cat("Creating database at:", db_path, "\n\n")

cat("Tables being created:\n")
cat("- customer_profile: Basic customer information\n")
cat("- orders: Sample order data\n\n")

cat("To use this database in your code:\n")
cat('con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "', db_path, '")\n\n')

cat("Usage examples:\n")
cat('1. Get customers: dbGetQuery(con, "SELECT * FROM customer_profile")\n')
cat('2. Get orders: dbGetQuery(con, "SELECT * FROM orders")\n')
cat('3. Join data: dbGetQuery(con, "SELECT c.name, o.amount FROM customer_profile c JOIN orders o ON c.id = o.customer_id")\n\n')

cat("Remember to disconnect when done: dbDisconnect(con, shutdown = TRUE)\n")
cat("=============================================\n")

# --- Create/connect to DuckDB file ---
# This will create 'mock_data.duckdb' in the 30_global_data directory
con <- dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = FALSE)

# --- Define sample data frames ---
customer_profile <- data.frame(
  id = 1:3,
  name = c("Alice", "Bob", "Charlie"),
  signup_date = as.Date(c("2021-01-01", "2021-06-15", "2021-12-31"))
)

orders <- data.frame(
  order_id = 1001:1003,
  customer_id = c(1, 2, 3),
  amount = c(99.99, 149.50, 20.00),
  order_date = Sys.Date() - c(10, 5, 1)
)

# --- Write tables to DuckDB ---
dbWriteTable(con, "customer_profile", customer_profile, overwrite = TRUE)
dbWriteTable(con, "orders", orders, overwrite = TRUE)

# --- Verify tables ---
tables <- dbListTables(con)
message("Created tables in ", db_path, ": ", paste(tables, collapse = ", "))

# --- Optional: Query a sample ---
res <- dbGetQuery(con, "SELECT * FROM customer_profile LIMIT 5;")
print(res)

# --- Disconnect and shutdown DuckDB ---
dbDisconnect(con, shutdown = TRUE)
message("Disconnected and shutdown ", db_path)

# --- Additional Information ---
cat("\n======== ADDITIONAL NOTES ========\n")
cat("The mock database has been successfully created and populated.\n\n")

cat("To extend this database in the future:\n")
cat("1. Add more sample tables by creating additional data frames\n")
cat("2. For larger datasets, consider using dbWriteTable with read.csv()\n")
cat("3. Create relationships between tables with foreign keys\n\n")

cat("For testing universal_data_accessor function:\n")
cat("- This database can be used with the universal_data_accessor function\n")
cat("- Example: universal_data_accessor(db_conn, \"customer_profile\")\n\n")

cat("For backup/versioning:\n")
cat("- Make a copy of the database file for version control\n")
cat("- Example: file.copy(\"", db_path, "\", \"", db_path, ".backup\")\n")
cat("================================\n")

