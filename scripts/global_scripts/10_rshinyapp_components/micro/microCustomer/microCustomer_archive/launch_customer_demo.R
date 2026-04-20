#!/usr/bin/env Rscript

# Simple script to directly launch the microCustomer R76 demo
# This script avoids the recursion issues with browser launching

# Source the test file in a clean environment
cat("Sourcing test script...\n")
source(file.path(
  dirname(sys.frame(1)$ofile),
  "microCustomer_test_R76.R"
), local = new.env(parent = globalenv()))

# Now we can safely launch the app
cat("Creating and launching demo app...\n")

# Set launch.browser option
options(shiny.launch.browser = TRUE)

# Create the test app (customer_test_data is defined in the sourced script)
app <- create_test_app(customer_test_data)

# Run the app without redundant launch.browser parameter
runApp(app, port = 8765)