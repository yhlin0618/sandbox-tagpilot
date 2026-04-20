# run_demo.R - Simple script to run the microCustomer_R76 demo directly
# This script launches the microCustomer demo in a browser without requiring interactive mode

# Source the test script (which has all the data and functions defined)
source(paste0(
  dirname(sys.frame(1)$ofile), 
  "/microCustomer_test_R76.R"
))

# Set browser launch option without custom browser function
# This avoids the infinite recursion problem
options(shiny.launch.browser = TRUE)

# Create the demo app
message("Launching microCustomer_R76 demo app...")
app <- create_test_app(customer_test_data)

# Run the app but DO NOT set both launch.browser and options(shiny.launch.browser)
# as this causes infinite recursion
runApp(app, port = 8765)