#' Root Path Configuration for Precision Marketing Projects
#'
#' This script detects and sets the appropriate root path based on the current environment.
#' It sources all required functions and initializes the ROOT_PATH variable.
#'
#' @author Precision Marketing Team
#' @date 2025-04-07

# Source all function files from the functions directory
function_dir <- file.path(dirname(sys.frame(1)$ofile), "functions")
function_files <- list.files(function_dir, pattern = "\\.R$", full.names = TRUE)

# Source the detect_root_path function first
detect_path_file <- grep("detect_root_path\\.R$", function_files, value = TRUE)
if (length(detect_path_file) > 0) {
  source(detect_path_file)
}

# Set the root path using the detect_root_path function
ROOT_PATH <- detect_root_path()

# Source all other function files
other_files <- function_files[!function_files %in% detect_path_file]
for (file in other_files) {
  source(file)
}

# Export the path for bash scripts
write_root_path_for_bash()