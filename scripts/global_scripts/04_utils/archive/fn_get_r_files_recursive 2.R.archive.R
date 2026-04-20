#' @title Enhanced recursive file finder 
#' @description Implements R33 Recursive Sourcing Rule to ensure all components in subdirectories are properly loaded
#' @param dir_path The directory path to search
#' @param include_pattern Regex pattern for files to include (default: ".R$")
#' @param exclude_pattern Regex pattern for files to exclude (default: "test\\.R$")
#' @param max_depth Maximum recursion depth (default: Inf)
#' @return Character vector of file paths
#' @export
get_r_files_recursive <- function(dir_path, 
                                  include_pattern = "\\.R$", 
                                  exclude_pattern = "test\\.R$", 
                                  max_depth = Inf) {
  
  # Check if directory exists
  if (!dir.exists(dir_path)) {
    warning("Directory does not exist: ", dir_path)
    return(character(0))
  }
  
  # Get all files and directories
  all_contents <- list.files(dir_path, full.names = TRUE, recursive = FALSE, 
                             include.dirs = TRUE, all.files = FALSE)
  
  # Split into files and directories
  is_dir <- dir.exists(all_contents)
  files <- all_contents[!is_dir]
  subdirs <- all_contents[is_dir]
  
  # Filter files based on include pattern
  if (!is.null(include_pattern)) {
    files <- files[grepl(include_pattern, basename(files))]
  }
  
  # Filter files based on exclude pattern
  if (!is.null(exclude_pattern)) {
    files <- files[!grepl(exclude_pattern, basename(files))]
  }
  
  # Recursively process subdirectories if not at max depth
  if (length(subdirs) > 0 && max_depth > 1) {
    subdir_files <- lapply(subdirs, function(subdir) {
      get_r_files_recursive(subdir, 
                          include_pattern = include_pattern,
                          exclude_pattern = exclude_pattern,
                          max_depth = max_depth - 1)
    })
    
    # Combine all files
    files <- c(files, unlist(subdir_files))
  }
  
  return(files)
}