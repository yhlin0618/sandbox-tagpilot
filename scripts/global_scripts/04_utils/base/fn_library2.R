# 函數檢查和安裝所需的套件
# 此版本依照 P106 (Performance Acceleration Principle) 可選擇性
# 在加速層級 > 0 時才向 CRAN 查詢套件清單，
# 以減少初始化延遲並配合 MP49 (Docker-Based Deployment)
# 的快速啟動需求。
# library2 <- function(..., force_update = TRUE) {
#   pkg_names <- as.character(unlist(list(...)))
#   cran_pkgs <- available.packages()
#   manual_update <- c()
#   
#   for (pkg in pkg_names) {
#     # Initial state
#     ns_loaded <- pkg %in% loadedNamespaces()
#     attached <- paste0("package:", pkg) %in% search()
#     
#     # Install if missing
#     if (!requireNamespace(pkg, quietly = TRUE)) {
#       message(sprintf("Installing missing package: %s", pkg))
#       do.call(install.packages, list(pkg, dependencies = TRUE))
#     } else {
#       # Check again in case loading happened during check
#       ns_loaded <- pkg %in% loadedNamespaces()
#       attached <- paste0("package:", pkg) %in% search()
#       installed_version <- packageVersion(pkg)
#       
#       if (pkg %in% rownames(cran_pkgs)) {
#         available_version <- package_version(cran_pkgs[pkg, "Version"])
#         needs_update <- installed_version < available_version
#         
#         if (force_update && needs_update) {
#           if (ns_loaded) {
#             warning(sprintf(
#               "⚠ Package '%s' is loaded (v%s) and cannot be auto-updated to v%s.\n➡ Please restart R and run: install.packages('%s')",
#               pkg, installed_version, available_version, pkg
#             ))
#             manual_update <- c(manual_update, pkg)
#           } else {
#             message(sprintf("Updating '%s' from version %s to %s", pkg, installed_version, available_version))
#             tryCatch({
#               do.call(install.packages, list(pkg, dependencies = TRUE))
#             }, error = function(e) {
#               warning(sprintf("❌ Failed to update '%s': %s", pkg, e$message))
#               manual_update <- c(manual_update, pkg)
#             })
#           }
#         } else {
#           message(sprintf("Package '%s' is up to date (version %s)", pkg, installed_version))
#         }
#       } else {
#         message(sprintf("Package '%s' not found on CRAN. Skipping version check.", pkg))
#       }
#     }
#     
#     # Only attach if not already attached
#     if (!attached) {
#       tryCatch({
#         library(pkg, character.only = TRUE)
#         message(sprintf("✅ Loaded package '%s' successfully.", pkg))
#       }, error = function(e) {
#         message(sprintf("❌ Failed to load '%s': %s", pkg, e$message))
#       })
#     } else {
#       message(sprintf("✅ Package '%s' already attached.", pkg))
#     }
#   }
#   
#   # Summary of packages that need manual update
#   if (length(manual_update) > 0) {
#     cat("\n⚠️ The following packages could not be auto-updated because they are currently loaded or in use:\n")
#     for (p in manual_update) {
#       cat(sprintf("   ➤ install.packages('%s')\n", p))
#     }
#     cat("🔁 Please restart R and run the above commands manually.\n")
#   }
# }


library2 <- function(...) {
  ACCELERATION_LEVEL <- get("ACCELERATION_LEVEL", envir = .GlobalEnv, inherits = TRUE)
  pkg_names <- as.character(unlist(list(...)))
  cran_pkgs <- if (ACCELERATION_LEVEL > 0) {
    available.packages()
  } else {
    NULL
  }
  manual_update <- c()
  
  for (pkg in pkg_names) {
    ns_loaded <- pkg %in% loadedNamespaces()
    attached <- paste0("package:", pkg) %in% search()
    if (!is.character(pkg) || nchar(pkg) == 0 || tolower(pkg) == "false") {
      warning(sprintf("⛔ Invalid package name: '%s'. Skipping.", pkg))
      next
    }
    
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message(sprintf("Installing missing package: %s", pkg))
      do.call(install.packages, list(pkg, dependencies = TRUE))
    } else {
      ns_loaded <- pkg %in% loadedNamespaces()
      attached <- paste0("package:", pkg) %in% search()
      installed_version <- packageVersion(pkg)
      
      if (!is.null(cran_pkgs) && pkg %in% rownames(cran_pkgs)) {
        available_version <- package_version(cran_pkgs[pkg, "Version"])
        needs_update <- installed_version < available_version

        if (ACCELERATION_LEVEL > 0 && needs_update) {
          if (ns_loaded) {
            warning(sprintf(
              "⚠ Package '%s' is loaded (v%s) and cannot be auto-updated to v%s.\n➡ Please restart R and run: install.packages('%s')",
              pkg, installed_version, available_version, pkg
            ))
            manual_update <- c(manual_update, pkg)
          } else {
            message(sprintf("Updating '%s' from version %s to %s", pkg, installed_version, available_version))
            tryCatch({
              do.call(install.packages, list(pkg, dependencies = TRUE))
            }, error = function(e) {
              warning(sprintf("❌ Failed to update '%s': %s", pkg, e$message))
              manual_update <- c(manual_update, pkg)
            })
          }
        } else {
          message(sprintf("Package '%s' is up to date (version %s)", pkg, installed_version))
        }
      } else if (!is.null(cran_pkgs)) {
        message(sprintf("Package '%s' not found on CRAN. Skipping version check.", pkg))
      }
    }
    
    if (!attached) {
      tryCatch({
        library(pkg, character.only = TRUE)
        message(sprintf("✅ Loaded package '%s' successfully.", pkg))
      }, error = function(e) {
        message(sprintf("❌ Failed to load '%s': %s", pkg, e$message))
      })
    } else {
      message(sprintf("✅ Package '%s' already attached.", pkg))
    }
  }
  
  if (length(manual_update) > 0) {
    cat("\n⚠️ The following packages could not be auto-updated because they are currently loaded or in use:\n")
    for (p in manual_update) {
      cat(sprintf("   ➤ install.packages('%s')\n", p))
    }
    cat("🔁 Please restart R and run the above commands manually.\n")
  }
}



# 使用範例：
# library2("dplyr", "ggplot2", "readr")
