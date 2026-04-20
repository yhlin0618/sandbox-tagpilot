## ----------------------  .Rprofile  ----------------------
## This file is named sc_Rprofile.R instead of .Rprofile to ensure Git tracking.
## To use this file, copy it to .Rprofile in your project root or working directory.
##
## Usage:
## cp sc_Rprofile.R ~/.Rprofile    # for global use
## cp sc_Rprofile.R ./.Rprofile    # for project-specific use
##

## ❶ 私有環境（存放全部狀態與常數） --------------------------
.InitEnv <- new.env(parent = baseenv())
.InitEnv$mode <- NULL # 目前 OPERATION_MODE

# ## 掛到搜尋路徑最前（名稱維持 .autoinit_env） ----------------
# if (!".autoinit_env" %in% search()) attach(.InitEnv, name = ".autoinit_env")
# search()

## ❷ 共用工具函式存進 .InitEnv -------------------------------
.InitEnv$detect_script_path <- function() {
  for (i in rev(seq_len(sys.nframe()))) {
    f <- tryCatch(sys.frame(i)$ofile, error = function(e) NULL)
    if (!is.null(f) && nzchar(f)) {
      return(normalizePath(f))
    }
  }
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
    p <- rstudioapi::getActiveDocumentContext()$path
    if (nzchar(p)) {
      return(normalizePath(p))
    }
  }
  ca <- commandArgs(trailingOnly = FALSE)
  fi <- sub("^--file=", "", ca[grep("^--file=", ca)])
  if (length(fi) == 1) {
    return(normalizePath(fi))
  }
  ""
}

.InitEnv$get_mode <- function(path) {
  parent <- tolower(basename(dirname(path)))
  if (identical(parent, "update_scripts")) {
    return("UPDATE_MODE")
  }
  if (identical(parent, "global_scripts")) {
    return("GLOBAL_MODE")
  }
  "APP_MODE"
}

## ❸ 初始化函式（存放於 .InitEnv） ----------------------------
.InitEnv$autoinit <- function() {
  .InitEnv$OPERATION_MODE <- .InitEnv$get_mode(.InitEnv$detect_script_path())
  if (identical(.InitEnv$mode, .InitEnv$OPERATION_MODE)) {
    return(invisible(NULL))
  }

  message(">> OPERATION_MODE = ", .InitEnv$OPERATION_MODE)
  .InitEnv$OPERATION_MODE <- .InitEnv$OPERATION_MODE

  if (!requireNamespace("here", quietly = TRUE)) {
    install.packages("here") # 若不存在就安裝
  }

  if (!exists("APP_DIR", envir = .InitEnv)) {
    base <- here::here() # 唯一前置依賴
    list2env(list(
      APP_DIR = base,
      COMPANY_DIR = dirname(base),
      GLOBAL_DIR = file.path(base, "scripts", "global_scripts"),
      GLOBAL_DATA_DIR = file.path(base, "scripts", "global_scripts", "30_global_data"),
      GLOBAL_PARAMETER_DIR = file.path(base, "scripts", "global_scripts", "30_global_data", "parameters"),
      CONFIG_PATH = file.path(base, "app_config.yaml"),
      APP_DATA_DIR = file.path(base, "data", "app_data"),
      APP_PARAMETER_DIR = file.path(base, "data", "app_data", "parameters"),
      LOCAL_DATA_DIR = file.path(base, "data", "local_data"),
      # app_config_path
      CONFIG_PATH = file.path(base, "app_config.yaml")
    ), envir = .InitEnv)

    # ---- 讀取 db_paths.yaml (DM_R048: YAML for configuration) -----------------
    yaml_path <- file.path(.InitEnv$GLOBAL_DIR, "30_global_data", "parameters",
                           "scd_type1", "db_paths.yaml")
    if (file.exists(yaml_path)) {
      # Load YAML and construct full paths
      db_config <- yaml::read_yaml(yaml_path)
      db_path_list <- list()

      # Process databases section (DM_R050: Mode-Specific Path Loading)
      if (!is.null(db_config$databases)) {
        if (.InitEnv$OPERATION_MODE == "APP_MODE") {
          # APP_MODE: 只載入 app_data（部署環境只有這個）
          if ("app_data" %in% names(db_config$databases)) {
            db_path_list$app_data <- file.path(base, db_config$databases$app_data)
          }
        } else {
          # UPDATE_MODE/GLOBAL_MODE: 載入所有路徑
          for (name in names(db_config$databases)) {
            db_path_list[[name]] <- file.path(base, db_config$databases[[name]])
          }
        }
      }

      # Process domain section (only for UPDATE_MODE/GLOBAL_MODE)
      if (!is.null(db_config$domain) &&
          .InitEnv$OPERATION_MODE %in% c("UPDATE_MODE", "GLOBAL_MODE")) {
        for (name in names(db_config$domain)) {
          db_path_list[[name]] <- file.path(base, db_config$domain[[name]])
        }
      }

      # Assign to environments
      assign("db_path_list", db_path_list, envir = .InitEnv)
      assign("db_path_list", db_path_list, envir = .GlobalEnv)
      list2env(db_path_list, envir = .GlobalEnv)
    } else {
      # Fallback: try legacy db_paths.R for backward compatibility
      r_path <- file.path(.InitEnv$GLOBAL_DIR, "30_global_data", "db_paths.R")
      if (file.exists(r_path)) {
        source(r_path, local = .InitEnv)
        if (exists("db_path_list", envir = .InitEnv)) {
          assign("db_path_list", get("db_path_list", envir = .InitEnv), envir = .GlobalEnv)
          list2env(get("db_path_list", envir = .InitEnv), envir = .GlobalEnv)
        }
      }
    }
  }

  ## 把 .InitEnv 裡所有綁定複製到 .GlobalEnv (這樣常數才能被使用)
  list2env(as.list(.InitEnv, all.names = TRUE), envir = .GlobalEnv)

  ## 1️⃣ 決定應該載入哪些初始化腳本（向量）
  init_files <- switch(OPERATION_MODE,
    UPDATE_MODE = c(
      "sc_initialization_app_mode.R",
      "sc_initialization_update_mode.R"
    ), # ← 兩支都跑
    GLOBAL_MODE = "sc_initialization_update_mode.R",
    APP_MODE = "sc_initialization_app_mode.R"
  )

  ## 2️⃣ 逐一載入 -------------------------------------------------
  for (f in init_files) {
    full <- file.path(.InitEnv$GLOBAL_DIR, "22_initializations", f)
    if (file.exists(full)) {
      sys.source(full, envir = .GlobalEnv)
    } else {
      warning("Init file not found: ", full)
    }
  }

  .GlobalEnv$INITIALIZATION_COMPLETED <- TRUE

  invisible(NULL)
}

## ❹ 收尾函式（存放於 .InitEnv） ------------------------------
.InitEnv$autodeinit <- function() {
  ## 1. 關閉所有資料庫連線（函式在 .InitEnv 內）
  if (exists("dbDisconnect_all")) {
    dbDisconnect_all()
  }

  # ## 2. 從搜尋路徑移除 .InitEnv（若有 attach 過）
  # if (".autoinit_env" %in% search()) detach(".autoinit_env")

  ## 3. 刪除 .GlobalEnv 中除 .InitEnv 以外的所有物件 ----------
  objs <- ls(envir = .GlobalEnv, all.names = TRUE)
  objs <- setdiff(objs, c(".InitEnv")) # 保留私有環境
  rm(list = objs, envir = .GlobalEnv)
  gc() # 觸發垃圾回收

  ## 4. 把轉接器薄殼函式重新放回 .GlobalEnv -------------------
  assign("autoinit",
    function(...) .InitEnv$autoinit(...),
    envir = .GlobalEnv
  )
  assign("autodeinit",
    function(...) .InitEnv$autodeinit(...),
    envir = .GlobalEnv
  )

  ## 5. 清除 MODE 旗標，讓下次 autoinit() 重新啟動 ------------
  .InitEnv$mode <- NULL
  message(">> De-init completed ‒ GlobalEnv 已清空並重建薄殼")

  invisible(NULL)
}

## ❺ .GlobalEnv 轉接器（薄殼函式） ---------------------------
assign("autoinit",
  function(...) .InitEnv$autoinit(...),
  envir = .GlobalEnv
)
assign("autodeinit",
  function(...) .InitEnv$autodeinit(...),
  envir = .GlobalEnv
)

## ❻ （可選）啟動即初始化；若不想自動請註解 ------------------
# autoinit()
## -----------------------------------------------------------
