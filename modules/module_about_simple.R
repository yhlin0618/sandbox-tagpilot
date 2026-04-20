# ==========================================
# About Us Module - Simple Version
# ==========================================
# Simplified version that just loads markdown
# Compatible with default module call pattern
# Version: 2.0
# Last Updated: 2025-10-15

library(shiny)
library(bs4Dash)
library(markdown)

# ==========================================
# Module UI
# ==========================================
aboutModuleUI <- function(id, module_config = NULL, lang_texts = NULL) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("about_content"))
  )
}

# ==========================================
# Module Server
# ==========================================
aboutModuleServer <- function(id, module_config = NULL, lang_texts = NULL) {
  moduleServer(id, function(input, output, session) {

    # Determine language directory
    get_language_dir <- function() {
      # Try to get language from lang_texts
      if (is.reactive(lang_texts)) {
        texts <- tryCatch(lang_texts(), error = function(e) NULL)
      } else {
        texts <- lang_texts
      }

      if (!is.null(texts) && !is.null(texts$language)) {
        lang_code <- texts$language

        # Map language code to directory
        lang_map <- c(
          "zh_TW" = "chinese",
          "en_US" = "english",
          "ja_JP" = "japanese"
        )

        return(lang_map[lang_code] %||% "chinese")
      }

      # Default to chinese
      return("chinese")
    }

    # Get page title
    get_title <- function() {
      if (is.reactive(lang_texts)) {
        texts <- tryCatch(lang_texts(), error = function(e) NULL)
      } else {
        texts <- lang_texts
      }

      if (!is.null(texts) && !is.null(texts$title)) {
        return(texts$title)
      }

      return("關於我們")
    }

    # Render about content
    output$about_content <- renderUI({
      lang_dir <- get_language_dir()
      page_title <- get_title()

      # Construct markdown file path
      md_path <- file.path("database/content", lang_dir, "markdown/about.md")

      cat("[About Module] Loading markdown from:", md_path, "\n")
      cat("[About Module] Language directory:", lang_dir, "\n")

      if (file.exists(md_path)) {
        tryCatch({
          # Read markdown file
          md_content <- paste(readLines(md_path, warn = FALSE), collapse = "\n")

          # Render markdown to HTML
          html_content <- markdownToHTML(
            text = md_content,
            fragment.only = TRUE,
            options = c("use_xhtml", "smartypants", "base64_images", "mathjax")
          )

          # Wrap in a card
          bs4Card(
            title = page_title,
            status = "primary",
            solidHeader = FALSE,
            collapsible = FALSE,
            width = 12,
            HTML(html_content)
          )
        }, error = function(e) {
          # Error rendering markdown
          bs4Card(
            title = page_title,
            status = "danger",
            solidHeader = FALSE,
            width = 12,
            div(
              class = "alert alert-danger",
              h4("Markdown Rendering Error"),
              p(paste0("Error: ", e$message)),
              p(paste0("File: ", md_path))
            )
          )
        })
      } else {
        # File not found
        bs4Card(
          title = page_title,
          status = "warning",
          solidHeader = FALSE,
          width = 12,
          div(
            class = "alert alert-warning",
            h4("Content Not Found"),
            p(paste0("Could not find markdown file: ", md_path)),
            p("Please ensure the about.md file exists in the correct language directory."),
            tags$ul(
              tags$li("Chinese: database/content/chinese/markdown/about.md"),
              tags$li("English: database/content/english/markdown/about.md"),
              tags$li("Japanese: database/content/japanese/markdown/about.md")
            )
          )
        )
      }
    })
  })
}

# NULL coalesce operator
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (is.character(x) && !nzchar(x))) y else x
}
