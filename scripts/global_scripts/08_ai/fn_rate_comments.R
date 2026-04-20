#' Rate Comments Using OpenAI API
#'
#' Analyzes review text to extract sentiment by property as part of D03_07 (Rate Reviews) step.
#' Uses OpenAI GPT API to evaluate product comments against defined properties.
#'
#' @param title Character. The review title (optional, can be NULL or NA).
#' @param body Character. The review content/body (optional, can be NULL or NA).
#' @param product_line_name Character. The product line name in English.
#' @param property_name Character. The property to evaluate.
#' @param type Character. The type of property (e.g., "brand personality").
#' @param gpt_key Character. OpenAI API key.
#' @param model Character. The model to use (default: "o4-mini").
#'
#' @return Character. Response in the format "[Score, Reason]" or "[NaN,NaN]" if not applicable.
#'
#' @examples
#' \dontrun{
#' rate_comments(
#'   title = "Great taste",
#'   body = "This helps my digestion a lot.",
#'   product_line_name = "Sympt-X",
#'   property_name = "balance",
#'   type = "brand personality",
#'   gpt_key = Sys.getenv("OPENAI_API_KEY"),
#'   model = "o4-mini"
#' )
#' }
#'
#' @export
rate_comments <- function(title, 
                          body,
                          product_line_name,
                          property_name,
                          type,
                          gpt_key,
                          model = "o4-mini") {
  
  # Required packages
  if (!requireNamespace("httr2", quietly = TRUE)) library(httr2)
  if (!requireNamespace("jsonlite", quietly = TRUE)) library(jsonlite)
  if (!requireNamespace("glue", quietly = TRUE)) library(glue)
  
  # Handle optional title and body
  has_title <- !is.null(title) && !is.na(title) && nchar(trimws(title)) > 0
  has_body <- !is.null(body) && !is.na(body) && nchar(trimws(body)) > 0
  
  # Build comment text based on available fields
  comment_text <- if (has_title && has_body) {
    glue::glue("Title: {title}\nBody: {body}")
  } else if (has_title) {
    glue::glue("Comment: {title}")
  } else if (has_body) {
    glue::glue("Comment: {body}")
  } else {
    return("[NaN,No_comment_text]")
  }
  
  # Create prompt
  prompt <- glue::glue(
    "The following is a comment on a {product_line_name} product:\n",
    "{comment_text}\n",
    "Evaluate the comment regarding the product's '{property_name}', ",
    "which is categorized as a {type} feature.\n\n",
    "Use the following rules to respond:\n",
    "1. If the comment does not demonstrate the stated characteristic ",
    "in any way, reply exactly [NaN,NaN] without any additional reasoning ",
    "or explanation.\n",
    "2. Otherwise, rate your agreement with the statement on a scale from 1 to 5:\n",
    "- '5' for Strongly Agree\n- '4' for Agree\n- '3' for Neither Agree nor Disagree\n",
    "- '2' for Disagree\n- '1' for Strongly Disagree\n",
    "Provide your rationale in the format: [Score, Reason]."
  )
  
  # Define rate limiting handler
  openai_after <- function(resp) {
    retry_after <- httr2::resp_header(resp, "Retry-After")
    if (!is.na(retry_after)) {
      return(as.numeric(retry_after))
    }
    reset_unix <- httr2::resp_header(resp, "x-ratelimit-reset-requests")
    if (!is.na(reset_unix)) {
      return(as.numeric(reset_unix) - unclass(Sys.time()))
    }
    NA  # Default to backoff
  }
  
  # Create and send the API request
  req <- httr2::request("https://api.openai.com/v1/chat/completions") |>
    httr2::req_auth_bearer_token(gpt_key) |>
    httr2::req_body_json(list(
      model    = model,
      messages = list(
        list(role = "system",
             content = "Forget any previous information."),
        list(role = "user", content = prompt)
      )
    )) |>
    httr2::req_retry(
      is_transient = \(resp) httr2::resp_status(resp) %in% c(429, 500:599),
      after        = openai_after,
      backoff      = \(n) min(2^(n - 1), 30),  # Exponential backoff: 2,4,8,16,30...
      max_tries    = 6,
      max_seconds  = 180
    ) |>
    httr2::req_timeout(300)  # 5 minutes to handle long AI analysis reports
  
  # Try to perform the request
  resp <- try(httr2::req_perform(req), silent = TRUE)
  
  # Handle errors and parse response
  if (inherits(resp, "try-error"))
    return("[NaN,Connection_error]")
  
  if (httr2::resp_status(resp) != 200)
    return(paste0("[NaN,HTTP_", httr2::resp_status(resp), "]"))
  
  out <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  
  if (!is.null(out$choices))
    return(trimws(out$choices[[1]]$message$content))
  if (!is.null(out$error))
    return(paste0("[NaN,API_error:", out$error$message, "]"))
  
  "[NaN,Unknown_format]"
}