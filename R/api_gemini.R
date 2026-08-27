# Google Gemini API Functions
# ---------------------------

#' Get Gemini API key from environment or R variable
#' @return API key string
get_gemini_api_key <- function() {
    api_key <- Sys.getenv("GEMINI_APIKEY")

    if (api_key == "") {
        api_key <- Sys.getenv("GEMINI_API_KEY")
    }

    if (api_key == "") {
        if (exists("gemini_apikey") && !is.null(gemini_apikey) && gemini_apikey != "") {
            api_key <- gemini_apikey
        } else {
            stop("Gemini API Key not found. Checked Sys.getenv('GEMINI_APIKEY'), Sys.getenv('GEMINI_API_KEY'), and R variable 'gemini_apikey'.")
        }
    }

    return(api_key)
}

#' Call Google Gemini API
#' @param system_prompt System instruction text
#' @param user_text User prompt text
#' @param model Model identifier (default: "gemini-3.7-flash")
#' @param max_tokens Maximum output tokens (default: 64)
#' @param max_attempts Maximum request attempts for transient failures
#' @param initial_delay Initial retry delay in seconds
#' @return Response text or NULL on failure
call_gemini_api <- function(system_prompt, user_text, model = "gemini-3.7-flash",
                            max_tokens = 64,
                            max_attempts = 5, initial_delay = 2) {
    api_key <- get_gemini_api_key()
    api_url <- paste0(
        "https://generativelanguage.googleapis.com/v1beta/models/",
        model, ":generateContent?key=", api_key
    )

    request_body <- list(
        systemInstruction = list(
            parts = list(list(text = system_prompt))
        ),
        contents = list(
            list(
                role = "user",
                parts = list(list(text = user_text))
            )
        ),
        generationConfig = list(
            maxOutputTokens = max_tokens,
            thinkingConfig = list(thinkingLevel = "low")
        )
    )

    response <- post_gemini_with_retry(
        api_url = api_url,
        request_body = request_body,
        max_attempts = max_attempts,
        initial_delay = initial_delay
    )

    extract_gemini_text(response)
}

GEMINI_RETRYABLE_STATUSES <- c(429L, 500L, 502L, 503L, 504L)

#' POST a Gemini request with bounded exponential backoff
#' @param api_url Complete Gemini API URL
#' @param request_body Request body as an R list
#' @param max_attempts Maximum request attempts
#' @param initial_delay Initial retry delay in seconds
#' @param timeout_seconds Per-request timeout in seconds
#' @param post_fn HTTP POST function, injectable for tests
#' @param sleep_fn Sleep function, injectable for tests
#' @return Successful httr response
post_gemini_with_retry <- function(api_url, request_body, max_attempts = 5,
                                   initial_delay = 2, timeout_seconds = 60,
                                   post_fn = httr::POST, sleep_fn = Sys.sleep) {
    if (max_attempts < 1) stop("max_attempts must be at least 1.")

    request_json <- jsonlite::toJSON(request_body, auto_unbox = TRUE)
    last_error <- "Gemini API request failed."

    for (attempt in seq_len(max_attempts)) {
        response <- tryCatch(
            post_fn(
                url = api_url,
                body = request_json,
                httr::add_headers("Content-Type" = "application/json"),
                httr::timeout(timeout_seconds)
            ),
            error = function(e) e
        )

        if (inherits(response, "error")) {
            failure <- paste("network error:", conditionMessage(response))
            last_error <- paste("Gemini API request failed with", failure)
            retryable <- TRUE
            retry_after <- NA_real_
        } else {
            status <- httr::status_code(response)
            if (status == 200) return(response)

            error_content <- httr::content(response, "text", encoding = "UTF-8")
            failure <- paste("HTTP", status)
            last_error <- paste(
                "Gemini API request failed with status:", status,
                "\nResponse:", error_content
            )
            retryable <- status %in% GEMINI_RETRYABLE_STATUSES
            retry_after <- suppressWarnings(as.numeric(
                httr::headers(response)[["retry-after"]]
            ))
        }

        if (!retryable || attempt == max_attempts) {
            stop(last_error, call. = FALSE)
        }

        wait_time <- if (length(retry_after) == 1 && is.finite(retry_after)) {
            max(0, retry_after)
        } else {
            min(initial_delay * 2^(attempt - 1), 60)
        }
        message(sprintf(
            "Gemini request attempt %d/%d failed (%s); retrying in %.0f seconds.",
            attempt, max_attempts, failure, wait_time
        ))
        sleep_fn(wait_time)
    }
}

#' Extract response text without simplifying Gemini's nested content structure
#' @param response Successful httr response
#' @return Trimmed response text or NULL
extract_gemini_text <- function(response) {
    parsed_response <- jsonlite::fromJSON(
        httr::content(response, "text", encoding = "UTF-8"),
        simplifyVector = FALSE
    )
    candidates <- parsed_response$candidates
    parts <- if (length(candidates) > 0) candidates[[1]]$content$parts else NULL
    text_parts <- Filter(
        function(part) !is.null(part$text) && !isTRUE(part$thought),
        parts
    )
    if (length(text_parts) == 0) {
        text_parts <- Filter(function(part) !is.null(part$text), parts)
    }
    result_text <- if (length(text_parts) > 0) {
        paste(vapply(text_parts, `[[`, character(1), "text"), collapse = "")
    } else {
        NULL
    }

    if (is.null(result_text) || length(result_text) == 0) {
        finish_reason <- if (length(candidates) > 0) candidates[[1]]$finishReason else NULL
        if (is.null(finish_reason)) finish_reason <- "unknown"
        warning(sprintf(
            "Could not extract text from Gemini response (finish reason: %s).",
            finish_reason
        ))
        return(NULL)
    }

    trimws(result_text)
}

#' Call Gemini API with retry logic (for must_read.R)
#' @param prompt Full prompt text
#' @param model Model identifier
#' @param temperature Generation temperature
#' @param max_retries Maximum retry attempts
#' @param timeout_seconds Per-request timeout for the full must-read selection
#' @return Response text or NULL on failure
gemini_request <- function(prompt, model = "gemini-3.1-pro-preview",
                           temperature = 1, max_retries = 5,
                           timeout_seconds = 300) {
    api_key <- get_gemini_api_key()
    api_url <- paste0(
        "https://generativelanguage.googleapis.com/v1beta/models/",
        model, ":generateContent"
    )

    body <- list(
        contents = list(
            list(parts = list(list(text = prompt)))
        ),
        generationConfig = list(
            temperature = temperature
        )
    )

    message(sprintf(
        "Must-read selection: model=%s, timeout=%g seconds per attempt, max attempts=%d.",
        model, timeout_seconds, max_retries
    ))
    response <- post_gemini_with_retry(
        api_url = paste0(api_url, "?key=", api_key),
        request_body = body,
        max_attempts = max_retries,
        timeout_seconds = timeout_seconds
    )
    extract_gemini_text(response)
}
