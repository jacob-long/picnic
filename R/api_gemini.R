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
            warning("Using GEMINI_API_KEY from R environment (credentials.R).")
        } else {
            stop("Gemini API Key not found. Checked Sys.getenv('GEMINI_APIKEY'), Sys.getenv('GEMINI_API_KEY'), and R variable 'gemini_apikey'.")
        }
    }

    return(api_key)
}

#' Call Google Gemini API
#' @param system_prompt System instruction text
#' @param user_text User prompt text
#' @param model Model identifier (default: "gemini-1.5-flash-latest")
#' @param temperature Generation temperature (default: 0.2)
#' @param max_tokens Maximum output tokens (default: 10)
#' @return Response text or NULL on failure
call_gemini_api <- function(system_prompt, user_text, model = "gemini-1.5-flash-latest",
                            temperature = 0.2, max_tokens = 10) {
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
            temperature = temperature,
            maxOutputTokens = max_tokens
        )
    )

    response <- httr::POST(
        url = api_url,
        body = jsonlite::toJSON(request_body, auto_unbox = TRUE),
        httr::add_headers("Content-Type" = "application/json"),
        httr::timeout(60)
    )

    if (httr::status_code(response) != 200) {
        error_content <- httr::content(response, "text", encoding = "UTF-8")
        stop(paste("Gemini API request failed with status:", httr::status_code(response), "\nResponse:", error_content))
    }

    parsed_response <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))

    if (length(parsed_response$candidates) > 0 &&
        length(parsed_response$candidates[[1]]$content$parts) > 0 &&
        !is.null(parsed_response$candidates[[1]]$content$parts[[1]]$text)) {

        result_text <- parsed_response$candidates[[1]]$content$parts[[1]]$text
        return(trimws(result_text))
    } else {
        warning("Could not extract text from Gemini response. Full response:")
        print(parsed_response)
        return(NULL)
    }
}

#' Call Gemini API with retry logic (for must_read.R)
#' @param prompt Full prompt text
#' @param model Model identifier
#' @param temperature Generation temperature
#' @param max_retries Maximum retry attempts
#' @return Response text or NULL on failure
gemini_request <- function(prompt, model = "gemini-3-pro-preview",
                           temperature = 1, max_retries = 3) {
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

    for (attempt in 1:max_retries) {
        response <- tryCatch({
            httr::POST(
                url = paste0(api_url, "?key=", api_key),
                body = jsonlite::toJSON(body, auto_unbox = TRUE),
                httr::add_headers("Content-Type" = "application/json")
            )
        }, error = function(e) e)

        if (!inherits(response, "error") && httr::status_code(response) == 200) {
            content <- httr::content(response, "parsed")
            return(content$candidates[[1]]$content$parts[[1]]$text)
        } else if (!inherits(response, "error") && httr::status_code(response) == 503) {
            wait_time <- 2^attempt
            message(sprintf("Attempt %d failed with 503 error. Waiting %d seconds...", attempt, wait_time))
            Sys.sleep(wait_time)
            next
        } else {
            if (attempt == max_retries) {
                warning("Max retries reached. Last error: ",
                        if (inherits(response, "error")) response$message
                        else paste(httr::status_code(response), "-", httr::content(response, "text")))
                return(NULL)
            }
            message(sprintf("Attempt %d failed. Retrying...", attempt))
            Sys.sleep(1)
        }
    }
}
