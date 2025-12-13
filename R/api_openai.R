# OpenAI API Functions
# --------------------

#' Call OpenAI Chat Completions API
#' @param system_prompt System message content
#' @param user_prompt User message content
#' @param model Model identifier (e.g., "gpt-4o-mini")
#' @return Parsed API response
call_openai_api <- function(system_prompt, user_prompt, model) {
    endpoint <- "https://api.openai.com/v1/chat/completions"
    body <- list(
        model = model,
        messages = list(
            list(role = "system", content = system_prompt),
            list(role = "user", content = user_prompt)
        )
    )
    body <- jsonlite::toJSON(body, auto_unbox = TRUE)
    res <- httr::POST(
        endpoint,
        body = body,
        encode = "raw",
        httr::content_type_json(),
        httr::add_headers(Authorization = paste("Bearer", openai_apikey, sep = " "))
    )

    return(httr::content(res))
}

#' Extract text response from OpenAI API response
#' @param response Parsed API response
#' @return Response text content
get_openai_response <- function(response) {
    return(response$choices[[1]]$message$content)
}

#' Get finish reason from OpenAI API response
#' @param response Parsed API response
#' @return Finish reason string
get_openai_finish_reason <- function(response) {
    return(response$choices[[1]]$finish_reason)
}

#' Get token usage from OpenAI API response
#' @param response Parsed API response
#' @return Total tokens used
get_openai_usage <- function(response) {
    return(unlist(response$usage$total_tokens))
}
