# Utility Functions
# -----------------

#' Perform HTTP request with retry logic for rate limiting
#' @param req httr2 request object
#' @param max_attempts Maximum number of retry attempts
#' @param base_delay Base delay for exponential backoff
#' @return Response object or NULL if all attempts fail
perform_with_retries <- function(req, max_attempts = 3, base_delay = 2) {
    for (attempt in seq_len(max_attempts)) {
        resp <- tryCatch(httr2::req_perform(req), error = function(e) e)
        if (!inherits(resp, "error") && (is.null(httr2::resp_status(resp)) || httr2::resp_status(resp) != 429)) {
            return(resp)
        }
        if (attempt < max_attempts) Sys.sleep(base_delay^attempt)
    }
    return(NULL)
}

#' Fetch URL with retry logic for 429 errors
#' @param url URL to fetch
#' @param user_agent_string User agent string
#' @param max_retries Maximum number of retries
#' @param initial_delay Initial delay in seconds
#' @return httr response object
fetch_with_retry <- function(url, user_agent_string, max_retries = 5, initial_delay = 5) {
    current_retry <- 0
    delay <- initial_delay

    while (current_retry < max_retries) {
        response <- httr::GET(url, httr::user_agent(user_agent_string), httr::timeout(30))
        status <- httr::status_code(response)

        if (status == 429) {
            current_retry <- current_retry + 1
            warning(paste("Rate limit hit (429) for URL:", url, "| Retry", current_retry, "of", max_retries, "after", delay, "seconds."))
            Sys.sleep(delay)
            delay <- delay * 2
        } else {
            return(response)
        }
    }

    warning(paste("Max retries reached for URL:", url))
    return(response)
}

#' Extract DOI ID from URL
#' @param url DOI URL
#' @return DOI identifier string
extract_doi_id <- function(url) {
    return(gsub("http(|s)://dx.doi.org/", "", url))
}

#' Strip HTML tags from string
#' @param str Input string
#' @return String with HTML tags removed
strip_html <- function(str) {
    if (is.null(str)) return(NA)
    else {
        str <- gsub("<.*?>", " ", str)
        str <- gsub("\\s+", " ", str)
        str <- trimws(str)
        return(str)
    }
}

#' Strip excess whitespace from string
#' @param str Input string
#' @return String with excess whitespace removed
strip_whitespace <- function(str) {
    if (is.null(str)) return(NA)
    else {
        str <- gsub("\\s+", " ", str)
        return(trimws(str))
    }
}

#' Check if file is empty
#' @param file File path
#' @return TRUE if file is empty
file_empty <- function(file) {
    length(readLines(file)) == 0
}

#' Read CSV file with empty file check
#' @param file File path
#' @param ... Additional arguments to read.csv2
#' @return Data frame or NULL if file is empty
read.csv2_check <- function(file, ...) {
    if (!file_empty(file)) {
        return(read.csv2(file, ...))
    } else {
        return(NULL)
    }
}
