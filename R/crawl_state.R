# Crawl state helpers
# -------------------

CRAWL_SUCCESS_FILE <- file.path("memory", "last_crawl_success.txt")

read_crawl_success_date <- function(path = CRAWL_SUCCESS_FILE) {
    if (!file.exists(path)) return(as.Date(NA_character_))

    value <- trimws(readLines(path, n = 1, warn = FALSE))
    date <- tryCatch(
        suppressWarnings(as.Date(value)),
        error = function(e) as.Date(NA_character_)
    )
    if (length(date) != 1 || is.na(date)) {
        stop("Invalid crawl success date in ", path, call. = FALSE)
    }
    date
}

get_crawl_start_date <- function(
    today = Sys.Date(),
    path = CRAWL_SUCCESS_FILE,
    overlap_days = 7
) {
    normal_start <- today - overlap_days
    last_success <- read_crawl_success_date(path)
    if (is.na(last_success)) return(normal_start)
    if (last_success > today) {
        stop("Crawl success date cannot be in the future", call. = FALSE)
    }

    # Retain the normal overlap, but widen it across an outage.
    min(normal_start, last_success - 1)
}

require_fresh_crawl <- function(today = Sys.Date(), path = CRAWL_SUCCESS_FILE) {
    last_success <- read_crawl_success_date(path)
    if (is.na(last_success)) {
        stop("Email delivery blocked: no successful crawl is recorded", call. = FALSE)
    }
    if (last_success != today) {
        stop(sprintf(
            "Email delivery blocked: latest successful crawl is %s, expected %s",
            last_success, today
        ), call. = FALSE)
    }
    invisible(last_success)
}
