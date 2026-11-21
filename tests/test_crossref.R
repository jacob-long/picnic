# Run from the repository root: Rscript tests/test_crossref.R
# All HTTP requests and retry sleeps are replaced.
stopifnot(
    requireNamespace("httr", quietly = TRUE),
    requireNamespace("jsonlite", quietly = TRUE)
)

crossref <- new.env(parent = globalenv())
sys.source("R/api_crossref.R", envir = crossref)
crossref$crossref_email <- "test@example.com"

response <- function(status = 200L, body = list(), headers = list()) {
    headers <- c(list("content-type" = "application/json"), headers)
    structure(list(
        url = "https://example.test/crossref",
        status_code = status,
        headers = headers,
        content = charToRaw(as.character(jsonlite::toJSON(
            body,
            auto_unbox = TRUE,
            null = "null"
        )))
    ), class = "response")
}

call_api <- function(get_fn, ...) {
    crossref$call_crossref_api(
        id = "0021-9916",
        start = "2026-11-10",
        end = "2026-11-20",
        get_fn = get_fn,
        sleep_fn = function(seconds) stop("Unexpected retry sleep"),
        ...
    )
}

item <- list(
    title = list("A valid article"),
    author = list(list(given = "Ada", family = "Lovelace")),
    created = list("date-parts" = list(c(2026, 11, 20))),
    abstract = "An abstract",
    URL = "https://doi.org/10.1234/example"
)
valid_response <- response(body = list(
    status = "ok",
    message = list(items = list(item))
))

request <- NULL
valid <- call_api(function(url, query) {
    request <<- list(url = url, query = query)
    valid_response
})
articles <- crossref$get_crossref_articles(valid)
stopifnot(
    identical(request$url,
              "https://api.crossref.org/journals/0021-9916/works"),
    identical(request$query$mailto, "test@example.com"),
    nrow(articles) == 1,
    identical(articles$title, "A valid article"),
    identical(articles$authors, "Ada Lovelace"),
    identical(articles$created, "2026-11-20")
)

empty <- call_api(function(...) response(body = list(
    status = "ok",
    message = list(items = list())
)))
stopifnot(is.null(crossref$get_crossref_articles(empty)))

attempts <- 0L
sleeps <- numeric()
rate_limited <- crossref$call_crossref_api(
    id = "0021-9916",
    start = "2026-11-10",
    end = "2026-11-20",
    max_attempts = 2,
    initial_delay = 9,
    get_fn = function(...) {
        attempts <<- attempts + 1L
        if (attempts == 1L) {
            response(
                status = 429L,
                body = list(message = "Too Many Requests"),
                headers = list("retry-after" = "3")
            )
        } else {
            valid_response
        }
    },
    sleep_fn = function(seconds) sleeps <<- c(sleeps, seconds)
)
stopifnot(
    attempts == 2L,
    identical(sleeps, 3),
    nrow(crossref$get_crossref_articles(rate_limited)) == 1
)

server_attempts <- 0L
server_error_recovered <- crossref$call_crossref_api(
    id = "0021-9916",
    start = "2026-11-10",
    end = "2026-11-20",
    max_attempts = 2,
    initial_delay = 0,
    get_fn = function(...) {
        server_attempts <<- server_attempts + 1L
        if (server_attempts == 1L) {
            response(503L, list(message = "Service Unavailable"))
        } else {
            valid_response
        }
    },
    sleep_fn = function(...) NULL
)
stopifnot(
    server_attempts == 2L,
    nrow(crossref$get_crossref_articles(server_error_recovered)) == 1
)

failed_attempts <- 0L
rate_limit_error <- tryCatch(
    crossref$call_crossref_api(
        id = "0021-9916",
        start = "2026-11-10",
        end = "2026-11-20",
        max_attempts = 2,
        initial_delay = 0,
        get_fn = function(...) {
            failed_attempts <<- failed_attempts + 1L
            response(429L, list(message = "Too Many Requests"))
        },
        sleep_fn = function(...) NULL
    ),
    error = identity
)
stopifnot(
    failed_attempts == 2L,
    inherits(rate_limit_error, "error"),
    grepl("HTTP 429", conditionMessage(rate_limit_error), fixed = TRUE)
)

malformed_error <- tryCatch(
    call_api(function(...) response(body = "temporarily unavailable")),
    error = identity
)
stopifnot(
    inherits(malformed_error, "error"),
    grepl("Malformed Crossref response", conditionMessage(malformed_error),
          fixed = TRUE),
    grepl("message.items", conditionMessage(malformed_error), fixed = TRUE)
)

cat("Crossref response handling regression tests passed.\n")
