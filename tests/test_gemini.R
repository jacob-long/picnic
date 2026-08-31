# Run from the repository root: Rscript tests/test_gemini.R
# All HTTP requests and sleeps are replaced; no credentials or network are used.
stopifnot(
    requireNamespace("httr", quietly = TRUE),
    requireNamespace("jsonlite", quietly = TRUE)
)
api <- new.env(parent = baseenv())
sys.source("R/api_gemini.R", envir = api)
api$get_gemini_api_key <- function() "test-only-key"
request_with_retry <- api$post_gemini_with_retry

response <- function(status = 200L, text = "Yes", headers = list()) {
    body <- if (status == 200L) {
        list(candidates = list(list(
            content = list(parts = list(list(text = text))),
            finishReason = "STOP"
        )))
    } else {
        list(error = list(code = status, message = "Simulated API failure"))
    }
    structure(list(
        url = "https://example.test/gemini",
        status_code = status,
        headers = headers,
        content = charToRaw(jsonlite::toJSON(body, auto_unbox = TRUE))
    ), class = "response")
}

exercise_request <- function(run, replies) {
    calls <- list()
    waits <- numeric()
    post <- function(url, body, ...) {
        configs <- list(...)
        calls[[length(calls) + 1L]] <<- list(
            url = url,
            body = jsonlite::fromJSON(body),
            timeout_ms = unlist(lapply(configs, function(x) x$options$timeout_ms))
        )
        reply <- replies[[length(calls)]]
        if (inherits(reply, "error")) stop(reply)
        reply
    }
    api$post_gemini_with_retry <- function(...) {
        request_with_retry(
            ...,
            post_fn = post,
            sleep_fn = function(seconds) waits <<- c(waits, seconds)
        )
    }
    result <- tryCatch(run(), error = identity)
    list(result = result, calls = calls, waits = waits)
}

must_read <- exercise_request(
    function() api$gemini_request("Choose the most relevant articles"),
    list(response(text = "10.1234/test"))
)
stopifnot(
    identical(must_read$result, "10.1234/test"),
    length(must_read$calls) == 1L,
    must_read$calls[[1]]$timeout_ms == 300000,
    grepl("/gemini-3.1-pro-preview:generateContent", must_read$calls[[1]]$url, fixed = TRUE)
)

classifier <- exercise_request(
    function() api$call_gemini_api("Classify the paper", "Title and abstract"),
    list(response())
)
stopifnot(
    identical(classifier$result, "Yes"),
    classifier$calls[[1]]$timeout_ms == 60000,
    grepl("/gemini-3.7-flash:generateContent", classifier$calls[[1]]$url, fixed = TRUE),
    classifier$calls[[1]]$body$generationConfig$maxOutputTokens == 64
)

recovered <- exercise_request(
    function() api$gemini_request("Select papers", timeout_seconds = 180, max_retries = 2),
    list(simpleError("Timeout was reached"), response())
)
stopifnot(
    identical(recovered$result, "Yes"),
    length(recovered$calls) == 2L,
    all(vapply(recovered$calls, function(x) x$timeout_ms == 180000, logical(1))),
    identical(recovered$waits, 2)
)

exhausted <- exercise_request(
    function() api$gemini_request("Select papers", max_retries = 2),
    list(simpleError("Timeout was reached"), simpleError("Timeout was reached"))
)
stopifnot(
    inherits(exhausted$result, "error"),
    grepl("Timeout was reached", conditionMessage(exhausted$result), fixed = TRUE),
    length(exhausted$calls) == 2L,
    identical(exhausted$waits, 2)
)

transient <- exercise_request(
    function() api$gemini_request("Select papers"),
    list(response(503L), response(429L, headers = list("retry-after" = "7")), response())
)
stopifnot(
    identical(transient$result, "Yes"),
    length(transient$calls) == 3L,
    identical(transient$waits, c(2, 7))
)

invalid <- exercise_request(
    function() api$gemini_request("Select papers"),
    list(response(400L))
)
stopifnot(
    inherits(invalid$result, "error"),
    length(invalid$calls) == 1L,
    length(invalid$waits) == 0L
)

cat("Gemini timeout, retry, and model regression tests passed.\n")
