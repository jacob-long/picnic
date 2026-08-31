# Run from the repository root: Rscript tests/test_email.R
# All HTTP requests are replaced; no credentials, network, or emails are used.
stopifnot(
    requireNamespace("httr", quietly = TRUE),
    requireNamespace("jsonlite", quietly = TRUE),
    requireNamespace("digest", quietly = TRUE)
)

expressions <- parse(file = "send_email.R")
email <- new.env(parent = globalenv())

# Load definitions but not the final command-line entry point.
for (expression in expressions[-length(expressions)]) {
    eval(expression, envir = email)
}

response <- function(status = 200L, body = list()) {
    structure(list(
        url = "https://example.test/buttondown",
        status_code = status,
        headers = list("content-type" = "application/json"),
        content = charToRaw(jsonlite::toJSON(body, auto_unbox = TRUE))
    ), class = "response")
}

capture_send <- function(subscriber_ids) {
    request <- NULL
    post <- function(url, ..., body, encode) {
        request <<- list(url = url, body = body, encode = encode, configs = list(...))
        response()
    }
    result <- email$send_draft_to_subscribers(
        api_key = "test-only-key",
        email_id = "em_test",
        subscriber_ids = subscriber_ids,
        frequency = "weekly",
        post_fn = post
    )
    list(result = result, request = request)
}

singleton <- capture_send("sub_b")
singleton_json <- as.character(jsonlite::toJSON(
    singleton$request$body,
    auto_unbox = TRUE
))
stopifnot(
    isTRUE(singleton$result),
    identical(singleton$request$url,
              "https://api.buttondown.com/v1/emails/em_test/send-draft"),
    identical(singleton$request$encode, "json"),
    identical(singleton$request$body$subscribers, list("sub_b")),
    identical(singleton_json, '{"subscribers":["sub_b"]}')
)

group <- capture_send(c("sub_b", "sub_a"))
group_json <- as.character(jsonlite::toJSON(group$request$body, auto_unbox = TRUE))
stopifnot(
    identical(group$request$body$subscribers, list("sub_a", "sub_b")),
    identical(group_json, '{"subscribers":["sub_a","sub_b"]}')
)

empty <- tryCatch(
    email$send_draft_to_subscribers(
        api_key = "test-only-key",
        email_id = "em_test",
        subscriber_ids = character(),
        frequency = "weekly",
        post_fn = function(...) stop("POST should not be called")
    ),
    error = identity
)
stopifnot(
    inherits(empty, "error"),
    grepl("At least one subscriber ID", conditionMessage(empty), fixed = TRUE)
)

failed <- suppressWarnings(email$send_draft_to_subscribers(
    api_key = "test-only-key",
    email_id = "em_test",
    subscriber_ids = "sub_a",
    frequency = "weekly",
    post_fn = function(...) response(
        422L,
        list(code = "list_type", detail = "Input should be a valid list")
    )
))
stopifnot(identical(failed, FALSE))

cat("Buttondown draft serialization and error regression tests passed.\n")
