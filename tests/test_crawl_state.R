# Run from the repository root: Rscript tests/test_crawl_state.R
state <- new.env(parent = globalenv())
sys.source("R/crawl_state.R", envir = state)

marker <- tempfile("crawl-success-")
today <- as.Date("2026-11-23")

stopifnot(identical(
    state$get_crawl_start_date(today, marker),
    as.Date("2026-11-16")
))

writeLines("2026-11-10", marker)
stopifnot(identical(
    state$get_crawl_start_date(today, marker),
    as.Date("2026-11-09")
))

writeLines("2026-11-22", marker)
stopifnot(identical(
    state$get_crawl_start_date(today, marker),
    as.Date("2026-11-16")
))

stale <- tryCatch(
    state$require_fresh_crawl(today, marker),
    error = identity
)
stopifnot(
    inherits(stale, "error"),
    grepl("latest successful crawl is 2026-11-22", conditionMessage(stale),
          fixed = TRUE)
)

writeLines("2026-11-23", marker)
stopifnot(identical(
    state$require_fresh_crawl(today, marker),
    as.Date("2026-11-23")
))

writeLines("not-a-date", marker)
malformed <- tryCatch(
    state$read_crawl_success_date(marker),
    error = identity
)
stopifnot(
    inherits(malformed, "error"),
    grepl("Invalid crawl success date", conditionMessage(malformed), fixed = TRUE)
)

unlink(marker)
missing <- tryCatch(
    state$require_fresh_crawl(today, marker),
    error = identity
)
stopifnot(
    inherits(missing, "error"),
    grepl("no successful crawl", conditionMessage(missing), fixed = TRUE)
)

cat("Crawl recovery state regression tests passed.\n")
