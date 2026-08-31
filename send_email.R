# Email Digest Sender for Communication Paper Picnic
# ---------------------------------------------------
# Sends personalized email digests via Buttondown API based on subscriber preferences
#
# Usage: Rscript send_email.R [frequency] [--dry-run]
#   frequency: "daily", "weekly", or "monthly"
#   --dry-run: Preview emails without sending
#
# Environment variables required:
#   BUTTONDOWN_API_KEY - Buttondown API key

library(httr)
library(jsonlite)
library(dplyr)
library(purrr)

# Configuration
BUTTONDOWN_API_URL <- "https://api.buttondown.com/v1"
BUTTONDOWN_API_VERSION <- "2026-04-01"
MEMORY_DIR <- "./memory"
OUTPUT_DIR <- "./output"

MAX_LOOKBACK_DAYS <- c(
    daily = 1,
    weekly = 7,
    monthly = 31
)

# Discipline tag to file mapping
DISCIPLINE_MAP <- list(
    "disc-communication" = list(name = "Communication", file = "communication"),
    "disc-politics" = list(name = "Politics", file = "politics"),
    "disc-po" = list(name = "Public Opinion", file = "po"),
    "disc-psych" = list(name = "Psychology", file = "psych"),
    "disc-sociology" = list(name = "Sociology", file = "sociology"),
    "disc-multidisciplinary" = list(name = "Multidisciplinary", file = "multidisciplinary"),
    "disc-preprints" = list(name = "Preprints", file = "preprints"),
    "disc-mustread" = list(name = "Must-Read", file = "must_read")
)

# Frequency tags
FREQUENCY_TAGS <- c("freq-daily", "freq-weekly", "freq-monthly")

# Resolve conflicting frequency tags conservatively to avoid duplicate sends.
resolve_frequency_tag <- function(tags) {
    matching_tags <- FREQUENCY_TAGS[FREQUENCY_TAGS %in% tags]

    if (length(matching_tags) == 0) {
        return(NA_character_)
    }

    if (length(matching_tags) == 1) {
        return(matching_tags[[1]])
    }

    precedence <- c("freq-monthly", "freq-weekly", "freq-daily")
    precedence[precedence %in% matching_tags][[1]]
}

# Get API key from environment
get_buttondown_api_key <- function() {
    key <- Sys.getenv("BUTTONDOWN_API_KEY")
    if (key == "") {
        if (file.exists("credentials.R")) {
            source("credentials.R")
            if (exists("buttondown_apikey")) {
                return(buttondown_apikey)
            }
        }
        stop("BUTTONDOWN_API_KEY not found in environment")
    }
    return(key)
}

# Read last sent timestamp for a frequency tier
get_last_sent_date <- function(frequency) {
    file_path <- file.path(MEMORY_DIR, paste0("last_email_", frequency, ".txt"))
    last_sent <- as.Date(NA_character_)

    if (file.exists(file_path)) {
        date_str <- trimws(readLines(file_path, n = 1))
        last_sent <- suppressWarnings(as.Date(date_str))
    }

    earliest_date <- Sys.Date() - MAX_LOOKBACK_DAYS[[frequency]]
    if (is.na(last_sent)) return(earliest_date)

    # A long outage should not produce an unusably large recovery digest.
    return(max(last_sent, earliest_date))
}

# Update last sent timestamp
update_last_sent_date <- function(frequency, date = Sys.Date()) {
    file_path <- file.path(MEMORY_DIR, paste0("last_email_", frequency, ".txt"))
    writeLines(as.character(date), file_path)
}

# ============================================================================
# Buttondown API Functions
# ============================================================================

#' Fetch all regular subscribers with pagination
#' @param api_key Buttondown API key
#' @return Data frame of subscribers
fetch_subscribers <- function(api_key) {
    all_subscribers <- list()
    page <- 1

    repeat {
        url <- paste0(BUTTONDOWN_API_URL, "/subscribers")
        response <- GET(
            url,
            query = list(page = page, type = "regular"),
            add_headers(Authorization = paste("Token", api_key))
        )

        if (status_code(response) != 200) {
            error_response <- tryCatch(
                content(response, "parsed", type = "application/json"),
                error = function(e) NULL
            )
            error_detail <- if (!is.null(error_response$detail)) {
                paste0(": ", error_response$detail)
            } else {
                ""
            }
            stop(
                paste0(
                    "Failed to fetch subscribers from Buttondown (",
                    status_code(response),
                    ")",
                    error_detail
                ),
                call. = FALSE
            )
        }

        data <- content(response, "parsed")

        if (length(data$results) == 0) break

        all_subscribers <- c(all_subscribers, data$results)

        if (is.null(data$`next`)) break
        page <- page + 1
    }

    # Convert to data frame
    if (length(all_subscribers) == 0) {
        return(data.frame(
            id = character(),
            email = character(),
            tags = I(list()),
            stringsAsFactors = FALSE
        ))
    }

    subscribers_df <- map_dfr(all_subscribers, function(sub) {
        data.frame(
            id = sub$id,
            email = sub$email_address,
            tags = I(list(unlist(sub$tags) %||% character())),
            stringsAsFactors = FALSE
        )
    })

    return(subscribers_df)
}

#' Get subscribers for a specific frequency tier with their discipline preferences
#' @param api_key Buttondown API key
#' @param frequency "daily", "weekly", or "monthly"
#' @return Data frame with subscriber info and discipline tags
get_subscribers_for_frequency <- function(api_key, frequency) {
    freq_tag <- paste0("freq-", frequency)

    cat("Fetching regular subscribers\n")
    subscribers <- fetch_subscribers(api_key)

    if (nrow(subscribers) == 0) {
        cat("No regular subscribers found.\n")
        return(subscribers)
    }

    subscribers$resolved_frequency <- map_chr(subscribers$tags, resolve_frequency_tag)

    conflicting_frequency_count <- sum(map_int(subscribers$tags, function(tags) {
        sum(FREQUENCY_TAGS %in% tags)
    }) > 1)

    if (conflicting_frequency_count > 0) {
        cat(
            "Resolved",
            conflicting_frequency_count,
            "subscriber(s) with multiple frequency tags using least-frequent precedence\n"
        )
    }

    # Buttondown's subscriber tag filter expects tag UUIDs, not tag names.
    # Filter locally because subscriber records already include tag names.
    subscribers <- subscribers[
        subscribers$resolved_frequency == freq_tag,
        ,
        drop = FALSE
    ]

    if (nrow(subscribers) == 0) {
        cat("No subscribers found for frequency:", frequency, "\n")
        return(subscribers)
    }

    cat("Found", nrow(subscribers), "subscribers\n")

    # Extract discipline tags for each subscriber
    subscribers$disciplines <- map(subscribers$tags, function(tags) {
        disc_tags <- tags[grepl("^disc-", tags)]
        if (length(disc_tags) == 0) {
            # Default: all disciplines if none specified
            return(names(DISCIPLINE_MAP))
        }
        return(disc_tags)
    })

    return(subscribers)
}

#' Format an error returned by the Buttondown API
#' @param response httr response object
#' @return Human-readable error detail
get_buttondown_error <- function(response) {
    parsed <- tryCatch(
        content(response, "parsed", type = "application/json"),
        error = function(e) NULL
    )

    if (is.list(parsed)) {
        fields <- unlist(parsed[c("code", "detail")], use.names = FALSE)
        fields <- fields[!is.na(fields) & nzchar(fields)]
        if (length(fields) > 0) return(paste(fields, collapse = ": "))
    }

    response_text <- tryCatch(
        content(response, "text", encoding = "UTF-8"),
        error = function(e) ""
    )
    if (!nzchar(response_text)) return("No response detail")
    substr(response_text, 1, 500)
}

#' Create a draft email without broadcasting it
#' @param api_key Buttondown API key
#' @param subject Email subject
#' @param body Email body (markdown)
#' @return Buttondown email ID
create_buttondown_email <- function(api_key, subject, body) {
    request_body <- list(
        subject = subject,
        body = body,
        email_type = "private",
        status = "draft"
    )
    idempotency_key <- paste0(
        "picnic-email-",
        digest::digest(paste(subject, body, sep = "\n"), algo = "sha256")
    )

    response <- POST(
        url = paste0(BUTTONDOWN_API_URL, "/emails"),
        add_headers(
            Authorization = paste("Token", api_key),
            `X-API-Version` = BUTTONDOWN_API_VERSION,
            `X-Idempotency-Key` = idempotency_key
        ),
        body = request_body,
        encode = "json"
    )

    if (status_code(response) != 201) {
        stop(
            "Failed to create Buttondown email (",
            status_code(response),
            "): ",
            get_buttondown_error(response),
            call. = FALSE
        )
    }

    email <- content(response, "parsed", type = "application/json")
    if (is.null(email$id) || !nzchar(email$id)) {
        stop("Buttondown created an email without returning its ID", call. = FALSE)
    }
    email$id
}

#' Send a draft email to a group of subscribers
#' @param api_key Buttondown API key
#' @param email_id Buttondown email ID
#' @param subscriber_ids Buttondown subscriber IDs
#' @param frequency Digest frequency
#' @param post_fn HTTP POST function, injectable for tests
#' @return TRUE on success
send_draft_to_subscribers <- function(
    api_key,
    email_id,
    subscriber_ids,
    frequency,
    post_fn = POST
) {
    subscriber_ids <- sort(as.character(subscriber_ids))
    if (length(subscriber_ids) == 0) {
        stop("At least one subscriber ID is required", call. = FALSE)
    }
    email_path <- URLencode(email_id, reserved = TRUE)
    idempotency_key <- paste0(
        "picnic-send-draft-",
        digest::digest(
            paste(frequency, email_id, subscriber_ids, collapse = "\n"),
            algo = "sha256"
        )
    )

    response <- post_fn(
        url = paste0(
            BUTTONDOWN_API_URL,
            "/emails/", email_path,
            "/send-draft"
        ),
        add_headers(
            Authorization = paste("Token", api_key),
            `X-API-Version` = BUTTONDOWN_API_VERSION,
            `X-Idempotency-Key` = idempotency_key
        ),
        # A nested list forces jsonlite to preserve an array for one subscriber.
        body = list(subscribers = unname(as.list(subscriber_ids))),
        encode = "json"
    )

    success <- status_code(response) == 200
    if (!success) {
        warning(
            "Failed to send draft ",
            email_id,
            " to ",
            length(subscriber_ids),
            " subscriber(s)",
            " (",
            status_code(response),
            "): ",
            get_buttondown_error(response)
        )
    }
    return(success)
}

# ============================================================================
# Content Loading Functions
# ============================================================================

#' Load articles from JSON file
load_articles <- function(discipline_file) {
    file_path <- file.path(OUTPUT_DIR, paste0(discipline_file, ".json"))
    if (!file.exists(file_path)) {
        return(NULL)
    }

    data <- fromJSON(file_path)

    # Handle must_read which has different structure
    if (discipline_file == "must_read") {
        if (is.data.frame(data)) {
            data$discipline <- "must_read"
            return(data)
        }
        return(NULL)
    }

    journal_entries <- if (is.data.frame(data$content)) {
        split(data$content, seq_len(nrow(data$content)))
    } else {
        data$content
    }

    # Extract articles from all journals
    all_articles <- map_dfr(journal_entries, function(journal) {
        if (is.data.frame(journal)) {
            articles <- journal$articles[[1]]
            journal_full <- journal$journal_full[[1]]
            journal_short <- journal$journal_short[[1]]
        } else {
            articles <- journal$articles
            journal_full <- journal$journal_full
            journal_short <- journal$journal_short
        }

        if (is.null(articles) || length(articles) == 0) {
            return(NULL)
        }
        if (is.data.frame(articles) && nrow(articles) == 0) {
            return(NULL)
        }

        articles$journal_full <- journal_full
        articles$journal_short <- journal_short
        articles$discipline <- discipline_file
        return(articles)
    })

    return(all_articles)
}

#' Filter articles by date
filter_by_date <- function(articles, since_date) {
    if (is.null(articles) || nrow(articles) == 0) {
        return(articles)
    }

    if ("created" %in% names(articles)) {
        articles <- articles %>%
            filter(as.Date(created) > since_date)
    }

    return(articles)
}

#' Load and filter articles for specified disciplines
load_content_for_disciplines <- function(discipline_tags, since_date) {
    all_content <- list()

    for (tag in discipline_tags) {
        if (!tag %in% names(DISCIPLINE_MAP)) next

        disc_info <- DISCIPLINE_MAP[[tag]]
        articles <- load_articles(disc_info$file)
        articles <- filter_by_date(articles, since_date)

        if (!is.null(articles) && nrow(articles) > 0) {
            all_content[[tag]] <- list(
                name = disc_info$name,
                articles = articles
            )
        }
    }

    return(all_content)
}

# ============================================================================
# Email Content Generation
# ============================================================================

#' Generate markdown email body for given content
generate_email_body <- function(content, subscriber_email = NULL) {
    if (length(content) == 0) {
        return(NULL)
    }

    sections <- list()
    total_articles <- 0

    # Must-read first if present
    if ("disc-mustread" %in% names(content)) {
        must_read <- content[["disc-mustread"]]
        section_md <- "## Must-Read Picks\n\n"
        section_md <- paste0(section_md, "*AI-curated highlights from recent publications*\n\n")

        for (i in seq_len(nrow(must_read$articles))) {
            article <- must_read$articles[i, ]
            section_md <- paste0(
                section_md,
                "**[", article$title, "](", article$url, ")**\n\n",
                if (!is.null(article$authors)) paste0(article$authors, "\n\n") else "",
                if (!is.null(article$journal_full)) paste0("*", article$journal_full, "*\n\n") else ""
            )

            if ("abstract" %in% names(article) && !is.na(article$abstract) && article$abstract != "") {
                abstract <- article$abstract
                if (nchar(abstract) > 400) {
                    abstract <- paste0(substr(abstract, 1, 397), "...")
                }
                section_md <- paste0(section_md, "> ", abstract, "\n\n")
            }
            section_md <- paste0(section_md, "---\n\n")
        }

        sections[["mustread"]] <- section_md
        total_articles <- total_articles + nrow(must_read$articles)
        content[["disc-mustread"]] <- NULL  # Remove so we do not duplicate
    }

    # Other discipline sections
    for (tag in names(content)) {
        disc <- content[[tag]]
        section_md <- paste0("## ", disc$name, "\n\n")

        articles <- disc$articles

        # Group by journal if available
        if ("journal_full" %in% names(articles)) {
            by_journal <- split(articles, articles$journal_full)

            for (journal_name in names(by_journal)) {
                journal_articles <- by_journal[[journal_name]]
                section_md <- paste0(section_md, "### ", journal_name, "\n\n")

                for (i in seq_len(nrow(journal_articles))) {
                    article <- journal_articles[i, ]
                    section_md <- paste0(
                        section_md,
                        "**[", article$title, "](", article$url, ")**\n\n"
                    )

                    if ("authors" %in% names(article) && !is.na(article$authors)) {
                        section_md <- paste0(section_md, article$authors, "\n\n")
                    }

                    if ("abstract" %in% names(article) && !is.na(article$abstract) && article$abstract != "") {
                        abstract <- article$abstract
                        if (nchar(abstract) > 300) {
                            abstract <- paste0(substr(abstract, 1, 297), "...")
                        }
                        section_md <- paste0(section_md, "> ", abstract, "\n\n")
                    }
                }
            }
        } else {
            # No journal grouping (e.g., must_read)
            for (i in seq_len(nrow(articles))) {
                article <- articles[i, ]
                section_md <- paste0(
                    section_md,
                    "**[", article$title, "](", article$url, ")**\n\n"
                )
                if ("authors" %in% names(article) && !is.na(article$authors)) {
                    section_md <- paste0(section_md, article$authors, "\n\n")
                }
            }
        }

        sections[[tag]] <- section_md
        total_articles <- total_articles + nrow(articles)
    }

    if (total_articles == 0) {
        return(NULL)
    }

    # Combine sections
    body <- paste0(
        "# Paper Picnic Digest\n\n",
        paste(unlist(sections), collapse = "\n"),
        "\n---\n\n",
        "*[Manage your subscription preferences]({{ manage_subscription_url }})*\n"
    )

    return(list(
        body = body,
        article_count = total_articles
    ))
}

#' Get date range description for subject line
get_date_range_description <- function(since_date) {
    today <- Sys.Date()
    days_diff <- as.numeric(today - since_date)

    if (days_diff == 1) {
        return(format(today, "%b %d"))
    } else {
        return(paste0(format(since_date + 1, "%b %d"), "-", format(today, "%b %d")))
    }
}

# ============================================================================
# Subscriber Grouping Strategy
# ============================================================================

#' Group subscribers by their discipline preferences
#' Returns a list of groups, each with a unique discipline combination
group_subscribers_by_disciplines <- function(subscribers) {
    # Create a signature for each subscriber's discipline set
    subscribers$disc_signature <- map_chr(subscribers$disciplines, function(discs) {
        paste(sort(discs), collapse = "|")
    })

    # Group by signature
    groups <- split(subscribers, subscribers$disc_signature)

    cat("Created", length(groups), "subscriber groups based on discipline preferences\n")

    return(groups)
}

# ============================================================================
# Main Execution
# ============================================================================

main <- function() {
    args <- commandArgs(trailingOnly = TRUE)

    frequency <- if (length(args) > 0) args[1] else "weekly"
    dry_run <- "--dry-run" %in% args

    if (!frequency %in% c("daily", "weekly", "monthly")) {
        stop("Frequency must be 'daily', 'weekly', or 'monthly'")
    }

    cat("===========================================\n")
    cat("Paper Picnic Email Digest\n")
    cat("Frequency:", frequency, "\n")
    if (dry_run) cat("MODE: DRY RUN (no emails will be sent)\n")
    cat("===========================================\n\n")

    api_key <- get_buttondown_api_key()

    # Get last sent date for this frequency
    since_date <- get_last_sent_date(frequency)
    cat("Including articles since:", as.character(since_date), "\n\n")

    # Fetch subscribers for this frequency tier
    subscribers <- get_subscribers_for_frequency(api_key, frequency)

    if (nrow(subscribers) == 0) {
        cat("No subscribers to send to. Exiting.\n")
        return(invisible(NULL))
    }

    # Group subscribers by discipline preferences
    groups <- group_subscribers_by_disciplines(subscribers)

    # Track statistics
    total_sent <- 0
    total_failed <- 0

    # Process each group
    for (group_sig in names(groups)) {
        group <- groups[[group_sig]]
        discipline_tags <- group$disciplines[[1]]  # All in group have same disciplines

        cat("\n--- Processing group (", nrow(group), " subscribers) ---\n")
        cat("Disciplines:", paste(discipline_tags, collapse = ", "), "\n")

        # Load content for this group's disciplines
        content <- load_content_for_disciplines(discipline_tags, since_date)

        if (length(content) == 0) {
            cat("No new content for this group. Skipping.\n")
            next
        }

        # Generate email body
        email <- generate_email_body(content)

        if (is.null(email)) {
            cat("No content to send. Skipping.\n")
            next
        }

        cat("Generated email with", email$article_count, "articles\n")

        # Generate subject
        date_range <- get_date_range_description(since_date)
        subject <- paste0("Paper Picnic: ", email$article_count, " new articles (", date_range, ")")

        if (dry_run) {
            cat("\n[DRY RUN] Subject:", subject, "\n")
            cat("[DRY RUN] Would send to", nrow(group), "subscribers:\n")
            cat(paste("  -", group$email, collapse = "\n"), "\n")
            cat("[DRY RUN] Body preview (first 500 chars):\n")
            cat(substr(email$body, 1, 500), "...\n")
            total_sent <- total_sent + nrow(group)
        } else {
            email_id <- create_buttondown_email(api_key, subject, email$body)
            cat("Created Buttondown email:", email_id, "\n")

            success <- send_draft_to_subscribers(
                api_key = api_key,
                email_id = email_id,
                subscriber_ids = group$id,
                frequency = frequency
            )

            if (success) {
                total_sent <- total_sent + nrow(group)
            } else {
                total_failed <- total_failed + nrow(group)
            }
        }
    }

    # Summary
    cat("\n===========================================\n")
    cat("Summary\n")
    cat("===========================================\n")
    cat("Total emails sent:", total_sent, "\n")
    if (total_failed > 0) cat("Failed:", total_failed, "\n")

    # Only advance the cursor after every attempted delivery succeeds.
    if (total_sent > 0 && total_failed == 0 && !dry_run) {
        update_last_sent_date(frequency)
        cat("Updated last sent date to:", as.character(Sys.Date()), "\n")
    }

    if (total_failed > 0) {
        stop(total_failed, " Buttondown delivery attempt(s) failed", call. = FALSE)
    }

    return(invisible(total_sent > 0))
}

# Run if called directly
if (!interactive()) {
    main()
}
