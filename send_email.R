# Email Digest Sender for Communication Paper Picnic
# ---------------------------------------------------
# Sends email digests via Buttondown API based on subscriber preferences
#
# Usage: Rscript send_email.R [frequency]
#   frequency: "daily", "weekly", or "monthly"
#
# Environment variables required:
#   BUTTONDOWN_API_KEY - Buttondown API key

library(httr)
library(jsonlite)
library(dplyr)
library(purrr)

# Configuration
BUTTONDOWN_API_URL <- "https://api.buttondown.com/v1"
MEMORY_DIR <- "./memory"
OUTPUT_DIR <- "./output"

# Discipline mapping
DISCIPLINES <- list(
    communication = "Communication",
    politics = "Politics",
    po = "Public Opinion",
    psych = "Psychology",
    sociology = "Sociology",
    multidisciplinary = "Multidisciplinary"
)

# Get API key from environment
get_buttondown_api_key <- function() {
    key <- Sys.getenv("BUTTONDOWN_API_KEY")
    if (key == "") {
        # Try loading from credentials.R for local testing
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
    if (file.exists(file_path)) {
        date_str <- trimws(readLines(file_path, n = 1))
        return(as.Date(date_str))
    }
    # Default to 7 days ago if no record exists
    return(Sys.Date() - 7)
}

# Update last sent timestamp
update_last_sent_date <- function(frequency, date = Sys.Date()) {
    file_path <- file.path(MEMORY_DIR, paste0("last_email_", frequency, ".txt"))
    writeLines(as.character(date), file_path)
}

# Load articles from JSON file
load_articles <- function(discipline) {
    file_path <- file.path(OUTPUT_DIR, paste0(discipline, ".json"))
    if (!file.exists(file_path)) {
        return(NULL)
    }

    data <- fromJSON(file_path)

    # Extract articles from all journals
    all_articles <- map_dfr(data$content, function(journal) {
        articles <- journal$articles
        if (is.null(articles) || length(articles) == 0 || nrow(articles) == 0) {
            return(NULL)
        }
        articles$journal_full <- journal$journal_full
        articles$journal_short <- journal$journal_short
        articles$discipline <- discipline
        return(articles)
    })

    return(all_articles)
}

# Load must-read articles
load_must_read <- function() {
    file_path <- file.path(OUTPUT_DIR, "must_read.json")
    if (!file.exists(file_path)) {
        return(NULL)
    }

    data <- fromJSON(file_path)
    if (is.data.frame(data)) {
        data$is_must_read <- TRUE
        return(data)
    }
    return(NULL)
}

# Load preprints
load_preprints <- function() {
    file_path <- file.path(OUTPUT_DIR, "preprints.json")
    if (!file.exists(file_path)) {
        return(NULL)
    }

    data <- fromJSON(file_path)

    # Preprints have a different structure
    all_preprints <- map_dfr(data$content, function(server) {
        articles <- server$articles
        if (is.null(articles) || length(articles) == 0 || nrow(articles) == 0) {
            return(NULL)
        }
        articles$journal_full <- server$journal_full
        articles$journal_short <- server$journal_short
        articles$discipline <- "preprints"
        return(articles)
    })

    return(all_preprints)
}

# Filter articles by date
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

# Generate markdown content for a single article
format_article_md <- function(article) {
    md <- paste0(
        "### [", article$title, "](", article$url, ")\n\n",
        "**", article$authors, "** — *", article$journal_full, "*\n\n"
    )

    if (!is.na(article$abstract) && article$abstract != "") {
        # Truncate long abstracts
        abstract <- article$abstract
        if (nchar(abstract) > 500) {
            abstract <- paste0(substr(abstract, 1, 497), "...")
        }
        md <- paste0(md, abstract, "\n\n")
    }

    md <- paste0(md, "---\n\n")
    return(md)
}

# Generate email content for specific disciplines
generate_email_content <- function(disciplines, since_date, include_must_read = TRUE) {
    sections <- list()
    total_articles <- 0

    # Must-read section first
    if (include_must_read) {
        must_read <- load_must_read()
        must_read <- filter_by_date(must_read, since_date)

        if (!is.null(must_read) && nrow(must_read) > 0) {
            section_md <- "## 🌟 Must-Read Picks\n\n"
            section_md <- paste0(section_md, "*AI-curated highlights from this week's publications*\n\n")

            for (i in seq_len(nrow(must_read))) {
                section_md <- paste0(section_md, format_article_md(must_read[i, ]))
            }

            sections[["must_read"]] <- section_md
            total_articles <- total_articles + nrow(must_read)
        }
    }

    # Discipline sections
    for (disc in disciplines) {
        if (disc == "preprints") {
            articles <- load_preprints()
        } else {
            articles <- load_articles(disc)
        }

        articles <- filter_by_date(articles, since_date)

        if (!is.null(articles) && nrow(articles) > 0) {
            disc_name <- if (disc == "preprints") "Preprints" else DISCIPLINES[[disc]]
            section_md <- paste0("## ", disc_name, "\n\n")

            # Group by journal
            by_journal <- split(articles, articles$journal_full)

            for (journal_name in names(by_journal)) {
                journal_articles <- by_journal[[journal_name]]
                section_md <- paste0(section_md, "### ", journal_name, "\n\n")

                for (i in seq_len(nrow(journal_articles))) {
                    article <- journal_articles[i, ]
                    section_md <- paste0(
                        section_md,
                        "**[", article$title, "](", article$url, ")**\n\n",
                        article$authors, "\n\n"
                    )

                    if (!is.na(article$abstract) && article$abstract != "") {
                        abstract <- article$abstract
                        if (nchar(abstract) > 300) {
                            abstract <- paste0(substr(abstract, 1, 297), "...")
                        }
                        section_md <- paste0(section_md, "> ", abstract, "\n\n")
                    }
                }
            }

            sections[[disc]] <- section_md
            total_articles <- total_articles + nrow(articles)
        }
    }

    if (total_articles == 0) {
        return(NULL)
    }

    # Combine sections
    body <- paste0(
        "# Paper Picnic Digest\n\n",
        "Here are the latest articles from your subscribed disciplines.\n\n",
        paste(unlist(sections), collapse = "\n"),
        "\n---\n\n",
        "*You're receiving this because you subscribed to Paper Picnic. ",
        "[Manage your preferences]({{ manage_subscription_url }})*\n"
    )

    return(list(
        body = body,
        article_count = total_articles
    ))
}

# Get date range description
get_date_range_description <- function(since_date) {
    today <- Sys.Date()
    days_diff <- as.numeric(today - since_date)

    if (days_diff == 1) {
        return("today")
    } else if (days_diff <= 7) {
        return(paste0(format(since_date + 1, "%b %d"), " - ", format(today, "%b %d")))
    } else {
        return(paste0(format(since_date + 1, "%b %d"), " - ", format(today, "%b %d")))
    }
}

# Send email via Buttondown API
send_email <- function(subject, body, tag_filter = NULL, dry_run = FALSE) {
    api_key <- get_buttondown_api_key()

    if (dry_run) {
        cat("=== DRY RUN ===\n")
        cat("Subject:", subject, "\n")
        cat("Tag filter:", tag_filter, "\n")
        cat("Body preview:\n")
        cat(substr(body, 1, 500), "...\n")
        return(TRUE)
    }

    # Build request body
    request_body <- list(
        subject = subject,
        body = body,
        email_type = "public"
    )

    # Note: Tag filtering via the new filters API would go here
    # For now, we send to all subscribers and rely on the subscription
    # form to only include people who want this content

    response <- POST(
        url = paste0(BUTTONDOWN_API_URL, "/emails"),
        add_headers(
            Authorization = paste("Token", api_key),
            `Content-Type` = "application/json"
        ),
        body = toJSON(request_body, auto_unbox = TRUE),
        encode = "raw"
    )

    if (status_code(response) %in% c(200, 201)) {
        cat("Email sent successfully!\n")
        return(TRUE)
    } else {
        cat("Error sending email:", status_code(response), "\n")
        cat(content(response, "text"), "\n")
        return(FALSE)
    }
}

# Main execution
main <- function() {
    args <- commandArgs(trailingOnly = TRUE)

    frequency <- if (length(args) > 0) args[1] else "weekly"
    dry_run <- "--dry-run" %in% args

    if (!frequency %in% c("daily", "weekly", "monthly")) {
        stop("Frequency must be 'daily', 'weekly', or 'monthly'")
    }

    cat("Running email digest for frequency:", frequency, "\n")
    if (dry_run) cat("(DRY RUN - no emails will be sent)\n")

    # Get last sent date
    since_date <- get_last_sent_date(frequency)
    cat("Filtering articles since:", as.character(since_date), "\n")

    # For now, include all disciplines
    # In future, this could be customized per subscriber segment
    all_disciplines <- c(names(DISCIPLINES), "preprints")

    # Generate content
    content <- generate_email_content(all_disciplines, since_date, include_must_read = TRUE)

    if (is.null(content)) {
        cat("No new articles to send.\n")
        return(invisible(NULL))
    }

    cat("Found", content$article_count, "new articles\n")

    # Generate subject line
    date_range <- get_date_range_description(since_date)
    subject <- paste0("Paper Picnic: ", content$article_count, " new articles (", date_range, ")")

    # Send email
    success <- send_email(
        subject = subject,
        body = content$body,
        dry_run = dry_run
    )

    # Update last sent date on success
    if (success && !dry_run) {
        update_last_sent_date(frequency)
        cat("Updated last sent date to:", as.character(Sys.Date()), "\n")
    }

    return(invisible(success))
}

# Run if called directly
if (!interactive()) {
    main()
}
