# Output/JSON Rendering Functions
# --------------------------------

source("R/constants.R")

#' Remove characters that are valid JSON but rejected by Jekyll/Psych.
#' @param x Character vector
#' @return Sanitized character vector
sanitize_json_text <- function(x) {
    if (!is.character(x)) {
        return(x)
    }

    sanitize_one_json_text <- function(text) {
        if (is.na(text)) {
            return(NA_character_)
        }

        codepoints <- utf8ToInt(enc2utf8(text))
        if (length(codepoints) == 0) {
            return(text)
        }

        low_16_bits <- codepoints %% 65536L
        keep <- !(
            (codepoints >= 0L & codepoints <= 8L) |
            codepoints %in% c(11L, 12L) |
            (codepoints >= 14L & codepoints <= 31L) |
            (codepoints >= 127L & codepoints <= 159L) |
            (codepoints >= 64976L & codepoints <= 65007L) |
            low_16_bits %in% c(65534L, 65535L)
        )

        if (!any(keep)) {
            return("")
        }

        intToUtf8(codepoints[keep])
    }

    vapply(x, sanitize_one_json_text, FUN.VALUE = character(1), USE.NAMES = FALSE)
}

#' Recursively sanitize nested data before JSON serialization.
#' @param x Object to sanitize
#' @return Sanitized object
sanitize_json_data <- function(x) {
    if (is.data.frame(x)) {
        x[] <- lapply(x, sanitize_json_data)
        return(x)
    }

    if (is.list(x)) {
        return(lapply(x, sanitize_json_data))
    }

    if (is.character(x)) {
        return(sanitize_json_text(x))
    }

    x
}

#' Render articles data frame to JSON format
#' @param df Data frame of articles
#' @param date Update date
#' @return JSON string
render_json <- function(df, date) {
    df <- split(df, df$journal_full)
    to_json <- list()

    # Include 'created' date for email filtering if available
    article_fields <- c("title", "authors", "abstract", "url", "doi", "filter")
    if ("created" %in% names(df[[1]])) {
        article_fields <- c(article_fields, "created")
    }

    for (i in 1:length(df)) {
        articles <- df[[i]]
        journal_full <- unique(articles$journal_full)
        journal_short <- unique(articles$journal_short)
        articles <- articles[intersect(article_fields, names(articles))]
        articles_hidden <- subset(articles, !(filter == FILTER_INCLUDE | filter == FILTER_ERROR))
        articles_hidden <- sort_by(articles_hidden, articles_hidden$filter)
        articles <- subset(articles, (filter == FILTER_INCLUDE | filter == FILTER_ERROR))
        to_json[[i]] <- list(
            "journal_full" = journal_full,
            "journal_short" = journal_short,
            "articles" = articles,
            "articles_hidden" = articles_hidden
        )
    }

    to_json <- list("update" = date, "content" = to_json)
    to_json <- sanitize_json_data(to_json)
    json <- jsonlite::toJSON(to_json, pretty = TRUE, auto_unbox = TRUE)
    return(json)
}

#' Render preprints data frame to JSON format (single journal)
#' @param df Data frame of preprints
#' @param date Update date
#' @return JSON string
render_json_pre <- function(df, date) {
    to_json <- list()
    articles <- df

    # Normalize date field name for preprints
    if ("date_created" %in% names(articles) && !"created" %in% names(articles)) {
        articles$created <- as.character(as.Date(articles$date_created))
    }

    journal_full <- unique(articles$journal_full)
    journal_short <- unique(articles$journal_short)

    # Include 'created' date for email filtering if available
    article_fields <- c("title", "authors", "abstract", "url", "doi", "filter")
    if ("created" %in% names(articles)) {
        article_fields <- c(article_fields, "created")
    }

    articles <- articles[intersect(article_fields, names(articles))]
    articles_hidden <- subset(articles, !(filter == FILTER_INCLUDE | filter == FILTER_ERROR))
    articles_hidden <- sort_by(articles_hidden, articles_hidden$filter)
    articles <- subset(articles, (filter == FILTER_INCLUDE | filter == FILTER_ERROR))
    to_json <- list(
        "journal_full" = journal_full,
        "journal_short" = journal_short,
        "articles" = articles,
        "articles_hidden" = articles_hidden
    )
    to_json <- list("update" = date, "content" = to_json)
    to_json <- sanitize_json_data(to_json)
    json <- jsonlite::toJSON(to_json, pretty = TRUE, auto_unbox = TRUE)
    return(json)
}
