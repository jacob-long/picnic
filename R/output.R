# Output/JSON Rendering Functions
# --------------------------------

source("R/constants.R")

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
    json <- jsonlite::toJSON(to_json, pretty = TRUE, auto_unbox = TRUE)
    return(json)
}
