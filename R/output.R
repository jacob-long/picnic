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

    for (i in 1:length(df)) {
        articles <- df[[i]]
        journal_full <- unique(articles$journal_full)
        journal_short <- unique(articles$journal_short)
        articles <- articles[c("title", "authors", "abstract", "url", "doi", "filter")]
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
    journal_full <- unique(articles$journal_full)
    journal_short <- unique(articles$journal_short)
    articles <- articles[c("title", "authors", "abstract", "url", "doi", "filter")]
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
