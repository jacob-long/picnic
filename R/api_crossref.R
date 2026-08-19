# Crossref API Functions
# ----------------------

#' Call Crossref API for journal/DOI works
#' @param id ISSN or DOI prefix
#' @param type Either "issn" or "doi"
#' @param start Start date for filtering
#' @param end End date for filtering
#' @param date_type Either "created" or "published"
#' @param rows Maximum number of rows to return
#' @return Parsed API response content
call_crossref_api <- function(id, type = "issn", start, end, date_type = "created", rows = 1000) {
    if (!type %in% c("issn", "doi")) stop("type must be either 'issn' or 'doi'")
    if (!date_type %in% c("created", "published")) stop("date_type must be either 'created' or 'published'")

    endpoint <- if (type == "issn") {
        paste0("https://api.crossref.org/journals/", id, "/works")
    } else {
        paste0("https://api.crossref.org/prefixes/", id, "/works")
    }

    filter <- if (date_type == "created") {
        paste0("from-created-date:", start, ",until-created-date:", end)
    } else {
        paste0("from-pub-date:", start, ",until-pub-date:", end)
    }

    param <- list(
        "filter" = filter,
        "select" = "title,author,abstract,URL,created",
        "mailto" = crossref_email,
        rows = rows
    )

    res <- httr::GET(endpoint, query = param)
    return(httr::content(res))
}

#' Retrieve articles from Crossref for multiple ISSNs
#' @param issn_list Vector of ISSN numbers
#' @param start_date Start date
#' @param end_date End date
#' @param verbose Print progress
#' @return Data frame of articles
retrieve_crossref_issn_data <- function(issn_list, start_date, end_date, verbose = FALSE) {
    K <- length(issn_list)
    out <- list()

    for (i in seq_len(K)) {
        issn <- issn_list[i]
        if (verbose) cat(issn, "\n")
        j <- 0
        tmp <- list()
        for (type in c("created", "published")) {
            j <- j + 1
            tmp[[j]] <- call_crossref_api(
                id = issn,
                type = "issn",
                start = start_date,
                end = end_date,
                date_type = type
            )
        }
        tmp <- rbind(
            get_crossref_articles(tmp[[1]]),
            get_crossref_articles(tmp[[2]])
        )
        if (is.null(tmp) || nrow(tmp) == 0) next

        tmp$issn <- issn
        out[[i]] <- tmp[!duplicated(tmp$url), , drop = FALSE]
    }

    out <- Filter(Negate(is.null), out)
    if (length(out) == 0) return(NULL)

    out <- do.call(rbind, out)
    rownames(out) <- NULL
    return(out)
}

#' Parse Crossref API response into data frame
#' @param items API response content
#' @return Data frame of articles
get_crossref_articles <- function(items) {
    item_list <- items$message$items
    if (is.null(item_list) || length(item_list) == 0) return(NULL)

    rows <- lapply(item_list, function(item) {
        info <- get_crossref_article_info(item)
        as.data.frame(as.list(info), stringsAsFactors = FALSE)
    })

    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    return(out)
}

#' Extract article info from Crossref item
#' @param item Single item from Crossref response
#' @return Named vector of article metadata
get_crossref_article_info <- function(item) {
    return(c(
        title = get_crossref_title(item),
        authors = get_crossref_authors(item),
        created = get_crossref_date(item, "created"),
        abstract = get_crossref_abstract(item),
        url = get_crossref_url(item)
    ))
}

get_crossref_abstract <- function(item) {
    if (is.null(item$abstract) || length(item$abstract) == 0) return(NA_character_)
    return(paste(unlist(item$abstract, use.names = FALSE), collapse = " "))
}

get_crossref_authors <- function(item) {
    if (is.null(item$author) || length(item$author) == 0) return(NA_character_)
    return(paste(vapply(item$author, get_crossref_author, character(1)), collapse = ", "))
}

get_crossref_author <- function(item) {
    parts <- c(item$given, item$family)
    parts <- unlist(parts, use.names = FALSE)
    parts <- parts[!is.na(parts) & nzchar(parts)]
    if (length(parts) == 0) return(NA_character_)
    paste(parts, collapse = " ")
}

get_crossref_date <- function(item, name) {
    if (is.null(item[[name]])) return(NA_character_)
    date_parts <- unlist(item[[name]][["date-parts"]], use.names = FALSE)
    if (length(date_parts) == 0) return(NA_character_)
    return(paste(date_parts, collapse = "-"))
}

get_crossref_title <- function(item) {
    if (is.null(item$title) || length(item$title) == 0) return(NA_character_)
    return(paste(unlist(item$title, use.names = FALSE), collapse = " / "))
}

get_crossref_journal <- function(item) {
    if (is.null(item$`container-title`) || length(item$`container-title`) == 0) {
        return(NA_character_)
    }
    return(paste(unlist(item$`container-title`, use.names = FALSE), collapse = " / "))
}

get_crossref_url <- function(item) {
    if (is.null(item$URL) || length(item$URL) == 0) return(NA_character_)
    return(as.character(item$URL[[1]]))
}

get_crossref_api_limits <- function(response) {
    out <- httr::headers(response)
    limit <- out$`x-ratelimit-limit`
    interval <- out$`x-ratelimit-interval`
    return(c("limit" = limit, "interval" = interval))
}
