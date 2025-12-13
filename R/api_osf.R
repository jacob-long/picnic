# OSF (Open Science Framework) API Functions
# -------------------------------------------

#' Fetch preprints from OSF API
#' @param provider Preprint provider name (e.g., "psyarxiv")
#' @param start_date Start date for filtering (default: 7 days ago)
#' @param max_results Maximum number of results to return
#' @return Data frame of preprints
get_all_osf_preprints <- function(provider, start_date = as.Date(Sys.time()) - 7, max_results = 1000) {
    base_url <- "https://api.osf.io/v2/preprints/"
    all_preprints <- data.frame()
    next_page_url <- NULL

    while (nrow(all_preprints) < max_results) {
        query <- list(
            "filter[provider]" = provider,
            "filter[date_created][gte]" = as.character(start_date),
            "page[size]" = 100
        )

        if (!is.null(next_page_url)) {
            response <- perform_with_retries(httr2::request(next_page_url))
        } else {
            response <- perform_with_retries(httr2::request(base_url) |> httr2::req_url_query(!!!query))
        }

        if (is.null(response)) {
            warning("OSF API unavailable or rate limited; skipping preprint retrieval.")
            return(all_preprints)
        }

        data <- httr2::resp_body_json(response)

        preprints <- purrr::map_df(data$data, function(item) {
            contributors_url <- item$relationships$contributors$links$related$href
            contributors_response <- purrr::safely(function(req) perform_with_retries(req))(httr2::request(contributors_url))

            authors <- if (!is.null(contributors_response$result)) {
                contributors_data <- httr2::resp_body_json(contributors_response$result)
                author_names <- purrr::map_chr(contributors_data$data, function(contributor) {
                    user_data <- contributor$embeds$users$data$attributes

                    if (!is.null(user_data) && is.list(user_data)) {
                        given <- ifelse(is.null(user_data$given_name), "", user_data$given_name)
                        middle <- ifelse(is.null(user_data$middle_names), "", user_data$middle_names)
                        family <- ifelse(is.null(user_data$family_name), "", user_data$family_name)

                        full_name <- paste(given, middle, family) |>
                            trimws() |>
                            gsub("\\s+", " ", x = _)

                        if (full_name != "") {
                            return(full_name)
                        } else {
                            return(NA_character_)
                        }
                    } else {
                        return(NA_character_)
                    }
                })
                paste(author_names, collapse = "; ")
            } else {
                "No authors listed"
            }

            doi <- item$links$preprint_doi
            if (is.null(doi)) {
                doi <- item$attributes$doi
            }
            if (is.null(doi)) {
                doi <- "No DOI available"
            }

            data.frame(
                id = item$id,
                title = item$attributes$title,
                date_created = as.POSIXct(item$attributes$date_created, format = "%Y-%m-%dT%H:%M:%S"),
                abstract = item$attributes$description,
                authors = authors,
                doi = doi,
                stringsAsFactors = FALSE
            )
        })

        all_preprints <- rbind(all_preprints, preprints)

        next_page_url <- data$links$`next`
        if (is.null(next_page_url)) break
    }

    if (nrow(all_preprints) > max_results) {
        all_preprints <- all_preprints[1:max_results, ]
    }

    return(all_preprints)
}
