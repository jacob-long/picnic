# Article Filtering Functions
# ---------------------------

source("R/constants.R")

#' Apply AI-based filter to a single article row
#' @param row Named vector of article data
#' @param system_prompt System prompt for classification
#' @param content_field Name of field containing text to classify ("abstract" or "description")
#' @param api Either "openai" or "gemini"
#' @param model Model identifier
#' @return Filter code (0 = include, 2 = exclude, -1 = error)
add_ai_filter <- function(row, system_prompt, content_field = "abstract",
                          api = "openai", model = "gpt-4o-mini") {
    row_nam <- names(row)
    cat(row[row_nam == "url"], "\n")
    row[row_nam == "filter"] <- as.integer(row[row_nam == "filter"])

    if (row[row_nam == "filter"] != FILTER_INCLUDE) {
        return(row[row_nam == "filter"])
    }

    user_prompt <- paste(
        "Journal Name:", row[row_nam == "journal_full"], "\n",
        "Title:", row[row_nam == "title"], "\n",
        row[row_nam == content_field]
    )

    if (api == "openai") {
        res <- call_openai_api(
            system_prompt = system_prompt,
            user_prompt = user_prompt,
            model = model
        )
        if (get_openai_finish_reason(res) != "stop") return(FILTER_ERROR)
        if (tolower(get_openai_response(res)) == "no") return(FILTER_AI_EXCLUDED)
    } else if (api == "gemini") {
        res <- call_gemini_api(
            system_prompt = system_prompt,
            user_text = user_prompt,
            model = model
        )
        if (is.null(res)) return(FILTER_ERROR)
        if (tolower(res) == "no") return(FILTER_AI_EXCLUDED)
    }

    return(FILTER_INCLUDE)
}

#' Filter for multidisciplinary journals using AI classification
#' @param row Named vector of article data
#' @return Filter code
add_multidisciplinary_filter <- function(row) {
    add_ai_filter(
        row = row,
        system_prompt = prompt_socsci_classifier,
        content_field = "abstract",
        api = "openai",
        model = "gpt-4o-mini"
    )
}

#' Filter for preprints using AI classification
#' @param row Named vector of article data
#' @return Filter code
add_preprint_filter <- function(row) {
    add_ai_filter(
        row = row,
        system_prompt = prompt_comm_classifier,
        content_field = "description",
        api = "openai",
        model = "gpt-4o-mini"
    )
}

#' Dispatch special filter functions based on journal configuration
#' @param data Data frame of articles
#' @return Filtered data frame
dispatch_special_filter <- function(data) {
    FUN <- unique(data$filter_function)
    if (FUN == "") return(data)
    else {
        filter_fun <- match.fun(FUN)
        return(filter_fun(data))
    }
}

#' Filter Science magazine articles (require abstracts of sufficient length)
#' @param data Data frame of articles
#' @return Data frame with filter column updated
add_science_filter <- function(data) {
    flag <- as.numeric(is.na(data$abstract))
    flag <- ifelse(flag == 0, as.numeric(nchar(data$abstract) < 200), flag)
    data$filter <- flag * FILTER_SCIENCE
    return(data)
}

#' Filter Nature articles (only include research articles)
#' @param data Data frame of articles
#' @return Data frame with filter column updated
add_nature_filter <- function(data) {
    data$filter <- as.numeric(!grepl("/s", data$url)) * FILTER_NATURE
    return(data)
}

#' Standard filter for editorials, frontmatter, etc.
#' @param data Data frame of articles
#' @return Data frame with filter column updated
add_standard_filter <- function(data) {
    str <- data$title
    flag <- rep(FILTER_INCLUDE, length(str))
    flag <- ifelse(is.na(str), FILTER_EDITORIAL, flag)
    flag <- ifelse(str == "Editorial Board", FILTER_EDITORIAL, flag)
    flag <- ifelse(str == "Issue Information", FILTER_EDITORIAL, flag)
    flag <- ifelse(str == "Forthcoming Papers", FILTER_EDITORIAL, flag)
    flag <- ifelse(
        grepl("ERRATUM|ERRATA|Frontmatter|Front matter|Backmatter|Back matter|Reviewer Acknowledgment|Corrigendum",
              str, ignore.case = TRUE),
        FILTER_EDITORIAL, flag
    )
    data$filter <- flag
    return(data)
}
