# Main 
########

retrieve_crossref_issn_data <- function(issn_list, start_date, end_date, verbose=FALSE){

    K <- length(issn_list)
    out <- list()

    for(i in 1:K){
        issn <- issn_list[i]
        if(verbose) cat(issn, "\n")
        j <- 0
        tmp <- list()
        for(type in c("created", "published")){
            j <- j + 1
            tmp[[j]] <- call_crossref_api(
                id=issn, 
                type="issn", 
                start=start_date, 
                end=end_date, 
                date_type=type)
        }
        tmp <- rbind(
                get_crossref_articles(tmp[[1]]), 
                get_crossref_articles(tmp[[2]])
            )
        if(!is.null(tmp)) tmp$issn <- issn
        out[[i]] <- tmp[!duplicated(tmp$url),]
        }

    if(is.null(out)) return(NULL)

    out <- do.call(rbind, out)
    return(out)
    }


# Filter 
#########

add_multidisciplinary_filter <- function(row){
    row_nam <- names(row)
        cat(row[row_nam=="url"], "\n")
    row[row_nam=="filter"] <- as.integer(row[row_nam=="filter"])
    if(row[row_nam=="filter"]!=0) return(row[row_nam=="filter"])
    else{
        res <- call_openai_api(
            system_prompt=prompt_socsci_classifier, 
            user_prompt=paste(
                "Journal Name:", row[row_nam=="journal_full"], "\n",
                "Title:", row[row_nam=="title"], "\n",
                row[row_nam=="abstract"]
            ),
            model="gpt-4o-mini")
        if( get_openai_finish_reason(res)!="stop" ) return(-1)
        if( tolower(get_openai_response(res))=="no" ) return(2)
        return(0)
    }
}

add_preprint_filter <- function(row){
    row_nam <- names(row)
        cat(row[row_nam=="url"], "\n")
    row[row_nam=="filter"] <- as.integer(row[row_nam=="filter"])
    if(row[row_nam=="filter"]!=0) return(row[row_nam=="filter"])
    else{
        res <- call_openai_api(
            system_prompt=prompt_comm_classifier, 
            user_prompt=paste(
                "Title:", row[row_nam=="title"], "\n",
                row[row_nam=="description"]
            ),
            model="gpt-4o-mini")
        if( get_openai_finish_reason(res)!="stop" ) {cat(get_openai_finish_reason(res)); return(-1)}
        if( tolower(get_openai_response(res))=="no" ) return(2)
        return(0)
    }
}

dispatch_special_filter <- function(data){
    FUN <- unique(data$filter_function)
    if(FUN=="") return(data)
    else {
        filter_fun <- match.fun(FUN)
        return(filter_fun(data))
    }
    }

add_science_filter <- function(data){
    flag <- as.numeric(is.na(data$abstract)) 
    flag <- ifelse(flag==0, as.numeric(nchar(data$abstract)<200), flag) 
    data$filter <- flag*3
    return(data)
    }

add_nature_filter <- function(data){
    data$filter <- as.numeric(!grepl("/s", data$url))*4
    return(data)
    }

add_standard_filter <- function(data){
    str <- data$title
    flag <- rep(0, length(str))
    flag <- ifelse(is.na(str),1, flag) # ToCs have no title 
    flag <- ifelse(str=="Editorial Board",1, flag)
    flag <- ifelse(str=="Issue Information",1, flag)
    flag <- ifelse(str=="Forthcoming Papers",1, flag)
    flag <- ifelse(grepl("ERRATUM|ERRATA|Frontmatter|Front matter|Backmatter|Back matter|Reviewer Acknowledgment|Corrigendum", str, ignore.case = TRUE),1, flag)
    data$filter <- flag
    return(data)
    }


# Helpers 
##########

perform_with_retries <- function(req, max_attempts = 3, base_delay = 2) {
  for (attempt in seq_len(max_attempts)) {
    resp <- tryCatch(req_perform(req), error = function(e) e)
    # If not error and not HTTP 429, return the response
    if (!inherits(resp, "error") && (is.null(resp_status(resp)) || resp_status(resp) != 429)) {
      return(resp)
    }
    # Exponential backoff
    if (attempt < max_attempts) Sys.sleep(base_delay ^ attempt)
  }
  return(NULL) # return NULL if all attempts fail
}

render_json <- function(df,date){

    df <- split(df, df$journal_full)
    to_json <- list()
    for(i in 1:length(df)){
        articles <- df[[i]]
        journal_full <- unique(articles$journal_full)
        journal_short <- unique(articles$journal_short)
        articles <- articles[c("title", "authors", "abstract", "url", "doi", "filter")]
        articles_hidden <- subset(articles, !(filter==0 | filter==-1) )
        articles_hidden <- sort_by(articles_hidden, articles_hidden$filter) 
        articles <- subset(articles, (filter==0 | filter==-1) )
        to_json[[i]] <- list(
            "journal_full"=journal_full, 
            "journal_short"=journal_short,
            "articles"=articles, 
            "articles_hidden"=articles_hidden)
    }
    to_json <- list("update"=date, "content"=to_json)
    json <- toJSON(to_json, pretty=TRUE, auto_unbox=TRUE) 
    return(json)
    }

render_json_pre <- function(df,date){
    to_json <- list()
    articles <- df
    journal_full <- unique(articles$journal_full)
    journal_short <- unique(articles$journal_short)
    articles <- articles[c("title", "authors", "abstract", "url", "doi", "filter")]
    articles_hidden <- subset(articles, !(filter==0 | filter==-1) )
    articles_hidden <- sort_by(articles_hidden, articles_hidden$filter) 
    articles <- subset(articles, (filter==0 | filter==-1) )
    to_json <- list(
        "journal_full"=journal_full, 
        "journal_short"=journal_short,
        "articles"=articles, 
        "articles_hidden"=articles_hidden)
    to_json <- list("update"=date, "content"=to_json)
    json <- toJSON(to_json, pretty=TRUE, auto_unbox=TRUE) 
    return(json)
    }

extract_doi_id <- function(url){
    return(gsub("http(|s)://dx.doi.org/", "", url))
    }

strip_html <- function(str) {
   if(is.null(str)) return(NA)
   else {
    str <- gsub("<.*?>", " ", str)
    str <- gsub("\\s+", " ", str)
    str <- trimws(str)
    return(str)
   }
}

strip_whitespace <- function(str) {
   if(is.null(str)) return(NA)
   else {
    str <- gsub("\\s+", " ", str)
    return(trimws(str))
   }
}

file_empty <- function(file){
    length(readLines(file))==0
    }

read.csv2_check <- function(file, ...){
    if(!file_empty(file)){ 
        return(read.csv2(file, ...))
    } else { 
        return(NULL)
    }
}

# Crossref 
call_crossref_api <- function(id,type="issn",start,end,date_type="created", rows=1000){
    if( sum(type %in% c("issn", "doi"))!=1 ) stop("type must be either 'issn' or 'doi'")
    if( sum(date_type %in% c("created", "published"))!=1 ) stop("date_type must be either 'created' or 'published'")
    if(type=="issn"){
        endpoint <- paste0("https://api.crossref.org/journals/", id, "/works")
    }
    if(type=="doi"){
        endpoint <- paste0("https://api.crossref.org/prefixes/", id, "/works")
    }
    if(date_type=="created") {
        filter <- paste0("from-created-date:", start, ",until-created-date:", end)
    }
    if(date_type=="published") {
        filter <- paste0("from-pub-date:", start, ",until-pub-date:", end)
    }
    param = list(
        "filter"=filter, 
        "select"="title,author,abstract,URL,created", 
        "mailto"=crossref_email, 
        rows=rows)
    res = GET(endpoint,query=param)
    return(content(res))
    }

get_crossref_articles <- function(items){
    ll <- lapply(items$message$items, get_crossref_article_info)
    ll <- do.call(rbind, lapply(ll, function(x) as.data.frame(t(x))))
    return(ll)
}

get_crossref_article_info <- function(item){

    return((c(
        title=get_crossref_title(item),
        authors=get_crossref_authors(item),
        created=get_crossref_date(item, "created"),
        abstract=get_crossref_abstract(item), 
        url = get_crossref_url(item)
    )))
}

get_crossref_abstract <- function(item){
    if(is.null(item$abstract)) return(NA)
    else return(item$abstract)
}

get_crossref_authors <- function(item){
    if(is.null(item$author)) return(NA)
    else return(paste(lapply(item$author, get_crossref_author), collapse=", "))
}

get_crossref_author <- function(item){
    paste(item$given, item$family)
}

get_crossref_date <- function(item, name){
    if(is.null(item[[name]])) return(NA)
    else paste(unlist(item[[name]][["date-parts"]]), collapse="-")
}

get_crossref_title <- function(item){
    if(is.null(item$title)) return(NA)
    else unlist(item$title)
}

get_crossref_journal <- function(item){
    if(is.null(item$`container-title`)) return(NA)
    else unlist(item$`container-title`)
}

get_crossref_url <- function(item){
    if(is.null(item$URL)) return(NA)
    else unlist(item$URL)
}

get_crossref_api_limits <- function(response){
    out <- headers(response)
    limit <- out$`x-ratelimit-limit`
    interval <- out$`x-ratelimit-interval`
    return(c("limit"=limit, "interval"=interval))
    }



# Open AI 
call_openai_api <- function(system_prompt, user_prompt, model){
    endpoint <- "https://api.openai.com/v1/chat/completions"
    body <- list(
        model = model,
        messages = list(
            list(role="system", content=system_prompt),
            list(role="user", content=user_prompt)
            )
        )
    body <- toJSON(body, auto_unbox=TRUE)
    res <- httr::POST(endpoint, 
        body=body, 
        encode='raw', 
        httr::content_type_json(), 
        httr::add_headers(Authorization = paste("Bearer", openai_apikey, sep = " ")))
    
    return(content(res))
    }

get_openai_response <- function(response){
    return(response$choices[[1]]$message$content)
}

get_openai_finish_reason <- function(response){
    return(response$choices[[1]]$finish_reason)
}

get_openai_usage <- function(response){
    return(unlist(response$usage$total_tokens))
}

### Preprints

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

    # Use retry logic for main API request
    if (!is.null(next_page_url)) {
      response <- perform_with_retries(request(next_page_url))
    } else {
      response <- perform_with_retries(request(base_url) %>% req_url_query(!!!query))
    }
    # If the response is NULL (all attempts failed), skip task
    if (is.null(response)) {
      warning("OSF API unavailable or rate limited; skipping preprint retrieval.")
      return(all_preprints)
    }
    
    data <- resp_body_json(response)
    
    preprints <- map_df(data$data, function(item) {
      # Get contributors
      contributors_url <- item$relationships$contributors$links$related$href
      contributors_response <- safely(function(req) perform_with_retries(req))(request(contributors_url))
      
      authors <- if (!is.null(contributors_response$result)) {
        contributors_data <- resp_body_json(contributors_response$result)
        author_names <- map_chr(contributors_data$data, function(contributor) {
          # Safely extract user attributes
          user_data <- contributor$embeds$users$data$attributes
          
          # Check if user_data exists and is a list/environment
          if (!is.null(user_data) && is.list(user_data)) {
            # Extract names, replacing NULL with empty string for safe pasting
            given <- ifelse(is.null(user_data$given_name), "", user_data$given_name)
            middle <- ifelse(is.null(user_data$middle_names), "", user_data$middle_names)
            family <- ifelse(is.null(user_data$family_name), "", user_data$family_name)
            
            # Construct full name
            full_name <- paste(given, middle, family) %>%
              trimws() %>%
              gsub("\\s+", " ", .)
              
            # Return name if not empty, otherwise NA
            if (full_name != "") {
              return(full_name)
            } else {
              return(NA_character_) # Return NA if name components were missing/empty
            }
          } else {
            # Return NA if user_data structure is missing
            return(NA_character_)
          }
        })
        paste(author_names, collapse = "; ")
      } else {
        "No authors listed"
      }
      
      # Extract DOI
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
  
  # Trim to max_results if exceeded
  if (nrow(all_preprints) > max_results) {
    all_preprints <- all_preprints[1:max_results, ]
  }
  
  return(all_preprints)
}
