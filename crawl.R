field <- commandArgs(trailingOnly = TRUE)

library(httr)
library(rvest)
library(jsonlite)

source("fun.R")
source("credentials.R")
source("./parameters/prompts.R")

now <- Sys.time()
crawl_start_date <- as.Date(now) - 7
crawl_end_date <- as.Date(now) - 1

journals <- read.csv(paste0("./parameters/", field, "_journals.csv"))
# past_urls <- read.csv(paste0("./memory/", field, "_urls.csv"))

# Crawl Crossref API 
out <- retrieve_crossref_issn_data(
    issn_list=journals$issn, 
    start_date=crawl_start_date, 
    end_date=crawl_end_date, 
    verbose=TRUE)

# Remove duplicates
out <- out[!duplicated(out$url),] 
out <- out[!duplicated(tolower(trimws(out$title))),]
out <- out[!is.na(out$authors),]
## I'm gonna let the old papers lie for now. I'm okay
## with them appearing twice, once upon advance online 
## access and again upon full publication.
# Remove past papers
# out <- out[!(out$url %in% past_urls$url), ]
if(is.null(out) || nrow(out) == 0) quit(save="no")

# Cleanup data
out$abstract <- strip_html(out$abstract)
out$abstract <- gsub("^(Abstract|ABSTRACT) ", "", out$abstract)
out$title <- strip_html(out$title)
out$doi <- extract_doi_id(out$url)

# Merge in journal information 
out <- merge(out, journals, by="issn")

# Apply standard filter flags 
out <- add_standard_filter(out) 

# Let's try to find the abstracts that are missing
if (nrow(subset(out, filter == 0 & is.na(abstract))) > 0) {
    # Get the missing abstracts 
    no_abs <- subset(out, filter == 0 & is.na(abstract))
    # Set the user agent for scraping
    user_agent <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.114 Safari/537.36"
    # Loop through them
    for (i in 1:nrow(no_abs)) {
        # Create holder object
        abstract <- NULL
        # Get the URL
        url <- no_abs$url[i]
        # Visit the URL
        response <- GET(url, 
                user_agent(user_agent))
        if (status_code(response) %in% c(403, 404)) {
            next
        }
        # Get the final URL after DOI redirect
        final_url <- response$url
        # Grab the page
        content <- read_html(response)

        # Different strategies depending on the publisher
        if (grepl("nature\\.com", response$url)) {
            abstract <- content |> html_element("#Abs1-content") |> html_text2()
            if (is.na(abstract)) {
                # Grabs teaser from Matters Arising
                abstract <- content |> html_element(".article__teaser") |> html_text2()
            }
        } 

        if (!is.null(abstract) && !is.na(abstract)) {
            out$abstract[out$url == url] <- abstract
        }
    }
}

# Filter flags: Multidisciplinary journals 
if(field=="multidisciplinary"){
    out_lst <- split(out, out$filter_function) 
    out_lst <- lapply(out_lst, dispatch_special_filter ) 
    out <- do.call(rbind, out_lst)
    out$filter <- apply(out, 1, function(x){
        tryCatch(
            {add_multidisciplinary_filter(x)
            }, error = function(msg){
                return(-1)
            })
        })
    rownames(out) <- NULL
    } 

# Output JSON
out_json <- render_json(out, date=as.Date(now)) 
write(out_json, paste0("./output/", field, ".json"))

# Update past urls
write.table(out[,"url"], 
    file=paste0("./memory/", field, "_urls.csv"), 
    na="", 
    sep=";", 
    append=TRUE, 
    quote=FALSE, 
    col.names=FALSE,
    row.names=FALSE)

# Write journal short list
journals_out <- unique(journals[,c("journal_full","journal_short")])
journals_out <- journals_out[order(journals_out$journal_full),]
journals_out <- toJSON(journals_out, pretty=TRUE, auto_unbox=TRUE) 
write(journals_out, paste0("./output/", field, "_journals.json"))
