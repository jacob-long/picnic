servers <- c("PsyArxiv", "SocArxiv", "MetaArxiv", "MediArxiv")

library(httr)
library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)

source("fun.R")
source("credentials.R")
source("./parameters/prompts.R")
source("R/api_gemini.R")

all_preprints <- data.frame(
    "id" = NA, "title" = NA, "abstract" = NA, "date_created" = NA, 
    "authors" = NA, "doi" = NA, "journal_full" = NA, "journal_short" = NA
)

for (server in servers) {
    preprints <- get_all_osf_preprints(tolower(server))
    if (nrow(preprints) > 0) {
        preprints$journal_full <- server
        preprints$journal_short <- server 
        all_preprints <- rbind(all_preprints, preprints)
    }
}

preprints <- all_preprints[-1,]
preprints$url <- paste0("https://osf.io/", preprints$id)
preprints$filter <- 0

for (i in seq_len(nrow(preprints))) {
    # Call the Gemini API
    # Note: Ensure prompt_comm_classifier is suitable for Gemini and expects "Yes"/"No"
    gemini_response_text <- call_gemini_api(prompt_comm_classifier,
                                            paste("Title:", preprints$title[i], "\n",
                                                  "Abstract:", preprints$abstract[i]),
                                            model = "gemini-3-flash-preview")
    # Parse the Gemini response text (assuming it returns "Yes" or "No")
    # Handle potential NULL response if API call failed or text extraction didn't work
    if (is.null(gemini_response_text)) {
        warning(paste("Skipping filter update for preprint index", i, "due to NULL API response."))
        new_filter <- preprints$filter[i] # Keep existing filter or assign a default error code like -1
    } else {
        # Simple case-insensitive check for "No"
        new_filter <- ifelse(tolower(gemini_response_text) == "no", 2, 0)
    }
    preprints$filter[i] <- new_filter
}

# Output JSON
out_json <- render_json(preprints, date=as.Date(Sys.time())) 
write(out_json, paste0("./output/preprints.json"))
