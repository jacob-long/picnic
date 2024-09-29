servers <- c("PsyArxiv", "SocArxiv", "MetaArxiv", "MediArxiv")

library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)

source("fun.R")
source("credentials.R")
source("./parameters/prompts.R")

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
    the_response <- call_openai_api(prompt_comm_classifier,
                                    paste("Title:", preprints$title[i], "\n",
                                          "Abstract:", preprints$abstract[i]),
                                    model = "gpt-4o-mini")  
    new_filter <- ifelse(the_response$choices[[1]]$message$content == "No", 2, 0)
    preprints$filter[i] <- new_filter
}

# Output JSON
out_json <- render_json(preprints, date=as.Date(now)) 
write(out_json, paste0("./output/preprints.json"))
