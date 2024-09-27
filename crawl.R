field <- commandArgs(trailingOnly = TRUE)

library(httr)
library(jsonlite)

source("fun.R")
source("credentials.R")
source("./parameters/prompts.R")

now <- Sys.time()
crawl_start_date <- as.Date(now) - 7
crawl_end_date <- as.Date(now) - 1

journals <- read.csv(paste0("./parameters/", field, "_journals.csv"))

urls_file <- paste0("./memory/", field, "_urls.csv")
if (file.exists(urls_file) && file.info(urls_file)$size > 0) {
  past_urls <- read.csv(urls_file)
} else {
  past_urls <- data.frame()  # Initialize with an empty data frame
}

# Crawl Crossref API 
out <- retrieve_crossref_issn_data(
    issn_list=journals$issn, 
    start_date=crawl_start_date, 
    end_date=crawl_end_date
)

# Update past urls
write.table(out[,"url"], 
    file=paste0("./memory/", field, "_urls.csv"), 
    na="", 
    sep=";", 
    append=TRUE, 
    quote=FALSE, 
    col.names=FALSE,
    row.names=FALSE
)

# Write journal short list
journals_out <- unique(journals[,c("journal_full","journal_short")])
journals_out <- journals_out[order(journals_out$journal_full),]
journals_out <- toJSON(journals_out, pretty=TRUE, auto_unbox=TRUE)

# Debugging: Print the JSON output
cat("JSON output:\n", journals_out, "\n")

json_file <- paste0("./output/", field, "_journals.json")
cat("JSON file path: ", json_file, "\n")

if (file.exists(json_file) && file.info(json_file)$size > 0) {
  cat("File exists and is not empty. Writing JSON data...\n")
  writeLines(journals_out, json_file)
} else {
  cat("File does not exist or is empty. Creating file and writing JSON data...\n")
  file.create(json_file)
  writeLines(journals_out, json_file)
}

cat("JSON data written successfully.\n")

# Additional debugging: Check the contents of the written file
written_json <- readLines(json_file)
cat("Contents of the written JSON file:\n", paste(written_json, collapse="\n"), "\n")

# Additional debugging: Check if any JSON reading or processing is happening later
cat("Reading the JSON file to check for errors...\n")
tryCatch({
  read_json <- fromJSON(json_file)
  cat("JSON file read successfully.\n")
  cat("Structure of the JSON data:\n")
  print(str(read_json))
}, error = function(e) {
  cat("Error reading JSON file: ", e$message, "\n")
})