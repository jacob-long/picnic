# Simulate what Gemini returns
gemini_response <- "doi: https://doi.org/10.1177/13548565251403582,doi: https://doi.org/10.1093/hcr/hqaf029"

# Clean it like the FIXED script does
dois <- trimws(unlist(strsplit(gemini_response, ",")))
dois <- gsub("(https?://doi.org/|https?://dx.doi.org/|doi:)", "", dois, perl = TRUE)
dois <- trimws(dois)  # TRIM AGAIN AFTER GSUB!

cat("Cleaned DOIs from Gemini:\n")
print(dois)

# Simulate database DOI
db_doi <- "https://doi.org/10.1177/13548565251403582"

# Clean it like the script does
doi_clean <- function(x) gsub("(https?://doi.org/|https?://dx.doi.org/|doi:)", "", x, perl = TRUE)
db_doic <- doi_clean(db_doi)

cat("\nCleaned DOI from database:\n")
print(db_doic)

cat("\nMatch test:\n")
print(db_doic %in% dois)
