servers <- c("PsyArxiv", "SocArxiv", "MetaArxiv", "MediArxiv")

library(httr)
library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)

source("fun.R")
source("credentials.R")
source("./parameters/prompts.R")

# Function to call Google Gemini API
call_gemini_api <- function(system_prompt, user_text, model = "gemini-1.5-flash-latest") { # Using standard identifier, adjust if needed
    # Attempt to retrieve API key: Env Var GEMINI_APIKEY -> Env Var GEMINI_API_KEY -> R Var GEMINI_API_KEY
    api_key <- Sys.getenv("GEMINI_APIKEY")
    
    if (api_key == "") {
      api_key <- Sys.getenv("GEMINI_API_KEY") # Check alternative env var name
    }
      
    if (api_key == "") {
      # If both env vars are empty, check for R variable (for local testing)
      if (exists("gemini_apikey") && !is.null(gemini_apikey) && gemini_apikey != "") {
        api_key <- gemini_apikey
        warning("Using GEMINI_API_KEY from R environment (credentials.R).") # Add warning for local use
      } else {
        # If none are found, stop with a more informative message
        stop("Gemini API Key not found. Checked Sys.getenv('GEMINI_APIKEY'), Sys.getenv('GEMINI_API_KEY'), and R variable 'GEMINI_API_KEY'. Ensure the secret/variable is correctly set.")
      }
    }
    
    api_url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model, ":generateContent?key=", api_key)
    
    # Construct the request body according to Gemini API format
    request_body <- list(
        contents = list(
            list(parts = list(list(text = system_prompt))),
            list(parts = list(list(text = user_text)))
        ),
        # Optional: Add generationConfig if needed (e.g., temperature, max output tokens)
        generationConfig = list(
            temperature = 0.2, # Adjust as needed for classification
            maxOutputTokens = 10 # Expecting short response ("Yes"/"No")
        )
    )
    
    response <- httr::POST(
        url = api_url,
        body = jsonlite::toJSON(request_body, auto_unbox = TRUE),
        httr::add_headers("Content-Type" = "application/json"),
        httr::timeout(60) # Increased timeout for API call
    )
    
    # Basic error handling
    if (httr::status_code(response) != 200) {
        error_content <- httr::content(response, "text", encoding = "UTF-8")
        stop(paste("Gemini API request failed with status:", httr::status_code(response), "\nResponse:", error_content))
    }
    
    # Parse the response
    parsed_response <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
    
    # Check for safety ratings or blocks if necessary (can add later if needed)
    # if (!is.null(parsed_response$promptFeedback$blockReason)) { ... }
    
    # Extract the text content
    # Handle potential variations in response structure (e.g., if no candidate is returned)
    if (length(parsed_response$candidates) > 0 && 
        length(parsed_response$candidates[[1]]$content$parts) > 0 &&
        !is.null(parsed_response$candidates[[1]]$content$parts[[1]]$text)) {
        
        result_text <- parsed_response$candidates[[1]]$content$parts[[1]]$text
        return(trimws(result_text)) # Return the trimmed text
    } else {
        warning("Could not extract text from Gemini response. Full response:")
        print(parsed_response)
        return(NULL) # Return NULL if text extraction fails
    }
}

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
                                            model = "gemini-2.0-flash") # Use requested model if different identifier needed
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
