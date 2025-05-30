library(dplyr)
library(purrr)
library(jsonlite)
library(httr)

process_json <- function(file) {
    root <- fromJSON(file)
    content <- root[["content"]]
    journals <- content[["journal_full"]]
    if (length(journals) == 0) {return(NULL)}
    all_articles <- list()
    for (j in journals) {
        index <- which(content[["journal_full"]] == j)
        articles <- content[["articles"]][[index]]
        if (nrow(articles) == 0) {next}
        articles$journal_full <- j
        articles$filter <- as.numeric(articles$filter)
        if (!("abstract" %in% names(articles))) {
            articles$abstract <- NA
        }
        all_articles <- rbind(all_articles, articles)
    }
    return(all_articles)
}

file_paths <- paste0("output/", c("communication", "politics", "po", "psych", "sociology", "multidisciplinary"), ".json")
names(file_paths) <- c("communication", "politics", "public opinion", "psychology", "sociology", "multidisciplinary")

articles <- map_dfr(file_paths, process_json, .id = "discipline") %>%
    filter(filter == 0)

articles <- articles[!duplicated(articles),]

library(dplyr)
library(stringr)

result_string <- articles %>%
  mutate(row_string = str_c("------\n",
    "doi: ", doi, "\n", 
    "Journal: ", journal_full, "\n", 
    "Discipline: ", discipline, "\n",
    "Title: ", title, "\n",
    "Abstract: ", ifelse(is.na(abstract), "No abstract available.", abstract))) %>%
  pull(row_string) %>%
  str_c(collapse = "\n")

sys_prompt1 <- "You are assisting a communication scientist in selecting important articles to read from newly published works. Focus on identifying key articles based on three main criteria: (1) relevance to quantitative and social scientific studies of communication; (2) apparent quality of research; and (3) broad interest across the discipline, including methodological advancements with wide applicability. Use titles and abstracts as primary indicators, supplemented by journal reputation when necessary. Prioritize articles from communication and public opinion disciplines but consider relevant work from other disciplines too. Do not include papers from other disciplines unless they have a clear relationship to media/communication or they describe an important new methodology that can be used for communication research. Do not include articles that are primarily about politics or sociology, especially if they are not quantitative social science papers. In papers that report new findings (rather than methodological research and reviews), look for surveys, experiments, and quantitative content analyses. Avoid qualitative case studies and news-like descriptive studies. Never include more than 3 articles from the same, non-communication journal. Note that in communication, the most prestigious journals include 'Journal of Communication', 'Political Communication', 'Communication Research', 'Human Communication Research', 'Journal of Computer-Mediated Communication', 'Communication Methods and Measures', and 'New Media & Society'. 'Internet Research' is not prestigious. Methodological research focused on survey designs, statistical analysis of experimental and survey data, methods for automated content analysis, and the use of AI as a research tool are useful to include. Important substantive topics include political partisanship, news media, health communication, social identity. Look for articles in the broad areas of political communication, public opinion, media effects, and quantitative social science methodology. Never include book reviews. Key themes of interest include:

Political behavior, attitudes, and identity
Media consumption patterns and effects
Social identity theory applications in politics or communication
Advancements in research methods, particularly for longitudinal or time-series data
Computational approaches to social science questions
Selective exposure and information processing
The intersection of entertainment and politics
Interpersonal communication in political contexts
Developments in statistical analysis techniques
Measurement issues in communication and political science

Also consider articles that present novel theoretical frameworks, introduce innovative research designs, or apply established theories to new contexts. Look for work that bridges multiple disciplines or applies insights from one field to another. Importantly, flag articles that challenge existing paradigms or present counterintuitive findings, as these might spark new research ideas.\n"

sys_prompt2 <- "Now, I present new articles for you to choose from based on these instructions. Provide only 25 DOI numbers in response, formatted as follows: 'doi1,doi2,...,doi25'. Do not include anything else in your response besides the DOIs.\n"

source("credentials.R")

api_url <- "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-pro-preview-03-25:generateContent"
gemini_request <- function(prompt, max_retries = 3) {
  body <- list(
    contents = list(
      list(parts = list(list(text = prompt)))
    ),
    generationConfig = list(
      temperature = 1
    )
  )
  
  for (attempt in 1:max_retries) {
    response <- tryCatch({
      POST(
        url = paste0(api_url, "?key=", gemini_apikey),
        body = toJSON(body, auto_unbox = TRUE),
        add_headers("Content-Type" = "application/json")
      )
    }, error = function(e) e)
    
    if (!inherits(response, "error") && status_code(response) == 200) {
      content <- content(response, "parsed")
      return(content$candidates[[1]]$content$parts[[1]]$text)
    } else if (!inherits(response, "error") && status_code(response) == 503) {
      wait_time <- 2^attempt  # Exponential backoff: 2, 4, 8 seconds
      message(sprintf("Attempt %d failed with 503 error. Waiting %d seconds...", attempt, wait_time))
      Sys.sleep(wait_time)
      next
    } else {
      if (attempt == max_retries) {
        warning("Max retries reached. Last error: ", 
                if(inherits(response, "error")) response$message 
                else paste(status_code(response), "-", content(response, "text")))
        return(NULL)
      }
      message(sprintf("Attempt %d failed. Retrying...", attempt))
      Sys.sleep(1)
    }
  }
}

response <- gemini_request(paste(sys_prompt1, sys_prompt2, result_string))

cat("Gemini response:\\n", response)

dois <- trimws(unlist(strsplit(response, ","))) %>%
    gsub("doi:", "", ., fixed = T)

cat("\\ndois:\\n", dois)

# Convert DOI URLs/prefixes to just DOI
doi_clean <- function(x) gsub("(https?://doi.org/|doi:)", "", x)

articles$doic <- doi_clean(articles$doi)

articles %>%
  filter(doic %in% dois) -> selected_articles

# cat("\\nselected_articles:\\n")
print(selected_articles)

write(toJSON(select(selected_articles, -doic), pretty=TRUE, auto_unbox=TRUE), "output/must_read.json")
