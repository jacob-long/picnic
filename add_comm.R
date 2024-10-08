### This is used by Jacob to produce the list of journals and would require
### some tweaking to be used by others. Especially the exploratory analysis
### at the beginning in which I explore my Zotero  library to look for ideas
### for journals to include.

# Load required libraries
library(tidyverse)

# Read the CSV file
zotero_data <- read_csv("~/Downloads/My Library.csv")

# Count the frequency of each publication
publication_counts <- zotero_data %>%
  mutate(journal_title = tolower(`Publication Title`)) %>%
  group_by(journal_title) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# View the top publications
print(publication_counts, n = 100)  # Adjust the number as needed

# Optionally, save the results to a CSV file
write_csv(filter(publication_counts, count > 5 & count < 1000), "~/downloads/publication_frequency.csv")

communication_journals <- c(
  "Journal of Communication",
  "Communication Research",
  "Political Communication",
  "Human Communication Research",
  "Communication Methods and Measures",
  "Journalism & Mass Communication Quarterly",
  "Journal of Broadcasting & Electronic Media",
  "Mass Communication and Society",
  "International Journal of Communication",
  "Journal of Computer-Mediated Communication",
  "New Media & Society",
  "Annals of the International Communication Association",
  "Communication Theory",
  "Communication Monographs",
  "Media Psychology",
  "Social Media + Society",
  "Communication Research Reports",
  "The International Journal of Press/Politics",
  "Digital Journalism",
  "Information, Communication & Society",
  "Communication Studies",
  "Journalism",
  "Journalism Studies",
  "Communication Quarterly",
  "Cyberpsychology, Behavior, and Social Networking",
  "European Journal of Communication",
  "Journal of Advertising",
  "Review of Communication Research",
  "Science Communication",
  "Journal of Health Communication",
  "Public Relations Review",
  "International Journal of Advertising",
  "Journal of Public Relations Research",
  "Public Understanding of Science",
  "Journal of Language and Social Psychology",
  "Journal of Media Psychology",
  "Health Communication",
  "Visual Communication",
  "Mobile Media & Communication",
  "Journal of Applied Communication Research",
  "Convergence: The International Journal of Research into New Media Technologies",
  "Games and Culture",
  "Environmental Communication",
  "Southern Communication Journal",
  "Howard Journal of Communications",
  "Personal Relationships",
  "Nordicom Review",
  "Journal of Science Communication",
  "Journal of Social and Personal Relationships",
  "Cyberpsychology: Journal of Psychosocial Research on Cyberspace",
  "Journal of Information Technology & Politics",
  "Media and Communication",
  "Telematics and Informatics",
  "Internet Research",
  "Information Technology & People"
)

library(httr)
library(jsonlite)

get_issn <- function(journal_name) {
  url <- paste0("https://api.crossref.org/journals?query=", URLencode(journal_name))
  
  tryCatch({
    response <- GET(url)
    if (status_code(response) == 200) {
      content <- fromJSON(content(response, "text"), flatten = TRUE)
      if (!is.null(content$message$items) && length(content$message$items) > 0) {
        issn <- content$message$items$ISSN[[1]]
        return(paste(issn, collapse = ", "))
      }
    }
    return(NA)
  }, error = function(e) {
    message("Error processing: ", journal_name)
    return(NA)
  })
}

issns <- sapply(communication_journals, get_issn)
result <- data.frame(Journal = communication_journals, ISSN = issns)

# Print results
print(result)

# Count how many ISSNs were found
num_found <- sum(!is.na(result$ISSN))
cat("\nISSNs found for", num_found, "out of", nrow(result), "journals.\n")

result %>%
  mutate(
    ISSN = case_when(
      Journal == "Journal of Communication" ~ "0021-9916",
      Journal == "Journalism & Mass Communication Quarterly" ~ "1077-6990",
      Journal == "Journal of Broadcasting & Electronic Media" ~ "0883-8151, 1550-6878",
      Journal == "Mass Communication and Society" ~ "1520-5436, 1532-7825",
      Journal == "New Media & Society" ~ "1461-4448",
      Journal == "Information, Communication & Society" ~ "1369-118X, 1468-4462",
      Journal == "Mobile Media & Communication" ~ "2050-1579",
      Journal == "Journal of Language and Social Psychology" ~ "0261-927X",
      Journal == "Journal of Information Technology & Politics" ~ "1933-1681, 1933-169X",
      Journal == "Information Technology & People" ~ "0959-3845",
      TRUE ~ ISSN
    )
  ) -> result

journal_abbrev <- c(
  "Journal of Communication" = "JOC",
  "Communication Research" = "CR",
  "Political Communication" = "PolComm",
  "Human Communication Research" = "HCR",
  "Communication Methods and Measures" = "CMM",
  "Journalism & Mass Communication Quarterly" = "JMCQ",
  "Journal of Broadcasting & Electronic Media" = "JOBEM",
  "Mass Communication and Society" = "MCS",
  "International Journal of Communication" = "IJOC",
  "Journal of Computer-Mediated Communication" = "JCMC",
  "New Media & Society" = "NMS",
  "Annals of the International Communication Association" = "Annals",
  "Communication Theory" = "CT",
  "Communication Monographs" = "CM",
  "Media Psychology" = "MediaPsych",
  "Social Media + Society" = "SM+S",
  "Communication Research Reports" = "CRR",
  "The International Journal of Press/Politics" = "IJPP",
  "Digital Journalism" = "DigJ",
  "Information, Communication & Society" = "ICS",
  "Communication Studies" = "CS",
  "Journalism" = "Journ",
  "Journalism Studies" = "JournStud",
  "Communication Quarterly" = "CQ",
  "Cyberpsychology, Behavior, and Social Networking" = "CBSN",
  "European Journal of Communication" = "EJC",
  "Journal of Advertising" = "JAdvert",
  "Review of Communication Research" = "RCR",
  "Science Communication" = "SciComm",
  "Journal of Health Communication" = "JHC",
  "Public Relations Review" = "PRR",
  "International Journal of Advertising" = "IJA",
  "Journal of Public Relations Research" = "JPRR",
  "Public Understanding of Science" = "PUOS",
  "Journal of Language and Social Psychology" = "JLSP",
  "Journal of Media Psychology" = "JMP",
  "Health Communication" = "HealthComm",
  "Visual Communication" = "VisComm",
  "Mobile Media & Communication" = "MMC",
  "Journal of Applied Communication Research" = "JACR",
  "Convergence: The International Journal of Research into New Media Technologies" = "Conv",
  "Games and Culture" = "G&C",
  "Environmental Communication" = "EnvComm",
  "Southern Communication Journal" = "SoComm",
  "Howard Journal of Communications" = "HoJo",
  "Personal Relationships" = "PersRel",
  "Nordicom Review" = "Nord",
  "Journal of Science Communication" = "JCOM",
  "Journal of Social and Personal Relationships" = "JSPR",
  "Cyberpsychology: Journal of Psychosocial Research on Cyberspace" = "CyberPsych",
  "Journal of Information Technology & Politics" = "JITP",
  "Media and Communication" = "MAC",
  "Telematics and Informatics" = "T&I",
  "Internet Research" = "IR",
  "Information Technology & People" = "ITP"
)

result$journal_short <- journal_abbrev
result <- select(result, journal_full = Journal, issn = ISSN, journal_short)

library(tidyr)
library(dplyr)

# Assuming you have your original data in a data frame called 'journals'
# If not, create it first from your vector of journal names and ISSNs

# Split the ISSNs and create separate rows
journals_split <- result %>%
  separate_rows(issn, sep = ", ") %>%
  filter(issn != "0000-0000")

journals_split <- journals_split[!duplicated(journals_split), ]

# View the result
print(journals_split)

# Optionally, write to a CSV file
write.csv(journals_split, "communication_journals.csv", row.names = FALSE)

po_journals <- c(
  "Public Opinion Quarterly",
  "International Journal of Public Opinion Research",
  "Journal of Elections, Public Opinion and Parties",
  "Journal of Survey Statistics and Methodology",
  "Journal of Official Statistics",
  "Politics, Groups, and Identities",
  "Social Science Computer Review"
)

issns <- sapply(po_journals, get_issn)
result <- data.frame(Journal = po_journals, ISSN = issns)

result %>%
  mutate(
    ISSN = case_when(
      Journal == "Survey Research Methods" ~ "1864-3361",
      TRUE ~ ISSN
    )
  ) -> result

journal_abbrev <- c(
  "Public Opinion Quarterly" = "POQ",
  "International Journal of Public Opinion Research" = "IJPOR",
  "Journal of Elections, Public Opinion and Parties" = "JEPOP",
  "Journal of Survey Statistics and Methodology" = "JSSAM",
  "Journal of Official Statistics" = "JOS",
  "Politics, Groups, and Identities" = "PGI",
  "Social Science Computer Review" = "SSCR"
)

result$journal_short <- journal_abbrev
result <- select(result, journal_full = Journal, issn = ISSN, journal_short)

# Split the ISSNs and create separate rows
journals_split <- result %>%
  separate_rows(issn, sep = ", ") %>%
  filter(issn != "0000-0000")

journals_split <- journals_split[!duplicated(journals_split), ]

# Optionally, write to a CSV file
write.csv(journals_split, "po_journals.csv", row.names = FALSE)


psych_journals <- c(
  "Psychological Methods",
  "Journal of Experimental Social Psychology",
  "Personality and Social Psychology Bulletin",
  "Psychological Science",
  "Multivariate Behavioral Research",
  "Journal of Personality and Social Psychology",
  "Advances in Methods and Practices in Psychological Science",
  "Psychological Bulletin",
  "Behavior Research Methods",
  "Organizational Research Methods",
  "Group Processes & Intergroup Relations",
  "Psychology of Music",
  "Psychology of Popular Media",
  "Computers in Human Behavior",
  "Technology, Mind, and Behavior"
)

issns <- sapply(psych_journals, get_issn)
result <- data.frame(Journal = psych_journals, ISSN = issns)

result %>%
  mutate(
    ISSN = case_when(
      Journal == "Group Processes & Intergroup Relations" ~ "1368-4302",
      TRUE ~ ISSN
    )
  ) -> result

journal_abbrev <- c(
  "Psychological Methods" = "PM",
  "Journal of Experimental Social Psychology" = "JESP",
  "Personality and Social Psychology Bulletin" = "PSPB",
  "Psychological Science" = "PsychSci",
  "Multivariate Behavioral Research" = "MBR",
  "Journal of Personality and Social Psychology" = "JPSP",
  "Advances in Methods and Practices in Psychological Science" = "AMPPS",
  "Psychological Bulletin" = "PsychBull",
  "Behavior Research Methods" = "BRM",
  "Organizational Research Methods" = "ORM",
  "Group Processes & Intergroup Relations" = "GPIR",
  "Psychology of Music" = "PsychMusic",
  "Psychology of Popular Media" = "PPM",
  "Computers in Human Behavior" = "CHB",
  "Technology, Mind, and Behavior" = "TMB"
)

result$journal_short <- journal_abbrev
result <- select(result, journal_full = Journal, issn = ISSN, journal_short)

# Split the ISSNs and create separate rows
journals_split <- result %>%
  separate_rows(issn, sep = ", ") %>%
  filter(issn != "0000-0000")

journals_split <- journals_split[!duplicated(journals_split), ]

# Optionally, write to a CSV file
write.csv(journals_split, "psych_journals.csv", row.names = FALSE)
