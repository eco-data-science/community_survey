################################################################################
# 01_clean_2021_responses
################################################################################
# 
# Reads in the raw data from the 2021 responses to the
# community survey (https://docs.google.com/forms/d/e/1FAIpQLSfVlLQJCXLf0qUjTvCp9FSftYoILSfQ0FtaZKfQ5e0mk6udqg/viewform?usp=sf_link)
# and cleans it up for analysis
#
################################################################################

# SET UP #######################################################################

# Load packages
library(here)
library(magrittr)
library(tidyverse)

# Load data
raw_data <- read_csv(here("raw_data", "2021_EcoDataScience_community.csv"))

# Check the colnames
#colnames(raw_data)

# Create a vector of new column names (The comments to the side point at the questions)
new_colnames <- c(
  "timestamp",              # Timestamp
  "preferred_contact",      # "In the past, we've used slack and email to deliver information about upcoming sessions. Which one do you prefer?" 
  "where_from",             # "Where are you from? (not required, but we want to know!)"
  "where_at",               # "Where are you based?"
  "affiliation",            # "What is your affiliation? (For standardization purposes, what is the domain on your email? For example, if your email \"data_nerd@ucsb.edu\", then type \"ucsb\")"
  "type_of_organization",   # "How would you describe you organization?"     
  "people_invited",         # "How many people have you invited to join the EDS Slack or list server?"
  "eds_attended",           # "How many EDS sessions have you attended?"   
  "eds_instructed",         # "How many EDS sessions have you instructed?"   
  "want_to_teach"           # "Would you like to teach a session in Winter or Spring quarters? If so, what topic(s)?"
)

clean_data <- raw_data %>% 
  set_colnames(value = new_colnames) %>%                                        # Update colnames
  mutate(preferred_contact = str_extract(preferred_contact, "[:alpha:]+"),
         people_invited = str_extract(people_invited, "[:digit:]"),
         eds_instructed = str_extract(eds_instructed, "[:digit:]"),
         affiliation = str_to_upper(affiliation),
         affiliation = str_remove_all(affiliation, "[:punct:]"),
         affiliation = str_remove(affiliation, "OF|GMAIL OR|INC"),
         affiliation = str_squish(affiliation),
         affiliation = case_when(str_detect(affiliation, "BREN") ~ "UCSB",
                                 T ~ affiliation)) %>% 
  rowwise() %>% 
  mutate(acronym = paste(str_extract_all(affiliation, "\\b[A-Z]", simplify = T), collapse = "")) %>%
  replace_na(list(where_from = "Missing",
                  affiliation = "Missing",
                  where_at = "Missing"))

write_csv(x = clean_data, file = here("clean_data", "2021_community_survey_data.csv"))
