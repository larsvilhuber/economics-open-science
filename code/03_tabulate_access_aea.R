# Read restricted JIRA data from AEA, and save a releasable local file

source(here::here("config.R"))
library(readxl)
library(dplyr)
library("stringr")
library("tidyr")
library("purrr")
library(tidylog)

# requires that the data are there
if ( confdata.present ) {
  message("Reading in restrictd JIRA data")
  jira_access_raw <- read_excel(file.path(confdir,jira_access), 
                      sheet = "Your Jira Issues")
  
# now clean the data
  
  jira_access_raw %>%
    # Clean language
    mutate(outcome = case_when(
      Resolution == "Mostly replicated" ~ "Reproduced*",
      Resolution == "Not able to replicate" ~  "Not able to reproduce",
      Resolution == "Partially replicated" ~ "Reproduced*",
      Resolution == "Replicated" ~ "Reproduced",
      TRUE ~ "Unknown outcome"
    )) %>%
    # Remove the trailing ".R?" from the Manuscript number
    mutate(mc_number = gsub("\\.R\\d+$", "", `Manuscript Central identifier`)) %>%
    # construct the manuscript number
           mutate(first_chars=str_to_lower(substr(mc_number,1,1))) %>%
       filter(first_chars %in% c("a","j","p")) %>%
       # other cleaning
       filter(substr(mc_number,1,6)!="AEAREP") %>%
       filter(str_detect(mc_number,"AEJ Policy")==FALSE) %>%
       # now transform into DOI
       mutate(first_chars=str_to_lower(substr(mc_number,1,4)),
              first_chars5=str_to_lower(substr(mc_number,1,5)),
              doi_article_prefix=case_when(
                    first_chars == "aeja" ~ "app",
                    first_chars == "app" ~ "app",
                    first_chars == "app " ~ "app",
                    first_chars == "aejp" ~ "pol",
                    first_chars == "pol2"  ~ "pol",
                    first_chars5 == "aejma" ~ "mac",
                    first_chars5 == "aejmi" ~ "mic",
                    first_chars == "jel-" ~ "jel",
                    first_chars == "pand" ~ "pandp",
                    first_chars == "aeri" ~ "aeri",
                    first_chars == "aer-" ~ "aer",
                    TRUE                  ~ "")) %>%
       # now construct DOI
       # In general, mc_number = AEJAPP-2017-0453
       #             doi = prefix.20170453
       mutate( doi_suffix = str_extract_all(mc_number,"\\d") %>% 
                 map_chr(~ paste(.x, collapse = "")),
               doi = paste(doi_prefix,
                           paste(doi_article_prefix,doi_suffix,sep="."),
                           sep="/")) %>%
       # keep only certain variables
       select("Key","outcome","Software used",
              "DCAF_Access_Restrictions_V2",
              "RepositoryDOI",
              "RCT number" ,"doi")    -> jira_access_cleaned
    
# save the file
  saveRDS(jira_access_cleaned,file=file.path(datadir,"jira_access_cleaned.Rds"))
  # also save as CSV
  write.csv(jira_access_cleaned,file=file.path(datadir,"jira_access_cleaned.csv"))
  message(paste0("Files written to folder ",datadir))
} else {
  message("Confidential data directory is empty.  Please populate it with the confidential data.")
}