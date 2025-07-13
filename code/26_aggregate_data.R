# R Download Statistics by Country for select
# This script downloads CRAN logs for R downloads in select,
# aggregates by country, creates maps, and computes regional statistics

source(file.path(rprojroot::find_root(rprojroot::has_file("config.R")),"config.R"),echo=FALSE)

# Load required libraries
library(tidyverse)
library(readr)
library(lubridate)
library(skimr)
library(tidylog)

# Load list of downloaded files from disk


# Save the list of downloaded files in interwrk as Rds
downloaded_files <- readRDS(file.path(interwrk, "cran_logs", "downloaded_files_select.rds"))


# Function to read and process a single log file
process_log_file <- function(file_path) {
  if (!file.exists(file_path)) {
    return(NULL)
  }
  
  # Read the compressed CSV file
    message("Reading file: |", file_path,"|")
    df <- readr::read_csv(file_path) |>
          # filter out src downloads
          filter(os != "src") |>
          # collapse by country
          group_by(country) |>
          summarise(downloads = n(), .groups = "drop") |>
          # add the year-month-day parsed from the filename
          mutate(
            date = as.Date(str_extract(file_path, "\\d{4}-\\d{2}-\\d{2}")),
            year = year(date),
            month = month(date),
            day = day(date))
    
    return(df)
  
}

# Process all downloaded files and combine
cat("Processing and combining log files...\n")
all_downloads <- purrr::map_dfr(downloaded_files, process_log_file)
skim(all_downloads)

cat("Total country-date cells in selected date range:", nrow(all_downloads), "\n")

# Save the combined dataset
write_csv(all_downloads, file.path(interwrk, "r_downloads_select_combined.csv"))
saveRDS(all_downloads, file.path(interwrk, "r_downloads_select_combined.rds"))


