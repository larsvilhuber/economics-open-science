# R Download Statistics by Country for 2024
# This script downloads CRAN logs for R downloads in 2024,
# aggregates by country, creates maps, and computes regional statistics

source(file.path(rprojroot::find_root(rprojroot::has_file("config.R")),"config.R"),echo=FALSE)

# Load required libraries
library(tidyverse)
library(countrycode)
library(maps)
library(ggplot2)
library(viridis)
library(scales)
library(readr)
library(lubridate)

# Create data directory if it doesn't exist
if (!dir.exists(file.path(interwrk,"cran_logs"))) {
  dir.create(file.path(interwrk,"cran_logs"), recursive = TRUE)
}

# Function to download a single day's R download log
download_daily_log <- function(date_str) {
  url <- paste0("http://cran-logs.rstudio.com/", year(as.Date(date_str)), "/", date_str, "-r.csv.gz")
  local_file <- file.path(interwrk,"cran_logs", paste0(date_str, "-r.csv.gz"))
  
  # Skip if file already exists
  if (file.exists(local_file)) {
    cat("File already exists:", local_file, "\n")
    return(local_file)
  }
  
  cat("Downloading:", url, "\n")
  tryCatch({
    download.file(url, local_file, mode = "wb", quiet = TRUE)
    return(local_file)
  }, error = function(e) {
    cat("Error downloading", url, ":", e$message, "\n")
    return(NULL)
  })
}

# Generate all dates for select
dates_select <- seq(as.Date(from_date), as.Date(until_date), by = "day")
date_strings <- format(dates_select, "%Y-%m-%d")

# Download all daily logs for select
cat("Downloading CRAN logs for select...\n")
downloaded_files <- purrr::map(date_strings, download_daily_log)
downloaded_files <- downloaded_files[!sapply(downloaded_files, is.null)]

cat("Downloaded", length(downloaded_files), "files\n")

# Save the list of downloaded files in interwrk as Rds
saveRDS(downloaded_files, file.path(interwrk, "cran_logs", "downloaded_files_select.rds"))


