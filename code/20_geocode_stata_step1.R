# Download log files from Stata archive
# Provided by Kit
# Takes about 24m15.649s to run

source(file.path(rprojroot::find_root(rprojroot::has_file("config.R")),"config.R"),echo=FALSE)
library(dplyr)
library(skimr)
library(stringr)
library(tidylog)
library(arrow)

# If file is not present, download it. Use the basename of baum.logs

if (!file.exists(file.path(rawdata,basename(baum.logs)))) {
  download.file(baum.logs,destfile = file.path(rawdata,basename(baum.logs)))
}

# 58.240.173.126 - - [23/Feb/2025:03:45:02 -0500] "GET /repec/bocode/f/fsort.ado HTTP/1.1" 200 1549 "-" "Stata/MP 18.0 (54.2.180120) on Macintosh 64-bit (Apple Silicon MacBookAir10,1 12.2.1)"
#223.88.237.195 - - [23/Feb/2025:03:45:02 -0500] "GET /repec/bocode/_/_eststo.hlp HTTP/1.0" 200 11 "-" "Stata/MP 15.1 (521.15.1.614) on Windows NT 10.0"
#repec.oru.se - - [23/Feb/2025:03:45:02 -0500] "HEAD /repec/bocode/x/xtrevu.ado HTTP/1.1" 200 - "-" "RePEc link checker (https://EconPapers.repec.org/check/)"

log_data <- readLines(file.path(rawdata, basename(baum.logs)))
parsed_data <- data.frame(
  first_field = str_extract(log_data, "^[^ ]+"),
  path = str_extract(log_data, "(?<=GET |HEAD )[^ ]+"),
  client = str_extract(log_data, "(?<=\\\" \")[^\"]+")
) %>%
 filter(grepl("Stata", client)) %>%
  select(-client)


# Determine if the first field is an IP address or a client name
parsed_data.df <- parsed_data %>%
  mutate(
    ipaddress = ifelse(grepl("^\\d+\\.\\d+\\.\\d+\\.\\d+$", first_field), first_field, NA),
    clientname = ifelse(!grepl("^\\d+\\.\\d+\\.\\d+\\.\\d+$", first_field), first_field, NA)
  ) %>%
  select(-first_field) %>%
  mutate(
    ipaddress = as.factor(ipaddress),
    clientname = as.factor(clientname)
  )

skim(parsed_data.df)

# Clean up, remove the two raw data objects

rm(log_data, parsed_data)

# save the Rds file with compression
saveRDS(parsed_data.df, file.path(interwrk, "ssclogs.rds"), compress = TRUE)

# save the data frame in parquet format
write_parquet(parsed_data.df, file.path(interwrk, "ssclogs.parquet"))
