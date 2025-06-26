# Geocode IP addresses
# Takes about 37.841 seconds to run

source(file.path(rprojroot::find_root(rprojroot::has_file("config.R")),"config.R"),echo=FALSE)
# Need remotes and rgeolocate as well as the Maxmind GeoLite2 database
# A free account is required to download the database, see https://dev.maxmind.com/geoip/geolite2-free-geolocation-data/
library(dplyr)
library(skimr)
library(stringr)
library(rgeolocate)
library(tidylog)

# by default, do not do the reverse IP lookup

lookup <- FALSE

# If file is not present, do not process.

if (file.exists(file.path(interwrk,"ssclogs.rds"))) {
  parsed_data.df <- readRDS(file.path(interwrk,"ssclogs.rds"))
} else {
  stop("ssclogs.rds not found in interwrk directory.  Please run 10_geocode_stata_step1.R first.")
}

# For those records that have already been mapped to a domain name, find the IP address
# This can take a VERY long time

if ( lookup ) {
# Check if the OS is Linux
if (Sys.info()["sysname"] == "Linux") {
  # Will only work on Linux
  if (nzchar(Sys.which("host"))) {
                                
  parsed_data.df <- parsed_data.df %>%
    mutate(ipaddress = ifelse(is.na(ipaddress), 
                                  sapply(clientname, function(name) {
                                    result <- system(paste0("host ", name), intern = TRUE)
                                    ipv4 <- str_extract(result, "\\d+\\.\\d+\\.\\d+\\.\\d+")
                                    ipv4[!is.na(ipv4)][1] # Take the first non-NA IPv4 address
                                  }),
                              ipaddress))
  } else {
          warning("The 'host' command is not available. Please install it.")
        }
} else {
  stop("This script is designed to run on Linux. Please run it on a Linux system.")
}
}
# Geolocate the data
# Approximate time:  1m14.532s

geolocations <- maxmind(parsed_data.df$ipaddress, 
                    file.path(rawdata,"GeoLite2-Country.mmdb"),
                    fields=c("continent_name","country_code", "country_name"))

skim(geolocations)

# Save the geolocated data as Rds file

saveRDS(geolocations, file.path(interwrk,"geolocations.rds"))

