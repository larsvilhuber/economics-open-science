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


# Function to read and process a single log file
process_log_file <- function(file_path) {
  if (!file.exists(file_path)) {
    return(NULL)
  }
  
  tryCatch({
    # Read the compressed CSV file
    df <- readr::read_csv(file_path, show_col_types = FALSE)
    
    # Filter for R downloads and select relevant columns
    if ("package" %in% names(df)) {
      df <- df %>%
        dplyr::filter(package == "R") %>%
        dplyr::select(date, time, size, r_version, r_arch, r_os, country, ip_id)
    } else {
      # Some files might have different column names
      df <- df %>%
        dplyr::select(date, time, size, r_version, r_arch, r_os, country, ip_id)
    }
    
    return(df)
  }, error = function(e) {
    cat("Error reading", file_path, ":", e$message, "\n")
    return(NULL)
  })
}

# Process all downloaded files and combine
cat("Processing and combining log files...\n")
all_downloads <- purrr::map_dfr(downloaded_files, process_log_file)

cat("Total R downloads in 2024:", nrow(all_downloads), "\n")

# Save the combined dataset
write_csv(all_downloads, "data/r_downloads_2024_combined.csv")

# Aggregate by country
country_stats <- all_downloads %>%
  group_by(country) %>%
  summarise(
    downloads = n(),
    unique_ips = n_distinct(ip_id),
    .groups = "drop"
  ) %>%
  arrange(desc(downloads))

# Add country names and region information
country_stats <- country_stats %>%
  mutate(
    country_name = countrycode(country, "iso2c", "country.name"),
    continent = countrycode(country, "iso2c", "continent"),
    region = countrycode(country, "iso2c", "region")
  ) %>%
  filter(!is.na(country_name))  # Remove invalid country codes

# Define regional groupings as requested
country_stats <- country_stats %>%
  mutate(
    custom_region = case_when(
      continent == "Europe" ~ "Europe",
      continent == "Asia" ~ "Asia",
      continent == "Africa" ~ "Africa",
      continent == "North America" ~ "North America",
      region %in% c("Latin America & Caribbean") ~ "Latin America & Caribbean",
      continent == "South America" ~ "Latin America & Caribbean",
      country %in% c("AR", "BO", "BR", "CL", "CO", "EC", "FK", "GF", "GY", "PE", "PY", "SR", "UY", "VE") ~ "Latin America & Caribbean",
      TRUE ~ "Other"
    )
  )

# Print top 20 countries
cat("\nTop 20 countries by R downloads in 2024:\n")
print(country_stats %>% head(20) %>% select(country, country_name, downloads, unique_ips))

# Regional statistics
regional_stats <- country_stats %>%
  group_by(custom_region) %>%
  summarise(
    countries = n(),
    total_downloads = sum(downloads),
    total_unique_ips = sum(unique_ips),
    avg_downloads_per_country = mean(downloads),
    .groups = "drop"
  ) %>%
  arrange(desc(total_downloads))

cat("\nRegional statistics for R downloads in 2024:\n")
print(regional_stats)

# Create world map visualization
world_map <- map_data("world")

# Prepare data for mapping
map_data <- country_stats %>%
  mutate(region = country_name) %>%
  select(region, downloads, unique_ips)

# Merge with world map data
world_map_data <- world_map %>%
  left_join(map_data, by = "region")

# Create the world map plot
world_plot <- ggplot(world_map_data, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = downloads), color = "white", size = 0.1) +
  scale_fill_viridis_c(
    name = "R Downloads",
    trans = "log10",
    labels = scales::comma_format(),
    na.value = "grey90"
  ) +
  labs(
    title = "R Downloads by Country in 2024",
    subtitle = "Data from CRAN download logs",
    caption = "Source: http://cran-logs.rstudio.com/"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(hjust = 0.5, size = 10),
    legend.position = "bottom"
  ) +
  coord_fixed(1.3)

# Save the world map
ggsave("outputs/r_downloads_world_map_2024.png", world_plot, 
       width = 12, height = 8, dpi = 300, bg = "white")

# Create regional bar chart
regional_plot <- ggplot(regional_stats, aes(x = reorder(custom_region, total_downloads), y = total_downloads)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  coord_flip() +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(
    title = "R Downloads by Region in 2024",
    x = "Region",
    y = "Total Downloads",
    caption = "Source: CRAN download logs"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12)
  )

# Save the regional plot
ggsave("outputs/r_downloads_regional_2024.png", regional_plot, 
       width = 10, height = 6, dpi = 300, bg = "white")

# Save summary statistics
write_csv(country_stats, "outputs/r_downloads_by_country_2024.csv")
write_csv(regional_stats, "outputs/r_downloads_by_region_2024.csv")

# Print final summary
cat("\n=== SUMMARY ===\n")
cat("Total R downloads in 2024:", format(nrow(all_downloads), big.mark = ","), "\n")
cat("Number of countries:", nrow(country_stats), "\n")
cat("Total unique IP addresses:", format(sum(country_stats$unique_ips), big.mark = ","), "\n")
cat("\nFiles saved:\n")
cat("- outputs/r_downloads_world_map_2024.png\n")
cat("- outputs/r_downloads_regional_2024.png\n")
cat("- outputs/r_downloads_by_country_2024.csv\n")
cat("- outputs/r_downloads_by_region_2024.csv\n")
cat("- data/r_downloads_2024_combined.csv\n")

