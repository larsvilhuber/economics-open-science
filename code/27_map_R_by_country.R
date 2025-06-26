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
library(xtable)
library(tidylog)

# Load data from disk


all_downloads <- readRDS(file.path(interwrk, "r_downloads_2024_combined.rds"))
names(all_downloads)


# Aggregate by country
country_stats <- all_downloads %>%
  group_by(country) %>%
  summarise(
    downloads = sum(downloads, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(downloads)) %>%
# Add country names and region information
  mutate(
    country_name = countrycode(country, "iso2c", "country.name"),
    continent = countrycode(country, "iso2c", "continent"),
    region = countrycode(country, "iso2c", "region")
  ) %>%
  filter(!is.na(country_name))  %>% # Remove invalid country codes
  mutate(
    custom_region = case_when(
      continent == "Europe" ~ "Europe",
      country == "CN" ~ "China",
      continent == "Asia" ~ "Rest of Asia",
      continent == "Africa" ~ "Africa",
      region == "Latin America & Caribbean" ~ "Latin America & Caribbean",
      region == "North America" ~ "North America",
      country == "AU" ~ "Australia",
      continent == "Oceania" ~ "Rest of Oceania",
      TRUE ~ "Other"
    )
  )

# Print top 20 countries
cat("\nTop 20 countries by R downloads in 2024:\n")
print(country_stats %>% head(20) %>% select(country, country_name, downloads, continent, region, custom_region ))

# Regional statistics
regional_stats <- country_stats %>%
  group_by(custom_region) %>%
  summarise(
    countries = n(),
    `Regional Downloads` = sum(downloads),
    .groups = "drop"
  ) %>%
  mutate(Fraction = round(`Regional Downloads`/sum(`Regional Downloads`)*100,2)) %>%
  arrange(desc(`Regional Downloads`))

cat("\nRegional statistics for R downloads in 2024:\n")
print(regional_stats)
# Save Latex Table to file using xtable
xtable(regional_stats, 
                     file = file.path(interwrk, "r_downloads_regional_stats_2024.tex"), 
                     include.rownames = FALSE,
                     caption = "Regional Statistics for R Downloads in 2024",
                     label = "tab:r_downloads_regional_stats_2024",
                     table.placement = "H",
                     size = "small",
                     booktabs = TRUE) 

# Collapse further to Global South/North

global_stats <- regional_stats %>%
   mutate(`North/South` = case_when(
     custom_region == "Europe" ~ "Global North",
     custom_region == "China" ~ "Global North",
     custom_region == "Rest of Asia" ~ "Global South",
     custom_region == "Africa" ~ "Global South",
     custom_region == "Latin America & Caribbean" ~ "Global South",
     custom_region == "North America" ~ "Global North",
     custom_region == "Australia" ~ "Global North",
     custom_region == "Rest of Oceania" ~ "Global South",
       TRUE ~ "Other"
   )) %>%
  group_by(`North/South`) %>%
  summarize(
    Countries = sum(countries),
    Downloads = sum(`Regional Downloads`),
  .groups = "drop"
  ) %>%
  mutate(Fraction = round(Downloads/sum(Downloads)*100,2)) %>%
  arrange(desc(Downloads))


cat("\nNorth/South statistics for R downloads in 2024:\n")
print(global_stats)
# Save Latex Table to file using xtable
xtable(global_stats, 
       file = file.path(interwrk, "r_downloads_global_stats_2024.tex"), 
       include.rownames = FALSE,
       caption = "Regional Statistics for R Downloads in 2024",
       label = "tab:r_downloads_global_stats_2024",
       table.placement = "H",
       size = "small",
       booktabs = TRUE) 

# Save the file so it can be combined with Stata stats

write_csv(all_downloads, file.path(interwrk, "r_downloads_2024_combined.csv"))
saveRDS(all_downloads, file.path(interwrk, "r_downloads_2024_combined.rds"))




# Create world map visualization
world_map <- map_data("world")

table(world_map$region)

# Prepare data for mapping
map_data <- country_stats %>%
  mutate(region = country_name,
         region = case_when(
           country_name == "United States" ~ "USA",
           country_name == "United Kingdom" ~ "UK",
           country_name == "Czechia" ~ "Czech Republic",
           country_name == "Bosnia & Herzegovina" ~ "Bosnia and Herzegovina",
           country_name == "Congo - Kinshasa" ~ "Democratic Republic of the Congo",
           country_name == "Congo - Brazzaville" ~ "Republic of Congo",
           # "Kosovo"
           country_name == "Myanmar (Burma)" ~ "Myanmar",
           country_name == "Côte d’Ivoire" ~ "Ivory Coast",
           # "Namibia"
           country_name == "Eswatini" ~ "Swaziland",
           TRUE ~ region
         )) %>%
  select(region, downloads)

# Merge with world map data
world_map_data <- world_map %>%
  filter(region != "Antarctica") %>%
  left_join(map_data, by = "region")

# Create the world map plot
world_plot <- ggplot(world_map_data, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = downloads), color = "gray70", size = 0.3) +
  scale_fill_viridis_c(
    name = "R Downloads",
    trans = "log10",
    breaks = scales::log_breaks(n = 5),
    labels = scales::label_number(scale_cut = scales::cut_si("")),
    na.value = "grey90",
    direction = -1
  ) +
  labs(
    title = "R Downloads by Country in 2024",
    subtitle = "Data from CRAN download logs"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(hjust = 0.5, size = 10),
    legend.position = "bottom",
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  coord_fixed(1.3)

# Save the world map
ggsave(file.path(outputs, "r_downloads_world_map_2024.png"), world_plot, 
       width = 12, height = 8, dpi = 300, bg = "white")

# Create regional bar chart
regional_plot <- ggplot(regional_stats, aes(x = reorder(custom_region, `Regional Downloads`), y = `Regional Downloads`)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  coord_flip() +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(
    title = "R Downloads by Region in 2024",
    x = "Region",
    y = "Total Downloads"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12)
  )

# Save the regional plot
ggsave(file.path(outputs, "r_downloads_regional_2024.png"), regional_plot, 
       width = 10, height = 6, dpi = 300, bg = "white")

# Save summary statistics
write_csv(country_stats, file.path(interwrk, "r_downloads_by_country_2024.csv"))
write_csv(regional_stats, file.path(interwrk, "r_downloads_by_region_2024.csv"))

# Print final summary
cat("\n=== SUMMARY ===\n")
cat("\nFiles saved:\n")
cat("- ", file.path(outputs, "r_downloads_world_map_2024.png"), "\n")
cat("- ", file.path(outputs, "r_downloads_regional_2024.png"), "\n")
cat("- ", file.path(interwrk, "r_downloads_regional_stats_2024.tex"), "\n")
cat("- ", file.path(interwrk, "r_downloads_global_stats_2024.tex"), "\n")
cat("- ", file.path(interwrk, "r_downloads_2024_combined.csv"), "\n")
cat("- ", file.path(interwrk, "r_downloads_2024_combined.rds"), "\n")
cat("- ", file.path(interwrk, "r_downloads_by_country_2024.csv"), "\n")
cat("- ", file.path(interwrk, "r_downloads_by_region_2024.csv"), "\n")

