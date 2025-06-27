# Generate a world map of Stata downloads
# The map shows circles for each country, with the diameter defined by 
# count of downloads normalized by the average download for any country

source(file.path(rprojroot::find_root(rprojroot::has_file("config.R")),"config.R"),echo=FALSE)
library(dplyr)
library(ggplot2)
library(viridis)
library(maps)
library(mapdata)
library(tidyr)
library(tidylog)

# Check if geolocations data exists
if (file.exists(file.path(interwrk,"geolocations.rds"))) {
  geolocations <- readRDS(file.path(interwrk,"geolocations.rds"))
} else {
  stop("geolocations.rds not found in interwrk directory. Please run 11_geocode_stata_step2.R first.")
}
  # Rename some countries to match the names in the map data
country_name_map <- c(
  "United States" = "USA",
  "United Kingdom" = "UK",
  "Türkiye" = "Turkey",
  "The Netherlands" = "Netherlands",
  "Hong Kong" = "China",
  "Macao" = "China"
  # Add more mappings if needed
)

# Count downloads by country
country_downloads <- geolocations %>%
    filter(!is.na(country_name)) %>%
    mutate(map_country = ifelse(country_name %in% names(country_name_map), 
                                                         country_name_map[country_name], 
                                                         country_name)) %>%
    group_by(map_country) %>%
    summarise(download_count = n(), .groups = "drop")

# Calculate average downloads per country
avg_downloads <- mean(country_downloads$download_count, na.rm = TRUE)

# Calculate median downloads per country
median_downloads <- median(country_downloads$download_count, na.rm = TRUE)

# Calculate number of countries with more than 10 downloads

country_downloads %>% filter(download_count > 50) %>% distinct(map_country) %>% nrow()

# Normalize by median downloads
country_downloads <- country_downloads %>%
    mutate(normalized_downloads = download_count / median_downloads)

# Get world map data
world_map <- map_data("world")


# Calculate centroids for each country to place circles
other_centroids <- world_map %>%
  filter(region != "USA" & region != "Antarctica") %>%
  group_by(region) %>%
  summarise(
    long = mean(long, na.rm = TRUE),
    lat = mean(lat, na.rm = TRUE),
    .groups = "drop"
    ) %>%
    # also filter out islands in the Pacific by longitude
    filter(long > -125 & long < 160)
  
# Special for the US: Calculate mainland USA centroid
# Properly filter out Alaska and Hawaii using their longitude/latitude bounds
usa_centroids <- world_map %>%
  filter(region == "USA") %>%
  # Continental US roughly between these coordinates
  filter(long > -125 & long < -65 & lat > 25 & lat < 50) %>%
  summarise(
    region = "USA",
    long = mean(long, na.rm = TRUE),
    lat = mean(lat, na.rm = TRUE)
  )

# Add the US centroid back to the country centroids
country_centroids <- bind_rows(other_centroids, usa_centroids) %>%
   # Shift Canada towards the south
    mutate(lat = ifelse(region == "Canada",  54, lat),
           long = ifelse(region == "Canada",  -100, long))

# Join download data with centroids
country_data <- country_centroids %>%
  left_join(country_downloads, by = c("region" = "map_country")) %>%
  filter(!is.na(normalized_downloads)) 

# Create the map
download_map <- ggplot() +
  geom_map(data = world_map, map = world_map %>% filter(region != "Antarctica"),
           aes(x = long, y = lat, map_id = region),
           color = "gray70", fill = "gray90", size = 0.3) +
  geom_point(data = country_data, 
             aes(x = long, y = lat, size = normalized_downloads, color = normalized_downloads),
             alpha = 0.7) +
  scale_size_continuous(name = "Downloads\n(normalized)", range = c(0, 10)) +
  scale_color_viridis(name = "Downloads\n(normalized)",direction = -1) +
  coord_fixed(1.3) +
  theme_minimal() +
  labs(
    title = "Stata Downloads by Country",
    subtitle = "Circle size represents downloads normalized by median downloads per country",
    caption = "Source: SSC log files"
  ) +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

# Save the map
ggsave(file.path(outputs, "stata_download_map.png"), download_map, width = 10, height = 5, dpi = 300)


message("Map generation complete. Output saved to ", file.path(outputs, "stata_download_map.png"))
