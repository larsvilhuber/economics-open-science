# Compare R and Stata Download Statistics by Region and North/South
# This script loads the regional and North/South statistics from the R and Stata analyses,
# computes the percent difference, and outputs combined tables and a horizontal histogram of differences.

source(file.path(rprojroot::find_root(rprojroot::has_file("config.R")),"config.R"),echo=FALSE)
library(tidyverse)
library(xtable)
library(scales)
library(ggplot2)
library(viridis)
library(patchwork)
library(ggpattern)

# Load regional stats
stata_reg <- readRDS(file.path(interwrk, "stata_downloads_by_region_2024_nochina.rds"))
r_reg <- readRDS(file.path(interwrk, "r_downloads_by_region_2024_nochina.rds"))

# Combine and compute percent difference for regions
region_compare <- full_join(
  stata_reg %>% select(custom_region, Stata = Fraction),
  r_reg %>% select(custom_region, R = Fraction),
  by = "custom_region"
) %>%
  filter(custom_region != "Other") %>%
  mutate(Diff = Stata - R) %>%
  arrange(desc(abs(Diff)))

# Save as LaTeX and CSV
print(xtable(region_compare, digits = 2, caption = "Regional Share of Downloads: Stata vs R", label = "tab:region_compare"),
      file = file.path(outputs, "compare_region_stats_2024.tex"))
write_csv(region_compare, file.path(outputs, "compare_region_stats_2024.csv"))



# Load North/South stats
stata_ns <- readRDS(file.path(interwrk, "stata_downloads_by_global_2024_nochina.rds"))
r_ns <- readRDS(file.path(interwrk, "r_downloads_by_global_2024_nochina.rds"))

# Combine and compute percent difference for North/South
ns_compare <- full_join(
  stata_ns %>% select(`North/South`, Stata = Fraction),
  r_ns %>% select(`North/South`, R = Fraction),
  by = "North/South"
) %>%
  filter(`North/South` != "Other") %>%
  mutate(Diff = Stata - R) %>%
  arrange(desc(abs(Diff)))

# Save as LaTeX and CSV
print(xtable(ns_compare, digits = 2, caption = "North/South Share of Downloads: Stata vs R", label = "tab:ns_compare"),
      file = file.path(outputs, "compare_ns_stats_2024.tex"))
write_csv(ns_compare, file.path(outputs, "compare_ns_stats_2024.csv"))



# Use a color from the viridis 'plasma' scale, about 33% from the left
plasma_orange <- viridis::plasma(100)[70]
plasma_purple <- viridis::plasma(100)[30]

# Plot horizontal histogram of differences for regions, annotate columns so text is inside the bars and aligned with zero
# Find the region with the max positive and max negative Diff
spacing_region <- 0.2
region_max_pos <- region_compare$custom_region[which.max(region_compare$Diff)]
region_max_neg <- region_compare$custom_region[which.min(region_compare$Diff)]

# Compute global y-axis limits for both plots
all_diffs <- c(ns_compare$Diff, region_compare$Diff)
ymin <- min(all_diffs) - 0.1
ymax <- max(all_diffs) + 0.1









# Create a version of the region plot without annotations for the combined plot, with hashmarks
region_diff_plot_no_annot <- ggplot(region_compare, aes(x = reorder(custom_region, Diff), y = Diff)) +
  geom_col_pattern(
    fill = plasma_orange,
    alpha = 1,
    pattern = "stripe",
    pattern_fill = plasma_orange,
    pattern_angle = 45,
    pattern_density = 0.5,
    pattern_spacing = 0.03,
    pattern_colour = "#222222",
    pattern_size = 0.3
  ) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(ymin, ymax)) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.margin = margin(t = 30, r = 10, b = 10, l = 10)
  )

# Add annotations
region_diff_plot <- region_diff_plot_no_annot +
  # Annotate the columns so text is inside the bars and aligned with zero
  annotate("text", x = region_max_pos, y = 0 + spacing_region, label = "More Stata downloads", hjust = 0, vjust = 0.5, size = 4, fontface = "italic", color = "white") +
  annotate("text", x = region_max_neg, y = 0 - spacing_region, label = "More R downloads", hjust = 1, vjust = 0.5, size = 4, fontface = "italic", color = "white")
  
ggsave(file.path(outputs, "compare_region_diff_2024.png"), region_diff_plot, width = 10, height = 5, dpi = 300)









# Plot horizontal histogram of differences for North/South, annotate columns so text is inside the bars and aligned with zero
spacing <- 0.2
ns_diff_plot <- ggplot(ns_compare, aes(x = reorder(`North/South`, Diff), y = Diff)) +
  geom_col(fill = plasma_purple, alpha = 1) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  # Annotate the columns so text is inside the bars and aligned with zero
  annotate("text", x = "Global North", y = 0 + spacing, label = "More Stata downloads", 
          hjust = 0, vjust = 0.5, size = 4, fontface = "italic", color = "white") +
  annotate("text", x = "Global South", y = 0 - spacing, label = "More R downloads", 
          hjust = 1, vjust = 0.5, size = 4, fontface = "italic", color = "white") +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(ymin, ymax)) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.margin = margin(t = 30, r = 10, b = 10, l = 10)
  )

ggsave(file.path(outputs, "compare_ns_diff_2024.png"), ns_diff_plot, width = 7, height = 3, dpi = 300)
ggsave(file.path(outputs, "compare_region_diff_2024.png"), region_diff_plot, width = 10, height = 5, dpi = 300)

ns_diff_plot_no_ticks <- ns_diff_plot + theme(
  axis.text.x = element_blank(),
  axis.ticks.x = element_blank()
)

# Combine the ns and region plots (no annotation on region plot)
combined_plot <- ns_diff_plot_no_ticks / region_diff_plot_no_annot + 
    plot_layout(heights = c(1, 2))
ggsave(file.path(outputs, "compare_combined_diff_2024.png"), combined_plot, 
       width = 10, height = 8, dpi = 300)

cat("\n=== SUMMARY ===\n")
cat("\nFiles saved:\n")
cat("- ", file.path(outputs, "compare_region_stats_2024.tex"), "\n")
cat("- ", file.path(outputs, "compare_region_stats_2024.csv"), "\n")
cat("- ", file.path(outputs, "compare_region_diff_2024.png"), "\n")
cat("- ", file.path(outputs, "compare_ns_stats_2024.tex"), "\n")
cat("- ", file.path(outputs, "compare_ns_stats_2024.csv"), "\n")
cat("- ", file.path(outputs, "compare_ns_diff_2024.png"), "\n")
cat("- ", file.path(outputs, "compare_combined_diff_2024.png"), "\n")
