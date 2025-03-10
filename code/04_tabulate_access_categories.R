# Tabulate the categories
source(here::here("config.R"))

library(dplyr)
library(knitr)
library(kableExtra)
library(stringr)


# Load the cleaned JIRA access data
# Still contains multiple categories in the fields
# e.g. "Moderately Easy to Obtain;Moderately Difficult to Obtain;Yes, data can be made available privately"
jira_access_cleaned <- readRDS(file.path(datadir, "jira_access_cleaned.Rds")) %>%
    # if one of the ;-separated fields is "Yes, data can be made available privately", set the variable "Privately shareable" to "Yes"
    dplyr::mutate(Privately_shareable = ifelse(grepl("Yes, data can be made available privately", DCAF_Access_Restrictions_V2), "Yes", "No")) %>%
    # now remove that value from field DCAF_Access_Restrictions_V2
    dplyr::mutate(DCAF_Access_Restrictions_V2 = gsub("Yes, data can be made available privately", "", DCAF_Access_Restrictions_V2)) %>%
    # split the field DCAF_Access_Restrictions_V2 into multiple rows
    tidyr::separate_rows(DCAF_Access_Restrictions_V2, sep = ";") %>%
    # drop rows with empty DCAF_Access_Restrictions_V2
    dplyr::filter(DCAF_Access_Restrictions_V2 != "") %>%
    # remove leading and trailing whitespaces
    dplyr::mutate(DCAF_Access_Restrictions_V2 = stringr::str_trim(DCAF_Access_Restrictions_V2)) %>%
    dplyr::mutate(DCAF_Access_Restrictions_V2 = factor(DCAF_Access_Restrictions_V2, 
                                                       levels = c("No", "Very Easy to Obtain", "Moderately Easy to Obtain", "Moderately Difficult to Obtain", "Very Difficult to Obtain"))) 

# Cross-Tabulate access categories and "Privately shareable"
# with rows coming from the access categories and columns from the "Privately shareable" variable
# and count the number of occurrences
# Order should be: No, Very Easy to Obtain, Moderately Easy to Obtain, Moderately Difficult to Obtain, Very Difficult to Obtain   
	
access_table <- jira_access_cleaned %>%
    dplyr::count(DCAF_Access_Restrictions_V2, Privately_shareable) %>%
    tidyr::pivot_wider(names_from = Privately_shareable, values_from = n, values_fill = list(n = 0)) %>%
    dplyr::mutate(Total = Yes + No) 

# Add a row with the column sums
access_table_totals <- access_table %>%
    filter(DCAF_Access_Restrictions_V2 != "No") %>%
    summarise(No=sum(No),Yes=sum(Yes),Total=sum(Total)) %>%
    mutate(DCAF_Access_Restrictions_V2 = "Any restriction") %>%
    select(DCAF_Access_Restrictions_V2,No,Yes,Total)


# For the print table, 
# - replace the DCAF_Access_Restrictions_V2 = No/Yes column with "n/a" if the value is 0
# - Add a column with the percentage of "Yes" next to the "Yes" column
print_access_table <- bind_rows(access_table,access_table_totals) %>%
    dplyr::mutate(
        Yes_percent = scales::percent(Yes / Total, accuracy = 0.01),
        Yes = ifelse(Yes == 0, "n/a", Yes),
        No  = ifelse(DCAF_Access_Restrictions_V2 == "No","",No),
        Yes_percent = ifelse(Yes == "n/a","",Yes_percent)
    ) %>%
# the percent column should be printed in parenthesis and italics
# The footnote should explain that the percentages are calculated as the number of "Yes" divided by the total number of responses
    dplyr::mutate(
        Yes_percent = ifelse(Yes_percent == "", Yes_percent, paste0("(", Yes_percent, ")"))
    ) %>%
    select("Access restrictions"=DCAF_Access_Restrictions_V2,No,Yes,Total,"Percent"=Yes_percent)

# Create the LaTeX table
kable(print_access_table, 
      format = "latex", 
      booktabs = TRUE, 
      label = "access_provision",
      caption = "Access categories and whether data can be shared privately") %>%
  kable_styling(latex_options = "hold_position") %>%
  row_spec(1, hline_after = TRUE) %>%
  row_spec(nrow(print_access_table) - 1, hline_after = TRUE) %>%
  add_footnote("Percentages are calculated as the number of 'Yes' divided by the total number of responses. An article can have multiple categories of data; the sum of responses is therefore higher than the number of articles.", 
               notation = "symbol") %>%
  save_kable(file.path(outputs, "table_access_provision.tex"))

# Create a horizontal bar graph visualization
library(ggplot2)
library(viridis)

# Prepare data for graph - convert print_access_table back to numeric for plotting
plot_data <- bind_rows(access_table, access_table_totals) %>%
  # Convert factors to character to ensure proper ordering
  mutate(DCAF_Access_Restrictions_V2 = as.character(DCAF_Access_Restrictions_V2)) %>%
  # Calculate percentages for labels
  mutate(
    Yes_percent = ifelse(Total > 0, Yes/Total, 0),
    NAx = ifelse(DCAF_Access_Restrictions_V2 == "No",Total,0),
    No  = if_else(NAx == Total,0,No),
    Total = ifelse(NAx == Total,0,Total),
    Yes_label_pct = ifelse(Yes > 0, scales::percent(Yes_percent, accuracy = 0.1), ""),
    Yes_label_count = ifelse(Yes > 0, as.character(Yes), ""),
    No_label_count = ifelse(No > 0, as.character(No), ""),
    NAx_label_count = ifelse(NAx > 0, as.character(NAx), "")
  )

# Set the order for the categories
plot_data$DCAF_Access_Restrictions_V2 <- factor(
  plot_data$DCAF_Access_Restrictions_V2,
  levels = c("Any restriction", "Very Difficult to Obtain", "Moderately Difficult to Obtain", 
             "Moderately Easy to Obtain", "Very Easy to Obtain", "No")
)

# need the max of all total counts - we'll add 10
maxcount <- max(plot_data$Total)

# Create the horizontal bar graph
#access_plot <- 
ggplot(plot_data, aes(x = DCAF_Access_Restrictions_V2)) +
  theme_minimal() +
  coord_flip() +
  # Convert data to long format for proper stacking
  geom_col(aes(y = maxcount + 10, fill = "No"), width = 0.7, alpha = 0) +
  geom_col(aes(y = NAx, fill = "NAx"), width = 0.7) +
  geom_col(aes(y = Total, fill = "No"), width = 0.7) +
  geom_col(aes(y = Yes, fill = "Yes"), width = 0.7) +
  # Add "Yes" percentage labels inside bars
  geom_text(aes(label = Yes_label_pct, y = maxcount + 10 ), 
            hjust = 0.5, 
            color = "black", 
            fontface = "bold",
            size = 3.5) +
  # Add "No" count labels outside bars to the right
  geom_text(aes(label = No_label_count, y = Yes + No/2 - 1), 
            hjust = -0.3, 
            color = "white", 
            fontface = "bold",
            size = 3.5) +
  # Add "Yes" count labels inside bars
  geom_text(aes(label = Yes_label_count, y = Yes/2 - 1), 
            hjust = -0.3, 
            color = "black", 
            fontface = "bold",
            size = 3.5) +
  # Add "NA" count labels inside bars
  geom_text(aes(label = NAx_label_count, y = NAx/2 - 1), 
            hjust = -0.3, 
            color = "black", 
            fontface = "bold",
            size = 3.5) +
  scale_fill_manual(values = c("No" = "#1F9E89FF", "Yes" = "#31688EFF", "NAx" = "#B4DE2CFF"),
                   name = "Can be shared privately",
                   labels = c("NA","No","Yes")) +
  labs(
    title = "Access categories and whether data can be shared privately",
    x = "",
    y = "Number of cases"
  ) +
  # Add extra space on the right for the external labels
  #scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    #panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Save the plot
ggsave(
  file.path(outputs, "figure_access_provision.pdf"),
  access_plot,
  width = 8,
  height = 5,
  device = cairo_pdf
)

# Also save as PNG for potential inclusion in presentations
ggsave(
  file.path(outputs, "figure_access_provision.png"),
  access_plot,
  width = 8,
  height = 5,
  dpi = 300
)


