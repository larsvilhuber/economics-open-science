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
    dplyr::bind_rows(
        tibble::tibble(
            DCAF_Access_Restrictions_V2 = "Total",
            Yes = sum(access_table$Yes, na.rm = TRUE),
            No = sum(access_table$No, na.rm = TRUE),
            Total = sum(access_table$Total, na.rm = TRUE)
        )
    )

# For the print table, 
# - replace the DCAF_Access_Restrictions_V2 = No/Yes column with "n/a" if the value is 0
# - Add a column with the percentage of "Yes" next to the "Yes" column
print_access_table <- access_table_totals %>%
    dplyr::mutate(
        Yes_percent = scales::percent(Yes / Total, accuracy = 0.01),
        Yes = ifelse(Yes == 0, "n/a", Yes)
    ) %>%
# the percent column should be printed in parenthesis and italics
# The footnote should explain that the percentages are calculated as the number of "Yes" divided by the total number of responses
    dplyr::mutate(
        Yes_percent = ifelse(Yes_percent == "n/a", Yes_percent, paste0("(", Yes_percent, ")"))
    ) %>%
    select("Access restrictions"=DCAF_Access_Restrictions_V2,No,Yes,"Percent"=Yes_percent,Total)

# Create the LaTeX table
kable(print_access_table, 
      format = "latex", 
      booktabs = TRUE, 
      label = "access_provision",
      caption = "Access categories and whether data can be shared privately") %>%
  kable_styling(latex_options = "hold_position") %>%
  add_footnote("Percentages are calculated as the number of 'Yes' divided by the total number of responses. An article can have multiple categories of data; the sum of responses is therefore higher than the number of articles.", 
               notation = "symbol") %>%
  save_kable(file.path(outputs, "table_access_provision.tex"))


