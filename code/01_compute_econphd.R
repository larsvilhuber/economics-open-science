# Compute the number of Econ grads
# Data file was manually downloaded.

source(here::here("config.R"))
library(readxl)
library(stringr)
library(dplyr)
library(tidyr)
library(tidylog)

# Read the Excel file
xlsx_file <- file.path(datadir,"nsf24336-tab001-005.xlsx")
nsf_data <- read_excel(xlsx_file,skip = 3)

# Find the row with "Economics" and extract the numbers
economics_row <- which(str_trim(nsf_data[[1]]) == "Economics")
if (length(economics_row) != 3) {
  stop("Verify data structure")
}
# The first row will be the all-sexes
economics_grads <- nsf_data[economics_row[1],] %>%
  select(starts_with("Field"),starts_with("2")) %>%
  # make long with the value of first column being the name of the variable
  pivot_longer(cols = -1, names_to = "year", values_to = "graduates") 
  
mean_economics_grads <- economics_grads  %>%
  # summarize
  summarise(value = mean(graduates, na.rm = TRUE))  


latexnums <- mean_economics_grads %>%
  mutate(pre="\\newcommand{\\",mid="}{",end="}",field="economicsgrads") %>%
  unite(latexcode,c("pre","field","mid","value","end"),sep = "")
write(latexnums$latexcode,file=file.path(outputs,"economicsgrads.tex"))
