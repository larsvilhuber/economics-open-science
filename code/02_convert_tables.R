# convert the restrictions to a LaTeX table

source(here::here("config.R"))
library(readxl)
library(xtable)
library(tidylog)

# Read the restictions, write out LaTeX table


excel_to_latex(file.path(datadir,"restrictions.xlsx"), 
               file.path(outputs,"table_restrictions.tex"), 
                "Example restrictions",
                label= "tab:restrictions",
                align = c("l","l", "l", "p{4in}"),
                digits = 2)

excel_to_latex(file.path(datadir,"restrictions.xlsx"),
               file.path(outputs,"table_categories.tex"), 
                "Restriction categories",
                label= "tab:categories",
                sheet= 2,
                align = c("l","l", "p{4in}"),
                digits = 2)

excel_to_latex(file.path(datadir,"stata-licenses-by-country.xlsx"), 
               file.path(outputs,"table_stata-licenses.tex"), 
                "Software licensing internationally",
                digits = 0,
                label = "tab:licensecost",
                align = c("l", "l", "r", "r","r"),
                footnotes = "Amounts in USD as of 2025. Source for Stata license prices: Stata website.")