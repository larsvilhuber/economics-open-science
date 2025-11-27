#!/usr/bin/env Rscript
#
# Compile README.Rmd to README.md and README.html
#
# This script renders the README.Rmd file to produce the README.md
# and README.html files.

# Load required library
library(rmarkdown)

# Get the project root directory
root_dir <- rprojroot::find_rstudio_root_file()

# Define paths
readme_rmd <- file.path(root_dir, "README.Rmd")
readme_md <- file.path(root_dir, "README.md")
readme_html <- file.path(root_dir, "README.html")

# Check if README.Rmd exists
if (!file.exists(readme_rmd)) {
  stop("README.Rmd not found at: ", readme_rmd)
}

# Render the README.Rmd file
cat("Compiling README.Rmd...\n")
rmarkdown::render(
  input = readme_rmd,
  output_format = "all",
  quiet = FALSE
)

cat("README compilation complete.\n")
cat("Generated files:\n")
cat("  - ", readme_md, "\n")
cat("  - ", readme_html, "\n")
