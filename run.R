# Run all R scripts in independent sessions
# This R script runs the same programs as run.sh but in separate R sessions

renv::restore()

# List of scripts to run in order
scripts <- c(
  "code/01_compute_econphd.R",
  "code/02_convert_tables.R", 
  "code/03_tabulate_access_aea.R",
  "code/04_tabulate_access_categories.R",
  "code/11_get_crossref.R",
  "code/13_analyse_authors.R",
  "code/20_geocode_stata_step1.R",
  "code/21_geocode_stata_step2.R",
  "code/22_generate_download_map.R",
  "code/25_download_R_logs.R",
  "code/26_aggregate_data.R",
  "code/27_map_R_by_country.R",
  "code/28_stata_map_by_country.R",
  "code/29_compare_stata_r_by_region.R",
  "code/99_zz_info.R",
  "code/99_zzz_compile_readme.R"
)

# Function to run a script in an independent R session
run_script <- function(script_path) {
  output_file <- paste0(tools::file_path_sans_ext(script_path), ".Rout")
  cmd <- paste("R --no-restore --no-save -f", script_path, ">", output_file, "2>&1")
  
  cat("Running:", script_path, "\n")
  result <- system(cmd, wait = TRUE)
  
  if (result != 0) {
    cat("ERROR: Script", script_path, "failed with exit code", result, "\n")
    return(FALSE)
  } else {
    cat("SUCCESS: Script", script_path, "completed\n")
    return(TRUE)
  }
}

# Run all scripts sequentially
cat("Starting batch execution of R scripts...\n\n")

for (script in scripts) {
  success <- run_script(script)
  if (!success) {
    error("Stopping execution due to error in", script, "\n")
    break
  }
  cat("\n")
}

cat("Batch execution completed.\n")

