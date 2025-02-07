# Path names

basedir=here::here()
datadir=file.path(basedir, "data")
codedir=file.path(basedir, "code")
outputs=file.path(basedir, "outputs")

for (dir in c(basedir, datadir, codedir, outputs)) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
}


##### Functions

# Requires: readxl, xtable
# Function to create LaTeX table from Excel
excel_to_latex <- function(excel_file, 
                          output_file = "table.tex",
                          caption = "Table Caption",
                          digits = 2,
                          align = NULL,
                          include_rownames = FALSE) {
    
    # Read Excel file
    # excel_file=file.path(datadir,"restrictions.xlsx")
    data <- read_excel(excel_file)

    # If align is not specified, create default alignment based on data types
    if (is.null(align)) {
        # First position is for row names (if included)
        align_vec <- "l"  
        
        # For each column, check type and assign appropriate alignment
        for(col in data) {
            if (is.numeric(col)) {
                align_vec <- c(align_vec, "r")  # right align numbers
            } else {
                align_vec <- c(align_vec, "l")  # left align text
            }
        }
    } else {
        # Use user-provided alignment
        # Check if alignment vector length matches data
        if (length(align) != ncol(data) + 1) {
            stop("Alignment vector must have length ", ncol(data) + 1, 
                 " (including row names position)")
        }
        align_vec <- align
    }
    
    
    # Convert to LaTeX table
    latex_table <- xtable(data,
                         caption = caption,
                         align = align_vec,
                         digits = digits)
    
    # Write to file with formatting
    sink(output_file)
    
    # Print LaTeX table with specified formatting
    print(latex_table,
          type = "latex",
          include.rownames = include_rownames,
          floating = TRUE,
          table.placement = "h",
          caption.placement = "top",
          latex.environments = c("center"),
          booktabs = TRUE,
          sanitize.text.function = function(x){x})
    
    sink()
    
    cat("LaTeX table has been written to", output_file, "\n")
}

# Example usage:
# excel_to_latex("your_file.xlsx", 
#                "output_table.tex", 
#                "Your Table Caption",
#                digits = 2)