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
                          footnotes = NULL,
                          include_rownames = FALSE) {
    
    # Read Excel file
    # excel_file=file.path(datadir,"restrictions.xlsx")
    data <- read_excel(excel_file)

    # Format numbers: commas for thousands and round non-integers to 1 decimal
    data <- as.data.frame(lapply(data, function(x) {
        if (is.numeric(x)) {
            formatted <- ifelse(x %% 1 == 0,  # check if integer
                              # For integers
                              ifelse(abs(x) >= 1000,
                                    format(x, big.mark = ",", scientific = FALSE),
                                    as.character(x)),
                              # For non-integers
                              ifelse(abs(x) >= 1000,
                                    format(round(x, 1), big.mark = ",", scientific = FALSE),
                                    as.character(round(x, 1))))
            return(formatted)
        }
        return(x)
    }))
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
    # Prepare footnotes if provided
    if (!is.null(footnotes)) {
        footnote_text <- paste("\\multicolumn{", ncol(data), 
                             "}{l}{\\textit{Notes:} ", 
                             paste(footnotes, collapse = " "),
                             "}", sep = "")
        add.to.row <- list(pos = list(nrow(data)),
                          command = paste("\\midrule", footnote_text, "\n"))
    } else {
        add.to.row <- NULL
    }
    
    
    # Print LaTeX table with specified formatting
    print(latex_table,
          file = output_file,
          type = "latex",
          include.rownames = include_rownames,
          floating = TRUE,
          table.placement = "h",
          caption.placement = "top",
          latex.environments = c("center"),
          booktabs = TRUE,
          sanitize.text.function = function(x){x},
          add.to.row = add.to.row)
    
    
    cat("LaTeX table has been written to", output_file, "\n")
}

# Example usage:
# excel_to_latex("your_file.xlsx", 
#                "output_table.tex", 
#                digits = 2,
# caption = "Employee Information",
#                footnotes = c("Salaries are in USD.", 
#                            "Data as of 2023."))
# )