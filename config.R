# Path names

basedir=here::here()
datadir=file.path(basedir, "data")
codedir=file.path(basedir, "code")
outputs=file.path(basedir, "outputs")
confdir=file.path(datadir,"confidential")
interwrk=file.path(datadir,"interwrk")
rawdata=file.path(datadir,"raw")

# Some parameters

## AEA DOI prefix

doi_prefix <- "10.1257"

## NBER prefix

nber_prefix <- "10.3386"

from_date <- "2024-01-01"
until_date <- "2024-12-31"

# filenames

issns.file <- file.path(datadir,"issns.Rds")

doi.file <- file.path(datadir,"crossref_dois")
doi.file.Rds <- paste(doi.file,"Rds",sep=".")
doi.file.csv <- paste(doi.file,"csv",sep=".")

doi.enhanced.file <- file.path(datadir,"crossref_dois_enhanced")
doi.enhanced.file.Rds <- paste(doi.enhanced.file,"Rds",sep=".")
doi.enhanced.file.csv <- paste(doi.enhanced.file,"csv",sep=".")

# Some restricted files need to be pulled from Dropbox and need the following values:
# The URL is composed of "www.dropbox.com", BASE, project name, "&rl=RLKEY" and for download, "&dl=1" 
#
#DROPBOX_SECRET_BASE=""
#DROPBOX_SECRET_RLKEY=""
#
#These can be added to a `.Renviron` in this directory, or otherwise grabbed from the environment


for (dir in c(basedir, datadir, codedir, outputs, confdir, interwrk)) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
}

# Populate the confidential dir from Dropbox, issue a message otherwise

confdata.present = FALSE

# Define project by directory, the last component is the project

project.name <- basename(basedir)

# Get the Dropbox files
if (dir.exists(confdir)) {
  if (length(dir(confdir)) == 0) {
    message("Confidential data directory is empty.  Will try to download.")
    DROPBOX_SECRET_BASE=Sys.getenv("DROPBOX_SECRET_BASE")
    DROPBOX_SECRET_RLKEY=Sys.getenv("DROPBOX_SECRET_RLKEY")
    if (DROPBOX_SECRET_BASE != "" & DROPBOX_SECRET_RLKEY != "") {
      # get the Dropbox files and unpack into confdir
      DROPBOX.URL = paste0("https://www.dropbox.com/",DROPBOX_SECRET_BASE,"/",
                           project.name,"?rlkey=",DROPBOX_SECRET_RLKEY,"&dl=1")
      download.file(DROPBOX.URL,
                    destfile = file.path(confdir,"download.zip"))
    }
  } else {
    message("Files already there, not re-downloading")
  }
  # now check for the ZIP file, and unzip
    if (file.exists(file.path(confdir,"download.zip"))) {
        unzip(file.path(confdir,"download.zip"), exdir = confdir)
        file.remove(file.path(confdir,"download.zip"))
    }
}
# check again
if (dir.exists(confdir)) {
  if (length(dir(confdir)) == 0) {
    message("Confidential data directory is empty.  Please populate it with the confidential data.")
  } else {    confdata.present = TRUE}
}


## Some data files that can change. Change the names here

jira_access <- "jira-search-84765891-47e0-43ec-874d-365f527ef997.xlsx"

##### Functions

# Add sanitize_tex function before the main function
sanitize_tex <- function(str) {
    if (is.character(str)) {
        str <- gsub("&", "\\\\&", str)
        str <- gsub("%", "\\\\%", str)
        str <- gsub("_", "\\\\_", str)
        str <- gsub("#", "\\\\#", str)
        str <- gsub("\\$", "\\\\$", str)
        str <- gsub("\\{", "\\\\{", str)
        str <- gsub("\\}", "\\\\}", str)
        str <- gsub("~", "\\\\~{}", str)
        str <- gsub("\\^", "\\\\^{}", str)
    }
    return(str)
}


# Requires: readxl, xtable
# Function to create LaTeX table from Excel
excel_to_latex <- function(excel_file, 
                          output_file = "table.tex",
                          caption = "Table Caption",
                          label = "tab:mytable",    # New parameter
                          sheet = 1,
                          digits = 2,
                          align = NULL,
                          footnotes = NULL,
                          include_rownames = FALSE) {
    
    # Read Excel file
    # excel_file=file.path(datadir,"restrictions.xlsx")
    data <- read_excel(excel_file, sheet = sheet)

    # Format numbers and sanitize text
    data <- as.data.frame(lapply(data, function(x) {
        if (is.numeric(x)) {
            formatted <- ifelse(x %% 1 == 0,  
                              ifelse(abs(x) >= 1000,
                                    format(x, big.mark = ",", scientific = FALSE),
                                    as.character(x)),
                              ifelse(abs(x) >= 1000,
                                    format(round(x, 1), big.mark = ",", scientific = FALSE),
                                    as.character(round(x, 1))))
            return(formatted)
        } else {
            # Sanitize text columns
            return(as.character(unlist(lapply(x, sanitize_tex))))
        }
    }), stringsAsFactors = FALSE)

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
                         label = label,    # Add label here
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


get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

pkgTest <- function(x)
{
	if (!require(x,character.only = TRUE))
	{
		install.packages(x,dep=TRUE)
		if(!require(x,character.only = TRUE)) stop("Package not found")
	}
	return("OK")
}