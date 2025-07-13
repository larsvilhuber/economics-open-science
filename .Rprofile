
#posit.date <- Sys.Date() - 31
posit.date <- "latest"
# posit.date <- "2020-01-01" # uncomment and set manually if the above does not work

# PPM only snapshots on weekdays (not sure why...)
# Only check for weekday if posit.date is a Date object, not a string
if (!is.character(posit.date) && weekdays(posit.date) %in% c("Saturday","Sunday")) {
  posit.date <- posit.date - 2
}

# Check if running on Linux
if (Sys.info()['sysname'] == "Linux") {
  # Try to determine the Linux distribution and version using /etc/os-release
  if (file.exists("/etc/os-release")) {
    os_release <- system("grep -E '^(ID|VERSION_ID|VERSION_CODENAME|ID_LIKE)=' /etc/os-release", intern = TRUE)
    
    # Extract distribution ID (like ubuntu, debian, rocky)
    distro_id <- gsub("ID=", "", grep("^ID=", os_release, value = TRUE))
    distro_id <- gsub("[\"']", "", distro_id) # Remove quotes if present
    
    # Extract version ID (like 9.4 for Rocky Linux)
    version_id <- gsub("VERSION_ID=", "", grep("^VERSION_ID=", os_release, value = TRUE))
    version_id <- gsub("[\"']", "", version_id) # Remove quotes if present
    
    # Extract codename (like focal, jammy, bullseye)
    codename <- gsub("VERSION_CODENAME=", "", grep("^VERSION_CODENAME=", os_release, value = TRUE))
    
    # Extract ID_LIKE (like rhel, centos, fedora)
    id_like <- gsub("ID_LIKE=", "", grep("^ID_LIKE=", os_release, value = TRUE))
    id_like <- gsub("[\"']", "", id_like) # Remove quotes if present
    
    # If we found Ubuntu or Debian
    if (length(distro_id) > 0 && grepl("^(ubuntu|debian)$", distro_id)) {
      # Set CRAN to binary PPM for Ubuntu/Debian
      options(repos = c(CRAN = paste0("https://packagemanager.posit.co/cran/__linux__/", codename, "/", posit.date)))
      message(paste0("Using binary PPM for Linux distribution: ", distro_id, " (", codename, ")"))
    } else if (length(distro_id) > 0 && distro_id == "rocky" && grepl("^9", version_id)) {
      # Set CRAN to binary PPM for Rocky Linux 9
      options(repos = c(CRAN = paste0("https://packagemanager.posit.co/cran/__linux__/rhel9/", posit.date)))
      message(paste0("Using binary PPM for Linux distribution: ", distro_id, " (version ", version_id, ")"))
    } else if (length(distro_id) > 0 && distro_id == "opensuse-leap" && version_id == "15.6") {
      # Set CRAN to binary PPM for opensuse-leap 15.6
      options(repos = c(CRAN = paste0("https://packagemanager.posit.co/cran/__linux__/opensuse156/",posit.date)))
      message(paste0("Using binary PPM for Linux distribution: ", distro_id, " (version ", version_id, ")"))
    } else {
      # Use standard PPM with date-based snapshot for other Linux
      options(repos = c(CRAN = paste0("https://packagemanager.posit.co/cran/", posit.date)))
    }
  } else {
    # Use standard PPM with date-based snapshot if os-release not available
    options(repos = c(CRAN = paste0("https://packagemanager.posit.co/cran/", posit.date)))
  }
} else {
  # Use standard PPM with date-based snapshot for non-Linux systems
  options(repos = c(CRAN = paste0("https://packagemanager.posit.co/cran/", posit.date)))
}



source("renv/activate.R")
