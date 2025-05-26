# Check and install required packages
cat("Starting package check...\n")

required_packages <- c("tidyverse", "tidytext", "lubridate", "stringr", "ggplot2", "scales", "zoo", "here")

# Function to check and install packages
check_and_install <- function(packages) {
    for (package in packages) {
        cat(sprintf("\nChecking package: %s\n", package))
        if (!require(package, character.only = TRUE, quietly = TRUE)) {
            cat(sprintf("Installing package: %s\n", package))
            tryCatch(
                {
                    install.packages(package, repos = "https://cloud.r-project.org")
                    if (require(package, character.only = TRUE)) {
                        cat(sprintf("Successfully installed and loaded %s\n", package))
                    } else {
                        cat(sprintf("Failed to load %s after installation\n", package))
                    }
                },
                error = function(e) {
                    cat(sprintf("Error installing %s: %s\n", package, e$message))
                }
            )
        } else {
            cat(sprintf("Package %s is already installed and loaded\n", package))
        }
    }
}

# Run the check
cat("Starting package installation check...\n")
check_and_install(required_packages)
cat("\nPackage check complete.\n")
