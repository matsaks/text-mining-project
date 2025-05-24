# R Profile for Sentiment Analysis Project

# Set default CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    tidyverse,
    tidytext,
    lubridate,
    stringr,
    ggplot2,
    scales,
    zoo
)

# Set default theme for ggplot2
theme_set(theme_minimal())

# Set default options
options(
    stringsAsFactors = FALSE,
    scipen = 999,
    digits = 4
)

# Create project directories if they don't exist
dirs <- c(
    "data/raw",
    "data/processed",
    "output/figures",
    "output/results",
    "R",
    "docs"
)

for (dir in dirs) {
    if (!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE)
    }
}
