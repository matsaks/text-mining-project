# Data Processing Script for News Articles Sentiment Analysis
# Author: [Your Name]
# Date: [Current Date]

# Load required packages
library(tidyverse)
library(tidytext)
library(lubridate)
library(stringr)
library(data.table)

# Function to load and preprocess news articles
load_news_data <- function(file_path) {
    # Read the CSV file
    news_data <- read_csv(file_path)

    # Basic preprocessing
    news_data <- news_data %>%
        # Convert date column to proper date format (adjust column name as needed)
        mutate(date = as.Date(date)) %>%
        # Remove any duplicate articles
        distinct() %>%
        # Remove any rows with missing values in key columns
        drop_na(article, date)

    return(news_data)
}

# Function to preprocess text data
preprocess_text <- function(data) {
    # Convert to data.table for better performance
    dt <- as.data.table(data)

    # Clean and tokenize text
    processed_data <- dt[,
        {
            # Convert to lowercase and remove special characters
            clean_text <- tolower(article)
            clean_text <- gsub("[^a-z\\s]", " ", clean_text)

            # Tokenize into words
            words <- unlist(strsplit(clean_text, "\\s+"))

            # Remove empty strings and numbers
            words <- words[words != ""]
            words <- words[!grepl("^[0-9]+$", words)]

            # Return as a list
            list(
                title = title,
                publication = publication,
                article_date = article_date,
                words = list(words)
            )
        },
        by = .(author, title, publication, article_date)
    ]

    return(processed_data)
}

# Main function to process data
main <- function() {
    # Read raw data
    raw_data <- read_csv("data/raw/news_sample_1000.csv",
        col_types = cols(
            year = col_integer(),
            month = col_double(), # Read as double to handle decimal months
            day = col_integer(),
            author = col_character(),
            title = col_character(),
            article = col_character(),
            publication = col_character()
        )
    )

    # Print initial data structure
    cat("Initial data structure:\n")
    str(raw_data)

    # Print diagnostics for month and day columns
    cat("\nUnique values in month column:\n")
    print(unique(raw_data$month))
    cat("\nUnique values in day column:\n")
    print(unique(raw_data$day))
    cat("\nNumber of NAs in month:", sum(is.na(raw_data$month)), "\n")
    cat("Number of NAs in day:", sum(is.na(raw_data$day)), "\n")
    cat("\nRows with NA in month or day:\n")
    print(head(raw_data[is.na(raw_data$month) | is.na(raw_data$day), ]))

    # Convert month and day to integer in case they are decimal
    raw_data <- raw_data %>%
        mutate(
            month = as.integer(month),
            day = as.integer(day),
            date_str = sprintf("%04d-%02d-%02d", year, month, day)
        )
    cat("\nFirst 10 constructed date strings:\n")
    print(head(raw_data$date_str, 10))
    # Now convert to Date
    raw_data <- raw_data %>%
        mutate(article_date = base::as.Date(date_str))
    # Remove the original date column to avoid confusion
    raw_data <- raw_data %>% select(-date)
    cat("\nAfter creating article_date and removing date column:\n")
    print(colnames(raw_data))
    # Convert article_date to POSIXct for lubridate compatibility
    raw_data <- raw_data %>%
        mutate(article_date = as.POSIXct(article_date))
    cat("\nAfter converting article_date to POSIXct:\n")
    print(colnames(raw_data))
    # Drop rows with NA article_date
    raw_data <- dplyr::filter(raw_data, !is.na(.data$article_date))
    cat("\nAfter filtering NAs in article_date:\n")
    print(colnames(raw_data))
    # Filter for 2016 data using dplyr::filter and lubridate::year explicitly
    raw_data <- dplyr::filter(raw_data, lubridate::year(.data$article_date) == 2016)
    cat("\nAfter filtering for 2016:\n")
    print(colnames(raw_data))

    # Print data after date processing
    cat("\nData after date processing:\n")
    str(raw_data)
    cat("\nColumn names in raw_data before preprocessing:\n")
    print(colnames(raw_data))

    # Preprocess text
    processed_data <- preprocess_text(raw_data)

    # Create processed directory if it doesn't exist
    dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

    # Save processed data as RDS file
    saveRDS(processed_data, "data/processed/processed_articles_2016.rds")

    # Print summary
    cat("\nProcessing complete!\n")
    cat("Number of articles processed:", nrow(processed_data), "\n")
    cat("Average words per article:", mean(sapply(processed_data$words, length)), "\n")

    # Print example of first article's words
    cat("\nExample of first article's words (first 10):\n")
    print(head(processed_data$words[[1]], 10))
}

# Run main function
main()
