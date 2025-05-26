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
load_news_data <- function(df) {
    # Always coerce year, month, and day to integer
    df <- df %>%
        mutate(
            year = as.integer(year),
            month = as.integer(month),
            day = as.integer(day),
            date_str = sprintf("%04d-%02d-%02d", year, month, day),
            article_date = as.POSIXct(base::as.Date(date_str))
        ) %>%
        # Remove any duplicate articles
        distinct() %>%
        # Remove any rows with missing values in key columns
        drop_na(article, article_date)
    return(df)
}

# Function to preprocess text data
preprocess_text <- function(data) {
    # Convert to data.table for better performance
    dt <- as.data.table(data)

    # Clean and tokenize text
    processed_data <- dt[,
        {
            # Convert to lowercase and remove special characters
            clean_text <- tolower(as.character(article))
            # Replace multiple newlines with a single space
            clean_text <- gsub("\\n+", " ", clean_text)
            # Remove other special characters
            clean_text <- gsub("[^a-z\\s]", " ", clean_text)
            # Remove extra whitespace
            clean_text <- gsub("\\s+", " ", clean_text)
            clean_text <- trimws(clean_text)

            # Tokenize into words
            words <- unlist(strsplit(clean_text, "\\s+"))

            # Remove empty strings and numbers
            words <- words[words != ""]
            words <- words[!grepl("^[0-9]+$", words)]

            # Return as a list
            list(
                title = title,
                article_date = article_date,
                url = url,
                publication = publication,
                words = list(words)
            )
        },
        by = .(title, article_date, url, publication)
    ]

    return(processed_data)
}

# Main function to process data
main <- function() {
    # Create processed directory if it doesn't exist
    dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

    # Initialize output file
    output_file <- "data/processed/processed_articles_2016.rds"

    # Remove existing output file if it exists
    if (file.exists(output_file)) {
        file.remove(output_file)
    }

    # Process the file in chunks
    chunk_size <- 1000
    chunk_number <- 1
    input_file <- "data/raw/news_2016_sample_50000.csv" # Use consistent file name

    # Read and process chunks
    while (TRUE) {
        # Read chunk of data
        tryCatch(
            {
                raw_data <- read_csv(input_file,
                    skip = (chunk_number - 1) * chunk_size,
                    n_max = chunk_size,
                    col_names = chunk_number == 1, # Only use column names for first chunk
                    col_types = cols(
                        year = col_integer(),
                        month = col_integer(),
                        day = col_integer(),
                        title = col_character(),
                        article = col_character(),
                        url = col_character(),
                        publication = col_character()
                    )
                )

                # If we got no data, we're done
                if (nrow(raw_data) == 0) {
                    cat("Reached end of file.\n")
                    break
                }

                # If this isn't the first chunk, we need to set column names
                if (chunk_number > 1) {
                    colnames(raw_data) <- c("year", "month", "day", "title", "article", "url", "publication")
                }

                # Diagnostics: print column names and first few rows
                cat(sprintf("\nChunk %d: column names: %s\n", chunk_number, paste(colnames(raw_data), collapse = ", ")))
                print(head(raw_data, 2))

                # Check for NA or malformed date fields
                cat("NAs in year:", sum(is.na(raw_data$year)), ", month:", sum(is.na(raw_data$month)), ", day:", sum(is.na(raw_data$day)), "\n")
                cat("Unique years:", paste(unique(raw_data$year), collapse = ", "), "\n")
                cat("Unique months:", paste(unique(raw_data$month), collapse = ", "), "\n")
                cat("Unique days:", paste(unique(raw_data$day), collapse = ", "), "\n")

                # Process the chunk
                cat(sprintf("\nProcessing chunk %d...\n", chunk_number))

                # Ensure columns are present and not all NA before processing
                required_cols <- c("year", "month", "day", "title", "article", "url", "publication")
                missing_cols <- setdiff(required_cols, colnames(raw_data))
                if (length(missing_cols) > 0) {
                    stop(sprintf("Missing columns: %s", paste(missing_cols, collapse = ", ")))
                }
                if (all(is.na(raw_data$year)) || all(is.na(raw_data$month)) || all(is.na(raw_data$day))) {
                    stop("All date columns are NA in this chunk.")
                }

                # Clean and preprocess the chunk
                processed_chunk <- load_news_data(raw_data)

                # Diagnostic: print column names and first row after cleaning
                cat(sprintf("After load_news_data, chunk %d: column names: %s\n", chunk_number, paste(colnames(processed_chunk), collapse = ", ")))
                print(head(processed_chunk, 1))

                # Filter for 2016 data
                processed_chunk <- dplyr::filter(processed_chunk, year(article_date) == 2016)

                if (nrow(processed_chunk) > 0) {
                    # Process the text
                    processed_chunk <- preprocess_text(processed_chunk)

                    # Save chunk to RDS file
                    if (chunk_number == 1) {
                        saveRDS(processed_chunk, output_file)
                    } else {
                        # Read existing data
                        existing_data <- readRDS(output_file)
                        # Combine with new chunk
                        combined_data <- rbindlist(list(existing_data, processed_chunk))
                        # Save back to file
                        saveRDS(combined_data, output_file)
                    }

                    # Print progress
                    cat(sprintf("Processed %d articles in chunk %d\n", nrow(processed_chunk), chunk_number))
                } else {
                    cat(sprintf("No valid articles found in chunk %d\n", chunk_number))
                }
            },
            error = function(e) {
                cat(sprintf("Error processing chunk %d: %s\n", chunk_number, e$message))
            }
        )

        # Increment chunk counter
        chunk_number <- chunk_number + 1
    }

    # Print final summary
    if (file.exists(output_file)) {
        final_data <- readRDS(output_file)
        total_articles <- nrow(final_data)
        cat("\nProcessing complete!\n")
        cat("Total number of articles processed:", total_articles, "\n")
        if (total_articles > 0) {
            cat("Average words per article:", mean(sapply(final_data$words, length)), "\n")
            # Print example of first article's words
            cat("\nExample of first article's words (first 10):\n")
            print(head(final_data$words[[1]], 10))
        }
    } else {
        cat("\nProcessing complete!\n")
        cat("Total number of articles processed: 0\n")
    }
}

# Run main function
main()
