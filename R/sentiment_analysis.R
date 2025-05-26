# Sentiment Analysis Script for News Articles
# Author: [Your Name]
# Date: [Current Date]

# Load required packages
library(tidyverse)
library(tidytext)
library(lubridate)
library(data.table)

# Function to perform sentiment analysis
analyze_sentiment <- function(processed_data) {
    # Load AFINN lexicon
    afinn <- get_sentiments("afinn")

    # Convert to data.table for better performance
    dt <- as.data.table(processed_data)

    # Process each article's words and calculate sentiment
    sentiment_scores <- dt[,
        {
            # Unlist words and convert to data.table
            words_dt <- data.table(word = unlist(words))

            # Join with AFINN lexicon
            sentiment <- words_dt[afinn, on = "word", nomatch = 0]

            # Calculate average sentiment score for the article
            list(
                sentiment_score = mean(sentiment$value, na.rm = TRUE),
                word_count = nrow(words_dt)
            )
        },
        by = .(article_date, title, url)
    ]

    # Aggregate by date
    daily_sentiment <- sentiment_scores[,
        {
            list(
                sentiment_score = mean(sentiment_score, na.rm = TRUE),
                article_count = .N,
                total_words = sum(word_count)
            )
        },
        by = article_date
    ]

    return(daily_sentiment)
}

# Function to identify significant events
identify_significant_events <- function(sentiment_data) {
    # Calculate rolling mean and standard deviation
    sentiment_data <- sentiment_data %>%
        arrange(article_date) %>%
        mutate(
            rolling_mean = zoo::rollmean(sentiment_score, k = 7, fill = NA),
            rolling_sd = zoo::rollapply(sentiment_score, width = 7, FUN = sd, fill = NA)
        )

    # Identify peaks (events where sentiment deviates significantly)
    significant_events <- sentiment_data %>%
        dplyr::filter(
            abs(sentiment_score - rolling_mean) > 2 * rolling_sd
        )

    return(significant_events)
}

# Main analysis pipeline
main <- function() {
    # Load processed data
    processed_data <- readRDS("data/processed/processed_articles_2016.rds")

    # Print structure for debugging
    cat("Data structure:\n")
    print(str(processed_data))

    # Perform sentiment analysis
    sentiment_scores <- analyze_sentiment(processed_data)

    # Identify significant events
    significant_events <- identify_significant_events(sentiment_scores)

    # Save results as CSV
    write_csv(sentiment_scores, "output/results/sentiment_scores.csv")
    write_csv(significant_events, "output/results/significant_events.csv")

    # Print summary statistics
    cat("\nSentiment analysis complete.\n")
    cat("Number of days analyzed:", nrow(sentiment_scores), "\n")
    cat("Number of significant events identified:", nrow(significant_events), "\n")

    # Print example of sentiment scores
    cat("\nExample of sentiment scores (first 5 days):\n")
    print(head(sentiment_scores, 5))
}

# Run the main function
main()
