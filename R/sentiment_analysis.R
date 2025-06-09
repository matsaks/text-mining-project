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
    afinn <- readRDS("data/raw/afinn.rds")

    # Convert to data.table for better performance
    dt <- as.data.table(processed_data)

    # Process each article's words and calculate sentiment
    sentiment_scores <- dt[,
        {
            # Unlist words and convert to data.table
            words_dt <- data.table(word = unlist(words))

            # Join with AFINN lexicon
            sentiment <- words_dt[afinn, on = "word", nomatch = 0]

            # Calculate weighted sentiment score for the article
            # Weight by the number of sentiment-bearing words
            sentiment_words <- nrow(sentiment)
            if (sentiment_words > 0) {
                sentiment_score <- sum(sentiment$value) / sentiment_words
            } else {
                sentiment_score <- 0
            }

            list(
                sentiment_score = sentiment_score,
                word_count = nrow(words_dt),
                sentiment_word_count = sentiment_words
            )
        },
        by = .(article_date, title, url, publication)
    ]

    # Aggregate by date with word count normalization
    daily_sentiment <- sentiment_scores[,
        {
            # Calculate weighted average based on sentiment-bearing words
            total_sentiment_words <- sum(sentiment_word_count)
            if (total_sentiment_words > 0) {
                weighted_score <- sum(sentiment_score * sentiment_word_count) / total_sentiment_words
            } else {
                weighted_score <- 0
            }

            list(
                sentiment_score = weighted_score,
                article_count = .N,
                total_words = sum(word_count),
                total_sentiment_words = total_sentiment_words
            )
        },
        by = article_date
    ]

    # Aggregate by publication with word count normalization
    publication_sentiment <- sentiment_scores[,
        {
            # Calculate weighted average based on sentiment-bearing words
            total_sentiment_words <- sum(sentiment_word_count)
            if (total_sentiment_words > 0) {
                weighted_score <- sum(sentiment_score * sentiment_word_count) / total_sentiment_words
            } else {
                weighted_score <- 0
            }

            list(
                sentiment_score = weighted_score,
                article_count = .N,
                total_words = sum(word_count),
                total_sentiment_words = total_sentiment_words
            )
        },
        by = publication
    ]

    return(list(
        daily_sentiment = daily_sentiment,
        publication_sentiment = publication_sentiment
    ))
}

# Function to identify significant events
identify_significant_events <- function(sentiment_data) {
    # Calculate multiple rolling windows
    sentiment_data <- sentiment_data %>%
        arrange(article_date) %>%
        mutate(
            # Short-term (7 days) - for immediate spikes
            rolling_mean_7d = zoo::rollmean(sentiment_score, k = 7, fill = NA),
            rolling_sd_7d = zoo::rollapply(sentiment_score, width = 7, FUN = sd, fill = NA),
            
            # Medium-term (30 days) - for monthly trends
            rolling_mean_30d = zoo::rollmean(sentiment_score, k = 30, fill = NA),
            rolling_sd_30d = zoo::rollapply(sentiment_score, width = 30, FUN = sd, fill = NA),
            
            # Long-term (90 days) - for seasonal trends
            rolling_mean_90d = zoo::rollmean(sentiment_score, k = 90, fill = NA),
            rolling_sd_90d = zoo::rollapply(sentiment_score, width = 90, FUN = sd, fill = NA)
        )

    # Identify significant events using hierarchical criteria
    significant_events <- sentiment_data %>%
        # First, calculate deviations for each window
        mutate(
            dev_7d = abs(sentiment_score - rolling_mean_7d) / rolling_sd_7d,
            dev_30d = abs(sentiment_score - rolling_mean_30d) / rolling_sd_30d,
            dev_90d = abs(sentiment_score - rolling_mean_90d) / rolling_sd_90d
        ) %>%
        # Then apply hierarchical filtering
        dplyr::filter(
            # Short-term spikes (2.5 standard deviations)
            dev_7d > 2.5 |
            # Medium-term trends (2 standard deviations, but not a short-term spike)
            (dev_30d > 2.0 & dev_7d <= 2.5) |
            # Long-term trends (1.5 standard deviations, but not a medium-term trend)
            (dev_90d > 1.5 & dev_30d <= 2.0)
        ) %>%
        # Add a column indicating which criteria triggered the significance
        mutate(
            significance_type = case_when(
                dev_7d > 2.5 ~ "Short-term spike",
                dev_30d > 2.0 & dev_7d <= 2.5 ~ "Medium-term trend",
                dev_90d > 1.5 & dev_30d <= 2.0 ~ "Long-term trend"
            )
        ) %>%
        # Remove the temporary deviation columns
        select(-dev_7d, -dev_30d, -dev_90d)

    return(significant_events)
}

# Main analysis pipeline
main <- function() {
    # Load processed data
    processed_data <- readRDS("data/processed/new_processed_articles_2016.rds")

    # Print structure for debugging
    cat("Data structure:\n")
    print(str(processed_data))

    # Perform sentiment analysis
    sentiment_results <- analyze_sentiment(processed_data)
    sentiment_scores <- sentiment_results$daily_sentiment
    publication_sentiment <- sentiment_results$publication_sentiment

    # Identify significant events
    significant_events <- identify_significant_events(sentiment_scores)

    # Save results as CSV
    write_csv(sentiment_scores, "output/results/sentiment_scores.csv")
    write_csv(significant_events, "output/results/significant_events.csv")
    write_csv(publication_sentiment, "output/results/publication_sentiment.csv")
    
    # Print summary statistics
    cat("\nSentiment analysis complete.\n")
    cat("Number of days analyzed:", nrow(sentiment_scores), "\n")
    cat("Number of significant events identified:", nrow(significant_events), "\n")
    cat("Number of publications analyzed:", nrow(publication_sentiment), "\n")

    # Print example of sentiment scores
    cat("\nExample of sentiment scores (first 5 days):\n")
    print(head(sentiment_scores, 5))
    
    # Print publication sentiment scores
    cat("\nPublication sentiment scores:\n")
    print(publication_sentiment)
}

# Run the main function
main()
