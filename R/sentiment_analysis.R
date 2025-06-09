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
    # Load AFINN lexicon and convert to data.table
    afinn <- as.data.table(readRDS("data/raw/afinn.rds"))
    
    # Define negation words
    negation_words <- c("not", "no", "never", "neither", "nor", "none", "nothing", 
                       "nowhere", "hardly", "barely", "scarcely", "doesn't", "isn't", 
                       "wasn't", "shouldn't", "wouldn't", "couldn't", "won't", "can't", 
                       "don't", "didn't", "haven't", "hasn't", "hadn't")
    
    # Define intensifiers and their strength multipliers
    intensifiers <- data.table(
        word = c("very", "extremely", "incredibly", "absolutely", "totally", 
                "completely", "utterly", "really", "so", "too", "highly", 
                "particularly", "especially", "exceptionally", "remarkably",
                "immensely", "enormously", "tremendously", "exceedingly",
                "intensely", "profoundly", "deeply", "thoroughly", "entirely"),
        multiplier = c(1.5, 2.0, 2.0, 1.8, 1.7, 1.6, 1.9, 1.3, 1.2, 1.4, 1.5,
                      1.4, 1.4, 1.6, 1.5, 1.7, 1.7, 1.6, 1.5, 1.6, 1.5, 1.4,
                      1.3, 1.5)
    )

    # Convert to data.table for better performance
    dt <- as.data.table(processed_data)

    # Process each article's words and calculate sentiment
    sentiment_scores <- dt[,
        {
            # Create bigrams from words
            words_vec <- unlist(words)
            if (length(words_vec) < 2) {
                list(
                    sentiment_score = 0,
                    word_count = length(words_vec),
                    sentiment_word_count = as.integer(0)
                )
            } else {
                # Create bigrams
                bigrams <- data.table(
                    word1 = words_vec[1:(length(words_vec)-1)],
                    word2 = words_vec[2:length(words_vec)]
                )
                
                # Join with AFINN lexicon for both words
                bigrams <- merge(bigrams, afinn, by.x = "word1", by.y = "word", all.x = TRUE)
                setnames(bigrams, "value", "value1")
                bigrams <- merge(bigrams, afinn, by.x = "word2", by.y = "word", all.x = TRUE)
                setnames(bigrams, "value", "value2")
                
                # Join with intensifiers
                bigrams <- merge(bigrams, intensifiers, by.x = "word1", by.y = "word", all.x = TRUE)
                setnames(bigrams, "multiplier", "intensifier1")
                bigrams <- merge(bigrams, intensifiers, by.x = "word2", by.y = "word", all.x = TRUE)
                setnames(bigrams, "multiplier", "intensifier2")
                
                # Calculate sentiment scores considering negations and intensifiers
                bigrams[, sentiment := {
                    # Initialize sentiment vector
                    sentiment <- rep(0, .N)
                    
                    # Handle negations
                    neg_mask <- word1 %in% negation_words
                    sentiment[neg_mask & !is.na(value2)] <- -value2[neg_mask & !is.na(value2)]
                    
                    # Handle intensifiers
                    intens_mask <- !neg_mask & !is.na(intensifier1) & !is.na(value2)
                    sentiment[intens_mask] <- value2[intens_mask] * intensifier1[intens_mask]
                    
                    # Handle regular sentiment words
                    reg_mask <- !neg_mask & !intens_mask & !is.na(value2)
                    sentiment[reg_mask] <- value2[reg_mask]
                    
                    # Handle first word sentiment if second word has no sentiment
                    first_word_mask <- !neg_mask & !intens_mask & is.na(value2) & !is.na(value1)
                    sentiment[first_word_mask] <- value1[first_word_mask]
                    
                    sentiment
                }]
                
                # Count sentiment-bearing bigrams
                sentiment_bigrams <- bigrams[sentiment != 0]
                sentiment_words <- as.integer(nrow(sentiment_bigrams))
                
                # Calculate weighted sentiment score
                if (sentiment_words > 0) {
                    sentiment_score <- sum(sentiment_bigrams$sentiment) / sentiment_words
                } else {
                    sentiment_score <- 0
                }
                
                list(
                    sentiment_score = sentiment_score,
                    word_count = length(words_vec),
                    sentiment_word_count = sentiment_words
                )
            }
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
            # Medium-term (30 days) - for monthly trends
            rolling_mean_30d = zoo::rollmean(sentiment_score, k = 30, fill = NA),
            rolling_sd_30d = zoo::rollapply(sentiment_score, width = 30, FUN = sd, fill = NA)
        )

    # Identify significant events using hierarchical criteria
    significant_events <- sentiment_data %>%
        # First, calculate deviations for each window
        mutate(
            dev_30d = abs(sentiment_score - rolling_mean_30d) / rolling_sd_30d
        ) %>%
        # Then apply hierarchical filtering
        dplyr::filter(
            # Medium-term trends (2 standard deviations)
            dev_30d > 2.0
        ) %>%
        # Remove the temporary deviation columns
        select(-dev_30d)

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
