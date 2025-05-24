# Visualization Script for News Articles Sentiment Analysis
# Author: [Your Name]
# Date: [Current Date]

# Load required packages
library(tidyverse)
library(ggplot2)
library(lubridate)
library(scales)
library(zoo)

# Function to create sentiment timeline plot
create_sentiment_timeline <- function(sentiment_data, significant_events) {
    # Add rolling mean and sd to sentiment_data
    sentiment_data <- sentiment_data %>%
        arrange(article_date) %>%
        mutate(
            rolling_mean = zoo::rollmean(sentiment_score, k = 7, fill = NA),
            rolling_sd = zoo::rollapply(sentiment_score, width = 7, FUN = sd, fill = NA)
        )

    # Create the main timeline plot
    timeline_plot <- ggplot(sentiment_data, aes(x = article_date, y = sentiment_score)) +
        # Add the sentiment line
        geom_line(color = "steelblue", linewidth = 1) +
        # Add rolling mean
        geom_line(aes(y = rolling_mean), color = "darkred", linetype = "dashed") +
        # Add significant events as points
        geom_point(
            data = significant_events,
            aes(x = article_date, y = sentiment_score),
            color = "red", size = 3
        ) +
        # Customize the theme
        theme_minimal() +
        theme(
            plot.title = element_text(size = 16, face = "bold"),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10)
        ) +
        # Add labels
        labs(
            title = "Sentiment Timeline of News Articles (2016)",
            x = "Date",
            y = "Sentiment Score",
            caption = "Red points indicate significant events"
        ) +
        # Format x-axis dates
        scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
        # Add a horizontal line at y=0
        geom_hline(yintercept = 0, linetype = "dotted", color = "gray50")

    return(timeline_plot)
}

# Function to create article count plot
create_article_count_plot <- function(sentiment_data) {
    # Create the article count plot
    count_plot <- ggplot(sentiment_data, aes(x = article_date, y = article_count)) +
        geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
        theme_minimal() +
        theme(
            plot.title = element_text(size = 16, face = "bold"),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10)
        ) +
        labs(
            title = "Number of Articles per Day (2016)",
            x = "Date",
            y = "Number of Articles"
        ) +
        scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")

    return(count_plot)
}

# Main visualization pipeline
main <- function() {
    # Load sentiment data
    sentiment_data <- read_csv("output/results/sentiment_scores.csv") %>%
        mutate(article_date = as.Date(article_date))

    significant_events <- read_csv("output/results/significant_events.csv") %>%
        mutate(article_date = as.Date(article_date))

    # Create plots
    timeline_plot <- create_sentiment_timeline(sentiment_data, significant_events)
    count_plot <- create_article_count_plot(sentiment_data)

    # Save plots
    ggsave("output/figures/sentiment_timeline.png", timeline_plot,
        width = 12, height = 6, dpi = 300
    )
    ggsave("output/figures/article_count.png", count_plot,
        width = 12, height = 6, dpi = 300
    )

    # Print confirmation
    cat("Visualizations created and saved to output/figures/\n")
}

# Run the main function
main()
