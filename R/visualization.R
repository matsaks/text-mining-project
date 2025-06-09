# Visualization Script for News Articles Sentiment Analysis
# Author: [Your Name]
# Date: [Current Date]

# Load required packages
library(tidyverse)
library(ggplot2)
library(lubridate)
library(scales)
library(zoo)
library(dplyr)

# Function to create sentiment timeline plot for a specific time window
create_sentiment_timeline <- function(sentiment_data, significant_events, window_days, window_name) {
    # Add rolling mean for the specific window
    sentiment_data <- sentiment_data %>%
        arrange(article_date) %>%
        mutate(
            rolling_mean = zoo::rollmean(sentiment_score, k = window_days, fill = NA)
        )

    # Create the timeline plot base
    timeline_plot <- ggplot() +
        # Add the sentiment line
        geom_line(data = sentiment_data, aes(x = article_date, y = sentiment_score), color = "steelblue", linewidth = 1) +
        # Add rolling mean
        geom_line(data = sentiment_data, aes(x = article_date, y = rolling_mean), color = "darkred", linetype = "dashed", alpha = 0.7) +
        # Add significant events as points
        geom_point(data = significant_events, aes(x = article_date, y = sentiment_score), color = "red", size = 3)
    timeline_plot <- timeline_plot +
        # Customize the theme
        theme_minimal() +
        theme(
            plot.title = element_text(size = 16, face = "bold"),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10)
        ) +
        # Add labels
        labs(
            title = paste("Sentiment Timeline with", window_name, "Trend (2016)"),
            x = "Date",
            y = "Sentiment Score",
            caption = paste("Red dashed line shows", window_name, "rolling mean")
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

# Function to create publication sentiment plot
create_publication_sentiment_plot <- function(publication_sentiment) {
    # Create the publication sentiment plot
    publication_plot <- ggplot(publication_sentiment, 
                             aes(x = reorder(publication, sentiment_score), 
                                 y = sentiment_score)) +
        geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
        # Add sentiment score labels on top of bars
        geom_text(aes(label = round(sentiment_score, 3)), 
                 vjust = -0.5, size = 3) +
        # Add article count labels inside bars
        geom_text(aes(label = paste0("n=", article_count)), 
                 vjust = 1.5, size = 3, color = "white") +
        theme_minimal() +
        theme(
            plot.title = element_text(size = 16, face = "bold"),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10),
            axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        labs(
            title = "Average Sentiment Score by Publication (2016)",
            x = "Publication",
            y = "Average Sentiment Score",
            caption = "n = number of articles analyzed"
        ) +
        # Add a horizontal line at y=0
        geom_hline(yintercept = 0, linetype = "dotted", color = "gray50")

    return(publication_plot)
}

# Function to create individual topic frequency plot
create_topic_frequency_plot <- function(topic_freq_data, topic_name) {
    # Debug: print column names and structure before filtering
    cat("\n[DEBUG] topic_freq_data column names before filtering:\n")
    print(colnames(topic_freq_data))
    cat("[DEBUG] topic_freq_data structure before filtering:\n")
    print(str(topic_freq_data))
    # Filter data for specific topic
    topic_data <- topic_freq_data[topic_freq_data$topic == topic_name, ]
    # Debug: print structure after filtering
    cat("[DEBUG] topic_data structure after filtering:\n")
    print(str(topic_data))
    
    # Create the topic frequency plot
    topic_plot <- ggplot(topic_data, 
                        aes(x = article_date, y = normalized_freq)) +
        geom_line(linewidth = 1, color = "steelblue") +
        # Add a smoother
        geom_smooth(se = TRUE, alpha = 0.2, color = "darkred") +
        theme_minimal() +
        theme(
            plot.title = element_text(size = 16, face = "bold"),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10)
        ) +
        labs(
            title = paste("Frequency of '", topic_name, "' Over Time (2016)", sep = ""),
            x = "Date",
            y = "Frequency (per 1000 words)"
        ) +
        scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")

    return(topic_plot)
}

# Function to create all topic frequency plots
create_all_topic_plots <- function(topic_freq_data) {
    # Ensure data is properly structured
    if (!"topic" %in% colnames(topic_freq_data)) {
        stop("Column 'topic' not found in topic frequency data")
    }
    
    # Get unique topics
    topics <- unique(topic_freq_data$topic)
    
    # Create a list to store plots
    plots <- list()
    
    # Create a plot for each topic
    for (topic in topics) {
        plots[[as.character(topic)]] <- create_topic_frequency_plot(topic_freq_data, as.character(topic))
    }
    
    return(plots)
}

# Function to create publication topic frequency heatmap
create_publication_topic_heatmap <- function(publication_topic_freq) {
    # Create the heatmap
    heatmap <- ggplot(publication_topic_freq, 
                     aes(x = publication, y = topic, fill = normalized_freq)) +
        geom_tile() +
        theme_minimal() +
        theme(
            plot.title = element_text(size = 16, face = "bold"),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10),
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.title = element_text(size = 10),
            legend.text = element_text(size = 8)
        ) +
        labs(
            title = "Topic Frequency by Publication (2016)",
            x = "Publication",
            y = "Topic",
            fill = "Frequency\n(per 1000 words)"
        ) +
        # Use a colorblind-friendly palette
        scale_fill_viridis_c(option = "plasma") +
        # Add frequency values to the tiles
        geom_text(data = publication_topic_freq, aes(label = round(normalized_freq, 1)), size = 2.5, color = "white")

    return(heatmap)
}

# Main visualization pipeline
main <- function() {
    # Load sentiment data
    sentiment_data <- read_csv("output/results/sentiment_scores.csv") %>%
        mutate(article_date = as.Date(article_date))

    significant_events <- read_csv("output/results/significant_events.csv") %>%
        mutate(article_date = as.Date(article_date))
    
    # Load publication sentiment data
    publication_sentiment <- read_csv("output/results/publication_sentiment.csv")

    # Load topic frequency data
    daily_topic_freq <- read_csv("output/results/daily_topic_frequency.csv") %>%
        mutate(article_date = as.Date(article_date))
    # Rename 'keyword' column to 'topic'
    colnames(daily_topic_freq)[colnames(daily_topic_freq) == "keyword"] <- "topic"
    daily_topic_freq$topic <- as.factor(daily_topic_freq$topic)
    
    # Print data structure for debugging
    cat("\nDaily topic frequency data structure:\n")
    print(str(daily_topic_freq))
    
    publication_topic_freq <- read_csv("output/results/publication_topic_frequency.csv")

    # Convert to data.frame to avoid tibble scoping issues
    publication_topic_freq <- as.data.frame(publication_topic_freq)
    # Rename 'keyword' column to 'topic'
    colnames(publication_topic_freq)[colnames(publication_topic_freq) == "keyword"] <- "topic"
    publication_topic_freq$topic <- as.factor(publication_topic_freq$topic)

    # Debug: print column names for publication_topic_freq
    cat("\nPublication topic frequency column names:\n")
    print(colnames(publication_topic_freq))
    if (!"topic" %in% colnames(publication_topic_freq)) {
        stop("Column 'topic' not found in publication_topic_freq")
    }

    # Create plots for different time windows
    medium_term_plot <- create_sentiment_timeline(sentiment_data, significant_events, 30, "30-Day")
    
    # Create other plots
    count_plot <- create_article_count_plot(sentiment_data)
    publication_plot <- create_publication_sentiment_plot(publication_sentiment)
    
    # Create topic frequency plots
    topic_plots <- create_all_topic_plots(daily_topic_freq)
    topic_heatmap <- create_publication_topic_heatmap(publication_topic_freq)

    # Save plots
    ggsave("output/figures/sentiment_timeline_30day.png", medium_term_plot,
        width = 12, height = 6, dpi = 300
    )
    ggsave("output/figures/article_count.png", count_plot,
        width = 12, height = 6, dpi = 300
    )
    ggsave("output/figures/publication_sentiment.png", publication_plot,
        width = 12, height = 8, dpi = 300
    )
    
    # Save individual topic plots
    for (topic in names(topic_plots)) {
        filename <- paste0("output/figures/topic_", tolower(gsub(" ", "_", topic)), ".png")
        ggsave(filename, topic_plots[[topic]],
            width = 12, height = 6, dpi = 300
        )
    }
    
    ggsave("output/figures/topic_heatmap.png", topic_heatmap,
        width = 12, height = 8, dpi = 300
    )

    # Print confirmation
    cat("Visualizations created and saved to output/figures/\n")
}

# Run the main function
main()
