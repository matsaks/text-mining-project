# Sentiment Analysis of News Articles (2017)

This project analyzes the sentiment of news articles from 2017, creating a timeline visualization to identify patterns and correlations with significant events.

## Project Structure

```
.
├── data/               # Data directory
│   ├── raw/           # Original, immutable data
│   └── processed/     # Cleaned and processed data
├── R/                 # R scripts
│   ├── data_processing.R
│   ├── sentiment_analysis.R
│   ├── visualization.R
│   └── utils/         # Utility and setup scripts
│       ├── check_packages.R
│       └── download_afinn.R
├── output/            # Generated outputs
│   ├── figures/       # Plots and visualizations
│   └── results/       # Analysis results
├── docs/              # Documentation
└── .Rprofile          # R profile for project settings
```

## Setup

1. Install required R packages:

```R
install.packages(c("tidytext", "dplyr", "ggplot2", "lubridate", "stringr"))
```

2. Run the analysis scripts in the following order:
   - `R/utils/check_packages.R` (to ensure required packages are installed)
   - `R/utils/download_afinn.R` (to download the AFINN lexicon)
   - `R/data_processing.R`
   - `R/sentiment_analysis.R`
   - `R/visualization.R`

## Data

The project uses news articles from 2017. The data is stored in the `data/raw` directory.

## Analysis

The analysis includes:

- Text preprocessing
- Sentiment analysis using tidytext
- Timeline visualization of sentiment trends
- Correlation with significant events
