suppressPackageStartupMessages({
  library(sentimentr)
  library(tidytext)
  library(lubridate)
  library(dplyr)
  library(tidyr)
  library(argparse)
  library(ggpubr)
  library(ggplot2)
})


load_data <- function(filename) {
  # Force R to read the 'id' column as a character string right away, 
  # preventing any accidental conversions to scientific notation
  df <- read.csv(filename, colClasses = c("id" = "character"), stringsAsFactors = FALSE)
  
  # Filter out non-English rows
  df <- df[df$language == "en" & !is.na(df$language), ]
  
  # Strip HTML tags
  df$content <- gsub("<[^>]+>", "", df$content)
  
  # Parse datetime objects using lubridate
  df$created_at <- lubridate::ymd_hms(df$created_at)
  
  return(df)
}

word_analysis<-function(toot_data, emotion) {
  target_emotion <- as.character(emotion)
  
  emotion_dict <- get_sentiments("nrc") %>%
    filter(sentiment == target_emotion)
  
  word_data <- toot_data %>%
    select(id, created_at, content) %>%
    unnest_tokens(word, content) %>%
    inner_join(emotion_dict, by = "word") %>%
    count(id, created_at, sentiment, word, sort = TRUE) %>%
    head(10)
  
  return(word_data)
  
}

sentiment_analysis <- function(toot_data) {
  # unnest_tokens automatically forces lowercase and strips punctuation
  toot_words <- toot_data %>%
    select(id, created_at, content) %>%
    unnest_tokens(word, content)
  
  # BING Lexicon Analysis
  bing_df <- toot_words %>%
    inner_join(get_sentiments("bing"), by = "word", relationship = "many-to-many") %>%
    # Converts binary positive/negative text strings into numbers (+1 / -1)
    mutate(score = if_else(sentiment == "positive", 1, -1)) %>%
    group_by(id, created_at) %>%
    summarise(sentiment = sum(score), .groups = "drop") %>%
    # Tracking column lets us separate methods later on the plot axis
    mutate(method = "bing")
  
  # AFINN Lexicon Analysis
  afinn_df <- toot_words %>%
    inner_join(get_sentiments("afinn"), by = "word", relationship = "many-to-many") %>%
    group_by(id, created_at) %>%
    # Sums up pre-existing numeric weights natively stored in the 'value' column
    summarise(sentiment = sum(value), .groups = "drop") %>%
    mutate(method = "afinn")
  
  # NRC Lexicon Analysis
  nrc_df <- toot_words %>%
    inner_join(get_sentiments("nrc"), by = "word", relationship = "many-to-many") %>%
    # Filters out specific emotional tracking to isolate net sentiment coordinates
    filter(sentiment %in% c("positive", "negative")) %>%
    mutate(score = if_else(sentiment == "positive", 1, -1)) %>%
    group_by(id, created_at) %>%
    summarise(sentiment = sum(score), .groups = "drop") %>%
    mutate(method = "nrc")
  
  # Stacks data frames vertically into long-format matching ggplot2 grouping
  combined_sentiment <- bind_rows(afinn_df, nrc_df, bing_df)
  
  return(combined_sentiment)
}

main <- function(args) {

}


if(sys.nframe() == 0) {

  # main program, called via Rscript
  parser = ArgumentParser(
                    prog="Sentiment Analysis",
                    description="Analyse toots for word and sentence sentiments"
                    )
  parser$add_argument("filename",
                    help="the file to read the toots from")
  parser$add_argument("--emotion",
                      default="anger",
                      help="which emotion to search for")
  parser$add_argument('-v', '--verbose',
                    action='store_true',
                    help="Print progress")
  parser$add_argument('-p', '--plot',
                    help="Plot something. Give the filename")
  
  args = parser$parse_args()  
  main(args)
}

# Copyright 2026 by Robin Tattersall. GPL-3