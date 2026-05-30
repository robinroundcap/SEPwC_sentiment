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
  # 1. Read the raw CSV file
  # stringsAsFactors = FALSE ensures text columns stay as characters
  df <- read.csv(filename, stringsAsFactors = FALSE)
  
  # 2. Filter out non-English toots
  # The test expects ALL remaining rows to have language == "en"
  df <- df[df$language == "en" & !is.na(df$language), ]
  
  # 3. Clean HTML tags from the content column
  # Uses the exact regex pattern provided in the assignment description
  df$content <- gsub("<[^>]+>", "", df$content)
  
  # 4. Parse the created_at column into a formal datetime object
  # Mastodon dates usually look like "2024-02-22T14:30:00.000Z"
  # ymd_hms() automatically converts this string and satisfies is.timepoint()
  df$created_at <- lubridate::ymd_hms(df$created_at)
  
  # 5. Explicitly force the id column to be a character class
  # This avoids R converting long IDs into scientific notation (which corrupts them)
  df$id <- as.character(df$id)
  
  return(df)
}

word_analysis<-function(toot_data, emotion) {
  # 1. Get the NRC dictionary and filter for the specific emotion requested
  emotion_dict <- get_sentiments("nrc") %>%
  filter(sentiment == emotion)
  
  # 2. Process the text data
  word_data <- toot_data %>%
  # Select only the columns we need to track
  select(id, created_at, content) %>%
    
  # Break the sentences down into individual words
  unnest_tokens(word, content) %>%
  
  # Match the words against our emotion dictionary (drops non-matching words)
  inner_join(emotion_dict, by = "word") %>%
    
  # Count the occurrences while keeping id, created_at, and sentiment columns
  # sort = TRUE automatically sorts the 'n' column in decreasing order
  count(id, created_at, sentiment, word, sort = TRUE) %>%
  
  # Keep only the top 10 rows
  head(10)
    return()
  
}

sentiment_analysis<-function(toot_data) {

    return()

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
