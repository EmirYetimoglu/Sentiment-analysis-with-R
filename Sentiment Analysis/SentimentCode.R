####Library Packages####
# package xml2 to remove HTML
library(xml2)

# spell correction package 
library(stringr)
library(tm)

library(dplyr)
library(ggplot2)
library(tidytext)

# Word Cloud Packages
library(RColorBrewer)
library(wordcloud)
library(cowplot)
library(magick)
library(wordcloud2)

####Data Cleaning Functions####

# URL removal function definition
removeURL <- function(data){
  data[] <- lapply(data, function(x) gsub("http[^[:space:]]*", "", x))
  return(data)
}

# Mention removal function definition
removeMention <- function(data){
  data[] <- lapply(data, function(x) gsub("@\\w+", "", x))
  return(data)
}

# Hashtag removal function definition
removeHashtag <- function(data){
  data[] <- lapply(data, function(x) gsub("#\\S+", "", x))
  return(data)
}

# Emoticon removal from text area only
removeEmoticon <- function(data){
  data$text <- gsub("[\U0001F600-\U0001F64F\U0001F300-\U0001F5FF\U0001F680-\U0001F6FF\U0001F700-\U0001F773\U0001F780-\U0001F7FF\U0001F800-\U0001F8FF\U0001F900-\U0001F9FF\U0001FA00-\U0001FA6F\U0001FA70-\U0001FAFF\U00002600-\U000027BF\U0001F1E0-\U0001F1FF\U0001F191-\U0001F251\U0001F004\U0001F0CF\U0001F170-\U0001F171\U0001F17E-\U0001F17F\U0001F18E\U0001F191-\U0001F19A\U0001F1E6-\U0001F1FF\U0001F201-\U0001F202\U0001F232-\U0001F23A\U0001F250-\U0001F251\U0001F300-\U0001F320\U0001F330-\U0001F335\U0001F337-\U0001F37C\U0001F380-\U0001F393\U0001F3A0-\U0001F3CA\U0001F3CF-\U0001F3D3\U0001F3E0-\U0001F3F0\U0001F400-\U0001F43E\U0001F440\U0001F442-\U0001F4F7\U0001F4F9-\U0001F4FC\U0001F500-\U0001F53D\U0001F550-\U0001F567\U0001F5FB-\U0001F640\U0001F645-\U0001F64F\U0001F680-\U0001F6C5\U0001F700-\U0001F773\U0001F780-\U0001F7D8\U0001F800-\U0001F80B\U0001F810-\U0001F847\U0001F850-\U0001F859\U0001F860-\U0001F887\U0001F890-\U0001F8AD\U0001F900-\U0001F90B\U0001F90D-\U0001F971\U0001F973-\U0001F976\U0001F97A\U0001F97C-\U0001F9A2\U0001F9B0-\U0001F9B9\U0001F9C0-\U0001F9C2\U0001F9D0-\U0001F9FF\U0001FA60-\U0001FA6D\U0001FA70-\U0001FA73\U0001FA78-\U0001FA7A\U0001FA80-\U0001FA82\U0001FA90-\U0001FA95\U00020000-\U0002FFFD\U000E0001-\U000E007F\U000FE000-\U000FE0FF\U00010100-\U0010FFFF]", "", data$text, perl=TRUE)
  return(data)
}

# Retweet removal
# Added ignore.case = TRUE to make the pattern case-insensitive
removeRT <- function(data){
  gsub("(rt|via)((?:\\b\\W*@\\w+)+)", "", data, ignore.case = TRUE)
  return(data) # Add return statement to return the modified data
}

# HTML tags removal
removeHTML <- function(data) {
  data$text <- gsub("<.*?>", "", data$text)
  return(data)
}

# Punctuation marks removal
removePunctuation <- function(data) {
  data$text <- gsub("[[:punct:]]", "", data$text)
  return(data)
}

# number removal function
removeNumbers <- function(data) {
  data$text <- gsub("\\d+", "", data$text)
  return(data)
}

# Function to convert text to lowercase
convertToLower <- function(data) {
  data$text <- tolower(data$text)
  return(data)
}

# Function to remove white space from data
removeWhiteSpace <- function(data){
  data[] <- lapply(data, trimws)
  return(data)
}

# Spell Normalization Function
spell_correction <- function(data, dict) {
  for (i in seq_along(data$text)) {
    sentence <- unlist(str_split(data$text[i], "\\s+"))
    corrected_sentence <- c()
    for (word in sentence) {
      if (word %in% dict$slang) {
        replacement <- dict$formal[which(dict$slang == word)]
        if (length(replacement) > 0) {
          corrected_sentence <- c(corrected_sentence, replacement)
          next
        }
      }
      corrected_sentence <- c(corrected_sentence, word)
    }
    data$text[i] <- paste(corrected_sentence, collapse = " ")
  }
  return(data)
}

# dictionary creation
slang <- c("lol", "u", "btw", "omg", "brb", "afaik", "tbh", "imho")
formal <- c("laughing out loud", "you", "by the way", "oh my god", "be right back", "as far as I know", "to be honest", "in my humble opinion")

# Creating the dictionary data frame
dict <- data.frame(slang, formal)

# If "id", "text", and "date" columns are same at the same time, removes the row
remove_duplicate_rows <- function(data) {
  # Identify duplicate rows based on "id", "text", and "date" columns
  duplicated_rows <- duplicated(data[, c("id", "text", "date")]) | duplicated(data[, c("id", "text", "date")], fromLast = TRUE)
  
  # Remove duplicate rows
  data <- data[!duplicated_rows, ]
  
  return(data)
}



# Call the functions on data frame
turkey_earthquake_tweets <- removeMention(turkey_earthquake_tweets)
turkey_earthquake_tweets <- removeURL(turkey_earthquake_tweets)
turkey_earthquake_tweets <- removeHashtag(turkey_earthquake_tweets)
turkey_earthquake_tweets <- removeRT(turkey_earthquake_tweets)
turkey_earthquake_tweets <- removeEmoticon(turkey_earthquake_tweets)
turkey_earthquake_tweets <- removeHTML(turkey_earthquake_tweets)
turkey_earthquake_tweets <- removePunctuation(turkey_earthquake_tweets)
turkey_earthquake_tweets <- removeNumbers(turkey_earthquake_tweets)
turkey_earthquake_tweets <- convertToLower(turkey_earthquake_tweets)
turkey_earthquake_tweets <- removeWhiteSpace(turkey_earthquake_tweets)
turkey_earthquake_tweets <- spell_correction(turkey_earthquake_tweets, dict)
turkey_earthquake_tweets <- remove_duplicate_rows(turkey_earthquake_tweets)

# if "text" is empty removes the row
turkey_earthquake_tweets <- subset(turkey_earthquake_tweets, !is.na(text) & text != "")

# View only id and text columns of data
View(subset(turkey_earthquake_tweets, select = c(id, text)))

#Total data count
total_data_count <- 28846

#  Cleaned data count after all cleaning steps
cleaned_data_count <- 22178

# Create a bar plot
barplot(c(total_data_count, cleaned_data_count), 
        names.arg = c("Total Data", "Cleaned Data"),
        main = "Total Data vs Cleaned Data",
        ylab = "Number of Rows",
        col = c("darkblue", "darkgreen"),
        ylim = c(0, max(total_data_count, cleaned_data_count) * 1.1))
text(1:2, c(total_data_count, cleaned_data_count) + 20, c(total_data_count, cleaned_data_count), pos = 3, col = "black")


####Sentiment Analysis####

# Tokenize the text and convert to a tidy format
tidy_tweets <- turkey_earthquake_tweets %>%
  select(text) %>%
  unnest_tokens(word, text)

# Remove stop words
data(stop_words)
tidy_tweets <- tidy_tweets %>%
  anti_join(stop_words)

# Calculate word frequencies
word_counts <- tidy_tweets %>%
  count(word, sort = TRUE)

# Select the top 10 most used words
top_words <- word_counts %>%
  top_n(10, n)

# Create the plot
ggplot(top_words, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Most Used Words",
       x = "Word",
       y = "Frequency") +
  theme_minimal()


# Count the number of unique words
unique_word_count <- tidy_tweets %>%
  distinct(word) %>%
  nrow()

# Print the count of unique words
print(unique_word_count)

# Perform sentiment analysis using the Bing lexicon
sentiment_tweets <- tidy_tweets %>%
  inner_join(get_sentiments("bing"))

# Calculate sentiment frequency
sentiment_freq <- sentiment_tweets %>%
  count(sentiment, sort = TRUE)

# Print the sentiment frequency
print(sentiment_freq)


# Calculate the total number of words
total_words <- nrow(tidy_tweets)

# Calculate the total number of positive and negative words and their percentages
total_sentiment <- sentiment_freq %>%
  mutate(percentage = (n / total_words) * 100)

# Ensure that the "positive" and "negative" sentiments are present in the data
if (!"positive" %in% total_sentiment$sentiment) {
  total_sentiment <- rbind(total_sentiment, data.frame(sentiment = "positive", n = 0, percentage = 0))
}
if (!"negative" %in% total_sentiment$sentiment) {
  total_sentiment <- rbind(total_sentiment, data.frame(sentiment = "negative", n = 0, percentage = 0))
}

# Calculate the ratios
total_sentiment <- total_sentiment %>%
  mutate(ratio = n / total_words * 100)

# Create the plot
ggplot(total_sentiment, aes(x = sentiment, y = ratio, fill = sentiment)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label = paste0(round(ratio, 2), "%")), vjust = -0.5, size = 5) +
  labs(y = "Percentage of Words",
       x = "Sentiment",
       title = "Percentage of Positive and Negative Words in Tweets",
       subtitle = "Using Bing Sentiment Lexicon") +
  theme_minimal() +
  ylim(0, max(total_sentiment$ratio) * 1.1)


# Calculate word frequencies for positive and negative sentiments
positive_words <- sentiment_tweets %>%
  filter(sentiment == "positive") %>%
  count(word, sort = TRUE) %>%
  top_n(10, n)

negative_words <- sentiment_tweets %>%
  filter(sentiment == "negative") %>%
  count(word, sort = TRUE) %>%
  top_n(10, n)

# Combine the top positive and negative words into one table
top_words <- bind_rows(
  positive_words %>% mutate(sentiment = "positive"),
  negative_words %>% mutate(sentiment = "negative")
)

# Plot the combined top words
ggplot(top_words, aes(x = reorder(word, n), y = n, fill = sentiment)) +
  geom_bar(stat = "identity", show.legend = TRUE) +
  coord_flip() +
  facet_wrap(~ sentiment, scales = "free_y") +
  labs(title = "Top 10 Most Used Positive and Negative Words",
       x = "Words",
       y = "Frequency") +
  theme_minimal() +
  scale_fill_manual(values = c("positive" = "darkgreen", "negative" = "darkred"))


#### Word Cloud Creation####


# Calculate word frequencies
words <- sentiment_tweets %>%
  count(word, sort = TRUE)

# Create a color palette
pal <- brewer.pal(8, "Dark2")

# Plot the word cloud
set.seed(1234)  # for reproducibility
wordcloud(words = words$word, 
          freq = words$n, 
          min.freq = 1, 
          max.words = 500,  
          random.order = FALSE, 
          rot.per = 0.35, 
          colors = pal)

# Calculate word frequencies and sentiment
word_counts <- sentiment_tweets %>%
  count(word, sentiment, sort = TRUE)

# Create a data frame for the word cloud
word_data <- word_counts %>%
  group_by(word) %>%
  summarize(freq = sum(n),
            sentiment = first(sentiment))

# Assign colors based on sentiment
word_data$color <- ifelse(word_data$sentiment == "positive", "green", "red")

# Create the word cloud
wordcloud2(data = word_data, 
           size = 0.5, 
           color = word_data$color)

