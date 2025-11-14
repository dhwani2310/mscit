##############################################
# SENTIMENT & TOPIC ANALYSIS - SAMSUNG TWEETS
# Single R Script (End-to-End)
##############################################

# Install packages (run once)
# install.packages(c("tidyverse","tidytext","textdata","wordcloud","topicmodels","syuzhet"))

library(tidyverse)
install.packages("tidytext")
library(tidytext)
install.packages("wordcloud")
library(wordcloud)
install.packages("topicmodels")
library(topicmodels)
install.packages("syuzhet")
library(syuzhet) 
install.packages("tm")   # run once
# For sentiment
library(tm)
library(ggplot2)

##############################################
# 1. LOAD DATA
##############################################
setwd("D:/projects/sentiment analysis")
df <- read.csv("samsung_tweets.csv")   # Make sure file is in working directory
df <- df %>% filter(!is.na(text))

##############################################
# 2. CLEAN TEXT
##############################################

clean_text <- function(t) {
  t <- gsub("http\\S+|www\\S+", "", t)  # Remove URLs
  t <- gsub("@\\w+", "", t)            # Remove mentions
  t <- gsub("#", "", t)                # Remove hashtags
  t <- gsub("[^A-Za-z\\s]", "", t)     # Remove symbols
  t <- tolower(t)
  t <- removeWords(t, stopwords("en"))
  return(t)
}

df$clean_text <- sapply(df$text, clean_text)

##############################################
# 3. SENTIMENT (Syuzhet + NRC Lexicon)
##############################################

sentiment_scores <- get_nrc_sentiment(df$clean_text)

df$sentiment <- ifelse(sentiment_scores$positive > sentiment_scores$negative,
                       "Positive",
                       ifelse(sentiment_scores$negative > sentiment_scores$positive,
                              "Negative", "Neutral"))

##############################################
# 4. SENTIMENT PLOT
##############################################

ggplot(df, aes(x = sentiment)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  ggtitle("Sentiment Distribution for Samsung Tweets")

ggsave("sentiment_distribution.png", width = 7, height = 5)

##############################################
# 5. WORDCLOUDS
##############################################

positive_words <- paste(df$clean_text[df$sentiment == "Positive"], collapse=" ")
negative_words <- paste(df$clean_text[df$sentiment == "Negative"], collapse=" ")

png("wordcloud_positive.png", width=800, height=600)
wordcloud(positive_words, max.words=100)
dev.off()

png("wordcloud_negative.png", width=800, height=600)
wordcloud(negative_words, max.words=100)
dev.off()

##############################################
# 6. TOPIC MODELING (LDA)
##############################################

# Convert to corpus
corpus <- Corpus(VectorSource(df$clean_text))
dtm <- DocumentTermMatrix(corpus,
                          control=list(wordLengths=c(3, Inf)))

# Remove sparse terms
dtm_sparse <- removeSparseTerms(dtm, 0.99)

# Fit LDA model (5 topics)
lda_model <- LDA(dtm_sparse, k = 5, control = list(seed = 123))

topics <- tidy(lda_model, matrix = "beta")

##############################################
# 7. PRINT TOP WORDS PER TOPIC
##############################################

top_terms <- topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  arrange(topic, -beta)

print(top_terms)

##############################################
# 8. TOPIC VISUALIZATION
##############################################

ggplot(top_terms, aes(reorder_within(term, beta, topic), beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  theme_minimal() +
  ggtitle("Top Terms in Each Topic")

ggsave("topics_visualization.png", width = 10, height = 7)

##############################################
# END OF SCRIPT
##############################################

