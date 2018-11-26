# Sentiment analysis
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
library(tm)


##READ CSV FILE
airlineTweets <- read.csv(file.choose(), header = T)
tweets <- iconv(airlineTweets$text, to='UTF-8', sub = "byte")
#names(airlineTweets)

# Obtain sentiment scores
s <- get_nrc_sentiment(tweets)
head(s)

tweets[4]
get_nrc_sentiment('blast')

# Bar plot
barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores for US Airlines Tweets')