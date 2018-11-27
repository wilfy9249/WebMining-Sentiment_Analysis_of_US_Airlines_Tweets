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


##To-do##
Senti <- get_nrc_sentiment(tweets)
tweets <- cbind(tweets, Senti)
sentimentSum <- data.frame(colSums(tweets[,c(16:25)]))
names(sentimentSum) <- "count"
sentimentSum <- cbind("sentiment" = rownames(sentimentSum), sentimentSum)
rownames(sentimentSum) <- NULL
ggplot(data = sentimentSum, aes(x = sentiment, y = count)) + geom_bar(aes(fill = sentiment), stat = "identity") + theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Sentiment Score for Airline")

adsSummary <- summary( tweets )
adsSummary 