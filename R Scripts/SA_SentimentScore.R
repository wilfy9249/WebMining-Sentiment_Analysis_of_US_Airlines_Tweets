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
names(airlineTweets)

# Obtain sentiment scores
s <- get_nrc_sentiment(tweets)
head(s)

### Sentiment Analysis 
tweets[4]
get_nrc_sentiment('blast')

# Bar plot
barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores for US Airlines Tweets')




## Sentiment Analysis based on each Airways

##-----------US Airways---------------------------------####
USAirways <- airlineTweets %>% filter(airline=="US Airways")
tweetsA <- iconv(USAirways$text, to='UTF-8', sub = "byte")
Senti <- get_nrc_sentiment(tweetsA)
tweetsA <- cbind(tweetsA, Senti)
sentimentSum <- data.frame(colSums(Senti))
names(sentimentSum) <- "count"
sentimentSum <- cbind("sentiment" = rownames(sentimentSum), sentimentSum)
#rownames(sentimentSum) <- NULL
ggplot(data = sentimentSum, aes(x = sentiment, y = count)) + geom_bar(aes(fill = sentiment), stat = "identity") + theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Sentiment Score for US Airways")


##----------Virgin America------------------------------###
VirginAmerica <- airlineTweets %>% filter(airline=="Virgin America")
tweetsVA <- iconv(VirginAmerica$text, to='UTF-8', sub = "byte")
Senti <- get_nrc_sentiment(tweetsVA)
tweetsVA <- cbind(tweetsVA, Senti)
sentimentSum <- data.frame(colSums(Senti))
names(sentimentSum) <- "count"
sentimentSum <- cbind("sentiment" = rownames(sentimentSum), sentimentSum)
#rownames(sentimentSum) <- NULL
ggplot(data = sentimentSum, aes(x = sentiment, y = count)) + geom_bar(aes(fill = sentiment), stat = "identity") + theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Sentiment Score for Virgin America")


##----------United-----------------------------------------##
united <- airlineTweets %>% filter(airline=="United")
tweetsU <- iconv(united$text, to='UTF-8', sub = "byte")
Senti <- get_nrc_sentiment(tweetsU)
tweetsU <- cbind(tweetsU, Senti)
sentimentSum <- data.frame(colSums(Senti))
names(sentimentSum) <- "count"
sentimentSum <- cbind("sentiment" = rownames(sentimentSum), sentimentSum)
#rownames(sentimentSum) <- NULL
ggplot(data = sentimentSum, aes(x = sentiment, y = count)) + geom_bar(aes(fill = sentiment), stat = "identity") + theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Sentiment Score for United")

##----------Southwest-----------------------------------------##
southwest <- airlineTweets %>% filter(airline=="Southwest")
tweetsSW <- iconv(southwest$text, to='UTF-8', sub = "byte")
Senti <- get_nrc_sentiment(tweetsSW)
tweetsSW <- cbind(tweetsSW, Senti)
sentimentSum <- data.frame(colSums(Senti))
names(sentimentSum) <- "count"
sentimentSum <- cbind("sentiment" = rownames(sentimentSum), sentimentSum)
#rownames(sentimentSum) <- NULL
ggplot(data = sentimentSum, aes(x = sentiment, y = count)) + geom_bar(aes(fill = sentiment), stat = "identity") + theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Sentiment Score for Southwest")

##----------Delta Airlines----------------------------------------##
delta <- airlineTweets %>% filter(airline=="Delta")
tweetsD <- iconv(delta$text, to='UTF-8', sub = "byte")
Senti <- get_nrc_sentiment(tweetsD)
tweetsD <- cbind(tweetsD, Senti)
sentimentSum <- data.frame(colSums(Senti))
names(sentimentSum) <- "count"
sentimentSum <- cbind("sentiment" = rownames(sentimentSum), sentimentSum)
#rownames(sentimentSum) <- NULL
ggplot(data = sentimentSum, aes(x = sentiment, y = count)) + geom_bar(aes(fill = sentiment), stat = "identity") + theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Sentiment Score for Delta")

##----------American Airlines----------------------------------------##
american <- airlineTweets %>% filter(airline=="American")
tweetsAmer <- iconv(american$text, to='UTF-8', sub = "byte")
Senti <- get_nrc_sentiment(tweetsAmer)
tweetsAmer <- cbind(tweetsAmer, Senti)
sentimentSum <- data.frame(colSums(Senti))
names(sentimentSum) <- "count"
sentimentSum <- cbind("sentiment" = rownames(sentimentSum), sentimentSum)
#rownames(sentimentSum) <- NULL
ggplot(data = sentimentSum, aes(x = sentiment, y = count)) + geom_bar(aes(fill = sentiment), stat = "identity") + theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Sentiment Score for American")




##Summary of Tweets
adsSummary <- summary( tweets )
adsSummary 