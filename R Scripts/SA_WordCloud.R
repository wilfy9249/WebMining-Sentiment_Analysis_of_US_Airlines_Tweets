library(dplyr)
library(ggplot2)
library("wordcloud")
library(reshape2)
library(plyr)
library(tm)

##READ CSV FILE
airlineTweets <- read.csv(file.choose(), header = T)
names(airlineTweets)

docs <- Corpus(VectorSource(airlineTweets$text))
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, c("usairways" ,"united", "flight" , "americanair" , "jetblue" , "southwestair")) 
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)


wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

##---NEGATIVE TWEETS--##

negativeTweets <- airlineTweets %>% filter(airline_sentiment=="negative")
docs2 <- Corpus(VectorSource(negativeTweets$text))
docs2 <- tm_map(docs2, content_transformer(tolower))
docs2 <- tm_map(docs2, removeNumbers)
docs2 <- tm_map(docs2, removeWords, stopwords("english"))
docs2 <- tm_map(docs2, removeWords, c("usairways" ,"united", "flight" , "americanair" , "jetblue" , "southwestair")) 
docs2 <- tm_map(docs2, removePunctuation)
docs2 <- tm_map(docs2, stripWhitespace)

dtm2 <- TermDocumentMatrix(docs2)
m2 <- as.matrix(dtm2)
v2 <- sort(rowSums(m2),decreasing=TRUE)
d2 <- data.frame(word = names(v2),freq=v2)
head(d2, 10)


wordcloud(words = d2$word, freq = d2$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))