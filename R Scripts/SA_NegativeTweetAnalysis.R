library(dplyr)
library(ggplot2)
library("wordcloud")
library(reshape2)
library(plyr)
library(tm)

##READ CSV FILE
airlineTweets <- read.csv(file.choose(), header = T)
names(airlineTweets)

##----1. FIND THE DISTRIBUTION OF NEGATIVE/POSITIVE -----------------##
airlineTweets$date <-  as.Date(airlineTweets$tweet_created)
  
##---GROUP BY AIRLINE SENTIMENT------------##
posNegDist = airlineTweets %>% group_by(airline_sentiment) %>% dplyr::summarise(count = n())
posNegDist

##PLOT THE GRAPH
overallSentiment = as.data.frame(table(airlineTweets$airline_sentiment))
colnames(overallSentiment) = c("Sentiment","Freq")
histPlot = ggplot(overallSentiment) + aes(x=Sentiment, y=Freq, fill=Sentiment) + scale_fill_manual(values=c("#d03501", "#E69F00", "#009E73"))
histPlot = histPlot + geom_bar(stat="identity")
histPlot

##----2. DISTRIBUTION BY AIRLINE-----------------##
ggplot(airlineTweets, aes(airlineTweets$airline, fill=airline_sentiment)) + geom_bar()+ scale_fill_manual(values=c("#d03501", "#E69F00", "#009E73"))

airlines= airlineTweets %>% group_by(airline) %>% dplyr::summarise(count=n())
posNegByAirline <-dcast(airlineTweets, airline ~ airline_sentiment)
posNegByAirline$negPer = posNegByAirline$negative / (posNegByAirline$negative + posNegByAirline$positive + posNegByAirline$neutral)
posNegByAirline = posNegByAirline[order(-posNegByAirline$negPer),] 
posNegByAirline


###----TODO: PLOT THE AIRLINES THAT HAVE NEGATIVE PERCENTAGE OF MORE THAN 50%-----##

##----NEGATIVE TWEETS--------##

negativeTweets <- airlineTweets %>% filter(airline_sentiment=="negative")
negativeTweetsByDate <- negativeTweets %>% group_by(date) %>% dplyr::summarise(count = n())
negativeTweetsByDatePlot = ggplot() + geom_line(data=negativeTweetsByDate, aes(x=date, y=count, group = 1))+ scale_fill_manual(values=c("#d03501"))
negativeTweetsByDatePlot

##----PLOT BY AIRLINES----##
negativeTweetsByDateByAirline <- negativeTweets %>% group_by(airline,date) %>% dplyr::summarise(count = n())
negativeTweetsByDateByAirlinePlot = ggplot() + geom_line(data=negativeTweetsByDateByAirline, aes(x=date, y=count, group =airline , color=airline)) 
negativeTweetsByDateByAirlinePlot

##----REASONS FOR NEGATIVE TWEETS---##
negativeReasonTweets <- negativeTweets %>% group_by(negativereason) %>% dplyr::summarise(count=n()) %>% arrange(desc(count))
negativeReasonTweets

##---BY AIRLINES---##
negativeReasonTweetsByAirline <- negativeTweets %>% group_by(airline,negativereason) %>% dplyr::summarise(count=n())%>% arrange(airline,desc(count))
negativeReasonTweetsByAirline

topReasonPerAirline <- ddply(negativeReasonTweetsByAirline, "airline", function(z) head(z,1))
topReasonPerAirline

##---PLOT THE GRAPH---## 
airlineTweets <- airlineTweets[!airlineTweets$negativereason=="",]
graph <- as.data.frame(prop.table(table(airlineTweets[,c("negativereason","airline")]))*100)
names(graph)
colnames(graph) <- c("Negative_Reason","Airline","Proportion_of_Negative_Reason")

##--American Airlines--##
graph_American <- subset(graph,Airline=="American")
ggplot(graph_American,aes(x=Airline,y=Proportion_of_Negative_Reason,fill=Negative_Reason))+geom_bar(stat="identity",position="dodge")

##--Delta Airlines--##
graph_Delta <- subset(graph,Airline=="Delta")
ggplot(graph_Delta,aes(x=Airline,y=Proportion_of_Negative_Reason,fill=Negative_Reason))+geom_bar(stat="identity",position="dodge")

##--SouthWest Airlines--##
graph_SouthWest <- subset(graph,Airline=="Southwest")
ggplot(graph_SouthWest,aes(x=Airline,y=Proportion_of_Negative_Reason,fill=Negative_Reason))+geom_bar(stat="identity",position="dodge")

##--United Airlines--##
graph_United <- subset(graph,Airline=="United")
ggplot(graph_United,aes(x=Airline,y=Proportion_of_Negative_Reason,fill=Negative_Reason))+geom_bar(stat="identity",position="dodge")

##--US Airways--##
graph_US <- subset(graph,Airline=="US Airways")
ggplot(graph_US,aes(x=Airline,y=Proportion_of_Negative_Reason,fill=Negative_Reason))+geom_bar(stat="identity",position="dodge")

##--Virgin America--##
graph_Virgin <- subset(graph,Airline=="Virgin America")
ggplot(graph_Virgin,aes(x=Airline,y=Proportion_of_Negative_Reason,fill=Negative_Reason))+geom_bar(stat="identity",position="dodge")




