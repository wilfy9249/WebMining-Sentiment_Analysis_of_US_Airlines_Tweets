library(dplyr)
library(ggplot2)
library("wordcloud")
library(reshape2)
library(plyr)
library(tm)

##READ CSV FILE
airlineTweets <- read.csv(file.choose(), header = T)
names(airlineTweets)