##------------------------------------------------------------------##
##-----------------GROUP1: WEB  MINING - FINAL PROJECT--------------##

##IMPORT LIBRARIES
library(readr)
library(tm)

##READ CSV FILE
airlineTweets <- read.csv(file.choose(), header = T)

#Display Info of the dataframe
str(airlineTweets)

#Show observations in columns
head(airlineTweets)

