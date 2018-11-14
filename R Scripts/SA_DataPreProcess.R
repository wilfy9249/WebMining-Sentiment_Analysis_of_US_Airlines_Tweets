
########################################################################
#                GROUP1: WEB  MINING - FINAL PROJECT
########################################################################

##IMPORT LIBRARIES
library(readr)
library(tm)

##READ CSV FILE
airlineTweets <- read.csv(file.choose(), header = T)

#Display Info of the dataframe
str(airlineTweets)

#Show observations in columns
head(airlineTweets)

##Create a DataFrame of Negative Sentiments
negativeSubset = subset(airlineTweets, airline_sentiment == "negative")
str(negativeSubset)

##----------DATA CLEANING------------------------##

##BUILD CORPUS AND CLEAN THE TEXT
corpus = Corpus(VectorSource(negativeSubset$text))
inspect(corpus[1:5])

corpus <- tm_map(corpus, stripWhitespace)
#strwrap(corpus[[8]])
inspect(corpus[1:5])

corpus <- tm_map(corpus, content_transformer(tolower))
inspect(corpus[1:5])

#Remove stopwords
corpus = tm_map(corpus, removeWords, stopwords("english"))
inspect(corpus[1:5])

# Want to get rid of @virginamerica, @usairways, @united, @southwestair, @jetblue, @americanair
corpus <- tm_map(corpus, function(x) gsub('@', 'ReplacedText', x))
inspect(corpus[1:5])

corpus = tm_map(corpus, removeWords, c("ReplacedTextvirginamerica","ReplacedTextusairways","ReplacedTextunited","ReplacedTextsouthwestair","ReplacedTextjetblue","ReplacedTextamericanair"))
inspect(corpus[1:5])


