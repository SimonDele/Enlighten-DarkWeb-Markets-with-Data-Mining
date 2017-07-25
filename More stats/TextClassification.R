#intall.packages("tm")
#intall.packages("plyr")
#intall.packages("class")
#intall.packages("caret")
#intall.packages("SnowBallC")



library(tm)
library(plyr)
library(class)
library(caret)

data <- as.data.frame(read.csv("../alphaClean.csv"))

dataText <- subset(data, select = c(ad))
dataText.vec <- dataText$ad
corpus <- Corpus(VectorSource(dataText.vec))

corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removeWords, stopwords("english"))
#tdm <- DocumentTermMatrix(corpus, list(removePunctuation = TRUE, stopwords = TRUE, stemming = TRUE, removeNumbers = TRUE))

tdm = TermDocumentMatrix(corpus)
tdm = removeSparseTerms(tdm, 0.7)

train <- sample(nrow(tdm), ceiling(nrow(tdm)*0.7))
test = (1:nrow(tdm))[-train]


train <- as.matrix(tdm[1:round(nrow(dataText)*3/4,0)])
train <- cbind(train, data$seller[1:nrow(train)])
colnames(train)[ncol(train)] <- 'seller'
train <- as.data.frame(train)
train$seller <- as.factor(train$seller)