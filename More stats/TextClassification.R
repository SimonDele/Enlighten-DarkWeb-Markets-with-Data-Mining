#install.packages("tm")
#install.packages("plyr")
#install.packages("class")
#install.packages("caret")
#install.packages("SnowBallC")



library(tm)
library(plyr)
library(class)
library(caret)

data <- as.data.frame(read.csv("alphaClean.csv"))

dataText <- subset(data, select = c(ad))
dataText.vec <- dataText$ad
corpus <- Corpus(VectorSource(dataText.vec))

corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removeWords, stopwords("english"))
#tdm <- DocumentTermMatrix(corpus, list(removePunctuation = TRUE, stopwords = TRUE, stemming = TRUE, removeNumbers = TRUE))

tdm = TermDocumentMatrix(corpus)
tdm = removeSparseTerms(tdm, 0.90)

#train <- sample(nrow(tdm), ceiling(nrow(tdm)*0.7))
#test = (1:nrow(tdm))[-train]
#corpus.data <- as.matrix(tdm)

#train data
train <- as.matrix(tdm[1:round(nrow(corpus.data)*3/4,0)])
train <- as.data.frame(train)
train <- cbind(train, data$seller[1:nrow(train)])
colnames(train)[ncol(train)] <- 'seller'

train$seller <- as.factor(train$seller)

#model
fit <- train(seller ~ ., data = train, method = 'bayesglm')
#test data
test <- as.matrix(tdm[(round(nrow(tdm)*3/4,0)+1), nrow(tdm),])
test <- as.data.frame(test)