#install.packages("tm")
#install.packages("plyr")
#install.packages("class")
#install.packages("caret")
#install.packages("SnowBallC")



library(tm)
library(plyr)
library(class)
library(caret)
library(stringr)

data <- as.data.frame(read.csv("alphaClean.csv"))

# Select all "Drugs & Chemicals" ads
matching_vector <- c( str_detect(data$category, "Drugs & Chemicals"))
new.data <- data[matching_vector,]

# Handling : seller
tab_sel <- table(new.data$seller)
tab_sel <- sort(tab_sel, decreasing=TRUE)  # Sorting (biggest in first)
tab_sel <- tab_sel[1:5] # Taking only the most important : main sellers
name_sel <- names(tab_sel)

# New data keeping only the main sellers
new.data <-subset(new.data, seller %in% name_sel) 
#new.data$seller <- factor(new.data$seller)

#Create corpus
dataText <- subset(new.data, select = c(ad))
dataText.vec <- new.data$ad
corpus <- Corpus(VectorSource(dataText.vec))

#Preprocessing corpus
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removeWords, stopwords("english"))

tdm = TermDocumentMatrix(corpus)
tdm = removeSparseTerms(tdm, 0.95)

#add sellers
corpus.data <- as.matrix(tdm)
corpus.data <- as.data.frame(corpus.data)
corpus.data <- t(corpus.data)

corpus.data <- cbind(corpus.data, as.character(new.data$seller))
corpus.data <- as.data.frame(corpus.data)
#names(corpus.data)[ncol(corpus.data)-1] <- "toDelete" #number of factor
names(corpus.data)[ncol(corpus.data)] <- "seller"
#corpus.data <- subset(corpus.data, select = -c(V233)) 
#corpus.data <- subset(corpus.data, select = -c(toDelete))  
#colnames(corpus.data)[ncol(corpus.data)] <- 'seller'

# Random rows :
corpus.data <- corpus.data[sample(nrow(corpus.data),nrow(corpus.data),replace=FALSE), ]

#train data
train <- corpus.data[1:round(nrow(corpus.data)*0.75,0),]
train <- as.data.frame(train)


#model
fit <- train(s ~ ., data = train, method = 'bayesglm')

#test data
test <- corpus.data[(round(nrow(corpus.data)*0.75,0)+1): nrow(corpus.data),]

pred <- predict(fit, newdata = test)

# Analysis:

# Comparison between the result and the prediction (prediction in colunm)
conf <- table(test[,match("s",names(test))],pred)

# Accurency :
acc <- sum(diag(conf)) / sum(conf)

print(conf)
print(acc)