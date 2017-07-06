#----------------------------------------------------------
#           Decision tree # CART
#----------------------------------------------------------

data <- as.data.frame(read.csv("data.csv"))

library(stringr)
library(rattle)
library(rpart)
library(rpart.plot)
library(RColorBrewer)

#Select all ads of "Drugs & Chemicals"
matching_vector <- c( str_detect(data$category, "Drugs & Chemicals"))
data_drugs <- data[matching_vector, ]

decision.data <- subset(data_drugs, select = c(origin,category,seller,priceUnitDose))
decision.data$category <- gsub(pattern = "/Drugs & Chemicals/", replacement = "", decision.data$category)

decision.data$origin <-c ( str_detect(decision.data$origin, "United Kingdom"))

names(decision.data)[match("origin",names(decision.data))] <- "origin_UK"

decision.data[,match("origin_UK",names(decision.data))]<-gsub(TRUE, 1, decision.data[,match("origin_UK",names(decision.data))])
decision.data[,match("origin_UK",names(decision.data))] <- gsub(FALSE, 0, decision.data[,match("origin_UK",names(decision.data))])

decision.data$origin_UK <- as.numeric(decision.data$origin_UK)

train <- decision.data[1:floor(length(decision.data[,1])/2),]
test <- decision.data[(floor(length(decision.data[,1])/2)+1):length(decision.data[,1]),]

tree <- rpart(origin_UK ~.,data=train, method="class")
fancyRpartPlot(tree)

pred <- predict(tree,test,type="class")
conf <- table(decision.data[(floor(length(decision.data[,1])/2)+1):length(decision.data[,1]),match("origin_UK",names(decision.data))],pred)
acc <- sum(diag(conf)) / sum(conf)