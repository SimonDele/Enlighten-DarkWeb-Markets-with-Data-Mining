#install.packages("RTextTools")
#install.packages("ipred")
install.packages("tree")
install.packages("randomForest")
install.packages("caTools")
install.packages("nnet")


library(RTextTools)
library(tm)
library(stringr)

library(ipred)
library(tree)
library(randomForest)
library(caTools)
library(nnet)

data <- as.data.frame(read.csv("alphaClean.csv"))


# Select all "Drugs & Chemicals" ads
matching_vector <- c( str_detect(data$category, "Drugs & Chemicals"))
new.data <- data[matching_vector,]

# Random rows :
new.data <- new.data[sample(nrow(new.data),nrow(new.data),replace=FALSE), ]


# Handling : seller
tab_sel <- table(new.data$seller)
tab_sel <- sort(tab_sel, decreasing=TRUE)  # Sorting (biggest in first)
tab_sel <- tab_sel[1:5] # Taking only the most important : main sellers
name_sel <- names(tab_sel)

# New data keeping only the main sellers
new.data <-subset(new.data, seller %in% name_sel) 

new.data$seller <- factor(new.data$seller)

# CREATE THE DOCUMENT-TERM MATRIX
doc_matrix <- create_matrix(new.data$ad, language="english", removeNumbers=TRUE,
                            stemWords=TRUE, removeSparseTerms=.998)

container <- create_container(doc_matrix, new.data$seller, trainSize=1:500,
                              testSize=500:697, virgin=FALSE)

'
SVM <- train_model(container,"SVM")
#GLMNET <- train_model(container,"GLMNET")
MAXENT <- train_model(container,"MAXENT")
SLDA <- train_model(container,"SLDA")
BOOSTING <- train_model(container,"BOOSTING")
BAGGING <- train_model(container,"BAGGING")
RF <- train_model(container,"RF")
NNET <- train_model(container,"NNET")
TREE <- train_model(container,"TREE")

SVM_CLASSIFY <- classify_model(container, SVM)
#GLMNET_CLASSIFY <- classify_model(container, GLMNET)
MAXENT_CLASSIFY <- classify_model(container, MAXENT)
SLDA_CLASSIFY <- classify_model(container, SLDA)
BOOSTING_CLASSIFY <- classify_model(container, BOOSTING)
BAGGING_CLASSIFY <- classify_model(container, BAGGING)
RF_CLASSIFY <- classify_model(container, RF)
NNET_CLASSIFY <- classify_model(container, NNET)
TREE_CLASSIFY <- classify_model(container, TREE)
'

models <-train_models(container, algorithms=c("MAXENT","SVM", "SLDA", "BOOSTING","BAGGING","RF","NNET","TREE"))
results <- classify_models(container, models)

test <- new.data[500:697,]
# Comparison between the result and the prediction (prediction in colunm)
conf <- table(test[,match("seller",names(test))],SVM_CLASSIFY$SVM_LABEL)

# Accurency :
acc <- sum(diag(conf)) / sum(conf)

print(conf)
print(acc)

analytics <- create_analytics(container, results)

#analytics <- create_analytics(container,SVM_CLASSIFY)

summary(analytics)

# CREATE THE data.frame SUMMARIES
topic_summary <- analytics@label_summary
alg_summary <- analytics@algorithm_summary
ens_summary <-analytics@ensemble_summary
doc_summary <- analytics@document_summary

