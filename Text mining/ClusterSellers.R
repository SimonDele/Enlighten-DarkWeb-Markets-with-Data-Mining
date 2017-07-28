#----------------------------------------------------------------------
#           Predict if one person is hidden behind 2 identities
#-----------------------------------------------------------------------


library(RTextTools)
library(tm)
library(stringr)


#data <- as.data.frame(read.csv("alphaClean.csv"))


# Select all "Drugs & Chemicals" ads
matching_vector <- c( str_detect(data$category, "Drugs & Chemicals"))
new.data <- data[matching_vector,]

# Random rows :
new.data <- new.data[sample(nrow(new.data),nrow(new.data),replace=FALSE), ]


# Handling : seller
tab_sel <- table(new.data$seller)
tab_sel <- sort(tab_sel, decreasing=TRUE)  # Sorting (biggest in first)
#tab_sel <- tab_sel[(tab_sel >15)] # Taking only the most important : main sellers
name_sel <- names(tab_sel)

# New data keeping only the main sellers
#new.data <-subset(new.data, seller %in% name_sel) 
colSeller <- new.data$seller
new.data$seller <- new.data$seller == "Fapppylicious"

new.data$seller <- factor(new.data$seller)

# CREATE THE DOCUMENT-TERM MATRIX
doc_matrix <- create_matrix(new.data$ad, language="english", removeNumbers=TRUE,
                            stemWords=TRUE, removeSparseTerms=.998)

container <- create_container(doc_matrix, new.data$seller, trainSize=1:round(0.75*nrow(new.data)),
                              testSize=round(0.75*nrow(new.data)+1,0):nrow(new.data), virgin=FALSE)


SVM <- train_model(container,"SVM")


SVM_CLASSIFY <- classify_model(container, SVM)



test <- new.data[round(0.75*nrow(new.data)+1,0):nrow(new.data),]
colSeller.test <- colSeller[round(0.75*nrow(new.data)+1,0):nrow(new.data)]
# Comparison between the result and the prediction (prediction in colunm)
conf <- table(test[,match("seller",names(test))],SVM_CLASSIFY$SVM_LABEL)

erreur <- colSeller.test[(test[,match("seller",names(test))] != SVM_CLASSIFY$SVM_LABEL)]


# Accuracy :
acc <- sum(diag(conf)) / sum(conf)

print(conf)
print(acc)


