#----------------------------------------------------------------------
#                  Bayesian Classification - Naive
#   Prediction of the Seller knowing the origin / price / category / products_sold
#-----------------------------------------------------------------------

#data <- as.data.frame(read.csv("data.csv"))

library(stringr)
library(e1071)
library(arules)

#-----------------
#   New Data 
#-----------------

# Select all "Drugs & Chemicals" ads
matching_vector <- c( str_detect(data$category, "Drugs & Chemicals"))
bayesian.data <- data[matching_vector,]

# Select the column of the data that are interesting
# ie removing colunm like "id" or "url" that don't give any informations
bayesian.data <- subset(bayesian.data, select=c(origin,category,seller,priceUnitDose, products_sold, sold_since ))
# Subset : choose the colunm that you want

# Handling : column category
# Regular expression for spliting the categories
regex <- "/(.*)/(.*)/(.*)"
cat <- str_match(bayesian.data$category, regex)
bayesian.data$category <- cat[,3] # keep only the second part


sellers <- names(sort(table(bayesian.data$seller), decreasing = TRUE))[1:10]

bayesian.data <-subset(bayesian.data, seller %in% sellers)

bayesian.data$seller <- factor(bayesian.data$seller, labels = sellers)


#---------------------
#   Bayesian stat
#---------------------

# Random rows :
bayesian.data <- bayesian.data[sample(nrow(bayesian.data),nrow(bayesian.data),replace=FALSE), ]

train.data <- bayesian.data[1:floor(nrow(bayesian.data)/2),]
pred.data <- bayesian.data[(floor(nrow(bayesian.data)/2)+1):nrow(bayesian.data),]

model <- naiveBayes(seller ~ ., data =  train.data)


preds <- predict(model, newdata = pred.data)
conf_matrix <- table(preds, pred.data$seller)
acc <- sum(diag(conf_matrix)) / sum(conf_matrix)

print(conf_matrix)
print("accuracy")
print(acc)



