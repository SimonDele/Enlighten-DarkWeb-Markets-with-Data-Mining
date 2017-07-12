#----------------------------------------------------------------------
#                  Bayesian Classification - Naive
#   Prediction of the product_sold knowing the seller / origin / price / category
#-----------------------------------------------------------------------

data <- as.data.frame(read.csv("data.csv"))

library(stringr)
library(e1071)
library(arules)


#-----------------
#   New Data 
#-----------------


# Select the column of the data that are interesting
# ie removing colunm like "id" or "url" that don't give any informations
bayesian.data <- subset(data, select=c(origin,category,seller,priceUnitDose, products_sold, sold_since, timestamp ))
# Subset : choose the colunm that you want

# Handling : column category
# Regular expression for spliting the categories
regex <- "/(.*)/(.*)/(.*)"
cat <- str_match(bayesian.data$category, regex)
bayesian.data$category <- cat[,3] # keep only the second part

#Get rid of lines with Null as products_sold value
bayesian.data <- bayesian.data[!is.element(bayesian.data$products_sold, "NULL"),]

#Convert products_sold to numeric and discretize it
bayesian.data$products_sold <- as.numeric(as.character(bayesian.data$products_sold))
bayesian.data$products_sold <- discretize(bayesian.data$products_sold, "interval", categories = 10)


#Given timestamp and sold_since calculate the lifetime of the ad
bayesian.data$sold_since <-  as.Date(bayesian.data$sold_since)
bayesian.data$timestamp <-  as.Date(bayesian.data$timestamp)

bayesian.data$timestamp <- bayesian.data$timestamp - bayesian.data$sold_since
bayesian.data$timestamp <- as.numeric(bayesian.data$timestamp)
#names(bayesian.data$timestamp) <- "lifetime"
bayesian.data <- subset(bayesian.data, select= -c(sold_since))

#Discretize timestamp variable
bayesian.data$timestamp <- discretize(bayesian.data$timestamp, "frequency", categories = 10)

#---------------------
#   Bayesian stat
#---------------------

# Random rows :
bayesian.data <- bayesian.data[sample(nrow(bayesian.data),nrow(bayesian.data),replace=FALSE), ]

train.data <- bayesian.data[1:floor(nrow(bayesian.data)/2),]
pred.data <- bayesian.data[(floor(nrow(bayesian.data)/2)+1):nrow(bayesian.data),]

model <- naiveBayes(products_sold ~ ., data =  train.data)


preds <- predict(model, newdata = pred.data)
conf_matrix <- table(preds, pred.data$products_sold)
acc <- sum(diag(conf_matrix)) / sum(conf_matrix)

print(conf_matrix)
print(acc)

