#----------------------------------------------------------------------
#                  Decision tree - CART algorithm
#   Prediction of the seller knowing the price / category / origin
#-----------------------------------------------------------------------

#data <- as.data.frame(read.csv("alphaClean.csv"))

library(stringr)
library(rattle)
library(rpart)
library(rpart.plot)
library(RColorBrewer)

#-----------------
#   New Data 
#-----------------

# Select all "Drugs & Chemicals" ads
matching_vector <- c( str_detect(data$category, "Drugs & Chemicals"))
dectree.data <- data[matching_vector,]

# Select the column of the data that are interesting for the tree
# ie removing colunm like "id" or "url" that don't give any informations
dectree.data <- subset(dectree.data, select=c(origin,category,seller,priceUnitDose))
# Subset : choose the colunm that you want

# Handling : column categorie
# Regular expression for spliting the categories
regex <- "/(.*)/(.*)/(.*)"
cat <- str_match(dectree.data$category, regex)
dectree.data$category <- cat[,3] # keep only the second part

Seller <- "klosterbier"

# For ex : (10 important dealers)
# klosterbier, jnenfrancis, rgn, ALaurizen, ROCKETLABS, optiman, 
# Fapppylicious, GreenLeafLabs, FelixUK, MicroDroper

# Conversion to binary value
# -> 1 if the seller = Seller
# -> 0 if the seller # Seller
dectree.data$seller <-c(str_detect(dectree.data$seller, Seller))
dectree.data$seller <-gsub(TRUE, 1, dectree.data$seller)
dectree.data$seller <- gsub(FALSE, 0, dectree.data$seller)
dectree.data$seller <- as.numeric(dectree.data$seller)

# Random rows :
dectree.data <- dectree.data[sample(nrow(dectree.data),nrow(dectree.data),replace=FALSE), ]

#---------------------
#   Decision tree
#---------------------

# Half of the data for making the decision tree
train <- dectree.data[1:(floor(nrow(dectree.data))/2),]

# Creation of the tree
tree <- rpart(seller ~.,data=train, method="class") 

# Plot
#fancyRpartPlot(tree)

fancyRpartPlot(prune(tree,cp=0.1))

#--------------------
#   Prediction
#--------------------

# The other half for the prediction
test <- dectree.data[(floor(nrow(dectree.data)/2)+1):nrow(dectree.data),]

# Making prediction
pred <- predict(tree,test,type="class")

# Analysis:

# Comparison between the result and the prediction (prediction in colunm)
conf <- table(test[,match("seller",names(test))],pred)

# Accurency :
acc <- sum(diag(conf)) / sum(conf)

print(conf)
print(acc)
