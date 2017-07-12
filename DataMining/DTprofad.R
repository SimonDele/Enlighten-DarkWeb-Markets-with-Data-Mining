#-------------------------------------------------------------------------------
#                  Decision tree - CART algorithm
#   Prediction of profitability of an ad knowing the seller / price / category
#-------------------------------------------------------------------------------

#data <- as.data.frame(read.csv("alphaClean.csv"))

library(stringr)
library(rattle)
library(rpart)
library(rpart.plot)
library(RColorBrewer)

#-----------------
#   New Data 
#-----------------

# Select the column of the data that are interesting for the tree
# ie removing colunm like "id" or "url" that don't give any informations
dectree.data <- subset(data, select=c(origin,category,seller,priceUnitDose,timestamp,sold_since,products_sold)) 
# Subset : choose the colunm that you want

# Handling : categorie
# Regular expression for spliting the categories
regex <- "/(.*)/(.*)/(.*)"
cat <- str_match(dectree.data$category, regex)
dectree.data$category <- cat[,3] # keep only the second part
# Removing all the ads that have no informations
matching_vector <- !(is.na(dectree.data$category))
dectree.data <- dectree.data[matching_vector,]

# Handling : products_sold
dectree.data$products_sold <- gsub(pattern="NULL", replacement="0", dectree.data$products_sold)
dectree.data$products_sold <- as.numeric(dectree.data$products_sold)

# Handling : timestamp and sold_since -> calculate the lifetime of the ad
dectree.data$sold_since <-  as.Date(dectree.data$sold_since)
dectree.data$timestamp <-  as.Date(dectree.data$timestamp)
dectree.data$timestamp <- dectree.data$timestamp - dectree.data$sold_since
# Put all the result in the column timestamp 
dectree.data$timestamp <- as.numeric(dectree.data$timestamp)
# Remove the column sold_since
#dectree.data <- subset(dectree.data, select= -c(sold_since))
# 1 day on the market at least
dectree.data <- dectree.data[which(dectree.data$timestamp > 0),]


# products_sold during 1 week

# Random rows :
dectree.data <- dectree.data[sample(nrow(dectree.data),nrow(dectree.data),replace=FALSE), ]
