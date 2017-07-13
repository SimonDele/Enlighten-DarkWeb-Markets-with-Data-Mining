#-------------------------------------------------------------------------------
#                  Decision tree - CART algorithm
#   Prediction of profitability of an ad knowing the seller / price / category
#-------------------------------------------------------------------------------

data <- as.data.frame(read.csv("alphaClean.csv"))

library(stringr)
library(rattle)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(arules)

#-----------------
#   New Data 
#-----------------

# Select all "Drugs & Chemicals" ads
matching_vector <- c( str_detect(data$category, "Drugs & Chemicals"))
dectree.data <- data[matching_vector,]

# Select the column of the data that are interesting for the tree
# ie removing colunm like "id" or "url" that don't give any informations
dectree.data <- subset(dectree.data, select=c(origin,category,seller,priceUnitDose,timestamp,sold_since,products_sold)) 
# Subset : choose the colunm that you want

# Handling : categorie
# Regular expression for spliting the categories
regex <- "/(.*)/(.*)/(.*)"
cat <- str_match(dectree.data$category, regex)
dectree.data$category <- cat[,3] # keep only the second part

#dectree.data <- dectree.data[which(dectree.data$origin != "Worldwide"),]
'
tab_cat <- table(dectree.data$category)
tab_cat <- sort(tab_cat, decreasing=TRUE)  # Sorting (biggest in first)
tab_cat <- tab_cat[1:5] # Taking only the most important : main sellers
name_cat <- names(tab_cat)
name_cat <- name_cat[!is.element(name_cat, "Other")]
# New data keeping only the main dealers
dectree.data <-subset(dectree.data, category %in% name_cat)
'

# Handling : seller
tab_sel <- table(dectree.data$seller)
tab_sel <- sort(tab_sel, decreasing=TRUE)  # Sorting (biggest in first)
tab_sel <- tab_sel[1:10] # Taking only the most important : main sellers
name_sel <- names(tab_sel)
name_sel <- name_sel[!is.element(name_sel, "NULL")]
# New data keeping only the main sellers
dectree.data <-subset(dectree.data, seller %in% name_sel)


# Handling : products_sold
dectree.data$products_sold <- gsub(pattern="NULL", replacement="0", dectree.data$products_sold)
dectree.data$products_sold <- as.numeric(dectree.data$products_sold)

# Handling : timestamp and sold_since -> calculate the lifetime of the ad
dectree.data$sold_since <-  as.Date(dectree.data$sold_since)
dectree.data$timestamp <-  as.Date(dectree.data$timestamp)
dectree.data$timestamp <- dectree.data$timestamp - dectree.data$sold_since
dectree.data$timestamp <- as.numeric(dectree.data$timestamp)
# 1 day on the market at least
dectree.data <- dectree.data[which(dectree.data$timestamp > 0),]
# Switch the time per mouth
dectree.data$timestamp <- (dectree.data$timestamp/30)

# products_sold during 1 mouth
productSoldPerWeek <- c()
for(i in 1 : length(dectree.data$products_sold)) {
  productSoldPerWeek[i] <- round(dectree.data$products_sold[i]/dectree.data$timestamp[i],digits=1)
}
dectree.data$ProdSoldPerWeek <- productSoldPerWeek

# Discretize the variable
dectree.data$ProdSoldPerWeek <- discretize(dectree.data$ProdSoldPerWeek, "frequency", categories = 10)

# Remove the column sold_since
dectree.data <- subset(dectree.data, select= -c(sold_since,products_sold,timestamp))

# Random rows :
dectree.data <- dectree.data[sample(nrow(dectree.data),nrow(dectree.data),replace=FALSE), ]

#---------------------
#   Decision tree
#---------------------

# Factor
dectree.data$ProdSoldPerWeek <- factor(dectree.data$ProdSoldPerWeek)

# Half of the data for making the decision tree
train <- dectree.data[1:(floor(nrow(dectree.data))/2),]

# Creation of the tree
tree <- rpart(ProdSoldPerWeek ~.,data=train, method="class") 

# Plot
fancyRpartPlot(tree)

#--------------------
#   Prediction
#--------------------

# The other half for the prediction
test <- dectree.data[(floor(nrow(dectree.data)/2)+1):nrow(dectree.data),]

# Making prediction
pred <- predict(tree,test,type="class")

# Analysis:

# Comparison between the result and the prediction (prediction in colunm)
conf <- table(test[,match("ProdSoldPerWeek",names(test))],pred)

# Accurency :
acc <- sum(diag(conf)) / sum(conf)

print(conf)
print(acc)
