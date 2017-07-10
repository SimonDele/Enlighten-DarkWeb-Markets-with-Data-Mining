#----------------------------------------------------------------------
#                  Decision tree - CART algorithm
#   Prediction of the country knowing the seller / price / category
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
drugs.data <- data[matching_vector,]

# Select the column of the data that are interesting for the tree
# ie removing colunm like "id" or "url" that don't give any informations
drugs.data <- subset(drugs.data, select=c(origin,category,seller,priceUnitDose))
# Subset : choose the colunm that you want

# Handling : column categorie
# Regular expression for spliting the categories
regex <- "/(.*)/(.*)/(.*)"
cat <- str_match(drugs.data$category, regex)
drugs.data$category <- cat[,3] # keep only the second part

country <- "United Kingdom"

# Conversion to binary value
# -> 1 if the origin = country
# -> 0 if the origin # country
drugs.data$origin <-c(str_detect(drugs.data$origin, country))
drugs.data$origin <-gsub(TRUE, 1, drugs.data$origin)
drugs.data$origin <- gsub(FALSE, 0, drugs.data$origin)
drugs.data$origin <- as.numeric(drugs.data$origin)


# Handling : seller
#drugs.data$seller <- as.character(drugs.data$seller)
tab_sell <- table(drugs.data$seller)
tab_sell <- sort(tab_sell, decreasing=TRUE)  # Sorting (biggest in first)
tab_sell <- tab_sell[1:30] # Taking only the most important : 100 main sellers

# Contruction of the matching vector with names of the 100 biggest seller
matching_vector <- c( str_detect(drugs.data$seller, names(tab_sell)[1]))
for(i in 2:length(tab_sell)) {
  matching_vector <- matching_vector | c( str_detect(drugs.data$seller, names(tab_sell)[i]))
}
# New data keeping only the 100 mains dealers
dectree.data <- drugs.data[matching_vector,]


#---------------------
#   Decision tree
#---------------------

# Half of the data for making the decision tree
train <- dectree.data[1:(floor(nrow(dectree.data))/2),]

# Creation of the tree
tree <- rpart(origin ~.,data=train, method="class", control=rpart.control(cp=0.0001,minsplit=10)) 

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
conf <- table(test[,match("origin",names(test))],pred)

# Accurency :
acc <- sum(diag(conf)) / sum(conf)

print(conf)
print(acc)