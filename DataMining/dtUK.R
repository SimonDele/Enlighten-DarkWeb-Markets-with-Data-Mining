#----------------------------------------------------------------------
#                  Decision tree - CART algorithm
#   Prediction of the country knowing the seller / price / category
#-----------------------------------------------------------------------

#data <- as.data.frame(read.csv("data.csv"))

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

country <- "France"

# Conversion to binary value
# -> 1 if the origin = country
# -> 0 if the origin # country
dectree.data$origin <-c(str_detect(dectree.data$origin, country))
dectree.data$origin <-gsub(TRUE, 1, dectree.data$origin)
dectree.data$origin <- gsub(FALSE, 0, dectree.data$origin)
dectree.data$origin <- as.numeric(dectree.data$origin)

#---------------------
#   Decision tree
#---------------------

# Half of the data for making the decision tree
train <- dectree.data[1:(floor(nrow(dectree.data))/2),]

# Creation of the tree
tree <- rpart(origin ~.,data=train, method="class") 

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


#-------------------------------------------------
# /!\ To Do : keep only the 100 mains dealers /!\
#-------------------------------------------------