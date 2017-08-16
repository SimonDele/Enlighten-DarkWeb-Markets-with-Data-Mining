#----------------------------------------------------------------------
#                  Decision tree - CART algorithm
#   Prediction of the seller knowing the price / category / origin
#-----------------------------------------------------------------------

data <- as.data.frame(read.csv("alphaClean.csv"))

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

# Handling : seller
tab_sel <- table(dectree.data$seller)
tab_sel <- sort(tab_sel, decreasing=TRUE)  # Sorting (biggest in first)
tab_sel <- tab_sel[1:10] # Taking only the most important : main sellers
name_sel <- names(tab_sel)
# New data keeping only the main sellers
dectree.data <-subset(dectree.data, seller %in% name_sel) 

# Random rows :
dectree.data <- dectree.data[sample(nrow(dectree.data),nrow(dectree.data),replace=FALSE), ]

#---------------------
#   Decision tree
#---------------------

# Factor
dectree.data$seller <- factor(dectree.data$seller)

# Half of the data for making the decision tree
train <- dectree.data[1:(floor(nrow(dectree.data))/2),]

# Creation of the tree
tree <- rpart(seller ~.,data=train, method="class") 

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
  conf <- table(test[,match("seller",names(test))],pred)
  
  # Accurency :
  acc <- sum(diag(conf)) / sum(conf)

print(conf)
print(acc)


library(corrplot)

for(i in 1:nrow(conf)){
  conf[,i] <- conf[,i]/sum(conf[,i])
}

corrplot(conf,cl.lim=c(0,1),method="color",type="lower")

#https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
