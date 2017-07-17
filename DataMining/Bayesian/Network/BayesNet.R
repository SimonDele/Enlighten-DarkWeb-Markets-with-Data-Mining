#----------------------------------------------------------------------
#                       Bayesian Network
#    with seller / origin / price / category / timestamp / sold_since / product_sold
#-----------------------------------------------------------------------

data <- as.data.frame(read.csv("data.csv"))


library(arules)
library(stringr)
library(bnlearn)

#-----------------
#   New Data 
#-----------------

# Select all "Drugs & Chemicals" ads
matching_vector <- c( str_detect(data$category, "Drugs & Chemicals"))
bayesian.data <- data[matching_vector,]


# Select the column of the data that are interesting
# ie removing colunm like "id" or "url" that don't give any informations
bayesian.data <- subset(bayesian.data, select=c(origin,category,seller,priceUnitDose, products_sold, sold_since, timestamp ))
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

#Given timestamp and sold_since calculate the lifetime of the ad
bayesian.data$sold_since <-  as.Date(bayesian.data$sold_since)
bayesian.data$timestamp <-  as.Date(bayesian.data$timestamp)
bayesian.data$timestamp <- bayesian.data$timestamp - bayesian.data$sold_since
bayesian.data$timestamp <- as.numeric(bayesian.data$timestamp)

# 1 day on the market at least
bayesian.data <- bayesian.data[which(bayesian.data$timestamp > 0),]

#Calculate profitability
bayesian.data$products_sold <- (bayesian.data$products_sold / bayesian.data$timestamp) * 30
names(bayesian.data)[match("products_sold",names(bayesian.data))] <- "profitability"

#Discretize profitability
nbCategory <- 5
bayesian.data$profitability <- arules::discretize(bayesian.data$profitability, method="frequency", categories = nbCategory)

bayesian.data <- subset(bayesian.data, select= -c(sold_since, timestamp))

#Convert variables to factor
bayesian.data$category <- as.factor(bayesian.data$category)
bayesian.data$seller <- as.factor(bayesian.data$seller)
bayesian.data$origin <- as.factor(bayesian.data$origin)

#Get rid of lines with NA as products_sold value
bayesian.data <- bayesian.data[!is.element(bayesian.data$profitability, NA),]

#---------------------
#   Bayesian Network
#---------------------

#Neural Network
res <- hc(bayesian.data)
plot(res)

#Conditionnal probability
fittedbn <- bn.fit(res, data = bayesian.data)
print(fittedbn$profitability)

#Handling interval
interv <- levels(bayesian.data$profitability)
interv <- unlist(strsplit(interv, ","))
interv <- gsub(pattern = "[^0-9.]*", replacement = "", interv)
interval <- data.frame(interv[seq(1, length(interv), 2)],interv[seq(2, length(interv), 2)])
colnames(interval) <- c("left", "right")
interval$left <- as.numeric(levels(interval$left))
interval$right <- as.numeric(levels(interval$right))

expectancy <- c()
#Calculate 
for(i in 0:length(table(bayesian.data$category))-1){
  left <- 0
  right <- 0
  for(j in 1:nbCategory){
    left<-left + interval$left[j] * fittedbn$profitability$prob[i*nbCategory + j]
    right<-right + interval$right[j] * fittedbn$profitability$prob[i*nbCategory + j]
  }
  expectancy[i] <- paste("[", left , "," , right , "]")
  
}


