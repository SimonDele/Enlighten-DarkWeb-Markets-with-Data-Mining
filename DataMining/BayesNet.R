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
bayesian.data$products_sold <- arules::discretize(bayesian.data$products_sold, method="frequency", categories = 10)


#Given timestamp and sold_since calculate the lifetime of the ad
bayesian.data$sold_since <-  as.Date(bayesian.data$sold_since)
bayesian.data$timestamp <-  as.Date(bayesian.data$timestamp)

bayesian.data$timestamp <- bayesian.data$timestamp - bayesian.data$sold_since
bayesian.data$timestamp <- as.numeric(bayesian.data$timestamp)
#names(bayesian.data$timestamp) <- "lifetime"
bayesian.data <- subset(bayesian.data, select= -c(sold_since))


#Discretize timestamp variable
bayesian.data$timestamp <- arules::discretize(bayesian.data$timestamp, "frequency", categories = 10)

bayesian.data$category <- as.factor(bayesian.data$category)
bayesian.data$seller <- as.factor(bayesian.data$seller)
bayesian.data$origin <- as.factor(bayesian.data$origin)

#---------------------
#   Bayesian stat
#---------------------

res <- hc(bayesian.data)
plot(res)

#res$arcs <- res$arcs[which((res$arcs[,'from'] == "timestamp" & res$arcs[,'to'] == "products_sold")),]

fittedbn <- bn.fit(res, data = bayesian.data)

print(fittedbn$products_sold)
