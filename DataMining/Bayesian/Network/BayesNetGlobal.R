#----------------------------------------------------------------------
#                       Bayesian Network
#    with seller / origin / price / category / timestamp / sold_since / product_sold
#-----------------------------------------------------------------------

data <- as.data.frame(read.csv("data.csv"))


library(arules)
library(stringr)

#---------------------
#   Preprocessing
#---------------------

# Select the column of the data that are interesting
# ie removing colunm like "id" or "url" that don't give any informations
network.data <- subset(data, select=c(seller, origin, category, products_sold, priceUnitDose, sold_since, timestamp))

for(i in 1 : length(colnames(network.data))){
  network.data <- network.data[!is.element(network.data[,i], "NULL"),]
}


#Given timestamp and sold_since calculate the lifetime of the ad
network.data$sold_since <-  as.Date(network.data$sold_since)
network.data$timestamp <-  as.Date(network.data$timestamp)

network.data$timestamp <- network.data$timestamp - network.data$sold_since
network.data$timestamp <- as.numeric(network.data$timestamp)
#names(network.data$timestamp) <- "lifetime"
network.data <- subset(network.data, select= -c(sold_since))

#Discretize timestamp variable
network.data$timestamp <- arules::discretize(network.data$timestamp, "frequency", categories = 10)

bayesian.data$category <- as.factor(bayesian.data$category)

#---------------------
#   Bayesian stat
#---------------------
library(bnlearn)

bn.gs <- hc(network.data)
plot(bn.gs)