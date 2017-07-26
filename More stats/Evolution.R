#------------------------------------------------------------
#                 Evolution of the Market
#------------------------------------------------------------

data <- as.data.frame(read.csv("alphaClean.csv"))

library(stringr)
library(lubridate)

#-----------------
#   New Data 
#-----------------

# Select all "Drugs & Chemicals" ads
matching_vector <- c( str_detect(data$category, "Drugs & Chemicals"))
Evo.data <- data[matching_vector,]

# Select the column of the data that are interesting for the tree
# ie removing colunm like "id" or "url" that don't give any informations
Evo.data <- subset(Evo.data, select=c(category,timestamp,sold_since,products_sold))
# Subset : choose the colunm that you want

# Handling : column categorie
# Regular expression for spliting the categories
regex <- "/(.*)/(.*)/(.*)"
cat <- str_match(Evo.data$category, regex)
Evo.data$category <- cat[,3] # keep only the second part
Evo.data <- Evo.data[which(Evo.data$category == "Cannabis & Hashish"),]

# Handling : products_sold
Evo.data$products_sold <- gsub(pattern="NULL", replacement="0", Evo.data$products_sold)
Evo.data$products_sold <- as.numeric(Evo.data$products_sold)
# 1 product sold at least
Evo.data <- Evo.data[which(Evo.data$products_sold > 0),]

# Handling : timestamp and sold_since 
Evo.data$sold_since <-  as.Date(Evo.data$sold_since)
Evo.data$timestamp <-  as.Date(Evo.data$timestamp)
NbDays <- c()
for(i in 1 : length(Evo.data$timestamp)) {
  NbDays[i] <- Evo.data$timestamp[i] - Evo.data$sold_since[i]
}
NbDays <- as.numeric(NbDays)
Evo.data$NbDays <- NbDays

# Products_sold Per Day
productSoldPerDay <- c()
for(i in 1 : length(Evo.data$products_sold)) {
  productSoldPerDay[i] <- (Evo.data$products_sold[i]/Evo.data$NbDays[i])
}
Evo.data$ProdSoldPerDay <- productSoldPerDay

# NbDays on Web site per Year

NbDays2015 <- c()
NbDays2016 <- c()
NbDays2017 <- c()

End2015 <- as.Date("2015-12-31")
End2016 <- as.Date("2016-12-31")
beginning2017 <- as.Date("2017-01-01")

for(i in 1:length(Evo.data$sold_since)) {
  if (year(Evo.data$sold_since[i]) == 2015) {
      NbDays2015[i] <- End2015-Evo.data$sold_since[i]
      NbDays2016[i] <- 365
      NbDays2017[i] <- Evo.data$timestamp[i]-beginning2017}
  
  else if (year(Evo.data$sold_since[i]) == 2016) {
      NbDays2015[i] <- 0
      NbDays2016[i] <- End2016-Evo.data$sold_since[i]
      NbDays2017[i] <- Evo.data$timestamp[i]-beginning2017}
  
  else {
    NbDays2015[i] <- 0
    NbDays2016[i] <- 0
    NbDays2017[i] <- 0}
}

Evo.data$NbDays2015 <- NbDays2015
Evo.data$NbDays2016 <- NbDays2016
Evo.data$NbDays2017 <- NbDays2017


# Products Sold in 201X 

# Initialisation

productSold2015 <- c()
productSold2016 <- c()
productSold2017 <- c()

for(i in 1:length(Evo.data$sold_since)) {
  if (year(Evo.data$sold_since[i]) == 2016) {productSold2015[i] <- 0}
  else if (year(Evo.data$sold_since[i]) == 2017) {
    productSold2015[i] <- 0
    productSold2016[i] <- 0
    productSold2017[i] <- Evo.data$products_sold[i]}
}

Evo.data$ProdSold2015<- productSold2015
Evo.data$ProdSold2016<- productSold2016
Evo.data$ProdSold2017<- productSold2017

# Processing 

for(i in 1:length(Evo.data$ProdSoldPerDay)) {
  if (year(Evo.data$sold_since[i]) == 2015) {
    Evo.data$ProdSold2015[i] <- (Evo.data$ProdSoldPerDay[i] * Evo.data$NbDays2015[i])
    Evo.data$ProdSold2016[i] <- (Evo.data$ProdSoldPerDay[i] * Evo.data$NbDays2016[i])
    Evo.data$ProdSold2017[i] <- (Evo.data$ProdSoldPerDay[i] * Evo.data$NbDays2017[i])}
  
  else if (year(Evo.data$sold_since[i]) == 2016) {
    Evo.data$ProdSold2016[i] <- (Evo.data$ProdSoldPerDay[i] * Evo.data$NbDays2016[i])
    Evo.data$ProdSold2017[i] <- (Evo.data$ProdSoldPerDay[i] * Evo.data$NbDays2017[i])}
}

tt <- c()
for(i in 1:length(Evo.data$ProdSoldPerDay)) {
  tt[i] <- round(Evo.data$ProdSold2015[i]+Evo.data$ProdSold2016[i]+Evo.data$ProdSold2017[i])
}
Evo.data$tt <- tt

SumProd <- c(sum(Evo.data$ProdSold2015), sum(Evo.data$ProdSold2016), sum(Evo.data$ProdSold2017))
barplot(SumProd)
#SumProd <- as.data.frame(SumProd)
#rownames(SumProd) <- c("2015", "2016", "2017")

data$sold_since <-  as.Date(data$sold_since)
ad2015 <- nrow(data[which(year(data$sold_since) == 2015),])
ad2016 <- nrow(data[which(year(data$sold_since) == 2016),])
ad2017 <- nrow(data[which(year(data$sold_since) == 2017),])

#Evo.data <- Evo.data[which(month(Evo.data$sold_since) >= 5),]