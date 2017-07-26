#------------------------------------------------------------
#                 Evolution of the Market
#------------------------------------------------------------

#data <- as.data.frame(read.csv("alphaClean.csv"))

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
Evo.data <- subset(Evo.data, select=c(category,sold_since,products_sold))
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

Evo.data$sold_since <-  as.Date(Evo.data$sold_since)

Evo.data <- Evo.data[which(year(Evo.data$sold_since) == 2015),]
#Evo.data <- Evo.data[which(month(Evo.data$sold_since) >= 5),]