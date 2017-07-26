#------------------------------------------------------------
#                 Evolution of the Market
#------------------------------------------------------------

data <- as.data.frame(read.csv("alphaClean.csv"))

library(stringr)

#-----------------
#   New Data 
#-----------------

# Select all "Drugs & Chemicals" ads
matching_vector <- c( str_detect(data$category, "Drugs & Chemicals"))
data <- data[matching_vector,]

# Select the column of the data that are interesting for the tree
# ie removing colunm like "id" or "url" that don't give any informations
#data <- subset(data, select=c(category,sold_since,products_sold))
# Subset : choose the colunm that you want

# Handling : column categorie
# Regular expression for spliting the categories
#regex <- "/(.*)/(.*)/(.*)"
#cat <- str_match(dectree.data$category, regex)
#data$category <- cat[,3] # keep only the second part