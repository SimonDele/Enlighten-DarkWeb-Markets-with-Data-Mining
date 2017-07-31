

library(stringr)

data <- as.data.frame(read.csv("alphaClean.csv"))

#-------------------------------------------------------------
#               Statistics fo decision tree
#-------------------------------------------------------------

# Select all "Drugs & Chemicals" ads
matching_vector <- c( str_detect(data$category, "Drugs & Chemicals"))
Drug.data <- data[matching_vector,]

# Select the column of the data that are interesting for the tree
# ie removing colunm like "id" or "url" that don't give any informations
Drug.data <- subset(Drug.data, select=c(origin,category,seller,priceUnitDose))
# Subset : choose the colunm that you want

# Handling : column categorie
# Regular expression for spliting the categories
regex <- "/(.*)/(.*)/(.*)"
cat <- str_match(Drug.data$category, regex)
Drug.data$category <- cat[,3] # keep only the second part

# Handling : seller
tab_sel <- table(Drug.data$seller)
tab_sel <- sort(tab_sel, decreasing=TRUE)  # Sorting (biggest in first)
tab_sel <- tab_sel[1:5] # Taking only the 5 main sellers
seller <- names(tab_sel) # Names of the 5 main sellers

# Initialization 
ori <- c()
price <- c()
cat <- c()

# calculation of informations for each seller
for(i in 1:5) {
  # Select ads from the ieme seller
  matching_vector <- c( str_detect(Drug.data$seller, seller[i]))
  sel.data <- Drug.data[matching_vector,]
  
  ori[i] <- names(sort(table(sel.data$origin),decreasing=TRUE)[1])  # First country 
  cat[i] <-names(sort(table(sel.data$category),decreasing=TRUE)[1]) # First category
  price[i] <- summary(sel.data$priceUnitDose)[[3]]  # Median price
}

sel.data <- as.data.frame(seller)
sel.data$origin <- ori
sel.data$category <- cat
sel.data$price <- price

print(sel.data)