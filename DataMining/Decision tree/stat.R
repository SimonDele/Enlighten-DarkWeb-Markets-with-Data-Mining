

library(stringr)

#data <- as.data.frame(read.csv("alphaClean.csv"))

# Select all "Drugs & Chemicals" ads
matching_vector <- c( str_detect(data$category, "Drugs & Chemicals"))
Drug.data <- data[matching_vector,]

# Select ads from UK
matching_vector <- c( str_detect(Drug.data$origin, "United Kingdom"))
DrugUK.data <- Drug.data[matching_vector,]

# Handling : seller
tab_selUK <- table(DrugUK.data$seller)
tab_selUK <- sort(tab_selUK, decreasing=TRUE)  # Sorting (biggest in first)
tab_selUK <- tab_selUK[1:5] # Taking only the most important : main sellers

# Select ads from UK
matching_vector <- c( str_detect(Drug.data$origin, "China"))
DrugChina.data <- Drug.data[matching_vector,]

# Handling : seller
tab_selCh <- table(DrugChina.data$seller)
tab_selCh <- sort(tab_selCh, decreasing=TRUE)  # Sorting (biggest in first)
tab_selCh <- tab_selCh[1:5] # Taking only the most important : main sellers

print(tab_selCh)
print()