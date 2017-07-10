#----------------------------------------------------------------------
#                  Bayesian Classification - Naive
#   Prediction of the country knowing the seller / price / category
#-----------------------------------------------------------------------

#data <- as.data.frame(read.csv("data.csv"))

library(stringr)
library(e1071)


#-----------------
#   New Data 
#-----------------

# Select all "Drugs & Chemicals" ads
matching_vector <- c( str_detect(data$category, "Drugs & Chemicals"))
bayesian.data <- data[matching_vector,]

# Select the column of the data that are interesting for the tree
# ie removing colunm like "id" or "url" that don't give any informations
bayesian.data <- subset(bayesian.data, select=c(origin,category,seller,priceUnitDose))
# Subset : choose the colunm that you want

# Handling : column category
# Regular expression for spliting the categories
regex <- "/(.*)/(.*)/(.*)"
cat <- str_match(bayesian.data$category, regex)
bayesian.data$category <- cat[,3] # keep only the second part


#Get the main countries
countries <- names(sort(table(bayesian.data$origin), decreasing = TRUE)[1:5])
#Remove Worldwide if present 
countries <- countries[!is.element(countries, "Worldwide")]

bayesian.data <-subset(bayesian.data, origin %in% countries) 

'
matching_vector <-  c( str_detect(bayesian.data$origin, countries[1]))
for(i in 1:length(countries)){
  matching_vector <- matching_vector |  c( str_detect(bayesian.data$origin, countries[i]))
}

bayesian.data <- bayesian.data[matching_vector,]
'
bayesian.data$origin <- factor(bayesian.data$origin, labels = countries)

#country <- "Canada"

# Conversion to binary value
# -> 1 if the origin = country
# -> 0 if the origin # country
#bayesian.data$origin <-c(str_detect(bayesian.data$origin, country))

train.data <- bayesian.data[1:floor(nrow(bayesian.data)/2),]
pred.data <- bayesian.data[(floor(nrow(bayesian.data)/2)+1):nrow(bayesian.data),]

model <- naiveBayes(origin ~ ., data =  train.data)

#model <- naiveBayes(train.data[,-1], train.data[,1])
class(model)
summary(model)
print(model)

preds <- predict(model, newdata = pred.data)
conf_matrix <- table(preds, pred.data$origin)
acc <- sum(diag(conf_matrix)) / sum(conf_matrix)

print(conf_matrix)
print(acc)

