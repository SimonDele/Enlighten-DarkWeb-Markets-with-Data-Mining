#----------------------------------------------------------
#           Association rules # Apriori algo
#----------------------------------------------------------

data <- as.data.frame(read.csv("data.csv"))

library(stringr)
library(arules)
library(arulesViz)

#Select all ads of "Drugs & Chemicals"
matching_vector <- c( str_detect(data$category, "Drugs & Chemicals"))
data_drugs <- data[matching_vector, ]


#Select some columns
asso.data <- subset(data_drugs, select = c(origin,category))

# Get rid of the first part of the category name "/Drugs & Chemicals/"
asso.data$category <- gsub(pattern = "/Drugs & Chemicals/", replacement = "", asso.data$category)

asso.data$origin <- factor(asso.data$origin)
asso.data$category <-factor(asso.data$category)


#Association Rules with rhs containing "China" only
rules <- apriori(asso.data,
                 parameter = list(minlen=2, supp=0.0005, conf=0.5),
                 appearance = list(rhs=c("origin=United States"),default="lhs"),
                 control = list(verbose=F))

rules.sorted <- sort(rules, by="lift")
arules::inspect(rules.sorted)

#Plot graph of rules
plot(rules.sorted, method="graph", control=list(type="items"),main ="Association Rules on the category and seller to deduce the country")



