#----------------------------------------------------------
#           Association rules # Apriori algo
#----------------------------------------------------------

#data <- as.data.frame(read.csv("data.csv"))

library(stringr)


#Select all ads of "Drugs & Chemicals"
matching_vector <- c( str_detect(data$category, "Drugs & Chemicals"))
data_drugs <- data[matching_vector, ]

#Select some columns
asso.data <- subset(data_drugs, select = c(origin,category,seller))

#Get rid of the first part of the category name "/Drugs & Chemicals/"
asso.data$category <- gsub(pattern = "/Drugs & Chemicals/", replacement = "", asso.data$category)

asso.data$origin <- factor(asso.data$origin)
asso.data$category <-factor(asso.data$category)
asso.data$seller <- factor(asso.data$seller)

#Association Rules with rhs containing "Ecstasy" only
rules <- apriori(asso.data,
                 parameter = list(minlen=3, supp=0.0005, conf=0.8),
                 appearance = list(rhs=c("origin=China"),default="lhs"),
                 control = list(verbose=F))

rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)
