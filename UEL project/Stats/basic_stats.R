
#Count the numbers of ads of Drugs in each countries
sum_country_drugs<- summary(data[str_detect(data$category, "/Drugs"),"origin"])

#Plot the 10 biggest countries in the market of Drugs
sum_country_drugs <- sort(sum_country_drugs, decreasing=TRUE)
barplot(sum_country_drugs[1:10], main="Market of Drugs", xlab="Countries",ylab="Number of ads")

#data <- as.data.frame(read.csv("Z:\\data.csv"))

#install.packages("stringr")
#install.packages("units")

#library(stringr)
#library(units)

#matching_vector <- c(str_detect(data$category, "Cocaine")| str_detect(data$title, "cocaine"))
matching_vector <- c(str_detect(data$category, "Cocaine"))
sumup <- sort(summary(data[matching_vector, "origin"]), decreasing=TRUE)

barplot(sumup[1:10], main="Cocaine market in the world", xlab="Countries", ylab="Number of ads")


