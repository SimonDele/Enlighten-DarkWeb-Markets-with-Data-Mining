#----------------------------------------
#           Number of ads of Drugs in the world
#----------------------------------------
data <- as.data.frame(read.csv("data.csv"))

library(stringr)

#Get rid of unwanted orign like Worldwide and Null which are not relevant
matching_vector <- c( str_detect(data$category, "Drugs") & !str_detect(data$origin, "Worldwide") & !str_detect(data$origin, "NULL"))

sumup <- sort(summary(data[matching_vector, "origin"]), decreasing=TRUE)
par(mfrow = c(1,1))
#Bar plot with the total number of ads of Drugs in each country

barp <- barplot(sumup[1:10], main="Number of ads of Drugs in the World", xlab="Countries", ylab="Number of ads",ylim = c(0,3200),  col = rainbow(10), cex.names = 0.8, angle = 45)
barp <- text(x = barp, y = sumup[1:10], label = sumup[1:10], pos=3 , cex = 0.8, col= "black")