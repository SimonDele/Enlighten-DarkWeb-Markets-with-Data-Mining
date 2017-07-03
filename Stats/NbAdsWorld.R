#----------------------------------------
#           Number of ads in the world
#----------------------------------------
#data <- as.data.frame(read.csv("data.csv"))

library(stringr)

#Get rid of unwanted orign like Worldwide and Null which are not relevant
matching_vector <- c(  !str_detect(data$origin, "Worldwide") & !str_detect(data$origin, "NULL"))

sumup <- sort(summary(data[matching_vector, "origin"]), decreasing=TRUE)

#Bar plot with the total number ofs ads in each country
par(mar=c(5,2,4,0)+.1)
barp <- barplot(sumup[1:10], main="Number of ads in the World", xlab="Countries", ylab="Number of ads",ylim = c(0,4000),  col = rainbow(10), cex.names = 0.8)
barp <- text(x = barp, y = sumup[1:10], label = sumup[1:10], pos=3 , cex = 0.8, col= "black")