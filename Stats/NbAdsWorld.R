#----------------------------------------
#           Number of ads in the world
#----------------------------------------
#data <- as.data.frame(read.csv("data.csv"))

library(stringr)

#Get rid of unwanted orign like Worldwide and Null which are not relevant
matching_vector <- c(  !str_detect(data$origin, "Worldwide") & !str_detect(data$origin, "NULL"))

sumup <- sort(summary(data[matching_vector, "origin"]), decreasing=TRUE)
par(mfrow = c(1,1))
#Bar plot with the total number ofs ads in each country
#par(mar=c(5,2,4,0)+.1)
par(las=1)

barp <- barplot(sumup[1:10], main="Number of ads in the World", xlim= c(0,max(sumup[1:10])+500), xlab="Number of ads",horiz = TRUE,  col = rainbow(10), cex.names = 0.8, angle = 45)
barp <- text(y = barp, x = sumup[1:10], label = sumup[1:10], pos=4 , cex = 0.8, col= "black")

