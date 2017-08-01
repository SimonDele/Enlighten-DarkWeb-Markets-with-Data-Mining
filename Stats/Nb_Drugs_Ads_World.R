#----------------------------------------
#           Number of ads of Drugs in the world
#----------------------------------------
data <- as.data.frame(read.csv("data.csv"))

library(stringr)

# Get rid of unwanted orign like Worldwide and Null which are not relevant
# Selection of Drugs Ads
matching_vector <- c( str_detect(data$category, "Drugs") & !str_detect(data$origin, "Worldwide") & !str_detect(data$origin, "NULL"))

sumup <- sort(table(data[matching_vector, "origin"]), decreasing=TRUE)

# Bar plot with the total number of ads of Drugs in each country
par(las=1)#display yaxis horizontally
par(mar=c(5,6.5,4,0.5)) #give space for yaxis

barp <- barplot(sumup[1:10], main="Number of ads of Drugs in the World", xlab="Number of ads",xlim = c(0,max(sumup[1:10]+1000)),  col = rainbow(10), cex.names = 0.8, horiz = TRUE)

# Calculation in percentage
sumuppercent<- round(100*(sumup/sum(sumup)), 1)
# round(a,1) : one digit after the comma

lab <- c()

for(i in 1:length(sumuppercent)) {
  lab[i] <- paste(sumuppercent[[i]], "%", sep=" ")
}

barp <- text(y = barp, x = sumup[1:10], label = lab[1:10], pos=4 , cex = 0.8, col= "black")