#data <- as.data.frame(read.csv("data.csv"))
library(stringr)

selectDrug <- function(drugName){
  matching_vector <- c( (str_detect(data$category, drugName) | str_detect(data$title, drugName) | str_detect(data$brief, drugName) ) )
  return(matching_vector)
}

drugs <- c("Cocaine", "Meth", "Opioids", "Cannabis", "Steroids", "Ecstasy", "Ketamine", "Heroin",  "NBOME","Shrooms", "Tobacco", "Benzos", "Paraphernalia")

med <-c()
for(i in 1:length(drugs)){
  matching_vector <- selectDrug(drugs[i]);
  med[i] <- median((data[matching_vector, "priceUnitDose"]))
}
priceDrugs <- data.frame(drugs, med); 


priceDrugs$med <- round(priceDrugs$med,2)

priceDrugs <- priceDrugs[order(priceDrugs$med, decreasing=TRUE), ]

barp <- barplot(priceDrugs$med, main="Average Price of Drugs in the World", names.arg = priceDrugs$drugs, ylim = c(0,500),  xlab="Drugs", ylab="Price in USD", cex.names = 0.8, col =rainbow(length(priceDrugs$drugs)) )
barp <- text(x = barp, y = priceDrugs$med, label = paste(priceDrugs$med, " $", sep=""), pos=3 , cex = 0.8, col= "red")
