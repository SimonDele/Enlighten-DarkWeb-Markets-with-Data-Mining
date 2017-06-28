data <- as.data.frame(read.csv("data.csv"))
library(stringr)

selectDrug <- function(drugName){
  matching_vector <- c( (str_detect(data$category, drugName) | str_detect(data$title, drugName) | str_detect(data$brief, drugName) ) )
  return(matching_vector)
}
#matching_vector <- c( (str_detect(data$category, "Cocaine")| str_detect(data$title, "cocaine") | str_detect(data$brief, "cocaine") ) )

drugs <- c("Cocaine", "Meth", "Opioids", "Cannabis", "Steroids", "Ecstasy", "Ketamine", "Heroin",  "NBOME","Shrooms", "Tobacco", "Benzos", "Paraphernalia")

med <-c()
for(i in 1:length(drugs)){
  matching_vector <- selectDrug(drugs[i]);
  med[i] <- median((data[matching_vector, "priceUnitDose"]))
}
priceDrugs <- data.frame(drugs, med); 


priceDrugs$med <- round(priceDrugs$med,2)

barp <- barplot(priceDrugs$med, main="Price of drugs", names.arg = priceDrugs$drugs,  xlab="Drugs", ylab="Price in USD")
barp <- text(x = barp, y = priceDrugs$med, label = priceDrugs$med, pos= 3, cex = 0.8, col= "red")