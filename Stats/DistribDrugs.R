data <- as.data.frame(read.csv("data.csv"))

library (stringr)

#----------------------------------------
#           The most common drugs
#----------------------------------------

selectDrug <- function(drugName){
  matching_vector <- c( (str_detect(data$category, drugName)))
  return(matching_vector)
}


drugs <- c("Cocaine", "Meth", "LSD", "Opioids", "Cannabis", "Steroids", "Ecstasy", "Ketamine", "Heroin",  "NBOME","Shrooms", "Tobacco", "Benzos", "Paraphernalia")

freq <- c()
for(i in 1:length(drugs)){
  matching_vector <- selectDrug(drugs[i]);
  sumup<-summary(matching_vector)
  freq[i] <- sumup[3]
  #med[i] <- median((data[matching_vector, "priceUnitDose"]))
}

freq <- as.numeric(freq)#/length(rownames(data))

res <- data.frame(drugs, freq)
res <- res[order(res$freq, decreasing = TRUE),]

# Calculation in percentage
piepercent<- round(100*res$freq/sum(res$freq), 1)
# round(a,1) : one digit after the comma

#----------------------------------------
#               PIE CHART 
#----------------------------------------

  # 1- Labels :
  lab <- c()
  
  for(i in 1:length(piepercent)) {
    lab[i] <- paste(piepercent[[i]], "%", sep=" ")
  }
  
  # 2- Title :
  title <- "Distribution of drugs"
  
  # 3- Colors :
  c <- rainbow(length(piepercent))
  
  # 4- Plot :
  pie(piepercent,labels = lab, main = title ,col=c)
  
  # 5- Legend :
  legend(1.2,0.8,res$drugs, cex = 0.7, fill = c)

#----------------------------------------
#     Price of the most common drugs according to articles
#----------------------------------------

prices <- c()
ref <- c()
for(i  in 1 : length(drugs)){
  prices[i] <- c()
  ref <- 
}

drugs_prices_art <- data.frame(drugs, prices, ref) 
