#data <- as.data.frame(read.csv("data.csv"))

library (stringr)

regex <- "/(.*?)(/.*)"

category1 <- str_match(data$category, regex )
categorysort <- sort(table(category1[,2]), decreasing=TRUE)


#Useless here but I was thinking of doing a stacked plot 
mylist <-list()
for(i in 1:length(categorysort)){
  matching_vector <- c(str_detect(category1[,2], names(categorysort)[i])); #spotting the lines corresponding to the i-th category
  category2 <- category1[matching_vector,3 ]; #get lines corresponding to the i-th category 
  category2 <- str_match(category2, "/(.*?)/" ) #get name of category, get rid of useless stuffs 
  mylist[[i]] <- c(table(category2[,2])); # list of table of frequency
}





#barplot(categorysort[1:10], main="Black Market", xlab="Category", ylab="Number of ads")

categorysort <- categorysort[1:10]

# Calculation in percentage
piepercent<- round(100*categorysort/sum(categorysort), 1)
# round(a,1) : one digit after the comma


# 1- Labels :
lab <- c()

for(i in 1:length(piepercent)) {
  lab[i] <- paste(piepercent[[i]], "%", sep=" ")
}

# 2- Title :
title <- "Distribution of ads per category"

# 3- Colors :
c <- rainbow(length(piepercent))

# 4- Plot :
pie(piepercent,labels = lab, main = title ,col=c)

# 5- Legend :
legend(1.2,0.8, names(piepercent), cex = 0.7, fill = c)


