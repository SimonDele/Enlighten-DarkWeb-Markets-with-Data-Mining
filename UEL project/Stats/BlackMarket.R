#data <- as.data.frame(read.csv("data.csv"))

library (stringr)

regex <- "/(.*?)(/.*)"

category1 <- str_match(data$category, regex )
categorysort <- sort(table(category1[,2]), decreasing=TRUE)


mylist <-list()
for(i in 1:length(categorysort)){
  matching_vector <- c(str_detect(category1[,2], names(categorysort)[i])); #spotting the lines corresponding to the i-th category
  category2 <- category1[matching_vector,3 ]; #get lines corresponding to the i-th category 
  category2 <- str_match(category2, "/(.*?)/" ) #get name of category, get rid of useless stuffs 
  mylist[[i]] <- c(table(category2[,2])); # list of table of frequency
}





barplot(categorysort[1:10], main="Black Market", xlab="Category", ylab="Number of ads")

