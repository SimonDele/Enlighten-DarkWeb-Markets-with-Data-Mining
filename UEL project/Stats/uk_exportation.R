#data <- as.data.frame(read.csv("data.csv"))[2:21]

library (stringr)

# United Kingdom as origin
matching_vector <- str_detect(data[,"origin"], "United Kingdom")

# list of the categories (among the line that have United Kingdom as origin)
# -> Products (categories) exporting by Uk 
uk_cat <- data[matching_vector,"category"] 

# Handling of this categories
# Regular expression for spliting the categories
regex <- "/(.*)/(.*)/(.*)"
cat <- str_match(uk_cat, regex )

# Counting this categories 
tab <- table(cat[,3])
tab <- sort(tab, decreasing = TRUE)[1:12]

piepercent<- round(100*tab/sum(tab), 1)
# round(a,1) : one digit after the comma

lab <- c()

for(i in 1:length(piepercent)) {
lab[i] <- paste(piepercent[[i]], "%", sep=" ")
}

pie(piepercent,labels = lab, main = "Uk - Exportation",col=rainbow(length(piepercent)))

legend(1.2,0.9, names(piepercent),cex = 0.9,fill = rainbow(length(piepercent)))
