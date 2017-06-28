#####################################################################
#                            Library
#####################################################################

library (stringr)

#####################################################################
#                         Initialization
#####################################################################

data <- as.data.frame(read.csv("data.csv"))[2:21]

# Choose Your country :
country <- "United Kingdom"


#####################################################################
#                           Analysis
#####################################################################


# Country as origin
matching_vector <- str_detect(data[,"origin"], country)

# list of the categories (among the line that have "Country" as origin)
# -> Products (categories) exporting by the country 
country_cat <- data[matching_vector,"category"] 

# Handling of this categories
# Regular expression for spliting the categories
regex <- "/(.*)/(.*)/(.*)"
cat <- str_match(country_cat, regex)

# Counting this categories 
tab <- table(cat[,3])   #cat[,3] : 2nd category 
tab <- sort(tab, decreasing = TRUE)  # Sorting (biggest in first) 
tab <- tab[1:10] # Taking only the most important

# Calculation in percentage
piepercent<- round(100*tab/sum(tab), 1)
# round(a,1) : one digit after the comma


#####################################################################
#                           Pie Chart     
#####################################################################

# 1- Labels :
lab <- c()

for(i in 1:length(piepercent)) {
  lab[i] <- paste(piepercent[[i]], "%", sep=" ")
}

# 2- Title :
title <- paste(country, "- Exportation" , sep=" ")

# 3- Colors :
c <- rainbow(length(piepercent))

# 4- Plot :
pie(piepercent,labels = lab, main = title ,col=c)

# 5- Legend :
legend(1.2,0.9, names(piepercent), cex = 0.9, fill = c)