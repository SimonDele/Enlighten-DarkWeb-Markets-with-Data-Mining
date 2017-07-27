#------------------------------------------------------------
#                Evolution of the market
#------------------------------------------------------------

data <- as.data.frame(read.csv("alphaClean.csv"))

library(stringr)
library(lubridate)

#-----------------
#   New Data 
#-----------------

# Select the column of the data that are interesting 
Evo.data <- subset(data, select=c(category,sold_since))
  # Subset : choose the colunm that you want

# Remove ads with no informations
Evo.data <- Evo.data[which(Evo.data$sold_since != "NULL"),]

# Formatting
Evo.data$sold_since <-  as.Date(Evo.data$sold_since)

# Remove ads from 2014 (only 10)
Evo.data <- Evo.data[which(year(Evo.data$sold_since) > 2014),]


ad2015 <- nrow(Evo.data[which(year(Evo.data$sold_since) == 2015),])
ad2016 <- nrow(Evo.data[which(year(Evo.data$sold_since) == 2016),])
ad2017 <- nrow(Evo.data[which(year(Evo.data$sold_since) == 2017),])


Evo.data <- Evo.data[which(month(Evo.data$sold_since) == 5),]