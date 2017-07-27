#------------------------------------------------------------
#                Evolution of the market
#------------------------------------------------------------

data <- as.data.frame(read.csv("alphaClean.csv"))

library(stringr)
library(lubridate)
library(dygraphs)
library(xts)

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

tab_Evo <- table(Evo.data$sold_since)
evo.data <- as.data.frame(tab_Evo)
evo.data$Var1 <- as.Date(evo.data$Var1)
evo.data <- as.xts(evo.data)

xts2 <- xts(x=evo.data$Freq, order.by=evo.data$Var1)

plot(tab_Evo)

a <- c()

for(i in 1:5) {
  a[i] <- xts(tab_Evo[i])
}

data <- rnorm(5)
dates <- seq(as.Date("2017-05-01"), length=5, by="days")
xts2 <- xts(x=data, order.by=dates)



dygraph(xts2)

#ad2015 <- nrow(Evo.data[which(year(Evo.data$sold_since) == 2015),])
#ad2016 <- nrow(Evo.data[which(year(Evo.data$sold_since) == 2016),])
#ad2017 <- nrow(Evo.data[which(year(Evo.data$sold_since) == 2017),])
#Evo.data <- Evo.data[which(month(Evo.data$sold_since) == 5),]