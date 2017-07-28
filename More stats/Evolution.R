#------------------------------------------------------------
#                Evolution of the market
#------------------------------------------------------------

data <- as.data.frame(read.csv("alphaClean.csv"))

library(stringr)
library(lubridate)
library(dygraphs)
library(xts)
library(zoo)
library(stats)
library(forecast)


#-----------------
#   New Data 
#-----------------

# Select the column of the data that are interesting 
evo.data <- subset(data, select=c(category,sold_since))
  # Subset : choose the colunm that you want

# Remove ads with no informations
evo.data <- evo.data[which(evo.data$sold_since != "NULL"),]

# Formatting
evo.data$sold_since <-  as.Date(evo.data$sold_since)

# Remove ads from 2014 (only 10)
evo.data <- evo.data[which(year(evo.data$sold_since) > 2014),]

# Month
evo.data$sold_since <- as.yearmon(evo.data$sold_since)

# Calculating the number od ads per Month
tab_evo <- table(evo.data$sold_since)
Evo.data <- as.data.frame(tab_evo)
colnames(Evo.data) <- c("Time","Ads")

# Formatting
Evo.data$Time <- as.yearmon(Evo.data$Time)

# Time element
xts.data <- as.xts(x=Evo.data$Ads, order.by=Evo.data$Time)
colnames(xts.data) <- "Number of Ads"


dygraph(xts.data,  main = "Evolution of the Market", ylab = "Number of Ads") %>%
  dyOptions(stackedGraph=TRUE, drawPoints=TRUE, pointSize=2) %>%
  dyHighlight(highlightCircleSize=6) %>%
  dyEvent("2017-06-01", "Shut-Down", labelLoc = "bottom") %>%
  dyRangeSelector


predicted <- predict(xts.data,5)
all <- cbind(xts.data,predicted)

dygraph(predicted) %>%
  dySeries(c("lwr", "fit", "upr"), label = "Deaths")


#ad2015 <- nrow(Evo.data[which(year(Evo.data$sold_since) == 2015),])
#ad2016 <- nrow(Evo.data[which(year(Evo.data$sold_since) == 2016),])
#ad2017 <- nrow(Evo.data[which(year(Evo.data$sold_since) == 2017),])
#Evo.data <- Evo.data[which(month(Evo.data$sold_since) == 5),]