#------------------------------------------------------------
#                Evolution of the market
#------------------------------------------------------------

data <- as.data.frame(read.csv("alphaClean.csv"))

library(stringr)
library(lubridate)
library(dygraphs)
library(xts)
library(zoo)
library(forecast)


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

# Month
Evo.data$sold_since <- as.yearmon(Evo.data$sold_since)

# Calculating the number od ads per Month
tab_Evo <- table(Evo.data$sold_since)
Evo.data <- as.data.frame(tab_Evo)
colnames(Evo.data) <- c("Time","Ads")
Evo.data <- Evo.data[1:(length(Evo.data$Time)-1),]
Evo.data$Time <- as.yearmon(Evo.data$Time)

# Time series object :
# start: from the first date that we have 
# frequency=12 i.e. monthly observations ((1=annual, 4=quartly)
Evo <- ts(Evo.data$Ads, start=Evo.data$Time[1], frequency=12)

# Prediction until Dec 2017 :
  # Analyse 
fit <- HoltWinters(Evo)
  # Prediction 
  # n.ahead : predict next six month with a confidence of 0.95
  # Prediction.interval : up and low prediction
pred <- predict(fit, n.ahead=6,prediction.interval=TRUE,level=0.95)

# Merge 
all <- cbind(Evo,pred)

# Ploting
dygraph(all, main = "Evolution of the Market", ylab = "Number of Ads") %>%
  dySeries("Evo", label = "Number of Ads") %>%
  dySeries(c("pred.lwr", "pred.fit", "pred.upr"), label = "Prediction") %>%
  dyOptions(drawPoints=TRUE, pointSize=2) %>%
  dyHighlight(highlightCircleSize=6) %>%
  dyEvent("2017-06-01", "Shut-Down", labelLoc = "bottom") %>%
  dyRangeSelector


#ad2015 <- nrow(Evo.data[which(year(Evo.data$sold_since) == 2015),])
#ad2016 <- nrow(Evo.data[which(year(Evo.data$sold_since) == 2016),])
#ad2017 <- nrow(Evo.data[which(year(Evo.data$sold_since) == 2017),])
#Evo.data <- Evo.data[which(month(Evo.data$sold_since) == 5),]