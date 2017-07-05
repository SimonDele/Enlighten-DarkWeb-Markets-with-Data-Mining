#----------------------------------------------------------
#           Descision tree UK
#----------------------------------------------------------

#data <- as.data.frame(read.csv("data.csv"))

library(stringr)

#Select all ads of "Drugs & Chemicals"
matching_vector <- c( str_detect(data$category, "Drugs & Chemicals"))
data_drugs <- data[matching_vector, ]

descision.data <- subset(data_drugs, select = c(seller, origin, destination, category, priceUnitDose))

descision.data$origin <-c ( str_detect(descision.data$origin, "United Kingdom"))

names(descision.data)[match("origin",names(descision.data))] <- "UK"

descision.data.num <-c()

descision.data[,match("UK",names(descision.data))]<-gsub(TRUE, 1, descision.data[,match("UK",names(descision.data))])
descision.data[,match("UK",names(descision.data))] <- gsub(FALSE, 0, descision.data[,match("UK",names(descision.data))])

descision.data$UK <- as.numeric(descision.data$UK)
