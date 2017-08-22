library(stringr)
library(jsonlite)
library(randomcoloR)

data <- as.data.frame(read.csv("../alphaClean.csv"))

# Read a file containing the latitude and longitude of the "center" of each country
data_country <- read.csv("./Stats/lat_long.csv")
lat_long <- data.frame(Country = data_country$Country , long=  data_country$Longitude..average., lat=  data_country$Latitude..average.)

for(i in 1:nrow(lat_long)){
  lat_long[i,4] <- randomColor() 
}


#Objective built a json object with {time1 : {Country1, Country2...}, time2 : {...},...}

new.data <- subset(data, select=c(origin, sold_since))

new.data$origin <- as.character(new.data$origin)

#Clean data
new.data <- new.data[new.data[,1]!="Worldwide",]
new.data <- new.data[new.data[,1]!="NULL",]

#keep only the year and the month
new.data$sold_since <- str_sub(new.data$sold_since, 0, 7)

perMonth <- table(new.data)

for(i in 1:nrow(perMonth)){
  perMonth[i,] <- cumsum(perMonth[i,])
}







liste <- list()
for(i in 1: ncol(perMonth)){ 'ncol(perMonth)'
  ligne <- list(date = "")
  ligne["date"] <- colnames(perMonth)[i]
  
  data.list <- list()
  for(j in 1:nrow(perMonth)){  'nrow(perMonth)'
    data.list<- append(data.list, list(list(name = rownames(perMonth)[j], radius= round(perMonth[j,i]/max(perMonth)*200,0), latitude = lat_long[lat_long==rownames(perMonth)[j],3 ] , longitude = lat_long[lat_long==rownames(perMonth)[j],2 ], color = lat_long[j,4])))
  }
  ligne <- append(ligne, list(data = data.list))
  liste <- append(liste,list(ligne))
}

jsonOut<-toJSON(liste,pretty = TRUE,auto_unbox = TRUE)
#cat(jsonOut)

write(jsonOut, "dataWorldMapNew.json")

