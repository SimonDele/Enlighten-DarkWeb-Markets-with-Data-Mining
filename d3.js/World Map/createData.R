#data <- as.data.frame(read.csv("data.csv"))

#Objective built a json object with {time1 : {Country1, Country2...}, time2 : {...},...}

new.data <- subset(data, select=c(origin, sold_since))

new.data$origin <- as.character(new.data$origin)

#Clean data
new.data <- new.data[new.data[,1]!="Worldwide",]
new.data <- new.data[new.data[,1]!="NULL",]

#keep only the year and the month
new.data$sold_since <- str_sub(new.data$sold_since, 0, 7)

perMonth <- table(new.data)

for(i in 1:ncol(perMonth)){
  perMonth[i,] <- cumsum(perMonth[i,])
}

#perMonth <- as.data.frame(as.matrix(t(perMonth)))

#data_plot <- merge(perMonth, lat_long,  by.x = "name", by.y = "Country" )

write.csv(perMonth, file = "test.csv")