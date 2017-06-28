#data <- as.data.frame(read.csv("data.csv"))

library(stringr)
library(ggmap)

#library(rworldmap)

matching_vector <- c( (str_detect(data$category, "Drugs") ) & !str_detect(data$origin, "Worldwide"))

sumup <- sort(summary(data[matching_vector, "origin"]), decreasing=TRUE)


barp <- barplot(sumup[1:10], main="Number of ads of Drugs in the World", xlab="Countries", ylab="Number of ads",ylim = c(0,3300),  col = rainbow(10), cex.names = 0.8)
barp <- text(x = barp, y = sumup[1:10], label = sumup[1:10], pos=3 , cex = 0.8, col= "red")


data_country <- read.csv("lat_long.csv")
lat_long <- data.frame(Country = data_country$Country , long=  data_country$Longitude..average., lat=  data_country$Latitude..average.)
v <- data.frame(name= names(sumup) , frequency = sumup)
data_plot <- merge(v, lat_long,  by.x = "name", by.y = "Country" )



#EUROPE
map <- get_map(location = 'Europe', zoom =4 )
mapPoints <- ggmap(map)  + xlab("") + ylab("") + ggtitle("Number of ads of Drugs in the World")+
  geom_point(data = data_plot,aes(x =long, y = lat, size =frequency)) +scale_size_continuous(limits=c(0,3000),breaks=c(0,500,1000,1500,2000), range = c(2,20)) 

mapPoints
#-END-EUROPE


#library(OpenStreetMap)

#map <- openmap(c(70,-110),
#               c(-60,179),zoom=2)
#map <- openproj(map)

#autoplot(map) + geom_point(data = data_plot,aes(x =long, y = lat, size = freq))
