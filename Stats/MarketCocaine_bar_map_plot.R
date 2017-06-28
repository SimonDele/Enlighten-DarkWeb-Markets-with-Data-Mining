#data <- as.data.frame(read.csv("data.csv"))

#install.packages("stringr")
#install.packages("units")

library(stringr)

library(ggmap)

#library(rworldmap)

#matching_vector <- c( (str_detect(data$category, "Cocaine")| str_detect(data$title, "cocaine") | str_detect(data$brief, "cocaine") ) & !str_detect(data$origin, "Worldwide"))
matching_vector <- c( (str_detect(data$category, "Drugs") ) & !str_detect(data$origin, "Worldwide"))

sumup <- sort(summary(data[matching_vector, "origin"]), decreasing=TRUE)

barplot(sumup[1:10], main="Cocaine market in the world", xlab="Countries", ylab="Number of ads")



data_country <- read.csv("lat_long.csv")
lat_long <- data.frame(Country = data_country$Country , long=  data_country$Longitude..average., lat=  data_country$Latitude..average.)
v <- data.frame(name= names(sumup) , frequency = sumup)
data_plot <- merge(v, lat_long,  by.x = "name", by.y = "Country" )




#world <- map_data("world")
#worldMap <- ggplot(world, aes(x=long, y=lat, size = sqrt(data_plot$frequency) ,data = data_plot)) +
#  geom_path() +
#  scale_y_continuous(breaks=(-2:2) * 30) +
#  scale_x_continuous(breaks=(-4:4) * 45)

#worldPoints <- worldMap +  geom_point(aes())

#worldPoints

#EUROPE
map <- get_map(location = 'Europe', zoom =4 )
mapPoints <- ggmap(map)  + xlab("") + ylab("") + title("edf")+ ggtitle("Number of ads of Drugs in the World")+
  geom_point(data = data_plot,aes(x =long, y = lat, size =frequency)) +scale_size_continuous(limits=c(0,3000),breaks=c(0,500,1000,1500,2000), range = c(2,20)) 

mapPoints
#-END-EUROPE


#library(OpenStreetMap)

#map <- openmap(c(70,-110),
#               c(-60,179),zoom=2)
#map <- openproj(map)

#autoplot(map) + geom_point(data = data_plot,aes(x =long, y = lat, size = freq))
