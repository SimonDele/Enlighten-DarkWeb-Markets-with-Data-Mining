data <- as.data.frame(read.csv("Z:\\data.csv"))

#install.packages("stringr")
#install.packages("units")

library(stringr)
library(units)
library(ggmap)

#library(rworldmap)

matching_vector <- c( (str_detect(data$category, "Cocaine")| str_detect(data$title, "cocaine") | str_detect(data$brief, "cocaine") ) & !str_detect(data$origin, "Worldwide"))

sumup <- sort(summary(data[matching_vector, "origin"]), decreasing=TRUE)

barplot(sumup[1:10], main="Cocaine market in the world", xlab="Countries", ylab="Number of ads")



data_country <- read.csv("Z:\\lat_long.csv")
lat_long <- data.frame(Country = data_country$Country , long=  data_country$Longitude..average., lat=  data_country$Latitude..average.)
v <- data.frame(name= names(sumup) , freq = sumup)
data_plot <- merge(v, lat_long,  by.x = "name", by.y = "Country" )



#map <- get_map(location = 'Europe', zoom = )
#world <- map_data("world")
#worldMap <- ggplot(world, aes(x=long, y=lat, size = sqrt(freq) ,data = data_plot)) +
#  geom_path() +
#  scale_y_continuous(breaks=(-2:2) * 30) +
#  scale_x_continuous(breaks=(-4:4) * 45)

#worldPoints <- worldMap +
#  geom_point(aes())

#worldPoints
#mapPoints <- ggmap(map) +
#  geom_point(data = data_plot,aes(x =long, y = lat, size = sqrt(freq)))

#mapPoints

library(OpenStreetMap)
map <- openmap(c(70,-110),
               c(-60,179),zoom=2)
map <- openproj(map)

autoplot(map) + geom_point(data = data_plot,aes(x =long, y = lat, size = freq))
