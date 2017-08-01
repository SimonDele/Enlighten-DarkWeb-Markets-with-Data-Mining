data <- as.data.frame(read.csv("data.csv"))

#-----------------------------------
#         Europe Map Plot
#-----------------------------------

library(stringr)
library(ggmap)

# Select the ads about drugs and get rid of the irrelevant orign Worlwide
matching_vector <- c( (str_detect(data$category, "Drugs") ) & !str_detect(data$origin, "Worldwide"))

sumup <- sort(summary(data[matching_vector, "origin"]), decreasing=TRUE)

# Read a file containing the latitude and longitude of the "center" of each country
data_country <- read.csv("../Stats/lat_long.csv")
lat_long <- data.frame(Country = data_country$Country , long=  data_country$Longitude..average., lat=  data_country$Latitude..average.)

# Create a data.frame with the name of the country and its nb of ads
v <- data.frame(name= names(sumup) , amount = sumup)

# Merge v with lat_long in order to have a data with Country/NbofAds/lattitude/longitude
data_plot <- merge(v, lat_long,  by.x = "name", by.y = "Country" )

# Create a map of EUROPE with circles showing the amount of ads
map <- get_map(location = 'Europe', zoom =4 )
mapPoints <- ggmap(map)  + xlab("") + ylab("") + ggtitle("Number of ads of Drugs in Europe")+
  geom_point(data = data_plot,aes(x =long, y = lat, size =amount)) +scale_size_continuous(limits=c(0,3000),breaks=c(0,500,1000,1500,2000), range = c(0,13)) 

# Display
mapPoints
