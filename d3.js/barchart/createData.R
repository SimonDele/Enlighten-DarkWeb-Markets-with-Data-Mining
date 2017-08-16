#------------------------------------------
#        Number of ads in the world
#------------------------------------------

library(stringr)

data <- as.data.frame(read.csv("alphaClean.csv"))

new.data <- subset(data, select = c(origin, sold_since))



# Get rid of unwanted orign like Worldwide and Null which are not relevant

new.data$sold_since <- str_sub(new.data$sold_since, 0, 4)

matching_vector <- c( !str_detect(new.data$origin, "Worldwide") & !str_detect(new.data$origin, "NULL"))

new.data <- new.data[matching_vector, ]

new.data <-subset(new.data, sold_since!=2014)



export.data <- data.frame(table(new.data))

annee2015 = sum(subset(export.data, sold_since==2015, select = Freq))
annee2016 = sum(subset(export.data, sold_since==2016, select = Freq))
annee2017 = sum(subset(export.data, sold_since==2017, select = Freq))

export.2015 <- subset(export.data, sold_since==2015 , select = c(origin, Freq))
export.2016 <- subset(export.data, sold_since==2016 , select = c(origin, Freq))
export.2017 <- subset(export.data, sold_since==2017 , select = c(origin, Freq))

export.2015$Freq <- export.data[export.data$sold_since == 2015, 3]*100/annee2015
export.2016$Freq <- export.data[export.data$sold_since == 2016, 3]*100/annee2016
export.2017$Freq <- export.data[export.data$sold_since == 2017, 3]*100/annee2017

write.csv(export.2015, file = "adsAnnee2015.csv")
write.csv(export.2017, file = "adsAnnee2016.csv")
write.csv(export.2017, file = "adsAnnee2017.csv")