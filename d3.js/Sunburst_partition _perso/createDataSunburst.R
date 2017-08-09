#----------------------------------------
#           Sunburst
#----------------------------------------
data <- as.data.frame(read.csv("alphaClean.csv"))

sunburst.data <- data.frame(table(subset(data, select=c(category))))

sunburst.data$Var1 <- gsub(" ","",sunburst.data$Var1)

sunburst.data$Var1<- gsub("/","-",sunburst.data$Var1)
sunburst.data$Var1<- gsub("^-","",sunburst.data$Var1)

write.csv(sunburst.data, file = "sunburst.data.csv")