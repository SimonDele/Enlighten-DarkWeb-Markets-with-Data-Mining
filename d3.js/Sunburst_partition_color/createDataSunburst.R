#----------------------------------------
#           Sunburst
#----------------------------------------
#data <- as.data.frame(read.csv("alphaClean.csv"))

library(stringr)

sunburst.data <- data.frame(table(subset(data, select=c(category))))


#Step 1 : Remove space and first "/"
sunburst.data$Var1 <- gsub(" ","",sunburst.data$Var1)

#sunburst.data$Var1<- gsub("/","-",sunburst.data$Var1)
sunburst.data$Var1<- gsub("^/","",sunburst.data$Var1)


#Step 2 : 


#Remove Other
tempCat <- as.data.frame(str_split_fixed(sunburst.data$Var1, "/", n=3))
tempCat$V3 <- gsub("Other", "end",tempCat$V3 )

#Remove when the sub-category is the same as the previous one (ex : Other/Other)
#Put end when the series end prematurly
for(i in 1:nrow(tempCat)){
  if(tempCat[i,2]==tempCat[i,3]){
    tempCat[i,3]<-""
  }
  if(str_length(tempCat[i,3])==0){
    tempCat[i,3]<-"end"
  }
}
cat <- paste(t(tempCat$V1),t(tempCat$V2), t(tempCat$V3),sep="/")

sunburst.data$Var1 <- cat

write.csv(sunburst.data, file = "sunburst.datav3.csv")