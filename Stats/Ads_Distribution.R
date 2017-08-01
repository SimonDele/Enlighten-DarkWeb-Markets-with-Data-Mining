#---------------------------------------------
#        Ads distribution in the market
#---------------------------------------------

library(plotrix)

#data <- as.data.frame(read.csv("alphaClean.csv"))

cat <- c()

for(i in 1:length(data$category)) {
  cat[i] <- unlist(strsplit(as.character(data$category[i]), "/"))[2]
  if(is.na(cat[i])) {cat[i] <- "Other Listings"}
}

tab_cat <- table(cat)
tab_cat <- sort(tab_cat, decreasing=TRUE) 
cat.data <- as.data.frame(tab_cat)

radial.plot(cat.data$Freq,labels=cat.data$cat,label.prop=1.1,rp.type="r",start=4,clockwise=TRUE,lwd=4,line.col=rainbow(length(tab_cat)),main="Market distibution")

