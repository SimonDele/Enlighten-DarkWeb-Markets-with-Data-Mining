#---------------------------------------------
#        Ads distribution in the market
#---------------------------------------------

library(plotrix)

data <- as.data.frame(read.csv("alphaClean.csv"))

cat <- c()

for(i in 1:length(data$category)) {
  cat[i] <- unlist(strsplit(as.character(data$category[i]), "/"))[2]
  if(is.na(cat[i])) {cat[i] <- "Other Listings"}
}

tab_cat <- table(cat)

radial.pie(tab_cat,labels=names(tab_cat),mar=c(3,3,3,3),sector.colors=rainbow(length(tab_cat)))

