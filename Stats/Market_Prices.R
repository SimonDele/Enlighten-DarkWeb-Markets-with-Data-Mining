#data <- as.data.frame(read.csv("data.csv"))

#-------------------------------------------
#            Drug Prices
#-------------------------------------------

library(stringr)

selectDrug <- function(drugName){
   matching_vector <- c( (str_detect(data$category, drugName)))
   return(matching_vector)  
}
  
drugs <- c("Cocaine", "Meth", "Opioids", "Cannabis", "Steroids", "Ecstasy", "Ketamine", "Heroin",  "NBOME","Shrooms", "Tobacco", "Benzos", "Paraphernalia")

med <-c()
for(i in 1:length(drugs)){
  matching_vector <- selectDrug(drugName =  drugs[i]);
  med[i] <- median((data[matching_vector, "priceUnitDose"]))
}
priceDrugs <- data.frame(drugs, med); 

priceDrugs$med <- round(priceDrugs$med,2)

priceDrugs <- priceDrugs[order(priceDrugs$med, decreasing=TRUE), ]

par(las=1) # Display yaxis horizontally
par(mar=c(4,8,3,2)) # Give space for yaxis

barp <- barplot(priceDrugs$med, main="Average Price of Drugs per Gram", names.arg = priceDrugs$drugs, xlim = c(0,max(priceDrugs$med+100)), cex.names = 0.8, col =rainbow(length(priceDrugs$drugs)), horiz =TRUE)
barp <- text(y = barp, x = priceDrugs$med, label = paste(priceDrugs$med, " $", sep=""), pos=4 , cex = 0.8, col= "Black")


#---------------------------------------
#        Prices find on articles 
#---------------------------------------

cols <- c("Cocaine", "Meth", "Opioids",      "Cannabis"      , "Steroids", "Ecstasy", "Ketamine", "Heroin",  "NBOME","Shrooms", "Tobacco", "Benzos", "Paraphernalia" , "MDMA", "Amphetamine", "Crack", "LSD"   , "URL")

ref1  <- c(    35   ,   200  ,     NA   ,     (5.3 + 7.85)/2   ,    NA     ,     15   ,     25    ,    100   ,     NA  ,    NA   ,    NA    ,     NA  ,        NA      ,    40 ,   5         ,  NA    ,  NA     ,   "http://www.drugwise.org.uk/how-much-do-drugs-cost/")
ref2  <- c(    67   ,   NA   ,     NA   ,      51              ,    NA     ,     15   ,     32    ,    129   ,     NA  ,    NA   ,    NA    ,     NA  ,        NA      ,   51  ,   15        ,  97    ,  NA     , "http://www.telegraph.co.uk/news/uknews/crime/11346133/The-cost-of-street-drugs-in-Britain.html")
ref3  <- c(    110  ,   80   ,     NA   ,      NA              ,    NA     ,     NA   ,     NA    ,    170   ,     NA  ,    5.7  ,    NA    ,     NA  ,        NA      ,  150  ,   NA        ,   NA   ,  12000  ,   "http://www.rehabcenter.net/the-average-cost-of-illegal-drugs-on-the-street/ " )
ref4  <- c(    80   ,   109  ,     NA   ,      NA              ,    NA     ,   19.12  ,     NA    ,    91.16 ,     NA  ,    NA   ,    NA    ,     NA  ,        NA      ,    NA ,   NA        ,   NA   ,     NA  ,   " http://o.canada.com/business/interactive-what-illegal-drugs-cost-on-the-street-around-the-world")
ref5  <- c(    64   ,   NA   ,     NA   ,      NA              ,    NA     ,     20   ,     NA    ,    NA    ,     NA  ,    NA   ,    NA    ,     NA  ,        NA      ,    NA ,   NA        ,  NA    ,   NA    , " http://www.thestudentpocketguide.com/2012/01/student-life/health-and-relationships/facts-about-drugs/")

doc_drugs <- t(data.frame(ref1, ref2, ref3, ref4, ref5))
colnames(doc_drugs) <- cols

# Calculate the mean price of each drugs find on articles
price_doc <- c()
for(i in 1 : length(cols)){
  price_doc[i] <- summary(as.numeric(doc_drugs[,i]))[[4]]
}
price_doc.data <- data.frame(cols, price_doc)

# Merge the previous dataframe which correspond to the mean price of each drugs in the data
# with the dataframe created above
beside_plot <- merge(price_doc.data, priceDrugs, by.x ="cols", by.y ="drugs") 

rownames(beside_plot) <- beside_plot[,1]
beside_plot <- beside_plot[,-1]

# Creating the barplot
par(las=1)#display yaxis horizontally
par(mar=c(6,8,4,3)) # Give space for yaxis

b <- barplot(rbind(beside_plot[,1], beside_plot[,2]), main="Average price of Drugs", xlim = c(0, 400),
             xlab="Price of Drugs", beside=TRUE, names.arg = rownames(beside_plot), col=1:2, horiz = TRUE, space = c(0,0.4), cex.names = 0.8)

axis(side=1,at=c(50,150,250,350),labels=c(50,150,250,350))

lab <- c("Street", "AlphaBay")

legend("topright",lab,fill=1:2, cex=0.8)

