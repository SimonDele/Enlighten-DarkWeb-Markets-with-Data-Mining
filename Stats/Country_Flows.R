
#data <- as.data.frame(read.csv("alphaClean.csv"))

#-----------------------------------------------
#     Exportation / Importation  of a country
#-----------------------------------------------

#-------------------
#  Initialization
#-------------------

# Enter the name of the Country
# 0 for Exportation
# 1 for Importation

country <- "United Kingdom"
num <- 0

#------------------
#  Pre-Processing
#------------------

if (num == 0) { 
  way <- "origin"
  txt <- "- Exportation"
} else if (num == 1) {
  way <- "destination"
  txt <- "- Importation"
}

#------------------
#    Analysis
#------------------

# Country as destination
matching_vector <- str_detect(data[,way], country)

# list of the categories (among the line that have "Country" as origin)
# -> Products (categories) exporting by the country 
country_cat <- data[matching_vector,"category"] 

# Handling of this categories
# Regular expression for spliting the categories
regex <- "/(.*)/(.*)/(.*)"
cat <- str_match(country_cat, regex)

# Counting this categories 
tab <- table(cat[,3])   #cat[,3] : 2nd category 
tab <- sort(tab, decreasing = TRUE)  # Sorting (biggest in first) 
tab <- tab[1:10] # Taking only the most important

#-----------------
#    Pie Chart     
#-----------------

# 1- Labels :
# Calculation in percentage
piepercent<- round(100*tab/sum(tab), 1)
# round(a,1) : one digit after the comma

lab <- c()

for(i in 1:length(piepercent)) {
  lab[i] <- paste(piepercent[[i]], "%", sep=" ")
}

# 2- Title :
title <- paste(country, txt, sep=" ")

# 3- Colors :
c <- rainbow(length(piepercent))

# 4- Plot :
pie3D(piepercent,labels = lab,labelcex = 1, main = title ,col=c, theta = 0.9, explode = 0.04)

# 5- Legend :
legend(x=-2.3,y=-1.1,names(piepercent), cex = 0.8, fill = c,ncol=4,border=NA, xpd=NA)